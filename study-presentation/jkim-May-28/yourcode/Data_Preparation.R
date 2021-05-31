## ----setup, include=FALSE---------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE,
                      fig.align = "center")


## ----load.lib, message = FALSE, warning = FALSE, results = 'hide'-----------------------------
# libraries
library(tidymodels)            # a collection of packages for modeling : load multiple 'tidymodels' packages in a single step
library(tidyverse)             # load multiple 'tidyverse' packages in a single step
library(magrittr)              # includes "pipe" (%>%) operator
library(skimr)                 # includes a function "skim" to skim the dataset
library(knitr)                 # includes a function "purl" to convert RMD to R 
library(naniar)                # includes functions to visualize and analyse NAs.
theme_set(theme_bw())          # theme for ggplot2


## ---- message = FALSE-------------------------------------------------------------------------
# designate the file path. It matches to the kaggle RMD setting.

# file path 
# input   /
# ...     / house-prices-advanced-regression-techniques/
# ...     / ... / [train.csv, test.csv, data_description.txt, sample_submission.csv] 
# output  /
# yourcode/
# ...     / [data_prep.RMD] 
 
file_path <- "../input/house-prices-advanced-regression-techniques"

# Retrieve the list of files in the path. 
files <- list.files(file_path)

# There are four files in the input folder
files

# Load the trainset 
train <- read_csv(file.path(file_path, "train.csv"))

# Load the testset
test <- read_csv(file.path(file_path, "test.csv"))

# Bind two dataset into "all_data" and clean variable names (turn into lower cases and delim with "_")
all_data <- bind_rows(train, test) %>% 
  janitor::clean_names()

all_data %>% head()


## ---------------------------------------------------------------------------------------------
# Recipe for the tidymodels
housing_recipe1 <- all_data %>% 
    recipe(sale_price ~ .) %>%
    step_rm(id) %>%
    step_log(sale_price) %>%
    step_impute_mode(all_nominal()) %>%
    step_dummy(all_nominal()) %>%
    step_impute_mean(all_predictors()) %>%
    step_normalize(all_predictors()) %>%
    prep()

print(housing_recipe1) 


## ---------------------------------------------------------------------------------------------
# Preprocess the recipe1
all_data1 <- recipes::juice(housing_recipe1)


## ---------------------------------------------------------------------------------------------
# Get the index for trainset
train_index <- seq_len(nrow(train))

# Get the pre-processed trainset
train1 <- all_data1[train_index,]

# Get the pre-processed testset
test1 <- all_data1[-train_index,]


## ----message = FALSE, warning = FALSE---------------------------------------------------------
# Lasso model using glmnet engine with penalty 0.01 and mixture 1
lasso_model1 <- 
  linear_reg(penalty = 0.01, mixture = 1) %>%
  set_engine("glmnet")

# Run the model with formula sale_price ~ .(all other predictors) with dataset train1.
lasso_fit1 <- 
  lasso_model1 %>%
  fit(sale_price ~ ., data = train1)

# Find the estimates that are higher than 0.001.
lasso_fit1 %>%
  tidy() %>%
  filter(abs(estimate) > 0.001) %>%
  arrange(estimate %>% abs %>% desc)



## ----warning = FALSE--------------------------------------------------------------------------
# Get the predicted values from the lasso model 1
result1 <- predict(lasso_fit1, test1)
result1 %>% head()


## ---- message = FALSE, warning = FALSE--------------------------------------------------------
# Load the submission file
submission <- read_csv(file.path(file_path, "sample_submission.csv"))

# Exponential the sale_price because it is log transformed in preprocessing.
submission$SalePrice <- exp(result1$.pred)

# Write the submission file 
write.csv(submission, row.names = FALSE,
          "../output/lasso_regression_0point1.csv")


## ---------------------------------------------------------------------------------------------
# Get the variables which have high estimates in the LASSO model
vars <- lasso_fit1 %>%
  tidy() %>%
  filter(abs(estimate) > 0.001) %>%
  dplyr::arrange(estimate %>% abs %>% desc)

# Variables that have high estimate values 
vars[1:10,]
vars[11:20,]
vars[21:30,]
vars[31:40,]
vars[41:50,]

# NA dominants variables (pool_qc, fence, misc_feature, alley, fireplace_qu)

lasso_fit1 %>% 
  tidy() %>%
  filter(stringr::str_starts(term, "pool_qc"))

lasso_fit1 %>% 
  tidy() %>%
  filter(stringr::str_starts(term, "fence"))

lasso_fit1 %>% 
  tidy() %>%
  filter(stringr::str_starts(term, "misc_feature"))

lasso_fit1 %>% 
  tidy() %>%
  filter(stringr::str_starts(term, "alley"))

lasso_fit1 %>% 
  tidy() %>%
  filter(stringr::str_starts(term, "fireplace_qu"))


## ---- message = FALSE, warning = FALSE--------------------------------------------------------
library(caret)

# Select numeric data
all_data_numeric <- all_data %>% 
  select_if(is.numeric) %>% 
  select(-"sale_price")

# Select categorical data
all_data_character <- all_data %>% select_if(is.character)

# Find the missing value distribution
all_data_numeric %>% naniar::gg_miss_var()

# Find the missing value distribution
all_data_numeric %>% naniar::gg_miss_upset()

# lot_frontage : Linear feet of street connected to property
# garage_yr_blt : the year when garage was built.

# Get the data where there is a missing value in lot_frontage
all_data_numeric %>% filter(is.na(lot_frontage))

# Get the data where there is a missing value in garage_yr_blt
all_data_numeric %>% filter(is.na(garage_yr_blt)) %>% .$garage_area

all_data_numeric %>% filter(is.na(garage_yr_blt)) %>% .$garage_area %>% length()

# Find the cases where year_built == garage_yr_blt
all_data_numeric %>% 
  mutate(isGarageBuilt = case_when(garage_yr_blt == year_built ~ TRUE, TRUE ~ FALSE)) %>% 
  .$isGarageBuilt %>% 
  sum(na.rm = TRUE)

# Add the cases where year_remod_add == garage_yr_blt
all_data_numeric %>%
  mutate(isGarageBuilt = case_when(garage_yr_blt == year_built ~ TRUE, 
                                   garage_yr_blt == year_remod_add ~ TRUE,
                                   TRUE ~ FALSE)) %>% 
  .$isGarageBuilt %>%
  sum(na.rm = TRUE)

# No patterns in year_built
all_data_numeric %>% 
  filter(garage_yr_blt != year_built) %>%
  filter(garage_yr_blt != year_remod_add) %>% 
  select(garage_yr_blt, year_built, year_remod_add)

all_data_numeric %>% filter(is.na(garage_yr_blt)) %>% filter(garage_area != 0)

all_data$garage_yr_blt[2127] <- all_data$year_built[2127]



## ---- warning = FALSE-------------------------------------------------------------------------
# Apply mean imputation 
all_data_numeric_meanimpute <- sapply(X = all_data_numeric,
                                      FUN = impute_mean)

# There is no negative data
any(all_data_numeric_meanimpute < 0)

# summary of numeric data
summary(all_data_numeric)

# Run the preprocess function
BoxCox <- caret::preProcess(x = all_data_numeric_meanimpute,
                            method = c("BoxCox", "center", "scale"))

# See the overall result of preprocess 
BoxCox

# See the variables that needed to be transformed
BoxCox$method

# See the details of the BoxCox transformation
BoxCox$bc

# Unlist the boxcox results
bc <- BoxCox$bc %>% unlist()

# get index that have the name ending with lambda
idx <- names(bc) %>% stringr::str_ends("lambda")

# get the lambda result
bc[idx]

# it is close to 0 -> log transformation
# it is close to 0.5 -> square root transformation
# it is close to 2 -> square transformation

# lot_area, x1st_flr_sf, gr_liv_area, tot_rms_abv_grd -> log transformation
# lot_frontage -> square root transformation
# garage_yr_blt, year_built -> square transformation


## ---- warning = FALSE-------------------------------------------------------------------------
# This is a categorical (not ordinal)
hist(all_data_numeric$ms_sub_class)  # Categorical

# Histogram of lot_frontage
hist(all_data_numeric$lot_frontage)  # Square root Transformation needed 

ggplot(all_data_numeric, aes(x = lot_frontage, y = id)) + 
  geom_point()

all_data_numeric %>% filter(lot_frontage > 300)

# Histogram of lot_area 
hist(all_data_numeric$lot_area)      # Log-Transform needed

ggplot(all_data_numeric, aes(x = lot_area, y = id)) + 
  geom_point()

all_data_numeric %>% filter(lot_area > 100000)

# Histogram of overall_qual
hist(all_data_numeric$overall_qual)  # No transform

# Histogram of yr_sold
hist(all_data_numeric$yr_sold)       # No transform

# Histogram of year_built
hist(all_data_numeric$year_built)    # Square Transform

hist(2011 - all_data_numeric$year_built)    # Log-Transform

# Histogram of year_remod_add
hist(all_data_numeric$year_remod_add) # Square Transform

# Histogram of 1st floor surface
hist(all_data_numeric$x1st_flr_sf)    # Log Transform

ggplot(data = all_data_numeric) + 
  aes(x = x1st_flr_sf, y = id) +
  geom_point()

all_data_numeric %>% filter(x1st_flr_sf > 3500)

# Big houses
all_data_numeric[c(1299,2189,2550),]

# Histogram of ground living area
hist(all_data_numeric$gr_liv_area)     

# gr_liv_area = x1st_flr_sf + x2nd_flr_sf + low_qual_fin_sf : redundant in linear model 
identical(all_data_numeric$x1st_flr_sf + all_data_numeric$x2nd_flr_sf + all_data_numeric$low_qual_fin_sf, all_data_numeric$gr_liv_area)

# Histogram of total rooms above ground
hist(all_data_numeric$tot_rms_abv_grd)

# Histogram of garage_yr_built
hist(all_data_numeric$garage_yr_blt)

ggplot(data = all_data_numeric) + 
  aes(x = garage_yr_blt, y = id) +
  geom_point() 

all_data_numeric %>% 
  filter(garage_yr_blt > 2200) %>% 
  select(id, year_built, year_remod_add, garage_yr_blt)

all_data[2593,]$garage_yr_blt <- 2007 

# Histogram of mo_sold
hist(all_data_numeric$mo_sold)


## ---------------------------------------------------------------------------------------------
# Recipe for the tidymodels
housing_recipe2 <- all_data %>% 
    recipe(sale_price ~ .) %>%
    step_rm(id) %>%
    step_log(sale_price) %>%
    step_impute_mean(all_numeric_predictors()) %>%
    step_BoxCox(all_numeric_predictors()) %>%
    step_normalize(all_numeric_predictors()) %>%
    step_impute_mode(all_nominal()) %>%
    step_dummy(all_nominal()) %>%
    prep()

print(housing_recipe2) 


## ---------------------------------------------------------------------------------------------
# Preprocess the recipe1
all_data2 <- recipes::juice(housing_recipe2)


## ---------------------------------------------------------------------------------------------
# Get the index for trainset
train_index <- seq_len(nrow(train))

# Get the pre-processed trainset
train2 <- all_data2[train_index,]

# Get the pre-processed testset
test2 <- all_data2[-train_index,]


## ----message = FALSE, warning = FALSE---------------------------------------------------------
# Lasso model using glmnet engine with penalty 0.01 and mixture 1
lasso_model2 <- 
  linear_reg(penalty = 0.01, mixture = 1) %>%
  set_engine("glmnet")

# Run the model with formula sale_price ~ .(all other predictors) with dataset train1.
lasso_fit2 <- 
  lasso_model2 %>%
  fit(sale_price ~ ., data = train2)

# Find the estimates that are higher than 0.001.
lasso_fit2 %>%
  tidy() %>%
  filter(abs(estimate) > 0.001)


## ----warning = FALSE--------------------------------------------------------------------------
# Get the predicted values from the lasso model 1
result2 <- predict(lasso_fit2, test2)
result2 %>% head()


## ---- message = FALSE, warning = FALSE--------------------------------------------------------
# Load the submission file
submission <- read_csv(file.path(file_path, "sample_submission.csv"))

# Exponential the sale_price because it is log transformed in preprocessing.
submission$SalePrice <- exp(result2$.pred)

# Write the submission file 
write.csv(submission, row.names = FALSE,
          "../output/lasso_regression_0point2.csv")


## ---------------------------------------------------------------------------------------------
# Recipe for the tidymodels
housing_recipe3 <- all_data %>% 
    recipe(sale_price ~ .) %>%
    step_rm(id) %>%
    step_log(sale_price) %>%
    step_impute_median(all_numeric_predictors()) %>%
    step_BoxCox(all_numeric_predictors()) %>%
    step_normalize(all_numeric_predictors()) %>%
    step_impute_mode(all_nominal()) %>%
    step_dummy(all_nominal()) %>%
    prep()

print(housing_recipe3) 


## ---------------------------------------------------------------------------------------------
# Preprocess the recipe1
all_data3 <- recipes::juice(housing_recipe3)


## ---------------------------------------------------------------------------------------------
# Get the index for trainset
train_index <- seq_len(nrow(train))

# Get the pre-processed trainset
train3 <- all_data3[train_index,]

# Get the pre-processed testset
test3 <- all_data3[-train_index,]


## ----message = FALSE, warning = FALSE---------------------------------------------------------
# Lasso model using glmnet engine with penalty 0.01 and mixture 1
lasso_model3 <- 
  linear_reg(penalty = 0.01, mixture = 1) %>%
  set_engine("glmnet")

# Run the model with formula sale_price ~ .(all other predictors) with dataset train1.
lasso_fit3 <- 
  lasso_model3 %>%
  fit(sale_price ~ ., data = train3)

# Find the estimates that are higher than 0.001.
lasso_fit3 %>%
  tidy() %>%
  filter(abs(estimate) > 0.001)


## ----warning = FALSE--------------------------------------------------------------------------
# Get the predicted values from the lasso model 1
result3 <- predict(lasso_fit3, test3)
result3 %>% head()


## ---- message = FALSE, warning = FALSE--------------------------------------------------------
# Load the submission file
submission <- read_csv(file.path(file_path, "sample_submission.csv"))

# Exponential the sale_price because it is log transformed in preprocessing.
submission$SalePrice <- exp(result3$.pred)

# Write the submission file 
write.csv(submission, row.names = FALSE,
          "../output/lasso_regression_0point3.csv")


## ---------------------------------------------------------------------------------------------
# Remove NA dominant variables (pool_qc, fence, misc_feature, alley, id)
all <- all_data %>% 
  select(-c(pool_qc, fence, misc_feature, alley, id)) %>% 
  mutate_if(is.character, as.factor) %>% # Character -> Factor
  mutate(
    ms_sub_class = as.factor(ms_sub_class), # MSSubClass는 범주형 변수이므로 factor로 변환
    overall_qual = factor(overall_qual, order = T, levels = c(1,2,3,4,5,6,7,8,9,10)),
    overall_cond = factor(overall_cond, order = T, levels = c(1,2,3,4,5,6,7,8,9,10)))


## ---------------------------------------------------------------------------------------------
# Recipe for the tidymodels
housing_recipe4 <- all %>% 
    recipe(sale_price ~ .) %>%
    step_log(sale_price) %>%
    step_impute_mean(all_numeric_predictors()) %>%
    step_BoxCox(all_numeric_predictors()) %>%
    step_impute_mode(all_nominal()) %>%
    step_dummy(all_nominal()) %>%
    step_normalize(all_predictors()) %>%
    prep()

print(housing_recipe4) 


## ---------------------------------------------------------------------------------------------
# Preprocess the recipe4
all_data4 <- recipes::juice(housing_recipe4)


## ---------------------------------------------------------------------------------------------
# Get the index for trainset
train_index <- seq_len(nrow(train))

# Get the pre-processed trainset
train4 <- all_data4[train_index,]

# Get the pre-processed testset
test4 <- all_data4[-train_index,]


## ----message = FALSE, warning = FALSE---------------------------------------------------------
# Lasso model using glmnet engine with penalty 0.01 and mixture 1
lasso_model4 <- 
  linear_reg(penalty = 0.01, mixture = 1) %>%
  set_engine("glmnet")

# Run the model with formula sale_price ~ .(all other predictors) with dataset train1.
lasso_fit4 <- 
  lasso_model4 %>%
  fit(sale_price ~ ., data = train4)

# Find the estimates that are higher than 0.001.
lasso_fit4 %>%
  tidy() %>%
  filter(abs(estimate) > 0.001)


## ----warning = FALSE--------------------------------------------------------------------------
# Get the predicted values from the lasso model 4
result4 <- predict(lasso_fit4, test4)
result4 %>% head()


## ---- message = FALSE, warning = FALSE--------------------------------------------------------
# Load the submission file
submission <- read_csv(file.path(file_path, "sample_submission.csv"))

# Exponential the sale_price because it is log transformed in preprocessing.
submission$SalePrice <- exp(result4$.pred)

# Write the submission file 
write.csv(submission, row.names = FALSE,
          "../output/lasso_regression_0point4.csv")

