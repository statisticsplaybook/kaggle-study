# makeit simple!
# group mean with random forest

## ----load_lib, message=FALSE, warning=FALSE, results='hide'----
library(tidymodels)
library(tidyverse)
library(stacks)
library(magrittr)
library(skimr)
library(knitr)
theme_set(theme_bw())


## ------------------------------------------
file_path <- "../input/walmart-recruiting-store-sales-forecasting/"
files <- list.files(file_path)
files


## ---- message=FALSE------------------------
train <- read_csv(file.path(file_path, "train.csv.zip")) %>% 
    janitor::clean_names()
test <- read_csv(file.path(file_path, "test.csv.zip")) %>% 
    janitor::clean_names()
features <- read_csv(file.path(file_path, "features.csv.zip")) %>% 
    janitor::clean_names()
stores <- read_csv(file.path(file_path, "stores.csv")) %>% 
    janitor::clean_names()
group_mean <- read_csv("./weekly_group_mean.csv") %>% 
    janitor::clean_names()

## ------------------------------------------
train$weekly_sales <- sign(train$weekly_sales) * (abs(train$weekly_sales))^(1/5)
all_data <- bind_rows(train, test)

# group mean combine
all_data$weekly_group_mean <- group_mean$weekly_group_mean

# store feature combine
all_data <- left_join(all_data, stores, by = c("store"= "store"))

# cpi and temp combine
all_data <- features %>% 
    select(-c(starts_with("mark"), is_holiday)) %>% 
    left_join(all_data, y = ., by = c("store"= "store",
                                      "date" = "date"))
all_data %>% head()

names(all_data)
dim(all_data)


## ------------------------------------------
walmart_recipe <- all_data %>% 
    recipe(weekly_sales ~ .) %>%
    step_mutate(year = lubridate::year(date)) %>%   
    step_mutate(month = lubridate::month(date)) %>%
    step_mutate(week = lubridate::week(date)) %>% 
    step_impute_linear(cpi, 
                       impute_with = imp_vars(store, dept, week, is_holiday)) %>% 
    step_impute_linear(unemployment, 
                       impute_with = imp_vars(store, dept, week, is_holiday)) %>% 
    step_rm(date, starts_with("date")) %>%
    prep(training = all_data)

print(walmart_recipe)


## ------------------------------------------
all_data2 <- juice(walmart_recipe)
all_data2 %>% dim()
all_data2 %>% head()

all_data2 %>% 
map_df(~sum(is.na(.))) %>% 
pivot_longer(cols = everything(), 
             names_to = "variable",
             values_to = "na_count")

## ------------------------------------------
train_index <- seq_len(nrow(train))
train2 <- all_data2[train_index,]
test2 <- all_data2[-train_index,]


## ------------------------------------------
set.seed(2021)
validation_split <- vfold_cv(train2, v = 3, 
                             strata = weekly_sales)

## Learning
cores <- parallel::detectCores() -1
cores

model_spec <- rand_forest(mtry = 8,
                          min_n = 21,
                          trees = 1000) %>% 
    set_engine("ranger", 
               num.threads = cores) %>% 
    set_mode("regression")

rf_workflow <- workflow() %>%
    add_model(model_spec) %>% 
    add_formula(weekly_sales ~ .)

rf_workflow

# for stacking
ctrl_res <- control_stack_resamples()

library(tictoc)
tic()
rf_fit_vfold <- 
    rf_workflow %>% 
    fit_resamples(weekly_sales ~ .,
        data = train2,
        resamples = validation_split,
        metrics = metric_set(mae),
        control = ctrl_res)
toc()

rf_fit_vfold %>% 
    collect_metrics()


## ------------------------------------------
lm_walmart_recipe <- 
    walmart_recipe %>% 
    step_mutate(store = as_factor(store),
                dept = as_factor(dept)) %>% 
    step_dummy(store, dept) %>% 
    prep(training = all_data)

all_data3 <- juice(lm_walmart_recipe)
all_data3 %>% dim()

train3 <- all_data3[train_index,]
test3 <- all_data3[-train_index,]

lm_spec <- linear_reg(penalty = tune(), 
                      mixture = 0) %>%
        set_engine("glmnet")

lambda_grid <- grid_regular(penalty(), levels = 10)

lm_workflow <- workflow() %>%
    add_model(lm_spec) %>% 
    add_formula(weekly_sales ~ .)

ctrl_grid <- control_stack_grid()

tic()

lm_fit_vfold <- 
    tune_grid(
        lm_workflow,
        data = train3,
        resamples = validation_split,
        grid = lambda_grid,
        metrics = metric_set(mae),
        control = ctrl_grid
    )
toc()

## ------------------------------------------
# stacking

walmart_stacking <- 
    stacks() %>% 
    add_candidates(rf_fit_vfold) %>% 
    add_candidates(lm_fit_vfold)

# print stacking
walmart_stacking

# as tibble
as_tibble(walmart_stacking) %>% head()

walmart_stacking %<>% 
    blend_predictions() %>% 
    fit_members()

walmart_stacking

# prediction
result <- predict(walmart_stacking, test2)
result %>% head()
result$.pred <- sign(result$.pred) * (abs(result$.pred)^5)

## ------------------------------------------
submission <- read_csv(file.path(file_path,
                                 "sampleSubmission.csv.zip"))
submission$Weekly_Sales <- result$.pred
write.csv(submission, row.names = FALSE,
          "stacking_april9.csv")
