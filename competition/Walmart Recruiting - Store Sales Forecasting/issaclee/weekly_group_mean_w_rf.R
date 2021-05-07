# makeit simple!
# group mean with random forest

## ----load_lib, message=FALSE, warning=FALSE, results='hide'----
library(tidymodels)
library(tidyverse)
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

# validation_split <- vfold_cv(train2, v = 5)
validation_split <- validation_split(train2, prop = 0.3, 
                                     strata = weekly_sales)

tune_spec <- rand_forest(mtry = tune(),
                         min_n = tune(),
                         trees = 1000) %>% 
    set_engine("ranger") %>% 
    set_mode("regression")

param_grid <- grid_random(finalize(mtry(), x = train2[,-1]),
                          min_n(),
                          size = 10)

workflow <- workflow() %>%
    add_model(tune_spec) %>% 
    add_formula(weekly_sales ~ .)

# library(doParallel)
# Cluster <- makeCluster(detectCores() - 1)
# registerDoParallel(Cluster)

# library(tictoc)
# tic()
# tune_result <- workflow %>% 
#     tune_grid(validation_split,
#               grid = param_grid,
#               metrics = metric_set(mae))
# toc()
# 731.4 sec elapsed

# tune_result %>% show_best(metric = "mae")
# # A tibble: 5 x 8
# mtry min_n .metric .estimator  mean     n std_err .config           
# <int> <int> <chr>   <chr>      <dbl> <int>   <dbl> <chr>             
#     1     8    21 mae     standard   0.129     1      NA Preprocessor1_Mod~
#     2     8    30 mae     standard   0.130     1      NA Preprocessor1_Mod~
#     3     8    37 mae     standard   0.130     1      NA Preprocessor1_Mod~
#     4     9    39 mae     standard   0.131     1      NA Preprocessor1_Mod~
#     5    10    29 mae     standard   0.131     1      NA Preprocessor1_Mod~

## ------------------------------------------
## metrics
# tune_result %>% 
#     collect_metrics()

# tune_best <- tune_result %>% select_best(metric = "mae")
tune_best <- tibble(mtry = 8,
                    min_n = 21)
tune_best$mtry
tune_best$min_n


## Learning
cores <- parallel::detectCores() -1
cores

rf_model <- 
    rand_forest(mtry = tune_best$mtry,
                min_n = tune_best$min_n,
                trees = 1000) %>% 
    set_engine("ranger", seed = 2021, num.threads = cores) %>% 
    set_mode("regression")

tic()
rf_fit <- 
    rf_model %>% 
    fit(weekly_sales ~ ., data = train2)
toc()

options(max.print = 10)
rf_fit

# prediction
result <- predict(rf_fit, test2)
result %>% head()
result

## ------------------------------------------
submission <- read_csv(file.path(file_path,
                                 "sampleSubmission.csv.zip"))
submission$Weekly_Sales <- result$.pred
write.csv(submission, row.names = FALSE,
          "group_mean_rf_april9.csv")
