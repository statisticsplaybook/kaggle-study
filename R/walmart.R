library(tidyverse)
library(tidymodels)
library(lubridate)
library(skimr)
library(magrittr)
library(probably)
library(gt)

# Store - the store number
# Dept - the department number
# Date - the week
# Weekly_Sales -  sales for the given department in the given store
# IsHoliday - whether the week is a special holiday week
# temperature - average temperature in the region
# Fuel_Price - cost of fuel in the region
# MarkDown1-5 - anonymized data related to promotional markdowns that Walmart is running. MarkDown data is only available after Nov 2011, and is not available for all stores all the time. Any missing value is marked with an NA.
# CPI - the consumer price index
# Unemployment - the unemployment rate


# Super Bowl: 12-Feb-10, 11-Feb-11, 10-Feb-12, 8-Feb-13
# Labor Day: 10-Sep-10, 9-Sep-11, 7-Sep-12, 6-Sep-13
# Thanksgiving: 26-Nov-10, 25-Nov-11, 23-Nov-12, 29-Nov-13
# Christmas: 31-Dec-10, 30-Dec-11, 28-Dec-12, 27-Dec-13

train <- read_csv("./data/walmart/train.csv.zip")
test <- read_csv("./data/walmart/test.csv.zip")
stores <- read_csv("./data/walmart/stores.csv")
features <- read_csv("./data/walmart/features.csv.zip")

DataExplorer::plot_intro(train)


train <- train %>% janitor::clean_names() 
test <- test %>% janitor::clean_names() 
features <- features %>% janitor::clean_names()
stores <- stores %>% janitor::clean_names()


features <- features %>% left_join(stores, by = 'store') 

# mark_down의 결측치 비율
features %>% select(starts_with('mark_down')) %>%
    summarise(across(.fns = ~sum(is.na(.))/length(.)))

# 각 mark_down 별 평균 
features %>% select(starts_with('mark_down')) %>%
    summarise(across(.fns = ~mean(., na.rm = T)))


train <- left_join(train, features, by = c('store', 'date', 'is_holiday'))
test <- left_join(test, features, by = c('store', 'date', 'is_holiday'))

train <- train %>% select(-starts_with('mark'))
test <- test %>% select(-starts_with('mark'))


names(train)


# preprocessing 

walmart_recipe <- 
    recipe(weekly_sales ~ .,
           data = train) %>%
    step_date(date, features = c('month', 'year')) %>% 
    step_rm(date) %>%
    step_mutate(store = as.factor(store), 
                dept = as.factor(dept)) %>%
    step_dummy(all_nominal()) %>% 
    step_zv(all_predictors()) %>% 
    step_nzv(all_predictors()) %>% 
    step_normalize(all_numeric(), -all_outcomes()) %>% 
    prep()


baked_train <- walmart_recipe %>% 
    bake(train)

baked_test <- walmart_recipe %>% 
    bake(test)


set.seed(1234)

cv_data <- vfold_cv(baked_train, v = 5, strata = weekly_sales)



# random forest 

tune_spec <- rand_forest(
    mtry = tune(),
    trees = 1000,
    min_n = tune()
) %>%
    set_mode("regression") %>%
    set_engine("ranger")


params <- parameters(tune_spec) %>%
    finalize(baked_train)

rf_wflow <- workflow() %>%
    add_recipe(walmart_recipe) %>%
    add_model(tune_spec)


options(tidymodels.dark = TRUE)
cl <- makePSOCKcluster(6)
registerDoParallel(cl)


folds <- vfold_cv(baked_train, v = 5)

tuned <- tune_bayes(
    object = rf_wflow,
    resamples = folds,
    param_info = params,
    iter = 30,
    metrics =  metric_set(rmse, mape),
    initial = 10,
    control = control_bayes(
        verbose = TRUE,
        no_improve = 10,
        seed = 123
    )
)

show_best(tuned, 'rmse') %>% 
    select(1:7, 11) %>% 
    gt()


best_model <- select_best(tuned, "rmse")

final_model <- finalize_model(mod, best_model)
walmart_workflow <- xgboost_wflow %>% update_model(final_model)
xgb_fit <- fit(walmart_workflow, data = train)


pred <- 
    predict(xgb_fit, test) %>% 
    mutate(modelo = "XGBoost")

pred




# xgboost 

# grid search 

xgboost_model<-
    parsnip::boost_tree(
        mode = "regression",
        trees = 1000,
        min_n = tune(),
        tree_depth = tune(),
        ) %>%
    set_engine('xgboost', objective = "reg:squarederror",verbose=-1)


xgboost_params <-
    dials::parameters(
        min_n(),
        tree_depth()
    )


xgboost_grid <- dials::grid_max_entropy(
    xgboost_params,
    size = 30
)


xgboost_wf <- workflow() %>%
    add_model(xgboost_model) %>%
    add_formula(weekly_sales ~ .)


xgboost_tuned <- tune::tune_grid(
    object = xgboost_wf,
    resamples = cv_data,
    grid = xgboost_grid,
    metrics = yardstick::metric_set(rmse),
    control = tune::control_grid(verbose = FALSE)
)

xgboost_tuned %>%
    tune::show_best(metric = "rmse",n = 5)

xgboost_best_params <- xgboost_tuned %>%
    tune::select_best("rmse")

# Best parameters (tuned)
xgboost_model_final <- xgboost_model%>%
    tune::finalize_model(xgboost_best_params)

xgboost_model_final

train_prediction <- xgboost_model_final %>%
    parsnip::fit(weekly_sales ~., data = train) %>% 
    stats::predict(new_data = train) %>%
    dplyr::bind_cols(train)



test_prediction <- xgboost_model_final %>%
    parsnip::fit(weekly_sales ~., data = train) %>% 
    stats::predict(new_data = test) %>%
    dplyr::bind_cols(test)


test_prediction_submission <- xgboost_model_final %>%
    parsnip::fit(weekly_sales ~., data = train) %>% 
    stats::predict(new_data = test)

test_prediction_submission$.pred

subfile <- read_csv("./data/walmart/sampleSubmission.csv.zip")
subfile

subfile$Weekly_Sales <- test_prediction_submission$.pred

write.csv(subfile, row.names = FALSE,
          "./data/walmart/xgboost_base_gridsearch.csv")



# https://www.hfshr.xyz/posts/2020-05-23-tidymodel-notes/

mod <- boost_tree(
    trees = 1000,
    min_n = tune(),
    learn_rate = tune(),
    loss_reduction = tune(),
    sample_size = tune(),
    mtry = tune(),
    tree_depth = tune()
) %>%
    set_engine("xgboost") %>%
    set_mode("regression")


params <- parameters(mod) %>%
    finalize(train)

xgboost_wflow <- workflow() %>%
    add_recipe(walmart_recipe) %>%
    add_model(mod)


options(tidymodels.dark = TRUE)
cl <- makePSOCKcluster(6)
registerDoParallel(cl)

folds <- vfold_cv(train, v = 5)

tuned <- tune_bayes(
    object = xgboost_wflow,
    resamples = folds,
    param_info = params,
    iter = 30,
    metrics =  metric_set(rmse, mape),
    initial = 10,
    control = control_bayes(
        verbose = TRUE,
        no_improve = 10,
        seed = 123
    )
)

show_best(tuned, 'rmse') %>% 
    select(1:7, 11) %>% 
    gt()


best_model <- select_best(tuned, "rmse")

final_model <- finalize_model(mod, best_model)
walmart_workflow <- xgboost_wflow %>% update_model(final_model)
xgb_fit <- fit(walmart_workflow, data = train)


pred <- 
    predict(xgb_fit, test) %>% 
    mutate(modelo = "XGBoost")

pred





subfile <- read_csv("./data/walmart/sampleSubmission.csv.zip")
subfile

subfile$Weekly_Sales <- pred$.pred

write.csv(subfile, row.names = FALSE,
          "./data/walmart/xgboost_bayes_result.csv")



