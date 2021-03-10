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


# preprocessing 

walmart_recipe <- 
    recipe(weekly_sales ~ .,
           data = train) %>%
    step_date(date, features = c('month')) %>% 
    step_rm(date) %>%
    step_mutate(store = as.factor(store), 
                dept = as.factor(dept)) %>%
    step_dummy(all_nominal()) %>% 
    step_normalize(all_numeric(), -all_outcomes())


walmart_recipe %>% juice()


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



