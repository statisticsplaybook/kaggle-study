dir.create("study-presentation/park-mar-19/submissions")

library(tidyverse)
library(tidymodels)
library(magrittr)
library(here) # dir 
library(parallel) # multi-processing
library(knitr)


# Walmart in `Tidymodels`

## Walmart mothership Data

train <- read_csv(here("data/walmart/train.csv.zip"))
test <- read_csv(here("data/walmart/test.csv.zip"))

### Combine train and test into one: all-data

all_data <- bind_rows(train, test) %>%
    janitor::clean_names()
names(all_data)


## Benchmark model: lm but use only a single predictor

### Make recipe

### Set a benchmark model as only take store as a predictor
bench_recipe <- all_data %>% 
    recipe(weekly_sales ~ store) %>% 
    # step_date(date, features = c("month")) %>% 
    # step_rm(date)  %>% 
    step_mutate(
        store = as.factor(store)) %>%
        # dept = as.factor(dept)
        # , date_year = as.factor(date_year) %>% 
    # step_dummy(all_nominal(), -all_outcomes()) %>%
    prep()

bench_recipe %>% print()

benchmark <- juice(bench_recipe)

index <- seq_len(nrow(train))
train_bench <- benchmark[index,]
test_bench <- benchmark[-index,]

lm_benchmark_fit <- 
    linear_reg() %>% 
    set_engine("lm") %>%
    fit(weekly_sales ~ store, data = train_bench)

result_bench <- predict(lm_benchmark_fit, test_bench)

submission_bench <- read_csv("study-presentation/park-mar-19/submissions/sampleSubmission.csv")
submission_bench$Weekly_Sales <- result_bench$.pred
write.csv(submission_bench, 
          row.names = F, 
          "study-presentation/park-mar-19/submissions/benchmark_model.csv")


### Score is 20379.23667

## Dept model: lm with factor variables (store, dept)

### Make recipe

### Set a benchmark model as only take store as a predictor
strdept_recipe <- all_data %>% 
    recipe(weekly_sales ~ store + dept) %>% 
    # step_date(date, features = c("month")) %>% 
    # step_rm(date)  %>% 
    step_mutate(
        store = as.factor(store),
        dept = as.factor(dept)) %>%
    # , date_year = as.factor(date_year) %>% 
    # step_dummy(all_nominal(), -all_outcomes()) %>%
    prep()

strdept_recipe %>% print()

strdept <- juice(strdept_recipe)

index <- seq_len(nrow(train))
train_strdept <- strdept[index,]
test_strdept <- strdept[-index,]

lm_train_strdept_fit <- 
    linear_reg() %>% 
    set_engine("lm") %>%
    fit(weekly_sales ~ store + dept, data = train_strdept)

result_strdept <- predict(lm_train_strdept_fit, test_strdept)

submission_strdept <- read_csv("study-presentation/park-mar-19/submissions/sampleSubmission.csv")
submission_strdept$Weekly_Sales <- result_strdept$.pred
write.csv(submission_strdept, 
          row.names = F, 
          "study-presentation/park-mar-19/submissions/strdept_model.csv")

### Score is 11309.05222

## One-hot encoding model: lm with one-hot encoding (store)

### Make recipe

### Set a benchmark model as only take store as a predictor
strbi_recipe <- all_data %>% 
    recipe(weekly_sales ~ store) %>% 
    # step_date(date, features = c("month")) %>% 
    # step_rm(date)  %>% 
    step_mutate(
        store = as.factor(store)) %>%
    #    dept = as.factor(dept)) %>%
    # , date_year = as.factor(date_year) %>% 
    step_dummy(all_nominal(), -all_outcomes()) %>%
    prep()

strbi_recipe %>% print()

strbi <- juice(strbi_recipe)

index <- seq_len(nrow(train))
train_strbi <- strbi[index,]
test_strbi <- strbi[-index,]

lm_train_strbi_fit <- 
    linear_reg() %>% 
    set_engine("lm") %>%
    fit(weekly_sales ~ ., data = train_strbi)

result_strbi <- predict(lm_train_strbi_fit, test_strbi)

submission_strbi <- read_csv("study-presentation/park-mar-19/submissions/sampleSubmission.csv")
submission_strbi$Weekly_Sales <- result_strbi$.pred
write.csv(submission_strbi, 
          row.names = F, 
          "study-presentation/park-mar-19/submissions/strbi_model.csv")

### Score is 19199.86864: one-hot encoding makes predictions better

## Second One-hot encoding model: lm with one-hot encoding (store, dept)

### Make recipe

### Set a benchmark model as only take store as a predictor
strdeptbi_recipe <- all_data %>% 
    recipe(weekly_sales ~ store + dept) %>% 
    # step_date(date, features = c("month")) %>% 
    # step_rm(date)  %>% 
    step_mutate(
        store = as.factor(store),
        dept = as.factor(dept)) %>%
    # , date_year = as.factor(date_year) %>% 
    step_dummy(all_nominal(), -all_outcomes()) %>%
    prep()

strdeptbi_recipe %>% print()

strdeptbi <- juice(strdeptbi_recipe)

index <- seq_len(nrow(train))
train_strdeptbi <- strdeptbi[index,]
test_strdeptbi <- strdeptbi[-index,]

lm_train_strdeptbi_fit <- 
    linear_reg() %>% 
    set_engine("lm") %>%
    fit(weekly_sales ~ ., data = train_strdeptbi)

result_strdeptbi <- predict(lm_train_strdeptbi_fit, test_strdeptbi)

submission_strdeptbi <- read_csv("study-presentation/park-mar-19/submissions/sampleSubmission.csv")
submission_strdeptbi$Weekly_Sales <- result_strdeptbi$.pred
write.csv(submission_strdeptbi, 
          row.names = F, 
          "study-presentation/park-mar-19/submissions/strdeptbi_model.csv")

### Score is 11309.05222: one-hot encoding makes predictions better

## Time factor model: lm with factored months.

### Make recipe

library(lubridate)
year_data <- all_data %>%
    mutate(
        week = week(date),
        month = month(date),
        novDec= ifelse(month >=11, 1, 0),
        holiday = ifelse(is_holiday=="TRUE", 1, 0))

monthfactor_recipe <- year_data %>% 
    recipe(weekly_sales ~ store + dept + month) %>% 
    step_mutate(
        store = as.factor(store),
        dept = as.factor(dept),
        month = as.factor(month)) %>% 
    step_dummy(all_nominal(), -all_outcomes()) %>%
    prep()

monthfactor_recipe %>% print()

monthfactor <- juice(monthfactor_recipe)

train_monthfactor <- monthfactor[index,]
test_monthfactor <- monthfactor[-index,]

lm_train_monthfactor_fit <- 
    linear_reg() %>% 
    set_engine("lm") %>%
    fit(weekly_sales ~ ., data = train_monthfactor)

result_monthfactor <- predict(lm_train_monthfactor_fit, test_monthfactor)

submission_monthfactor <- read_csv("study-presentation/park-mar-19/submissions/sampleSubmission.csv")
submission_monthfactor$Weekly_Sales <- result_monthfactor$.pred
write.csv(submission_monthfactor, 
          row.names = F, 
          "study-presentation/park-mar-19/submissions/monthfactor_model.csv")

### Score is 11245.93266: Month slightly makes better the fits.
### How about novDec and holiday?

## Time factor model: lm with November-December dummy.

### Make recipe

novDec_recipe <- year_data %>% 
    recipe(weekly_sales ~ store + dept + novDec) %>% 
    step_mutate(
        store = as.factor(store),
        dept = as.factor(dept),
        novDec = as.factor(novDec)) %>% 
    step_dummy(all_nominal(), -all_outcomes()) %>%
    prep()

novDec_recipe %>% print()

novDec <- juice(novDec_recipe)

train_novDec <- novDec[index,]
test_novDec <- novDec[-index,]

lm_train_novDec_fit <- 
    linear_reg() %>% 
    set_engine("lm") %>%
    fit(weekly_sales ~ ., data = train_novDec)

result_novDec <- predict(lm_train_novDec_fit, test_novDec)


submission_novDec <- read_csv("study-presentation/park-mar-19/submissions/sampleSubmission.csv")
submission_novDec$Weekly_Sales <- result_novDec$.pred
write.csv(submission_novDec, 
          row.names = F, 
          "study-presentation/park-mar-19/submissions/novDec_model.csv")

### Score is 11262.45756: NovDec slightly makes better the fits, but less than month

## Time factor model: lm with holliday dummy.

### Make recipe

holiday_recipe <- year_data %>% 
    recipe(weekly_sales ~ store + dept + holiday) %>% 
    step_mutate(
        store = as.factor(store),
        dept = as.factor(dept),
        holiday = as.factor(holiday)) %>% 
    step_dummy(all_nominal(), -all_outcomes()) %>%
    prep()

holiday_recipe %>% print()

holiday <- juice(holiday_recipe)

train_holiday <- holiday[index,]
test_holiday <- holiday[-index,]

lm_train_holiday_fit <- 
    linear_reg() %>% 
    set_engine("lm") %>%
    fit(weekly_sales ~ ., data = train_holiday)

result_holiday <- predict(lm_train_holiday_fit, test_holiday)


submission_holiday <- read_csv("study-presentation/park-mar-19/submissions/sampleSubmission.csv")
submission_holiday$Weekly_Sales <- result_holiday$.pred
write.csv(submission_holiday, 
          row.names = F, 
          "study-presentation/park-mar-19/submissions/holiday_model.csv")

### Score is 11268.50315: holiday is not different from the other time vars.
### How about include them all?

## Time factor model: lm with month, novDec, and holliday dummy.

### Make recipe

alltime_recipe <- year_data %>% 
    recipe(weekly_sales ~ store + dept + month + novDec + holiday) %>% 
    step_mutate(
        store = as.factor(store),
        dept = as.factor(dept),
        month = as.factor(month),
        novDec = as.factor(novDec),
        holiday = as.factor(holiday)) %>% 
    step_dummy(all_nominal(), -all_outcomes()) %>%
    prep()

alltime_recipe %>% print()

alltime <- juice(alltime_recipe)

train_alltime <- alltime[index,]
test_alltime <- alltime[-index,]

lm_train_alltime_fit <- 
    linear_reg() %>% 
    set_engine("lm") %>%
    fit(weekly_sales ~ ., data = train_alltime)

result_alltime <- predict(lm_train_alltime_fit, test_alltime)


submission_alltime <- read_csv("study-presentation/park-mar-19/submissions/sampleSubmission.csv")
submission_alltime$Weekly_Sales <- result_alltime$.pred
write.csv(submission_alltime, 
          row.names = F, 
          "study-presentation/park-mar-19/submissions/alltime_model.csv")

### Score is 11247.44131. Really slightly improved. Do we need the all time vars?

# Additional Dataset: features and stores
features <- read_csv("data/walmart/features.csv.zip") %>%
    janitor::clean_names()
stores <- read_csv("data/walmart/stores.csv") %>%
    janitor::clean_names()

### Combine train and test into one: all-data

all_data_fs <- all_data %>% 
    left_join(features, 
              by =c("store", "date", "is_holiday"))
all_data_fs <- all_data_fs %>% 
    left_join(stores, by ="store")

all_data_fs <- all_data_fs %>%
    mutate(
        week = week(date),
        month = month(date),
        novDec= ifelse(month >=11, 1, 0),
        holiday = ifelse(is_holiday=="TRUE", 1, 0))

## benchmark + fuel_price model

### Make recipe

### Set a benchmark model as only take store as a predictor
fuelprice_recipe <- all_data_fs %>% 
    recipe(weekly_sales ~ store + dept + fuel_price) %>% 
    step_mutate(
        store = as.factor(store),
        dept = as.factor(dept)) %>%
    step_dummy(all_nominal(), -all_outcomes()) %>%
    prep()

fuelprice_recipe %>% print()

fuelprice <- juice(fuelprice_recipe)

train_fuelprice <- fuelprice[index,]
test_fuelprice <- fuelprice[-index,]

lm_train_fuelprice_fit <- 
    linear_reg() %>% 
    set_engine("lm") %>%
    fit(weekly_sales ~ ., data = train_fuelprice)

result_fuelprice <- predict(lm_train_fuelprice_fit, test_fuelprice)

submission_fuelprice <- read_csv("study-presentation/park-mar-19/submissions/sampleSubmission.csv")
submission_fuelprice$Weekly_Sales <- result_fuelprice$.pred
write.csv(submission_fuelprice, 
          row.names = F, 
          "study-presentation/park-mar-19/submissions/fuelprice_model.csv")

### Score is 11307.13964. How about normalizing it?

## benchmark + fuel_price model (normalized)

### Make recipe

normfp_recipe <- all_data_fs %>% 
    recipe(weekly_sales ~ store + dept + fuel_price) %>% 
    step_mutate(
        store = as.factor(store),
        dept = as.factor(dept)) %>%
    step_dummy(all_nominal(), -all_outcomes()) %>%
    step_normalize(all_predictors(), -all_nominal()) %>%
    prep()

normfp_recipe %>% print()

normfp <- juice(normfp_recipe)

train_normfp <- normfp[index,]
test_normfp <- normfp[-index,]

lm_train_normfp_fit <- 
    linear_reg() %>% 
    set_engine("lm") %>%
    fit(weekly_sales ~ ., data = train_normfp)

result_normfp <- predict(lm_train_normfp_fit, test_normfp)

submission_normfp <- read_csv("study-presentation/park-mar-19/submissions/sampleSubmission.csv")
submission_normfp$Weekly_Sales <- result_normfp$.pred
write.csv(submission_normfp, 
          row.names = F, 
          "study-presentation/park-mar-19/submissions/normfp_model.csv")

### Score is 11307.13964. Identical. Why? Normalizing does not affect the distribution itself.

## Then, add time variables in this model.

## benchmark + fuel_price model (normalized) + time vars.

### Make recipe

## benchmark + fuel_price model (normalized) + time vars.

### Make recipe

normfp_time_recipe <- all_data_fs %>% 
    recipe(weekly_sales ~ store + dept + fuel_price + month + novDec + holiday) %>% 
    step_mutate(
        store = as.factor(store),
        dept = as.factor(dept),
        month = as.factor(month),
        novDec = as.factor(novDec),
        holiday = as.factor(holiday)
    ) %>%
    step_dummy(all_nominal(), -all_outcomes()) %>%
    step_normalize(all_predictors(), -all_nominal()) %>%
    prep()

normfp_time_recipe %>% print()

normfp <- juice(normfp_recipe)

train_normfp <- normfp[index,]
test_normfp <- normfp[-index,]

lm_train_normfp_fit <- 
    linear_reg() %>% 
    set_engine("lm") %>%
    fit(weekly_sales ~ ., data = train_normfp)

result_normfp <- predict(lm_train_normfp_fit, test_normfp)

submission_normfp <- read_csv("study-presentation/park-mar-19/submissions/sampleSubmission.csv")
submission_normfp$Weekly_Sales <- result_normfp$.pred
write.csv(submission_normfp, 
          row.names = F, 
          "study-presentation/park-mar-19/submissions/normfp_model.csv")


## benchmark + fuel_price model (normalized) + time vars.

### Make recipe

normfp_time_recipe <- all_data_fs %>% 
    recipe(weekly_sales ~ store + dept + fuel_price + month + novDec + holiday) %>% 
    step_mutate(
        store = as.factor(store),
        dept = as.factor(dept),
        month = as.factor(month),
        novDec = as.factor(novDec),
        holiday = as.factor(holiday)
    ) %>%
    step_dummy(all_nominal(), -all_outcomes()) %>%
    step_normalize(all_predictors(), -all_nominal()) %>%
    prep()

normfp_time_recipe %>% print()

normfp_time <- juice(normfp_time_recipe)

train_normfp_time <- normfp_time[index,]
test_normfp_time <- normfp_time[-index,]

lm_train_normfp_time_fit <- 
    linear_reg() %>% 
    set_engine("lm") %>%
    fit(weekly_sales ~ ., data = train_normfp_time)

result_normfp_time <- predict(lm_train_normfp_time_fit, test_normfp_time)

submission_normfp_time <- read_csv("study-presentation/park-mar-19/submissions/sampleSubmission.csv")
submission_normfp_time$Weekly_Sales <- result_normfp_time$.pred
write.csv(submission_normfp_time, 
          row.names = F, 
          "study-presentation/park-mar-19/submissions/normfp_time_model.csv")

### Score is 11248.84665.

## benchmark + cpi model (normalized) + time vars.

### Make recipe
## benchmark + fuel_price model (normalized) + time vars.

### Make recipe

### As cpi has some NAs, we have several options.
### First, remove the NAs.
### Second, impute values for the NAs.

## first, drop NAs.

fpcpi_nona_recipe <- all_data_fs %>% 
    recipe(weekly_sales ~ store + dept + fuel_price + cpi + 
               month + novDec + holiday) %>% 
    step_naomit(cpi) %>%
    step_mutate(
        store = as.factor(store),
        dept = as.factor(dept),
        month = as.factor(month),
        novDec = as.factor(novDec),
        holiday = as.factor(holiday)
    ) %>%
    step_dummy(all_nominal(), -all_outcomes()) %>%
    step_normalize(all_predictors(), -all_nominal()) %>%
    prep()

fpcpi_nona_recipe %>% print()

fpcpi_nona <- juice(fpcpi_nona_recipe)

train_fpcpi_nona <- fpcpi_nona[index,]
test_fpcpi_nona <- fpcpi_nona[-index,]

lm_train_fpcpi_nona_fit <- 
    linear_reg() %>% 
    set_engine("lm") %>%
    fit(weekly_sales ~ ., data = train_fpcpi_nona)

result_fpcpi_nona <- predict(lm_train_fpcpi_nona_fit, test_fpcpi_nona)

submission_fpcpi_nona <- read_csv("study-presentation/park-mar-19/submissions/sampleSubmission.csv")
submission_fpcpi_nona$Weekly_Sales <- result_fpcpi_nona$.pred
write.csv(submission_fpcpi_nona, 
          row.names = F, 
          "study-presentation/park-mar-19/submissions/fpcpi_nona_model.csv")

## Score is 21924.30518. If you just remove the NAs, score gets worse.

## Impute:

### Mean impute for continuous

fpcpi_mean_recipe <- all_data_fs %>% 
    recipe(weekly_sales ~ store + dept + fuel_price + cpi + 
               month + novDec + holiday) %>% 
    step_mutate(
        store = as.factor(store),
        dept = as.factor(dept),
        month = as.factor(month),
        novDec = as.factor(novDec),
        holiday = as.factor(holiday)
    ) %>%
    step_meanimpute(cpi) %>%
    step_dummy(all_nominal(), -all_outcomes()) %>%
    step_normalize(all_predictors(), -all_nominal()) %>%
    prep()

fpcpi_mean_recipe %>% print()

fpcpi_mean <- juice(fpcpi_mean_recipe)

## compare
summary(fpcpi_nona$cpi)
summary(fpcpi_mean$cpi)

train_fpcpi_mean <- fpcpi_mean[index,]
test_fpcpi_mean <- fpcpi_mean[-index,]

lm_train_fpcpi_mean_fit <- 
    linear_reg() %>% 
    set_engine("lm") %>%
    fit(weekly_sales ~ ., data = train_fpcpi_mean)

result_fpcpi_mean <- predict(lm_train_fpcpi_mean_fit, test_fpcpi_mean)

submission_fpcpi_mean <- read_csv("study-presentation/park-mar-19/submissions/sampleSubmission.csv")
submission_fpcpi_mean$Weekly_Sales <- result_fpcpi_mean$.pred
write.csv(submission_fpcpi_mean, 
          row.names = F, 
          "study-presentation/park-mar-19/submissions/fpcpi_mean_model.csv")

### Score is 11308.56202.

### Median impute for continuous

fpcpi_median_recipe <- all_data_fs %>% 
    recipe(weekly_sales ~ store + dept + fuel_price + cpi + 
               month + novDec + holiday) %>% 
    step_mutate(
        store = as.factor(store),
        dept = as.factor(dept),
        month = as.factor(month),
        novDec = as.factor(novDec),
        holiday = as.factor(holiday)
    ) %>%
    step_medianimpute(cpi) %>%
    step_dummy(all_nominal(), -all_outcomes()) %>%
    step_normalize(all_predictors(), -all_nominal()) %>%
    prep()

fpcpi_median_recipe %>% print()

fpcpi_median <- juice(fpcpi_median_recipe)

## compare
summary(fpcpi_nona$cpi)
summary(fpcpi_mean$cpi)
summary(fpcpi_median$cpi)

train_fpcpi_median <- fpcpi_median[index,]
test_fpcpi_median <- fpcpi_median[-index,]

lm_train_fpcpi_median_fit <- 
    linear_reg() %>% 
    set_engine("lm") %>%
    fit(weekly_sales ~ ., data = train_fpcpi_median)

result_fpcpi_median <- predict(lm_train_fpcpi_median_fit, test_fpcpi_median)

submission_fpcpi_median <- read_csv("study-presentation/park-mar-19/submissions/sampleSubmission.csv")
submission_fpcpi_median$Weekly_Sales <- result_fpcpi_median$.pred
write.csv(submission_fpcpi_median, 
          row.names = F, 
          "study-presentation/park-mar-19/submissions/fpcpi_median_model.csv")

### Score is 11322.97579. Mean impute is slightly better to predict.

### Imputation via K-Nearest Neighbors
####  impute the missing values for the variables using a nearest neighbors 
####  model with all predictors.

fpcpi_kmean_recipe <- all_data_fs %>% 
    recipe(weekly_sales ~ store + dept + fuel_price + cpi + 
               month + novDec + holiday) %>% 
    step_mutate(
        store = as.factor(store),
        dept = as.factor(dept),
        month = as.factor(month),
        novDec = as.factor(novDec),
        holiday = as.factor(holiday)
    ) %>%
    step_dummy(all_nominal(), -all_outcomes()) %>%
    step_normalize(all_predictors(), -all_nominal()) %>%
    step_knnimpute(all_predictors()) %>%
    prep()

fpcpi_kmean_recipe %>% print()

fpcpi_kmean <- juice(fpcpi_kmean_recipe)

## compare
summary(fpcpi_nona$cpi)
summary(fpcpi_mean$cpi)
summary(fpcpi_kmean$cpi)

train_fpcpi_kmean <- fpcpi_kmean[index,]
test_fpcpi_kmean <- fpcpi_kmean[-index,]

lm_train_fpcpi_kmean_fit <- 
    linear_reg() %>% 
    set_engine("lm") %>%
    fit(weekly_sales ~ ., data = train_fpcpi_kmean)

result_fpcpi_kmean <- predict(lm_train_fpcpi_kmean_fit, test_fpcpi_kmean)

submission_fpcpi_kmean <- read_csv("study-presentation/park-mar-19/submissions/sampleSubmission.csv")
submission_fpcpi_kmean$Weekly_Sales <- result_fpcpi_kmean$.pred
write.csv(submission_fpcpi_kmean, 
          row.names = F, 
          "study-presentation/park-mar-19/submissions/fpcpi_kmean_model.csv")



step_impute_linear()

step_unknown()




### random Forest `worflow`

```{r}
cores <- parallel::detectCores() -1
```


```{r}
rf_model <- 
    rand_forest(mtry = tune(), min_n = tune(), trees = 1000) %>% 
    set_engine("ranger", seed = 1234, num.threads = cores) %>% 
    set_mode("regression")
```

```{r}
walmart_wflow <- workflow() %>% 
    add_model(rf_model) %>% 
    add_recipe(walmart_recipe)

walmart_wflow
```

#### hyperparameter tuning 
```{r}
set.seed(1234)

rf_result <- walmart_wflow %>% 
    tune_grid(walmart_validation,
              grid = 5,
              control = control_grid(save_pred = TRUE),
              metrics = metric_set(rmse))
```

```{r}
rf_result %>% show_best()
```

```{r}
rf_best <- 
    rf_result %>% 
    select_best(metric = "rmse")

rf_best
```

#### best fit with tuned hyperparameters
```{r}
### the last model
rf_best_model <- 
    rand_forest(mtry = rf_best$mtry, min_n = rf_best$min_n, trees = 1000) %>% 
    set_engine("ranger", seed = 1234, 
               num.threads = cores,
               importance = "impurity") %>% 
    set_mode("regression")


final_rf_wflow <- walmart_wflow %>% 
    update_model(rf_best_model)

set.seed(1234)
rf_best_fit <- final_rf_wflow %>% 
    fit(train)
```

```{r}
rf_best_fit
```

## submit prediction
```{r}
subfile <- read_csv(here("walmart/sampleSubmission.csv.zip"))


subfile$Weekly_Sales <- rf_best_fit %>% 
    predict(test) %>% 
    select(.pred) %>% unlist()


subfile


write.csv(subfile, row.names = FALSE,
          here("walmart/tuning-rf.csv"))

```

```{r}
last_week <- c(20660.01047 , 20238.71579)
today <- c(3672.12956, 3536.56464)

scores <- rbind(last_week, today) 
colnames(scores) <- c("private", "public" )
scores
```

- last week's score : 20660.01047 / 20238.71579
- today's score : 3672.12956 / 3536.56464



## Attachment 1 : CV with decision tree in `tidymodels`

```{r}
set.seed(1234)
walmart_split <- initial_split(train, prop = 0.7, strata = is_holiday)

walmart_split
```
```{r}
train_data <- walmart_split %>% training()
val_data <- walmart_split %>% testing()
```

```{r}
tune_spec <-
    decision_tree(
        cost_complexity = tune(),
        tree_depth = tune()
    ) %>% 
    set_engine("rpart") %>%     
    set_mode("regression")


tune_spec
```

```{r}
tree_grid <- grid_regular(cost_complexity(),
                          tree_depth(),
                          levels = 3
)

tree_grid %>% head() %>% kable()

# tree_grid %>% count(cost_complexity)
# tree_grid %>% count(tree_depth)
```

```{r}
set.seed(1234)
walmart_folds <- vfold_cv(train, v= 5, strata = is_holiday)

# walmart_folds

tree_wf <- workflow() %>% 
    add_model(tune_spec) %>% 
    add_recipe(walmart_recipe)
```

```{r}
walmart_treefit <- 
    tree_wf %>% 
    tune_grid(
        resamples = walmart_folds,
        grid = tree_grid
    )


walmart_treefit
```

```{r}
walmart_treefit %>% 
    collect_metrics() %>% 
    head(12) %>% 
    kable()

```

```{r}
best_tree <- walmart_treefit %>% 
    select_best("rmse")

best_tree
```

```{r}
final_walmart_tree <-
    tree_wf %>% 
    finalize_workflow(best_tree)

final_walmart_tree
```

```{r}
walmart_treefit2 <- 
    final_walmart_tree %>% 
    fit(data = train_data)
```

```{r}
walmart_treefit2 %>% 
    predict(val_data) %>% 
    bind_cols(val_data) %>%  select(weekly_sales,.pred) %>% 
    metrics(truth = weekly_sales, estimate = .pred)
```


##  Attachment 2 :oridinary linear model fitting



```{r}
walmart_recipe2 <- train_data %>% 
    recipe(weekly_sales ~ .) %>% 
    step_date(date, features = c("month")) %>% 
    step_rm(date)  %>% 
    step_mutate(
        store = as.factor(store),
        dept = as.factor(dept)
        # , date_year = as.factor(date_year)
    ) %>% 
    step_dummy(all_nominal(), -all_outcomes()) %>% prep()

walmart_recipe2 %>% print()
```

```{r}
lm_model <- 
    linear_reg() %>% 
    set_engine("lm") %>% 
    set_mode("regression")
```


```{r}
walmart_wflow2 <- workflow() %>% 
    add_model(lm_model) %>% 
    add_recipe(walmart_recipe2)

walmart_wflow2
```

```{r}
walmart_lmfit <- walmart_wflow2 %>% 
    fit(train_data)
```



```{r}
walmart_lmfit %>% 
    predict(val_data) %>% 
    bind_cols(val_data) %>%  select(weekly_sales,.pred) %>% 
    metrics(truth = weekly_sales, estimate = .pred)
```

### ??

```{r}
walmart_lmfit %>% tidy()
```

```{r}
untidy_fit <- lm(weekly_sales ~ ., data = juice(walmart_recipe2))
```

```{r}
predict(untidy_fit, 
        newdata = 
            bake(walmart_recipe2, new_data = val_data)) %>% 
    bind_cols(val_data) %>%
    select(weekly_sales, '...1') %>%
    metrics(truth = weekly_sales,
            estimate = '...1')
```

```{r}
untidy_fit %>% summary()
```

- reference :
    - [A Gentle Introduction to tidymodels](https://rviews.rstudio.com/2019/06/19/a-gentle-intro-to-tidymodels/)
- [왜 당근(caret)은 안되고 새로운 신상 당근(Tidymodel)인가?](https://statkclee.github.io/ds-authoring/ds-why-tidymodels.html#/predictive-model)
                                             - [Tidymodels' GET STARTED](https://www.tidymodels.org/start/)
