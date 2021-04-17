library(tidymodels)
library(tidyverse)
library(stacks)
library(magrittr)
library(skimr)
library(knitr)
theme_set(theme_bw())

## ------------------------------------------
file_path <- "../input/actuarial-loss-estimation/"
files <- list.files(file_path)
files

## ---- message=FALSE------------------------
train <- read_csv(file.path(file_path, "train.csv")) %>% 
    janitor::clean_names()
test <- read_csv(file.path(file_path, "test.csv")) %>% 
    janitor::clean_names()
topic_info <- read_csv(file.path(file_path, "topic_info.csv")) %>% 
    janitor::clean_names()
price_index <- read_csv(file.path(file_path, "oecd_price_index.csv")) %>% 
    janitor::clean_names() %>% select(time, value)
emb_train <- read_csv(file.path(file_path, "embeddings_train_20.csv"),
                      col_names = F) %>%
    janitor::clean_names()
emb_test <- read_csv(file.path(file_path, "embeddings_test_20.csv"),
                     col_names = F) %>%
    janitor::clean_names()
emb_all <- bind_rows(emb_train, emb_test)

## Random forest------------------------------------------
all_data <- bind_rows(train, test)
all_data <- bind_cols(all_data, emb_all)
rm(list = c("emb_train", "emb_test", "emb_all"))
all_data$claim_description <- topic_info$keywords
all_data$dominant_topic <- topic_info$dominant_topic
all_data$topic_perc_contrib <- topic_info$topic_perc_contrib
all_data$gender[all_data$gender == "U"] <- NA
all_data$marital_status[all_data$marital_status == "U"] <- NA
all_data$weekly_wages[all_data$weekly_wages > 2000] <- 2000
all_data$hours_worked_per_week[all_data$hours_worked_per_week > 70] <- 70

price_index %<>% 
    mutate(time = paste0(time, "-01"),
           time = lubridate::ymd(time),
           year = lubridate::year(time),
           month = lubridate::month(time)) %>%
    select(year, month, value) 

all_data %<>%
    mutate(year = lubridate::year(date_time_of_accident),
           month = lubridate::month(date_time_of_accident)) %>%
    left_join(y = price_index,
              by = c("year" = "year",
                     "month" = "month"))
all_data %<>%
    mutate(unit_dollar_init = initial_incurred_calims_cost / value) %>%
    mutate(unit_dollar_ult = ultimate_incurred_claim_cost / value) %>%
    select(everything(), ultimate_incurred_claim_cost)

all_data$ultimate_incurred_claim_cost %>% hist()
all_data %>% head()
## ------------------------------------------

rf_actuarial_recipe <- all_data %>% 
    recipe(ultimate_incurred_claim_cost ~ .) %>%
    step_log(ultimate_incurred_claim_cost, offset = 1) %>%
    step_log(initial_incurred_calims_cost, offset = 1) %>%
    ## ------------------------------------------
    step_rm(unit_dollar_init, unit_dollar_ult) %>% 
    step_mutate(rp_year = lubridate::year(date_reported)) %>%
    step_mutate(rp_month = lubridate::month(date_reported)) %>%
    step_mutate(rp_week = lubridate::week(date_reported)) %>% 
    step_mutate(ac_year = lubridate::year(date_time_of_accident)) %>%
    step_mutate(ac_month = lubridate::month(date_time_of_accident)) %>%
    step_mutate(ac_week = lubridate::week(date_time_of_accident)) %>% 
    ## ------------------------------------------
    step_modeimpute(marital_status) %>%
    step_modeimpute(gender) %>%
    step_mutate(has_child = ifelse(dependent_children > 0, "yes", "no")) %>%
    step_mutate(wage_per_hour = weekly_wages / hours_worked_per_week) %>%
    step_mutate(wage_per_day = weekly_wages / days_worked_per_week) %>% 
    step_mutate(dependent_total = dependent_children + dependents_other) %>%
    ## ------------------------------------------
    step_mutate(body_back = sum(str_detect(claim_description, "back"))) %>% 
    step_mutate(body_wrist = sum(str_detect(claim_description, "wrist"))) %>% 
    step_mutate(body_head = sum(str_detect(claim_description, "head"))) %>% 
    step_mutate(body_teeth = sum(str_detect(claim_description, c("teeth", "head")))) %>% 
    step_mutate(body_neck = sum(str_detect(claim_description, "neck"))) %>% 
    step_mutate(body_shoulder = sum(str_detect(claim_description, "shoulder"))) %>% 
    step_mutate(body_finger = sum(str_detect(claim_description, "finger"))) %>% 
    step_mutate(body_hand = sum(str_detect(claim_description, "hand"))) %>% 
    step_mutate(body_foot = sum(str_detect(claim_description, "foot"))) %>% 
    step_mutate(body_eye = sum(str_detect(claim_description, c("eye", "eyes")))) %>% 
    step_mutate(body_knee = sum(str_detect(claim_description, c("knees", "knee")))) %>% 
    step_mutate(body_rib = sum(str_detect(claim_description, c("rib", "ribs")))) %>% 
    ## ------------------------------------------
    step_rm(date_reported, date_time_of_accident, claim_number, claim_description) %>% 
    step_integer(all_nominal()) %>%
    step_center(all_numeric(), -all_outcomes(), -wage_per_hour) %>% 
    prep(training = all_data)

print(rf_actuarial_recipe)

## ------------------------------------------
all_data_rf2 <- juice(rf_actuarial_recipe)
all_data_rf2 %>% dim()
all_data_rf2 %>% names()
# all_data_rf %>% 
# map_df(~sum(is.na(.))) %>% 
#     pivot_longer(cols = everything(), 
#                  names_to = "variable",
#                  values_to = "na_count") %>% 
#     print(n = 100)

## ------------------------------------------
train_index <- seq_len(nrow(train))
train_rf <- all_data_rf2[train_index,]
test_rf <- all_data_rf2[-train_index,]

## ------------------------------------------
train_rf %>% 
    select_if(is.numeric) %>% 
    drop_na() %>% 
    cor() %>% as.data.frame() %>% 
    rownames_to_column(var = "variables") %>% 
    select(variables, ultimate_incurred_claim_cost) %>% 
    arrange(desc(abs(ultimate_incurred_claim_cost)))

## ------------------------------------------
set.seed(2021)
cores <- parallel::detectCores() -1

validation_split <- vfold_cv(train, v=5, strata = ultimate_incurred_claim_cost)

rf_spec <- rand_forest(mtry = 17,
                       min_n = 21,
                       trees = 1000) %>% 
    set_engine("ranger", seed = 2021,
               importance = "none",
               verbose = TRUE,
               num.threads = cores) %>%
    set_mode("regression")

# rf_parm_set <- parameters(rf_spec) %>%
#     finalize(mtry(), x = train2[,-1])

# param_grid <- grid_latin_hypercube(
#   finalize(mtry(), x = train2[,-1]),
#   min_n(), size = 10) %>% filter(min_n > 15 & mtry > 8)
# 25, 26
# 34, 15
# 17, 21 previous
# param_grid <- tibble(mtry = c(17, 25, 29),
#                      min_n = c(21, 26, 25))
ctrl_res <- control_stack_resamples()

rf_workflow <- workflow() %>%
    add_recipe(rf_actuarial_recipe) %>% 
    add_model(rf_spec)

library(tictoc)
tic()
rf_fit_vfold <- 
    rf_workflow %>% 
    fit_resamples(ultimate_incurred_claim_cost ~ .,
                  data = train,
                  resamples = validation_split,
                  metrics = metric_set(rmse),
                  control = ctrl_res)
toc()

rf_fit_vfold %>% 
    collect_metrics()

## Ridge ------------------------------------------
# train$ultimate_incurred_claim_cost %>% log() %>% hist()

lg_actuarial_recipe <- all_data %>% 
    recipe(ultimate_incurred_claim_cost ~ .) %>%
    step_log(ultimate_incurred_claim_cost, offset = 1) %>%
    step_log(initial_incurred_calims_cost, offset = 1) %>%
    step_rm(var(starts_with("x"))) %>% 
    ## ------------------------------------------
    step_mutate(rp_year = lubridate::year(date_reported)) %>%
    step_mutate(rp_month = lubridate::month(date_reported)) %>%
    step_mutate(rp_week = lubridate::week(date_reported)) %>% 
    step_mutate(ac_year = lubridate::year(date_time_of_accident)) %>%
    step_mutate(ac_month = lubridate::month(date_time_of_accident)) %>%
    step_mutate(ac_week = lubridate::week(date_time_of_accident)) %>% 
    ## ------------------------------------------
    step_modeimpute(marital_status, gender) %>%
    step_mutate(has_child = ifelse(dependent_children > 0, 1, 0)) %>%
    step_mutate(wage_per_day = weekly_wages / days_worked_per_week) %>% 
    step_mutate(dependent_total = dependent_children + dependents_other) %>%
    ## ------------------------------------------
    step_mutate(body_back = sum(str_detect(claim_description, "back"))) %>% 
    step_mutate(body_wrist = sum(str_detect(claim_description, "wrist"))) %>% 
    step_mutate(body_head = sum(str_detect(claim_description, "head"))) %>% 
    step_mutate(body_teeth = sum(str_detect(claim_description, c("teeth", "head")))) %>% 
    step_mutate(body_neck = sum(str_detect(claim_description, "neck"))) %>% 
    step_mutate(body_shoulder = sum(str_detect(claim_description, "shoulder"))) %>% 
    step_mutate(body_finger = sum(str_detect(claim_description, "finger"))) %>% 
    step_mutate(body_hand = sum(str_detect(claim_description, "hand"))) %>% 
    step_mutate(body_foot = sum(str_detect(claim_description, "foot"))) %>% 
    step_mutate(body_eye = sum(str_detect(claim_description, c("eye", "eyes")))) %>% 
    step_mutate(body_knee = sum(str_detect(claim_description, c("knees", "knee")))) %>% 
    step_mutate(body_rib = sum(str_detect(claim_description, c("rib", "ribs")))) %>% 
    ## ------------------------------------------
    step_mutate(symp_concussion = sum(str_detect(claim_description, c("concussion")))) %>%
    step_mutate(symp_injury = sum(str_detect(claim_description, c("injury")))) %>%
    step_mutate(symp_infection = sum(str_detect(claim_description, c("infection", "infected")))) %>%
    step_mutate(symp_fracture = sum(str_detect(claim_description, c("fractured", "fracture")))) %>%
    step_mutate(symp_strain = sum(str_detect(claim_description, c("strain", "strained")))) %>%
    step_mutate(symp_bruised = sum(str_detect(claim_description, c("bruised", "bruise")))) %>%
    step_mutate(symp_cut = sum(str_detect(claim_description, c("cut", "cutting", "laceration")))) %>%
    ## context------------------------------------------
    step_mutate(context_struck = sum(str_detect(claim_description, c("struck")))) %>%
    step_mutate(context_fell = sum(str_detect(claim_description, c("fell", "slipped")))) %>%
    step_mutate(context_twisted = sum(str_detect(claim_description, c("twist", "twisted")))) %>%
    step_mutate(context_caught = sum(str_detect(claim_description, c("caught")))) %>%
    step_mutate(context_repetative = sum(str_detect(claim_description, c("repetative")))) %>%
    step_mutate(context_floor = sum(str_detect(claim_description, c("floor")))) %>%
    step_mutate(context_hammer = sum(str_detect(claim_description, c("hammer")))) %>%
    step_mutate(context_metal = sum(str_detect(claim_description, c("metal", "iron", "steel")))) %>%
    step_mutate(context_lift = sum(str_detect(claim_description, c("lifting")))) %>%
    step_mutate(context_vehicle = sum(str_detect(claim_description, c("vehicle", "car")))) %>%
    step_mutate(context_accident = sum(str_detect(claim_description, c("accident", "collision", "crush", "crushed")))) %>%
    step_mutate(context_severe = sum(str_detect(claim_description, c("severe")))) %>%
    step_mutate(context_severe = sum(str_detect(claim_description, c("severe")))) %>%
    ## interaction------------------------------------------
    step_mutate(footcut = symp_cut * body_foot) %>%
    step_mutate(footstrain = symp_strain * body_foot) %>%
    step_mutate(footfracture = symp_fracture * body_foot) %>%
    step_mutate(footbruised = symp_bruised * body_foot) %>%
    step_mutate(fingercut      = symp_cut      * body_finger) %>%
    step_mutate(fingerstrain   = symp_strain   * body_finger) %>%
    step_mutate(fingerfracture = symp_fracture * body_finger) %>%
    step_mutate(fingerbruised  = symp_bruised  * body_finger) %>%
    step_mutate(backcut      = symp_cut      * body_back) %>%
    step_mutate(backstrain   = symp_strain   * body_back) %>%
    step_mutate(backfracture = symp_fracture * body_back) %>%
    step_mutate(backbruised  = symp_bruised  * body_back) %>%
    ## ------------------------------------------
    step_rm(date_reported, date_time_of_accident, claim_number, claim_description) %>%
    step_mutate(dominant_topic = as.factor(dominant_topic)) %>% 
    step_dummy(all_nominal()) %>%
    step_nzv(all_predictors(), unique_cut = 1) %>% 
    step_normalize(all_numeric(), -all_outcomes()) %>% 
    # step_corr(all_predictors(), threshold = 0.2) %>% 
    prep(training = all_data)

print(lg_actuarial_recipe)

## ------------------------------------------
all_data_lg2 <- juice(lg_actuarial_recipe)
all_data_lg2 %>% dim()
all_data_lg2 %>% head()

all_data_lg2 %>% 
map_df(~sum(is.infinite(.))) %>% 
    pivot_longer(cols = everything(), 
                 names_to = "variable",
                 values_to = "na_count") %>% 
    print(n = 100)

## ------------------------------------------
train_index <- seq_len(nrow(train))
train_lg <- all_data_lg2[train_index,]
test_lg <- all_data_lg2[-train_index,]

## ------------------------------------------
train_lg %>% 
    select_if(is.numeric) %>% 
    drop_na() %>% 
    cor() %>% as.data.frame() %>% 
    rownames_to_column(var = "variables") %>% 
    select(variables, ultimate_incurred_claim_cost) %>% 
    arrange(desc(abs(ultimate_incurred_claim_cost)))

## ------------------------------------------
# set.seed(2021)
# 
# validation_split <- vfold_cv(train, v=5, strata = ultimate_incurred_claim_cost)

lm_spec <- linear_reg(penalty = tune(), 
                      mixture = 0) %>%
    set_engine("glmnet")

lambda_grid <- grid_regular(penalty(), levels = 10)

lm_workflow <- workflow() %>%
    add_model(lm_spec) %>% 
    add_formula(ultimate_incurred_claim_cost ~ .)

ctrl_grid <- control_stack_grid()

tic()
# lm_fit_vfold$.notes[[1]]$.notes
lm_fit_vfold <- 
    tune_grid(
        lm_workflow,
        data = train_lg,
        resamples = validation_split,
        grid = lambda_grid,
        metrics = metric_set(rmse),
        control = ctrl_grid
    )
toc()

lm_fit_vfold %>% 
    collect_metrics()

## ------------------------------------------
# stacking
actuarial_stacking <- 
    stacks() %>% 
    add_candidates(rf_fit_vfold) %>% 
    add_candidates(lm_fit_vfold)
actuarial_stacking
actuarial_stacking %<>%  
    blend_predictions() %>% 
    fit_members()

options(max.print = 50)

result <- predict(actuarial_stacking, test)
result %>% head()

# ## ------------------------------------------
submission <- read_csv(file.path(file_path, "sample_submission.csv"))
submission$UltimateIncurredClaimCost <- exp(result$.pred) - 1
write.csv(submission, row.names = FALSE,
          "stack_lg_rf_april10.csv")
