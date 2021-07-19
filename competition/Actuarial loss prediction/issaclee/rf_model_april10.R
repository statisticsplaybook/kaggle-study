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

## ------------------------------------------
train$ultimate_incurred_claim_cost %>% log() %>% hist()

all_data <- bind_rows(train, test)
all_data <- bind_cols(all_data, emb_all)
all_data$claim_description <- topic_info$keywords
all_data$dominant_topic <- topic_info$dominant_topic
all_data$topic_perc_contrib <- topic_info$topic_perc_contrib
# price_index %<>% 
#     mutate(time = paste0(time, "-01"),
#            time = lubridate::ymd(time),
#            year = lubridate::year(time),
#            month = lubridate::month(time)) %>%
#     select(year, month, value) 

# all_data %<>%
#     mutate(year = lubridate::year(date_time_of_accident),
#            month = lubridate::month(date_time_of_accident)) %>% 
#     left_join(y = price_index, 
#             by = c("year" = "year",
#                    "month" = "month"))
# all_data %<>% 
#     mutate(unit_dollar_init = initial_incurred_calims_cost / value) %>% 
#     mutate(unit_dollar_ult = ultimate_incurred_claim_cost / value) %>% 
#     select(everything(), ultimate_incurred_claim_cost)
    
all_data$ultimate_incurred_claim_cost %>% hist()
all_data %>% head()

## ------------------------------------------
# NA
all_data$gender[all_data$gender == "U"] <- NA
all_data$marital_status[all_data$marital_status == "U"] <- NA
all_data$weekly_wages[all_data$weekly_wages > 2000] <- 2000
all_data$hours_worked_per_week[all_data$hours_worked_per_week > 70] <- 70

names(all_data)
dim(all_data)

actuarial_recipe <- all_data %>% 
    recipe(ultimate_incurred_claim_cost ~ .) %>%
    step_log(ultimate_incurred_claim_cost, offset = 1) %>%
    step_log(initial_incurred_calims_cost, offset = 1) %>%
    ## ------------------------------------------
    step_mutate(rp_year = lubridate::year(date_reported)) %>%
    step_mutate(rp_month = lubridate::month(date_reported)) %>%
    step_mutate(rp_week = lubridate::week(date_reported)) %>% 
    step_mutate(ac_year = lubridate::year(date_time_of_accident)) %>%
    step_mutate(ac_month = lubridate::month(date_time_of_accident)) %>%
    step_mutate(ac_week = lubridate::week(date_time_of_accident)) %>% 
    ## ------------------------------------------
    step_modeimpute(marital_status, gender) %>%
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
    # step_mutate(symp_concussion = sum(str_detect(claim_description, c("concussion")))) %>%
    # step_mutate(symp_injury = sum(str_detect(claim_description, c("injury")))) %>%
    # step_mutate(symp_infection = sum(str_detect(claim_description, c("infection", "infected")))) %>%
    # step_mutate(symp_fracture = sum(str_detect(claim_description, c("fractured", "fracture")))) %>%
    # step_mutate(symp_strain = sum(str_detect(claim_description, c("strain", "strained")))) %>%
    # step_mutate(symp_bruised = sum(str_detect(claim_description, c("bruised", "bruise")))) %>%
    # step_mutate(symp_cut = sum(str_detect(claim_description, c("cut", "cutting", "laceration")))) %>%
    ## context------------------------------------------
    # step_mutate(context_struck = sum(str_detect(claim_description, c("struck")))) %>%
    # step_mutate(context_fell = sum(str_detect(claim_description, c("fell", "slipped")))) %>%
    # step_mutate(context_twisted = sum(str_detect(claim_description, c("twist", "twisted")))) %>%
    # step_mutate(context_caught = sum(str_detect(claim_description, c("caught")))) %>%
    # step_mutate(context_repetative = sum(str_detect(claim_description, c("repetative")))) %>%
    # step_mutate(context_floor = sum(str_detect(claim_description, c("floor")))) %>%
    # step_mutate(context_hammer = sum(str_detect(claim_description, c("hammer")))) %>%
    # step_mutate(context_metal = sum(str_detect(claim_description, c("metal", "iron", "steel")))) %>%
    # step_mutate(context_lift = sum(str_detect(claim_description, c("lifting")))) %>%
    # step_mutate(context_vehicle = sum(str_detect(claim_description, c("vehicle", "car")))) %>%
    # step_mutate(context_accident = sum(str_detect(claim_description, c("accident", "collision", "crush", "crushed")))) %>%
    # step_mutate(context_severe = sum(str_detect(claim_description, c("severe")))) %>%
    # step_mutate(context_severe = sum(str_detect(claim_description, c("severe")))) %>%
    ## interaction------------------------------------------
    # step_mutate(footcut = symp_cut * body_foot) %>%
    # step_mutate(footstrain = symp_strain * body_foot) %>%
    # step_mutate(footfracture = symp_fracture * body_foot) %>%
    # step_mutate(footbruised = symp_bruised * body_foot) %>%
    # step_mutate(fingercut      = symp_cut      * body_finger) %>%
    # step_mutate(fingerstrain   = symp_strain   * body_finger) %>%
    # step_mutate(fingerfracture = symp_fracture * body_finger) %>%
    # step_mutate(fingerbruised  = symp_bruised  * body_finger) %>%
    # step_mutate(backcut      = symp_cut      * body_back) %>%
    # step_mutate(backstrain   = symp_strain   * body_back) %>%
    # step_mutate(backfracture = symp_fracture * body_back) %>%
    # step_mutate(backbruised  = symp_bruised  * body_back) %>%
    ## ------------------------------------------
    step_rm(date_reported, date_time_of_accident, claim_number, claim_description) %>% 
    step_integer(all_nominal()) %>%
    step_center(all_numeric(), -all_outcomes(), -wage_per_hour) %>% 
    prep(training = all_data)

print(actuarial_recipe)

## ------------------------------------------
all_data2 <- juice(actuarial_recipe)
all_data2 %>% dim()
all_data2 %>% head()

map_df(~sum(is.na(.))) %>% 
pivot_longer(cols = everything(), 
             names_to = "variable",
             values_to = "na_count") %>% 
    print(n = 100)

## ------------------------------------------
train_index <- seq_len(nrow(train))
train2 <- all_data2[train_index,]
test2 <- all_data2[-train_index,]

## ------------------------------------------
train2 %>% 
    select_if(is.numeric) %>% 
    drop_na() %>% 
    cor() %>% as.data.frame() %>% 
    rownames_to_column(var = "variables") %>% 
    select(variables, ultimate_incurred_claim_cost) %>% 
    arrange(desc(abs(ultimate_incurred_claim_cost)))

## ------------------------------------------
set.seed(2021)
cores <- parallel::detectCores() -1

# validation_split <- validation_split(train2, prop = 0.7,
#                                      strata = ultimate_incurred_claim_cost)
validation_split <- vfold_cv(train2, v=5, strata = ultimate_incurred_claim_cost)

rf_spec <- rand_forest(mtry = tune(),
                       min_n = tune(),
                       trees = 500) %>% 
    set_engine("ranger", seed = 2021,
               importance = "none",
               verbose = TRUE,
               num.threads = cores) %>%
    set_mode("regression")

# rf_parm_set <- parameters(rf_spec) %>%
#     finalize(mtry(), x = train2[,-1])

# param_grid <- grid_latin_hypercube(
#   finalize(mtry(), x = train2[,-1]),
#   min_n(), size = 15)
# 25, 26
# 34, 15
# 17, 21 previous
param_grid <- tibble(mtry = 17,
                     min_n = 21)

workflow <- workflow() %>%
    add_model(rf_spec) %>% 
    add_formula(ultimate_incurred_claim_cost ~ .)

## ------------------------------------------
# library(doParallel)
# Cluster <- makeCluster(detectCores() - 1)
# registerDoParallel(Cluster)


library(tictoc)
tic()
# tune_result <- workflow %>%
#     tune_bayes(resample = validation_split,
#                param_info = rf_parm_set,
#                initial = 5,
#                iter = 15,
#                metrics = metric_set(rmse),
#                control = control_bayes(no_improve = 30,
#                                        verbose = TRUE)
#             )
tune_result <- workflow %>%
    tune_grid(validation_split,
              grid = param_grid,
              control = control_grid(save_pred = FALSE),
              metrics = metric_set(rmse))
toc()

## ------------------------------------------
tune_result %>% 
    collect_metrics()

# tune_result %>%
#     collect_metrics() %>%
#     filter(.metric == "rmse") %>% 
#     ggplot(aes(x = trees, y = mean)) +
#     geom_point(size = 1.5) +
#     theme(legend.position = "none") +
#     labs(title = "RMSE")

## ------------------------------------------
# v5 best 0.619 | dominant_topic w/ prob
# v5 best 0.6181405 | dominant_topic w/ prob has_child
# v5 best 0.5784208 | dominant_topic w/ prob has_child
# v5 best 0.5695722 | dominant_topic w/ prob has_child w/ body
tune_result %>% show_best(metric = "rmse") %>% select(mean) %>% unlist() %>% mean()

tune_best <- tune_result %>% select_best(metric = "rmse")
tune_best$mtry
tune_best$min_n

rf_model <- rand_forest(mtry = tune_best$mtry,
                       min_n = tune_best$min_n,
                       trees = 500) %>%
    set_engine("ranger",
               importance = "none",
               seed = 2021,
               num.threads = cores) %>%
    set_mode("regression")

# workflow <- workflow %>% update(tune_best)
tictoc::tic()
rf_fit <-
    rf_model %>%
    fit(ultimate_incurred_claim_cost ~ ., data = train2)
tictoc::toc()

options(max.print = 50)

result <- predict(rf_fit, test2)
result %>% head()

# ## ------------------------------------------
submission <- read_csv(file.path(file_path, "sample_submission.csv"))
submission$UltimateIncurredClaimCost <- exp(result$.pred) - 1
write.csv(submission, row.names = FALSE,
          "stack_rf_april10.csv")
