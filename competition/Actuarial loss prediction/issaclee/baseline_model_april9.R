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
# train$ultimate_incurred_claim_cost %>% log() %>% hist()

## ------------------------------------------
all_data <- bind_rows(train, test)
all_data$dominant_topic <- topic_info$dominant_topic
all_data$topic_perc_contrib <- topic_info$topic_perc_contrib
all_data %>% head()

names(all_data)
dim(all_data)
actuarial_recipe <- all_data %>% 
    recipe(ultimate_incurred_claim_cost ~ .) %>%
    step_log(ultimate_incurred_claim_cost, offset = 1) %>%
    step_log(initial_incurred_calims_cost, offset = 1) %>%
    step_mutate(has_child = ifelse(dependent_children > 0, "yes", "no")) %>% 
    step_mutate(marital_status = replace_na(marital_status, "U")) %>%
    step_mutate(year = lubridate::year(date_reported)) %>% 
    step_discretize(weekly_wages, num_breaks = 4, min_unique = 10) %>% 
    step_discretize(dependent_children, num_breaks = 3, min_unique = 10) %>% 
    step_rm(date_reported, date_time_of_accident) %>% 
    prep(training = all_data)

print(actuarial_recipe) 

## ------------------------------------------
all_data2 <- juice(actuarial_recipe)
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
topic_status <- train2 %>%
    filter(topic_perc_contrib > 0.7) %>% 
    group_by(dominant_topic, has_child, marital_status, part_time_full_time) %>% 
    summarise(ultimate_incurred_claim_cost = 
                  mean(ultimate_incurred_claim_cost, na.rm = TRUE))


topic_year <- train2 %>%
    filter(topic_perc_contrib > 0.7) %>% 
    group_by(dominant_topic, year) %>% 
    summarise(ultimate_incurred_claim_cost = 
                  mean(ultimate_incurred_claim_cost, na.rm = TRUE))

topic_wage_child <- train2 %>%
    filter(topic_perc_contrib > 0.7) %>% 
    group_by(dominant_topic, weekly_wages, dependent_children) %>% 
    summarise(ultimate_incurred_claim_cost = 
                  mean(ultimate_incurred_claim_cost, na.rm = TRUE))


result_topic_status <- all_data2 %>% 
    select(-ultimate_incurred_claim_cost) %>% 
    left_join(y = topic_status, 
              by = c("dominant_topic"="dominant_topic",
                     "has_child" ="has_child",
                     "marital_status"="marital_status",
                     "part_time_full_time"="part_time_full_time")) %>% 
    select(ultimate_incurred_claim_cost) %>% 
    mutate(ultimate_incurred_claim_cost = exp(ultimate_incurred_claim_cost) - 1) %>% 
    unlist() %>% as.numeric()

result_topic_year <- all_data2 %>% 
    select(-ultimate_incurred_claim_cost) %>% 
    left_join(y = topic_year, 
              by = c("dominant_topic"="dominant_topic",
                     "year"="year")) %>% 
    select(ultimate_incurred_claim_cost) %>% 
    mutate(ultimate_incurred_claim_cost = exp(ultimate_incurred_claim_cost) - 1) %>% 
    unlist() %>% as.numeric()

result_topic_wage_child <- all_data2 %>% 
    select(-ultimate_incurred_claim_cost) %>% 
    left_join(y = topic_wage_child, 
              by = c("dominant_topic"="dominant_topic",
                     "weekly_wages"="weekly_wages",
                     "dependent_children"="dependent_children")) %>% 
    select(ultimate_incurred_claim_cost) %>% 
    mutate(ultimate_incurred_claim_cost = exp(ultimate_incurred_claim_cost) - 1) %>% 
    unlist() %>% as.numeric()


# Manage NA's
result_topic_status %>% head()
is.na(result_topic_status) %>% sum()
result_topic_status[is.na(result_topic_status)] <- 0

result_topic_year
result_topic_year %>% head()
is.na(result_topic_year) %>% sum()
result_topic_year[is.na(result_topic_year)] <- 0


result_topic_wage_child %>% head()
is.na(result_topic_wage_child) %>% sum()
result_topic_wage_child[is.na(result_topic_wage_child)] <- 0

result <- tibble(topic_status = result_topic_status,
                 topic_year = result_topic_year,
                 topic_wage_child = result_topic_wage_child)

result$ultimate_incurred_claim_cost <- all_data$ultimate_incurred_claim_cost

lg_model <- lm(ultimate_incurred_claim_cost ~ ., data = result[train_index,])
test_pred <- predict(lg_model, result[-train_index,]) 
result$pred <- predict(lg_model, result)

## ------------------------------------------
write.csv(result, row.names = FALSE, "topic_group_mean.csv")
submission <- read_csv(file.path(file_path, "sample_submission.csv"))
submission$UltimateIncurredClaimCost <- test_pred
write.csv(submission, row.names = FALSE,
          "topic_group_mean_baseline.csv")
submission %>% head()

# 
# train %>% names()
# train %>% 
#     ggplot(aes(x = ultimate_incurred_claim_cost)) +
#     geom_histogram()
# 
# train %>% 
#     ggplot(aes(x = log(ultimate_incurred_claim_cost + 1))) +
#     geom_histogram() +
#     labs(title = "Transformed distribution of claim_cost",
#          x = "claim_cost")
# 
# train %>% filter(dependent_children > 0) %>% dim()
# train %>% filter(dependent_children == 0,
#                  marital_status == "M") %>% dim()
# train %>% filter(dependent_children == 0,
#                  marital_status == "S") %>% dim()
# train %>% filter(dependent_children == 0,
#                  marital_status == "U") %>% dim()
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# dataA <- train[(log(train$ultimate_incurred_claim_cost + 1) > 8 &
#     log(train$ultimate_incurred_claim_cost + 1) < 10),]
# dataB <- train[!(log(train$ultimate_incurred_claim_cost + 1) > 8 &
#                     log(train$ultimate_incurred_claim_cost + 1) < 10),]
# 
# 
# train %>% 
#     filter(dependent_children == 0,
#            
#            part_time_full_time == "F",
#            marital_status == "M",
#            age > 50
#            ) %>% 
#     ggplot(aes(x = log(ultimate_incurred_claim_cost + 1))) +
#     geom_histogram() +
#     labs(title = "Transformed distribution of claim_cost",
#          x = "claim_cost")
# 
# train$date_reported
# 
# train %>% 
#     filter(marital_status == "S") %>% 
#     ggplot(aes(x = log(ultimate_incurred_claim_cost + 1))) +
#     geom_histogram() +
#     labs(title = "Transformed distribution of claim_cost",
#          x = "claim_cost")
# 
# train %>% 
#     filter(part_time_full_time == "P") %>% 
#     ggplot(aes(x = log(ultimate_incurred_claim_cost + 1))) +
#     geom_histogram() +
#     labs(title = "Transformed distribution of claim_cost",
#          x = "claim_cost")
# 
# dataA %>% group_by(dependents_other) %>% count()
# dataB %>% group_by(dependent_children) %>% count()

