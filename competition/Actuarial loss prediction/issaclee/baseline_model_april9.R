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


## ------------------------------------------
all_data <- bind_rows(train, test)
all_data %>% head()
names(all_data)
dim(all_data)
actuarial_recipe <- all_data %>% 
    recipe(ultimate_incurred_claim_cost ~ .) %>%
    step_log(ultimate_incurred_claim_cost, offset = 1) %>% 
    step_log(initial_incurred_calims_cost, offset = 1) %>%
    step_mutate(has_child = ifelse(dependent_children > 0, "yes", "no")) %>% 
    step_mutate(marital_status = replace_na(marital_status, "U")) %>%
    step_mutate(
        year = lubridate::year(date_reported),
        month = lubridate::month(date_reported)) %>% 
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
mean_model <- train2 %>% 
    group_by(year, has_child, marital_status, part_time_full_time) %>% 
    summarise(ultimate_incurred_claim_cost = 
                  mean(ultimate_incurred_claim_cost, na.rm = TRUE))

result <- all_data2 %>% 
    select(-ultimate_incurred_claim_cost) %>% 
    left_join(y = mean_model, 
              by = c("year"="year",
                     "has_child" ="has_child",
                     "marital_status"="marital_status",
                     "part_time_full_time"="part_time_full_time")) %>% 
    select(ultimate_incurred_claim_cost) %>% 
    mutate(ultimate_incurred_claim_cost = exp(ultimate_incurred_claim_cost) - 1) %>% 
    unlist() %>% as.numeric()

# Manage NA's
result %>% head()
is.na(result) %>% sum()
result[is.na(result)] <- 0

## ------------------------------------------
write.csv(tibble(ultimate_group_mean = result), row.names = FALSE,
          "weekly_group_mean.csv")


train %>% names()
train %>% 
    ggplot(aes(x = ultimate_incurred_claim_cost)) +
    geom_histogram()

train %>% 
    ggplot(aes(x = log(ultimate_incurred_claim_cost + 1))) +
    geom_histogram() +
    labs(title = "Transformed distribution of claim_cost",
         x = "claim_cost")

train %>% filter(dependent_children > 0) %>% dim()
train %>% filter(dependent_children == 0,
                 marital_status == "M") %>% dim()
train %>% filter(dependent_children == 0,
                 marital_status == "S") %>% dim()
train %>% filter(dependent_children == 0,
                 marital_status == "U") %>% dim()










dataA <- train[(log(train$ultimate_incurred_claim_cost + 1) > 8 &
    log(train$ultimate_incurred_claim_cost + 1) < 10),]
dataB <- train[!(log(train$ultimate_incurred_claim_cost + 1) > 8 &
                    log(train$ultimate_incurred_claim_cost + 1) < 10),]


train %>% 
    filter(dependent_children == 0,
           
           part_time_full_time == "F",
           marital_status == "M",
           age > 50
           ) %>% 
    ggplot(aes(x = log(ultimate_incurred_claim_cost + 1))) +
    geom_histogram() +
    labs(title = "Transformed distribution of claim_cost",
         x = "claim_cost")

train$date_reported

train %>% 
    filter(marital_status == "S") %>% 
    ggplot(aes(x = log(ultimate_incurred_claim_cost + 1))) +
    geom_histogram() +
    labs(title = "Transformed distribution of claim_cost",
         x = "claim_cost")

train %>% 
    filter(part_time_full_time == "P") %>% 
    ggplot(aes(x = log(ultimate_incurred_claim_cost + 1))) +
    geom_histogram() +
    labs(title = "Transformed distribution of claim_cost",
         x = "claim_cost")

dataA %>% group_by(dependents_other) %>% count()
dataB %>% group_by(dependent_children) %>% count()

