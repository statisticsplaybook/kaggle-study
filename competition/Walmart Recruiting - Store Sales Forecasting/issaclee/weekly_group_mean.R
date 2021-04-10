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


## ------------------------------------------
train$weekly_sales <- sign(train$weekly_sales) * (abs(train$weekly_sales))^(1/5)
all_data <- bind_rows(train, test)
all_data %>% head()
names(all_data)
dim(all_data)


## ------------------------------------------
walmart_recipe <- all_data %>% 
    recipe(weekly_sales ~ .) %>%
    step_mutate(
        year = lubridate::year(date),
        week = lubridate::week(date)) %>% 
    step_rm(date) %>% 
    prep(training = all_data)

print(walmart_recipe)


## ------------------------------------------
all_data2 <- juice(walmart_recipe)
all_data2 %>% dim()
all_data2 %>% head()


## ------------------------------------------
train_index <- seq_len(nrow(train))
train2 <- all_data2[train_index,]
test2 <- all_data2[-train_index,]


## ------------------------------------------
mean_model <- train2 %>% 
    group_by(store, dept, week, is_holiday) %>% 
    summarise(weekly_sales = mean(weekly_sales, na.rm = TRUE))

result <- all_data2 %>% 
    select(-weekly_sales) %>% 
    left_join(y = mean_model, 
              by = c("store"="store",
                     "dept" ="dept",
                     "week"="week",
                     "is_holiday"="is_holiday")) %>% 
    select(weekly_sales) %>% 
    mutate(weekly_sales = sign(weekly_sales) * (abs(weekly_sales)^5)) %>% 
    unlist() %>% as.numeric()

# Manage NA's
result %>% head()
is.na(result) %>% sum()
result[is.na(result)] <- 0

## ------------------------------------------
write.csv(tibble(weekly_group_mean = result), row.names = FALSE,
          "weekly_group_mean.csv")

