library(tidyverse)
library(tidymodels)
library(lubridate)
library(skimr)
library(magrittr)

train <- read_csv("./walmart/train.csv.zip")
test <- read_csv("./walmart/test.csv.zip")


# size of data
dim(train)
dim(test)

# train
names(train)
train %>% head()

# test
names(test)
test %>% head()

# alldata combine
all_data <- bind_rows(train, test)
all_data <- all_data %>% janitor::clean_names()
names(all_data)

all_data %>% head()
all_data %>% skim()

all_data %>% 
    mutate(year = year(date),
           month = month(date)) %>% 
    select(-c(date)) -> all_data2

walmart_recipe <- 
    recipe(weekly_sales ~ .,
           data = all_data2) %>% 
    step_normalize(all_numeric(), -all_outcomes())
walmart_recipe

walmart_recipe <- prep(walmart_recipe, training = all_data2)
walmart_recipe

all_data2 <- bake(walmart_recipe, 
                  new_data = all_data2)
names(all_data2)
head(all_data2)
# View(all_data2)

# train, test
train_index <- seq_len(nrow(train))
train2 <- all_data2[train_index,]
test2 <- all_data2[-train_index,]

train2 %>% dim()

lm_model <- 
    linear_reg() %>% 
    set_engine("lm")

lm_form_fit <- 
    lm_model %>% 
    fit(weekly_sales ~ ., data = train2)

tidy(lm_form_fit)

result <- predict(lm_form_fit, new_data = test2)

subfile <- read_csv("./walmart/sampleSubmission.csv.zip")
subfile$Weekly_Sales <- result$.pred

subfile

write.csv(subfile, row.names = FALSE,
          "./walmart/baseline-lm-02262021.csv")
