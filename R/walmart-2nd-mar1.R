library(tidyverse)
library(tidymodels)
library(lubridate)
library(skimr)
library(magrittr)
options(tibble.width = 100)


train <- read_csv("./data/walmart/train.csv.zip")
test <- read_csv("./data/walmart/test.csv.zip")

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
    mutate( year = year(date),
           month = month(date), # not so good?
           store = as_factor(store),
           dept = as_factor(dept)) %>% 
    select(-c(date)) -> all_data2

all_data2 %>% 
    count(dept) %>% 
    ggplot(aes(x = n, y = fct_reorder(dept, n))) + 
    geom_col()

all_data2 %>% head()

walmart_recipe <- 
    recipe(weekly_sales ~ .,
           data = all_data2) %>% 
    step_other(dept, threshold = 0.01) %>% 
    step_dummy(all_nominal()) %>% 
    step_normalize(all_numeric(), -all_outcomes())
walmart_recipe

walmart_recipe <- prep(walmart_recipe, training = all_data2)
walmart_recipe

all_data3 <- bake(walmart_recipe, 
                  new_data = all_data2)
names(all_data3)
head(all_data3)
View(all_data3)

# train, test
train_index <- seq_len(nrow(train))
train2 <- all_data3[train_index,]
test2 <- all_data3[-train_index,]

train2 %>% dim()

lm_model <- 
    linear_reg() %>% 
    set_engine("lm")

lm_form_fit <- 
    lm_model %>% 
    fit(weekly_sales ~ ., data = train2)

lm_form_fit

result <- predict(lm_form_fit, new_data = test2)

subfile <- read_csv("./data/walmart/sampleSubmission.csv.zip")
subfile$Weekly_Sales <- result$.pred

write.csv(subfile, row.names = FALSE,
          "./data/walmart/baseline-lm-03012021.csv")
