library(tidyverse)
library(tidymodels)
library(lubridate)
library(skimr)
library(magrittr)


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


# overview 

train %>% head()
test %>% head()
stores %>% head()
features %>% head()
features %>% tail()



# alldata combine : train, test 전부 전처리하고 시작 

all_data <- bind_rows(train, test) # 없는 변수는 NA 처리. rbind는 error 뜸 
all_data <- all_data %>% janitor::clean_names() # 변수 이름 소문자로 전환. 
features <- features %>% janitor::clean_names()
stores <- stores %>% janitor::clean_names()

features <- features %>% left_join(stores, by = 'store')

names(all_data)
names(features)

all_data %>% head()
all_data %>% skim() # n_missing : NA 개수, n_unique : unique 개수 

# 
# all_data <- all_data %>% 
#     mutate(year = year(date),
#            month = month(date), 
#            day = day(date),
#            weekday = weekdays(date)) %>% 
#     select(-c(date)) %>% 
#     select(year, month, day, weekday, everything()) %>% 
#     arrange(year, month, day, weekday, store, dept)
# 
# all_data %>% head()
# 
# 
# features <- features %>%
#     mutate(year = year(date),
#            month = month(date), 
#            day = day(date), 
#            weekday = weekdays(date)) %>% 
#     select(-c(date)) %>% 
#     select(year, month, day, weekday, everything()) %>% 
#     arrange(year, month, day, weekday, store)


features %>% select(starts_with('mark_down')) %>%
    summary()




# all_data %>% inner_join(features, by = c('year', 'month', 'weekday', 'store')) %>% head(n = 20)


# step_corr() - Removes variables that have large absolute correlations with other variables
# step_center() - Normalizes numeric data to have a mean of zero
# step_scale() - Normalizes numeric data to have a standard deviation of one
# step_log() - step_log(변수, base = 10)
# step_dummy(all_nominal()) - 더미변수 처리, all_nominal() : factor or character columns 모두 지정 
# all_numeric(), all_predictors(), all_outcomes()


walmart_recipe <- 
    recipe(weekly_sales ~ .,
           data = all_data) %>%
    step_date(date, features = c('dow', 'month', 'year'), label = T) %>% # dow = day of week, abbr = Sunday or Sun, label = Sunday or number
    step_holiday(date, holidays = timeDate::listHolidays('US')) %>% 
    step_rm(date) %>% # we remove the original date variable since we no longer want it in the model.
    step_dummy(all_nominal()) %>% 
    step_normalize(all_numeric(), -all_outcomes()) # weekly sales는 normalize x, 나머지는 normalize o 
walmart_recipe

walmart_recipe <- prep(walmart_recipe, training = all_data2) # preperation ?
walmart_recipe

all_data2 <- bake(walmart_recipe, 
                  new_data = all_data2) # 실제 전처리 실시 
names(all_data2)
head(all_data2)
View(all_data2)



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

lm_form_fit

result <- predict(lm_form_fit, new_data = test2)

subfile <- read_csv("./data/walmart/sampleSubmission.csv.zip")
subfile

subfile$Weekly_Sales <- result$.pred

write.csv(subfile, row.names = FALSE,
          "./data/walmart/baseline-lm-02262021.csv")



