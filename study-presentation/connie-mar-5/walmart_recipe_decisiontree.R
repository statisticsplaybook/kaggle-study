library(tidyverse)
library(tidymodels)
library(lubridate)
library(skimr)
library(magrittr)
library("data.table")


train <- read_csv("data/walmart/train.csv.zip")
test <- read_csv("data/walmart/test.csv.zip")
features <- read_csv("data/walmart/features.csv.zip")
stores <- read_csv("data/walmart/stores.csv")

train_test <- bind_rows(train,test)
train_test <- train_test %>% janitor::clean_names()

fea_sto <- left_join(features,stores,by="Store")
fea_sto <- fea_sto %>% janitor::clean_names()

alldata <- left_join(train_test,fea_sto,by=c("store"= "store",
                                             "date"="date",
                                             "is_holiday"="is_holiday"))

dim(alldata)

dim(train)
dim(test)
dim(features)
dim(stores)

names(train)
names(test)
names(features)
names(stores)


###eda

alldata %>% mutate(year=year(date),
                   month=month(date),
                   week=week(date)) ->alldata

alldata %>% skim()


#1)markdown: complete rate 0.50 이하 -> drop
alldata %>% select(-c(mark_down1, mark_down2, mark_down3, mark_down4, mark_down5))-> alldata2
dim(alldata2)





#2)week & holiday
##year, week의 sales ggplot으로 보기위해서 sales_year 테이블 생성


alldata %>% group_by(year,week) %>% 
    summarise(weekly_sales = mean(weekly_sales,na.rm=TRUE)) %>%
    ggplot()+
    geom_line(aes(x=week, y=weekly_sales, color=factor(year)))+
    scale_x_continuous(breaks = 1:53)



alldata %>% filter(is_holiday==TRUE) %>%
    select(year,week, weekly_sales, is_holiday) %>%
    group_by(year,week) %>%
    summarise(weekly_sales = mean(weekly_sales))


alldata %>% group_by(year,week) %>% 
    summarise(weekly_sales = mean(weekly_sales,na.rm=TRUE)) %>%
    ggplot()+
    geom_line(aes(x=week, y=weekly_sales, color=factor(year)))+
    scale_x_continuous(breaks = 1:53)+
    geom_vline(xintercept = c(7,37,48,53,6,36,47,52), 
               color="pink", size=.5)

#holiday로 표시되어 있지 않지만, 매출이 높은 지점 around w13~w17 발견
##포함되지 않은 공휴일:Easter day 추가해주기 
#[Python]
##2010 week13
##2011 week16
##2012 week14
##2013 week13
#[R]
##2010 week14
##2011 week17
##2012 week14
##2013 week13
alldata %>% mutate(holiday_add = case_when(
    year=="2010"&week==14 ~ 1,
    year=="2011"&week==17 ~ 1,
    year=="2012"&week==14 ~ 1,
    year=="2013"&week==13 ~ 1,
    is_holiday==TRUE ~ 1,
    T ~ 0
)) -> alldata
alldata %>% head()




#3)store&dept


#4)type



##Sales는 A>B>C 순인것을 확인 할 수 있음
##상관분석을 하고, model develop과정에서 이용하기 위해 숫자형 데이터로 변환



skim(alldata)

library(GGally)

alldata %>% 
    select(weekly_sales, holiday_add, store, dept, 
           temperature, fuel_price, size, week, type) %>%
    ggpairs()



###Model Development
#recipe 이용한 전처리
#recipe: 시각화와 예측모형을 개발하는데 필요한 모형설계행렬을 전처리하는 작업과정을 담당하는 역할
#https://recipes.tidymodels.org/
#part of tidymodels


final_data <- alldata %>%
    select(weekly_sales,date, holiday_add, store, dept, 
           temperature, fuel_price, size, week, type)

final_data %>% head

final_data %>% 
    step_holiday(date, holidays = c("LaborDay","NewYearsDay","ChristmasDay","EasterSunday"))



timeDate::listHolidays


class(final_data$store)
final_data

# final_data_recipe <- recipe(weekly_sales~., data=final_data) %>% 
#     step_mutate(store=as_factor(store),
#                 dept=as_factor(dept)) %>%
#     step_date(date) %>%
#     step_dummy(all_numeric(), -all_outcomes()) %>%
#     step_zv(all_numeric()) %>%
#     step_normalize(all_numeric()) %>%
#     prep(final_data)
    
date_rec <- recipe(weekly_sales~., data=final_data) %>%
    step_date(date) %>%
    step_rm(holiday_add, store, dept, temperature, fuel_price, size, week, type)

summary(date_rec)
tidy(date_rec, number = 1)
date_rec <- prep(date_rec, training = final_data)
summary(date_rec)




final_data_recipe <- recipe(weekly_sales~., data=final_data) %>% 
    step_mutate(store=as_factor(store),
                dept=as_factor(dept),
                holiday_add=as_factor(holiday_add))%>%
    step_mutate(type=as.factor(type))%>%
    step_zv(all_numeric()) %>% #확인
    step_date(date) %>%
    step_rm(date, week, date_dow) %>%
    step_dummy(all_nominal()) %>%
    step_normalize(all_numeric(), -all_outcomes()) %>%
    prep()


final_data_recipe
summary(final_data_recipe)



final_data2 <- juice(final_data_recipe)
final_data2 %>% head()


#split train/test set


train_index <- seq_len(nrow(train))
train2 <- final_data2[train_index,]
test2 <- final_data2[-train_index,]

train2 %>% dim()
test2 %>% dim()

skim(train2)

#fit model <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<


fit_model <- linear_reg()%>%
    set_engine("lm")

lm_fit_model <- fit_model %>%
    fit(weekly_sales~., data=train2)

lm_fit_model


#Predict

result <- predict(lm_fit_model, new_data = test2)
result %>% head

submission <- read_csv("D:/user/Documents/R projects/kaggle-study/study-presentation/connie-mar-5/submission.csv")
submission$Weekly_Sales <- result$.pred
write.csv(submission, row.names = FALSE,
          "submission2.csv")

submission %>% head()

###Result

result<- predict(lm_form_fit, new_data = test2)
subfile <- read_csv("data/walmart/sampleSubmission.csv.zip")
subfile$Weekly_Sales <- result$.pred
write.csv(subfile, row.names = FALSE, "./walmart EDA/submission.csv")



#Prediction and submission 


