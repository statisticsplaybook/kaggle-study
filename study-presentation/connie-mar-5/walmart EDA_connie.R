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
dim(train)
dim(test)
dim(train_test)
train_test <- train_test %>% janitor::clean_names()

fea_sto <- left_join(features,stores,by="Store")
dim(features)
dim(stores)
dim(fea_sto)
fea_sto <- fea_sto %>% janitor::clean_names()

alldata <- left_join(train_test,fea_sto,by=c("store"= "store",
                                             "date"="date",
                                             "is_holiday"="is_holiday"))
dim(alldata)

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

alldata2 %>% group_by(year,week) %>% 
    summarise(weekly_sales = sum(weekly_sales,na.rm=TRUE)) %>% 
    filter(weekly_sales !=0) -> sales_year


# 
#<original code> 
# alldata2[1:421570,] %>% group_by(year,week) %>% 
#     summarise(weekly_sales = sum(weekly_sales)) -> sales_year



##holiday week를 차트에 표시하기 위해 holiday= TRUE값 추출

alldata2 %>% filter(is_holiday == TRUE) %>% select(year, week, weekly_sales, is_holiday) %>%
    group_by(year, week) %>% 
    summarise(weekly_sales = sum(weekly_sales))->holiday_week
    
    
#2010년과 2011년에 7,37,48,53week, 2012년에 6,36, 47, 52week, 2013년엔 6week holiday.
#weekly/yearly sales 데이터를 그린 ggplot에 holiday week 표시 


sales_year %>% ggplot()+
    geom_line(aes(x=week, y=weekly_sales, color=factor(year)))+
    scale_x_continuous(breaks = 1:53) + scale_y_continuous(labels=comma)+
    geom_vline(xintercept = c(7,37,48,53,6,36,47,52), 
               color="pink", size=.5) +labs(title = "Visualization of sales per week",
                                            subtitle = "Vline is holiday")


# 바로 alldata2로 만드는 방법은..없나.. 
# alldata2 %>% filter(weekly_sales!=0) %>% 
#     summarise(weekly_sales = sum(weekly_sales,na.rm=TRUE)) %>%
#     ggplot()+geom_line(na.rm=TRUE,aes(x=week,y=weekly_sales,color=factor(year)))


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

alldata2 %>% mutate(holiday_add = case_when(
    year=="2010"&week==14 ~ 1,
    year=="2011"&week==17 ~ 1,
    year=="2012"&week==14 ~ 1,
    year=="2013"&week==13 ~ 1,
    is_holiday==TRUE ~ 1,
    T ~ 0
)) -> alldata2

#Easter Day를 공휴일로 추가한 column으로 공휴일 잘 추가되었는지, holiday_week2 테이블 만들어서 확인

alldata2 %>% filter(holiday_add == 1) %>% select(year, week, weekly_sales, is_holiday) %>%
    group_by(year, week) %>% 
    summarise(weekly_sales = sum(weekly_sales))->holiday_week2

holiday_week2



#3)store&dept

#store
alldata2 %>% group_by(store) %>% 
    summarise(weekly_sales = sum(weekly_sales,na.rm=TRUE)) -> sales_stores

sales_stores %>% ggplot() +
    geom_bar(aes(x=store, y=weekly_sales, fill=factor(store)), stat="identity") +
    scale_x_continuous(breaks=c(1:45)) + scale_y_continuous(labels = comma) +
    theme(legend.position = "none") + 
    labs(title = "Visualization of sales per Store")

#dept
alldata2 %>% group_by(dept) %>% 
    summarise(weekly_sales = sum(weekly_sales,na.rm=TRUE)) -> sales_dept

sales_dept %>% ggplot() +
    geom_bar(aes(x=dept, y=weekly_sales, fill=factor(dept)), stat="identity") +
    scale_x_continuous(breaks=c(1:99)) + scale_y_continuous(labels = comma) +
    theme(legend.position = "none") +
    labs(title = "Visualization of sales per dept")

#size
alldata2 %>% group_by(size) %>% 
    summarise(weekly_sales = sum(weekly_sales, na.rm=TRUE)) -> sales_size

sales_size %>% ggplot() +
    geom_line(aes(x=size, y=weekly_sales)) +
    scale_x_continuous() + scale_y_continuous(labels = comma)+
    labs(title = "Visualization of sales per size")

##store/dept마다 세일즈 편차가 크다는 것을 확인 할 수 있고,
##size가 클수록 점진적으로 세일즈도 커진다는 것을 확인할 수 있음


#4)type

ggplot(alldata2, aes(x=type, y=weekly_sales, color=type)) + geom_boxplot(na.rm=TRUE) +
    scale_y_continuous(labels = comma)

ggplot(alldata2, aes(x=type, y=weekly_sales, color=type)) + geom_jitter(na.rm=TRUE) +
    scale_y_continuous(labels = comma)

##Sales는 A>B>C 순인것을 확인 할 수 있음
##상관분석을 하고, model develop과정에서 이용하기 위해 숫자형 데이터로 변환

skim(alldata2)

alldata2 %>% mutate(type_num = case_when(
    type=="A" ~ 3,
    type=="B" ~ 2,
    type=="C" ~ 1
)) -> alldata2
skim(alldata2)

skim(alldata2)




###상관분석

library(corrplot)

alldata2[1:421570,] %>% select(-c(is_holiday,date,type)) -> M
skim(M)


cor(M, method = "pearson") ->M
corrplot(M, method = "number", type="lower")

##매장 size와 type은 양의 상관관계가 있음.(매장 type은 규모와 관련되어 구분되는 것으로 추정) 
##year와 fuel_price도 양의 상관관계가 있음
##매장 size와 weekly_sales도 0.2 이상의 양의 상관관계




###Model Development
#recipe 이용한 전처리
#recipe: 시각화와 예측모형을 개발하는데 필요한 모형설계행렬을 전처리하는 작업과정을 담당하는 역할
#https://recipes.tidymodels.org/
#part of tidymodels

alldata2 %>% select(c(store,dept,weekly_sales,size,year,week,holiday_add,type_num))->final_data

walmart_recipe <-
    recipe(weekly_sales~., data=final_data) %>%
    step_normalize(all_numeric(),  -all_outcomes())
walmart_recipe

walmart_recipe <- prep(walmart_recipe, training = final_data)
walmart_recipe

final_data <- bake(walmart_recipe, new_data = final_data)

names(final_data)
head(final_data)


#split train/test set

train_index <- seq_len(nrow(train))
train2 <- final_data[train_index,]
test2 <- final_data[-train_index,]

train2 %>% dim()

#model building and training

lm_model <- linear_reg() %>%
    set_engine("lm")

lm_form_fit <- lm_model %>%
    fit(weekly_sales~., data=train2)

lm_form_fit

###Result

#Prediction and submission 
result<- predict(lm_form_fit, new_data = test2)
subfile <- read_csv("data/walmart/sampleSubmission.csv.zip")
subfile$Weekly_Sales <- result$.pred
write.csv(subfile, row.names = FALSE, "./walmart EDA/submission.csv")



