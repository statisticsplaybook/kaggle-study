library("tidymodels")
library("modeldata")
library("dplyr")
library("lubridate")


rdata2_ulsan <- read.csv("D:/user/Documents/R projects/kaggle-study/study-presentation/connie-mar-5/energy_dacon/data/rdata2_ulsan.csv")
submission <- read.csv("D:/user/Documents/R projects/kaggle-study/study-presentation/connie-mar-5/energy_dacon/data/sample_submission.csv")


glimpse(rdata2_ulsan)
names(rdata2_ulsan)


rdata2_ulsan<- rdata2_ulsan %>% 
    mutate(month = month(time),
           hour = hour(time),
           date = date(time),
           year = year(time))



# rdata2_ulsan <- rdata2_ulsan %>% dplyr::filter(date< "2021-03-01",
#                                                month == "2")
# data row number 198 -> drop



rdata2_ulsan %>% head()
rdata2_ulsan %>% tail()
rdata2_ulsan_14 <- rdata2_ulsan %>% dplyr::filter(hour == "14")
rdata2_ulsan_14 %>% head()

train_rdata2_ulsan <- rdata2_ulsan %>% filter(date < "2018-02-01")
test_rdata2_ulsan <- rdata2_ulsan %>% filter(date > "2021-01-31")


rdata2_ulsan_14 <- rdata2_ulsan_14 %>% 
    dplyr::filter(date >= "2018-02-01" &
               date <= "2021-01-31")

rdata2_ulsan_14


glimpse(rdata2_ulsan_14)



# feature visualisations

rdata2_ulsan_14 %>% 
    select(date, ulsan) %>%
    ggplot(aes(x=date, y=ulsan))+
    geom_line()


rdata2_ulsan_14 %>% 
    select(date, month, ulsan) %>%
    ggplot(aes(x=month, y=ulsan, color=as.factor(year(date))))+
    geom_smooth(se=FALSE) #예상대로 월별로 같은 패턴을 보이지는 않음. 기상의 영향을 많이 받는 듯


# feature 3가지 비교해보기

#1) solar_radiation

library(ggplot2)
names(rdata2_ulsan_14)


p1<- rdata2_ulsan_14 %>% 
    select(date, ulsan, solar_radiation) %>% 
    ggplot(aes(x=date))+
    geom_line(aes(y=ulsan))
    
p_solar <- rdata2_ulsan_14 %>% 
    select(date, ulsan, solar_radiation) %>% 
    ggplot(aes(x=date))+
    geom_line(aes(y=solar_radiation))

p_temp <- rdata2_ulsan_14 %>%
    select(date, ulsan, temperature) %>%
    ggplot(aes(x=date, y=temperature)) +
    geom_line()

p_visi <- rdata2_ulsan_14 %>%
    select(date,visibility) %>%
    ggplot(aes(x=date, y=visibility)) +
    geom_line() #발전양과 가장 비슷한 그래프를 보이는 건 visibility


##what type of patterns? - cyclical, seasonal, trends
##seasonality patterns -> reoccurring seasonal patterns. 

#Let's see if there are seasonal patterns with ulsan 

rdata2_ulsan_14 %>% 
    ggplot(aes(x=ulsan, color=as.factor(year)))+
    geom_density() +
    facet_wrap(~year) #density:연속형 변수 하나를 집계. 1차원 


#detecting seasonality patterns

rdata2_ulsan_14 %>%
    select(month, ulsan) %>%
    group_by(month) %>%
    summarise(ulsan=mean(ulsan)) %>%
    ggplot(aes(x=month, y=ulsan))+
    geom_line()

#let's see if the seasonality pattern is maintained each year. 

rdata2_ulsan_14 %>%
    select(year, month, ulsan) %>%
    group_by(year, month) %>%
    summarise(ulsan = mean(ulsan)) %>%
    ggplot(aes(x=month, y=ulsan)) +
    geom_line() +
    facet_wrap(~as.factor(year))

#measuring standard deviation per month through each year

rdata2_ulsan_14 %>%
    select(year, month, ulsan) %>%
    group_by(year, month) %>%
    summarise(sd = sd(ulsan)) %>%
    ggplot(aes(x=month, y=sd)) +
    geom_point(aes(size=sd, color="pink")) +
    geom_segment(aes(x=month,
                     xend=month,
                     y=min(sd),
                     yend=max(sd)),
                 linetype="dashed",
                 size=0.1)+
    coord_flip()+
    facet_wrap(~year)
    
#Trend/autocorrelation

library(forecast)
library(timeSeries)
library(lubridate)


rdata2_ulsan_14 %>%
    select(date, ulsan) %>% 
    ts() %>%
    mstl() %>%
    autoplot()


rdata2_ulsan_14 %>%
    select(ulsan) %>% 
    ts() %>%
    ggtsdisplay()


glimpse((rdata2_ulsan_14))
rdata2_ulsan_14$date <- as.Date(rdata2_ulsan_14$date)


rdata2_ulsan_14_lag <- rdata2_ulsan_14 %>%
    select(date,ulsan) %>%
    mutate(ulsan_daily_change = ulsan - dplyr::lag(ulsan,1),
           lagged_ulsan = dplyr::lag(ulsan,1))

rdata2_ulsan_14_lag %>% 
    ggplot(aes(x=date, y=ulsan_daily_change))+
    geom_line()


rdata2_ulsan_14_lag %>%
    select(ulsan_daily_change) %>%
    ts() %>%
    ggtsdisplay()
    

rdata2_ulsan_14_lag %>%
    select(ulsan_daily_change) %>%
    ts() %>%
    mstl() %>%
    autoplot()

rdata2_ulsan_14_lag %>%
    select(ulsan_daily_change) %>%
    ts() %>%
    auto.arima() %>%
    forecast(h=30) %>%
    autoplot()




