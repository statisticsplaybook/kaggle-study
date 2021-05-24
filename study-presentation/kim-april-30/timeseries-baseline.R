#### 1. 데이터 전처리 ####
pacman::p_load(lubridate,
               reshape2,      # 데이터 전처리용 rename이 dplyr와 충돌
               tidyverse,     # 데이터 전처리용
               skimr,
               tibbletime,    # 시계열 데이터 분석용
               timetk,        # 시계열 데이터 전처리용
               imputeTS,      # 시계열 데이터 결측치 채우기
               anomalize,     # 시계열 데이터 이상치 탐지
               tidymodels,    # 머신러닝용
               forecast       # 시계열 모델 및 정확도 확인
)    


#### 가. univariate ####
rdata <- read.csv("data/energy/energy.csv") %>% mutate(time=ymd_hms(time)) %>% arrange(time)
submission <- read.csv("data/energy/sample_submission.csv")

# 결측치 처리
rdata %>% is.na() %>% colSums()
rdata <- na_kalman(rdata, model="StructTS", smooth=T)


#### 나. multivariate ####
#### _1) 당진 ####
rdata1 <- read.csv("data/energy/OBS_2015.csv") %>% 
    bind_rows(read.csv("data/energy/OBS_2016.csv")) %>% 
    bind_rows(read.csv("data/energy/OBS_2017.csv")) %>% 
    bind_rows(read.csv("data/energy/OBS_2018.csv")) %>% 
    bind_rows(read.csv("data/energy/OBS_2019.csv")) %>% 
    bind_rows(read.csv("data/energy/OBS_2020.csv")) %>% 
    bind_rows(read.csv("data/energy/OBS_2021.csv")) %>% 
    select(-c(1:2)) %>% rename(time=1, 기온=2, 강수=3, 풍속=4, 풍향=5, 습도=6, 증기압=7,
                               이슬점=8, 기압=9, 해면기압=10, 일조=11, 일사=12,
                               적설=13, 전운량=14, 운형=15, 시정=16, 지면온도=17) %>% 
    as_tibble() %>% mutate(time=ymd_hm(time)) %>% select(-c(운형, 적설)) %>% 
    right_join(data.frame(time=seq(as.POSIXct("2018-03-01 01:00", tz="gmt"), 
                                   as.POSIXct("2021-04-25 00:00", tz="gmt"), by="hour"))) %>% 
    left_join(read.csv("data/energy/energy.csv") %>% mutate(time=ymd_hms(time))) %>% arrange(time) %>% 
    mutate(일사=ifelse(is.na(일사), 0, 일사), 강수=ifelse(is.na(강수), 0, 강수))

# 상관관계가 높은 변수, 결측치가 적은 변수 선택
rdata1 %>% mutate(기온=ifelse(dangjin==0, NA, 기온), 강수=ifelse(dangjin==0, NA, 강수),
                    풍속=ifelse(dangjin==0, NA, 풍속), 풍향=ifelse(dangjin==0, NA, 풍향),
                    습도=ifelse(dangjin==0, NA, 습도), 증기압=ifelse(dangjin==0, NA, 증기압),
                    이슬점=ifelse(dangjin==0, NA, 이슬점), 기압=ifelse(dangjin==0, NA, 기압),
                    해면기압=ifelse(dangjin==0, NA, 해면기압), 일조=ifelse(dangjin==0, NA, 일조),
                    일사=ifelse(dangjin==0, NA, 일사), 전운량=ifelse(dangjin==0, NA, 전운량), 
                    시정=ifelse(dangjin==0, NA, 시정), 지면온도=ifelse(dangjin==0, NA, 지면온도)) %>% 
    select(기온:지면온도, dangjin) %>% cor(use="complete.obs") %>% 
    data.frame() %>% arrange(desc(abs(dangjin))) # 일사량, 습도, (시정, 전운량), (강수, 풍속, 풍향)
rdata1 %>% select(일사, 습도, 시정, 전운량, 강수, 풍속, 풍향) %>% is.na() %>% colSums()
rdata1 <- rdata1 %>% select(time, 일사, 습도, 시정, 강수, 풍속, 풍향, dangjin_floating:ulsan)

# 결측치 확인하기
rdata1 %>% is.na() %>% colSums()
ggplot_na_gapsize(rdata1$습도, include_total = F)                           # 연속된 결측치의 개수 비교
ggplot_na_intervals(rdata1$습도, interval_size=1000, color_missing="red")   # 결측치들의 위치
ggplot_na_distribution(x = rdata1$습도, x_axis_labels = rdata1$time)        # 결측치의 위치와 개수 등
rdata1 %>% select(time, 습도) %>% filter(is.na(습도))

# 결측치 채우기
rdata1 <- na_kalman(rdata1, model="StructTS", smooth=T)

# 결측치 채운 것 보정하기
rdata1 %>% filter(dangjin<0)
rdata1 <- rdata1 %>% mutate(dangjin_floating=ifelse(dangjin_floating<0, 0, dangjin_floating),
                            dangjin_warehouse=ifelse(dangjin_warehouse<0, 0, dangjin_warehouse),
                            dangjin=ifelse(dangjin<0, 0, dangjin),
                            ulsan=ifelse(ulsan<0, 0, ulsan))


#### _2) 울산 ####
rdata2 <- read.csv("data/energy/OBS_ulsan2015.csv") %>% 
    bind_rows(read.csv("data/energy/OBS_ulsan2016.csv")) %>% 
    bind_rows(read.csv("data/energy/OBS_ulsan2017.csv")) %>% 
    bind_rows(read.csv("data/energy/OBS_ulsan2018.csv")) %>% 
    bind_rows(read.csv("data/energy/OBS_ulsan2019.csv")) %>% 
    bind_rows(read.csv("data/energy/OBS_ulsan2020.csv")) %>% 
    bind_rows(read.csv("data/energy/OBS_ulsan2021.csv")) %>% 
    select(-c(1:2)) %>% rename(time=1, 기온=2, 강수=3, 풍속=4, 풍향=5, 습도=6, 증기압=7,
                               이슬점=8, 기압=9, 해면기압=10, 일조=11, 일사=12,
                               적설=13, 전운량=14, 운형=15, 시정=16, 지중온도=17) %>% 
    as_tibble() %>% mutate(time=ymd_hm(time)) %>% select(-c(운형, 적설)) %>% 
    right_join(data.frame(time=seq(as.POSIXct("2018-03-01 01:00", tz="gmt"), 
                                   as.POSIXct("2021-04-25 00:00", tz="gmt"), by="hour"))) %>% 
    left_join(read.csv("data/energy/energy.csv") %>% mutate(time=ymd_hms(time))) %>% arrange(time) %>% 
    mutate(일사=ifelse(is.na(일사), 0, 일사), 강수=ifelse(is.na(강수), 0, 강수))
rdata2 <- rdata2 %>% select(time, 일사, 습도, 시정, 강수, 풍속, 풍향, ulsan)

# 결측치 채우기
rdata2 %>% is.na() %>% colSums()
rdata2 <- na_kalman(rdata2, model="StructTS", smooth=T)

# 결측치 채운 것 보정하기
rdata2 %>% filter(ulsan<0)
rdata2 <- rdata2 %>% mutate(ulsan=ifelse(ulsan<0, 0, ulsan))


#### 2. EDA ####
#### 가. 발전소별 비교 ####
# 시간대별 평균 발전량 비교 (당진>수상>창고>울산)
rdata %>% select(time, dangjin, dangjin_warehouse, dangjin_floating, ulsan) %>% 
    melt(id.var="time") %>% mutate(hour=hour(time)) %>% 
    group_by(variable, hour) %>% summarise(mean=mean(value, na.rm=T)) %>% 
    ggplot(aes(x=hour, y=mean, group=variable, color=variable)) +
    geom_line()
# 장소별 차이가 크다. 장소별로 구분해서 예측해야할 것 같다.


# 시간별 월에 따른 발전량 비교 (당진)
rdata %>% select(time, dangjin) %>% mutate(month=month(time), hour=as.factor(hour(time))) %>% 
    group_by(month, hour) %>% summarise(mean=mean(dangjin, na.rm=T)) %>%   # dangjin_floating, dangjin_warehouse
    filter(hour==10 | hour==11 | hour==12 | hour==13 | hour==14) %>% 
    ggplot(aes(x=month, y=mean, group=hour, color=hour)) +
    geom_line()

# 시간별 월에 따른 발전량 비교 (울산)
rdata %>% select(time, ulsan) %>% mutate(month=month(time), hour=as.factor(hour(time))) %>% 
    group_by(month, hour) %>% summarise(mean=mean(ulsan, na.rm=T)) %>% 
    filter(hour==10 | hour==11 | hour==12 | hour==13 | hour==14) %>% 
    ggplot(aes(x=month, y=mean, group=hour, color=hour)) +
    geom_line()
# 월별 발전량도 장소별로 차이가 크다. 울산은 9월 10월에 구름이 많이 끼는 듯하다.


# 년도별 월에 따른 발전량 비교 (당진)
rdata %>% select(time, dangjin) %>% mutate(year=as.factor(year(time)), month=month(time)) %>% 
    group_by(month, year) %>% summarise(mean=mean(dangjin, na.rm=T)) %>%   # dangjin_floating, dangjin_warehouse
    filter(!(year==2021 & month==2)) %>% 
    ggplot(aes(x=month, y=mean, group=year, color=year)) +
    geom_point() + geom_line()

# 연도별 같은 날 시간에 따른 발전량 비교 (당진)
rdata %>% mutate(year=year(time), month=month(time), day=day(time), hour=hour(time)) %>% 
    filter(month==1, day==4) %>% 
    ggplot(aes(x=hour, y=dangjin, color=as.factor(year)))+
    geom_line()
# 연도별 같은 날 같은 시간이라도 발전량에는 차이가 매우 크다. 단순 시계열이 아닌 다변량 시계열로 분석해야할 듯 하다.



#### 3. 베이스라인 ####
#### 가. univariate stlf ####
#### _1) 당진 발전소 ####
rdata %>% pull(dangjin) %>% ts() %>% autoplot()
data <- ts(rdata$dangjin, frequency = 365*24, start=c(2018, (31+28)*24+1))
system.time(dangjin.stlf <- stlf(data, h=nrow(submission)))    

#### _2) 당진 수상발전소 ####
rdata %>% pull(dangjin_floating) %>% ts() %>% autoplot()  # 중간에 몇일 동안 0인 경우가 있지만 그냥 넘어가자.
data <- rdata$dangjin_floating %>% ts(frequency = 365*24, start=c(2018, (31+28+29)*24+1))
system.time(floating.stlf <- stlf(data, h=nrow(submission))) 

#### _3) 당진 창고발전소 ####
rdata %>% pull(dangjin_warehouse) %>% ts() %>% autoplot() # 3월 29일까지 결측치가 제법 있다. 빼자.
data <- rdata %>% slice(-c(1:696)) %>% pull(dangjin_warehouse) %>% 
    ts(frequency = 365*24, start=c(2018, (31+28+29)*24+1))
system.time(warehouse.stlf <- stlf(data, h=nrow(submission))) 

#### _4) 울산 발전소 ####
rdata %>% pull(ulsan) %>% ts() %>% autoplot()             # 중간에 몇일 동안 0인 경우가 있지만 그냥 넘어가자.
data <- ts(rdata$ulsan, frequency = 365*24, start=c(2018, (31+28)*24+1))
system.time(ulsan.stlf <- stlf(data, h=nrow(submission))) 

#### _5) 제출 ####
names(submission <- read.csv("data/energy/sample_submission.csv"))
submission <- submission %>% 
    mutate(dangjin_floating=floating.stlf$mean) %>% 
    mutate(dangjin_warehouse=warehouse.stlf$mean) %>% 
    mutate(dangjin=dangjin.stlf$mean) %>% 
    mutate(ulsan=ulsan.stlf$mean) %>% 
    melt(id.var="time") %>% mutate(value=ifelse(value<50, 0, value)) %>% 
    acast(time~variable) %>% data.frame() %>% 
    rownames_to_column("time")

write.csv(submission, "uni_stlf_ets.csv", row.names=F)  # 15.08483 (55등)


#### 나. multivariate stlf ####
skimr::skim(train <- read.csv("data/energy/energy.csv") %>% mutate(time=ymd_hms(time)))
skimr::skim(train <- rdata1 %>% slice(1:nrow(train)))

test <- read.csv("data/energy/sample_submission.csv")
test <- setdiff(rdata1, train) %>% slice(1:nrow(test))


#### _1) 당진 발전소 ####
data <- ts(train$dangjin, frequency = 365*24, start=c(2018, (31+28)*24+1))
BoxCox.lambda(data)
system.time(dangjin.stlfax <- stlf(data, h=nrow(test), 
                               method="arima", lambda=4.102259e-05, 
                               xreg=as.matrix(train[2:7]),
                               newxreg=as.matrix(test[2:7])))                                    # 19초

system.time(dangjin.nnetar <- nnetar(data, xreg=as.matrix(train[2:7])))                          # 356초
system.time(dangjin.nnetar <- forecast(dangjin.nnetar, h=nrow(test), xreg=as.matrix(test[2:7]))) # 2.44초

system.time(dangjin.arimax <- auto.arima(data, seasonal=TRUE, xreg=as.matrix(train[2:7])))       # 3.3초
system.time(dangjin.arimax <- forecast(dangjin.arimax, h=nrow(test), xreg=as.matrix(test[2:7]))) # 0초


#### _2) 수상 발전소 ####
data <- ts(train$dangjin_floating, frequency = 365*24, start=c(2018, (31+28)*24+1))
BoxCox.lambda(data)
system.time(floating.stlfax <- stlf(data, h=nrow(test), 
                                   method="arima", lambda=0.8605489, 
                                   xreg=as.matrix(train[2:7]),
                                   newxreg=as.matrix(test[2:7])))                                  # 6초

system.time(floating.nnetar <- nnetar(data, lambda=0.8605489, xreg=as.matrix(train[2:7])))         # 355.42초
system.time(floating.nnetar <- forecast(floating.nnetar, h=nrow(test), xreg=as.matrix(test[2:7]))) # 2.74초

system.time(floating.arimax <- auto.arima(data, seasonal=TRUE, xreg=as.matrix(train[2:7])))        # 29.42초
system.time(floating.arimax <- forecast(floating.arimax, h=nrow(test), xreg=as.matrix(test[2:7]))) # 0초


#### _3) 창고 발전소 ####
data <- ts(train$dangjin_warehouse, frequency = 365*24, start=c(2018, (31+28)*24+1))
BoxCox.lambda(data)
system.time(warehouse.stlfax <- stlf(data, h=nrow(test), 
                                    method="arima", #lambda=4.102259e-05, 
                                    xreg=as.matrix(train[2:7]),
                                    newxreg=as.matrix(test[2:7])))     # 15초

system.time(warehouse.nnetar <- nnetar(data, MaxNWts=1353, decay = 0.0005, #lambda=4.102259e-05, 
                                      xreg=as.matrix(train[2:7])))     # 3240.77초
system.time(warehouse.nnetar <- forecast(warehouse.nnetar, h=nrow(test), xreg=as.matrix(test[2:7]))) # 2.5초
# size(hidden node 수), maxit(반복횟수, default=100)-클수록 느려진다. 
# decay = 0.0005(overfitting을 피하기 위해 사용하는 weight decay parameter, 디폴트는 0이다.), 
# rang = 0.1 (Initial random weights on [-rang, rang]. default 0.5)
# MaxNWts=1000(커질수록 느려진다. "too many (1353) weights"라는 에러가 나면 딱 이 숫자만큼 늘려준다.),  <- 중요사항
# abstol = 1.0e-4 (이정도 적합되면 중단), reltol = 1.0e-8 (적합도가 이것보다 덜 줄어들면 중지)
# Wts(초기값. 설정하지 않으면 random), mask(default all), 
# linout = FALSE(switch for linear output units. Default logistic output units.), 
# entropy = FALSE (switch for entropy (= maximum conditional likelihood) fitting. Default by leastsquares.), 
# softmax = FALSE (switch for softmax (log-linear model) and maximum conditional likelihood fitting.), 
# linout, entropy, softmax는 상호 배타적임
# censored = FALSE (softmax의 변형), skip = FALSE (입력과 출력 스킵 레이어 추가 여부),  
# Hess = FALSE, trace = TRUE

system.time(warehouse.arimax <- auto.arima(data, seasonal=TRUE, xreg=as.matrix(train[2:7])))              # 30.84초
system.time(warehouse.arimax <- forecast(warehouse.arimax, h=nrow(submission), xreg=as.matrix(test[2:7])))# 0초


#### _4) 울산발전소 ####
skimr::skim(train <- read.csv("data/energy/energy.csv") %>% mutate(time=ymd_hms(time)))
skimr::skim(train <- rdata2 %>% slice(1:nrow(train)))

test <- read.csv("data/energy/sample_submission.csv")
test <- setdiff(rdata2, train) %>% slice(1:nrow(test))

data <- ts(train$ulsan, frequency = 365*24, start=c(2018, (31+28)*24+1))
BoxCox.lambda(data)
system.time(ulsan.stlfax <- stlf(data, h=nrow(test)))     # 15초

system.time(ulsan.nnetar <- nnetar(data, #lambda=4.102259e-05, 
                                       xreg=as.matrix(train[2:7])))     # 262.62초
system.time(ulsan.nnetar <- forecast(ulsan.nnetar, h=nrow(test), xreg=as.matrix(test[2:7])))      # 2.61초


system.time(ulsan.arimax <- auto.arima(data, seasonal=TRUE, xreg=as.matrix(train[3:7]), num.cores=15))  # 무한반복
system.time(ulsan.arimax <- forecast(ulsan.arimax, h=nrow(test), xreg=as.matrix(test[3:7])))      # 0초


#### _5) 제출 ####
names(submission <- read.csv("data/sample_submission.csv"))
submission <- submission %>% 
    mutate(dangjin_floating=floating.stlfax$mean) %>% 
    mutate(dangjin_warehouse=warehouse.stlf$mean) %>% 
    mutate(dangjin=dangjin.stlf$mean) %>% 
    mutate(ulsan=ulsan.stlf$mean) %>% 
    melt(id.var="time") %>% mutate(value=ifelse(value<50, 0, value)) %>% 
    acast(time~variable) %>% data.frame() %>% 
    rownames_to_column("time")

write.csv(submission, "result/multi_stlf_arimax.csv", row.names=F)  # 14.45256 (65등)

names(submission <- read.csv("data/sample_submission.csv"))
submission <- submission %>% 
    mutate(dangjin_floating=floating.arimax$mean) %>% 
    mutate(dangjin_warehouse=warehouse.arimax$mean) %>% 
    mutate(dangjin=dangjin.arimax$mean) %>% 
    mutate(ulsan=ulsan.nnetar$mean) %>% 
    mutate(dangjin_floating=ifelse(dangjin_floating<50, 0, dangjin_floating)) %>% 
    mutate(dangjin_warehouse=ifelse(dangjin_warehouse<50, 0, dangjin_warehouse)) %>% 
    mutate(dangjin=ifelse(dangjin<50, 0, dangjin)) %>% 
    mutate(ulsan=ifelse(ulsan<50, 0, ulsan))

write.csv(submission, "result/multi_arimax.csv", row.names=F)  # 11.96 (54등)

names(submission <- read.csv("data/sample_submission.csv"))
submission <- submission %>% 
    mutate(dangjin_floating=floating.nnetar$mean) %>% 
    mutate(dangjin_warehouse=warehouse.nnetar$mean) %>% 
    mutate(dangjin=dangjin.nnetar$mean) %>% 
    mutate(ulsan=ulsan.nnetar$mean) %>% 
    melt(id.var="time") %>% mutate(value=ifelse(value<50, 0, value)) %>% 
    acast(time~variable) %>% data.frame() %>% 
    rownames_to_column("time")

write.csv(submission, "result/multi_nnetar.csv", row.names=F)  # 8.05 (7등)

# stlf보다 stlfax가 더 좋다.
data.frame(stlf=warehouse.stlf$mean, stlfax=warehouse.stlfax$mean, nnetar=warehouse.nnetar$mean, arimax=warehouse.arimax) %>% 
    mutate_at(vars(1:2), round, 3) %>% slice(1:24*7) %>% 
    mutate(time=1:nrow(.)) %>% melt(id.var="time") %>% 
    mutate(value=ifelse(value<50, 0, value)) %>% 
    ggplot(aes(x=time, y=value, color=variable)) +
    geom_point(aes(shape=variable, color=variable))+
    geom_line()

# stlf보다 stlfax가 더 좋고 그것보다 nnetar이 더 좋다.
data.frame(stlf=floating.stlf$mean, stlfax=floating.stlfax$mean, nnetar=floating.nnetar$mean) %>% 
    mutate_at(vars(1:2), round, 3) %>% slice(1:24*7) %>% 
    mutate(time=1:nrow(.)) %>% melt(id.var="time") %>% 
    mutate(value=ifelse(value<50, 0, value)) %>% 
    ggplot(aes(x=time, y=value, color=variable)) +
    geom_point(aes(shape=variable, color=variable))+
    geom_line()
