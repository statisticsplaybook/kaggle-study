source('R/util.R')
source('R/postprocess.R')

library(forecast)
library(tidyverse)  # rename이 겹치기 때문에 tidyverse를 맨 뒤에 써줘야 한다.

train <- raw.train()
test <- raw.test()

df <- fit <- result <- list()



#### 1. EDA ####
unique(train$Dept)               # 81개 Department
unique(train$Store)              # 45개 Store
unique(train$Date) %>% length()  # 143주

unique(test$Dept)                # 81개 Department
unique(test$Store)               # 45개 Store
unique(test$Date) %>% length()   # 39주

train %>% group_by(Store, Dept) %>% count() %>% filter(n<52) # 340개 52주 미만

train %>% filter(Store==2, Dept==77)
test %>% filter(Store==2, Dept==77) 

test %>% group_by(Store, Dept) %>% count() %>% filter(n<10)  # 10 미만 157개
test %>% filter(Store==1, Dept==45)                          # 날짜도 제각각
test %>% filter(Dept==47)



#### 2. basic ####
# Department 내에 store가 있고 store 매출액이 시계열이다!
# 완성형의 깡통 데이터 만들기(날짜를 store 수만큼 생성, store도 날짜 수만큼)
(df$train <- data.frame(Date=rep(unique(train$Date), 45),          
                        Store=rep(unique(train$Store),each=143)) %>% as_tibble)
# 6435개 (1개 부서당) 143일*45개 store

# test 날짜를 store 수만큼 생성하고 store를 날짜 수 만큼 반복
(df$forecast <- data.frame(Date=rep(unique(test$Date), 45),
                           Store=rep(unique(test$Store),each=39)) %>% as_tibble)                                                      # 1755개 (1개 부서당) 39일*45개 store 


#### 가. TSML ####
# pred <- grouped.forecast(train, test, "tslm.basic") 코드 내용 설명
(df$tslm.pred <- test %>% mutate(Weekly_Sales=0) %>% as_tibble())

for(d in  unique(test$Dept)){
    print(paste('dept:', d))    # 47로 예시
    
    # 완성형의 train 데이터 만들기
    df$tr.d <- left_join(df$train, train[train$Dept==d, c(1,3:4)]) %>% as_tibble()
    df$tr.d <- cast(df$tr.d, Date ~ Store)  # reshape의 cast는 Store가 살아있다.
    df$tr.d[is.na(df$tr.d)] <- 0
    
    # 완성형의 test 데이터 만들기 
    df$fc.d <- df$forecast
    df$fc.d$Weekly_Sales <- 0
    df$fc.d <- cast(df$fc.d, Date ~ Store)
    
    # tslm으로 예측해서 test 데이터 채우기
    for(j in 2:ncol(df$tr.d)){
        fit$uni.model <- tslm(ts(df$tr.d[, j], frequency=52) ~ trend + season)
        df$fc.d[, j] <- forecast(fit$uni.model, h=39)$mean %>% as.numeric()
    }
    
    # 실제 test에 있는 해당 Dept의 Store 값과 일치되는 조건의 날짜의 값 채우기
    result$uni.result <- melt(df$fc.d) %>% as_tibble() %>% rename(Weekly_Sales=2)
    df$tslm.pred$Weekly_Sales[df$tslm.pred$Dept==d] <- df$tslm.pred %>% 
        filter(Dept==d) %>% select(Store, Date) %>% 
        left_join(result$uni.result) %>% pull(Weekly_Sales)
}
# write.submission(df$tslm.pred)  # 3007.71 (139등)

train %>% filter(Store==1, Dept==10) %>% 
    mutate(week=lubridate::week(Date)) %>% as_tibble() %>% tail()
# train 데이터는 2010-02-05(6번째 주)에서 2012-10-26(43번째 주)까지이다.

df$tslm.pred %>% filter(Store==1, Dept==10) %>% 
    mutate(week=lubridate::week(Date)) %>% as_tibble() %>% tail()
# test 데이터는 2012-11-02(44번째 주)에서 2013-07-26(30번째 주)까지이다.

train %>% bind_rows(df$tslm.pred) %>% 
    filter(Store==34, Dept==65) %>% select(Weekly_Sales) %>% 
    ts(.$Weekly_Sales, frequency = 52, start = c(2010,6), end = c(2013,30)) %>% 
    ggseasonplot(year.labels=T, col=1:11)


# pred <- postprocess(train, pred, shift=2.5) 코드 내용
# Weekly_Sales를 가진 값
(df$tslm.shift.pred <- df$tslm.pred %>% mutate(Weekly_Sales=0) %>% as_tibble())
for(d in unique(test$Dept)){
    df$threshold <- 1.1
    df$shift <- 2.5
    print(paste('dept:', d))
    
    # 완성형의 train 데이터 만들기
    df$tr.d <- left_join(df$train, train[train$Dept==d, c(1, 3:4)])
    df$tr.d <- cast(df$tr.d, Date ~ Store) 
    df$tr.d[is.na(df$tr.d)] <- 0
    
    # 완성형의 test 데이터 만들기
    df$fc.d <- left_join(df$forecast, df$tslm.pred[df$tslm.pred$Dept==d, c(1,3,5)])
    df$fc.d <- cast(df$fc.d, Date ~ Store)
    
    # 48,52주 예측값보다 49:51주 예측값이 10%이상 큰 경우
    df$s <- ts(rep(0,39), frequency=52, start=c(2012,44))
    df$holiday <- df$fc.d[cycle(df$s) %in% 48:52, 2:46]
    df$baseline <- mean(rowMeans(df$holiday[c(1, 5), ], na.rm=TRUE))
    df$surge <- mean(rowMeans(df$holiday[2:4, ], na.rm=TRUE))
    df$holiday[is.na(df$holiday)] <- 0
    
    if(is.finite(df$surge/df$baseline) & df$surge/df$baseline > df$threshold){
        df$shifted.sales <- ((7-df$shift)/7) * df$holiday
        df$shifted.sales[2:5, ] <- df$shifted.sales[2:5, ] + (df$shift/7) * df$holiday[1:4, ]
        df$shifted.sales[1, ] <- df$holiday[1, ]
        df$fc.d[cycle(df$s) %in% 48:52, 2:46] <- df$shifted.sales
    }
    
    # 실제 test에 있는 해당 Dept의 Store 값 채우기
    result$uni.result <- melt(df$fc.d) %>% as_tibble() %>% rename(Weekly_Sales=2)
    df$tslm.shift.pred$Weekly_Sales[df$tslm.shift.pred$Dept==d] <- df$tslm.pred %>% 
        mutate(Weekly_Sales=0) %>% filter(Dept==d) %>% select(Store, Date) %>% 
        left_join(result$uni.result) %>% pull(Weekly_Sales)
}
write.submission(df$tslm.shift.pred)  # 2606.86, 20등

# shift를 적용하기 전
train %>% bind_rows(df$tslm.pred) %>% 
    filter(Store==6, Dept==48) %>% select(Weekly_Sales) %>% 
    ts(.$Weekly_Sales, frequency = 52, start = c(2010,6), end = c(2013,30)) %>% 
    ggseasonplot(year.labels=T, col=1:11)

# shift를 적용한 후
train %>% bind_rows(df$tslm.shift.pred) %>% 
    filter(Store==6, Dept==48) %>% select(Weekly_Sales) %>% 
    ts(.$Weekly_Sales, frequency = 52, start = c(2010,6), end = c(2013,30)) %>% 
    ggseasonplot(year.labels=T, col=1:11)



#### 나. SNAIVE ####
(df$snaive.pred <- test %>% mutate(Weekly_Sales=0) %>% as_tibble())      

for(d in  unique(test$Dept)){
    print(paste('dept:', d))                     # 47로 예시
    
    # 완성형의 train 데이터 만들기
    df$tr.d <- left_join(df$train, train[train$Dept==d, c(1,3,4)]) %>% as_tibble()
    df$tr.d <- cast(df$tr.d, Date ~ Store)  
    df$tr.d[is.na(df$tr.d)] <- 0
    
    # 완성형의 test 데이터 만들기 
    df$fc.d <- df$forecast
    df$fc.d$Weekly_Sales <- 0
    df$fc.d <- cast(df$fc.d, Date ~ Store)
    
    # SNAIVE로 예측해서 test 데이터 채우기
    for(j in 2:ncol(df$tr.d)){
        fit$uni.tbats <- snaive(ts(df$tr.d[, j], frequency=52), h=nrow(df$fc.d))
        df$fc.d[, j] <- forecast(fit$uni.tbats, h=nrow(df$fc.d))$mean %>% as.numeric()
    }
    
    # 실제 test에 있는 해당 Dept의 Store 값과 일치되는 조건의 날짜의 값 채우기
    result$uni.result <- melt(df$fc.d) %>% as_tibble() %>% rename(Weekly_Sales=2)
    df$snaive.pred$Weekly_Sales[df$snaive.pred$Dept==d] <- df$snaive.pred %>% 
        filter(Dept==d) %>% select(Store, Date) %>% 
        left_join(result$uni.result) %>% pull(Weekly_Sales)
}

# write.submission(df$snaive.pred)  # 2943.93 (47등)

df$snaive.shift.pred <- postprocess(train, df$snaive.pred, shift=2) 
#write.submission(df$snaive.shift.pred) # shift=2.5 : 2647.20  23등
write.submission(df$snaive.shift.pred)  # shift=2   : 2674.51  25등



#### 다. product ####
df$product.pred <- test %>% mutate(Weekly_Sales=0) %>% as_tibble()   
system.time(
    for(d in  unique(test$Dept)){
        print(paste('dept:', d))                     # 47로 예시
        
        # 완성형의 train 데이터 만들기
        df$tr.d <- left_join(df$train, train[train$Dept==d, c(1,3,4)]) %>% as_tibble()
        df$tr.d <- cast(df$tr.d, Date ~ Store)  
        df$tr.d[is.na(df$tr.d)] <- 0
        
        # 완성형의 test 데이터 만들기 
        df$fc.d <- df$forecast
        df$fc.d$Weekly_Sales <- 0
        df$fc.d <- cast(df$fc.d, Date ~ Store)
        
        # product로 test 데이터 채우기
        df$tr <- df$tr.d[nrow(df$tr.d) - (52:1) + 1,]  # 마지막 52주만 선택
        df$levels <- colMeans(df$tr[,2:ncol(df$tr)])   # store별 평균 구하기
        df$profile <- rowMeans(df$tr[,2:ncol(df$tr)])  # 날짜별 평균 구하기
        df$overall <- mean(df$levels)                  # store별 평균의 전체 평균
        df$pred <- matrix(df$profile, ncol=1) %*% matrix(df$levels, nrow=1)/df$overall
        df$fc.d[,2:ncol(df$fc.d)] <- df$pred[1:nrow(df$fc.d),]
        
        # 실제 test에 있는 해당 Dept의 Store 값과 일치되는 조건의 날짜의 값 채우기
        result$uni.result <- melt(df$fc.d) %>% as_tibble() %>% rename(Weekly_Sales=2)
        df$product.pred$Weekly_Sales[df$product.pred$Dept==d] <- df$product.pred %>% 
            filter(Dept==d) %>% select(Store, Date) %>% 
            left_join(result$uni.result) %>% pull(Weekly_Sales)
    }
)

# write.submission(df$product.pred)  # 3012.93  (143등)

df$product.shift.pred <- postprocess(train, df$product.pred, shift=2)
#write.submission(df$product.shift.pred) # shift=2.5 : 2742.49 (36등)
write.submission(df$product.shift.pred)  # shift=2   : 2767.60 (44등)


#### 다. TBATS ####
(df$tbats.pred <- test %>% mutate(Weekly_Sales=0) %>% as_tibble())                

for(d in  unique(test$Dept)){
    print(paste('dept:', d))                     # 47로 예시
    
    # 완성형의 train 데이터 만들기
    df$tr.d <- left_join(df$train, train[train$Dept==d, c(1,3,4)]) %>% as_tibble()
    df$tr.d <- cast(df$tr.d, Date ~ Store)      
    df$tr.d[is.na(df$tr.d)] <- 0
    
    # 완성형의 test 데이터 만들기 
    df$fc.d <- df$forecast
    df$fc.d$Weekly_Sales <- 0
    df$fc.d <- cast(df$fc.d, Date ~ Store)
    
    # tslm으로 예측해서 test 데이터 채우기
    for(j in 2:ncol(df$tr.d)){
        fit$uni.tbats <- tbats(ts(df$tr.d[, j], frequency=52))
        df$fc.d[, j] <- forecast(fit$uni.tbats, h=nrow(df$fc.d))$mean %>% as.numeric()
    }
    
    # 실제 test에 있는 해당 Dept의 Store 값과 일치되는 조건의 날짜의 값 채우기
    result$uni.result <- melt(df$fc.d) %>% as_tibble() %>% rename(Weekly_Sales=2)
    df$tbats.pred$Weekly_Sales[df$tbats.pred$Dept==d] <- df$tbats.pred %>% 
        filter(Dept==d) %>% select(Store, Date) %>% 
        left_join(result$uni.result) %>% pull(Weekly_Sales)
}
# write.submission(df$tbats.pred)  # 3419.06 (274등)

df$tbats.shift.pred <- postprocess(train, df$tbats.pred, shift=2.5) 
write.submission(df$tbats.shift.pred)   # shift=2.5 : 3335.73, 261등
# write.submission(df$tbats.shift.pred) # shift=2   : 3349.41, 263등



#### 라. NNETAR ####
(df$nnetar.pred <- test %>% mutate(Weekly_Sales=0) %>% as_tibble())                

for(d in  unique(test$Dept)){
    print(paste('dept:', d))                     # 47로 예시
    
    # 완성형의 train 데이터 만들기
    df$tr.d <- left_join(df$train, train[train$Dept==d, c(1,3,4)]) %>% as_tibble()
    df$tr.d <- cast(df$tr.d, Date ~ Store)      
    df$tr.d[is.na(df$tr.d)] <- 0
    
    # 완성형의 test 데이터 만들기 
    df$fc.d <- df$forecast
    df$fc.d$Weekly_Sales <- 0
    df$fc.d <- cast(df$fc.d, Date ~ Store)
    
    # tslm으로 예측해서 test 데이터 채우기
    for(j in 2:ncol(df$tr.d)){
        fit$uni.tbats <- nnetar(ts(df$tr.d[, j], frequency=52))
        df$fc.d[, j] <- forecast(fit$uni.tbats, h=nrow(df$fc.d))$mean %>% as.numeric()
    }
    
    # 실제 test에 있는 해당 Dept의 Store 값과 일치되는 조건의 날짜의 값 채우기
    result$uni.result <- melt(df$fc.d) %>% as_tibble() %>% rename(Weekly_Sales=2)
    df$nnetar.pred$Weekly_Sales[df$nnetar.pred$Dept==d] <- df$nnetar.pred %>% 
        filter(Dept==d) %>% select(Store, Date) %>% 
        left_join(result$uni.result) %>% pull(Weekly_Sales)
}
# write.submission(df$nnetar.pred)  # 3097.43 (203등)

df$nnetar.shift.pred <- postprocess(train, df$nnetar.pred, shift=2) 
# write.submission(df$nnetar.shift.pred) # shift=2.5 : 2975.50, 122등
write.submission(df$nnetar.shift.pred)   # shift=2   : 2971.45, 121등


#### 마. ARIMA ####
(df$arima.pred <- test %>% mutate(Weekly_Sales=0) %>% as_tibble())

for(d in  unique(test$Dept)){
    print(paste('dept:', d))                     # 47로 예시
    
    # 완성형의 train 데이터 만들기
    df$tr.d <- left_join(df$train, train[train$Dept==d, c(1,3,4)]) %>% as_tibble()
    df$tr.d <- cast(df$tr.d, Date ~ Store)  
    df$tr.d[is.na(df$tr.d)] <- 0
    
    # 완성형의 test 데이터 만들기 
    df$fc.d <- df$forecast
    df$fc.d$Weekly_Sales <- 0
    df$fc.d <- cast(df$fc.d, Date ~ Store)
    
    # tslm으로 예측해서 test 데이터 채우기
    for(j in 2:ncol(df$tr.d)){
        fit$uni.tbats <- auto.arima(ts(df$tr.d[, j], frequency=52))
        df$fc.d[, j] <- forecast(fit$uni.tbats, h=nrow(df$fc.d))$mean %>% as.numeric()
    }
    
    # 실제 test에 있는 해당 Dept의 Store 값과 일치되는 조건의 날짜의 값 채우기
    result$uni.result <- melt(df$fc.d) %>% as_tibble() %>% rename(Weekly_Sales=2)
    df$arima.pred$Weekly_Sales[df$arima.pred$Dept==d] <- df$arima.pred %>% 
        filter(Dept==d) %>% select(Store, Date) %>% 
        left_join(result$uni.result) %>% pull(Weekly_Sales)
}

# write.submission(df$arima.pred)  # 2784.12 (47등)

df$arima.shift.pred <- postprocess(train, df$arima.pred, shift=2.5) 
write.submission(df$arima.shift.pred) # shift=2.5 : 2483.65, 9등
write.submission(df$arima.shift.pred) # shift=2   : 2505.63, 10등









#### 바. STLF ####
(df$stlf.pred <- test %>% mutate(Weekly_Sales=0) %>% as_tibble())    

system.time(
    for(d in  unique(test$Dept)){
        print(paste('dept:', d))                     # 47로 예시
        
        # 완성형의 train 데이터 만들기
        df$tr.d <- left_join(df$train, train[train$Dept==d, c(1,3,4)]) %>% as_tibble()
        df$tr.d <- cast(df$tr.d, Date ~ Store)    
        df$tr.d[is.na(df$tr.d)] <- 0
        
        # 완성형의 test 데이터 만들기 
        df$fc.d <- df$forecast
        df$fc.d$Weekly_Sales <- 0
        df$fc.d <- cast(df$fc.d, Date ~ Store)
        
        # tslm으로 예측해서 test 데이터 채우기
        for(j in 2:ncol(df$tr.d)){
            fit$uni.tbats <- stlf(ts(df$tr.d[, j], frequency=52), h=nrow(df$fc.d))
            df$fc.d[, j] <- forecast(fit$uni.tbats, h=nrow(df$fc.d))$mean %>% as.numeric()
        }
        
        # 실제 test에 있는 해당 Dept의 Store 값과 일치되는 조건의 날짜의 값 채우기
        result$uni.result <- melt(df$fc.d) %>% as_tibble() %>% rename(Weekly_Sales=2)
        df$stlf.pred$Weekly_Sales[df$stlf.pred$Dept==d] <- df$stlf.pred %>% 
            filter(Dept==d) %>% select(Store, Date) %>% 
            left_join(result$uni.result) %>% pull(Weekly_Sales)
    }
)
# write.submission(df$stlf.pred)  # 2815.94 (56등)

df$stlf.shift.pred <- postprocess(train, df$stlf.pred, shift=2.5) 
write.submission(df$stlf.shift.pred)   # shift=2.5 : 2436.05 (7등)
# write.submission(df$stlf.shift.pred) # shift=2   : 2471.43 (9등)


#### 사. basic average ####
# ARFIMA : error
# HOLTW  : error
# HOLT   : 5496.76       -> 5496.76
# ETS    : 5172.43       -> 5172.43
# NAIVE  : 4883.59       -> 4883.59
# RWF    : 4883.59       -> 4883.59
# SES    : 4646.17       -> 4646.17
# TBATS  : 3419.06       -> 3335.73
# NNETAR : 3097.43       -> 2971.45 (121등)
# SNIVE  : 2943.93       -> 2647.20 (23등)
# TSLM   : 3007.71       -> 2606.86 (20등)
# ARIMA  : 2784.12(47등) -> 2483.65 (9등)
# STLF   : 2815.94(56등) -> 2436.05 (7등)

# shift를 하지 않은 경우
pred <- read.csv(paste0(paths$data, 'sampleSubmission.csv'))
pred$Weekly_Sales <- pred$Weekly_Sales + 
    df$stlf.pred$Weekly_Sales/3 + 
    df$arima.pred$Weekly_Sales/3 + 
    df$tslm.pred$Weekly_Sales/3
write.submission(pred) # 2729.23 (36등)

# shift를 적용한 경우
pred <- read.csv(paste0(paths$data, 'sampleSubmission.csv'))
pred$Weekly_Sales <- pred$Weekly_Sales + 
    df$stlf.shift.pred$Weekly_Sales/3 + 
    df$arima.shift.pred$Weekly_Sales/3 + 
    df$tslm.shift.pred$Weekly_Sales/3
write.submission(pred) 
# TSLM, ARIMA, STLF (2379.58, 3등) 
# TSLM, SNIVE, product (2426.77, 5등)
result$uni.basic <- pred


#### 아. 베스트 average ####
pred$Weekly_Sales <- pred$Weekly_Sales/5
    result$uni.fa$Weekly_Sales/5 + 
    result$uni.sse$Weekly_Sales/5 + 
    result$uni.ssa$Weekly_Sales/5 + 
    result$uni.sn$Weekly_Sales/5
write.submission(pred) 
# TSLM, ARIMA, STLF 포함(2272.71, 1등)  
# TSLM, SNIVE, product 포함 (2250.60, 1등)




#### 2. svd 적용 ####
#### 가. stlf.svd.ets ####
(df$svd.stlf.pred <- test %>% mutate(Weekly_Sales=0) %>% as_tibble())                # 115,064개 데이터
df$n.comp <- 12

system.time(
    for(d in  unique(test$Dept)){
        print(paste('dept:', d))                     # 47로 예시
        
        # 완성형의 train 데이터 만들기
        df$tr.d <- left_join(df$train, train[train$Dept==d, c(1,3,4)]) %>% as_tibble()
        df$tr.d <- cast(df$tr.d, Date ~ Store)  
        df$tr.d[is.na(df$tr.d)] <- 0       
        
        # svd로 train 데이터 바꾸기
        df$z <- svd(df$tr.d[, 2:ncol(df$tr.d)], nu=df$n.comp, nv=df$n.comp)
        df$s <- diag(df$z$d[1:df$n.comp])
        df$tr.d[, 2:ncol(df$tr.d)] <- df$z$u %*% df$s %*% t(df$z$v)
        
        # 완성형의 test 데이터 만들기 
        df$fc.d <- df$forecast
        df$fc.d$Weekly_Sales <- 0
        df$fc.d <- cast(df$fc.d, Date ~ Store)
        
        #
        for(j in 2:ncol(df$tr.d)){
            fit$svd.stlf <- stlf(ts(df$tr.d[, j], frequency=52), h=nrow(df$fc.d),
                                 s.window=3,
                                 #method='ets',
                                 ic='bic',
                                 opt.crit='mae')
            df$fc.d[, j] <- forecast(fit$svd.stlf, h=nrow(df$fc.d))$mean %>% as.numeric()
        }
        
        # 실제 test에 있는 해당 Dept의 Store 값과 일치되는 조건의 날짜의 값 채우기
        result$uni.result <- melt(df$fc.d) %>% as_tibble() %>% rename(Weekly_Sales=2)
        df$svd.stlf.pred$Weekly_Sales[df$svd.stlf.pred$Dept==d] <- df$svd.stlf.pred %>% 
            filter(Dept==d) %>% select(Store, Date) %>% 
            left_join(result$uni.result) %>% pull(Weekly_Sales)
    }
)
# write.submission(df$svd.stlf.pred)  #
# default(다 빠진 경우): 2802.14 (53등)
# opt.crit='mae'만 포함: 2791.95 (47등)
# ic까지 포함시킨 경우 : 2782.59 (47등)
# method까지 포함      : 2782.59 (47등)
# s.window=3만 포함    : 2649.06 (21등)
# s.window=3, ic 포함  : 2630.92 (20등)
# 모두 다 포함시킨 경우: 2632.52 (20등)

df$svd.stlf.shift.pred <- postprocess(train, df$svd.stlf.pred, shift=2.5) 
write.submission(df$svd.stlf.shift.pred) # shift=2.5 : 2328.86 (2등), s.window=3과 ic로 한 경우 2329.17임.
write.submission(df$svd.stlf.shift.pred) # shift=2   : 2351.63 (2등)


