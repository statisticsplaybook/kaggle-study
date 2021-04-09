#### 1. 데이터 전처리 ####
# devtools::install_github("business-science/modeltime.h2o")
pacman::p_load(ggpubr,        # ggplot2 기반 통계처리. group_by, select 등이 dplyr와 충돌
               h2o,           # AutoML. year, month, week는 h2o와 lubridate가 충돌
               lubridate,
               magrittr,      # pipeline 기능 추가
               tidyverse,     # 데이터 전처리용
               dlookr,        # EDA용
               FSA,           # dunnTest 등
               timetk,        # 시계열 데이터 전처리용
               modeltime.h2o, # 시계열 데이터 h2o 사용하기
               tidymodels,    # 머신러닝용
               forecast       # 시계열 모델 및 정확도 확인
               )          

# df <- splits <- recipe <- fit <- result <- list()

#### 가. 데이터 로딩 ####
(rdata <- read_csv("./data/walmart/train.csv.zip") %>% 
    bind_rows(read_csv("./data/walmart/test.csv.zip")) %>% 
    left_join(read_csv("data/walmart/features.csv.zip")) %>% 
    left_join(read_csv("data/walmart/stores.csv")) %>% 
    janitor::clean_names() %>%                  # 대문자를 소문자로 바꿔준다.
    mutate(type=as_factor(type),
           store=as_factor(store), 
           dept=as_factor(dept),
           is_holiday=as_factor(is_holiday)))


#### 나. EDA ####
skimr::skim(rdata)

#### _1) 상관관계 살펴보기 ####
# 더미변수로 만들고 정규화하기
system.time(data <- rdata %>% 
              recipe(weekly_sales ~ .) %>% 
              step_dummy(all_nominal()) %>% 
              step_normalize(all_numeric(), -all_outcomes()) %>% 
              prep() %>% bake(rdata))

correlate(data) %>% 
  filter(var1=="weekly_sales" & !grepl("dept", var2) & !grepl("store", var2)) %>% 
  arrange(desc(abs(coef_corr)))  # weekly_sales과 상관관계 높은 변수 찾기 


#### _2) 부서별 매출 ####
# 같은 store 다른 dept
rdata %>% filter(store==1 & (dept==1 | dept==2 | dept==10 | dept==20)) %>% 
  group_by(dept) %>% 
  plot_time_series(date, weekly_sales, .facet_ncol = 2, .interactive = T)

rdata %>% filter(store==1 & (dept==1 | dept==2 | dept==10 | dept==20)) %>% 
  Summarize(weekly_sales~dept, .)


#### _3) 상점별 매출 ####
rdata %>% filter(dept==1 & (store==1 | store==2 | store==10 | store==20)) %>% 
  group_by(store) %>% 
  plot_time_series(date, weekly_sales, .facet_ncol = 2, .interactive = F)

rdata %>% filter(dept==1 & (store==1 | store==2 | store==10 | store==20)) %>% 
  Summarize(weekly_sales~store, .)


#### _4) 주별 매출 ####
# holiday 체크가하기
rdata %>% filter(is_holiday==TRUE) %>% 
  mutate(year=year(date), week=week(date)) %>% 
  select(year, week) %>% unique() %>% arrange(week)

# 주별 평균 매출액
rdata %>% mutate(year=year(date), week=week(date)) %>% 
  group_by(year, week) %>% summarise(weekly_sales=mean(weekly_sales, na.rm=T)) %>% 
  ggplot(aes(x=week, y=weekly_sales, color=factor(year))) + geom_line()+
  geom_vline(xintercept = c(6,7,36,37,47,48,52,51), color="red", size=.5)
# holiday 53주를 빼고 51주를 추가해야하지 않을까?, test는?


#### _5) temperature에 따른 weekly_sales ####
rdata %>% sample_n(1000) %>% 
  ggplot(aes(x=temperature, y=weekly_sales)) + geom_point(alpha=0.5)


#### _6) fuel_price에 따른 weekly_sales ####
rdata %>% sample_n(1000) %>% 
  ggplot(aes(x=fuel_price, y=weekly_sales)) + geom_point(alpha=0.5)


#### _7) mark_down5에 따른 weekly_sales ####
rdata %>% filter(!is.na(mark_down5)) %>% sample_n(1000) %>% 
  ggplot(aes(x=mark_down5, y=weekly_sales)) + geom_point(alpha=0.5)


#### _8) cpi에 따른 weekly_sales ####
rdata %>% filter(!is.na(cpi)) %>% sample_n(1000) %>% 
  ggplot(aes(x=cpi, y=weekly_sales)) + geom_point(alpha=0.5)


#### _9) unemployment에 따른 weekly_sales ####
rdata %>% filter(!is.na(unemployment)) %>% sample_n(1000) %>% 
  ggplot(aes(x=unemployment, y=weekly_sales)) + geom_point(alpha=0.5)


#### _10) type에 따른 weekly_sales ####
rdata %>% sample_n(1000) %>% 
  ggplot(aes(x=type, y=weekly_sales, color=type)) + geom_jitter(na.rm=TRUE) +
  scale_y_continuous(labels = comma)


#### _11) size에 따른 weekly_sales ####
rdata %>% sample_n(1000) %>% 
  ggplot(aes(x=size, y=weekly_sales)) + geom_point(alpha=0.5)


#### 다. 결측치 채우기 ####
skimr::skim(rdata)               # 결측치가 적은 것 찾기
rdata %>% filter(is.na(cpi) & is.na(unemployment)) %>% nrow()

# 1000개만 샘플링해서 cpi와 unemployment의 산점도 살펴보기
rdata %>% filter(!is.na(cpi)) %>% sample_n(1000) %>% 
  ggplot(aes(x=cpi, y=unemployment)) + geom_point(alpha=0.5)


#### _1) cpi ####
#### __가) tidymodel ####
# cpi를 제외한 결측치 있는 변수와 target 변수를 제거한다.
(df$cpi_data <- rdata %>% select(-c(4, 8:12, 14)) %>% na.omit %>% 
   mutate(month=month(date), week=week(date)))

# 데이터의 날짜 파악해서 적절한 기간만큼 자르자.
train %>% summarise(min=min(date), max=max(date))
test %>% summarise(min=min(date), max=max(date))
df$cpi_data %>% summarise(min=min(date), max=max(date))

# (splits$cpi <- initial_split(df$cpi_data, prop = 0.9))
(splits$cpi <- time_series_split(df$cpi_data, assess = "4 month", cumulative = T))
(splits$cpi_train <- training(splits$cpi))
(splits$cpi_test <- testing(splits$cpi))

splits$cpi_train %>% summarise(min=min(date), max=max(date))
splits$cpi_test %>% summarise(min=min(date), max=max(date))

# resamples
# (splits$cpi_train_cv <- vfold_cv(splits$cpi_train, v = 5))
(splits$cpi_train_cv <- time_series_cv(splits$cpi_train, 
                                              assess = "1 year", 
                                              skip   = "5 months")) # 5개월(5개), 1개월(24개)

splits$cpi_train_cv %>% tk_time_series_cv_plan() %>% 
  group_by(.id) %>% summarise(min=min(date), max=max(date))

# recipe 만들기
(recipe$cpi <- recipe(cpi ~ ., data = splits$cpi_train) %>% 
    step_rm(date) %>%                                   # 날짜를 지움.
    step_other(dept, threshold = 0.01) %>%              # 비율이 매우 작은 dept를 other로 묶음.
    step_dummy(all_nominal()) %>%                       # 더미변수로 만들어줌.
    step_normalize(all_numeric(), -all_outcomes()) %>%  # 정규화
    step_nzv(all_nominal()))                            # near zero var

# 모델 만들기 
(fit$cpi_rf_model <- 
    rand_forest(mtry = tune(), trees = 1000, min_n = tune()) %>% 
    set_engine("ranger", seed = 1234, num.threads = 15) %>% 
    set_mode("regression")) 

# workflow
(fit$cpi_rf_wf <- workflow() %>% 
    add_recipe(recipe$cpi) %>% 
    add_model(fit$cpi_rf_model))

# grid search (20개 조합, 캐글에선 200개?)
system.time(splits$cpi_train_tune_grid <- fit$cpi_rf_wf %>% 
              tune_grid(resamples = splits$cpi_train_cv,
                        grid = 20,
                        control = tune::control_grid(verbose = TRUE),
                        metrics = yardstick::metric_set(rmse))) 
# 5개 618초(10분 18초), 24개 2922초(48분 42초)

show_best(splits$cpi_train_tune_grid)  # 5개(mtry=9, min_n=10), 24개(mtry=2, min_n=5)
autoplot(splits$cpi_train_tune_grid)   
splits$cpi_train_tune_grid %>% collect_metrics() %>% 
  select(mean, mtry, min_n) %>%
  pivot_longer(mtry:min_n, values_to = "value", names_to = "parameter") %>% 
  ggplot(aes(value, mean, color = parameter)) +
  geom_point(show.legend = FALSE) +
  facet_wrap(~parameter, scales = "free_x")

# select best hiperparameter found
(fit$cpi_rf_wf <- fit$cpi_rf_wf %>% 
    finalize_workflow(select_best(splits$cpi_train_tune_grid)))

(fit$cpi_rf_model <- 
    rand_forest(mtry = 9, trees = 1000, min_n = 10) %>% 
    set_engine("ranger", seed = 1234, num.threads = 15) %>% 
    set_mode("regression")) 

(fit$cpi_rf_wf <- workflow() %>% 
    add_recipe(recipe$cpi) %>% 
    add_model(fit$cpi_rf_model))

# last fit
system.time(fit$cpi_last_fit <- fit(fit$cpi_rf_wf, splits$cpi_train)) # 655 

# predict
(result$cpi_preds <- predict(fit$cpi_last_fit, splits$cpi_test) %>% 
    rename(predict=1) %>% mutate(truth=splits$cpi_test$cpi))
with(result$cpi_preds, accuracy(predict, truth))
# 5개 (mtry=6, min_n=15) : RMSE(7.02), MAE(5.28)
# 5개 (mtry=9, min_n=10) : RMSE(5.11), MAE(4.29)
# 24개: RMSE(27.46), MAE(25.9)

# 예측값과 실제값 비교해보기
result$cpi_preds %>% sample_n(1000) %>% 
  ggscatter(x="predict", y="truth", alpha=0.5, add="reg.line")+
  stat_cor(aes(label=paste(..rr.label.., ..p.label.., sep="~`,`~")))

cor(result$cpi_preds, use="complete.obs")^2 
# random forest : 5개(0.9985), 24개(0.9510)



#### __나) AutoML using h2o ####
h2o.init()   

# cpi를 제외한 결측치 있는 변수와 target 변수를 제거한다.
(df$cpi_data <- rdata %>% select(-c(4, 8:12, 14)) %>% na.omit)

# 데이터의 날짜 파악해서 적절한 기간만큼 자르자.
(splits$cpi <- time_series_split(df$cpi_data, assess = "4 month", cumulative = T))
(splits$cpi_train <- training(splits$cpi))
(splits$cpi_test <- testing(splits$cpi))
(splits$cpi_train_h2o <- as.h2o(splits$cpi_train))
(splits$cpi_test_h2o <- as.h2o(splits$cpi_test))

# 모델 만들기
(fit$cpi_rf <- h2o.randomForest(y = "cpi",
                                nfolds = 5,
                                seed = 1111,
                                training_frame = splits$cpi_train_h2o))
(fit$cpi_gbm <- h2o.gbm(y = "cpi",
                        nfolds = 5,
                        seed = 1111,
                        training_frame = splits$cpi_train_h2o))

# 예측값과 실제값 
(result$cpi_valid <- predict(fit$cpi_rf, splits$cpi_test_h2o) %>% 
    as_tibble() %>% mutate(truth=splits$cpi_test$cpi))
(result$cpi_valid <- predict(fit$cpi_gbm, splits$cpi_test_h2o) %>% 
    as_tibble() %>% mutate(truth=splits$cpi_test$cpi))

# 예측값과 실제값 비교해보기
result$cpi_valid %>% sample_n(1000) %>% 
  ggscatter(x="predict", y="truth", alpha=0.5, add="reg.line")+
  stat_cor(aes(label=paste(..rr.label.., ..p.label.., sep="~`,`~")))

cor(result$cpi_valid, use="complete.obs")^2
# random forest              : 0.99815
# gradient boosting machines : 0.99991

with(result$cpi_valid, accuracy(predict, truth))
# random forest              : RMSE(3.421), MAE(2.496)
# gradient boosting machines : RMSE(1.058), MAE(0.859)

# variable importance
h2o.varimp_plot(fit$cpi_gbm)
splits$cpi_train %>% sample_n(1000) %>% 
  ggscatter(x="store", y="cpi", alpha=0.5)
splits$cpi_test %>% sample_n(1000) %>% 
  ggscatter(x="store", y="cpi", alpha=0.5)

# SHAP Summary (contribution of features)
h2o.shap_summary_plot(fit$cpi_gbm, splits$cpi_test_h2o)

# Partial Dependence Plots (연속변인만 가능)
h2o.partialPlot(fit$cpi_gbm, data = splits$cpi_test_h2o, cols = "fuel_price")

# 결측치 채우기
(fit$cpi_gbm_all <- h2o.gbm(y = "cpi",
                            nfolds = 5,
                            seed = 1111,
                            keep_cross_validation_predictions = TRUE,
                            training_frame = as.h2o(df$cpi_data)))
(rdata <- rdata %>% mutate(pred=as.vector(predict(fit$cpi_gbm_all, as.h2o(rdata)))) %>% 
    mutate(cpi=ifelse(is.na(cpi), pred, cpi)) %>% 
    select(-pred))

skimr::skim(rdata)




#### _2) unemployment ####
(df$unem_data <- rdata %>% select(-c(4, 8:12)) %>% na.omit)

# 데이터 나누기
(splits$unem <- time_series_split(df$unem_data, assess = "4 month", cumulative = T))
(splits$unem_train <- training(splits$unem))
(splits$unem_test <- testing(splits$unem))
(splits$unem_train_h2o <- as.h2o(splits$unem_train))
(splits$unem_test_h2o <- as.h2o(splits$unem_test))
 
# 모델 만들기
(fit$unem_gbm <- h2o.gbm(y = "unemployment",
                         nfolds = 5,
                         seed = 1111,
                         training_frame = splits$unem_train_h2o))

# 모델에 의한 예측과 실제 값 비교
(result$unem_valid <- predict(fit$unem_gbm, splits$unem_test_h2o) %>% as_tibble() %>% 
    mutate(truth=splits$unem_test$unemployment))

result$unem_valid %>% sample_n(1000) %>% 
  ggscatter(x="predict", y="truth", alpha=0.5, add="reg.line")+
  stat_cor(aes(label=paste(..rr.label.., ..p.label.., sep="~`,`~")))

# 변수 중요도 및 기여도
h2o.varimp_plot(fit$unem_gbm)
h2o.shap_summary_plot(fit$unem_gbm, splits$unem_test_h2o)

# 결측치 채우기
(fit$unem_gbm_all <- h2o.gbm(y = "unemployment",
                             nfolds = 5,
                             seed = 1111,
                             training_frame = as.h2o(df$unem_data)))
(rdata <- rdata %>% mutate(pred=as.vector(predict(fit$unem_gbm_all, as.h2o(rdata)))) %>% 
    mutate(unemployment=ifelse(is.na(unemployment), pred, unemployment)) %>% 
    select(-pred))

skimr::skim(rdata)



#### _3) mark_down5 ####
(df$md5_data <- rdata %>% select(-c(4, 8:11)) %>% na.omit)

(fit$md5_gbm <- h2o.gbm(y = "mark_down5",
                        nfolds = 5,
                        seed = 1111,
                        keep_cross_validation_predictions = TRUE,
                        training_frame = as.h2o(df$md5_train)))
h2o.varimp_plot(fit$md5_gbm)

rdata %>% summarise(mean=mean(mark_down5, na.rm=T), sd=sd(mark_down5, na.rm=T))

(result$md5_valid <- predict(fit$md5_gbm, as.h2o(df$md5_test)) %>% as_tibble() %>% 
    mutate(real=df$md5_test$mark_down5))
cor(result$md5_valid, use="complete.obs")^2
ggplot(result$md5_valid, aes(x=predict, y=real)) + geom_point(alpha=0.1)

# 지금까지의 작업 백업
df$rdata_backup1 <- rdata




#### 2. AutoML ####
pacman::p_load(magrittr, h2o, lubridate, dlookr, tidyverse, tidymodels)
h2o.init()

# 데이터 나누기
(data <- rdata %>% filter(!is.na(weekly_sales)) %>% select(-c(8:12)))
(splits$data <- time_series_split(data, assess = "1 year", cumulative = T))
(splits$train <- training(splits$data))
(splits$test <- testing(splits$data))
(splits$train_h2o <- as.h2o(splits$train))
(splits$test_h2o <- as.h2o(splits$test))

(fit$aml <- h2o.automl(y="weekly_sales",
                       training_frame    = splits$train_h2o,
                       leaderboard_frame = splits$test_h2o,
                       max_models        = 20,
                       max_runtime_secs  = 300, 
                       nfolds            = 5,
                       seed              = 1234))   # , project_name="aml"


h2o.varimp_plot(h2o.getModel(fit$aml@leaderboard[3,1]))
h2o.shap_summary_plot(h2o.getModel(fit$aml@leaderboard[3,1]), splits$test_h2o)

head(result$vaild <- predict(fit$aml, splits$test_h2o) %>% 
       as_tibble() %>% mutate(truth=splits$test$weekly_sales))

ggplot(result$vaild, aes(x=predict, y=truth)) + 
  geom_point(alpha=0.1) +
  scale_y_continuous(labels=comma) +
  scale_x_continuous(labels=comma)

cor(result$vaild$predict, result$vaild$truth)^2
# max_runtime_secs(60) : 0.665
# max_runtime_secs(120) : 0.730  step_other(0.849)   cpi&un (0.729)
# max_runtime_secs(180) : 0.752
# max_runtime_secs(300) : 0.778
# max_runtime_secs(600) : 0.781             0.849            0.796    
# max_runtime_secs(6000): 0.849             0.859            0.841
#max_runtime_secs(36000):                                    0.852
# max_runtime_secs(300) : 0.949

(data <- rdata %>% filter(!is.na(weekly_sales)) %>% select(-c(8:12)))
(fit$aml <- h2o.automl(y="weekly_sales",
                       training_frame   = as.h2o(data),
                       max_models       = 20,
                       max_runtime_secs = 300, 
                       nfolds           = 5,
                       seed=1234))   # , project_name="aml"

(test <- rdata %>% filter(is.na(weekly_sales)) %>% select(-c(8:12)))
(subfile <- read_csv("./data/walmart/sampleSubmission.csv.zip") %>% 
    mutate(Weekly_Sales=as.vector(predict(fit$aml, as.h2o(test)))))

write.csv(subfile, row.names = FALSE,
          "./data/walmart/automl-03191914.csv") # 600 DRF 6738(6931) 493등
# max_runtime_secs(60) : GLM 14816(20555) 624등
# max_runtime_secs(120) : DRF 7076(9100)  510등 step_other 6738(8927) 507등   cpi&un DRF 6914(9019) 
# max_runtime_secs(180) : DRF 7076(8211)  499등
# max_runtime_secs(300) : Dee 6761(7615)  495등        
# max_runtime_secs(600) : XRT 7005(7386)  495등        DRF 6738(7239) 494등          DRF 6914(7436) 495등
# max_runtime_secs(6000): GBM 4781(4959)  428등        GBM 4663(4740) 409등          GBM 4855(4787) 414등
# max_runtime_secs(36000):                                                           GBM 4359(4130) 369등

# max_runtime_secs(300) : GBM 2357(5161)  446등 (3개월)
# max_runtime_secs(300) : GBM 3467(5161)  446등 (1년)
# max_runtime_secs(30000):                                                           GBM 1664(4502) 387등

# rdata : 9143 (511등)
# data : 6785 (490등)

h2o.shutdown()



#### 3. GBM ####
h2o.init()

(fit.gbm <- h2o.gbm(y = "weekly_sales",
                    nfolds = 5,
                    seed = 1111,
                    keep_cross_validation_predictions = TRUE,
                    training_frame = as.h2o(train)))
h2o.varimp_plot(fit.gbm)

rdata %>% summarise(mean=mean(weekly_sales, na.rm=T), sd=sd(weekly_sales, na.rm=T))

(impute <- predict(fit.gbm, as.h2o(valid)) %>% as_tibble() %>% 
    mutate(real=valid$weekly_sales))
cor(impute, use="complete.obs")^2
ggplot(impute, aes(x=predict, y=real)) + geom_point(alpha=0.1)

(test <- data %>% filter(is.na(weekly_sales)))
(train <- setdiff(data, test))

(fit.gbm <- h2o.gbm(y = "weekly_sales",
                    nfolds = 5,
                    seed = 1111,
                    keep_cross_validation_predictions = TRUE,
                    training_frame = as.h2o(train)))

(subfile <- read_csv("./data/walmart/sampleSubmission.csv.zip") %>% 
    mutate(Weekly_Sales=as.vector(predict(fit.gbm, as.h2o(test)))))

write.csv(subfile, row.names = FALSE,
          "./data/walmart/baseline-deep-03141000.csv")
