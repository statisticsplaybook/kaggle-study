library(data.table)
 # 미세먼지데이터도 불러와서 확인해보면 좋을 것같다.
weather_daily<-fread("2016~daily.csv") #일별 데이터
weather_2016<-fread("2016.csv")#시간별 데이터
weather_2017<-fread("2017.csv")
weather_2018<-fread("2018.csv")
weather_2019<-fread("2019.csv")
weather_2020<-fread("2020.csv")
weather_2021<-fread("2021.csv")
weather <- bind_rows(weather_2016,
          weather_2017,
          weather_2018,
          weather_2019,
          weather_2020,
          weather_2021)


# na많은 열 삭제
im<-inspect_na(weather_daily)
im<-im %>% filter(pcnt>94) %>% select(col_name) %>% as_vector()

weather_daily<-weather_daily %>% select(-im)
inspect_na(weather_daily) %>% show_plot()




im<-inspect_na(weather)
im<-im %>% filter(pcnt>94) %>% select(col_name) %>% as_vector()

weather<-weather %>% select(-im)
inspect_na(weather) %>% show_plot()
# 시간데이터로 한다면, 오전시간대 대표값12시,오후시간대 대표값18시) 각각 변수들 추가
colnames(weather)
weather %>% 
  mutate(연=as.numeric(substr(weather$일시,1,4)),
                   월=as.numeric(substr(weather$일시,6,7)),
                   일=as.numeric(substr(weather$일시,9,10)),
                   hour=as.numeric(substr(weather$일시,12,13))) %>%
  filter((hour==12) | (hour==18)) %>% 
  mutate(lunchdinner= factor(ifelse(hour==12,"오전","오후"))) -> weather_select

colnames(weather_select)<-c("지점" , "일시"   , "기온" ,"강수량"      ,"풍속"  , "풍향"      ,
                            "습도"  ,  "증기압" ,  "이슬점온도", "현지기압"  , "해면기압" ,    
                            "일조" ,  "일사" , "전운량",  "중하층운량", "최저운고"   ,
                            "시정",  "지면온도","연","월","일", "hour" , "lunchdinner"  )
weather_select[is.na(weather_select$강수량),]$강수량<-0
weather_select$지면온도<-abs(weather_select$지면온도)
weather_select$기온<-abs(weather_select$기온)
weather_select %>% 
  recipe(지점~.) %>% 
  step_impute_knn(all_numeric()) %>% 
  prep() %>% juice() ->weather_select



train_base_lunch %>% select(연,월,일,요일)->imsi
test_base_dinner %>% select(연,월,일,요일)->imsi2

left_join(imsi2,weather_select,c("연","월","일"))->weather_select_test
left_join(imsi,weather_select,c("연","월","일"))->weather_select





weather_select %>% select(-is.integer) %>% filter(lunchdinner=="오전")->morning
weather_select %>% select(-is.integer) %>% filter(lunchdinner=="오후")->afternoon

weather_select_test %>% select(-is.integer) %>% filter(lunchdinner=="오전")->morning_test
weather_select_test %>% select(-is.integer) %>% filter(lunchdinner=="오후")->afternoon_test



morning %>% select(-c(요일,일시,hour,lunchdinner))->morning
afternoon %>% select(-c(요일,일시,hour,lunchdinner))->afternoon

train_weather_lunch<-left_join(train_base_lunch,morning, c("연","월","일"))
train_weather_dinner<-left_join(train_base_dinner,afternoon, c("연","월","일"))

morning_test %>% select(-c(요일,일시,hour,lunchdinner))->morning_test
afternoon_test %>% select(-c(요일,일시,hour,lunchdinner))->afternoon_test

 
test_weather_lunch<-left_join(test_base_lunch,morning_test, c("연","월","일"))
test_weather_dinner<-left_join(test_base_dinner,afternoon_test, c("연","월","일"))
train_weather_lunch %>% inspect_cor() %>%  filter(col_1 %in%c("석식계","중식계") |
                                               col_2 %in%c("석식계","중식계")) %>% show_plot()
train_weather_dinner%>% inspect_cor() %>%  filter(col_1 %in%c("석식계","중식계") |
                                                    col_2 %in%c("석식계","중식계")) %>% show_plot()






# 모델링 ---------------------------------------------------------------------


#### XGBoost - basic

cores <- parallel::detectCores()
cores

set.seed(125)
xgb_mod <- boost_tree() %>% 
  set_mode("regression") %>% 
  set_engine("xgboost",
             num.threads = cores)


xgb_wf_lunch <- 
  workflow() %>% 
  add_model(xgb_mod) %>% 
  add_formula(중식계~.)
xgb_fit_lunch <- xgb_wf_lunch %>% 
  fit(train_weather_lunch)
xgb_pred_lunch <- predict(xgb_fit_lunch,new_data = test_weather_lunch)

xgb_wf_dinner <- 
  workflow() %>% 
  add_model(xgb_mod) %>% 
  add_formula(석식계~.)
xgb_fit_dinner <- xgb_wf_dinner %>% 
  fit(train_weather_dinner)
xgb_pred_dinner <- predict(xgb_fit_dinner,new_data = test_weather_dinner)

#변수중요도
xgb_fit_lunch%>% 
  pull_workflow_fit() %>% 
  vip(num_features = 30)
xgb_fit_dinner %>% 
  pull_workflow_fit() %>% 
  vip(num_features = 30)

xb_pred_lunch <- predict(xgb_fit_lunch, new_data = test_weather_lunch) 
xb_pred_dinner <- predict(xgb_fit_dinner, new_data = test_weather_dinner) 

sample_submission$중식계<-xb_pred_lunch$.pred
sample_submission$석식계<-xb_pred_dinner$.pred

write.csv(sample_submission,"xgboost_model+weather.csv",row.names = F,fileEncoding = "UTF-8")


