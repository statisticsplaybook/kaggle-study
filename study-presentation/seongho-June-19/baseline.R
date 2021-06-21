library(tidymodels)
library(tidyverse)
library(vroom)
library(skimr)
library(inspectdf)
library(tidylog)
library(lubridate)
library(vip)
train<-vroom("train.csv")
test <- vroom("test.csv")
sample_submission<-vroom("sample_submission.csv")



# 간단한 EDA -----------------------------------------------------------------

train$요일 <- ordered(train$요일, levels=c("월", "화","수", "목","금"))
test$요일 <- ordered(test$요일, levels=c("월", "화","수", "목","금"))
#요일 순서 재지정 ( default= 가나다 순)

skim(train)
inspect_cor(train) %>% show_plot()
inspect_na(train) %>% show_plot() #석식 메뉴 na값 존재
inspect_num(train) %>% show_plot()
inspect_types(train) %>% show_plot()


train %>% filter(nchar(석식메뉴)<10 | is.na(석식메뉴)) %>% view  # 이날은 석식 제공 X 
train[is.na(train$석식메뉴),]$석식메뉴<-"*"  #결측치를 "*"로 대체




train %>% select(중식계,석식계) %>% colMeans() # 전체 날짜의 평균 식수인원 




# 코로나 이전과 이후 --------------------------------------------------------------

ggplot(train) +
 aes(x = 일자, y = 현본사소속재택근무자수) +
 geom_line(size = 0.5, colour = "#112446") +
 theme_minimal()
# 재택근무라는 개념이 코로나 이후에 생김.


train %>%
  mutate(corona=factor(ifelse(현본사소속재택근무자수>0,1,0)) )%>% 
  group_by(corona,요일) %>% 
  summarise(중=mean(중식계),
            석= mean(석식계))   # 코로나 이전과 이후 요일별 식수인원은 큰 차이가 없음.

ggplot(train) +
  aes(x = 일자, y = 본사정원수) +
  geom_line(size = 0.5, colour = "#112446") +
  theme_minimal()   #하지만 본사정원수는 꾸준히 증가하는 추세이므로, 이를 식수인원 비율로 변환하여 확인해야 함을 인식.


# 인원 -> 정원수에 대한비율로 변환 -----------------------------------------------------


train %>%
  mutate(최대가능인원=본사정원수-본사휴가자수-본사출장자수-현본사소속재택근무자수,
               휴가비율=본사휴가자수/본사정원수,
               출장비율=본사출장자수/본사정원수,
               시간외근무명령서승인건수비율=본사시간외근무명령서승인건수/본사정원수,
               본사재택근무비율=현본사소속재택근무자수/본사정원수,
               중식계비율=중식계/최대가능인원,
               석식계비율=석식계/최대가능인원,
               점심저녁차이=중식계비율-석식계비율) %>% # 점심 저녁 차이도 보려고 한다.
  select(-c(본사휴가자수,본사출장자수,본사시간외근무명령서승인건수,
                  현본사소속재택근무자수)) ->imsi

imsi %>%
  mutate(corona=factor(ifelse(본사재택근무비율>0,1,0)) )%>% 
  group_by(corona,요일) %>% 
  summarise(중=mean(중식계비율),
             석= mean(석식계비율))-> imsi2
imsi2
cbind(요일=imsi2$요일[1:5],round(imsi2[1:5,3:4]-imsi2[6:10,3:4],3))
# 대체로 차이를 보이지 않는데 금요일 석식은 평균 6%차이를 보인다. 

# 중식, 석식에 따라 요일별 경향이 다름
ggplot(imsi) +
  aes(x = 요일, y = 중식계비율) +
  geom_boxplot(shape = "circle", fill = "#112446") +
  theme_minimal()

ggplot(imsi) +
  aes(x = 요일, y = 석식계비율) +
  geom_boxplot(shape = "circle", fill = "#112446") +
  theme_minimal()



ggplot(imsi) +
  aes(x = 일자, y =시간외근무명령서승인건수비율 ) +
  geom_line(size = 0.5, colour = "#112446") +
  theme_minimal()  # 시간외 근무가 많을수록 야근이 많다고 가정하고, 언제 많은지 알고싶었다.
# 비율이 0.2 이상넘어가는 날들을 확인해보면,
imsi %>%
  filter(시간외근무명령서승인건수비율>0.2) %>% 
  arrange(-시간외근무명령서승인건수비율) %>% 
  select(일자, 요일, 시간외근무명령서승인건수비율)->imsi2

table(month(imsi2$일자)) %>%plot()
table(imsi2$요일,month(imsi2$일자))# 수요일과 금요일은 야근 안하는 경향
table(year(imsi2$일자),month(imsi2$일자))  # 보통 연말에 바쁜경향을 보일 줄 알았는데,
#2020년에는 골고루 ,오히려 연초에 바빴다.

test %>%
  mutate(최대가능인원=본사정원수-본사휴가자수-본사출장자수-현본사소속재택근무자수,
               휴가비율=본사휴가자수/본사정원수,
               출장비율=본사출장자수/본사정원수,
               시간외근무명령서승인건수비율=본사시간외근무명령서승인건수/본사정원수,
               본사재택근무비율=현본사소속재택근무자수/본사정원수) %>% 
  select(-c(본사휴가자수,본사출장자수,본사시간외근무명령서승인건수,
                  현본사소속재택근무자수)) ->imsi_test
imsi_test %>%
  filter(시간외근무명령서승인건수비율>0.2) %>% 
  arrange(-시간외근무명령서승인건수비율) %>% 
  select(일자, 요일, 시간외근무명령서승인건수비율)->imsi_test2

table(month(imsi_test2$일자)) # 2020년과같이 연초에 바쁜 경향.(LH사태도 있었음)
table(imsi_test2$요일,month(imsi_test2$일자)) # 수요일과 금요일은 야근 안하는 경향

# 연휴 전후에 대한 변수 추가 ---------------------------------------------------------

holiday<-rep(0,1205)

weekday<-as.numeric(train$요일)

for(i in 1:(length(weekday)-1)){
  if(weekday[i]>=1 & weekday[i]<=4){# 월화수목은 다음날이 화수목금이 아니면 휴가 전날인 것으로
    if(weekday[i]!=weekday[i+1]-1){
      holiday[i]<-1#연휴전날
      holiday[i+1]<-2 #연휴다음날
      i=i+1# 하루 스킵
    }
    
  }
  else{
    if(weekday[i+1]!=1){
      holiday[i]<-1#연휴전날
      holiday[i+1]<-2 #연휴다음날
      i=i+1
    }
    
  }
  
}

table(holiday)
# 1 과 2가 같지 않은 이유를 보니, 징검다리 휴가 때문.
# 월 수 금이 휴일이면, 화요일은 연휴다음날과 연휴 전날이 중첩되는데 이것이 연휴 전날로 처리 된 것

holiday_test<-rep(0,50)
weekday<-as.numeric(test$요일)

for(i in 1:(length(weekday)-1)){
  if(weekday[i]>=1 & weekday[i]<=4){ 
    if(weekday[i]!=weekday[i+1]-1){
      holiday_test[i]<-1#연휴전날
      holiday_test[i+1]<-2 #연휴다음날
      i=i+1 
    }
    
  }
  else{
    if(weekday[i+1]!=1){# 금욜은 다음날이 월요일이 아니면 휴가 전날인 것으로
      holiday_test[i]<-1#연휴전날
      holiday_test[i+1]<-2 #연휴다음날
      i=i+1
    }
    
  }
  
}
table(holiday_test)

imsi %>% mutate(holiday=factor(holiday)) %>% group_by(holiday) %>% summarise(m=mean(중식계비율))
imsi %>% mutate(holiday=factor(holiday)) %>% group_by(holiday) %>% summarise(m=mean(석식계비율))

# 연휴 전날에는 중식, 석식 식수인원 비율이 감소하고
# 연휴 다음날에는 식수인원 비율이 증가하는 경향을 보임 
# 비록 test셋에서는 설 연휴, 삼일절 밖에 없지만..변수로 추가
imsi<-imsi %>% mutate(holiday=factor(holiday))

# 점심식사 인원과 저녁식사 인원 비율 차이가 큰날은 어떤 특징이 있을까? ---------------------------------

imsi %>% filter(본사재택근무비율==0) %>% 
  ggplot() +
  aes(x = 일자, y =점심저녁차이 ) +
  geom_line(size = 0.5, colour = "#112446") +
  theme_minimal()  # 어떤 경향이 보이시나요..?

imsi %>% 
  group_by(month(일자))%>%
  summarise(mean(점심저녁차이)) %>% plot   
# 1월 2월에 대해서는 다른 달과 비교했을때 3퍼정도 차이가 난다.
# 3%는 최대가능인원을 고려했을때 70명이라고 생각하면 큰 차이라고도 할수 있을 것같다.

inspect_num(imsi) %>% show_plot()
inspect_cor(imsi) %>% filter(col_1 %in%c("석식계","중식계") |
                               col_2 %in%c("석식계","중식계")) %>% show_plot()


# 모델링 ---------------------------------------------------------------------

# 중식계 모델
base_rec_lunch <- train %>% 
  recipe(중식계~.) %>% 
  step_mutate(최대가능인원=본사정원수-본사휴가자수-본사출장자수-현본사소속재택근무자수,
                    최대가능인원비율=최대가능인원/본사정원수,
                    휴가비율=본사휴가자수/본사정원수,
                    출장비율=본사출장자수/본사정원수,
                    시간외근무명령서승인건수비율=본사시간외근무명령서승인건수/본사정원수,
                    본사재택근무비율=현본사소속재택근무자수/본사정원수,
                    연=year(일자),
                    월=month(일자),
                    일=day(일자),
  ) %>% 
  step_rm(일자,본사정원수,본사휴가자수,본사출장자수,본사시간외근무명령서승인건수,
               현본사소속재택근무자수,석식계) %>% 
  prep()


train_base_lunch <- base_rec_lunch %>% juice()
test_base_lunch <- bake(base_rec_lunch,test) # 왜 메뉴들이 NA로 나올까요?

train_base_lunch<-train_base_lunch %>% mutate(holiday= factor(holiday))
test_base_lunch<-test_base_lunch %>% mutate(holiday= factor(holiday_test))


train_base_lunch<-train_base_lunch %>% select(-c(조식메뉴,중식메뉴,석식메뉴))
test_base_lunch<-test_base_lunch %>% select(-c(조식메뉴,중식메뉴,석식메뉴))

#석식계모델 데이터

base_rec_dinner <- train %>% 
  recipe(석식계~.) %>% 
  step_mutate(최대가능인원=본사정원수-본사휴가자수-본사출장자수-현본사소속재택근무자수,
                    최대가능인원비율=최대가능인원/본사정원수,
                    휴가비율=본사휴가자수/본사정원수,
                    출장비율=본사출장자수/본사정원수,
                    시간외근무명령서승인건수비율=본사시간외근무명령서승인건수/본사정원수,
                    본사재택근무비율=현본사소속재택근무자수/본사정원수,
                    연=year(일자),
                    월=month(일자),
                    일=day(일자),
  ) %>% 
  step_rm(일자,본사정원수,본사휴가자수,본사출장자수,본사시간외근무명령서승인건수,
            현본사소속재택근무자수,중식계) %>% 
  prep()
#데이터 준비
train_base_dinner <- base_rec_dinner %>% juice()
test_base_dinner <- bake(base_rec_dinner,test) 

train_base_dinner<-train_base_dinner %>% mutate(holiday= factor(holiday))%>% filter(nchar(as.character(석식메뉴))>15)
test_base_dinner<-test_base_dinner %>% mutate(holiday= factor(holiday_test)) 



train_base_dinner<-train_base_dinner %>% select(-c(조식메뉴,중식메뉴,석식메뉴))
test_base_dinner<-test_base_dinner %>% select(-c(조식메뉴,중식메뉴,석식메뉴))


set.seed(125)
rf_mod <- 
  rand_forest() %>% 
  set_mode("regression") %>% 
  set_engine("ranger", importance = 'impurity') 
#importance = "impurity" , "permutation" 설정해야 vip() 가능
rf_wf <- 
  workflow() %>% 
  add_model(rf_mod) %>% 
  add_formula(중식계~.)
rf_fit <- rf_wf %>% 
  fit(train_base_lunch)


rf_wf2 <- 
  workflow() %>% 
  add_model(rf_mod) %>% 
  add_formula(석식계~.)
rf_fit2 <- rf_wf2 %>% 
  fit(train_base_dinner)

#변수 중요도 그리기 - vip()
#중식계 모델
rf_fit %>% 
  pull_workflow_fit() %>% 
  vip(num_features = 30)
#석식계 모델
rf_fit2%>% 
  pull_workflow_fit() %>% 
  vip(num_features = 30)


rf_pred1 <- predict(rf_fit, new_data = test_base_lunch) 

rf_pred2 <- predict(rf_fit2, new_data = test_base_dinner) 

sample_submission$중식계<-rf_pred1$.pred
sample_submission$석식계<-rf_pred2$.pred



write.csv(sample_submission,"basemodel.csv",row.names = F,fileEncoding = "UTF-8")
#MAP =94.2451914283
#정리하면서 한것 말고 중간에 holiday포함 모델은 91.37x
