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
# 시간데이터로 한다면, 오전시간대 평균 (ex. 09~12),오후시간대 평균 (ex.12~18)의 평균값으로 각각 변수들 추가
weather %>% 
  mutate(year=as.numeric(substr(weather$일시,1,4)),
                   month=as.numeric(substr(weather$일시,6,7)),
                   day=as.numeric(substr(weather$일시,9,10)),
                   hour=as.numeric(substr(weather$일시,12,13))) %>%
  filter((hour>=9 & hour<=12) | (hour>=15 & hour<=18)) %>% 
  mutate(lunchdinner= factor(ifelse(hour<=12,"오전","오후"))) %>% 
  group_by(lunchdinner,year,month,day)  
  
           
  

