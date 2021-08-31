library(DBI)
library(RSQLite)
library(dplyr)
con <- DBI::dbConnect(RSQLite::SQLite(),
                      dbname="C:/_sqlite/trading.db" )
dbListTables(con)
tbl(con,"total_data") %>% collect() # collect를 통해 데이터프레임화
tally(tbl(con,"total_data")) #행수


# 데이터 조회 및 가져오기 -----------------------------------------------------------

tbl(con,"total_data") %>% 
  filter(날짜>20210601) %>% 
  group_by(종목명) %>%
  summarise(거래량평균=mean(거래량,na.rm=T))%>%
  show_query()

get1 <- dbGetQuery(con,
                   "SELECT `종목명`, AVG(`거래량`) AS `거래량평균`
                   FROM `total_data`
                   WHERE (`날짜` > 20210601.0)
                   GROUP BY `종목명`")
get1 %>% head


#27일 오전 11:53분 시세데이터 크롤링후  거래량 분석
#날라가서 그냥 종가기준으로 확인


imsi <- left_join(all_things_sise, get1,
                  by = intersect(names(get1),
                                 names(all_things_sise)   ) 
                  )
imsi %>%
  mutate(평소대비거래량=round(거래량/거래량평균*100),1) %>%   #평소 거래량평균보다 10배이상 높은 것.
  filter(평소대비거래량>1000) %>% 
  select(종목명,종가,대비,등락률,시가,고가,저가, 평소대비거래량) %>% 
  arrange(-등락률) %>% tail#
# 미투젠은 거래량이 이상함 지금 2만 거래량도안되는데 68만..?



# ++ 장끝나고 나중에 확인해보니
# 미투젠 시간외거래로 67만주를 18150원에 블록딜로 팔았다네요.
# 일반 증권어플로 보는 거래량과 차이가 있다니..!






#올해 최고가에서 현재 제일 많이 떨어진 순

tbl(con,"total_data") %>% 
  group_by(종목명) %>% 
  summarise(max_price= max(고가,na.rm=T)) %>% 
  show_query()
  
get2<-dbGetQuery(con,
                 "SELECT `종목명`, MAX(`고가`) AS `max_price`
                 FROM `total_data`
                 GROUP BY `종목명`")

imsi2 <- left_join(all_things_sise, get2,
                  by = intersect(names(get2),
                                 names(all_things_sise)   ) 
)

# db에저장된 종목은 약2300개이고 (우선주+스팩주) 현재크롤링은 2580이어서 NA존재
imsi2 %>%
  filter(complete.cases(max_price),거래량>0) %>% 
  mutate(금일이전최고가대비=(1-(max_price-(종가))/max_price)*100) %>% 
  select(종목명,종가,max_price,금일이전최고가대비,거래량) %>% 
  arrange(금일이전최고가대비) %>% tail # 100보다 크면 조회기간 내 신고가 갱신
#수정주가가 반영 안됨 ().. 뭔가 좀 이상한것 같음.







  

