library(stringr)
library(httr)
library(rvest)
library(readr)
library(tidymodels)
library(lubridate)
library(tictoc)

otp_url = 'http://data.krx.co.kr/comm/fileDn/GenerateOTP/generate.cmd'
csv_url = 'http://data.krx.co.kr/comm/fileDn/download_csv/download.cmd'
con <- DBI::dbConnect(RSQLite::SQLite(),
                      dbname="C:/_sqlite/trading.db" )
dbListTables(con)
tbl(con,"total_data") %>% collect() # collect를 통해 데이터프레임화
tally(tbl(con,"total_data")) #행수



start_date<-'20210101'
end_date<-'20210826'

# 개별종목 크롤링을 통한 개장날짜 긁어오기 --------------------------------------------------
otp_form_data_date = list(
 tboxisuCd_finder_stkisu0_7= '005930/삼성전자',
isuCd= 'KR7005930003',
isuCd2= 'KR7005930003',
codeNmisuCd_finder_stkisu0_7= '삼성전자',
param1isuCd_finder_stkisu0_7= 'ALL',
strtDd= start_date,
endDd= end_date,
share= '1',
money= '1',
csvxls_isNo= 'false',
name= 'fileDown',
url= 'dbms/MDC/STAT/standard/MDCSTAT01701'
)

otp_all_things_date = POST(otp_url, query = otp_form_data_date) %>% read_html() %>% html_text()
all_date = POST(csv_url, query = list(code = otp_all_things_date)) %>% 
  read_html(encoding = 'EUC-KR') %>% html_text() %>% read_csv() 
duration<-gsub('\\D','', all_date$일자)


# For문으로 지정기간 거래데이터 긁고 데이터에베이스에 저장해보기 -------------------------------------


tic()

for(i in duration){

# 전종목 시세 ------------------------------------------------------------------
otp_url = 'http://data.krx.co.kr/comm/fileDn/GenerateOTP/generate.cmd'
otp_form_data_all_things_sise = list(
  mktId= 'ALL',
  trdDd= i,
  share= '1',
  money= '1',
  csvxls_isNo= 'false',
  name= 'fileDown',
  url= 'dbms/MDC/STAT/standard/MDCSTAT01501'
)

otp_all_things_sise = POST(otp_url, query = otp_form_data_all_things_sise) %>% read_html() %>% html_text()

csv_url = 'http://data.krx.co.kr/comm/fileDn/download_csv/download.cmd'
all_things_sise = POST(csv_url, query = list(code = otp_all_things_sise)) %>% 
  read_html(encoding = 'EUC-KR') %>% html_text() %>% read_csv() #%>% mutate(time=Sys.time())
#data_sise2 = POST(csv_url, query = list(code = otp1)) %>% 
#read_html(encoding = 'EUC-KR') %>% html_text() %>% read_csv() %>% mutate(time=Sys.time())





# 투자자별 순매수 정도 -------------------------------------------------------------
otp_form_data_investor = list(
  mktId='ALL',
  invstTpCd='6000',#연기금 등
  strtDd=i,
  endDd=i,
  share='1',
  money='1',
  csvxls_isNo='false',
  name='fileDown',
  url='dbms/MDC/STAT/standard/MDCSTAT02401'
)
# 금융투자=1000
# 보험=2000
# 투신=3000
# 사모=3100
# 은행=4000
# 기타금융=5000
# 연기금 등=6000
# 기관합계= 7050
# 기타법인= 7100
# 개인= 8000
# 외국인= 9000
# 기타외국인=9001
# 전체=9999
otp_investor = POST(otp_url, query = otp_form_data_investor) %>% read_html() %>% html_text()


investor = POST(csv_url, query = list(code = otp_investor)) %>% 
  read_html(encoding = 'EUC-KR') %>% html_text() %>% read_csv() %>%  mutate(날짜=i[2])

# 투자지표들 -------------------------------------------------------------------
otp_form_data_investment_index = list(
  searchType='1',
  mktId='ALL',
  trdDd=i,
  tboxisuCd_finder_stkisu0_0='',
  isuCd='',
  isuCd2='', 
  codeNmisuCd_finder_stkisu0_0=' ',
  param1isuCd_finder_stkisu0_0='ALL',
  strtDd='',
  endDd='', 
  csvxls_isNo='false',
  name='fileDown',
  url='dbms/MDC/STAT/standard/MDCSTAT03501'
)

otp_investment_index = POST(otp_url, query = otp_form_data_investment_index) %>% read_html() %>% html_text()
investment_index = POST(csv_url, query = list(code = otp_investment_index)) %>% 
  read_html(encoding = 'EUC-KR') %>% html_text() %>% read_csv() 

# 외국인보유비중 -----------------------------------------------------------------
otp_form_data_foreinger_proportion = list(
  searchType='1',
  mktId='ALL',
  trdDd=i,
  tboxisuCd_finder_stkisu0_2='',
  isuCd='',
  isuCd2='', 
  codeNmisuCd_finder_stkisu0_2=' ',
  param1isuCd_finder_stkisu0_2='ALL',
  strtDd='',
  endDd='', 
  csvxls_isNo='false',
  name='fileDown',
  url='dbms/MDC/STAT/standard/MDCSTAT03701'
)

otp_foreinger_proportion = POST(otp_url, query = otp_form_data_foreinger_proportion) %>% read_html() %>% html_text()


foreinger_proportion = POST(csv_url, query = list(code = otp_foreinger_proportion)) %>% 
  read_html(encoding = 'EUC-KR') %>% html_text() %>% read_csv() 
# 업종별 분포 ------------------------------------------------------------------

otp_form_data_industry_distribution = list(
  searchType= '1',
  mktId= 'STK', # 코스피=STK, ,코스닥= KSQ
  trdDd= i,
  idxIndCd= '005',
  strtDd= '',
  endDd= '',
  share= '2',
  money= '3',
  csvxls_isNo= 'false',
  name= 'fileDown',
  url= 'dbms/MDC/STAT/standard/MDCSTAT03801'
  
)
otp_industry_distribution = POST(otp_url, query = otp_form_data_industry_distribution) %>% read_html() %>% html_text()
industry_distribution = POST(csv_url, query = list(code = otp_industry_distribution)) %>% 
  read_html(encoding = 'EUC-KR') %>% html_text() %>% read_csv() %>% mutate(날짜=i)

# 업종분류현황 ------------------------------------------------------------------

otp_form_data_all_things_industry_KOSDAQ = list(
  mktId= 'KSQ', #코스닥만 불러오게됨, 코스피=STK
  trdDd= i,
  money= '1',
  csvxls_isNo= 'false',
  name= 'fileDown',
  url= 'dbms/MDC/STAT/standard/MDCSTAT03901'
)

otp_all_things_industry_KOSDAQ = POST(otp_url, query = otp_form_data_all_things_industry_KOSDAQ) %>% read_html() %>% html_text()


all_things_industry_KOSDAQ = POST(csv_url, query = list(code = otp_all_things_industry_KOSDAQ)) %>% 
  read_html(encoding = 'EUC-KR') %>% html_text() %>% read_csv() 

otp_form_data_all_things_industry_STK = list(
  mktId= 'STK',
  trdDd= i,
  money= '1',
  csvxls_isNo= 'false',
  name= 'fileDown',
  url= 'dbms/MDC/STAT/standard/MDCSTAT03901'
)

otp_all_things_industry_STK = POST(otp_url, query = otp_form_data_all_things_industry_STK) %>% read_html() %>% html_text()
all_things_industry_STK = POST(csv_url, query = list(code = otp_all_things_industry_STK)) %>% 
  read_html(encoding = 'EUC-KR') %>% html_text() %>% read_csv() 

all_things_industry<- bind_rows(all_things_industry_KOSDAQ,all_things_industry_STK)



#+해야할 것  크롤링 한 날짜에 대한 열 추가 , for문 이용해서 크롤링 쫙 해보기 
# 
# 

imsi <- left_join(all_things_sise, all_things_industry,
          by = intersect(names(all_things_industry),
                         names(all_things_sise)
                         )
          )
imsi <-left_join(imsi, foreinger_proportion,
                   by = intersect(names(imsi),
                                  names(foreinger_proportion)
                                  )
                 )
imsi <-left_join(imsi, investment_index,
                 by = intersect(names(imsi),
                                names(investment_index
                                    )
                 )
)
KOR_ticker<-imsi
rm(imsi)
KOR_ticker<-KOR_ticker[!grepl('스팩', KOR_ticker$종목명), ]  
KOR_ticker<-
  KOR_ticker %>%
  filter(as.numeric(종목코드)%%10==0,
                                  거래량>0,
                                  시장구분!="KONEX") %>% 
  mutate(날짜=i)

# 테이블에 데이터 넣기 - 한번에 여러 행 --------------------------------------------------
# 변수명 사이에 띄어쓰기가 있으면 안됨.
colnames(KOR_ticker)<-c(
  "종목코드"      ,   "종목명"           , "시장구분"          ,"소속부"           ,
  "종가"          ,   "대비"             , "등락률"            ,"시가"             ,
  "고가"          ,   "저가"             , "거래량"            ,"거래대금"         ,
  "시가총액"     ,    "상장주식수"      ,  "업종명"            ,"외국인보유수량"  ,
  "외국인지분율",    "외국인한도수량" ,  "외국인한도소진율", "EPS"              ,
  "PER"          ,    "BPS"             ,  "PBR"               ,"주당배당금"       ,
  "배당수익률"   ,    "날짜" 
)

dbWriteTable(con, "total_data",KOR_ticker , row.names = FALSE, append=TRUE)


# 타임슬립 적용(지정시간간격 크롤링 가능)
#  Sys.sleep(1)

}

toc()
# 163일치데이터 크롤링해서 저장하는데 968.96 sec elapsed

# 
# 
# # 테이블 내 전체 데이터 삭제
# rs <- dbSendQuery(con, "DELETE from total_data")
# dbClearResult(rs)
# 
# # SQLite DB 연결 해제
# dbDisconnect(con)


#write.csv(KOR_ticker,"total_data.csv",row.names = F) #DB-Browser통해 테이블 생성

