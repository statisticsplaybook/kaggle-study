library(httr)
library(rvest)
library(readr)
library(tidymodels)
library(lubridate)


# SECTOR-기본통계 --------------------------------------------------------------------
# all_things_industry = 업종분류현황
# all_things_kibon =종목별기본 
# all_things_sise = 종목별시세세
# foreinger_proportion =종목별외국인보유비중
# industry_distribution =업종별 분포
# investment_index =투자지표들(pbr 등)
# investor = 투자자들(연기금=6000, 금융투자 외국인 등 )


# 전종목기본정보 -----------------------------------------------------------------
otp_url = 'http://data.krx.co.kr/comm/fileDn/GenerateOTP/generate.cmd'
otp_form_data_all_things_kibon = list(
  mktId = 'ALL',
  share = '1',
  csvxls_isNo = 'false',
  name = 'fileDown',
  url = 'dbms/MDC/STAT/standard/MDCSTAT01901'
)

otp_all_things_kibon = POST(otp_url, query = otp_form_data_all_things_kibon) %>% read_html() %>% html_text()

csv_url = 'http://data.krx.co.kr/comm/fileDn/download_csv/download.cmd'
all_things_kibon = POST(csv_url, query = list(code = otp_all_things_kibon)) %>% 
  read_html(encoding = 'EUC-KR') %>% html_text() %>% read_csv() 

# 전종목 시세 ------------------------------------------------------------------
otp_url = 'http://data.krx.co.kr/comm/fileDn/GenerateOTP/generate.cmd'
otp_form_data_all_things_sise = list(
  mktId= 'ALL',
  trdDd= '20210827',
  #adjStkPrc_check="Y", 혹시나해서 넣어봤는데 수정주가 적용안됨
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
  strtDd='20210817',
  endDd='20210824',
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
  read_html(encoding = 'EUC-KR') %>% html_text() %>% read_csv() 


# 투자지표들 -------------------------------------------------------------------
otp_form_data_investment_index = list(
  searchType='1',
  mktId='ALL',
  trdDd='20210825',
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
  trdDd='20210825',
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
  trdDd= '20210825',
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
  read_html(encoding = 'EUC-KR') %>% html_text() %>% read_csv() 


# 업종분류현황 ------------------------------------------------------------------

otp_form_data_all_things_industry_KOSDAQ = list(
  mktId= 'KSQ', #코스닥만 불러오게됨, 코스피=STK
  trdDd= '20210825',
  money= '1',
  csvxls_isNo= 'false',
  name= 'fileDown',
  url= 'dbms/MDC/STAT/standard/MDCSTAT03901'
)

otp_all_things_industry_KOSDAQ = POST(otp_url, query = otp_form_data_all_things_industry_KOSDAQ) %>% read_html() %>% html_text()


all_things_industry_KOSDAQ = POST(csv_url, query = list(code = otp_all_things_industry_KOSDAQ)) %>% 
  read_html(encoding = 'EUC-KR') %>% html_text() %>% read_csv() 

otp_form_data_all_things_industry_STK = list(
  mktId= 'STK', #코스닥만 불러오게됨, 코스피=STK
  trdDd= '20210825',
  money= '1',
  csvxls_isNo= 'false',
  name= 'fileDown',
  url= 'dbms/MDC/STAT/standard/MDCSTAT03901'
)

otp_all_things_industry_STK = POST(otp_url, query = otp_form_data_all_things_industry_STK) %>% read_html() %>% html_text()
all_things_industry_STK = POST(csv_url, query = list(code = otp_all_things_industry_STK)) %>% 
  read_html(encoding = 'EUC-KR') %>% html_text() %>% read_csv() 

all_things_industry<- bind_rows(all_things_industry_KOSDAQ,all_things_industry_STK)



