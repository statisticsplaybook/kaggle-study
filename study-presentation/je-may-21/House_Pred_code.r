#### Library Call
# install.packages("PerformanceAnalytics")

library(tidyverse)
library(tidymodels)
library(skimr)

#### data load
train <- read_csv('data/train.csv')
test <- read_csv('data/test.csv')

all_origin <- bind_rows(train, test)

skim(all_origin) # PoolQC, Fence, Alley, MiscFeature, SaleType가 결측률이 높음

all_origin %>% 
  select(-c(PoolQC, Fence, MiscFeature, Alley, SaleType, Id)) %>% 
  # janitor::clean_names() %>% 
  mutate_if(is.character, as.factor) %>% # Character -> Factor
  mutate(
    MSSubClass = as.factor(MSSubClass), # MSSubClass는 범주형 변수이므로 factor로 변환
    OverallQual = factor(OverallQual, order = T, levels = c(1,2,3,4,5,6,7,8,9,10)),
    OverallCond = factor(OverallCond, order = T, levels = c(1,2,3,4,5,6,7,8,9,10)),
    )-> all

all %>% skim()

#### 질문을 준비해보자
# 1. 에임스 시티는 아이오와주에 있으며, 대학도시로 유명함
 # 날씨는 한국과 비슷하고, 여름엔 덥고 겨울엔 춥다
 # 그러면 수영장의 유무는 별로 메리트가 아닐 수도? 365일 쓸 수 있는 게 아니니까
 # Neighborhood에 대학교가 있다면 더 비쌀수도?

# 2. 내가 집을 산다면 뭘 고려할까?
 # 일단 집이 오래되지 않았으면 좋겠다.
 # 근처에 공원이 있으면 좋겠다.
 # 편의시설과 가까우면 좋겠다.
 # 2층집이면 좋을 것 같다.
 # 하지만 대학교에 다닐 일이면, 다른 거 다 필요 없고 가장 간편한 집이 좋지 않을까?
 # 면적당 가격은 어떨까? 면적이 작을수록 오히려 면적당 가격은 높지 않을까?

# 3. 집을 사는 목적
 # 부동산 투자의 목적일 수도 있고 거주의 목적일 수도 있다.
 # 부동산 투자 용도라면 뭘로 구분할 수 있을까?
 # 미국은 오래된 집이더라도 리모델링 되고 잘 관리되었다면 가격이 좋은 편


#### EDA

# 잘 모르겠으니까 집의 종류부터 EDA 시작

## MSSubClass
## 1-1. MSSubClass barchart
all[1:1459, ] %>% 
  ggplot(aes(x = MSSubClass, y = SalePrice, fill = MSSubClass))+
  geom_col(position = 'dodge')+
  scale_y_continuous(labels = comma)
# 인기 있는 집과 없는 집의 종류가 있는 것 같다. 20과 60이 가장 높은 가격대를 형성한다.

## 1-2. MSSubClass freqpoty
all[1:1459, ] %>% 
  ggplot(aes(x = SalePrice, color = MSSubClass))+
  geom_freqpoly(binwidth = 50000)+
  scale_x_continuous(labels = comma)

## 1-3. MSSubClass boxplot
all[1:1459, ] %>% 
  ggplot(aes(x = MSSubClass, y = SalePrice))+
  geom_boxplot()+
  scale_y_continuous(labels = comma)

# 2. SalePrice 분포
all[1:1459, ] %>% 
  ggplot(aes(x = SalePrice))+
  geom_histogram(fill = 'purple')+ 
  scale_x_continuous(labels = comma)

# 3. 수영장은 가격에 중요할까?

all[1:1459, ] %>% 
  ggplot(aes(x = PoolArea, y = SalePrice))+
  geom_point() +
  geom_smooth(method = 'lm', formula = y ~ x)
# 데이터 자체는 거의 없는 정도이므로 차후 분석에서 제외하는 게 좋을 것 같다.
# 아마 아이오와는 추울 때가 많아서 풀장 건설은 거의 하지 않는 듯하다.
# 게다가 비용과 관리적인 측면에서 오히려 단점으로 작용한다고 한다.


# 4. Wood Deck은?
all[1:1459, ] %>% 
  ggplot(aes(x = WoodDeckSF, y = SalePrice))+
  geom_point() +
  geom_smooth(method = 'lm', formula = y ~ x)
# Wood Deck은 상당히 중요한 요소로 작용하는 것으로 판단된다.



# 5. Remodel 여부 
all[1:1459, ] %>% 
  mutate(RemodelFlag = as.factor(ifelse(YearBuilt == YearRemodAdd, 'Remodel','NoRemodel'))) %>% 
  count(RemodelFlag)


all[1:1459, ] %>% 
  mutate(RemodelFlag = as.factor(ifelse(YearBuilt == YearRemodAdd, 'NoRemodel','Remodel')),
         YearBuilt_Class = case_when(
           YearBuilt < 1950 ~ "~1950",
           YearBuilt < 1980 ~ "1950~1980",
           YearBuilt < 2000 ~ "1980~2000",
           T ~ "2000~"
         )) %>%
  ggplot(aes(x = YearBuilt_Class, y = SalePrice, fill = RemodelFlag))+
  geom_boxplot()
# 리모델링을 하면 가격이 더 높아짐
# 그리고 연식이 오래됐을수록 리모델링한 집과 안한집의 가격 차이가 생김
# 리모델링한 집은 Outlier도 크고 분위수 범위도 넓은 편, 즉 가격 협상의 여지가 더 많은 듯함
# 리모델링플래그는 새로운 Feature로 만들어도 좋겠다는 생각이 들었음

all[1:1459, ] %>% 
  mutate(RemodelFlag = as.factor(ifelse(YearBuilt == YearRemodAdd, 'NoRemodel','Remodel')),
         YearBuilt_Class = case_when(
           YearBuilt < 1950 ~ "~1950",
           YearBuilt < 1980 ~ "1950~1980",
           YearBuilt < 2000 ~ "1980~2000",
           T ~ "2000~"
         )) %>%
  ggplot(aes(x = MSSubClass, y = SalePrice, fill = RemodelFlag))+
  geom_boxplot()

# 6. 대학가 근처라면 더 비쌀까?

# 6-1. 지역에 따른 집가격
all[1:1459, ] %>% 
  ggplot(aes(x = Neighborhood, y = SalePrice, fill = Neighborhood))+
  geom_boxplot(position = 'dodge')+
  coord_flip()
# geom_col을 쓰면 단순 합산이므로 이상치에 대한 확인을 할 수 없음
# 그래서 geom_boxplot이 더 좋은 시각화
# 대학가 주변은 오히려 싼 것을 알 수 있음, 아마 작은 방이 많아서일까?

# 6-2. 지역에 따른 방면적
all[1:1459, ] %>% 
  mutate(total_feet = `1stFlrSF` + `2ndFlrSF`) %>% 
  ggplot(aes(x = Neighborhood, y = total_feet, fill = Neighborhood))+
  geom_boxplot(position = 'dodge')+
  coord_flip()

# 6-3. 지역에 따른 방면적2
all[1:1459, ] %>% 
  mutate(total_feet = `1stFlrSF` + `2ndFlrSF`,
         Price_per_feet = SalePrice/total_feet) %>% 
  ggplot(aes(x = reorder(Neighborhood,Price_per_feet,median ), y = Price_per_feet, fill = Neighborhood))+
  geom_boxplot(position = 'dodge')+
  coord_flip()
# 대학 주변은 오히려 단위면적당 가격이 낮은편인 것 같습니다.
# 그렇다면 오히려 대학가에 가까울수록 Sales Price가 낮을 것이라는 예상이 되는데 이유가 있을까요?

# 7. 판매된 년,월은 가격과 관련이 있을까?

# 7-1) 년도별?
all[1:1459, ] %>% 
  group_by(YrSold) %>% 
  summarise(SalePrice = mean(SalePrice)) %>% 
  ggplot(aes(x = YrSold, y = SalePrice))+
  geom_col(position = 'dodge')+
  scale_y_continuous(labels = comma)
## 2008년 금융위기에 한 번 집값이 내렸을 것으로 예상했으나 의외로 많이 내리지는 않았음



# 7-2) 월별?
all[1:1459, ] %>% 
  group_by(YrSold, MoSold) %>% 
  summarise(SalePrice = mean(SalePrice)) %>% 
  ggplot(aes(x = MoSold, y = SalePrice))+
  geom_col(position = 'dodge')+
  geom_line(col = 'red', size = 1)+
  scale_y_continuous(labels = comma)+
  facet_grid(YrSold ~ .)
# 월에 따라서는 특별한 추이는 없는 것으로 생각됨

#### 상관분석
library(PerformanceAnalytics)
num_var <- c('LotArea', 'LotFrontage', 'YearBuilt', 'YearRemodAdd',
             'MasVnrArea', 'BsmtFinSF1', 'BsmtFinSF2', 'BsmtUnfSF', 'TotalBsmtSF',
             '1stFlrSF','2ndFlrSF','LowQualFinSF', 'GrLivArea', 'FullBath',
             'BedroomAbvGr','KitchenAbvGr','TotRmsAbvGrd','Fireplaces','GarageYrBlt',
             'GarageCars','GarageArea','WoodDeckSF','OpenPorchSF','OpenPorchSF','EnclosedPorch',
             '3SsnPorch','ScreenPorch','MiscVal','SalePrice')

# 상관분석을 한 번에 다 해버리고 싶지만, 아주 결과가 안이쁘게 나온다.
# 그래서 필요한 항목들만 골라서 하는 게 중요하다.


## 1. 집 면적과 가격
all[1:1459, c('1stFlrSF','2ndFlrSF','SalePrice')] %>% 
  chart.Correlation(histogram = T)
# 1층의 넓이와 상관관계가 있으며 2층과는 적은 상관관계를 보임

## 2. 지하실과 가격
all[1:1459, c('BsmtFinSF1', '1stFlrSF', 'BsmtUnfSF', 'TotalBsmtSF','SalePrice')] %>% 
  chart.Correlation(histogram = T)

# 3. 서로 다른 것들과의 상관관계
all[1:1459, c('TotalBsmtSF','GarageArea','YearBuilt','WoodDeckSF','OpenPorchSF', 'SalePrice')] %>% 
  chart.Correlation(histogram = T)


### 느낀점
# 1. 변수가 많을수록 EDA의 경우의 수가 많아서 힘듬
# 2. 적절한 미국 부동산 지식이 EDA의 접근 가능성을 높임
# 3. EDA의 결과는 Feature Engineering과 이어져야만 의미가 있는 것 같음


