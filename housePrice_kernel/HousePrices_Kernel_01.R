# HousePrice Kernel study
# 2019_05_31. 죽을때까지 공부를 손에 놓지 말자.

# Introduction
# Kaggle의 House price prediction 대회
# Lasso, XGBoost를 앙상블 해서 예측. 세부적인 EDA가 특징
# 이번 kernel을 통해서는 EDA를 어떤식으로 접근해야 하는지 배워보자

#' House Prices: Advanced Regression Techniques competition 정리
#' 데이터명 : Boston Housing Price

# 우선 필요한 라이브러리를 로딩하자

library(knitr) # R 코드 문서화하는 패키지
library(ggplot2)
library(plyr)
library(dplyr)
library(corrplot)
library(caret)
library(gridExtra)
library(scales)
library(Rmisc)    # 분석 및 유틸리티 함수가 포함되어 있는 패키지
library(ggrepel)  # ggrepel provides geoms for ggplot2 to repel overlapping text labels
library(randomForest) 
library(psych)
library(xgboost)

# train, test dataset loading
train <- read.csv('house_train.csv', stringsAsFactors = F)
test <- read.csv('house_test.csv', stringsAsFactors = F)

#' train data는 character, integer 변수로 구성되어 있다
#' 대개 Character 변수는 실제적으로 factor 변수이지만, 대부분 cleansing / feature engineering이 필요하므로,
#' character 변수로 R에 불러오자
#' 구조는 81개 변수, 1460개의 관측치가 있으며, 종속 변수는 SalePrice 변수이다
#' 구조를 확인해보자

str(train)
dim(train) # train,test data 의 row 개수 : 1459, 1460 으로 거의 1:1
dim(test)

test_labels <- test$Id  # 후 submission을 위해 test의 id는 벡터로 저장.
test$Id <- NULL         # 나머지 train, test에 있는 Id는 삭제 
train$Id <- NULL

# train, test data rbind
test$SalePrice <- NA
all <- rbind(train, test)


# 중요 변수 탐색해보기
# 79개의 독립 변수가 있는데, 뭐가 가장 좋은 변수일까...? 
# 그래도 일단 가장 중요한 변수는 SalePrice -> 종속 변수이니 한번 확인해보자
# 독립변수가 매우 많은 개수를 가지고 있으므로, 처음부터 어떤 변수인지를 확인하는 것보단,
# 상관 계수 등 slicing 을 통해 좁혀서 접근하는 것도 좋은 방법이라는 생각이 든다~!

# 1. SalePrice : histogram
# -> test data의 SalePrice는 NA처리 했고, train data는 결측이 없음
# -> x축의 scale 처리

ggplot(data = all[!is.na(all$SalePrice), ], aes(x = SalePrice)) +
  geom_histogram(fill = 'blue', binwidth =10000) +
  scale_x_continuous(breaks = seq(0, 800000, by = 100000), labels = comma)

# 왼쪽으로 치우친 분포 -> 역시나 싼 집이 많다는 의미 
# 치우친 분포에 대해서는 손을 봐야할수도 있음. 

# 2. SalePrice 요약 값 확인
summary(train$SalePrice)

# 평균 18만달러. 약 2억원대로 형성
# 중위값은 16만달러로, 어느정도 차이가 있음 -> 정규분포에 어느정도 어긋난다고 생각할 수 있음

## ** 3. SalePrice와 상관 계수가 높은 numeric 변수 선별하기
# 3.1. 우선, 수치형 변수 중 SalePrice외 상관 계수가 높은 변수들을 선별

numericVars <- which(sapply(all, is.numeric))
numericVarNames <- all %>% select_if(is.numeric) %>% colnames
cat('There are', length(numericVarNames), 'numeric variables')

# 37개의 수치형 독립 변수 존재
all_numVar <- all[, numericVars]
cor_numVar <- cor(all_numVar, use = "pairwise.complete.obs") # 전 numeric 변수의 상관 계수
cor_sorted <- as.matrix(sort(cor_numVar[, 'SalePrice'], decreasing = TRUE))

# 3.2. 상관계수가 큰 변수만을 선택해보자
CorHigh    <- names(which(apply(cor_sorted, 1, function(x) abs(x) > 0.5)))
cor_numVar <- cor_numVar[CorHigh, CorHigh]

corrplot.mixed(cor_numVar,
               tl.col = 'black', # 변수명 색깔
               tl.pos = 'lt',    # 변수명 왼쪽 표시 ** -> 이거 좋네
               number.cex = .7)  # matrix안 상관계수 text 크기

# corrplot에서 확인할 수 있듯이, 가장 상관관계가 높은 두 변수
# OverallQual, GrLivArea를 시각화 해보자.
# 또한, 다중공선성 문제도 동시에 해결해보자(예를들어, GarageArea와 GarageCar는 상관관계가 매우 높다)

# OverallQual : 집 전체적인 quality
# GrLivArea   : 지하실 제외한 지상의 면적

# 3.3. Overall Quality
ggplot(data = all[!is.na(all$SalePrice),], aes(x = factor(OverallQual), y= SalePrice)) +
  geom_boxplot(col = 'blue') + labs(x = 'Overall Quality') + 
  scale_y_continuous(breaks = seq(0, 800000, by = 100000), labels = comma)

# SalePrice와 Overall Quality 간은 양의 상관 관계를 보이며,
# 4 등급의 비싼 집 같은 경우는 이상치일 확률이 있다.
# 나머지는 특출난 값은 보이지 않는다

# 3.4 GrLivArea(Above Grade Ground Living Area (square feet))
# 많은 관측치가 있지만, 당연하게 큰집은 일반적으로 비싸다

# ggplot : 기존 y = x 선형 그래프에 해당 데이터의 점을 찍음

ggplot(data = all[!is.na(all$SalePrice),], aes(x = GrLivArea, y = SalePrice)) +
  geom_point(col = 'blue') +
  geom_smooth(method = 'lm', se = FALSE, color = 'black', aes(group = 1)) +
  scale_y_continuous(breaks = seq(0, 800000, by = 100000), labels = comma) +
  geom_text_repel(aes(label = ifelse(all$GrLivArea[!is.na(all$SalePrice)] > 4500, rownames(all), '')))

# -> 면적 대비 가격이 낮은 두 개의 이상치 같은 것이 보임(524, 1299) : 그냥 빼는 것이 능사인가? 
#    이럴때는 다른 변수의 값들을 체크하면서 이상치가 맞는지, 아니면 그럴만한 이유가 있는지를 파악하는 것도 좋다

all[c(524, 1299), c('SalePrice', 'GrLivArea', 'OverallQual')]  

# -> 확인 결과 최고 등급의 집으로 나타나고 있다. 그러면 특정 변수에 의해 가격이 낮게 책정되었을 수도 있으므로
#    기억만 하고 넘어가자




