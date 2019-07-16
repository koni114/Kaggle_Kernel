## 4. 결측치가 포함된 변수 확인

NAcol <- which(colSums(is.na(all)) > 0)                                  # 결측이 1개라도 있는 변수 선별 
NAcolSum <- sort(colSums(sapply(all[NAcol],  is.na)), decreasing = TRUE) # 컬럼 별 결측 데이터 개수 확인
NAcolSum / nrow(all)                                                     # 결측 비율 확인

# c총 37개의 수치형 변수 중 35개의 변수가 결측을 포함하고 있음
# 그중 50% 이상의 결측을 포함하고 있는 변수는 4개(PoolQC, Mis) -> 뭔가 조치가 필요
# 데이터의 특성에 따라 변수를 제거 하거나, 결측치 대체
cat('There are', length(NAcol), 'columns with missing values')

# 4.1. 결측치 처리
# 결측치가 포함된 34개 독립 변수 수정 -> 개수의 내림차순으로 작업
# 다른 변수와 유의미한 관계를 갖는 변수를 찾는다면, 그룹핑 : ex) Pool, Garage, Basement

# 4.1.1. Pool variable
# Ex, Gd, TA, Fa, NA 5개의 level로 구성되어 있지만, 대부분 결측 
# -> NA 값은 NONE으로 대체

all$PoolQC[is.na(all$PoolQC)] <- 'None'

# 이 변수들을 순서형으로 변환. 
# 동일한 품질 수준을 쓰는 다양한 변수가 있으므로, 이후에 사용하기 위해 벡터 생성

Qualities <- c('None' = 0, 'Po' = 1, 'Fa' = 2, 'TA' = 3, 'Gd' = 4, 'Ex' = 5)

# revalue 함수를 이용해서 정수형으로 변환

all$PoolQC <- as.integer(revalue(all$PoolQC, Qualities))
table(all$PoolQC)

# PoolArea 변수를 이용해 PoolQC 결측 변수 3개 대체 
# OverallQual 을 이용해서 대체

all[all$PoolArea > 0 & all$PoolQC == 0, c('PoolArea', 'PoolQC', 'OverallQual')]
all$PoolQC[2421] <- 2
all$PoolQC[2504] <- 3
all$PoolQC[2600] <- 2


# 4.1.2 MiscFeature 변수
# Miscellanuous Feature 는 2814개의 결측 존재, 데이터 타입은 순서형이 아니기에 factor로 변환
# Elev : Elevator
# Gar2 : 2nd Garage
# Othr : Other
# Shed : Shed(창고) (over 100 SF)
# TenC : Tennis Court
# NA   : None

all$MiscFeature[is.na(all$MiscFeature)] <- 'None'
all$MiscFeature <- as.factor(all$MiscFeature)

ggplot(all[!is.na(all$SalePrice), ] , aes(x = MiscFeature, y = SalePrice)) +
  geom_bar(stat = 'summary', fun.y = 'median', fill = 'blue') +
  scale_y_continuous(breaks = seq(0, 800000, by = 100000), labels = comma) +
  geom_label(stat = 'count', aes(label = ..count.., y = ..count..))

table(all$MiscFeature)

# 큰 의미는 없다. 헛간이 있는 집이 낮은 가격을 가지는 이유는 no Garage 일 것이라 추측
# 테니스장이 있는 집은 당연히..비싸겠지

#  4.1.3 Alley 
# 집에 접근하는 골목길 유형(자갈, 포장도로, 없음)
# -> 순서형은 아니기에 범주형으로 전환

all$Alley[is.na(all$Alley)] <- 'None'
all$Alley <- as.factor(all$Alley)

# 중앙 값 y 
ggplot(all[!is.na(all$SalePrice), ] , aes(x = Alley, y = SalePrice)) +
  geom_bar(stat = 'summary', fun.y = 'median', fill = 'blue') +
  scale_y_continuous(breaks = seq(0, 200000, by = 50000), labels = comma) 

table(all$Alley)

#  4.1.4 Fence
# 해당 변수는 순서형 변수가 아니기 때문에 결측 처리 후factor로 변환

all$Fence[is.na(all$Fence)] <- 'None'
table(all$Fence)

all[!is.na(all$SalePrice),] %>% 
  group_by(Fence) %>%  #Fence 그룹핑
  summarise(median = median(SalePrice), counts = n()) #Fence변수의 price 중위값, 개수 확인

all$Fence <- as.factor(all$Fence)

# 4.1.5 Fireplace(화로) quality, and Number of fireplaces
# FireplaceQu의 결측 개수는 fireplace 개수가 0인 것들과 일치한다
# 순서형 변수이므로, 이전에 Qualities 벡터를 사용하겠다

# Ex   Excellent - Exceptional Masonry Fireplace
# Gd   Good - Masonry Fireplace in main level
# TA   Average - Prefabricated Fireplace in main living area or Masonry Fireplace in basement
# Fa     Fair - Prefabricated Fireplace in basement
# Po     Poor - Ben Franklin Stove
# NA     No Fireplace 

all$FireplaceQu[is.na(all$FireplaceQu)] <- 'None'
all$FireplaceQu <- as.integer(revalue(all$FireplaceQu, Qualities))
table(all$FireplaceQu)

# 정수형으로 변환하였고, 결측치는 없기에 별도의 작업이 필요 없음

# 4.1.6 Lot variable
# 3개의 변수 중, 2개의 변수는 결측이 없고, 나머지 한개의 변수는 결측이 존재(486개)

# ** LotFrontage :  Linear feet of street connected to property 
# -> 각 동네의 중위값을 구해 이로 대체

ggplot(all[!is.na(all$LotFrontage),], 
       aes(x = as.factor(Neighborhood), y = LotFrontage)) +
  geom_bar(stat = 'summary', fun.y = 'median', fill = 'blue') + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Text 45도 기울기, 높이는 1로 설정

# 반복문을 통해 결측 값 -> 중위 값으로 대체
for(i in 1:nrow(all)){
  if(is.na(all$LotFrontage[i])){
    all$LotFrontage[i] <- as.integer(median(all$LotFrontage[all$Neighborhood == all$Neighborhood[i]], na.rm = TRUE))
  }
}

# ** LotShape: General shape of property
# 결측치가 없고, 순서형 타입으로 파악
# Reg    Regular 
# IR1    Slightly irregular
# IR2    Moderately Irregular
# IR3    Irregular  

all$LotShape <- as.integer(revalue(all$LotShape, c('IR3' = 0
                                                   , 'IR2' = 1
                                                   , 'IR1' = 2
                                                   , 'Reg' = 3)))

table(all$LotShape)

# ** LotConfig : Lot configuration
# Inside   Inside lot
# Corner   Corner lot
# CulDSac  Cul-de-sac
# FR2  Frontage on 2 sides of property
# FR3  Frontage on 3 sides of property

ggplot(all[!is.na(all$SalePrice),], aes(x = as.factor(LotConfig), y = SalePrice)) +
  geom_bar(stat = 'summary', fun.y = 'median', fill = 'blue') +
  scale_y_continuous(breaks = seq(0, 800000, by = 100000), labels = comma) +
  geom_label(stat = 'count', aes(label = ..count.., y = ..count..))


all$LotConfig <- as.factor(all$LotConfig)
table(all$LotConfig)

sum(table(all$LotConfig))

# 4.1.7 Garage variable
# 결측치의 개수는 2개 변수(GarageCars, GarageArea)가 1개
# GarageType : 157개, 나머지 4개 변수 159개

# 우선적으로 GarageYrBlt : 창고 건축년도의 결측치 값을 YearBuilt의 값으로 대체
# 리모델링이나 증축을 하지 않은 yearBuilt 의 default 값이 YearReomdAdd 변수와 유사

all$GarageYrBlt[is.na(all$GarageYrBlt)] <- all$YearBuilt[is.na(all$GarageYrBlt)]

# 결측치는 문자형 타입이며, 'no garage'를 의미
# -> 157개와 159개의 결측치를 가진 세 변수의 차이점을 찾아보자
length(which(is.na(all$GarageType) & is.na(all$GarageFinish) & is.na(all$GarageCond) & is.na(all$GarageQual)))
# -> 157이 나왔다는 것은 157개의 결측치 변수가 동일하다는 의미

# 그렇다면 나머지 2개는 뭘까? 
# 한번 확인해보자 kable
kable(all[!is.na(all$GarageType) & is.na(all$GarageFinish), 
          c('GarageCars', 'GarageArea', 'GarageType', 'GarageCond', 'GarageQual', 'GarageFinish')])

# 2127, 2577 의 다른 두채를 확인할 수 있는데,
# 2127은 차고지가 있지만 2577은 없다. 따라서 2127은 나머지 3개 변수(GarageCond, GarageQual, GarageFinish)
# 의 최빈값으로 대체 하겠다

# 최빈값으로 대체
all$GarageCond[2127] <- names(sort(-table(all$GarageCond)))[1]
all$GarageQual[2127] <- names(sort(-table(all$GarageQual)))[1]
all$GarageFinish[2127] <- names(sort(-table(all$GarageFinish)))[1]

# 대체 후 값 확인
kable(all[2127, c('GarageYrBlt', 'GarageCars', 'GarageArea', 'GarageType', 'GarageCond', 'GarageQual', 'GarageFinish')])

# 2577의 GarageCar, GarageArea 를 0으로 대체 하고 나머지 변수들은 NA로 대체 하자
all$GarageCars[2577] <- 0
all$GarageArea[2577] <- 0
all$GarageType[2577] <- NA

# 문자형 변수인 4 변수의 결측치가 모두 158개인지 확인해보겠다.
length(which(is.na(all$GarageType) & is.na(all$GarageFinish) & is.na(all$GarageCond) & is.na(all$GarageQual)))

# -> 4개의 문자형 변수는 Garage와 연관이 있으며, 모든 셋에 158개의 동일한 결측치가 존재
# -> 이건 'No garage' 를 의미

# ** GarageType : Garage 위치에 따른 타입
# -> 순서형 변수가 아니기에 factor로 변환
# 2Types   More than one type of garage
# Attchd   Attached to home
# Basment  Basement Garage
# BuiltIn  Built-In (Garage part of house - typically has room above garage)
# CarPort  Car Port
# Detchd   Detached from home
# NA       No Garage

all$GarageType[is.na(all$GarageType)] <- 'No Garage'
all$GarageType <- as.factor(all$GarageType)
table(all$GarageType)

# ** GarageFinish : 인테리어 완료
# 순서형 변수이다

# Fin  Finished
# RFn  Rough Finished  
# Unf  Unfinished
# NA   No Garage  

all$GarageFinish[is.na(all$GarageFinish)] <- 'None'
Finish <- c('None' = 0, 'Unf' = 1, 'RFn' = 2, 'Fin' = 3)

all$GarageFinish <- as.integer(revalue(all$GarageFinish, Finish))
table(all$GarageFinish)

# ** GarageQual -> GarageQuality
# 순서형으로 변환

# Ex   Excellent
# Gd   Good
# TA   Typical/Average
# Fa   Fair
# Po   Poor
# NA   No Garage

all$GarageQual[is.na(all$GarageQual)] <- 'None'
all$GarageQual <- as.integer(revalue(all$GarageQual, Qualities))
table(all$GarageQual)

# ** ** GarageCond -> Garage condition 
# 순서형으로 변환
# Ex   Excellent
# Gd   Good
# TA   Typical/Average
# Fa   Fair
# Po   Poor
# NA   No Garage

all$GarageCond[is.na(all$GarageCond)] <- 'None'
all$GarageCond <- as.integer(revalue(all$GarageCond, Qualities))
table(all$GarageCond)

## 4.1.8. Basement variables : 지하실 관련 변수 -> 다중공선성을 고려해야 할수도!
# BsmtQual:     지하실 높이를 등급화
# BsmtCond:     지하실의 일반적인 상태로 본 등급
# BsmtExposure: 벽이나 정원의 도보 적합도 수준
# BsmtFinType1: 지하실 완공 면적의 등급
# BsmtFinType2: multiple type일 경우의 지하실 완공 면적 등급
# BsmtFullBath: full bathrooms을 갖춘 지하실
# BsmtHalfBath: half bathrooms을 갖춘 지하실
# BsmtFinSF1:   크기가 1 평방 미터인 지하실
# BsmtFinSF2:   크기가 2 평방 미터인 지하실
# BsmtUnfSF:    면적이 평방 미터로 측정이 안되는 지하실
# TotalBsmtSF:  지하실의 총 평방 미터

# 이중 5개의 변수는 79 - 82개의 변수, 6개의 변수는 1~2개의 결측치가 존재

# 79개의 결측치 관측치가 80개 이상의 결측 관측치와 동일한지 확인

length(which(is.na(all$BsmtQual) & is.na(all$BsmtCond) & is.na(all$BsmtExposure) & 
               is.na(all$BsmtFinType1) & is.na(all$BsmtFinType2)))

# --> 동일함을 확인!

# 추가 결측치 찾기 
#  BsmtFinType1은 결측치가 아니지만, 다른 4개 변수들이 결측치인 경우
all[!is.na(all$BsmtFinType1) & (is.na(all$BsmtCond) | is.na(all$BsmtQual) | 
                                  is.na(all$BsmtExposure) | is.na(all$BsmtFinType2)), 
    c('BsmtQual', 'BsmtCond', 'BsmtExposure', 'BsmtFinType1', 'BsmtFinType2')]

# 79개는 지하실이 없는 것으로 보이지만, 나머지 9채는 중간 중간 결측치가 확인된다.
# 이또한 최빈값으로 대체

all$BsmtFinType2[333] <- names(sort(-table(all$BsmtFinType2)))[1]
all$BsmtExposure[c(949, 1488, 2349)] <- names(sort(-table(all$BsmtExposure)))[1]
all$BsmtQual[c(2218,2219)] <- names(sort(-table(all$BsmtQual)))[1]
all$BsmtCond[c(2041, 2186, 2525)] <- names(sort(-table(all$BsmtCond)))[1]

# 79 ~ 82개의 결측치를 가진 변수는 지하실이 없는 것으로 판명되므로,
# None 으로 대체 후, hot encoding 수행

# ** BsmtQual: 지하실 높이를 등급화 **
# 순서형 변수로 변환
# Ex   Excellent (100+ inches) 
# Gd   Good (90-99 inches)
# TA   Typical (80-89 inches)
# Fa   Fair (70-79 inches)
# Po   Poor (<70 inches
# NA   No Basement
           
sum(is.na(all$BsmtQual))

all$BsmtQual[is.na(all$BsmtQual)] <- 'None'
all$BsmtQual <- as.integer(revalue(all$BsmtQual, Qualities))
table(all$BsmtQual)

# ** BsmtCond: 지하실의 일반적인 상태로 본 등급 
# 순서형 변수로 변환
# Ex   Excellent
# Gd   Good
# TA   Typical - slight dampness allowed
# Fa   Fair - dampness or some cracking or settling
# Po   Poor - Severe cracking, settling, or wetness
# NA   No Basement

sum(is.na(all$BsmtCond))

all$BsmtCond[is.na(all$BsmtCond)] <- 'None'
all$BsmtCond <- as.integer(revalue(all$BsmtCond, Qualities))
table(all$BsmtCond)

# ** BsmtExposure: 벽이나 정원의 도보 적합도 수준
# 순서형 변수로 변환
# Gd   Good Exposure
# Av   Average Exposure (split levels or foyers typically score average or above)  
# Mn   Mimimum Exposure
# No   No Exposure
# NA   No Basement

all$BsmtExposure[is.na(all$BsmtExposure)] <- 'None'
Exposure <- c('None' = 0, 'No' = 1, 'Mn' = 2, 'Av' = 3, 'Gd' = 4)

all$BsmtExposure <- as.integer(revalue(all$BsmtExposure, Exposure))
table(all$BsmtExposure)

# ** BsmtFinType1: 지하실 완공 면적의 등급 **
# 순서형 변수로 변환
# GLQ  Good Living Quarters
# ALQ  Average Living Quarters
# BLQ  Below Average Living Quarters   
# Rec  Average Rec Room
# LwQ  Low Quality
# Unf  Unfinshed
# NA   No Basement

all$BsmtFinType1[is.na(all$BsmtFinType1)] <- 'None'
Fintype <- c('None' = 0, 'Unf' = 1, 'LwQ' = 2, 'Rec' = 3, 'BLQ' = 4, 'ALQ' = 5, 'GLQ' = 6)

all$BsmtFinType1 <- as.integer(revalue(all$BsmtFinType1, Fintype))
table(all$BsmtFinType1)

# ** BsmtFinType2: multiple type일 경우의 지하실 완공 면적 등급
# 변수는 순서형 타입이고, Fintype 벡터이다.
# GLQ  Good Living Quarters
# ALQ  Average Living Quarters
# BLQ  Below Average Living Quarters   
# Rec  Average Rec Room
# LwQ  Low Quality
# Unf  Unfinshed
# NA   No Basement

all$BsmtFinType2[is.na(all$BsmtFinType2)] <- 'None'
all$BsmtFinType2 <- as.integer(revalue(all$BsmtFinType2, Fintype))
table(all$BsmtFinType2)

# 나머지 1~6개 결측치를 포함하는 6개의 변수 결측치 처리
# .. 노가다다다다다..

# 상기에 관측했던 지하실이 없었던 79채를 참고하여 남은 결측치를 확인해보자

all[(is.na(all$BsmtFullBath) | is.na(all$BsmtHalfBath) | is.na(all$BsmtFinSF1) | 
       is.na(all$BsmtFinSF2) | is.na(all$BsmtUnfSF) | is.na(all$TotalBsmtSF)), 
    c('BsmtQual', 'BsmtFullBath', 'BsmtHalfBath', 'BsmtFinSF1', 'BsmtFinSF2', 'BsmtUnfSF', 'TotalBsmtSF')]
all$BsmtFullBath[is.na(all$BsmtFullBath)] <- 0
table(all$BsmtFullBath)

# ** BsmtFullBath: full bathrooms을 갖춘 지하실
all$BsmtFullBath[is.na(all$BsmtFullBath)] <- 0
table(all$BsmtFullBath)

# ** BsmtHalfBath: half bathrooms을 갖춘 지하실
all$BsmtHalfBath[is.na(all$BsmtHalfBath)] <- 0
table(all$BsmtHalfBath)

# ** BsmtFinSF1: 크기가 1 평방 미터인 지하실
all$BsmtFinSF1[is.na(all$BsmtFinSF1)] <- 0

# ** BsmtFinSF2: 크기가 2 평방 미터인 지하실
all$BsmtFinSF2[is.na(all$BsmtFinSF2)] <- 0

# ** BsmtUnfSF: 면적이 평방 미터로 측정이 안되는 지하
all$BsmtUnfSF[is.na(all$BsmtUnfSF)] <- 0

# ** TotalBsmtSF: 지하실의 총 평방 미터 
all$TotalBsmtSF[is.na(all$TotalBsmtSF)] <- 0

## 4.1.9. Masonry variable : 석조 
# Masonry veneer area : 석조 area
# Masonry veneer type : 석조 type
# 두 개의 결측 개수가 맞지 않는 것이 이상하다 ( 23개, 24개). 확인해보자

cat("MasVnrArea Na : " , sum(is.na(all$MasVnrArea)), " MasVnrType Na :",  sum(is.na(all$MasVnrType)))
length(which(is.na(all$MasVnrType) & is.na(all$MasVnrArea)))

# 차이나는 1개의 결측치를 확인해보자
all[is.na(all$MasVnrType) & !is.na(all$MasVnrArea), c('MasVnrType', 'MasVnrArea')]

# -> veneer type의 결측치를 최빈값으로 대체하자

all$MasVnrType[2611] <- names(sort(-table(all$MasVnrType)))[2] #최빈도는 'none'이라 두번째 빈도로 했다.
all[2611, c('MasVnrType', 'MasVnrArea')]

# veneer type과 veneer area의 동일한 23개 결측치는 석조가 없는걸 의미한다(no masonry)
# ** MasVnrType : Masonry veneer type

all$MasVnrType[is.na(all$MasVnrType)] <- 'None'
all[!is.na(all$SalePrice),] %>% 
  group_by(MasVnrType) %>% 
  dplyr::summarise(median = median(SalePrice), counts=n()) %>%  
  dplyr::arrange(median)

# -> common Brick / None <-> 나머지 두 level 간의 차이가 있는 것으로 보인다
# 즉, 심플한 돌이나 목조로 만든 주택이 저렴한 것으로 추정된다 -> 메모 후 확인해보자

Masonry <- c('None' = 0, 'BrkCmn' = 0, 'BrkFace' = 1, 'Stone' = 2)
all$MasVnrType <- as.integer(revalue(all$MasVnrType, Masonry))
table(all$MasVnrType)

Masonry <- c('None' = 0, 'BrkCmn' = 0, 'BrkFace' = 1, 'Stone' = 2)
all$MasVnrType <- as.integer(revalue(all$MasVnrType, Masonry))
table(all$MasVnrType)

# ** MasVnrArea: 평방 미터 안의 Masonry veneer area
# 정수형 변수이므로, 0으로 대체하자

all$MasVnrArea[is.na(all$MasVnrArea)] <- 0


# ** 4.1.10 ** MSZoning variables : 용도별 지구(지역지구) 식별자
# 결측치는 4개이고, 값은 범주형
# -> 최빈값으로 대체

# A    Agriculture
# C    Commercial
# FV   Floating Village Residential
# I    Industrial
# RH   Residential High Density
# RL   Residential Low Density
# RP   Residential Low Density Park 
# RM   Residential Medium Density

all$MSZoning[is.na(all$MSZoning)] <- names(sort(-table(all$MSZoning)))[1]
all$MSZoning <- as.factor(all$MSZoning)
table(all$MSZoning)

# ** 4.1.11 : Kitchen Variables : Kitchen quality and number of Kitchen above grade
# Kitchen quality 는 결측치가 1개, number of Kitchen 은 결측치가 없다.
# ** Kitchen quality : 부엌 퀄리리
# 순서형으로 변환

# Ex   Excellent
# Gd   Good
# TA   Typical/Average
# Fa   Fair
# Po   Poor

all$KitchenQual[is.na(all$KitchenQual)] <- 'TA' 
all$KitchenQual <- as.integer(revalue(all$KitchenQual, Qualities))
table(all$KitchenQual)

# ** KitchenAbvGr : Number of Kitchens above grade 
# AllPub   All public Utilities (E,G,W,& S)    
# NoSewr   Electricity, Gas, and Water (Septic Tank)
# NoSeWa   Electricity and Gas Only
# ELO  Electricity only

# -> 순서형 변수이고, 결측치는 없다

# ** 4.1.12 : Utilities variables :  사용할 수 있는 Utilities의 종류
# 결측치는 2개
# 특이하게도, 관측된 변수 중 한 집을 제외하곤 전부 AllPub 이다.
# testData에서 전부 AllPub을 가지고 있으므로, 해당 변수는 전부 지우도록 하겠다

table(all$Utilities)
kable(all[is.na(all$Utilities) | all$Utilities == "NoSeWa", 1:9])
all$Utilities <- NULL

# ** 4.1.13 : Home functionality variables : 집에 있는 기능들
# 결측치는 한개이고, 순서형 타입으로 변환
# Typ이 가장 좋고(8),Sal이 가장 안좋다(0)
# Typ  Typical Functionality
# Min1 Minor Deductions 1
# Min2 Minor Deductions 2
# Mod  Moderate Deductions
# Maj1 Major Deductions 1
# Maj2 Major Deductions 2
# Sev  Severely Damaged
# Sal  Salvage only

all$Functional[is.na(all$Functional)] <- names(sort(-table(all$Functional)))[1]
all$Functional <- as.integer(revalue(all$Functional, +
                                       c('Sal' = 0, 'Sev' = 1, 'Maj2' = 2, 'Maj1' = 3, 'Mod' = 4, 'Min2' = 5, 'Min1' = 6, 'Typ' = 7)))
table(all$Functional)

# ** 4.1.13 : Exterior variables : 건물 외장 변수
# 총 4개의 변수 존재

# ** Exterior1st : 건물 외장 형태
# 결측치는 1개이며, 값은 범주형이다. -> 최빈값으로 대체
 
# AsbShng  Asbestos Shingles
# AsphShn  Asphalt Shingles
# BrkComm  Brick Common
# BrkFace  Brick Face
# CBlock   Cinder Block
# CemntBd  Cement Board
# HdBoard  Hard Board
# ImStucc  Imitation Stucco
# MetalSd  Metal Siding
# Other    Other
# Plywood  Plywood
# PreCast  PreCast 
# Stone    Stone
# Stucco   Stucco
# VinylSd  Vinyl Siding
# Wd Sdng  Wood Siding
# WdShing  Wood Shingles

all$Exterior1st[is.na(all$Exterior1st)] <- names(sort(-table(all$Exterior1st)))[1]
all$Exterior1st <- as.factor(all$Exterior1st)
table(all$Exterior1st)

all$Exterior2nd[is.na(all$Exterior2nd)] <- names(sort(-table(all$Exterior2nd)))[1]
all$Exterior2nd <- as.factor(all$Exterior2nd)
table(all$Exterior2nd)

# ** Exterior2nd: 건물 외장 형태(2개 이상의 형태를 쓴 경우) **
# AsbShng  Asbestos Shingles
# AsphShn  Asphalt Shingles
# BrkComm  Brick Common
# BrkFace  Brick Face
# CBlock   Cinder Block
# CemntBd  Cement Board
# HdBoard  Hard Board
# ImStucc  Imitation Stucco
# MetalSd  Metal Siding
# Other    Other
# Plywood  Plywood
# PreCast  PreCast 
# Stone    Stone
# Stucco   Stucco
# VinylSd  Vinyl Siding
# Wd Sdng  Wood Siding
# WdShing  Wood Shingles

# ** ExterQual: 사용한 외장 형태의 등급 
# 결측치는 없고, Qualities 벡터로 순서형으로 변환한다.
all$ExterQual <- as.integer(revalue(all$ExterQual, Qualities))

# ** Extercond: 현재 외장의 상태 등급 
# 결측치는 없고, Qualities 벡터로 순서형으로 변환

all$ExterCond <- as.integer(revalue(all$ExterCond, Qualities))
table(all$ExterCond)

# 4.1.14 : Electrical System variables : 전기 시스템
# 결측치는 1개이고, 값은 범주형이다.
# SBrkr    Standard Circuit Breakers & Romex
# FuseA    Fuse Box over 60 AMP and all Romex wiring (Average) 
# FuseF    60 AMP Fuse Box and mostly Romex wiring (Fair)
# FuseP    60 AMP Fuse Box and mostly knob & tube wiring (poor)
# Mix      Mixed

all$Electrical[is.na(all$Electrical)] <- names(sort(-table(all$Electrical)))[1]
all$Electrical <- as.factor(all$Electrical)
table(all$Electrical)

# 4.1.15 : SaleType and Condition

# WD       Warranty Deed - Conventional
# CWD      Warranty Deed - Cash
# VWD      Warranty Deed - VA Loan
# New      Home just constructed and sold
# COD      Court Officer Deed/Estate
# Con      Contract 15% Down payment regular terms
# ConLw    Contract Low Down payment and low interest
# ConLI    Contract Low Interest
# ConLD    Contract Low Down
# Oth      Other

all$SaleType[is.na(all$SaleType)] <- names(sort(-table(all$SaleType)))[1]
all$SaleType <- as.factor(all$SaleType)
table(all$SaleType)

# SaleCondition : 판매 조건

all$SaleCondition <- as.factor(all$SaleCondition)
table(all$SaleCondition)

