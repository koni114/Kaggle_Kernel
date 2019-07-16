# 5.3 Label encoding/ factorizing the ramaining character variables
# 결측치는 살펴보았지만, character type 변수들은 확인하지 않았다
# 그래서 이번에도 탭으로 구분해서 살펴보도록 하겠다

Charcol <- names(all[,sapply(all, is.character)])
cat('There are', length(Charcol), 'remaining columns with character values')

# 5.3.1 ** Foundation: 건물 기초(토대)의 종류

# BrkTil          Brick & Tile
# CBlock          Cinder Block
# PConc           Poured Contrete 
# Slab            Slab
# Stone           Stone
# Wood            Wood

# 순서형 아니기에, factor 형으로 변환

all$Foundation <- as.factor(all$Foundation)
table(all$Foundation)

# 5.3.2 ** Heating 변수와 1개의 에어컨 변수(Y/N)

# Heating : 난방 장치 종류

# Floor    Floor Furnace
# GasA Gas forced warm air furnace
# GasW Gas hot water or steam heat
# Grav Gravity furnace 
# OthW Hot water or steam heat other than gas
# Wall Wall furnace

# 순서형 아니기에, factor 형으로 변환
all$Heating <- as.factor(all$Heating)
table(all$Heating)

# ** HeatingQC: 난방 장치의 품질과 상태
# Ex   Excellent
# Gd   Good
# TA   Average/Typical
# Fa   Fair
# Po   Poor

all$HeatingQC <- as.integer(revalue(all$HeatingQC, Qualities))

# ** CentralAir: 중앙 냉방 에어컨 유무
# N    No
# Y    Yes

all$CentralAir <- as.integer(revalue(all$CentralAir, c('N' = 0, 'Y' = 1)))
table(all$CentralAir)

# 5.3.3 ** RoofStyle: 지붕의 종류 

# ** RoofStyle: 지붕의 종류
# Flat Flat
# Gable    Gable
# Gambrel  Gabrel (Barn)
# Hip  Hip
# Mansard  Mansard
# Shed Shed

all$RoofStyle <- as.factor(all$RoofStyle)
table(all$RoofStyle)

# ** RoofMatl: 지붕 재료

# ClyTile  Clay or Tile
# CompShg  Standard (Composite) Shingle
# Membran  Membrane
# Metal    Metal
# Roll Roll
# Tar&Grv  Gravel & Tar
# WdShake  Wood Shakes
# WdShngl  Wood Shingles

# 순서형이 아니기에, factor형으로 변환하겠다.
all$RoofMatl <- as.factor(all$RoofMatl)
table(all$RoofMatl)

# 5.3.4 : Land : 부지가 평지인지, 비탈인지 2개의 변수

# ** LandContour: 부지의 평탄함
# Lvl  Near Flat/Level 
# Bnk  Banked - Quick and significant rise from street grade to building
# HLS  Hillside - Significant slope from side to side
# Low  Depression

# 순서형이 아니기에, factor형으로 변환하겠다.
all$LandContour <- as.factor(all$LandContour)
table(all$LandContour)

# ** LandSlope: 부지의 경사(비탈)
# Gtl  Gentle slope
# Mod  Moderate Slope  
# Sev  Severe Slope

# 순서형 타입, 정수형으로 변환하겠다.
all$LandSlope <- as.integer(revalue(all$LandSlope, c('Sev' = 0, 'Mod' = 1, 'Gtl' = 2)))
table(all$LandSlope)

# 5.3.5 : Dwell : 주거에 따른 2개의 변수
# ** BldgType: 주거의 형태

# 1Fam     Single-family Detached  
# 2FmCon   Two-family Conversion; originally built as one-family dwelling
# Duplx    Duplex
# TwnhsE   Townhouse End Unit
# TwnhsI   Townhouse Inside Unit

# 순서형 범주가 아니기에, factor로 변환한다.
all$BldgType <- as.factor(all$BldgType)
table(all$BldgType)

# ** HouseStyle: 주거의 Style 
# 1Story   One story
# 1.5Fin   One and one-half story: 2nd level finished
# 1.5Unf   One and one-half story: 2nd level unfinished
# 2Story   Two story
# 2.5Fin   Two and one-half story: 2nd level finished
# 2.5Unf   Two and one-half story: 2nd level unfinished
# SFoyer   Split Foyer
# SLvl     Split Level

all$HouseStyle <- as.factor(all$HouseStyle)
table(all$HouseStyle)

# 5.3.6 : 물리적 거리, 근방의 입지에 따른 3개의 변수

# ** Neighborhood: 물리적 위치는 ‘Ames’ 시내이다.
# Blmngtn  Bloomington Heights
# Blueste  Bluestem
# BrDale   Briardale
# BrkSide  Brookside
# ClearCr  Clear Creek
# CollgCr  College Creek
# Crawfor  Crawford
# Edwards  Edwards
# Gilbert  Gilbert
# IDOTRR   Iowa DOT and Rail Road
# MeadowV  Meadow Village
# Mitchel  Mitchell
# Names    North Ames
# NoRidge  Northridge
# NPkVill  Northpark Villa
# NridgHt  Northridge Heights
# NWAmes   Northwest Ames
# OldTown  Old Town
# SWISU    South & West of Iowa State University
# Sawyer   Sawyer
# SawyerW  Sawyer West
# Somerst  Somerset
# StoneBr  Stone Brook
# Timber   Timberland
# Veenker  Veenker

all$Neighborhood <- as.factor(all$Neighborhood)
table(all$Neighborhood)

# ** Condition1: 근방의 다양한 조건
# 
# Artery   Adjacent to arterial street
# Feedr    Adjacent to feeder street   
# Norm Normal  
# RRNn Within 200' of North-South Railroad
# RRAn Adjacent to North-South Railroad
# PosN Near positive off-site feature--park, greenbelt, etc.
# PosA Adjacent to postive off-site feature
# RRNe Within 200' of East-West Railroad
# RRAe Adjacent to East-West Railroad

# 순서형 범주가 아니기에, factor로 변환한다.
all$Condition1 <- as.factor(all$Condition1)
table(all$Condition1)

# ** Condition2: 근방의 다양한 조건 (2개 이상)

# Artery   Adjacent to arterial street
# Feedr    Adjacent to feeder street   
# Norm Normal  
# RRNn Within 200' of North-South Railroad
#  RRAn Adjacent to North-South Railroad
#  PosN Near positive off-site feature--park, greenbelt, etc.
#  PosA Adjacent to postive off-site feature
#  RRNe Within 200' of East-West Railroad
# RRAe Adjacent to East-West Railroad

# 순서형 범주가 아니기에, factor로 변환한다.
all$Condition2 <- as.factor(all$Condition2)
table(all$Condition2)

# 5.3.7 Street variables 
# 2개의 변수 존재

# ** Street: 부지에 접한 길의 종류
# Grvl Gravel  
# Pave Paved

# 순서형 범주로, 정수형으로 변환하겠다.
all$Street <- as.integer(revalue(all$Street, c('Grvl' = 0, 'Pave' = 1)))
table(all$Street)

# ** PavedDrive: 진입로의 포장
# Y    Paved 
# P    Partial Pavement
# N    Dirt/Gravel

#순서형 범주로, 정수형으로 변환하겠다.
all$PavedDrive <- as.integer(revalue(all$PavedDrive, c('N' = 0, 'P' = 1, 'Y' = 2)))
table(all$PavedDrive)

# ** 5. numeric variables -> factor variables
# 현재 수치형 변수로 되어있지만, 범주형으로 변환해야 할 변수들이 존재한다

# 5.4.1 Year and Month Sold
# 판매 연도에서 YearBuilt(or remodeled)의 범주는 오래된 집이 가치가 낮게 되어있다.
# 2009년 subprime mortgage를 기준으로 집값이 매우 낮음을 확인 가능
# 모델링 전에는 YrSold 변수를 factor로 변환할 것이지만, Age 변수를 만들기 위해 수치형도 필요하므로, 일단 나둠
# Month Sold 도 범주형 변수로 변환

str(all$YrSold)
str(all$MoSold)

all$MoSold <- as.factor(all$MoSold)

# 예상보다 변동폭이 적지만, 금융 위기의 타격은 2007년말 부터 제대로 나타남
# 매매가는 2007년이 가장 높은 중위값을 보인 이후에 점차 감소
# 계절별로 더 큰 규칙이 있어 보임

# 연 단위 집 값
ys <- ggplot(all[!is.na(all$SalePrice),], aes(x = as.factor(YrSold), y = SalePrice)) +
  geom_bar(stat = 'summary', fun.y = 'median', fill = 'blue') +
  scale_y_continuous(breaks = seq(0, 800000, by = 25000), labels = comma) +
  geom_label(stat = 'count', aes(label = ..count.., y = ..count..)) +
  coord_cartesian(ylim = c(0, 200000)) +
  geom_hline(yintercept = 163000, linetype = 'dashed', color = 'red')

# 월 단위 집 값
ms <- ggplot(all[!is.na(all$SalePrice),], aes(x = MoSold, y = SalePrice)) + 
  geom_bar(stat = 'summary', fun.y = 'median', fill = 'blue') +
  scale_y_continuous(breaks = seq(0, 800000, by = 25000), labels = comma) + 
  geom_label(stat = 'count', aes(label = ..count.., y = ..count..)) + 
  coord_cartesian(ylim = c(0, 200000)) +
  geom_hline(yintercept = 163000, linetype = 'dashed', color = 'red') 

grid.arrange(ys, ms, widths = c(1,2))

# MSSubClass 
# ** MSSubClass: 판매와 연관된 주거 타입
str(all$MSSubClass)
all$MSSubClass <- as.factor(all$MSSubClass)
all$MSSubClass <- revalue(all$MSSubClass, c('20'='1 story 1946+', '30'='1 story 1945-', '40'='1 story unf attic', '45'='1,5 story unf', '50'='1,5 story fin', '60'='2 story 1946+', '70'='2 story 1945-', '75'='2,5 story all ages', '80'='split/multi level', '85'='split foyer', '90'='duplex all style/age', '120'='1 story PUD 1946+', '150'='1,5 story PUD all', '160'='2 story PUD 1946+', '180'='PUD multilevel', '190'='2 family conversion'))

