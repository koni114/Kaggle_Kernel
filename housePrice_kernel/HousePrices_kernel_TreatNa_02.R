## 4. ����ġ�� ���Ե� ���� Ȯ��

NAcol <- which(colSums(is.na(all)) > 0)                                  # ������ 1���� �ִ� ���� ���� 
NAcolSum <- sort(colSums(sapply(all[NAcol],  is.na)), decreasing = TRUE) # �÷� �� ���� ������ ���� Ȯ��
NAcolSum / nrow(all)                                                     # ���� ���� Ȯ��

# c�� 37���� ��ġ�� ���� �� 35���� ������ ������ �����ϰ� ����
# ���� 50% �̻��� ������ �����ϰ� �ִ� ������ 4��(PoolQC, Mis) -> ���� ��ġ�� �ʿ�
# �������� Ư���� ���� ������ ���� �ϰų�, ����ġ ��ü
cat('There are', length(NAcol), 'columns with missing values')

# 4.1. ����ġ ó��
# ����ġ�� ���Ե� 34�� ���� ���� ���� -> ������ ������������ �۾�
# �ٸ� ������ ���ǹ��� ���踦 ���� ������ ã�´ٸ�, �׷��� : ex) Pool, Garage, Basement

# 4.1.1. Pool variable
# Ex, Gd, TA, Fa, NA 5���� level�� �����Ǿ� ������, ��κ� ���� 
# -> NA ���� NONE���� ��ü

all$PoolQC[is.na(all$PoolQC)] <- 'None'

# �� �������� ���������� ��ȯ. 
# ������ ǰ�� ������ ���� �پ��� ������ �����Ƿ�, ���Ŀ� ����ϱ� ���� ���� ����

Qualities <- c('None' = 0, 'Po' = 1, 'Fa' = 2, 'TA' = 3, 'Gd' = 4, 'Ex' = 5)

# revalue �Լ��� �̿��ؼ� ���������� ��ȯ

all$PoolQC <- as.integer(revalue(all$PoolQC, Qualities))
table(all$PoolQC)

# PoolArea ������ �̿��� PoolQC ���� ���� 3�� ��ü 
# OverallQual �� �̿��ؼ� ��ü

all[all$PoolArea > 0 & all$PoolQC == 0, c('PoolArea', 'PoolQC', 'OverallQual')]
all$PoolQC[2421] <- 2
all$PoolQC[2504] <- 3
all$PoolQC[2600] <- 2


# 4.1.2 MiscFeature ����
# Miscellanuous Feature �� 2814���� ���� ����, ������ Ÿ���� �������� �ƴϱ⿡ factor�� ��ȯ
# Elev : Elevator
# Gar2 : 2nd Garage
# Othr : Other
# Shed : Shed(â��) (over 100 SF)
# TenC : Tennis Court
# NA   : None

all$MiscFeature[is.na(all$MiscFeature)] <- 'None'
all$MiscFeature <- as.factor(all$MiscFeature)

ggplot(all[!is.na(all$SalePrice), ] , aes(x = MiscFeature, y = SalePrice)) +
  geom_bar(stat = 'summary', fun.y = 'median', fill = 'blue') +
  scale_y_continuous(breaks = seq(0, 800000, by = 100000), labels = comma) +
  geom_label(stat = 'count', aes(label = ..count.., y = ..count..))

table(all$MiscFeature)

# ū �ǹ̴� ����. �갣�� �ִ� ���� ���� ������ ������ ������ no Garage �� ���̶� ����
# �״Ͻ����� �ִ� ���� �翬��..��ΰ���

#  4.1.3 Alley 
# ���� �����ϴ� ���� ����(�ڰ�, ���嵵��, ����)
# -> �������� �ƴϱ⿡ ���������� ��ȯ

all$Alley[is.na(all$Alley)] <- 'None'
all$Alley <- as.factor(all$Alley)

# �߾� �� y 
ggplot(all[!is.na(all$SalePrice), ] , aes(x = Alley, y = SalePrice)) +
  geom_bar(stat = 'summary', fun.y = 'median', fill = 'blue') +
  scale_y_continuous(breaks = seq(0, 200000, by = 50000), labels = comma) 

table(all$Alley)

#  4.1.4 Fence
# �ش� ������ ������ ������ �ƴϱ� ������ ���� ó�� ��factor�� ��ȯ

all$Fence[is.na(all$Fence)] <- 'None'
table(all$Fence)

all[!is.na(all$SalePrice),] %>% 
  group_by(Fence) %>%  #Fence �׷���
  summarise(median = median(SalePrice), counts = n()) #Fence������ price ������, ���� Ȯ��

all$Fence <- as.factor(all$Fence)

# 4.1.5 Fireplace(ȭ��) quality, and Number of fireplaces
# FireplaceQu�� ���� ������ fireplace ������ 0�� �͵�� ��ġ�Ѵ�
# ������ �����̹Ƿ�, ������ Qualities ���͸� ����ϰڴ�

# Ex   Excellent - Exceptional Masonry Fireplace
# Gd   Good - Masonry Fireplace in main level
# TA   Average - Prefabricated Fireplace in main living area or Masonry Fireplace in basement
# Fa     Fair - Prefabricated Fireplace in basement
# Po     Poor - Ben Franklin Stove
# NA     No Fireplace 

all$FireplaceQu[is.na(all$FireplaceQu)] <- 'None'
all$FireplaceQu <- as.integer(revalue(all$FireplaceQu, Qualities))
table(all$FireplaceQu)

# ���������� ��ȯ�Ͽ���, ����ġ�� ���⿡ ������ �۾��� �ʿ� ����

# 4.1.6 Lot variable
# 3���� ���� ��, 2���� ������ ������ ����, ������ �Ѱ��� ������ ������ ����(486��)

# ** LotFrontage :  Linear feet of street connected to property 
# -> �� ������ �������� ���� �̷� ��ü

ggplot(all[!is.na(all$LotFrontage),], 
       aes(x = as.factor(Neighborhood), y = LotFrontage)) +
  geom_bar(stat = 'summary', fun.y = 'median', fill = 'blue') + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Text 45�� ����, ���̴� 1�� ����

# �ݺ����� ���� ���� �� -> ���� ������ ��ü
for(i in 1:nrow(all)){
  if(is.na(all$LotFrontage[i])){
    all$LotFrontage[i] <- as.integer(median(all$LotFrontage[all$Neighborhood == all$Neighborhood[i]], na.rm = TRUE))
  }
}

# ** LotShape: General shape of property
# ����ġ�� ����, ������ Ÿ������ �ľ�
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
# ����ġ�� ������ 2�� ����(GarageCars, GarageArea)�� 1��
# GarageType : 157��, ������ 4�� ���� 159��

# �켱������ GarageYrBlt : â�� ����⵵�� ����ġ ���� YearBuilt�� ������ ��ü
# ���𵨸��̳� ������ ���� ���� yearBuilt �� default ���� YearReomdAdd ������ ����

all$GarageYrBlt[is.na(all$GarageYrBlt)] <- all$YearBuilt[is.na(all$GarageYrBlt)]

# ����ġ�� ������ Ÿ���̸�, 'no garage'�� �ǹ�
# -> 157���� 159���� ����ġ�� ���� �� ������ �������� ã�ƺ���
length(which(is.na(all$GarageType) & is.na(all$GarageFinish) & is.na(all$GarageCond) & is.na(all$GarageQual)))
# -> 157�� ���Դٴ� ���� 157���� ����ġ ������ �����ϴٴ� �ǹ�

# �׷��ٸ� ������ 2���� ����? 
# �ѹ� Ȯ���غ��� kable
kable(all[!is.na(all$GarageType) & is.na(all$GarageFinish), 
          c('GarageCars', 'GarageArea', 'GarageType', 'GarageCond', 'GarageQual', 'GarageFinish')])

# 2127, 2577 �� �ٸ� ��ä�� Ȯ���� �� �ִµ�,
# 2127�� �������� ������ 2577�� ����. ���� 2127�� ������ 3�� ����(GarageCond, GarageQual, GarageFinish)
# �� �ֺ����� ��ü �ϰڴ�

# �ֺ����� ��ü
all$GarageCond[2127] <- names(sort(-table(all$GarageCond)))[1]
all$GarageQual[2127] <- names(sort(-table(all$GarageQual)))[1]
all$GarageFinish[2127] <- names(sort(-table(all$GarageFinish)))[1]

# ��ü �� �� Ȯ��
kable(all[2127, c('GarageYrBlt', 'GarageCars', 'GarageArea', 'GarageType', 'GarageCond', 'GarageQual', 'GarageFinish')])

# 2577�� GarageCar, GarageArea �� 0���� ��ü �ϰ� ������ �������� NA�� ��ü ����
all$GarageCars[2577] <- 0
all$GarageArea[2577] <- 0
all$GarageType[2577] <- NA

# ������ ������ 4 ������ ����ġ�� ��� 158������ Ȯ���غ��ڴ�.
length(which(is.na(all$GarageType) & is.na(all$GarageFinish) & is.na(all$GarageCond) & is.na(all$GarageQual)))

# -> 4���� ������ ������ Garage�� ������ ������, ��� �¿� 158���� ������ ����ġ�� ����
# -> �̰� 'No garage' �� �ǹ�

# ** GarageType : Garage ��ġ�� ���� Ÿ��
# -> ������ ������ �ƴϱ⿡ factor�� ��ȯ
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

# ** GarageFinish : ���׸��� �Ϸ�
# ������ �����̴�

# Fin  Finished
# RFn  Rough Finished  
# Unf  Unfinished
# NA   No Garage  

all$GarageFinish[is.na(all$GarageFinish)] <- 'None'
Finish <- c('None' = 0, 'Unf' = 1, 'RFn' = 2, 'Fin' = 3)

all$GarageFinish <- as.integer(revalue(all$GarageFinish, Finish))
table(all$GarageFinish)

# ** GarageQual -> GarageQuality
# ���������� ��ȯ

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
# ���������� ��ȯ
# Ex   Excellent
# Gd   Good
# TA   Typical/Average
# Fa   Fair
# Po   Poor
# NA   No Garage

all$GarageCond[is.na(all$GarageCond)] <- 'None'
all$GarageCond <- as.integer(revalue(all$GarageCond, Qualities))
table(all$GarageCond)

## 4.1.8. Basement variables : ���Ͻ� ���� ���� -> ���߰������� �����ؾ� �Ҽ���!
# BsmtQual:     ���Ͻ� ���̸� ���ȭ
# BsmtCond:     ���Ͻ��� �Ϲ����� ���·� �� ���
# BsmtExposure: ���̳� ������ ���� ���յ� ����
# BsmtFinType1: ���Ͻ� �ϰ� ������ ���
# BsmtFinType2: multiple type�� ����� ���Ͻ� �ϰ� ���� ���
# BsmtFullBath: full bathrooms�� ���� ���Ͻ�
# BsmtHalfBath: half bathrooms�� ���� ���Ͻ�
# BsmtFinSF1:   ũ�Ⱑ 1 ��� ������ ���Ͻ�
# BsmtFinSF2:   ũ�Ⱑ 2 ��� ������ ���Ͻ�
# BsmtUnfSF:    ������ ��� ���ͷ� ������ �ȵǴ� ���Ͻ�
# TotalBsmtSF:  ���Ͻ��� �� ��� ����

# ���� 5���� ������ 79 - 82���� ����, 6���� ������ 1~2���� ����ġ�� ����

# 79���� ����ġ ����ġ�� 80�� �̻��� ���� ����ġ�� �������� Ȯ��

length(which(is.na(all$BsmtQual) & is.na(all$BsmtCond) & is.na(all$BsmtExposure) & 
               is.na(all$BsmtFinType1) & is.na(all$BsmtFinType2)))

# --> �������� Ȯ��!

# �߰� ����ġ ã�� 
#  BsmtFinType1�� ����ġ�� �ƴ�����, �ٸ� 4�� �������� ����ġ�� ���
all[!is.na(all$BsmtFinType1) & (is.na(all$BsmtCond) | is.na(all$BsmtQual) | 
                                  is.na(all$BsmtExposure) | is.na(all$BsmtFinType2)), 
    c('BsmtQual', 'BsmtCond', 'BsmtExposure', 'BsmtFinType1', 'BsmtFinType2')]

# 79���� ���Ͻ��� ���� ������ ��������, ������ 9ä�� �߰� �߰� ����ġ�� Ȯ�εȴ�.
# �̶��� �ֺ����� ��ü

all$BsmtFinType2[333] <- names(sort(-table(all$BsmtFinType2)))[1]
all$BsmtExposure[c(949, 1488, 2349)] <- names(sort(-table(all$BsmtExposure)))[1]
all$BsmtQual[c(2218,2219)] <- names(sort(-table(all$BsmtQual)))[1]
all$BsmtCond[c(2041, 2186, 2525)] <- names(sort(-table(all$BsmtCond)))[1]

# 79 ~ 82���� ����ġ�� ���� ������ ���Ͻ��� ���� ������ �Ǹ��ǹǷ�,
# None ���� ��ü ��, hot encoding ����

# ** BsmtQual: ���Ͻ� ���̸� ���ȭ **
# ������ ������ ��ȯ
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

# ** BsmtCond: ���Ͻ��� �Ϲ����� ���·� �� ��� 
# ������ ������ ��ȯ
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

# ** BsmtExposure: ���̳� ������ ���� ���յ� ����
# ������ ������ ��ȯ
# Gd   Good Exposure
# Av   Average Exposure (split levels or foyers typically score average or above)  
# Mn   Mimimum Exposure
# No   No Exposure
# NA   No Basement

all$BsmtExposure[is.na(all$BsmtExposure)] <- 'None'
Exposure <- c('None' = 0, 'No' = 1, 'Mn' = 2, 'Av' = 3, 'Gd' = 4)

all$BsmtExposure <- as.integer(revalue(all$BsmtExposure, Exposure))
table(all$BsmtExposure)

# ** BsmtFinType1: ���Ͻ� �ϰ� ������ ��� **
# ������ ������ ��ȯ
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

# ** BsmtFinType2: multiple type�� ����� ���Ͻ� �ϰ� ���� ���
# ������ ������ Ÿ���̰�, Fintype �����̴�.
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

# ������ 1~6�� ����ġ�� �����ϴ� 6���� ���� ����ġ ó��
# .. �밡�ٴٴٴٴ�..

# ��⿡ �����ߴ� ���Ͻ��� ������ 79ä�� �����Ͽ� ���� ����ġ�� Ȯ���غ���

all[(is.na(all$BsmtFullBath) | is.na(all$BsmtHalfBath) | is.na(all$BsmtFinSF1) | 
       is.na(all$BsmtFinSF2) | is.na(all$BsmtUnfSF) | is.na(all$TotalBsmtSF)), 
    c('BsmtQual', 'BsmtFullBath', 'BsmtHalfBath', 'BsmtFinSF1', 'BsmtFinSF2', 'BsmtUnfSF', 'TotalBsmtSF')]
all$BsmtFullBath[is.na(all$BsmtFullBath)] <- 0
table(all$BsmtFullBath)

# ** BsmtFullBath: full bathrooms�� ���� ���Ͻ�
all$BsmtFullBath[is.na(all$BsmtFullBath)] <- 0
table(all$BsmtFullBath)

# ** BsmtHalfBath: half bathrooms�� ���� ���Ͻ�
all$BsmtHalfBath[is.na(all$BsmtHalfBath)] <- 0
table(all$BsmtHalfBath)

# ** BsmtFinSF1: ũ�Ⱑ 1 ��� ������ ���Ͻ�
all$BsmtFinSF1[is.na(all$BsmtFinSF1)] <- 0

# ** BsmtFinSF2: ũ�Ⱑ 2 ��� ������ ���Ͻ�
all$BsmtFinSF2[is.na(all$BsmtFinSF2)] <- 0

# ** BsmtUnfSF: ������ ��� ���ͷ� ������ �ȵǴ� ����
all$BsmtUnfSF[is.na(all$BsmtUnfSF)] <- 0

# ** TotalBsmtSF: ���Ͻ��� �� ��� ���� 
all$TotalBsmtSF[is.na(all$TotalBsmtSF)] <- 0

## 4.1.9. Masonry variable : ���� 
# Masonry veneer area : ���� area
# Masonry veneer type : ���� type
# �� ���� ���� ������ ���� �ʴ� ���� �̻��ϴ� ( 23��, 24��). Ȯ���غ���

cat("MasVnrArea Na : " , sum(is.na(all$MasVnrArea)), " MasVnrType Na :",  sum(is.na(all$MasVnrType)))
length(which(is.na(all$MasVnrType) & is.na(all$MasVnrArea)))

# ���̳��� 1���� ����ġ�� Ȯ���غ���
all[is.na(all$MasVnrType) & !is.na(all$MasVnrArea), c('MasVnrType', 'MasVnrArea')]

# -> veneer type�� ����ġ�� �ֺ����� ��ü����

all$MasVnrType[2611] <- names(sort(-table(all$MasVnrType)))[2] #�ֺ󵵴� 'none'�̶� �ι�° �󵵷� �ߴ�.
all[2611, c('MasVnrType', 'MasVnrArea')]

# veneer type�� veneer area�� ������ 23�� ����ġ�� ������ ���°� �ǹ��Ѵ�(no masonry)
# ** MasVnrType : Masonry veneer type

all$MasVnrType[is.na(all$MasVnrType)] <- 'None'
all[!is.na(all$SalePrice),] %>% 
  group_by(MasVnrType) %>% 
  dplyr::summarise(median = median(SalePrice), counts=n()) %>%  
  dplyr::arrange(median)

# -> common Brick / None <-> ������ �� level ���� ���̰� �ִ� ������ ���δ�
# ��, ������ ���̳� ������ ���� ������ ������ ������ �����ȴ� -> �޸� �� Ȯ���غ���

Masonry <- c('None' = 0, 'BrkCmn' = 0, 'BrkFace' = 1, 'Stone' = 2)
all$MasVnrType <- as.integer(revalue(all$MasVnrType, Masonry))
table(all$MasVnrType)

Masonry <- c('None' = 0, 'BrkCmn' = 0, 'BrkFace' = 1, 'Stone' = 2)
all$MasVnrType <- as.integer(revalue(all$MasVnrType, Masonry))
table(all$MasVnrType)

# ** MasVnrArea: ��� ���� ���� Masonry veneer area
# ������ �����̹Ƿ�, 0���� ��ü����

all$MasVnrArea[is.na(all$MasVnrArea)] <- 0


# ** 4.1.10 ** MSZoning variables : �뵵�� ����(��������) �ĺ���
# ����ġ�� 4���̰�, ���� ������
# -> �ֺ����� ��ü

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
# Kitchen quality �� ����ġ�� 1��, number of Kitchen �� ����ġ�� ����.
# ** Kitchen quality : �ξ� ������
# ���������� ��ȯ

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

# -> ������ �����̰�, ����ġ�� ����

# ** 4.1.12 : Utilities variables :  ����� �� �ִ� Utilities�� ����
# ����ġ�� 2��
# Ư���ϰԵ�, ������ ���� �� �� ���� �����ϰ� ���� AllPub �̴�.
# testData���� ���� AllPub�� ������ �����Ƿ�, �ش� ������ ���� ���쵵�� �ϰڴ�

table(all$Utilities)
kable(all[is.na(all$Utilities) | all$Utilities == "NoSeWa", 1:9])
all$Utilities <- NULL

# ** 4.1.13 : Home functionality variables : ���� �ִ� ��ɵ�
# ����ġ�� �Ѱ��̰�, ������ Ÿ������ ��ȯ
# Typ�� ���� ����(8),Sal�� ���� ������(0)
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

# ** 4.1.13 : Exterior variables : �ǹ� ���� ����
# �� 4���� ���� ����

# ** Exterior1st : �ǹ� ���� ����
# ����ġ�� 1���̸�, ���� �������̴�. -> �ֺ����� ��ü
 
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

# ** Exterior2nd: �ǹ� ���� ����(2�� �̻��� ���¸� �� ���) **
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

# ** ExterQual: ����� ���� ������ ��� 
# ����ġ�� ����, Qualities ���ͷ� ���������� ��ȯ�Ѵ�.
all$ExterQual <- as.integer(revalue(all$ExterQual, Qualities))

# ** Extercond: ���� ������ ���� ��� 
# ����ġ�� ����, Qualities ���ͷ� ���������� ��ȯ

all$ExterCond <- as.integer(revalue(all$ExterCond, Qualities))
table(all$ExterCond)

# 4.1.14 : Electrical System variables : ���� �ý���
# ����ġ�� 1���̰�, ���� �������̴�.
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

# SaleCondition : �Ǹ� ����

all$SaleCondition <- as.factor(all$SaleCondition)
table(all$SaleCondition)
