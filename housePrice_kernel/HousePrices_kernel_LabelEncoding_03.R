# 5.3 Label encoding/ factorizing the ramaining character variables
# ����ġ�� ���캸������, character type �������� Ȯ������ �ʾҴ�
# �׷��� �̹����� ������ �����ؼ� ���캸���� �ϰڴ�

Charcol <- names(all[,sapply(all, is.character)])
cat('There are', length(Charcol), 'remaining columns with character values')

# 5.3.1 ** Foundation: �ǹ� ����(���)�� ����

# BrkTil          Brick & Tile
# CBlock          Cinder Block
# PConc           Poured Contrete 
# Slab            Slab
# Stone           Stone
# Wood            Wood

# ������ �ƴϱ⿡, factor ������ ��ȯ

all$Foundation <- as.factor(all$Foundation)
table(all$Foundation)

# 5.3.2 ** Heating ������ 1���� ������ ����(Y/N)

# Heating : ���� ��ġ ����

# Floor    Floor Furnace
# GasA Gas forced warm air furnace
# GasW Gas hot water or steam heat
# Grav Gravity furnace 
# OthW Hot water or steam heat other than gas
# Wall Wall furnace

# ������ �ƴϱ⿡, factor ������ ��ȯ
all$Heating <- as.factor(all$Heating)
table(all$Heating)

# ** HeatingQC: ���� ��ġ�� ǰ���� ����
# Ex   Excellent
# Gd   Good
# TA   Average/Typical
# Fa   Fair
# Po   Poor

all$HeatingQC <- as.integer(revalue(all$HeatingQC, Qualities))

# ** CentralAir: �߾� �ù� ������ ����
# N    No
# Y    Yes

all$CentralAir <- as.integer(revalue(all$CentralAir, c('N' = 0, 'Y' = 1)))
table(all$CentralAir)

# 5.3.3 ** RoofStyle: ������ ���� 

# ** RoofStyle: ������ ����
# Flat Flat
# Gable    Gable
# Gambrel  Gabrel (Barn)
# Hip  Hip
# Mansard  Mansard
# Shed Shed

all$RoofStyle <- as.factor(all$RoofStyle)
table(all$RoofStyle)

# ** RoofMatl: ���� ���

# ClyTile  Clay or Tile
# CompShg  Standard (Composite) Shingle
# Membran  Membrane
# Metal    Metal
# Roll Roll
# Tar&Grv  Gravel & Tar
# WdShake  Wood Shakes
# WdShngl  Wood Shingles

# �������� �ƴϱ⿡, factor������ ��ȯ�ϰڴ�.
all$RoofMatl <- as.factor(all$RoofMatl)
table(all$RoofMatl)

# 5.3.4 : Land : ������ ��������, ��Ż���� 2���� ����

# ** LandContour: ������ ��ź��
# Lvl  Near Flat/Level 
# Bnk  Banked - Quick and significant rise from street grade to building
# HLS  Hillside - Significant slope from side to side
# Low  Depression

# �������� �ƴϱ⿡, factor������ ��ȯ�ϰڴ�.
all$LandContour <- as.factor(all$LandContour)
table(all$LandContour)

# ** LandSlope: ������ ���(��Ż)
# Gtl  Gentle slope
# Mod  Moderate Slope  
# Sev  Severe Slope

# ������ Ÿ��, ���������� ��ȯ�ϰڴ�.
all$LandSlope <- as.integer(revalue(all$LandSlope, c('Sev' = 0, 'Mod' = 1, 'Gtl' = 2)))
table(all$LandSlope)

# 5.3.5 : Dwell : �ְſ� ���� 2���� ����
# ** BldgType: �ְ��� ����

# 1Fam     Single-family Detached  
# 2FmCon   Two-family Conversion; originally built as one-family dwelling
# Duplx    Duplex
# TwnhsE   Townhouse End Unit
# TwnhsI   Townhouse Inside Unit

# ������ ���ְ� �ƴϱ⿡, factor�� ��ȯ�Ѵ�.
all$BldgType <- as.factor(all$BldgType)
table(all$BldgType)

# ** HouseStyle: �ְ��� Style 
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

# 5.3.6 : ������ �Ÿ�, �ٹ��� ������ ���� 3���� ����

# ** Neighborhood: ������ ��ġ�� ��Ames�� �ó��̴�.
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

# ** Condition1: �ٹ��� �پ��� ����
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

# ������ ���ְ� �ƴϱ⿡, factor�� ��ȯ�Ѵ�.
all$Condition1 <- as.factor(all$Condition1)
table(all$Condition1)

# ** Condition2: �ٹ��� �پ��� ���� (2�� �̻�)

# Artery   Adjacent to arterial street
# Feedr    Adjacent to feeder street   
# Norm Normal  
# RRNn Within 200' of North-South Railroad
#  RRAn Adjacent to North-South Railroad
#  PosN Near positive off-site feature--park, greenbelt, etc.
#  PosA Adjacent to postive off-site feature
#  RRNe Within 200' of East-West Railroad
# RRAe Adjacent to East-West Railroad

# ������ ���ְ� �ƴϱ⿡, factor�� ��ȯ�Ѵ�.
all$Condition2 <- as.factor(all$Condition2)
table(all$Condition2)

# 5.3.7 Street variables 
# 2���� ���� ����

# ** Street: ������ ���� ���� ����
# Grvl Gravel  
# Pave Paved

# ������ ���ַ�, ���������� ��ȯ�ϰڴ�.
all$Street <- as.integer(revalue(all$Street, c('Grvl' = 0, 'Pave' = 1)))
table(all$Street)

# ** PavedDrive: ���Է��� ����
# Y    Paved 
# P    Partial Pavement
# N    Dirt/Gravel

#������ ���ַ�, ���������� ��ȯ�ϰڴ�.
all$PavedDrive <- as.integer(revalue(all$PavedDrive, c('N' = 0, 'P' = 1, 'Y' = 2)))
table(all$PavedDrive)

# ** 5. numeric variables -> factor variables
# ���� ��ġ�� ������ �Ǿ�������, ���������� ��ȯ�ؾ� �� �������� �����Ѵ�

# 5.4.1 Year and Month Sold
# �Ǹ� �������� YearBuilt(or remodeled)�� ���ִ� ������ ���� ��ġ�� ���� �Ǿ��ִ�.
# 2009�� subprime mortgage�� �������� ������ �ſ� ������ Ȯ�� ����
# �𵨸� ������ YrSold ������ factor�� ��ȯ�� ��������, Age ������ ����� ���� ��ġ���� �ʿ��ϹǷ�, �ϴ� ����
# Month Sold �� ������ ������ ��ȯ

str(all$YrSold)
str(all$MoSold)

all$MoSold <- as.factor(all$MoSold)

# ���󺸴� �������� ������, ���� ������ Ÿ���� 2007�⸻ ���� ����� ��Ÿ��
# �ŸŰ��� 2007���� ���� ���� �������� ���� ���Ŀ� ���� ����
# �������� �� ū ��Ģ�� �־� ����

# �� ���� �� ��
ys <- ggplot(all[!is.na(all$SalePrice),], aes(x = as.factor(YrSold), y = SalePrice)) +
  geom_bar(stat = 'summary', fun.y = 'median', fill = 'blue') +
  scale_y_continuous(breaks = seq(0, 800000, by = 25000), labels = comma) +
  geom_label(stat = 'count', aes(label = ..count.., y = ..count..)) +
  coord_cartesian(ylim = c(0, 200000)) +
  geom_hline(yintercept = 163000, linetype = 'dashed', color = 'red')

# �� ���� �� ��
ms <- ggplot(all[!is.na(all$SalePrice),], aes(x = MoSold, y = SalePrice)) + 
  geom_bar(stat = 'summary', fun.y = 'median', fill = 'blue') +
  scale_y_continuous(breaks = seq(0, 800000, by = 25000), labels = comma) + 
  geom_label(stat = 'count', aes(label = ..count.., y = ..count..)) + 
  coord_cartesian(ylim = c(0, 200000)) +
  geom_hline(yintercept = 163000, linetype = 'dashed', color = 'red') 

grid.arrange(ys, ms, widths = c(1,2))

# MSSubClass 
# ** MSSubClass: �Ǹſ� ������ �ְ� Ÿ��
str(all$MSSubClass)
all$MSSubClass <- as.factor(all$MSSubClass)
all$MSSubClass <- revalue(all$MSSubClass, c('20'='1 story 1946+', '30'='1 story 1945-', '40'='1 story unf attic', '45'='1,5 story unf', '50'='1,5 story fin', '60'='2 story 1946+', '70'='2 story 1945-', '75'='2,5 story all ages', '80'='split/multi level', '85'='split foyer', '90'='duplex all style/age', '120'='1 story PUD 1946+', '150'='1,5 story PUD all', '160'='2 story PUD 1946+', '180'='PUD multilevel', '190'='2 family conversion'))
