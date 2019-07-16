# HousePrice Kernel study
# 2019_05_31. ���������� ���θ� �տ� ���� ����.

# Introduction
# Kaggle�� House price prediction ��ȸ
# Lasso, XGBoost�� �ӻ�� �ؼ� ����. �������� EDA�� Ư¡
# �̹� kernel�� ���ؼ��� EDA�� ������� �����ؾ� �ϴ��� �������

#' House Prices: Advanced Regression Techniques competition ����
#' �����͸� : Boston Housing Price

# �켱 �ʿ��� ���̺귯���� �ε�����

library(knitr) # R �ڵ� ����ȭ�ϴ� ��Ű��
library(ggplot2)
library(plyr)
library(dplyr)
library(corrplot)
library(caret)
library(gridExtra)
library(scales)
library(Rmisc)    # �м� �� ��ƿ��Ƽ �Լ��� ���ԵǾ� �ִ� ��Ű��
library(ggrepel)  # ggrepel provides geoms for ggplot2 to repel overlapping text labels
library(randomForest) 
library(psych)
library(xgboost)

# train, test dataset loading
train <- read.csv('house_train.csv', stringsAsFactors = F)
test <- read.csv('house_test.csv', stringsAsFactors = F)

#' train data�� character, integer ������ �����Ǿ� �ִ�
#' �밳 Character ������ ���������� factor ����������, ��κ� cleansing / feature engineering�� �ʿ��ϹǷ�,
#' character ������ R�� �ҷ�����
#' ������ 81�� ����, 1460���� ����ġ�� ������, ���� ������ SalePrice �����̴�
#' ������ Ȯ���غ���

str(train)
dim(train) # train,test data �� row ���� : 1459, 1460 ���� ���� 1:1
dim(test)

test_labels <- test$Id  # �� submission�� ���� test�� id�� ���ͷ� ����.
test$Id <- NULL         # ������ train, test�� �ִ� Id�� ���� 
train$Id <- NULL

# train, test data rbind
test$SalePrice <- NA
all <- rbind(train, test)


# �߿� ���� Ž���غ���
# 79���� ���� ������ �ִµ�, ���� ���� ���� �����ϱ�...? 
# �׷��� �ϴ� ���� �߿��� ������ SalePrice -> ���� �����̴� �ѹ� Ȯ���غ���
# ���������� �ſ� ���� ������ ������ �����Ƿ�, ó������ � ���������� Ȯ���ϴ� �ͺ���,
# ��� ��� �� slicing �� ���� ������ �����ϴ� �͵� ���� ����̶�� ������ ���~!

# 1. SalePrice : histogram
# -> test data�� SalePrice�� NAó�� �߰�, train data�� ������ ����
# -> x���� scale ó��

ggplot(data = all[!is.na(all$SalePrice), ], aes(x = SalePrice)) +
  geom_histogram(fill = 'blue', binwidth =10000) +
  scale_x_continuous(breaks = seq(0, 800000, by = 100000), labels = comma)

# �������� ġ��ģ ���� -> ���ó� �� ���� ���ٴ� �ǹ� 
# ġ��ģ ������ ���ؼ��� ���� �����Ҽ��� ����. 

# 2. SalePrice ��� �� Ȯ��
summary(train$SalePrice)

# ��� 18���޷�. �� 2������ ����
# �������� 16���޷���, ������� ���̰� ���� -> ���Ժ����� ������� ��߳��ٰ� ������ �� ����

## ** 3. SalePrice�� ��� ����� ���� numeric ���� �����ϱ�
# 3.1. �켱, ��ġ�� ���� �� SalePrice�� ��� ����� ���� �������� ����

numericVars <- which(sapply(all, is.numeric))
numericVarNames <- all %>% select_if(is.numeric) %>% colnames
cat('There are', length(numericVarNames), 'numeric variables')

# 37���� ��ġ�� ���� ���� ����
all_numVar <- all[, numericVars]
cor_numVar <- cor(all_numVar, use = "pairwise.complete.obs") # �� numeric ������ ��� ���
cor_sorted <- as.matrix(sort(cor_numVar[, 'SalePrice'], decreasing = TRUE))

# 3.2. �������� ū �������� �����غ���
CorHigh    <- names(which(apply(cor_sorted, 1, function(x) abs(x) > 0.5)))
cor_numVar <- cor_numVar[CorHigh, CorHigh]

corrplot.mixed(cor_numVar,
               tl.col = 'black', # ������ ����
               tl.pos = 'lt',    # ������ ���� ǥ�� ** -> �̰� ����
               number.cex = .7)  # matrix�� ������ text ũ��

# corrplot���� Ȯ���� �� �ֵ���, ���� ������谡 ���� �� ����
# OverallQual, GrLivArea�� �ð�ȭ �غ���.
# ����, ���߰����� ������ ���ÿ� �ذ��غ���(�������, GarageArea�� GarageCar�� ������谡 �ſ� ����)

# OverallQual : �� ��ü���� quality
# GrLivArea   : ���Ͻ� ������ ������ ����

# 3.3. Overall Quality
ggplot(data = all[!is.na(all$SalePrice),], aes(x = factor(OverallQual), y= SalePrice)) +
  geom_boxplot(col = 'blue') + labs(x = 'Overall Quality') + 
  scale_y_continuous(breaks = seq(0, 800000, by = 100000), labels = comma)

# SalePrice�� Overall Quality ���� ���� ��� ���踦 ���̸�,
# 4 ����� ��� �� ���� ���� �̻�ġ�� Ȯ���� �ִ�.
# �������� Ư�Ⳮ ���� ������ �ʴ´�

# 3.4 GrLivArea(Above Grade Ground Living Area (square feet))
# ���� ����ġ�� ������, �翬�ϰ� ū���� �Ϲ������� ��δ�

# ggplot : ���� y = x ���� �׷����� �ش� �������� ���� ����

ggplot(data = all[!is.na(all$SalePrice),], aes(x = GrLivArea, y = SalePrice)) +
  geom_point(col = 'blue') +
  geom_smooth(method = 'lm', se = FALSE, color = 'black', aes(group = 1)) +
  scale_y_continuous(breaks = seq(0, 800000, by = 100000), labels = comma) +
  geom_text_repel(aes(label = ifelse(all$GrLivArea[!is.na(all$SalePrice)] > 4500, rownames(all), '')))

# -> ���� ��� ������ ���� �� ���� �̻�ġ ���� ���� ����(524, 1299) : �׳� ���� ���� �ɻ��ΰ�? 
#    �̷����� �ٸ� ������ ������ üũ�ϸ鼭 �̻�ġ�� �´���, �ƴϸ� �׷����� ������ �ִ����� �ľ��ϴ� �͵� ����

all[c(524, 1299), c('SalePrice', 'GrLivArea', 'OverallQual')]  

# -> Ȯ�� ��� �ְ� ����� ������ ��Ÿ���� �ִ�. �׷��� Ư�� ������ ���� ������ ���� å���Ǿ��� ���� �����Ƿ�
#    ��︸ �ϰ� �Ѿ��



