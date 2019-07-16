# ** House Price Visualization
# �ռ� �۾��� ���� ��� ������ ������ ������ factor��, �󺧸��Ͽ� ���������� ���ڵ���
# ���ؼ� 3���� ��ġ�� ������ factor�� ��ȯ, 1���� ������ ����
# ��ġ�� ���� -> 56��(���� ���� ����)
# ������ ���� -> 23��

numericVars <- which(sapply(all, is.numeric))
factorVars  <- which(sapply(all, is.factor))
cat('There are', length(numericVars), 'numeric variables, and', length(factorVars), 'categoric variables')


# 6.1 Correlations again
# ��� ����� �ٽ��� �� Ȯ���غ��� 0.5 �̻��� ������ 10 -> 16���� �þ���

all_numVar <- all[, numericVars]
cor_numVar <- cor(all_numVar, use = 'pairwise.complete.obs')

cor_sorted <- as.matrix(sort(cor_numVar[, 'SalePrice'], decreasing = TRUE))
CorHigh    <- names(which(apply(cor_sorted, 1, function(x) abs(x) > 0.5)))
cor_numVar <- cor_numVar[CorHigh, CorHigh]

corrplot.mixed(cor_numVar, 
               tl.col = 'black',  # ������ ����
               tl.pos = 'lt',     # ������ ���� ǥ��
               tl.cex = 0.7,      # ������ text ũ��
               cl.cex = 0.7,      # y�� ������ text ũ��
               number.cex = .7    # matrix�� ������ text ũ��
)


# 6.2 Finding variables importance with a quick Random Forest
# ������ �߿伺�� ���� �û縦 ��� ����, 100���� Tree�� �̿�,
# RandomForest ���� �����ϰ� ����ϱ�� ��. 
# -> ��������� ���� ���� tree���� ���⿡ �׸� ���� �ɸ��� ����

set.seed(2018) # 2018 seed ����

# randomForest ���� �� ��, ���� ������ �ȵ��ư�
quick_RF <- randomForest(x = all[1:1460, -79]
                         , y = all$SalePrice[1:1460]
                         , ntree = 100
                         , importance = TRUE)
imp_RF <- importance(quick_RF)
imp_DF <- data.frame(Variables = row.names(imp_RF), MSE = imp_RF[,1])
imp_DF <- imp_DF[order(imp_DF$MSE, decreasing = TRUE),]

ggplot(imp_DF[1:20, ],
       aes(x = reorder(Variables, MSE), y = MSE, fill = MSE)) +
       geom_bar(stat = 'identity') +
       labs(x = 'Variables', y = '% increase MSE if variables is randomly permuted') +
  coord_flip() +
  theme(legend.position = 'none')
       
# ���� �߿��� ���� ��, 3���� �������̴�
# Neighborhood, MSSubClass, GarageType

# 6.2.1 Above Ground Living Area, and other surface related variables(in square feet)
# Above Gorund Living Area �� SalePrice �������� �������� �̹� Ȯ���Ͽ�����,
# �ѹ� �� ��Ÿ���ڴ�. ���� Square feet ���� ������ ������ ���� 20���� ������ ���� ����
# ��Total Rooms Above Ground(TotRmsAbvGrd)�� ���� ���� Above Ground Living Area�� ���� ��� ����(0.81)�� ���̱⿡ ���ؼ� ��Ÿ����.

s1 <- ggplot(data = all, aes(x = GrLivArea)) +
  geom_density() + labs(x = 'Square feet living area')
s2 <- ggplot(data = all, aes(x = as.factor(TotRmsAbvGrd))) +
  geom_histogram(stat = 'count') + labs(x = 'Rooms above Ground')
s3 <- ggplot(all, aes(x = X1stFlrSF)) +
  geom_density() + labs(x = 'Square feet first floor')
s4 <- ggplot(all, aes(x = X2ndFlrSF)) + 
  geom_density() + labs(x = 'Square feet second floor')
s5 <- ggplot(all, aes(x = TotalBsmtSF)) +
  geom_density()+ labs(x = 'Square feet basement')
s6 <- ggplot(all[all$LotArea < 100000,], aes(x = LotArea)) + 
  geom_density() + labs(x = 'Square feet lot')
s7 <- ggplot(all, aes(x = LotFrontage)) +
  geom_density() + labs(x = 'Linear feet lot frontage')
s8 <- ggplot(all, aes(x = LowQualFinSF)) +
  geom_histogram() + labs(x = 'Low quality square feet 1st & 2nd')

layout <- matrix(c(1,2,5,3,4,8,6,7),4,2, byrow = TRUE) # 4�� 2�� ()���� ������ ����� matrix ����
multiplot(s1, s2, s3, s4, s5, s6, s7, s8, layout = layout)
