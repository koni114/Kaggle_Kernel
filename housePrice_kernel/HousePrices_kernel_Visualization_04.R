# ** House Price Visualization
# 앞서 작업을 통해 모든 문자형 변수를 범주형 factor나, 라벨링하여 숫자형으로 인코딩함
# 더해서 3개의 수치형 변수는 factor로 변환, 1개의 변수는 삭제
# 수치형 변수 -> 56개(종속 변수 포함)
# 범주형 변수 -> 23개

numericVars <- which(sapply(all, is.numeric))
factorVars  <- which(sapply(all, is.factor))
cat('There are', length(numericVars), 'numeric variables, and', length(factorVars), 'categoric variables')


# 6.1 Correlations again
# 상관 계수를 다시한 번 확인해보니 0.5 이상의 변수가 10 -> 16개로 늘었다

all_numVar <- all[, numericVars]
cor_numVar <- cor(all_numVar, use = 'pairwise.complete.obs')

cor_sorted <- as.matrix(sort(cor_numVar[, 'SalePrice'], decreasing = TRUE))
CorHigh    <- names(which(apply(cor_sorted, 1, function(x) abs(x) > 0.5)))
cor_numVar <- cor_numVar[CorHigh, CorHigh]

corrplot.mixed(cor_numVar, 
               tl.col = 'black',  # 변수명 색깔
               tl.pos = 'lt',     # 변수명 왼쪽 표시
               tl.cex = 0.7,      # 변수명 text 크기
               cl.cex = 0.7,      # y축 상관계수 text 크기
               number.cex = .7    # matrix안 상관계수 text 크기
)


# 6.2 Finding variables importance with a quick Random Forest
# 변수의 중요성에 대한 시사를 얻기 위해, 100개의 Tree를 이용,
# RandomForest 모델을 간략하게 사용하기로 함. 
# -> 상대적으로 적은 수의 tree만을 쓰기에 그리 오래 걸리지 않음

set.seed(2018) # 2018 seed 생성

# randomForest 수행 할 때, 결측 있으면 안돌아감
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
       
# 가장 중요한 변수 중, 3개만 범주형이다
# Neighborhood, MSSubClass, GarageType

# 6.2.1 Above Ground Living Area, and other surface related variables(in square feet)
# Above Gorund Living Area 와 SalePrice 변수간의 연관성은 이미 확인하였지만,
# 한번 더 나타내겠다. 또한 Square feet 연관 변수중 측정된 상위 20개의 값으로 번들 생성
# ‘Total Rooms Above Ground(TotRmsAbvGrd)’ 변수 또한 Above Ground Living Area와 높은 상관 관계(0.81)를 보이기에 더해서 나타낸다.

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

layout <- matrix(c(1,2,5,3,4,8,6,7),4,2, byrow = TRUE) # 4행 2열 ()안의 순으로 행부터 matrix 생성
multiplot(s1, s2, s3, s4, s5, s6, s7, s8, layout = layout)

