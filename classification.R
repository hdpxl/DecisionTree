setwd("E:/R/decisionTree")
# 加载数据集 -------------------------------------------------------------------
library(RODBC)
#odbcDataSources()
channel <-
  odbcConnect("BI_DW", uid = "u_biuser", pwd = "L0mRUp2fju")
data <-
  sqlQuery(channel, "SELECT top 5000 * FROM v_Dim_CRM_buy_201611")
dim(data)
names(data)
#将分类变量因子化
for (i in 4:19) {
  data[, i] <- factor(data[, i])
}
data$isBuy <- as.factor(data$isBuy)
data$regTime <- unclass(as.Date(data$regTime))
data$birthday <- unclass(as.Date(data$birthday))
#str(data)
#names(data)
formula = isBuy ~  city_tier + R + F + MP + K +
  T_hour + T_weekday + TD + R_rtn + F_search
formulaModel = as.formula(formula)


# 数据预处理 -------------------------------------------------------------------
# 分层抽样
D=33
library(sampling)
a = round(1 / 4 * sum(data[, D] == "0"))
b = round(1 / 4 * sum(data[, D] == "1"))
sub = strata(data,
             stratanames = "isBuy",
             size = c(a, b),
             method = "srswor")
Train = data[-sub$ID_unit,]
Test = data[sub$ID_unit,]
nrow(Train)
nrow(Test)
# 采样：人工合成数据
library(ROSE)
Train <- ROSE(formula,
              data = Train,
              N = dim(Train)[1]*0.8,
              seed = 1)$data
table(Train$isBuy)


# 构建分类树rpart -------------------------------------------------------------------
library(rpart)
library(rpart.plot)
rpResult = list(0)
rpartFunc = function() {
  rpModel <- rpart(formulaModel, data = Train, method = "class")
  # rpModel
  # rpart.plot(rpModel,type = 1,extra = 4)
  
  rpPred <- predict(rpModel, Test, type = "class")
  tab <- table(rpPred, Test[, D])
  rpResult = list(
    M = table(rpPred, Test[, D]),
    A = (tab[1, 1] + tab[2, 2]) / sum(tab),
    P = tab[2, 2] / sum(tab[, 2]),
    R = tab[2, 2] / sum(tab[2,]),
    MSE = mean((as.numeric(rpPred) - as.numeric(Test[, D])) ^ 2)
  )
}

# 构建分类树CHAID -------------------------------------------------------------------
library(CHAID)
chResult = list(0)
chaidFunc = function() {
  chModel <- chaid(formulaModel, data = Train)
}

# 构建分类boosting -------------------------------------------------------------------
library(adabag)
bsResult = list(0)
boostingFunc = function() {
  bsModel <- boosting(formulaModel, Train)
  # 输出模型
  # 画图
  #barplot(bsModel$importance,cex.names = .6)#变量重要性
  
  bsPred <- predict(bsModel, Test)$class
  tab <- table(bsPred, Test[, D])
  bsResult = list(
    M = table(bsPred, Test[, D]),
    A = (tab[1, 1] + tab[2, 2]) / sum(tab),
    P = tab[2, 2] / sum(tab[, 2]),
    R = tab[2, 2] / sum(tab[2,]),
    MSE = mean((as.numeric(bsPred) - as.numeric(Test[, D])) ^ 2)
  )
}

# 构建分类bagging -------------------------------------------------------------------
library(adabag)
bgResult = list(0)
baggingFunc = function() {
  bgModel <- bagging(formulaModel, Train)
  # 输出模型
  # 画图
  #barplot(bgModel$importance,cex.names = .6)#变量重要性
  
  bgPred <- predict(bgModel, Test)$class
  tab <- table(bgPred, Test[, D])
  bgResult = list(
    M = table(bgPred, Test[, D]),
    A = (tab[1, 1] + tab[2, 2]) / sum(tab),
    P = tab[2, 2] / sum(tab[, 2]),
    R = tab[2, 2] / sum(tab[2,]),
    MSE = mean((as.numeric(bgPred) - as.numeric(Test[, D])) ^ 2)
  )
}

# 构建分类randomForest -------------------------------------------------------------------
library(randomForest)
rfResult = list(0)
randomForestFunc = function() {
  rfModel = randomForest(
    formulaModel,
    data = Train,
    importance = TRUE,
    proximity = TRUE
  )
  #rfModel
  #plot(rfModel)
  #importance(rfModel) #重要性评分及Gini指数
  #varImpPlot(rfModel)
  # par(mfrow=c(3,1))
  # matplot(importance(rfModel)[,1:2],type = "o")
  # barplot(importance(rfModel)[,3],cex.names = 0.6)#Accuracy
  # barplot(importance(rfModel)[,4],cex.names = 0.6)#Gini
  
  rfPred <- predict(rfModel, Test)
  tab <- table(rfPred, Test[, D])
  rfResult = list(
    M = table(rfPred, Test[, D]),
    A = (tab[1, 1] + tab[2, 2]) / sum(tab),
    P = tab[2, 2] / sum(tab[, 2]),
    R = tab[2, 2] / sum(tab[2,]),
    MSE = mean((as.numeric(rfPred) - as.numeric(Test[, D])) ^ 2)
  )
}

# 构建分类树SVM -------------------------------------------------------------------
library(e1071)
svmResult = list(0)
svmFunc = function() {
  svmModel <- svm(formulaModel, data = Train, kernal="sigmoid")
  # svmModel
  svmPred <- predict(svmModel, Test)
  tab <- table(svmPred, Test[, D])
  svmResult = list(
    M = table(svmPred, Test[, D]),
    A = (tab[1, 1] + tab[2, 2]) / sum(tab),
    P = tab[2, 2] / sum(tab[, 2]),
    R = tab[2, 2] / sum(tab[2,]),
    MSE = mean((as.numeric(svmPred) - as.numeric(Test[, D])) ^ 2)
  )
}
svmResult$M;svmResult$A;svmResult$P;svmResult$R;svmResult$MSE

# 构建分类树KSVM -------------------------------------------------------------------
library(kernlab)
ksvmResult = list(0)
ksvmFunc = function() {
  ksvmModel <- ksvm(formulaModel, data = Train)
  # ksvmModel
  ksvmPred <- predict(ksvmModel, Test)
  tab <- table(ksvmPred, Test[, D])
  ksvmResult = list(
    M = table(ksvmPred, Test[, D]),
    A = (tab[1, 1] + tab[2, 2]) / sum(tab),
    P = tab[2, 2] / sum(tab[, 2]),
    R = tab[2, 2] / sum(tab[2,]),
    MSE = mean((as.numeric(ksvmPred) - as.numeric(Test[, D])) ^ 2)
  )
}
ksvmResult$M;ksvmResult$A;ksvmResult$P;ksvmResult$R;ksvmResult$MSE


# 执行分类器 -------------------------------------------------------------------
startRP <- Sys.time()
resultRP <- rpartFunc()
endRP <- Sys.time()
print(endRP - startRP)
resultSVM <- svmFunc()
endSVM <- Sys.time()
print(endSVM - endRP)
resultKSVM = ksvmFunc()
endKSVM <- Sys.time()
print(endKSVM - endSVM)
resultRF = randomForestFunc()
endRF <- Sys.time()
print(endRF - endKSVM)

# 比较分类器 -------------------------------------------------------------------
# 选取最大值
c <- c(resultRP$P, resultSVM$P, resultKSVM$P, resultRF$P)
selectMaxFunc = function(c) {
  temp <- c[1]
  t = 1
  for (i in 1:length(c)) {
    if (temp >= c[i])
      temp
    else{
      t = i
      temp <- c[i]
    }
  }
  print(t)
  print(temp)
}
compareFunc(c)
# 选取最小值
c <- c(resultRP$MSE, resultSVM$MSE, resultKSVM$MSE, resultRF$MSE)
selectMinFunc = function(c) {
  temp <- c[1]
  t = 1
  for (i in 1:length(c)) {
    if (temp <= c[i])
      temp
    else{
      t = i
      temp <- c[i]
    }
  }
  print(t)
  print(temp)
}
compareFunc(c)
