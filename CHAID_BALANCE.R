setwd("E:/R/decisionTree")
# 加载并处理数据集 -------------------------------------------------------------------
library(RODBC)
channel <-
  odbcConnect("BI_DW", uid = "u_biuser", pwd = "L0mRUp2fju")
#训练数据集
data <-
  sqlQuery(channel, "SELECT top 3000 * FROM v_Dim_CRM_buy_201611")
dim(data)
names(data)

#将分类变量因子化
for (i in 4:19) {
  data[, i] = factor(data[, i])
}
data$regTime <- as.Date(data$regTime, origin = "1900-01-01")
data$birthday <- as.Date(data$birthday, origin = "1900-01-01")
data$isBuy <- as.factor(data$isBuy)

##原始概率(买，不买=1-买)
probBefore = table(data$isBuy)[2] / sum(table(data$isBuy))
probBefore

#模型规则
formula = isBuy ~  city_tier + R + F + MP + K +
  T_hour + T_weekday + TD + R_rtn + F_search
formulaModel = as.formula(formula)

#平衡训练数据集
library(ROSE)
data <- ROSE(formula,
             data = data,
             N = dim(data)[1] * 0.8,
             seed = 1)$data
table(data$isBuy)

#预测数据集
dataPred <-
  sqlQuery(channel, "SELECT * FROM v_Dim_CRM_buy_pred_201611")
dim(dataPred)
names(dataPred)
close(channel)

##将分类变量因子化
for (i in 4:19) {
  dataPred[, i] = factor(dataPred[, i])
}
dataPred$regTime <- as.Date(dataPred$regTime)
dataPred$birthday <- as.Date(dataPred$birthday)


# 构建分类树CHAID(用于可视化展示，需数据还原) -------------------------------------------------------------------
library(CHAID)
set.seed(290875)
ctrl <- chaid_control(minsplit = 1 ,
                      minprob = 0.2,
                      maxheight = 4)
chaidUS <- chaid(formulaModel, data = data, control = ctrl)
print(chaidUS)
plot(chaidUS)

##平衡后概率(买，不买=1-买)
probAfter = table(data$isBuy)[2] / sum(table(data$isBuy))


##预测chaid树模型
# chaidPred <- predict(chaidUS, dataPred , type = "prob")
# chaidPrednode <- predict(chaidUS, dataPred , type = "node")
# head(chaidPred)
# notbuy = chaidPred[, 1]
