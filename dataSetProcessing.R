setwd("E:/R/decisionTree")
# 加载数据集 -------------------------------------------------------------------
library(RODBC)
#odbcDataSources()
channel <- odbcConnect("BI_DW", uid="u_biuser", pwd="L0mRUp2fju")
data<-sqlQuery(channel,"SELECT top 1000 * FROM v_Dim_CRM_buy")
str(data)
#将分类变量因子化
data$isBuy <- as.factor(data$isBuy)

data <- data[,-c(2,3)]

D = 37
formula = isBuy ~ +R + F + M +
  MP + T_hour + T_weekday +
  R_visit + R_rtn + R_cmp
formulaModel = as.formula(formula)


# 数据预处理 -------------------------------------------------------------------
library(sampling)
#计算应抽取测试样本数
a=round(1/4*sum(data[,D]=="0"))
b=round(1/4*sum(data[,D]=="1"))
sub=strata(data,stratanames="isBuy",size=c(a,b),method="srswor")
Train=data[-sub$ID_unit,]
Test=data[sub$ID_unit,]
nrow(Train);nrow(Test)


# 不平衡数据采样处理 -------------------------------------------------------------------
# 双采样
Train_both <- ovun.sample(formula, data = Train, method = "both", p=0.5, N=1000, seed = 1)$data
table(Train_both$isBuy)
# 人工合成数据
library(ROSE)
Train_rose <- ROSE(formula, data = Train, N=1000, seed = 1)$data
table(Train_rose$isBuy)
