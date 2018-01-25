getwd()
setwd("C:/Users/Administrator/Documents/学习R")
#导入csv文件数据
data <- read.csv("朝阳医院2016年销售数据.csv")
# 一、数据预处理
# 数据预处理主要有以下步骤：
# 1.列名重命名
# 2.删除缺失数据
# 3.处理日期
# 4.数据类型转换
# 5.数据排序
names(data) <- c("time","cardNo","drugId","drugName","saleNumber","virtualMoney","actualMoney")
data <- na.omit(data)
data
install.packages("stringr")
library(stringr)
timesplit <- str_split_fixed(data$time," ",n=2)
data$time <- timesplit[,1]
data
#数据类型转换
class(data$time)
data$time <- as.Date(data$time,"%Y-%m-%d")
class(data$time)
data$saleNumber <- as.numeric(data$saleNumber)
#数据排序
data <- data[order(data$time),]
data
# 分析业务指标
# 原始数据经过预处理之后就可以根据业务需要进行业务指标分析了。
# 月均消费次数=总消费次数/月份数
index_kpI <- !duplicated(data[,c("time","cardNo")])
KpI <- data[index_kpI,]
consume <- nrow(KpI)#总消费次数
startTime <- KpI$time[1]
endTime <- KpI$time[nrow(KpI)]
months <- as.numeric((endTime-startTime)) %/% 30 #消费月数
avg_consume <- consume %/% months#月消费次数
#月均消费金额=总消费金额/月份数
total_money <- sum(data$virtualMoney,na.rm = TRUE)
avg_money <- total_money / months
# 课单价
# 客单价=总消费金额/总消费次数
pct <-  total_money / consume
#按每组分组，计算每周的实际消费金额
week_consume <- tapply(data$actualMoney,format(data$time,"%Y-%U"),sum)

#将array数组转化为二维表
week_consume <- as.data.frame.table(week_consume)

names(week_consume) <- c("TIME","Consume")

week_consume$TIME <- as.character(week_consume$TIME)
#绘制每周消费金额曲线
library(ggplot2)
ggplot(week_consume,aes(TIME,Consume))+geom_point()+geom_line()#点线图
week_consume$timeNumber <- c(1:nrow(week_consume))
plot(week_consume$timeNumber,week_consume$Consume,
     xlab = "时间（年份-第几周）",    #x轴标签
     ylab = "消费金额",               #y轴标签
     xaxt = "n",                      #禁用X轴
     main = "2016年朝阳医院消费曲线", #标题
     col = "blue",                   #绘图颜色绿色        
     type = "b")
axis(1,at = week_consume$timeNumber,labels = week_consume$TIME) #绘制坐标抽











