getwd()
setwd("C:/Users/Administrator/Documents/ѧϰR")
#����csv�ļ�����
data <- read.csv("����ҽԺ2016����������.csv")
# һ������Ԥ����
# ����Ԥ������Ҫ�����²��裺
# 1.����������
# 2.ɾ��ȱʧ����
# 3.��������
# 4.��������ת��
# 5.��������
names(data) <- c("time","cardNo","drugId","drugName","saleNumber","virtualMoney","actualMoney")
data <- na.omit(data)
data
install.packages("stringr")
library(stringr)
timesplit <- str_split_fixed(data$time," ",n=2)
data$time <- timesplit[,1]
data
#��������ת��
class(data$time)
data$time <- as.Date(data$time,"%Y-%m-%d")
class(data$time)
data$saleNumber <- as.numeric(data$saleNumber)
#��������
data <- data[order(data$time),]
data
# ����ҵ��ָ��
# ԭʼ���ݾ���Ԥ����֮��Ϳ��Ը���ҵ����Ҫ����ҵ��ָ������ˡ�
# �¾����Ѵ���=�����Ѵ���/�·���
index_kpI <- !duplicated(data[,c("time","cardNo")])
KpI <- data[index_kpI,]
consume <- nrow(KpI)#�����Ѵ���
startTime <- KpI$time[1]
endTime <- KpI$time[nrow(KpI)]
months <- as.numeric((endTime-startTime)) %/% 30 #��������
avg_consume <- consume %/% months#�����Ѵ���
#�¾����ѽ��=�����ѽ��/�·���
total_money <- sum(data$virtualMoney,na.rm = TRUE)
avg_money <- total_money / months
# �ε���
# �͵���=�����ѽ��/�����Ѵ���
pct <-  total_money / consume
#��ÿ����飬����ÿ�ܵ�ʵ�����ѽ��
week_consume <- tapply(data$actualMoney,format(data$time,"%Y-%U"),sum)

#��array����ת��Ϊ��ά��
week_consume <- as.data.frame.table(week_consume)

names(week_consume) <- c("TIME","Consume")

week_consume$TIME <- as.character(week_consume$TIME)
#����ÿ�����ѽ������
library(ggplot2)
ggplot(week_consume,aes(TIME,Consume))+geom_point()+geom_line()#����ͼ
week_consume$timeNumber <- c(1:nrow(week_consume))
plot(week_consume$timeNumber,week_consume$Consume,
     xlab = "ʱ�䣨���-�ڼ��ܣ�",    #x���ǩ
     ylab = "���ѽ��",               #y���ǩ
     xaxt = "n",                      #����X��
     main = "2016�곯��ҽԺ��������", #����
     col = "blue",                   #��ͼ��ɫ��ɫ        
     type = "b")
axis(1,at = week_consume$timeNumber,labels = week_consume$TIME) #���������










