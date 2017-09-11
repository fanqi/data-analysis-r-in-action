#ch15 R在数据预测中的应用
#【案例1】人口死亡率拟合
#1)数据的处理和描述
setwd("d:/data")  #设置文件读取的路径
mortality=read.csv('swedish mortality.csv',header=T)
mortality=na.omit(mortality)
mortality=mortality[mortality$q_male>0,]
mortality=mortality[mortality$q_male<=1,]
attach(mortality)

library(rgl)
plot3d(Year,Age,q_male,col='grey',type='p',zlim=1)
#二维散点图
par(mfrow=c(1,2))
plot(Age,log(q_male),main='年龄与死亡率（对数）')
plot(Year,log(q_male),main='年份与死亡率（对数）')
layout(1)  #取消图形区域拆分
plot(Age,L_male_exp,main='年龄与对数生存人数')


#2)拟合研究对象的分布类型
hist(Male_death,freq=F,breaks=100)
lg.md=log(Male_death)
#将数据分为两部分
data1=lg.md[lg.md<6];data2=lg.md[lg.md>6]
p1=length(data1)/(length(data1)+length(data2))  #第一个正态分布的比重
library(MASS)
para1=fitdistr(data1,'normal')$estimate  #拟合第一个正态分布的参数
para2=fitdistr(data2,'normal')$estimate  #拟合第二个正态分布
#计算样本的双峰混合分布拟合值
p=p1*dnorm(lg.md,para1[1],para1[2])+(1-p1)*dnorm(lg.md,para2[1],para2[2])

hist(log(Male_death),freq=F,breaks=100,main='经验值和拟合值',ylim=c(0,0.33))
points(lg.md,p)  #描出拟合值的点

#计算双峰分布的累积分布函数
F.mix=p1*pnorm(lg.md,para1[1],para1[2])+(1-p1)*pnorm(lg.md,para2[1],para2[2])
ks.test(lg.md,F.mix)


#3)普通线性回归
ols=lm(Male_death~Age+Year+L_male_exp)
summary(ols)


#4)广义线性模型
m.nb=glm.nb(Male_death~factor(Age)+factor(Year)+offset(L_male_exp))
summary(m.nb)
anova(m.nb,test='Chisq')
par(mfrow=c(2,2))
plot(m.nb)

location=c(1,102,307,5414,5626)
mortality=mortality[-location,]
attach(mortality)


#5)模型改进
model.final <- glm.nb(Male_death~poly(Age,25)+poly(Year,4)+offset(L_male_exp))
options(digits=2)
summary(model.final)


#6)模型的拟合结果
pre.final=predict(model.final)  #计算拟合值
layout(1)  #取消图形区域拆分
plot(Male_death,exp(pre.final),xlab='观测值',ylab='拟合值',main='正交多项式改进模型')
abline(0,1,col='red')  #画y=x直线

q_pre=exp(pre.final)/Male_Exp  #死亡率的拟合值
plot(Age,log(q_male),pch='.',main='对数死亡率')
points(Age,log(q_pre),pch='.',col=2)
legend(70,-7,legend=c('观测值','拟合值'),lty=1,col=c(1,2))



#【案例2】CPI的向量自回归模型
data=read.table("d:/data/cpi_data.txt",header=T)
dat=ts(data,start=c(2005,1),frequency=12)  #根据原始数据构造时间序列
#由于是月度数据，因此设置frequency为12，以2005年1月为序列起点
dat=log(dat)  #取数据的对数值
plot.ts(dat,main='time series of cpi')  #绘制时间序列图

dat_dif=diff(dat)  #得到一阶差分序列
plot.ts(dat_dif^2)

library(tseries)
cpi=ts(data$CPI,frequency=12, start=c(2005,1))  #构造CPI的单变量时间序列
cpi_dif=diff(log(cpi))  #对cpi取对数后，再进行一阶差分
pp.test(cpi_dif)

install.packages("vars")
library(vars)
options(digits=2)  #计算结果至少显示2位小数
result=VAR(dat_dif,p=2)
plot(result)
result$varresult$CPI  #varresult给出模型中各参数的估计结果，用”$”提取CPI的部分
summary(result)$varresult$CPI  #给出模型中各变量的显著性

dat_mod=dat_dif[,c(-2,-4)]  #剔除M2和PPI分别对应的第二列和第四列
result2=VAR(dat_mod,p=2)  #对提出变量后的数据建立VAR模型
summary(result2)$varresult$CPI

plot(result2)
pre=predict(result2,n.ahead=12)
CPI.pre=pre$fcst$CPI  #提出对CPI预测的结果
CPI.pre

options(digits=5)  #修改小数显示格式
LCPI.pre=log(cpi[119])+cumsum(CPI.pre[,1])  #计算CPI对数的预测值
CPI.pre=exp(LCPI.pre)  #计算CPI预测值
CPI.pre
#plot(CPI.pre)