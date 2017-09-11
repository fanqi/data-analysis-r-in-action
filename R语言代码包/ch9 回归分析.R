#ch9 回归分析
#9.1.3 一元线性回归
age=18:29
height=c(76.1,77,78.1,78.2,78.8,79.7,79.9,81.1,81.2,81.8,82.8,83.5)
plot(age,height)

lm.reg=lm(formula=height~age)  #可简化为lm(height~age)
summary(lm.reg)  #提取模型计算结果
abline(lsfit(age,height))  #可简化为abline(lm.reg)
lm.res=residuals(lm.reg)
plot(lm.res)

par(mfrow=c(2,2))  #将图像窗口分成四份（两行及两列）
plot(lm.reg)

age=age[-3];height=height[-3]
lm.reg2=lm(formula=height~age)
summary(lm.reg2)

age.pre<-data.frame(age=30)  #predict函数中使用的数据是data.frame或list格式
h.pre<-predict(lm.reg2,age.pre,interval="prediction",level=0.95)
h.pre


#9.2.3 多元回归分析
revenue=read.table("d:/data/revenue.txt",header=T)  #读取数据
lm.reg=lm(y~x1+x2+x3+x4+x5+x6,data=revenue)
summary(lm.reg)  #汇总回归分析结果

lm.reg1=update(lm.reg,.~.-x1-x2)
summary(lm.reg1)


#9.2.4 逐步回归
lm.step=step(lm.reg)
summary(lm.step)


#9.3.1残差诊断

y.res=lm.reg$residual
y.fit=predict(lm.reg)  #计算y的预测值，作为残差图的横坐标
plot(y.res~y.fit,main="残差图")

shapiro.test(y.res)


#9.3.2 影响分析
hii=hatvalues(lm.step)  #计算Leverage
p=4;n=20
hii>2*(p+1)/n #将hii与经验判断标准进行比较

dff=dffits(lm.step)
dff>2*sqrt((p+1)/n)

cook=cooks.distance(lm.step)
cook>4/n

covratio(lm.step)

options(digits=3)  #打印结果显示3位小数
influence.measures(lm.step)


#9.3.3 多重共线性诊断
options(digits=3)
xx=cor(revenue[3:8])  #提取设计矩阵并标准化
eigen(xx)

kappa(xx)

lm.reg=lm(y~x1+x2+x3+x4+x5+x6,data=revenue)
library(DAAG)
vif(lm.reg,digits=3)
attach(revenue)
cor(x2,x3)


#9.4 岭回归
y=c(8.4,9.6,10.4,11.4,12.2,14.2,15.8,17.9,19.6,20.8)
x1=c(82.9,88,99.9,105.3,117.7,131,148.2,161.8,174.2,184.7)
x2=c(92,93,96,94,100,101,105,112,112,112)
x3=c(17.1,21.3,25.1,29,34,40,44,49,51,53)
x4=c(94,96,97,97,100,101,104,109,111,111)
x=cbind(x1,x2,x3,x4)  #将数据按列合并
xx=crossprod(x)  #计算矩阵交叉积，结果为矩阵X’X
kappa(xx,exact=T)   #计算条件数

library(MASS)
plot(lm.ridge(y~x1+x2+x3+x4,lambda=seq(0,0.5,0.001)))
select(lm.ridge(y~x1+x2+x3+x4,lambda=seq(0,0.5,0.001)))
options(digits=3)
lm.ridge(y~x1+x2+x3+x4,lambda=0.0045)


#9.5 广义线性模型
#######泊松回归的参数估计程序#######
#建立数据 
dat=data.frame(y=c(42, 37, 10, 101, 73, 14),n=c(500, 1200, 100, 400, 500, 300),
               type=rep(c('小','中','大'),2),gender=rep(c('男','女'),each=3))
dat$logn=log(dat$n)  #风险暴露数取对数
dat.glm=glm(y~type+gender,offset=logn,data=dat,family=poisson(link=log))#offset风险单位数事先已知
summary(dat.glm)  #glm的输出结果 

dat.pre=predict(dat.glm)
layout(1)  #取消绘图区域分割
plot(y,exp(dat.pre),xlab='观测值',ylab='拟合值',main="索赔次数的拟合效果",pch="*")
abline(0,1)  #添加直线y=x，截距为0，斜率为1

library(MASS)
attach(dat)
dat.glmnb=glm.nb(y~type+gender+offset(logn))  #负二项回归
summary(dat.glmnb)  #输出结果
