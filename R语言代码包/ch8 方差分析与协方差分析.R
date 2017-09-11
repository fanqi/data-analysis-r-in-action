#8.1.1 单因子方差分析
#正态性检验
x1=c(103,101,98,110,105,100,106)
x2=c(113,107,108,116,114,110,115)
x3=c(82,92,84,86,84,90,88)
shapiro.test(x1)
shapiro.test(x2)
shapiro.test(x3)
#方差齐性检验
x=c(x1,x2,x3)
account=data.frame(x,A=factor(rep(1:3,each=7)))
bartlett.test(x~A,data=account)


#8.1.2 单因素方差分析
a.aov=aov(x~A,data=account)
summary(a.aov)
plot(account$x~account$A)

anova(lm(x~A,data=account))
oneway.test(x~A,data=account,var.equal=TRUE)

library(car)
levene.test(account$x,account$A)


#8.1.3 多重t检验
p.adjust.methods
attach(account)
pairwise.t.test(x,A,p.adjust.method="bonferroni")

x=c(20,12,20,10,14,22,10,20,12,6,24,14,18,18,10,16,4,8,6,18,26,22,16,20,10)
sales=data.frame(x,A=gl(5,5),B=gl(5,1,25))
sales$B
bartlett.test(x~A,data=sales)
bartlett.test(x~B,data=sales)
sales.aov=aov(x~A+B,data=sales)
summary(sales.aov)

time=c(25,24,27,25,25,19,20,23,22,21,29,28,31,28,30,20,17,22,21,17,18,17,13,16,12,22,18,24,21,22)
traffic=data.frame(time,A=gl(2,15,30),B=gl(3,5,30,labels=c("I","II","III")))
bartlett.test(time~A,data=traffic)
bartlett.test(time~B,data=traffic)
op=par(mfrow=c(1,2))  #分割图形区域
plot(time~A+B,data=traffic)
attach(traffic)
interaction.plot(A,B,time,legend=F)
interaction.plot(B,A,time,legend=F)
traf.aov=aov(time~A*B,data=traffic)
summary(traf.aov)


Weight_Initial=c(15,13,11,12,12,16,14,17,17,16,18,18,21,22,19,18,22,24,20,23,25,27,30,32)
Weight_Increment=c(85,83,65,76,80,91,84,90,97,90,100,95,103,106,99,94,89,91,83,95,100,102,105,110)
feed=gl(3,8,24)
data_feed=data.frame(Weight_Initial,Weight_Increment,feed)
library(HH)
m=ancova(Weight_Increment~Weight_Initial+feed, data=data_feed)
summary(m)

data=data.frame(x=c(25,70,60,85,95,90,80,60,20,30,15,40,35,50,70,60,80,90,70,75),g=factor(rep(1:3,c(7,6,7))))
kruskal.test(x~g, data=data)