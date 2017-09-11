#5.2.2
data=read.csv("d:/data/中国人寿股价.csv")
attach(data)
price=Clsprc[!is.na(Clsprc)]  #获得价格向量，同时去掉缺失数据
mean(price)
median(price)
which.max(table(price))
quantile(price)
fivenum(price)
min(price);max(price)
summary(price)


#5.3.2 离散趋势
m=range(price);m[2]-m[1]
max(price)-min(price)
q=fivenum(price);q[4]-q[2]
var(price);sd(price)
sqrt((sum(price^2)-(sum(price))^2/length(price))/(length(price)-1))
mad(price)


#5.4.2 偏度/峰度
library(timeDate)  #加载程序包
skewness(price)  #计算偏度系数
kurtosis(price)

SK=function(x){
  n=length(x);m=mean(x)
  C=n/((n-1)*(n-2))*sum((x-m)^3)/(sd(x))^3;C
}
SK(price)  #手动计算偏度系数

kurtosis(price)
K=function(x){
  n=length(x);m=mean(x);s=sd(x)
  g2=((n*(n+1))/((n-1)*(n-2)*(n-3))*sum((x-m)^4)/s^4-(3*(n-1)^2)/((n-2)*(n-3)))
  g2
}
K(price)


#5.5.1 直方图和密度函数图
hist(price,breaks=50)  #参数breaks设置直方图的组距，prob=T规定绘制密度直方图
hist(price,breaks=50,prob=T)
lines(density(price),col="blue")  #利用核密度估计函数density()，绘制密度曲线图

#5.5.2 QQ图
qqnorm(price)
qqline(price)

a=c(21.0,21.4,22.5,21.6,19.6,20.6,20.3,19.2,20.2,21.3)
qqnorm(a);qqline(a)

#5.5.3 茎叶图
set.seed(111)  #设置抽样种子
s=sample(price,50)  #从数据集price中抽取50个样本
stem(s)

#5.5.4 箱线图
boxplot(price,main="Boxplot of price")

#5.5.5 经验分布图
plot(ecdf(price),main="empirical cdf",xlab="price",ylab="Fn(price)")
x=min(price):max(price)
lines(x,pnorm(x,mean(price),sd(price)),col="red")  #正态分布函数曲线
legend(60,0.4,legend=c("样本分布","正态分布"),lty=1,col=1:2)


#5.6.1 多组数据
group=read.csv("d:/data/09-11股价.csv")
group=na.omit(group)  #忽略缺失样本
summary(group)
options(digits=3) #设置显示格式：数字只显示3位
var(group)
cov(group)
cor(group,method="spearman")


#5.6.2
#二维散点图
attach(group)
plot(Price10~Price09,xlab="Price of 2009",ylab="Price of 2010")
lines(lowess(Price09,Price10),col="red",lwd=2)  #拟合曲线

#等高线图
library(MASS)
a=kde2d(Price09,Price10)
contour(a,col="blue",main="Contour plot",xlab="Price of 2009",ylab="Price of 2010")
persp(a)

#矩阵散点图
plot(group,main="Scatterplot Matrices")
pairs(group)

#矩阵图
matplot(group,type="l",main="Matplot")  #type=”l”表示绘制曲线而不是散点
legend(0,35,legend=2009:2011,pch="——",cex=0.6,col=1:3)  #cex指定字体大小

#箱线图
boxplot(group,cex.axis=0.6)

#星图
score=read.table("d:/data/score.txt",header=T)  #读入数据
stars(score)  #绘制星图，所有参数均设置为默认值
stars(score,full=FALSE,draw.segments=TRUE,key.loc=c(5,0.5),mar=c(2,0,0,0)) #full=F表示绘制成半圆的图形，draw.segments=T表示画出弧线

#折线图
outline=function(x){
  if(is.data.frame(x)==TRUE)
    x=as.matrix(x)  #若x为数据框，则先转换为矩阵形式
  m=nrow(x);n=ncol(x)  #提取x的行列数
  plot(c(1,n),c(min(x),max(x)),type="n",main="The outline graph of data",xlab="Number",ylab="Value")
  for(i in 1:m){
    lines(x[i,],col=i)
  }
}
outline(score)

#调和曲线图
unison=function(x){
  if (is.data.frame(x)==TRUE)
    x=as.matrix(x)  #若x为数据框，则先转换为矩阵形式
  t=seq(-pi, pi, pi/30)  #设置t的变化范围
  m=nrow(x); n=ncol(x)   #提取x的行列数
  f=array(0, c(m,length(t)))  #f赋值为一个数组
  for(i in 1:m){
    f[i,]=x[i,1]/sqrt(2)
    for( j in 2:n){
      if (j%%2==0)
        f[i,]=f[i,]+x[i,j]*sin(j/2*t)
      else
        f[i,]=f[i,]+x[i,j]*cos(j%/%2*t)
    }
  }
  plot(c(-pi,pi),c(min(f),max(f)),type="n",main ="The Unison graph of Data",xlab="t",ylab="f(t)")
  for(i in 1:m) lines(t,f[i,],col=i)
}
unison(score)