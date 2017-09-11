#6.6.1 矩估计
num=c(rep(0:5,c(1532,581,179,41,10,4)))
#用rep()函数生成样本，样本值有0~5的数字构成，函数中的第二个向量对应表示每个数字的重复次数
lambda=mean(num)  #泊松的似然估计值即样本均值

k=0:5
ppois=dpois(k,lambda)
poisnum=ppois*length(num)  #由poisson分布生成的损失次数
plot(k,poisnum,ylim=c(0,1600))
#画图比较，为图形效果更好，用参数ylim设置纵轴的范围，最小值为0，最大值要大于样本的最值，选取1600
samplenum=as.vector(table(num))  #样本的损失次数
points(k,samplenum,type="p",col=2)
legend(4,1000,legend=c("num","poisson"),col=1:2,pch="o")

#多个参数的矩估计
install.packages("rootSolve")
x=c(4,5,4,3,9,9,5,7,9,8,0,3,8,0,8,7,2,1,1,2)
m1=mean(x)  #样本均值
m2=var(x)  #样本方差
model=function(x,m1,m2){
  c(f1=x[1]+x[2]-2*m1,
    f2=(x[2]-x[1])^2/12-m2)
}
library(rootSolve)
multiroot(f=model,start=c(0,10),m1=m1,m2=m2)
m1-sqrt(3*m2);m1+sqrt(3*m2)


#6.1.2 MLE
#极值函数计算
library(MASS)
head(geyser,5)
attach(geyser)
hist(waiting,freq=FALSE)

ll=function(para)
{
  f1=dnorm(waiting,para[2],para[3])
  f2=dnorm(waiting,para[4],para[5])
  f=para[1]*f1+(1-para[1])*f2
  ll=sum(log(f))
  return(-ll)
}
geyser.est=nlminb(c(0.5,50,10,80,10),ll,lower=c(0.0001,-Inf,0.0001,-Inf,0.0001),upper=c(0.9999,Inf,Inf,Inf,Inf))
options(digits=3)
geyser.est$par  #查看拟合的参数结果
p=geyser.est$par[1]
mu1=geyser.est$par[2];sigma1=geyser.est$par[3]
mu2=geyser.est$par[4];sigma2=geyser.est$par[5]
x=seq(40,120)
#将估计的参数函数代入原密度函数
f=p*dnorm(x,mu1,sigma1)+(1-p)*dnorm(x,mu2,sigma2)
hist(waiting,freq=F)
lines(x,f)  #画出拟合曲线

#函数maxLik()计算
num=c(rep(0:5,c(1532,581,179,41,10,4)))
install.packages("maxLik")
loglik=function(para){
   f=dnbinom(num,para[1],1/(1+para[2]))#注意第二个参数不是beta，是prob
   ll=sum(log(f))
   return(ll)
 }
library(maxLik)
para=maxLik(loglik,start=c(0.5,0.4))$estimate  #极大似然估计
r=para[1];beta=para[2]
l=length(num)
nbinomnum=dnbinom(0:5,r,1/(1+beta))*l;nbinomnum
plot(0:5,nbinomnum,ylim=c(0,1600))  #画图比较
points(0:5,nbinomnum,type="p",col=2)
legend(3,1000,legend=c("num","poisson"),col=1:2,lty=1)


#6.2.1 均值的区间估计
#方差已知
conf.int=function(x,sigma,alpha){
  mean=mean(x)
  n=length(x)
  z=qnorm(1-alpha/2,mean=0,sd=1,lower.tail=TRUE)
  c(mean-sigma*z/sqrt(n),mean+sigma*z/sqrt(n))
}
set.seed(111)  #设定随机种子
x=rnorm(20,10,2)
conf.int(x,2,0.05)

library("BSDA")
z.test(x,sigma.x=2)$conf.int

library("UsingR", lib.loc="D:/R-3.0.2/R-3.0.2/library")
simple.z.test(x,2)

#方差未知
t.test(x)$conf.int


#6.2.2 方差的区间估计
var.conf.int=function(x,mu=Inf,alpha){
  n=length(x)
  if(mu<Inf){
    s2=sum((x-mu)^2)/n
    df=n
  }
  else{
    s2=var(x)
    df=n-1
  }
  c(df*s2/qchisq(1-alpha/2,df),df*s2/qchisq(alpha/2,df))
}
var.conf.int(x,alpha=0.05)


#6.3.1 均值差的区间估计
#两总体方差已知
sales=read.table("d:/data/sales.txt",header=T)
head(sales)
attach(sales)
par(mfrow=c(1,2))
hist(prior)  #分别绘制计划前后销售额的直方图
hist(post)

twosample.ci=function(x,y,alpha,sigma1,sigma2){
  n1=length(x);n2=length(y)
  xbar=mean(x)-mean(y)
  z=qnorm(1-alpha/2)*sqrt(sigma1^2/n1+sigma2^2/n2)
  c(xbar-z,xbar+z)
}
twosample.ci(post,prior,alpha=0.05,8,12)
z.test(post,prior,sigma.x=8,sigma.y=12)$conf.int

#两总体方差未知但相等
t.test(post,prior,var.equal=TRUE)$conf.int

#两总体方差未知且不等
twosample.ci2=function(x,y,alpha){
  n1=length(x);n2=length(y)
  xbar=mean(x)-mean(y)
  S1=var(x);S2=var(y)
  nu=(S1/n1+S2/n2)^2/(S1^2/n1^2/(n1-1)+S2^2/n2^2/(n2-1))
  z=qt(1-alpha/2,nu)*sqrt(S1/n1+S2/n2)
  c(xbar-z,xbar+z)
}
twosample.ci2(post,prior,0.05)


#6.3.2 两方差比的区间估计
var.test(prior,post)$conf.int

#6.4 比率的区间估计
prop.test(214,2000)
binom.test(214,2000)