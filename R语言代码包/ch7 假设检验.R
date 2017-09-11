#7.2.1 均值的假设检验
#方差已知
z.test=function(x,mu,sigma,alternative="two.sided"){
  n=length(x)
  result=list()  #构造一个空的list，用于存放输出结果
  mean=mean(x)
  z=(mean-mu)/(sigma/sqrt(n))  #计算z统计量的值
  options(digits=4)  #结果显示至小数点后4位
  result$mean=mean;result$z=z  #将均值、z值存入结果
  result$P=2*pnorm(abs(z),lower.tail=FALSE)  #根据z计算P值
  #若是单侧检验，重新计算P值
  if(alternative=="greater") result$P=pnorm(z,lower.tail=FALSE)
  else if(alternative=="less") result$P=pnorm(z)
  result  
}

bj=c(102.5,102.4,102.0,101.8,101.8,102.1,102.3,102.5,102.6,102.8,103.4,104.2)
hist(bj)
z.test(x=bj,mu=102.4,sigma.x=0.67,alternative="two.sided")
library(BSDA)
z.test(x=bj,mu=102.4,sigma.x=0.67,alternative="two.sided")

#方差未知
t.test(x=bj,mu=102.4,alternative="less")


#方差的假设检验
chisq.var.test=function(x,var,mu=Inf,alternative="two.sided"){
  n=length(x)
  df=n-1  #均值未知时的自由度
  v=var(x)  #均值未知时的方差估计值
  #总体均值已知的情况
  if(mu<Inf){df=n;v=sum((x-mu)^2)/n}
  chi2=df*v/var  #卡方统计量
  options(digits=4)
  result=list()  #产生存放结果的列表
  result$df=df;result$var=v;result$chi2=chi2;
  result$P=2*min(pchisq(chi2,df),pchisq(chi2,df,lower.tail=F))
  #若是单侧检验，重新计算P值
  if(alternative=="greater") result$P=pchisq(chi2,df,lower.tail=F)
  else if(alternative=="less") result$P=pchisq(chi2,df)
  result
}
chisq.var.test(bj,0.25,alternative="less")


#7.3.1 均值差的假设检验
#两总体方差已知
z.test2=function(x,y,sigma1,sigma2,alternative="two.sided"){
  n1=length(x);n2=length(y)
  result=list()  #构造一个空的list，用于存放输出结果
  mean=mean(x)-mean(y)
  z=mean/sqrt(sigma1^2/n1+sigma2^2/n2)  #计算z统计量的值
  options(digits=4)  #结果显示至小数点后4位
  result$mean=mean;result$z=z  #将均值、z值存入结果
  result$P=2*pnorm(abs(z),lower.tail=FALSE)  #根据z计算P值
  #若是单侧检验，重新计算P值
  if(alternative=="greater") result$P=pnorm(z,lower.tail=FALSE)
  else if(alternative=="less") result$P=pnorm(z)
  result  
}

sales=read.table("d:/data/sales.txt",header=T)
attach(sales)
z.test2(prior,post,8,12,alternative="less")
z.test(prior,post,sigma.x=8,sigma.y=12,alternative="less")

#两总体方差未知且不等
t.test(prior,post,var.equal=FALSE,alternative="less")
var.test(prior,post)


#7.3.2 成对数据t检验
x=c(117,127,141,107,110,114,115,138,127,122)
y=c(113,108,120,107,104,98,102,132,120,114)
t.test(x,y,paired=TRUE,alternative="greater")


#7.3.3两总体方差的检验
var.test(prior,post)


#7.4.1比率的二项分布检验
binom.test(214,2000,p=0.1)
#7.4.2比率的近似检验
prop.test(214,2000,p=0.1)


#7.5.1 总体分布的卡方检验
bj=c(102.5,102.4,102.0,101.8,101.8,102.1,102.3,102.5,102.6,102.8,103.4,104.2)
A=table(cut(bj,breaks=c(101.4,101.9,102.4,102.9,104.5)))  #两个函数嵌套使用
A
br=c(101.5,102,102.5,103,104.5)
p=pnorm(br,mean(bj),sd(bj))  #注意pnorm()计算出的是分布函数
p=c(p[1],p[2]-p[1],p[3]-p[2],1-p[3])
options(digits=2)
p
chisq.test(A,p=p)


#7.5.2 KS检验
X=c(420,500,920,1380,1510,1650,1760,2100,2300,2350)
ks.test(X,"pexp",1/1500)  #pxep为指数分布累积分布函数的名称，1/1500为指数分布参数

#双样本KS检验
xx=c(0.61,0.29,0.06,0.59,-1.73,-0.74,0.51,-0.56,0.39,1.64,0.05,-0.06,0.64,-0.82,0.37,1.77,1.09,-1.28,2.36,1.31,1.05,-0.32,-0.40,1.06,-2.47)
yy=c(2.20,1.66,1.38,0.20,0.36,0.00,0.96,1.56,0.44,1.50,-0.30,0.66,2.31,3.29,-0.27,-0.37,0.38,0.70,0.52,-0.71)
ks.test(xx,yy)