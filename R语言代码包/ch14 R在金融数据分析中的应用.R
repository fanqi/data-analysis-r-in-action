#ch14 R在金融数据分析中的应用
#【案例1】 股票投资组合最优化
setwd("d:/data")  #设定数据文件所在的路径为默认路径
data=read.csv("Dalyr.csv",header=TRUE)
code=unique(data$Stkcd)  #提取股票的唯一标识——股票代码
n=length(code)
#整理数据
#计算30支股票的协方差阵和平均收益
Dalyr=matrix(0,220,n)
for (i in 1:n)
+ {
+ Dalyr[,i]=data$Dretnd[which(data$Stkcd==code[i])]
+ }
r.cov=cov(Dalyr)#cov对一个矩阵求协方差,结果为一个矩阵
r.mean=apply(Dalyr,2,mean)  #对每列计算均值
options(digits=2)
r.mean
plot(r.mean,main='30支股票期望收益率',pch=8,col=2)


###均值—方差模型###
t11=Sys.time()
num=100000#循环十万次，画出随机点
rp=numeric(num);sigmap=numeric(num)  #定义两个变量，存放投资组合的均值和标准差
for (i in 1:num)
+ {
+   x1=runif(30)
+   x=x1/sum(x1)
+   rp[i]=sum(r.mean*x)
+   sigmap[i]=t(x)%*%r.cov%*%x
+ }
plot(sigmap,rp,pch='.',main='随机打点法有效边界')  #绘制10万次模拟的投资组合散点
#计算有效边缘
rp=round(rp,4)
rp.uni=unique(rp)
nn=length(rp.uni)
sigma.min=numeric(nn)
for (i in 1:nn)
+ {
+   sigma.min[i]=min(sigmap[which(rp==rp.uni[i])])
+ }
rp.sort=sort(rp.uni)
order=order(rp.uni)
lines(rp.sort~sigma.min[order],col='red')
t12=Sys.time()
time.used1=t12-t11;time.used1#朴素法使用的时间


###simulated annealing###
t21=Sys.time()
temp0=10000  #确定初温
temp=temp0
x0=c(1,runif(29))
x=x0/sum(x0)
L=10  #循环次数
M=1000  #惩罚因子
R=0.0025  #目标收益率
f1=function(x){t(x)%*%r.cov%*%x+M*max(0,R-sum(r.mean*x))}  #乘法函数法
f2=function(x){(R-sum(r.mean*x))/sqrt(t(x)%*%r.cov%*%x)}  #正态假设法

f=f1(x)  #记录每次退温的最优解
f=as.numeric(f)
k=1  #记录退温次数
a=data.frame(x)#记录权重
e=0.00001
while(temp>0.001)
+ {
+   for(i in 1:L)
+   {
+     x1=x+runif(30)
+     x1=x1/sum(x1)
+     deltaf=f1(x1)-f1(x)
+     pr=exp(-deltaf/temp)  #Meropolis判断概率
+     if(deltaf<0) {x=x1;f[k+1]=f1(x1)} else {if(pr>runif(1)) x=x1;f[k+1]=f1(x1)}
+   }
+   k=k+1
+   a[k]=x
+   temp=temp0/(k^3)#退温
+ }

#退火法选取的投资组合
r.sa=0;sigma.sa=0
for (i in 1:length(a))
+ {
+   r.sa[i]=sum(r.mean*a[[i]])
+   sigma.sa[i]=t(a[[i]])%*%r.cov%*%a[[i]]
+ }
plot(sigma.sa,r.sa,main='模拟退火法有效边界')
#有效边缘
r.sa=round(r.sa,4)
rsa.uni=unique(r.sa)
nn=length(rsa.uni)
sigma.min=numeric(nn)
for (i in 1:nn)
+ {
+   sigma.min[i]=min(sigma.sa[which(r.sa==rsa.uni[i])])
+ }
rsa.sort=sort(rsa.uni)
order=order(rsa.uni)
lines(rsa.sort~sigma.min[order],col='red',lwd=2)
legend(0.000101,-4e-04,legend='正态假设法')
t22=Sys.time()
time.used2=t22-t21;time.used2#退火法使用的时间



#【案例2】构造投资组合的有效前沿
install.packages("quantmod")
library(quantmod) #载入程序包
getSymbols(c('IBM','SPY','YHOO'))   #获取数据
IBM_ret=dailyReturn(IBM)  
SPY_ret=dailyReturn(SPY)
YHOO_ret=dailyReturn(YHOO)
data=merge(IBM_ret,SPY_ret,YHOO_ret)  #合并收益率

library(timeSeries)
data=as.timeSeries(data)  #转换对象
library(fPortfolio)
Frontier=portfolioFrontier(data)  #计算有效前沿
Frontier
plot(Frontier)



#【案例3】股票聚类分析
stock=read.table("d:/data/stock.txt",header=T)
rownames(stock)=stock[,1]
KM=kmeans(stock[,2:8],4)
KM

#library(fpc)  #为了引入绘制聚类图的函数
#plotcluster(KM)
sort(KM$cluster)

#分层聚类
subset=subset(stock,select=2:8) #去掉第一列的股票名称
d=dist(subset)  #计算欧式距离
hc=hclust(d,method="ward")  #进行Ward聚类
plclust(hc)  #绘制分层聚类的谱系图
re=rect.hclust(hc,k=4,border="red")