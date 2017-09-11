#3.1
data=read.table("d:/data/salary.txt",header=T)
attach(data)
mean(Salary)  #求均值
length(Salary)  #数据长度（个数）
cumsum(Salary)  #累积工资

salary1=cut(Salary,3,labels=c("low","medium","high"))
table(salary1)
salary1=cut(Salary,3,labels=c("low","medium","high"))  #给每个区间设置标签
table(salary1)

breakpoints=c(0,30,40,50,60,70)
salary2=cut(Salary,breaks=breakpoints)
table(salary2)

pic=function(x){
  par(mfrow=c(2,2))  #绘图区域分割为四部分
  hist(x)            #直方图
  dotchart(x)        #点图
  boxplot(x)         #箱线图
  qqnorm(x);qqline(x)#正态概率图
  par(mfrow=c(1,1))  #恢复单图区域
}
pic(Salary)  #调用编写好的函数pic()


#3.2.1 修改数据标签
data=read.table("d:/data/salary.txt",header=T,stringsAsFactors=F)
names(data)=c("CITY","WORK","PRICE","SALARY")
names(data)

#3.2.2 行列删除
data2=data[-1,-3]
data2


#3.3.1
attach(data)
data$SALARY=replace(SALARY,SALARY>65,NA)
#data1=data
#data$PRICE=replace(PRICE,PRICE>80,NA)
is.na(SALARY)
sum(is.na(SALARY))
complete.cases(data$SALARY)


#3.3.2
data$PRICE=replace(PRICE,PRICE>80,NA)
install.packages("mice")
library(mice)
md.pattern(data)

install.packages("VIM")
library(VIM)
aggr(data)


#3.3.3
data1=data[complete.cases(data$SALARY),]
dim(data1)
data2=data[!is.na(SALARY),]
dim(data2)
data3=na.omit(data)
dim(data3)

data[is.na(data)]=mean(SALARY[!is.na(SALARY)])   #mean函数是对非NA值的SALARY数据求平均

data=read.table("d:/data/salary.txt",header=T)
names(data)=c("CITY","WORK","PRICE","SALARY")
attach(data)
data$SALARY=replace(SALARY,SALARY>65,NA)
imp=mice(data,seed=1)  #随机模拟数据
fit=with(imp,lm(SALARY~WORK+PRICE))  #线性回归
pooled=pool(fit)  #回归结果
options(digits=3)  #显示小数点后三位
summary(pooled)

data.pre=data[is.na(data$SALARY),][,2:3]  #选取缺失样本的WORK和PRICE值
data.pre=as.matrix(cbind(rep(1,4),data.pre))
q=pooled$qbar  #通过拟合回归预测SALARY
pre=data.pre%*%q;pre  #预测结果
index=is.na(data$SALARY)
data$SALARY[index]=pre   #替换缺失值
data[index,]


#3.4.1
a=c("Hongkong",1910,75.0,41.8)
data1=rbind(data,a)
data1[14:16,]

weight=c(150,135,210,140)  #数值型向量
height=c(65,61,70,65)
gender=c("F","F","M","F")  #字符型向量
stu=data.frame(weight,height,gender)
row.names(stu)=c("Alice","Bob","Cal","David")
stu[,"weight"]
stu["Cal",]  #获取行
stu[1:2,1:2]
stu$weightt  # ”$”用于取列
stu[["weight"]]  #双括号+名称
stu[[1]]  #双括号+下标，用于数据框和列表数据的获取

index=list("City"=data$City,"Index"=1:15)
index
data.index=merge(data,index,by="City")
data.index


#3.4.2
data[data$Salary>65,]
data[c(2,4),]
data[data$Price==65.6,]


#3.4.3
order.salary=order(data$Salary)
order.salary
sort.list(data$Salary)
data[order.salary,]
rank(data$Salary)


#3.5.1
t(data)
x=data.frame(A=1:4,B=seq(1.2,1.5,0.1),C=rep(1,4))
x
x1=stack(x)
x1
unstack(x1,form=values~ind)


#3.5.2
install.packages("reshape2")
library("reshape2", lib.loc="D:/R-3.0.1/library")
melt(x)
data(airquality)
str(airquality)  #显示R对象的内部结构，功能类似于summary()
longdata=melt(airquality,id.vars=c("Ozone","Month","Day"),measure.vars=2:4)
str(longdata)

library(ggplot2)
p=ggplot(data=longdata,aes(x=Ozone,y=value,color=factor(Month)))
p+geom_point(shape=20,size=4)+facet_wrap(~variable,scales="free_y")+ geom_smooth(aes(group=1), fill="gray80")  #scale=”free_y”设置每个图形自动调整y轴范围

shortdata=dcast(longdata,formula=Ozone+Month+Day~variable)
head(shortdata,5)