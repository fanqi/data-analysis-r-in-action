#11.1  典型相关分析
post=read.table("d:/data/post.txt",header=T)
post=post[,-1]  #去除第一列（通常为年份或序号）
post=scale(post)  #对数据作正态离差标准化
ca=cancor(post[,1:4],post[,5:8])
options(digits=4)
ca


#11.2.3   对应分析——函数corresp()
ch=data.frame(A=c(47,22,10),B=c(31,32,11),C=c(2,21,25),D=c(1,10,20))   #直接给出矩阵的列名
rownames(ch)=c("Pure-Chinese","Semi-Chinese","Pure-English")  #给出矩阵的行名
library(MASS)
ch.ca=corresp(ch,nf=2)
options(digits=4)
ch.ca
biplot(ch.ca,xlim=c(-1,1),ylim=c(-0.3,0.3))  #控制横纵轴的范围

#11.2.4  对应分析——程序包ca
brand=data.frame(low=c(2,49,4,4,15,1),medium=c(7,7,5,49,2,7),high=c(16,3,23,5,5,14))
rownames(brand)=c("A","B","C","D","E","F")
library(ca)
options(digits=3)
brand.ca=ca(brand)
brand.ca
names(brand.ca)
brand.ca$rowcoord
plot(brand.ca)