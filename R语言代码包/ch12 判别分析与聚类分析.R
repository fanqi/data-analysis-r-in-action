# ch12 判别分析与聚类分析
# 12.1.2距离判别法的R语言实现
B=read.table("d:/data/bankruptcy.txt",header=T)
mu=colMeans(B)  #对矩阵的列求均值，直接得到各指标的均值
Sx=cov(B)  #计算训练样品
distance=mahalanobis(B,mu,Sx)
options(digits=3)  #设置显示小数点格式
distance

library(WMDB)
G=c(rep(1,17),rep(2,21))  #生成38个训练样品的已知类别
G=as.factor(G)  #转换成因子向量，才能代入函数wmd()计算
wmd(B,G)
newdata=data.frame(X1=c(0.04,-0.06,0.07,-0.13,0.15,0.16,0.29,0.54),
              X2=c(0.01,-0.06,-0.01,-0.14,0.06,0.05,0.06,0.11),
              X3=c(1.5,1.37,1.37,1.42,2.23,2.31,1.84,2.33),
              X4=c(0.71,0.4,0.34,0.44,0.56,0.2,0.38,0.48))
wmd(B,G,TstX =to)  #TstX表示待判样品矩阵


# 12.1.4 Fisher判别法的R语言实现
B=read.table("d:/data/bankruptcy.txt",header=T)
G=c(rep(1,17),rep(2,21))  #生成38个训练样品的已知类别
G=as.factor(G)  #转换成因子向量
B$class=G  #将因子向量G存入数据框B中
attach(B)
names(B)  #显示数据框B中的所有对象
library(MASS)
B.lda=lda(class~X1+X2+X3+X4)
B.lda

class.pre=predict(B.lda)$class  #选择预测结果中的对象class，是预测的样本所属类别
table(class.pre,class)
chisq.test(class,class.pre)

newdata=data.frame(X1=c(0.04,-0.06,0.07,-0.13,0.15,0.16,0.29,0.54),
              X2=c(0.01,-0.06,-0.01,-0.14,0.06,0.05,0.06,0.11),
              X3=c(1.5,1.37,1.37,1.42,2.23,2.31,1.84,2.33),
              X4=c(0.71,0.4,0.34,0.44,0.56,0.2,0.38,0.48))
predict(B.lda,newdata=newdata)
#plot(predict(B.lda)$x,type="n",xlab="LD I",ylab="LD II")
#text(predict(B.lda)$x,levels(predict(B.lda)$class[predict(B.lda)$class]),col=unclass(class),cex=1.5)


# 12.1.6贝叶斯判别法的R语言实现
library(WMDB)
dbayes(B,G)


#12.2 聚类分析
drink=read.table("d:/data/drink.txt",header=T)
drink=drink[,-1]  #去掉第一列编号
d=dist(drink)
hc1=hclust(d,method="ward")  #离差平方和法
hc2=hclust(d,method="single")  #最短距离法
hc3=hclust(d,method="complete")  #最长距离法
hc4=hclust(d,method="median")  #中间距离法
opar=par(mfrow=c(2,2))  #分割绘图区域
plot(hc1,hang=-1);plot(hc2,hang=-1);plot(hc3,hang=-1);plot(hc4,hang=-1)
par(opar)  #释放绘图区域分割
class=cutree(hc1,4)

drink.hc=as.dendrogram(hc1)
par(mfrow=c(1,2))
plot(drink.hc,type="triangle",nodePar=list(pch=c(1,NA),lab.cex=0.8))
plot(drink.hc,nodePar=list(pch=2:1,cex=0.4*2:1,col=2:3),horiz=TRUE)

dat=read.table("d:/data/real estate.txt",header=T)
dat=dat[,-1]
d=dist(dat)
hc=hclust(d,method="ward")
plot(hc,hang=-1)