#10.1.2 主成分分析
agri=read.table("d:/data/agriculture.txt",header=TRUE)
agri=agri[,-1]  #剔除第一列序号
agri.pr=princomp(agri,cor=TRUE)  #用相关阵计算
options(digits=4)  #结果显示4位有效数字
summary(agri.pr,loadings=TRUE)  #loadings=TURE选项列出主成分对应原始变量的系数

screeplot(agri.pr,type="line",main="碎石图")
biplot(agri.pr)

food=read.table("d:/data/food.txt",header=T)   #读入数据
food=food[,-1]
library(labdsv)
food.pca=pca(food,dim=4,cor=TRUE)  #利用相关系数矩阵计算

summary(food.pca)
loadings.pca(food.pca)

op=par(mfrow=c(1,2))  #分割图形区域
varplot.pca(food.pca)
layout(1)



#10.2 主成分分析法·因子分析函数
factor.analysis=function(x,m){
  p=nrow(x);x.diag=diag(x);sum.rank=sum(x.diag)
  rowname=paste("X",1:p,sep="")  #设置行名、列名
  colname=paste("Factor",1:m,sep="")
  A=matrix(0,nrow=p,ncol=m,dimnames=list(rowname,colname))  #构造因子载荷矩阵A，初值设为0
  eig=eigen(x)  #eig包含两个元素,values为特征根，vectors为特征向量
  for(i in 1:m)
    A[,i]=sqrt(eig$values[i])*eig$vectors[,i]  #填充矩阵A的值
  var.A=diag(A%*%t(A))  #公共因子的方差
  rowname1=c("SS loadings","Proportion Var","Cumulative Var")
  result=matrix(0,nrow=3,ncol=m,dimnames=list(rowname1,colname))  #构造输出结果的矩阵，初值设为0
  for(i in 1:m){
    result[1,i]=sum(A[,i]^2)  #计算各因子的方差
    result[2,i]=result[1,i]/sum.rank  #计算方差贡献率
    result[3,i]=sum(result[1,1:i])/sum.rank  #累计方差贡献率
  }
  method=c("Principal Component Method")
  #输出计算结果
  list(method=method,loadings=A,var=cbind(common=var.A,specific=x.diag-var.A),result=result)
}

bank=read.table("d:/data/bank.txt",header=T)
bank=bank[,-1]  #剔除第一列序号
R=cor(bank)  #计算相关系数矩阵
options(digits=3)  #结果显示3位有效数字
factor.analysis(R,5)

yg=read.table("d:/data/yagao.txt",header=T)
data=yg[,-1]
factanal(data,factors=2)


library(labdsv)
p5=pca(USArrests,dim=4,cor=TRUE)#利用相关系数矩阵计算
summary(p5)
varplot.pca(p5)#绘制碎石图以及累计方差图形
loadings.pca(p5)
plot(p5)


