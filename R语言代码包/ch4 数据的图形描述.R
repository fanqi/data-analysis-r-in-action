#4.2.1
#首先，准备绘图数据：从外部读取或随机数生成
dat=read.table("d:/data/online shopping.txt",header=T)
attach(dat)
x=rnorm(1000)
x=x[x<0]
y=data.frame(x1=1:5,x2=rnorm(5,0,1),x3=rgamma(5,2,3))
#将区域分为4个部分
par(mfrow=c(2,2))
#分别输入4个绘图命令
plot(period,amount)
hist(x,xlim=range(x),main="hist of x",freq=F,nclass=30,density=20,angle=45)
matplot(y,type="l",col=1:3)
plot(period,amount,pch=22,col="red",bg="yellow",cex=1.5)
title("online shopping",font.main=3,adj=1)


#4.2.3
split.screen(c(2,1))   #将图形区域分成上下两部分显示
split.screen(c(1,2),screen=2)  #将第二部分（下半区）又分割成两个区域
screen(1)   #准备在第一个区域绘图
hist(x,xlim=range(x),main="hist of x",freq=F,nclass=30,density=20,angle=45)
screen(3)   #准备在下半区的第一个区域绘图
matplot(y,type="l",col=1:3)
screen(4)   #准备在下半区的第二个区域绘图
plot(period,amount,pch=22,col="red",bg="yellow",cex=1.5)


#4.3.1
dat=read.table("d:/data/online shopping.txt",header=T)
attach(dat)
plot(period,amount)
plot(amount~period,data=dat)
with(dat,plot(period,amount))

percent=c(56.7,19.6,5.5,4.7,2.7,10.8)
labels=c("天猫","京东","苏宁易购","腾讯B2C","亚马逊中国","其他")  #添加标签
pie(percent,labels,col=2:7,main="Pie chart for online shopping",font=2)

x=rnorm(1000)
x=x[x<0]
hist(x,xlim=range(x),main="hist of x",freq=F,nclass=30,density=20,angle=45)

#4.3.2
y=data.frame(x1=1:5,x2=rnorm(5,0,1),x3=rgamma(5,2,3))
matplot(y,type="l",col=1:3)
#添加图例
legend(1,5,col=1:3,pch="-",legend=c("x1","x2","x3"))

data(warpbreaks)
coplot(breaks ~ 1:54 | wool * tension, data = warpbreaks,col = "red", bg = "pink", pch = 21,bar.bg = c(fac = "light blue"))

#4.3.3 低级绘图函数
lines(density(x),col="blue")
lines(-3:0,dnorm(-3:0,mean(x),sd(x)),col="red")
legend(-3,1,pch=c(15,-1,-1),lty=c(-1,1,1),col=c("gray","blue","red"),legend=c("histogram","density line","normal density line"))

#4.3.4
plot(period,amount,pch=22,col="red",bg="yellow",cex=1.5)
title("online shopping",font.main=3,adj=1)

#4.3.5 交互式绘图命令
x=rnorm(10)  #生成10个随机数
plot(x)    #对x绘制散点图
locator(5,"o",col="red")
locator(2)

plot(x)
identify(4,1,label="e")
identify(4,1,label="e",col="red")
identify(c(4,6),c(1,0),label=c("a","b"),col="red")


#4.4
x=seq(-10, 10, length.out = 50)
y=x
#自定义函数rotsinc
rotsinc=function(x,y)
  {
  sinc=function(x) { y=sin(x)/x ; y[is.na(y)]=1; y }
  10 * sinc( sqrt(x^2+y^2) )
  }
sinc.exp=expression(z == Sinc(sqrt(x^2 + y^2)))  #expression用于转换数学表达式
z=outer(x, y, rotsinc)#数组外积运算：通过x,y和rotsinc函数生成矩阵z
oldpar=par(bg = "white")
persp(x, y, z, theta = 30, phi = 30, expand = 0.5, col = "lightblue")
title(main = sinc.exp)

mortality=read.csv('d:/data/swedish mortality.csv',header=T)
library(rgl)
attach(mortality)
plot3d(Year,Age,q_male,col='grey',type='p',zlim=1)


#4.5 lattice程序包
install.packages("ggplot2")
library("ggplot2")
data(diamonds,package="ggplot2")

#为图形效果更好，从数据中随机抽取1000个样本作图
sample=diamonds[sample(nrow(diamonds),1000),]
xyplot(price~carat,data=sample,groups=cut,auto.key=list(corner=c(1,0)),type=c("p","smooth"),span=0.7,main="Price VS. Carat")
bwplot(color~price | cut,data=diamonds,main="Box-and-Whisker Plots of Price")

library(lattice)
histogram(~price | color,data=diamonds,layout=c(2,4))

x=seq(-pi,pi,len = 20)
y=seq(-pi,pi,len = 20)
g=expand.grid(x=x,y=y)  #构造一个数据框，内部变量为x,y
g$z=sin(sqrt(g$x^2+g$y^2))  #对数据框g添加变量z
wireframe(z~x*y,data=g,drape=T,aspect=c(3,1),colorkey=TRUE,main=expression(z=sin(sqrt(x^2+y^2))))


#4.6.1 ggplot2快速绘图
sample=diamonds[sample(nrow(diamonds),1000),]
#参数shape指定变量cut为分类依据，按变量color绘制不同颜色的散点
qplot(carat,price,data=sample,color=color)
qplot(carat,price,data=sample,geom=c("point","smooth"),span=0.3)
qplot(carat,data=diamonds,geom="histogram",binwidth=0.1,xlim=c(0,3),fill=color)


#4.6.2分图层绘图
sample=diamonds[sample(nrow(diamonds),1000),]
p=ggplot(data=sample,aes(x=carat,y=price,color=clarity))

p+geom_point()+geom_smooth()
#another type
d=ggplot(data=sample,aes(x=carat,y=price,color=clarity))+geom_point()+geom_smooth()
print(d)

#整体平滑
p=ggplot(data=sample,aes(x=carat,y=price))  #这里不再指定color的分类变量
p+geom_point(aes(color=clarity))+geom_smooth()  #仅第二图层分类画点

sample=diamonds[sample(nrow(diamonds),100),] #从diamonds中取一个样本量100的子集
p=ggplot(data=sample,aes(x=carat,y=price))
#设置三个分类变量color, cut和clarity，分别用不同颜色、形状和大小表示
#参数alpha控制透明度
#position="jitter"对散点增加扰动，防止点的过度重叠
p+geom_point(aes(color=color,shape=cut,size=clarity),alpha=0.5,position="jitter")

#标度
p=ggplot(data=diamonds,aes(x=carat))  #指定x轴为carat变量
p+geom_histogram()+scale_x_continuous(limits=c(0,3))+opts(title="Hist of carat")
#可以使用xlim(0,3)代替scale_x_continuous(limits=c(0,3))
opts(title) #添加图像标题

p=ggplot(data=diamonds,aes(x=carat,fill=color))
#注意，由于要绘制直方图，color应该映射到参数fill表示填充颜色
p+geom_histogram()+xlim(0,3)+scale_colour_manual(values=rainbow(7))

#统计变换
sample=diamonds[sample(nrow(diamonds),1000),]
ggplot(sample,aes(x=carat,y=price))+geom_point()+scale_y_log10()+stat_smooth()
#第二图层添加散点；第三图层对y轴作log10变换；第四图层添加平滑的统计变换

#分面
ggplot(sample,aes(x=carat,y=price))+geom_point(aes(colour=cut))+scale_y_log10()+stat_smooth()+facet_wrap(~cut,ncol=3)
#在geom_point()中将cut映射到颜色属性中；facet_wrap()中的ncol=3表示每行画3个图

#坐标轴系统
ggplot(diamonds)+geom_bar(aes(x=factor(1),fill=cut))+coord_polar(theta="y")
#theta=”y”表示将条形图y轴高度转换为饼图的角度


#4.7 图像保存
png(file="d:/data/pie.png", bg="transparent")
dev.off()

ggsave(filename="d:/data/pie.pdf")
jpeg(file="d:/data/pie.jpeg", bg="transparent")
dev.off()


#4.8案例：数据地图
#方法一
library(ggplot2)
lbj=read.table("d:/data/lbj.txt",header=T,quote="'") #读取数据
attach(lbj)
head(lbj)  #查看数据集的前5行
state_map=map_data("state")  #获取美国地图的数据信息

p=ggplot(lbj,aes(map_id=state))+geom_map(aes(fill=AvgPTS),map=state_map)+
  expand_limits(x=state_map$long,y=state_map$lat)+
  scale_fill_continuous(limits=c(19,max(AvgPTS)),high='red3',low='yellow',guide="colorbar")+
  opts(title='詹姆斯客场平均得分')

attach(state_map)
state.uni=unique(region)  #存放各州的名称
xx=0;yy=0  #事先建立变量xx和yy，下面用循环找到每个州对应的坐标值
for(i in 1:length(state.uni))
{
  xx[i]=mean(long[region==state.uni[i]])
  yy[i]=mean(lat[region==state.uni[i]])
}

order=0  #按变量state.uni的顺序找到数据集lbj中各州的位置，存放于变量order
for(i in 1:length(state.uni)){
  order[i]=which(state==state.uni[i])
}
labels=Opp[order]  #通过位置找到各州对应的球队名称
p+annotate("text",x=xx,y=yy,label=labels)  #最后绘图并添加注释


#方法二：sp()函数
install.packages(“sp”) 
library(sp)  #安装并加载程序包
#从gadm.org网站上得到中国的省区地理数据，并加载到R软件内存中
load(url("http://gadm.org/data/rda/CHN_adm1.RData"))
#将每个省人口数据按顺序存放在数据框gadm中，生成一个变量pop
gadm$pop=c(1961,1293,7185,3571,2470,4374,2745,3831,2301,7866,5442,
           5950,3689,4456,9579,9402,5723,6570,10432,4602,867,2884,
           8041,3474,4596,300,3732,2557,562,630,2181,706)
#利用空间绘图命令进行绘图
spplot(gadm,"pop",col.regions = rev(terrain.colors(gadm$pop)),main="中国各省人口数据")