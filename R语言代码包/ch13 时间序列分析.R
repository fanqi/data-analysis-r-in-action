#ch13 时间序列分析
#13.1 时间序列概述
data=read.table("d:/data/M1.txt",header=T)
M=ts(data$M1,frequency=12, start=c(2002,1))
M
plot.ts(M)


#13.2.1 分解非季节性数据
library(TTR)
M.SMA3=SMA(M,n=3)
plot(M.SMA3)
M.SMA5=SMA(M,n=5)
plot(M.SMA5)


#13.2.2 分解季节性数据
data=c(362,385,432,341,382,409,498,387,
        473,513,582,474,544,582,681,557,
        628,707,773,592,627,725,854,661)
sales=ts(data,frequency=4,start=c(2004,1))
plot.ts(sales)
components=decompose(sales)
options(digits=3)  #显示小数点后3位有效数字
components$seasonal  #季节性部分
plot(components)
components1=stl(sales,s.window="periodic")
components1
plot(components1)


#13.3.1 简单指数平滑法
data=scan("http://robjhyndman.com/tsdldata/hurst/precip1.dat",skip=1)  #skip=1表示跳过第一行（基本信息）再开始读入数据
rain=ts(data,start=1813)  #start设置时间序列从1813年开始
plot.ts(rain)  #画时序图

rain.pre=HoltWinters(rain,beta=FALSE,gamma=FALSE)
rain.pre
plot(rain.pre)
rain.pre$SSE
library(forecast)
rain.pre2=forecast.HoltWinters(rain.pre, h=10) 
plot.forecast(rain.pre2)


#13.3.2 残差的白噪声检验
acf(rain.pre2$residuals,lag.max=20)
Box.test(rain.pre2$residuals,lag=20,type="Ljung-Box")


#13.3.3 Holt双参数线性指数平滑法
data=read.table("d:/data/M1.txt",header=T)
M=ts(data$M1,frequency=12, start=c(2002,1))
M.pre=HoltWinters(M,gamma=FALSE)
M.pre
M.pre$SSE
plot(M.pre)

M.pre2=forecast.HoltWinters(M.pre,h=20)  #20个季度，所以h=20
plot.forecast(M.pre2)
acf(M.pre2$residuals,lag.max=10)
Box.test(M.pre2$residuals,lag=10, type="Ljung-Box")


#13.3.4 Winters线性和季节性指数平滑法
data=c(362,385,432,341,382,409,498,387,
+       473,513,582,474,544,582,681,557,
+       628,707,773,592,627,725,854,661)
sales=ts(data,frequency=4,start=c(2004,1))
sales.pre=HoltWinters(sales)
sales.pre
sales.pre$SSE
plot(sales.pre)

sales.pre2=forecast.HoltWinters(sales.pre,h=4)
plot.forecast(sales.pre2)  #这里使用函数plot.forecast()与plot()是一样的
acf(sales.pre2$residuals,lag.max=8)
Box.test(sales.pre2$residuals,lag=8, type="Ljung-Box")


#13.4.2 时间序列的平稳化处理
data=read.table("d:/data/M1.txt",header=T)
M=ts(data$M1,frequency=12, start=c(2002,1))
plot.ts(M)

M.diff=diff(M)
plot(M.diff)

logM.diff=diff(log(M))
op=par(mfrow=c(1,2))
plot(diff(logM.diff))
acf(logM.diff,lag.max=10)


#13.4.3 建立适当的ARIMA模型
layout(1)
pacf(M.diff,lag.max=10)
library(forecast)
auto.arima(M.diff,ic="bic")


#13.4.4 ARIMA模型的参数估计
M.arima=arima(log(M),order=c(1,1,1))
M.arima
options(digits=5)


#13.4.5 模型预测及检验
M.pre3=forecast.Arima(M.arima,h=12)
M.pre3
plot.forecast(M.pre3)

acf(M.pre3$residuals,lag.max=5)
Box.test(M.pre3$residuals,lag=5,type="Ljung-Box")
plot(M.pre3$residuals)
