#2.1.1
data()  #查看数据集列表
data(CO2)  #载入CO2数据集（来自datasets）

library(MASS)   #载入package MASS
data(package="MASS")   #查看MASS中数据集
data(SP500,package="MASS")  #载入MASS中的SP500数据集，也可简化为data(SP500)


#2.1.2
getwd()   #返回当前工作目录
setwd("d:/data")  #也可以写成setwd("d:\\data")
getwd()

data=read.table("d:/data/salary.txt",header=T)  #读取数据
data=read.table("salary.txt",header=T)
data
data=read.csv("salary.csv",header=T)

data2=scan("salary.txt",skip=1,what=list(City="",Work=0,Price=0,Salary=0))  #由于不存在header参数，skip=1说明读取时跳过表示名称的第一行
data2
mode(data)
names(data)
dim(data)
data$Salary
attach(data)
Salary
detach(data)
Salary


#2.1.3
data.fwf=read.fwf("d:/data/fwf.txt",widths=c(2,4,4,3),col.names=c("W","X","Y","Z"))
data.fwf


#2.1.4
data.excel=read.delim("clipboard")  #clipboard即剪切板
mode(data.excel);dim(data.excel)

install.packages("RODBC")
library(RODBC)
channel=odbcConnectExcel2007("d:/data/Salary.xlsx")  #获取Excel连接
sqlTables(channel)  #列出excel中的表格
#获取Sheet1中的数据，可以使用如下的任意一种方式
data.excel2=sqlFetch(channel,"Sheet1")  #直接获取
data.excel2=sqlQuery(channel,"select*from[Sheet1$]")  #使用SQL语句获取
close(channel)  #关闭ODBC连接，释放空间
mode(data.excel2);dim(data.excel2)


#2.1.5
odbcDataSources()  #查看可用的数据源

#通过RMySQL/DBI读取数据库
library(RMySQL)  #同时也会加载DBI程序包
con=dbConnect(MySQL(),user=”root”,password=”111111”,dbname = "test")  #打开一个MySQL数据库的连接
table.names=dbListTables(con)  # 数据库中的表名存入table.names，方便查看
field.names=dbListFeilds(con,”students”)  # 列出表students中的字段
dbReadTable(con,”students”)  #获得并列出整个表
dbSendQuery(con, “SET NAMES gbk”)  #传送查询，说明用什么字符集来获取数据库字段，gbk或utf8要与之前设置的保持一致。
query=dbSendQuery(con, “select * from students order by age”)
fetch(query)  #显示以年龄排序的查询结果
dbRemoveTable(con,”students”)  # 删除表（删除成功后显示逻辑值TRUE）
dbDisconnect(con)  # 关闭连接

#通过RJDBC读取数据库
library(RJDBC)
help(JDBC)
drv=JDBC("com.mysql.jdbc.Driver",  "/etc/jdbc/mysql-connector-java-3.1.14-bin.jar", "`")
conn=dbConnect(drv, "jdbc:mysql://localhost/test")
dbListTables(conn)  #列出数据库中的表
dbGetQuery(conn, "select count(*) from iris")  #执行查询


#2.1.6
install.packages("XML")  #安装解析XML的包
library(XML)
baseURL="http://data.eastmoney.com/center/stock.html"  #存入网址
table=readHTMLTable(baseURL,header=TRUE,which=1)
mode(table);dim(table)   #查看table的类型和数据维度
head(table,2)   #查看列表table前两行的数据
names(table)=c("类别","成交量","成交金额","总市值","流通市值","上市公司","平均市盈率")  #给变量名重新赋值
table$类别=c("沪市","深市","中小板","创业板")   #给第一个变量“类别”重新赋值
head(table,2)

u="http://www.basketball-reference.com/players/j/jamesle01.html"
James=readHTMLTable(readLines(u), which=3, header=TRUE)
dim(James)
James[1:5,1:10]  #查看前5年的数据，取其中前10个变量


#2.1.7
load("d:/data/salary.Rdata")
head(data,5)  #显示数据框前5行的记录


#2.1.8
library(foreign)
data.spss=read.spss("d:/data/salary2.sav",to.data.frame=T)  #data.spss读入后为数据框变量
dim(data.spss)

library(Hmisc)
data.spss2= spss.get(,"d:/data/salary.sav")

company=read.xport("d:/data/company.xpt")
head(company)
company
company1=sasxport.get("d:/data/company.xpt")


#2.2.1
cat(c("AB", "C"), c("E", "F"), "n", sep="")  #在屏幕上输出
i=1:5
cat("i = ", i, "n",sep=",")  #以逗号为分隔符

cat(c("AB", "C"), c("E", "F"),file="d:/data/cat.txt",sep=".")  #向指定文件写入数据
readLines("d:/data/cat.txt")  #以行的形式读取文本

i=1:5
cat(i,file="d:/data/cat.txt",append=TRUE)
readLines("d:/data/cat.txt")

a=file("d:/data/cat.txt")
cat("1 2 3 4 ","2 3 5 7","11 13 15 17", file=a, sep="\n")  #分隔符sep="\n"表示换行
read.table(a)


#2.2.2
write.csv(data,file="d:/data/salary1.csv",row.names=F,quote=F)
data.csv=read.csv("d:/data/salary1.csv")  #保存文本文件
dim(data.csv)


#2.2.3
save(data,file="d:/data/salary1.Rdata")  #保存R格式文件
load("d:/data/salary.Rdata")


#2.2.4
library(foreign)
write.foreign(data,datafile="d:/data/salary.sav",codefile="d:/data/code.txt",package="SPSS")  #保存为sav文件格式