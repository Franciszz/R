# -*- coding:utf-8 -*-
#===data.table===============================================================================
##===读取数据需要用fread,默认不转化为因子
library(xlsx)
stock <- read.xlsx("data/stock/stock2017.xlsx",sheetName="2017",header=T,
                   encoding='UTF-8',check.names = FALSE)
library(data.table)
stock_dt <- data.table(stock)
abroad <- fread('data/employ/abroad.csv')
#===row manipulation
library(xlxs)
sz50 <- read.xlsx('data/stock/SZ50.xlsx',sheetName='SZ50',
                      check.names=FALSE,header=T,encoding='UTF-8')
###DT[condition,manipulation,group by]###
str(stock_dt)
##===row mani.
library(magrittr)
library(lubridate)
stock_dt <- stock_dt[1:3467,]%>%.[year(ipo)<2017&mv17>0,]
stock_sz50 <- stock_dt[company %in% sz50$证券简称,]#stock_dt[as.character(sz50$证券简称),on='company']
##===设置主键
setkey(stock_dt,industry)
stock_dt['创业板',on='board']#创业板企业
stock_sz50[order(-change,mv18),][,.(company,change,mv18)]#上证50按照涨跌幅,市值排序
stock_dt[,length(company),industry]#stock_dt[,length(company),.(w_ind1,w_ind2)]/stock_dt[,.N,.(w_ind1,w_ind2)]
##===col mani.
stock_dt[,.(count=length(company),mv_total=sum(mv18),change_av=mean(change)),by=board]
stock_dt[,lapply(.SD,mean),.SDcols=c('change','mv17','mv18'),keyby=.(board,industry)]
##===by
stock_dt[,.(count=length(company),change_mean=mean(change),
            mv17_av=mean(mv17),mv17_total=sum(mv18),pe=mean(pe17,na.rm=T)),
         by=.(w_ind1,w_ind2)]#分类统计
stock_dt[,.(count=length(company),change_mean=mean(change),
            mv17_av=mean(mv17),mv17_total=sum(mv18),pe=mean(pe17,na.rm=T)),
         keyby=.(w_ind1,w_ind2)]#分类统计并排序
##===数据重塑
library(reshape2);library(magrittr);library(data.table)
stock_dt_industry <- stock_dt[,.(count=length(company),change_mean=mean(change),
            mv17_av=mean(mv17),mv17_total=sum(mv18),pe=mean(pe17,na.rm=T)),
         by=.(w_ind1,w_ind2)] %>%
  melt(.,id.vars=c('w_ind1','w_ind2'),measure.vars=c('mv17_av','mv17_total'),
       variable.name='index',value.name='value',variable.factor=T)
#===pply-family================================================================================
##===apply(data,margin,fun,...):input array,output vector
apply(stock[,c('mv17','mv18')],2,mean,na.rm=T)
myfun <- function(x){
  c(max(x,na.rm=T),min(x,na.rm=T))
}
apply(stock[,c('change','mv17','mv18')],2,myfun)
##===lapply:输入和返回向量
set.seed(0997)
l <- replicate(20,runif(sample(1:10,1)),simplify = F);l[1:3]
out <- vector('list',length(l))
formonth, (i in seq_along(l)){
  out[[i]]=length(l[[i]])
};unlist(out)#lapply(l,length)
stock_ind <- data.frame(stock_dt_industry)%>%
  cbind(.,as.data.frame(lapply(.[,3:4],function(x) round(x/sum(x,na.rm=T),4))))
##===sapply:则是lapply的特俗.sapply(x,function,...,simplify=T/F)
##===tapply
tapply(stock$mv18,stock$industry,mean,na.rm=T)
as.data.frame(unlist(tapply(stock$mv18,list(stock$board,stock$industry),mean,na.rm=T)))#output array
with(stock$mv18,by(stock$board,stock$industry),mean,na.rm=T)#output list
aggregate(stock$mv18,list(stock$board,stock$industry),mean,na.rm=T)#output df

#===字符格式化输出==============================================================================
myword <- sample(letters,10,replace = F)
paste(myword,collapse='-');paste(myword,1:10,sep="-")
paste0(myword,collapse = " ");paste0(myword,1:10,sep=" ")#sep
paste(2000:2005,'06',sep="/")
library(stringr)
str_c(myword,collapse = '')
library(sca)#百分比输出函数
percent(1:10/100,d=2,sep='');percent(rnorm(10),d=4)
#which is equivalent to 
sprintf('%4.2f%%',1:10);sprintf('%4.2f%%',100*rnorm(10));sprintf('%4.2f',rnorm(10))
sprintf('%d-%d-%02d',2001,12,1:30);sprintf("有%.1f%%的人评价%s很好",99,'WISER CLUB')

#===因子变量和分类重编码======================================================================
vector <- rep(LETTERS[1:5],6)
plyr::count(vector)
myfactor <- (factor(vector,levels=c('E','D','C','B','A'),
                    labels=c('EEE','DDD','CCC','BBB','AAA'),ordered=T))
library(dplyr)
as.character(as.factor(1:10)) %>% str()
as.numeric(as.factor(1:10)) %>% str()
scale<-runif(100,0,100)
factor1<-cut(scale,breaks=c(0,20,40,60,80,100),
              labels=c("0~20","20~40","40~60","60~80","80~100"),
              include.lowest=TRUE,ordered=TRUE)
qa <- quantile(scale,c(0,0.2,0.4,0.6,0.8,1.0))
(cut(scale,breaks=qa,labels=c("0%~20%","20%~40%","40%~60%","60%~80%","80%~100%"),
     include.lowest=TRUE,ordered=TRUE))

#===数据指引=================================================================================
library(ggplot2)
mpg[mpg$model=="audi"|mpg$manufacturer=="mercury",]
subset(mpg,model=="audi"&manufacturer=="mercury",select=c("model","manufacturer","year"))
library(dplyr)
mpg%>%filter(model=="audi"|manufacturer=="mercury")%>%select(model,manufacturer,year)

#===数据聚合=================================================================================
iris1 <- iris;str(iris1)
iris1 <- transform(iris1,dek=Sepal.Length/Sepal.Width,pek=Petal.Length+Petal.Width)
iris1<-dplyr::mutate(iris1,dek=Sepal.Length+Sepal.Width,jek=sqrt(dek))
##===aggregate专门用于分组聚合
aggregate(Sepal.Length~Species,iris1,mean)
library(dplyr)
iris1 %>% group_by(Species) %>% summarize(means=mean(Sepal.Length))
tapply(iris1$Sepal.Length,iris$Species,sum)
library(plyr)
ddply(iris1,.(Species),summarize,means=mean(Sepal.Length))

#===数据排序=================================================================================
stock[order(stock$change,decreasing=T),][1:5,1:5]
sort(stock$vol,decreasing = T)[1:5]
#arrange
library(dplyr)
stock %>% plyr::arrange(-mv18,-mv17)%>%.[1:5,1:7]

#===数据去重与缺失值处理=======================================================================
##===删除重复
library(dplyr)
stock_dup <- dplyr::distinct(stock);dim(stock_dup)#stock[!duplicated(stock$company),] 
stock_uni <- stock[!complete.cases(stock$mv17),]
##===删除缺失
nrow(na.omit(stock))
sum(complete.cases(stock))#判断行是否完整
stock_comple <- stock[!complete.cases(mydata$A),]

#===字符串合并拆分==============================================================================
##===列表/向量字符串
myyear<-sprintf("20%02d",sample(0:17,10))
mymonth<-sprintf("%02d",sample(0:12,10))
myday<-sprintf("%02d",sample(0:31,10))
full <- paste(myyear,mymonth,myday,sep="-")
full%>% strsplit(.,'-')%>%data.frame%>%t(.)
full%>% stringr::str_split(.,'-')%>%data.frame%>%t(.)
##===数据框
mydata<-data.frame(myyear,mymonth,myday);mydata
library(tidyr);library(dplyr);library(stringr)
mydata1<-unite(mydata,col="datetime",c("myyear","mymonth","myday"),sep="-",
               remove=FALSE);mydata1
mydata2 <- separate(mydata1,'datetime',into=c("myyear1","mymonth1","myday1"),sep="-",
                    remove=FALSE);mydata2

#===描述性统计===============================================================================
mystats <- function(x,na.omit=F){
  if(na.omit)
    x=x[!is.na(x)]
  m=mean(x)
  n=length(x)
  s=sd(x)
  skew <- sum((x-m)^3/s^3)/n
  kurt <- sum((x-m)^4/s^4)/n - 3
  return(c(n=n, mean=m, stdev=s, skew=skew, kurtosis=kurt))
}
sapply(diamonds[,5:7],mystats)
lapply(diamonds[,5:7],mystats)
library(Hmisc)
##===输出变量与观测值个数、缺失值与唯一值个数、均值与分位数，五最大值最小值
Hmisc::describe(diamonds[,5:7])
library(pastecs)
##===计算所有值、空值、缺失值数量,最大值、最小值、值域即总和
pastecs::stat.desc(diamonds[,5:7])
##===分类聚合
aggregate(.~cut,data=diamonds[,c("depth","table","price","cut")],FUN=mean)
by(diamonds[,5:7],diamonds$cut,function(x)sapply(x, mystats))
library("doBy")
library("psych")
summaryBy(mpg+hp+wt~am, data=mtcars, FUN=mystats)
##===列联表
mytable<-with(diamonds,table(cut,color));mytable
mytable<-xtabs(~cut+color,diamonds);mytable
library(gmodels)
##===输出SPSS和SAS风格的二维列联表
with(diamonds,CrossTable(cut,color))
mytable<-xtabs(~cut+color+clarity,data=diamonds);mytable
ftable(mytable)  #使用ftable函数将三维列联表进行矩阵化

#===时间与日期处理========================================================================
Sys.Date();date();Sys.time()
##===as.Date 无时间
mydate <- c('1996-01-01','1995-05-20');class(mydate)
mydata <- as.Date(mydate);class(mydata);mydata
mydate1 <- c('1996/1/1','1995/05/20');class(mydate1)
mydata1 <- as.Date(mydate1);class(mydata1);mydata1
##===只有以上两种格式可自动识别，其他格式需声明
mydate2 <- c('01/01/1996','05/20/1995');class(mydate2)
mydata2 <- as.Date(mydate2,'%m%d%Y');mydata2
mydate3 <- c('01|01|1996','05|20|1995');class(mydate3)
mydata3 <- as.Date(mydate3,'%m|%d|%Y');mydata3
format(mydata3,'%Y')#%Y %y %d/D/m/M %A/a星期/周
format(mydata3,format='%Y年%m月%d日,%A')#输出日期
seq(from=as.Date('1995-05-20'),by='1 week',length=10)
##===POSIXct(POSIXct类将日期/时间值作为1970年1月1日以来的秒数存储)/POSIXlt(POSIXlt类则将其作为一个具有秒、分、小时、日、月、年等元素的列表存储)
mydate4 <- c('1996/01/01','1995/05/20',
             '1996/01/01 05:20',
             '1995/05/20 11:13:13')
as.POSIXlt(mydate4)
as.POSIXct(mydate4)
mydata4 <- as.POSIXlt("01/1月/1996 01:01:36",
                      format="%d/%b/%Y %H:%M:%S")
mydata4
##===years(mydata4)#seconds,minutes,seconds
library(lubridate)
ymd('19960101')
mdy('05201995')

##===MATH========================
demo(persp)
demo(plotmath)
demo(Hershey)
demo(graphics)
windows(7,7)
library(scales)
help(package='scales')
url='https://CRAN.R-project.org/view=Distributions'
## WDI 发展中国家数据库
## stock data: http://finance.yahoo.com
## OEDC: stats.oecd.org(偏发达国家)

##===Tex header==========
#---
  title: "环境的工作方式"
author: "Mentos.Z"
output:
  ioslides_presentation: default
  beamer_presentation:
  colortheme: seahorse
  latex_engine: xelatex
  theme: CambridgeUS
  toc: yes
  slidy_presentation: default
header-includes: #\usepackage{ctex}
#---
---
title: "Recharts"
author: "Evans.Z"
output:
  ioslides_presentation: default
  beamer_presentation:
    latex_engine: xelatex
    theme: cosmo
    toc: yes
  slidy_presentation: default
header-includes: \usepackage{ctex}
---
##====多任务处理与并行运算包=====
library(foreach)
library(doParallel)
## 定义函数 
library(httr)
library(jsonlite)
library(magrittr)
getpdf <-  function(i){
  url<-"https://index.toutiao.com/api/report"
  headers<-c(        
    "Host"="index.toutiao.com", 
    "User-Agent"="Mozilla/5.0 (Windows NT 10.0; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/61.0.3163.79 Safari/537.36"
  )
  payload <-list("page"=1,"size"=12)
  payload[["page"]]=i
  web <-  GET(url,add_headers(.headers=headers),query=payload)
  content <- web %>% content(as='text',encoding = 'utf-8') %>% fromJSON() %>% '[['(9)
}

## 执行多线程函数-%do% 执行向量运算
library(doParallel);library(foreach)
system.time({
  cl = makeCluster(4)
  registerDoParallel(cl)
  mydata1 = foreach(i=1:16,
                    .combine = rbind,
                    .packages = c('httr','jsonlite','magrittr')
  ) %dopar% getpdf(i)
  stopCluster(cl)
})
## 适用%dopar%函数执行多线程运算
system.time({
  cl = makeCluster(4)
  registerDoParallel(cl)
  mydata2 = foreach(i=1:16,.combine = rbind)%do% getpdf(i)
})
library(data.table)
DT::datatable(mydata1)## html table
head(mydata1)
## 使用plyr
system.time({
  mydata3 <- plyr::ldply(1:16,getpdf)
})
identical(mydata1,mydata1)


