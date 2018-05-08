#===深圳二手房信息爬虫==========================================
library(RCurl)
library(magrittr);library(rvest);library(XML)
##读取数据
url_sz='http://sz.centanet.com/ershoufang/'
##构造请求头
sz_header=c(
  'User-Agent'='Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/64.0.3282.140 Safari/537.36',
  'Accept'='text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,image/apng,*/*;q=0.8',
  'Accept-Language'='zh-CN,zh;q=0.9',
  'Connection'='keep-alive'
)
##读取页面
page <- getURL(url_sz,httpheader=sz_header,.encoding='utf-8')%>%
  htmlParse(.,encoding = 'utf-8')%>% xmlRoot(.)
##所有页面url
parsePage <- paste0(url_sz,'g',1:800)#实际总共801
##获取信息
district_name=room_type=room_area=room_price_t=room_price=dis_price=
  room_build=room_direction=room_deca_type=district_region=room_year=room_link=c(rep(NA,19600))
library(raster)
Content <- function(parsePage){
  #解析网页源代码
  doc = getURL(parsePage,.encoding = 'UTF-8')%>%htmlParse(.,encoding = 'UTF-8')%>%xmlRoot(.)
  #获取小区名称
  district_name = xpathSApply(doc,'//div[@class="house-item clearfix"]/div[@class="item-info fl"]/p[@class="house-name"]/a',
                              xmlValue,'title')%>%unlist(.)%>%as.data.frame(.)
  #获取小区地址
  district_region = xpathSApply(doc,'//div[@class="house-item clearfix"]/div[@class="item-info fl"]/p[@class="house-txt"][2]/span',
                                xmlValue)%>%unlist(.)%>%as.data.frame(.)
  #获取楼层建筑
  room_build = xpathSApply(doc,'//div[@class="house-item clearfix"]/div[@class="item-info fl"]/p[@class="house-txt"][1]/span[1]',
                           xmlValue)%>%unlist(.)%>%as.data.frame(.)
  #获取房间朝向
  room_direction = xpathSApply(doc,'//div[@class="house-item clearfix"]/div[@class="item-info fl"]/p[@class="house-txt"][1]/span[2]',
                               xmlValue)%>%unlist(.)%>%as.data.frame(.)
  #获取房间装修类型
  room_deca_type = xpathSApply(doc,'//div[@class="house-item clearfix"]/div[@class="item-info fl"]/p[@class="house-txt"][1]/span[3]',
                               xmlValue)%>%unlist(.)%>%as.data.frame(.)
  #获取房间建造年份
  room_year = xpathSApply(doc,'//div[@class="house-item clearfix"]/div[@class="item-info fl"]/p[@class="house-txt"][1]/span[4]',
                          xmlValue)%>%unlist(.)%>%as.data.frame(.)
  #获取房间总价
  room_price_t = xpathSApply(doc,'//div[@class="house-item clearfix"]/div[@class="item-pricearea fr"]/p[@class="price-nub cRed tc"]/span',
                             xmlValue)%>%unlist(.)%>%as.data.frame(.)
  #获取房间均价
  room_price = xpathSApply(doc,'//div[@class="house-item clearfix"]/div[@class="item-pricearea fr"]/p[@class="price-txt tc"]',
                           xmlValue)%>%unlist(.)%>%as.data.frame(.)
  #获取所在小区均价
  dis_price = xpathSApply(doc,'//div[@class="house-item clearfix"]/div[@class="item-pricearea fr"]/p[@class="price-txtB tc"]',
                          xmlValue)%>%unlist(.)%>%as.data.frame(.)
  room_inf = xpathSApply(doc,'//div[@class="house-item clearfix"]/div[@class="item-info fl"]/p[@class="house-name"]/span',
                         xmlValue)%>%str_replace_all(.,'[|]','')%>%trim(.)
  room_inf = room_inf[which(nchar(room_inf)!=0)]
  #获取房间面积
  room_area = room_inf[seq(2,39200,2)]%>%unlist(.)%>%as.data.frame(.)
  #获取房间类型
  room_type = room_inf[seq(1,39199,2)]%>%unlist(.)%>%as.data.frame(.)
  #获取详细链接
  room_link = xpathSApply(doc,'//div[@class="house-item clearfix"]/div[@class="item-info fl"]/h4[@class="house-title"]/a',xmlGetAttr,'href')%>%
    paste0('http://sz.centanet.com',.)%>%unlist(.)%>%as.data.frame(.)%>%.[1:19600,]
  cbind(district_name,room_type,room_area,room_price_t,room_price,
        dis_price,room_build,room_direction,room_deca_type,
        district_region,room_year,room_link)%>%
    set_colnames(c('小区','类型','面积','总价','均价','小区均价','楼层','朝向',
                   '装修类型','小区地址','建造年份','链接'))
}
data_sz <- cbind(district_name,room_type,room_area,room_price_t,room_price,
                 dis_price,room_build,room_direction,room_deca_type,
                 district_region,room_year,room_link)%>%
  set_colnames(c('小区','类型','面积','总价','均价','小区均价','楼层','朝向',
                 '装修类型','小区地址','建造年份','链接'))
write.csv(data_sz,'scrape/sz_house.csv')


library(magrittr)
astate_sz <- read.csv('scrape/sz_house.csv',encoding='utf-8',stringsAsFactors = FALSE,na.strings = 'NA')%>%.[,-1]
head(astate_sz)
library(stringr)
#===数据简化处理
astate_sz$面积 %<>% str_sub(.,1,nchar(astate_sz$面积)-1) %>% as.numeric
astate_sz$总价%<>% as.numeric
astate_sz$总价[which(astate_sz$总价<5)] <- astate_sz$总价[which(astate_sz$总价<5)]*10000
astate_sz$均价%<>% str_sub(.,1,nchar(astate_sz$均价)-3)%<>%as.numeric
astate_sz$小区均价%<>% str_sub(.,6,nchar(astate_sz$小区均价)-3)%<>%as.numeric
astate_sz$朝向%<>% str_sub(.,1,2)%<>%as.factor
astate_sz$建造年份%<>% str_sub(.,1,4)%<>%as.numeric
astate_sz <- separate(astate_sz,col='小区地址',
                      into=c('小区1','街道','具体地址'),sep='[:space:]|-')
# region_list <- astate_sz$小区地址%>%str_split(.,'[:space:]|-')
# region <- list();region_2 <- list();region_3 <- list()
# for(i in 1:length(region_list)){
#   region[[i]] = unlist(region_list[i])[1]
#   region_2[[i]] = unlist(region_list[i])[2]
#   region_3[[i]] = unlist(region_list[i])[3]
# }

#===数据简单描述
library(ggplot2)
library(dplyr)
source('mytheme.R')
str(astate_sz)
ggplot(astate_sz,aes(x=面积,y=均价))+geom_point(color='steelblue',size=0.1,alpha=0.1)+
  stat_density2d(color='grey20')+
  scale_x_log10(breaks=c(20,50,100,200,500),labels=c(20,50,100,200,500))+
  scale_y_log10(breaks=c(20000,50000,100000,200000,500000),labels=scales::comma)+
  labs(x='房屋面积/平',y='价格/平',title='深圳2018年二手房价格情况')+
  theme_os()
library(plyr)
region <-  ddply(astate_sz,.(小区1),summarize,均价=mean(均价),最高价=max(均价),
      最低价=min(均价),总价=mean(总价),供应=length(小区)) %>% subset(.,供应 > 2)
region <- plyr::arrange(region,均价)
region$order <- 1:9
ggplot(region,aes(x=order,y=均价,group=1))+
  geom_point(size=3,shape=1,color='steelblue')+geom_line(color='grey20')+
  geom_text(aes(label=小区1),nudge_y = 3000,nudge_x=-0.1)+
  labs(x='小区',y='均价/平',title='深圳2018各区二手房均价')+
  theme_os()%+replace% theme(axis.text.x = element_blank(),
                             axis.ticks.x = element_blank())
