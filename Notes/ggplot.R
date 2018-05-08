# -*- coding:utf-8 -*-
#A GGPLOT CASE-2017 CHINESE STOCK MARKET
#=====读取数据=========================
#A股数据
source('mytheme.R')
library(xlsx)
stock <- read.xlsx("examples/stock2017.xlsx",sheetName="2017",header=T,
                   encoding='UTF-8',check.names = FALSE)
library(magrittr);library(lubridate)
stock <- stock[year(stock$ipo)< 2018,][1:3467,]  %>%  subset(.,mv17>0&mv18>0)
stock$mv17_d <- cut(stock$mv17,breaks=c(0,50,100,200,500,1000,16000),
                    labels=c('0-50','50-100','100-200','200-500','500-1000','>1000'),
                    right=FALSE,include.lowest=TRUE)
stock$change_d <- cut(stock$change,breaks=c(-100,-20,0,20,50,400),
                      labels=c('<-20%','-20~0%','0~20%','20~50%','>50%'),
                      right=FALSE,include.lowest=TRUE)
stock$board<- factor(stock$board,levels = c('主板','中小企业板','创业板'),
                     labels=c('主板','中小板','创业板'),ordered=T)
#上证50成分
sz50 <-  read.xlsx('examples/SZ50.xlsx',sheetName="SZ50",header=T,encoding='UTF-8',
                   check.names = FALSE)  %>%  set_colnames(c('code','company','control'))
stock_sz50 <- subset(stock,company %in% sz50$company)
#中国地图
library(maptools)
library(rgdal) 
map_data_china <- rgdal::readOGR('DataWarehouse-master/Rstudy/CHN_adm/bou2_4p.shp')
map_province <- map_data_china@data  %>%  data.frame(.,id=seq(0:924)-1)#省份信息
library(ggplot2)
map_china <- fortify(map_data_china)  %>%  plyr::join(.,map_province,type='full')#地图转化为数据框
#省会信息
map_city_data <- read.csv('DataWarehouse-master/Rstudy/CHN_adm/chinaprovincecity.csv') %>% 
  set_colnames(c('NAME','city','long','lat','index','class'))
# CHN_adm <- rgdal::readOGR("DataWarehouse-master/Rstudy/CHN_adm/CHN_adm2.shp",encoding = 'gbk')
# map_data_c2 <- readShapePoly('DataWarehouse-master/Rstudy/CHN_adm/bou2_4p.shp') 
library(plyr)
stock_province_mv <- ddply(stock,.(province),summarize,mv_total=sum(mv18)) %>% 
  set_colnames(c('NAME','mv_total'))
stock_count <- data.frame(table(stock$province))  %>%  set_colnames(c('NAME','count'))  %>%  
  plyr::join(stock_province_mv,.,by='NAME')
library(stringr)
map_china$NAME <- map_china$NAME %>% as.character(.) %>% str_sub(.,1,2)  %>%  
  str_replace(.,'黑龙','黑龙江') %>% str_replace(.,'内蒙','内蒙古') %>% as.factor(.)
stock_count$NAME <- stock_count$NAME %>% as.character(.) %>% str_sub(.,1,2) %>% 
  str_replace(.,'黑龙','黑龙江') %>% str_replace(.,'内蒙','内蒙古') %>% as.factor(.)
map_city_data$NAME <- map_city_data$NAME %>% as.character(.) %>% str_sub(.,1,2) %>% 
  str_replace(.,'黑龙','黑龙江') %>% str_replace(.,'内蒙','内蒙古') %>% as.factor(.)
map_china <- plyr::join(map_china,stock_count,by='NAME',type='full')
map_city <- plyr::join(stock_count,map_city_data[,1:4],by='NAME',type='inner')

#==============城市市值地图=====================
library(ggplot2)
library(ggthemes)

##============标签和企业数量========================

d_map1 <- ggplot(map_china,aes(x=long,y=lat,fill=log(mv_total))) +
  geom_polygon(aes(group=group),colour="grey40")+
  scale_fill_distiller('总市值',breaks=c(7.6,9.21,9.90,10.82,11.51),
                       labels=c('2e3','1e4','2e4','5e4','1e5'),
                       palette = 'Blues',direction = 0)+#指定渐变填充色，可使用RGB
  coord_map("polyconic")+ggtitle('中国上市公司总市值各省份分布')+
  geom_text(data=map_city,aes(x=long,y=lat,label=NAME),size=3,colour='#8B0000',
            fontface='bold',alpha=0.8,check_overlap = T)+
  theme_os(base_size =12,inblank = FALSE)%+replace%
  theme(legend.position = c(0.9,0.5),legend.key.height = unit(0.85,'lines'),
        legend.key.width = unit(0.6,'lines'),
        legend.title.align= 0,legend.direction = 'vertical')
d_map1
d_map2 <- ggplot(map_china,aes(x=long,y=lat,fill=log(mv_total)),colour='white')+
  geom_polygon(aes(group=group),colour='grey40')+
  scale_fill_distiller('总市值',breaks=c(7.6,9.21,9.90,10.82,11.51),
                       labels=c('2e3','1e4','2e4','5e4','1e5'),
                       palette = 'Blues',direction = 0)+
  geom_point(aes(x=long,y=lat,size=count,colour=count),shape=16,data=map_city)+
  scale_colour_gradient('企业数',low='white',high='#D73434')+
  scale_size_area('企业\n数量',max_size=6,guide=FALSE)+
  coord_map("polyconic")+ggtitle('中国上市公司总市值-企业数量各省份分布')+
  theme_os(inblank = F,base_size =11)%+replace%
  theme(legend.position = c(0.9,0.5),legend.key.height = unit(0.8,'lines'),
        legend.key.width = unit(0.6,'lines'),
        legend.title.align= 0,legend.direction = 'vertical')
d_map2
##====================标签与企业市值条形============
library(leaflet)
map_city$count_d <- cut(map_city$count,breaks=c(0,100,200,300,400,500),
                        labels=c('0-100','100-200','200-300','300-400','400-500'),
                        right=FALSE,include.lowest=TRUE)
###=======简单图1
leaflet(map_city)%>% addTiles()%>%addMarkers(lng=~long,lat=~lat,popup = ~city)
###=======简单图2
#制作配色板colorNumeric,colorBin,colorQuantile,colorFactor,从连续到离散映射到颜色
pal <- colorFactor(brewer.pal(6,'Set1')[5:1],domain = map_city$count_d)
library(RColorBrewer)
d_map3 <- leaflet(map_city)%>% addTiles()%>%
  addCircles(lng=~long,lat=~lat,color=~pal(count_d),weight=~0.1*count,popup = ~city)%>%
addLegend('bottomright',pal=pal,values=~count_d,title='企业数',opacity=1)
d_map3
plot(map_city$count)
# leaflet(map_city)%>% addTiles()%>%
#   addCircleMarkers(lng=~long,lat=~lat,color=~count,fill=TRUE,
#                    radius=~mv_total*0.0001,popup = ~city)
pal_c <- colorNumeric('Blues',map_city$mv_total)
d_map4 <- leaflet(map_city)%>%addTiles()%>%
  addPolygons(lng=~long,lat=~lat,color=~pal_c(mv_total),fillOpacity = 0.9,popup=~NAME)%>%
  addLegend('bottomright',pal=pal_c,values=~mv_total,title='总市值')
d_map4
leaflet(map_data_china)%>%addTiles()%>%
  addPolygons(popup=~map_data_china@data,fillColor = topo.colors(10,alpha=NULL),stroke=FALSE)
map_data_china@data$NAME <- map_data_china@data$NAME %>% as.character(.) %>% str_sub(.,1,2) %>% 
  str_replace(.,'黑龙','黑龙江') %>% str_replace(.,'内蒙','内蒙古') %>% as.factor(.)
map_data_china@data <- plyr::join(map_data_china@data,stock_count,by='NAME')
pal <- colorNumeric('Blues',map_data_china@data$mv_total)
leaflet(map_data_china)%>%addTiles()%>%
  setView(116.387021,39.969369,zoom=3)%>%
  addPolygons(fillColor=~pal(map_data_china@data$mv_total),fillOpacity=0.8,
              color="#000000",weight=1,popup=~map_data_china@data$NAME)%>%
  addLegend(pal=pal,values=map_data_china@data$mv_total,position="bottomright",
            title='全国各省份市值分布')
#================市场市值-涨跌分布=========
##=============市场热力图========================================
###全市热力图
d_1 <- ggplot(stock,aes(x=mv17,y=change))+geom_point(aes(color=change),size=1)+
  geom_hline(yintercept = 0,size=0.5,color='grey')+
  labs(title='2017A股市值-涨跌幅分布图',x='2017市值/亿',y='涨跌幅/%',color='市值')+
  scale_color_gradient2(low='#006837',mid = '#F0FFFF',high='#A50026')+
  theme_os(base_size = 12)+
  scale_x_log10(breaks=c(0,50,100,200,1000,2000,5000))+ylim(-100,200)+
  stat_density2d(color='grey')
d_1
###创业板热力图
d_1 %+% subset(stock,board=='创业板')+
  annotate('text',x=1000,y=100,label='创业板')
##============行业树状图=========================================
library(treemapify)
library(tweenr)
library(gganimate)
library(RColorBrewer)
stock_ind <- ddply(stock,.(w_ind1,w_ind2),summarize,
                   mv_tot17=sum(mv17,na.rm = T),
                   mv_tot18=sum(mv18,na.rm = T),
                   pe_17=weighted.mean(pe17,na.rm = T),
                   pe_18=weighted.mean(pe18,na.rm = T),
                   counts=length(company)) %>%
  set_colnames(c('WindOne','WindTwo','MV2017','MV2018','2017PE','2018PE','Counts'))
d_2 <- ggplot(stock_ind,aes(area=MV2018,label=WindTwo,subgroup=WindOne))+
  geom_treemap(aes(fill=Counts),color='white')+
  geom_treemap_text(fontface='italic',size=12,colour='black',
                    place='topleft',reflow=T,alpha=0.9)+
  geom_treemap_subgroup_border(colour='grey20',size=1)+
  geom_treemap_subgroup_text(size=20,colour="grey20",
                             place="centre",reflow=T,alpha=0.5)+
  scale_fill_distiller('',palette='Blues',direction=1)+guides(fill=FALSE)+
  labs(title='2018上市企业市值行业分布',
       captions='注:格子面积与行业市值正比,颜色深度与行业企业数正比')+
  theme_os()
d_2
##================行业涨跌幅=====================================
st_in2 <- ddply(stock,.(w_ind2),summarize,count=length(company))
stock_ind2 <- ddply(stock,.(w_ind2,change_d),summarize,count2=length(company))%>%
  plyr::join(.,ddply(stock,.(w_ind2),summarize,count=length(company)),
             by='w_ind2',type='full')
stock_ind2$per <- round(stock_ind2$count2/stock_ind2$count*100,2)
library(plyr)
library(tidyr)
library(dplyr)
library(scales)
plyr::arrange(stock_ind2,change_d,-per)$w_ind2[1:24]
d3 <- ggplot(data=stock_ind2,aes(x=w_ind2,y=per,fill=change_d))+
  geom_bar(stat='identity',width=0.95)+
  geom_text(aes(x=w_ind2,y=100,label=w_ind2),size=2.8,
            color='grey6',angle=-90,vjust=0.2,hjust=0)+
  labs(title='2017各行业涨跌分布',fill=NULL)+
  scale_x_discrete(limits=plyr::arrange(stock_ind2,change_d,-per)$w_ind2[1:24])+
  scale_fill_brewer(palette = 'Blues')+
  ylim(0,101)+theme_os(blank = T)%+replace%
  theme(legend.key.width = unit(3,'lines'),
        legend.position = 'top',
        legend.box.spacing = NULL,
        legend.justification = 'bottom')
d3
#==================桑基流量图=========================
library(networkD3)

#==================指数板市================================
##=====================分析仪表盘=
library(sca)
bardata<-seq(from=0,to=270,length=1000)
rectdata<-seq(from=0,to=270,by=27)%>%c(360)
target<- c(0.0800+0.057+0.126,0.067+0.051+0.113,0.04+0.031+0.074)
assist <- target*270
d4 <- ggplot(data=NULL)+
  geom_rect(aes(xmin=rectdata[-12],xmax=rectdata[-1],ymin=5,ymax=10),fill="#F2F2F2",col="white")+
  geom_bar(aes(x=bardata,y=5,col=bardata),stat="identity",fill=NA,size=2)+
  geom_text(aes(x=rectdata[-12],y=10,label=seq(0,100,by=10)),vjust=.5,hjust=.5,size=3,col="#0F1110")+
  geom_segment(aes(x=assist[1],y=-50,xend=assist[1],yend=-10),
               arrow =arrow(length=unit(0.4,"cm")),size=1.2,col="#228B22")+
  geom_point(aes(x=assist[1],y=-50),shape=21,fill="white",col="black",size=5)+
  annotate("text",x=315,y=-30,label=percent(target[1]),
           size=4,hjust=.5,vjust=.5,col=ifelse(target[1]>.5,"#F32626","#38E968"),fontface="plain")+ 
  annotate("text",x=315,y=-15,label="上证",size=4,hjust=.5,vjust=.5)+ 
  ylim(-50,12)+
  coord_polar(theta="x",start=179.85)+
  scale_colour_gradient(low="#38E968",high="#F32626",guide=FALSE)+
  theme_os(inblank=FALSE,base_size = 1)%+replace%theme(aspect.ratio = 1)
d5 <- ggplot(data=NULL)+
  geom_rect(aes(xmin=rectdata[-12],xmax=rectdata[-1],ymin=5,ymax=10),fill="#F2F2F2",col="white")+
  geom_bar(aes(x=bardata,y=5,col=bardata),stat="identity",fill=NA,size=2)+
  geom_text(aes(x=rectdata[-12],y=10,label=seq(0,100,by=10)),vjust=.5,hjust=.5,size=3,col="#0F1110")+
  geom_segment(aes(x=assist[2],y=-50,xend=assist[2],yend=-10),
               arrow =arrow(length=unit(0.4,"cm")),size=1.2,col="green3")+
  geom_point(aes(x=assist[2],y=-50),shape=21,fill="white",col="black",size=5)+
  annotate("text",x=315,y=-30,label=percent(target[2]),
           size=4,hjust=.5,vjust=.5,col=ifelse(target[2]>.5,"#F32626","#38E968"),fontface="plain")+ 
  annotate("text",x=315,y=-15,label="中小板",size=4,hjust=.5,vjust=.5)+ 
  ylim(-50,12)+
  coord_polar(theta="x",start=179.85)+
  scale_colour_gradient(low="#38E968",high="#F32626",guide=FALSE)+
  theme_os(inblank=FALSE,base_size = 1)%+replace%theme(aspect.ratio = 1)
d6 <- ggplot(data=NULL)+
  geom_rect(aes(xmin=rectdata[-12],xmax=rectdata[-1],ymin=5,ymax=10),fill="#F2F2F2",col="white")+
  geom_bar(aes(x=bardata,y=5,col=bardata),stat="identity",fill=NA,size=2)+
  geom_text(aes(x=rectdata[-12],y=10,label=seq(0,100,by=10)),vjust=.5,hjust=.5,size=3,col="#0F1110")+
  geom_segment(aes(x=assist[3],y=-50,xend=assist[3],yend=-10),
               arrow =arrow(length=unit(0.4,"cm")),size=1.2,col="#00FF00")+
  geom_point(aes(x=assist[3],y=-50),shape=21,fill="white",col="black",size=5)+
  annotate("text",x=315,y=-30,label=percent(target[3]),
           size=4,hjust=.5,vjust=.5,col=ifelse(target[3]>.5,"#F32626","#38E968"),fontface="plain")+ 
  annotate("text",x=315,y=-15,label="创业板",size=4,hjust=.5,vjust=.5)+ 
  ylim(-50,12)+
  coord_polar(theta="x",start=179.85)+
  scale_colour_gradient(low="#38E968",high="#F32626",guide=FALSE)+
  theme_os(inblank=FALSE,base_size = 1)%+replace%theme(aspect.ratio = 1)
library(Rmisc)
multiplot(d4,d5,d6,cols=3)
##=====================2017-2018市值变化=======================
stock_mv <- ddply(stock,.(board),summarise,mv17_t=sum(mv17),mv18_t=sum(mv18))%>%
  rbind(.,cbind(board='上证50',ddply(stock_sz50,.(board),summarise,mv17_t=sum(mv17),mv18_t=sum(mv18))[,2:3]))
stock_mv$x2017 <- stock_mv$mv17_t/stock_mv[1,2]*100
stock_mv$x2018 <- stock_mv$mv18_t/stock_mv[1,2]*100
library(reshape2)
stock_mv <- melt(stock_mv,id.vars = c('board','mv17_t','mv18_t'))
stock_mv$ymax <- stock_mv$value*10/max(stock_mv$value) 
stock_mv$xmin <- rep(c(0,8),each=4)
stock_mv$xmax <- rep(c(2,10),each=4)
stock_mv$xlab <- (stock_mv$xmin+stock_mv$xmax)/2
stock_mv <- plyr::arrange(stock_mv,-value)
poly <- cbind(board=rep(as.character(unique(stock_mv$board)),each=4),
              data.frame(matrix(c(8,10,8,3.96,2,3.1684,2,8.996,
                                  2,3.1684,8,3.96,8,2.2757,2,2.2475,
                                  8,2.2757,2,2.2475,2,1.1875,8,0.9946,
                                  2,1.1875,8,0.9946,8,0,2,0),
                                nrow=16,byrow = T)))%>%
  set_colnames(c('board','lat','long'))
d7 <- ggplot()+
  geom_rect(data=stock_mv,aes(xmin=xmin,xmax=xmax,ymin=0,ymax=ymax,fill=board),
            colour='white')+
  geom_text(data=stock_mv,aes(x=xlab,y=ymax-0.5,label=round(value/100*395902/10000,2)),
            size=2.5,colour='white',vjust=0)+
  geom_text(aes(x=c(1,9),y=c(9.3,10.3)),label=c('2017总市值','2018总市值'),size=3)+
  geom_polygon(data=poly,aes(x=lat,y=long,fill=board),color='white')+
  guides(fill=guide_legend(title=NULL))+
  scale_fill_manual(values=c("#0579AD","#0899DA","#519F46","#C7C8CC"))+
  labs(title="2017-2018指数总市值变化:万亿",caption="DataResoure:Wind")+
  theme_os(base_size=12,inblank = FALSE) %+replace%
  theme(legend.position=c(.32,.95))
d7
##=====================分页直方图===========================================================
d8 <- ggplot(stock,aes(x=change,fill=board))+ 
  geom_histogram(aes(y=..density..),bins=100)+facet_grid(board~.,scales='free_y')+
  labs(title='沪深指数涨跌',x='涨跌幅',y='频率')+
  scale_x_continuous(breaks=seq(-100,400,50),expand=c(0.02,0.02))+
  guides(fill=FALSE)+theme_bw()+
  scale_fill_manual(values=c('#023858','#A6BDDB','#0570B0'))
d8
d8_1 <- ggplot(stock_50,aes(x=change),alpha=0.2,size=0.2)+
  labs(title='上证50涨跌')+
  geom_histogram(aes(y=..density..),bins=30,fill='grey50')+
  geom_vline(xintercept = 0,color='white')+
  theme_os()
library(grid)
vp <- viewport(x=0.72,y=0.82,width=0.36,height=0.36)#用viewport函数指定子图的大小的位置
d8
print(d8_1,vp=vp)
##=====================圆环条形图=
#==================年度明星股=============================
##红色版
xs <- seq(16-1.75,16+1.75,length=1000);ys <- sqrt(1.75^2-(xs-16)^2)+9.5
xs2 <- seq(16+1.75,16-1.75,length=1000);ys <- sqrt(1.75^2-(xs2-16)^2)+9.5
poly1 <- data.frame(x=c(xs,xs2),y=c(ys,ys2))

ys_1 <- seq(9.5-1.25,9.5+1.25,length=1000);xs_1 <- sqrt(1.25^2-(ys_1-9.5)^2)+7.8
poly2 <- data.frame(x=xs_1,y=ys_1)
poly3 <- data.frame(x=32-xs_1,y=ys_1)

ys_2 <- seq(9.50-7.05,9.50+7.05,length=1000);xs_2 <- sqrt(7.05^2-(ys_2-9.50)^2)+2
poly4 <- data.frame(x=c(xs_2[1:999],2),y=ys_2)
poly5 <- data.frame(x=32-c(xs_2[1:999],2),y=ys_2)

set.seed(0997)
random <- data.frame(x=sample(5:24,5,replace = F)+2,
                     y=sample(3:15,5,replace=F)+2)%>%
  data.frame(com=c('中科信息','寒锐钴业','江丰电子','方大碳素','华大基因'),type=rep(1,5),.)
random2 <- data.frame(x=sample(16:28,5,replace = F),
                      y=sample(2:17,5,replace=F))%>%
  data.frame(com=c('贵州茅台','中国平安','招商银行','万科A','格力电器'),type=rep(2,5),.)
data <- rbind(random,random2)
data$type <- as.factor(data$type)
# ggplot()+xlim(0,32)+ylim(-1,20)+
#   geom_rect(aes(xmin=0,xmax=32,ymin=-1,ymax=20),fill='#8B1A1A',color='white',size=1.2)+
#   geom_rect(data=NULL,aes(xmin=2,xmax=30,ymin=1,ymax=18),fill='#3A5FCD',color='white',size=1.2)+
#   geom_polygon(data=poly4,aes(x=x,y=y),fill='#3A5FCD',color='white',size=1.2)+
#   geom_polygon(data=poly5,aes(x=x,y=y),fill='#3A5FCD',color='white',size=1.2)+
#   geom_polygon(data=poly1,aes(x=x,y=y),fill='#8B1A1A',color='white',size=1.2)+
#   geom_polygon(data=poly2,aes(x=x,y=y),fill='#3A5FCD',color='white',size=1.2)+
#   geom_polygon(data=poly3,aes(x=x,y=y),fill='#3A5FCD',color='white',size=1.2)+
#   geom_polygon(data=NULL,aes(x=c(2,7.8,7.8,2),y=c(6.5,8.25,10.75,12.5)),
#                fill='#8B1A1A',color='white',size=1.2)+
#   geom_polygon(data=NULL,aes(x=c(30,24.2,24.2,30),y=c(6.5,8.25,10.75,12.5)),
#                fill='#8B1A1A',color='white',size=1.2)+
#   geom_line(data=NULL,aes(x=c(16,16),y=c(1,18)),color='white',size=1.2)+
#   annotate('text',x=3,y=9.5,label='中小创',hjust=0,size=6,color='orange')+
#   annotate('text',x=29,y=9.5,label='大蓝筹',hjust=1,size=6,color='orange')+
#   geom_point(data=data,aes(x=x,y=y,colour=type),size=5)+
#   geom_text(data=data,aes(x=x,y=y,label=com),size=4,color='white',
#             nudge_x = 0.5,nudge_y = 0.5)+
#   geom_point(data=NULL,aes(x=18,y=0),size=6,colour='white')+
#   geom_text(data=NULL,aes(x=20,y=0),color='white',label='CRSC: 进攻犯规！',
#             nudge_x = 1,hjust=0.3)+
#   geom_text(data=NULL,aes(x=16,y=9.5),label='2017',color='grey50',size=36,alpha=0.5)+
#   scale_color_manual(values = c('#FFF68F','grey66'),guide=FALSE)+
#   theme(plot.background = element_blank(),
#         plot.margin = unit(c(0,0,0,0),'points'),
#         panel.background=element_blank(),
#         axis.title.x = element_blank(),axis.title.y = element_blank(),
#         axis.text.x = element_blank(),
#         axis.text.y = element_blank(),
#         axis.ticks = element_blank())
#紫金色
#ys_2 <- seq(9.50-7.05,9.50+7.05,length=1000);xs_2 <- sqrt(7.05^2-(ys_2-9.50)^2)+2.5
poly4 <- data.frame(x=c(2,xs_2[1:999],2.5,2),y=c(9.50-7.05,ys_2,9.50+7.05))
poly5 <- data.frame(x=32-c(2,xs_2[1:999],2.5,2),y=c(9.50-7.05,ys_2,9.50+7.05))

windowsFonts(myfont=windowsFont('New Time Roman'))
ggplot()+xlim(0,32)+ylim(-1,20)+
  geom_rect(aes(xmin=0,xmax=32,ymin=-1,ymax=20),fill='#FFFF00',color='#6959CD',size=1.2)+
  geom_rect(aes(xmin=2,xmax=30,ymin=1,ymax=18),fill='#FFEC8B',color='#6959CD',size=1.2)+
  geom_polygon(data=poly4,aes(x=x,y=y),fill='#FFEC8B',color='#6959CD',size=1.2)+
  geom_polygon(data=poly5,aes(x=x,y=y),fill='#FFEC8B',color='#6959CD',size=1.2)+
  geom_polygon(data=poly1,aes(x=x,y=y),fill='#CD9B1D',color='#6959CD',size=1.2)+
  geom_polygon(data=poly2,aes(x=x,y=y),fill='#FFEC8B',color='#6959CD',size=1.2)+
  geom_polygon(data=poly3,aes(x=x,y=y),fill='#FFEC8B',color='#6959CD',size=1.2)+
  geom_polygon(data=NULL,aes(x=c(2,7.8,7.8,2),y=c(6.5,8.25,10.75,12.5)),
               fill='#6959CD',color='#6959CD',size=1.2)+
  geom_polygon(data=NULL,aes(x=c(30,24.2,24.2,30),y=c(6.5,8.25,10.75,12.5)),
               fill='#6959CD',color='#6959CD',size=1.2)+
  geom_line(data=NULL,aes(x=c(16,16),y=c(1,18)),color='#6959CD',size=1.2)+
  annotate('text',x=3,y=9.5,label='中小创',hjust=0,size=6,color='orange')+
  annotate('text',x=29,y=9.5,label='大蓝筹',hjust=1,size=6,color='orange')+
  geom_point(data=data,aes(x=x,y=y,colour=type),size=5)+
  geom_text(data=data,aes(x=x,y=y,label=com),size=4,color='black',
            nudge_x = 0.5,nudge_y = 0.5)+
  geom_point(data=NULL,aes(x=18,y=0),size=6,colour='grey20')+
  geom_text(data=NULL,aes(x=20,y=0),color='grey20',label='CRSC: Foul！',
            nudge_x = 1,hjust=0.3)+
  geom_text(data=NULL,aes(x=16,y=9.5),label='2017',family='myfont',
            color='#6959CD',size=20,alpha=0.5)+
  scale_color_manual(values = c('#4F94CD','#8B5A2B'),guide=FALSE)+
  theme(plot.background = element_blank(),
        plot.margin = unit(c(0,0,0,0),'points'),
        panel.background=element_blank(),
        axis.title.x = element_blank(),axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank())

#===========colors
library(RColorBrewer)
display.brewer.all(type='all')
brewer.pal(6,'BuGn')




##====================仿第一财经曲线================
stock_l <- subset(stock,company %in% c('中国平安','贵州茅台','万科A','招商银行','京东方A',
                                       '复星医药','科大讯飞','天齐锂业','中国神华'))%>%
  .[,c('company','industry','mv18')] %>%
  rbind(.,c('江丰电子','电子','151.2018'))%>%
  plyr::arrange(.,mv18)
stock_l$industry <- as.character(stock_l$industry);
stock_l$mv18 <- round(sqrt(as.numeric(stock_l$mv18)));
stock_l <- plyr::arrange(stock_l,-mv18)
stock_l$label <- c('中国平安,预测市值将超工行',
                   '飞天茅台,全球最贵酒业,曾破万亿',
                   '招商银行,银行中发展质量最好,银行股先驱',
                   '中国神华,神之派息',
                   '股权争夺激烈,全球市值第一房企',
                   '因为京东?NO,2017科技股代表',
                   '千亿企业,拒绝回调',
                   '人工智能?I am Sorry!',
                   '爱锂不锂,不讲理',
                   '央二代--我爸是央视')
stock_l <- plyr::arrange(stock_l,mv18)
xdd <- c()
ydd <- c()
for(i in 1:10){
  x = seq(0,stock_l$mv18[i],length=1000)
  y = sqrt((0.5*stock_l$mv18[i])^2-(x-0.5*stock_l$mv18[i])^2)
  xdd = c(xdd,x)
  ydd = c(ydd,y)
}
data_p <- data.frame(company=rep(stock_l$company,each=1000),
                     lat=xdd,long=ydd)%>%
  plyr::join(.,stock_l,by='company')

library(ggplot2)
mycolor <- c('white',brewer.pal(9,'Blues'))
data_p <- plyr::arrange(data_p,-mv18)
ggplot()+
  geom_point(data=data_p,aes(x=mv18,y=0,color=industry),size=3)+
  geom_path(data=data_p,aes(x=lat,y=long,color=industry))+
  geom_hline(yintercept=0,color='white')+
  annotate('text',x=stock_l$mv18,y=-1,label=stock_l$label,
           hjust=1,angle=90,size=4,color='white')+
  scale_color_manual(values=mycolor,guide=FALSE)+ylim(-40,60)+
  theme_os(base_color = 'grey50',inblank = F)%+replace%
  theme(aspect.ratio = 0.8,
        plot.title=element_text(size=25,colour="white",hjust=0,
                                lineheight=1.2,margin = margin(b=10)),
        plot.subtitle=element_text(size=15,colour="white",hjust=0,lineheight=1.2),
        plot.caption=element_text(size=10,hjust=0,colour="white"))+
  expand_limits(x=c(0,0),y=c(0,0))+
  labs(title='2017年A股十大代表人物',
       subtitle='你看到了什么？什么？什么？',
       caption='图中横轴按照市值的开方大小排列,线条颜色按照行业分类。')



#===============================REmap==========================================
#=读取数据

# filename <- dir('examples/employment')
# library(magrittr)
# library(stringr)
# filelist <- filename %>% str_extract(.,'[:alpha:]+')
# # for(i in 1:7){
# #   assign(filelist[i],
# #          read.csv2(paste0('examples/employment/',filename[2*i-1]),header = T,encoding='UTF-8'))
# # }#为什么批量导入会乱码，fuck
# filelist <- c('depart','ori','home','labor','abroad','region','company')
# sheetnames <- c('Department','Origins','Home','Labor','Abroad','Region','Company')
# for(i in 1:7){
#   assign(filelist[1],read.xlsx2('examples/employ2017.xlsx',sheetName = sheetnames[i]))
# }
library(rJava);library(xlsxjars)
library(xlsx)
depart <- xlsx::read.xlsx2('examples/employ2017.xlsx',sheetName = 'Department')
write.csv(depart,'examples/employment/department.csv')
company <- xlsx::read.xlsx2('examples/employ2017.xlsx',sheetName = 'Company')
write.csv(company,'examples/employment/company.csv')
abroad <- xlsx::read.xlsx2('examples/employ2017.xlsx',sheetName = 'Abroad')
write.csv(abroad,'examples/employment/abroad.csv')
region <- xlsx::read.xlsx2('examples/employ2017.xlsx',sheetName = 'Region') 
write.csv(region,'examples/employment/region.csv')
labor <- xlsx::read.xlsx2('examples/employ2017.xlsx',sheetName = 'Labor')
write.csv(labor,'examples/employment/labor.csv')
home <- xlsx::read.xlsx2('examples/employ2017.xlsx',sheetName = 'Home')
write.csv(home,'examples/employment/home.csv')
origin <- xlsx::read.xlsx2('examples/employ2017.xlsx',sheetName = 'Origins')
write.csv(origin,'examples/employment/origin.csv')
library(REmap)#https://www.jianshu.com/p/31c9194c95f3
# mapNames()可提供国家、省份、城市列表
mapNames('world')
mapNames('福建')
# REmapCityGeo给出城市列表和经纬度
head(REmapCityGeo)
# get_city_coord()
get_city_coord('厦门')
# get_geo_position()返回一批城市的经纬度数据
region$省市
city_list <- c('厦门','深圳','福州','上海','杭州','广州','北京','南京','济南','成都','武汉')
get_geo_position(city_list)
# get_theme(theme=,lineColor,backgroundColor,titleColor,regionColor,labelShow,pointShow,printColor=)
# markLineControl(),markPointControl()
# markPointControl(symbol='none/circle',#点样式
#                 symbolSize = '',#点大小
#                 effect=T/F,#是否显示动态效果
#                 effectType='scale/bounce',
#                 color='Random')
# markPointControl(symbol = c(),#线两端
#                  symbolSize = ,
#                  smooth=T/F,
#                  smoothness=range(0,1),#平滑度
#                  effect=,color=,
#                  lineWidth=,lineType=solid/dotted/dashed
# )
#=============remap
mdata <- data.frame(origin= rep('厦门',11),destination = as.character(region$省市))
map_1 <- remap(mdata,title='就业流向',
               theme=get_theme(
                 theme='Bright',
                 lineColor='blue',
                 backgroundColor='white',
                 titleColor='blue',
                 labelShow = T,
                 borderColor='skyblue',
                 regionColor='yellow'))
plot(map_1)
##保存图片
getwd()
setwd('examples/employment')
options(remap.js.web=T)
plot(map_1)
setwd('C:/App/Coding/Imp.1/Vasualize')
#=============remapB调取百度地图
# remapB(center=,zoom=,#放大
#        color=,title=,subtitle=,
#        markLineData=,markPointData=,
#        markLineTheme=markLineControl(),
#        markPointTheme=markPointControl(),
#        geoData=
#        )
#就业城市分布
map_2 <- remapB(center=get_geo_position('厦门'),
                zoom=5,color='darkblue',
                title='2017厦门大学就业流向',
                subtitle='主要城市',
                markLineData=mdata,
                markPointData=mdata[,2],
                markLineTheme=markLineControl(),
                markPointTheme=markPointControl(),
                geoData=get_geo_position(unique(union(mdata[,2],mdata[,1]))))
getwd()
setwd('examples/employment')
options(remapB.js.web=T)
plot(map_2)
setwd('C:/App/Coding/Imp.1/Vasualize')
#========remapC热力图==就业生生源地分布
# remapC(data=,maptype='',#地图设置为world,china,省份
#        color=,theme=,title,subtitle,
#        markLineTheme,markPointTheme=,
#        geoData=,mindata=,maxdata#颜色上下限)
m_data2 <- data.frame(province=as.character(origin[,1]),
                      value=as.numeric(origin[,2]))
origin2 <- origin
library(magrittr)
library(stringr)
origin2$省份 <- origin2$省份 %>% as.character(.) %>% str_sub(.,1,2) %>% 
  str_replace(.,'黑龙','黑龙江') %>% str_replace(.,'内蒙','内蒙古') %>% as.factor(.)
origin2$毕业生 <- as.numeric(as.character(origin2$毕业生))
map_3 <- remapC(data=origin2[,c(1,2)],
                maptype='china',
                color=c('blue'),
                title='2017厦门大学就业生生源',
                theme=get_theme('Sky'))
setwd('examples/employment')
options(remapC.js.web=T)
plot(map_3)
setwd('C:/App/Coding/Imp.1/Vasualize')
ori <- origin2[,1]
des <- rep('厦门',32)
labelper<-origin2[order(origin2[,'毕业生'],decreasing=T),][1:10,]
line<-data.frame(ori,des)
map_4 <- remapC(origin2[2:32,],maptype='china',title='毕业生生源地',theme=get_theme('Dark'),
                color=c("#CD0000","#FFEC8B"),markLineData = line,
                markLineTheme = markLineControl(
                  color='white',lineWidth = 2,lineType = 'dashed'),
                markPointData = line[,1],
                markPointTheme = markPointControl(
                  symbolSize = 13,effect=T,effectType = 'scale',color='white')
)
setwd('examples/employment')
options(remapC.js.web=T)
plot(map_4)
setwd('C:/App/Coding/Imp.1/Vasualize')
province <- mapNames("china")
values <- c(133,34,122,52,201,94,109,131,159,300,
            143,208,85,230,317,131,435,291,523,
            128,176,362,2194,352,251,115,59,46,
            1,68,59,32,0,0)
m_data3 <- data.frame(province=province,values)
m_data3
#========remapH热力图==就业生生源地分布
# remapH(data=,maptype=,theme=,
#        blurSize = ,#泛化效果
#        color=,minAlpha = ,opactity=#透明度)
#===============================leaflet==========================================


#===============================Recharts===============
###基本函数echartr()
echartr#基本函数
setSymbols#设置图形标度
setSeries#修改数据系列的定义
addMarkline#标注线markLine标注点markPoint
setTitle/setLegend/setToolbox#verpos
setDataZoom#缩放
setXAis/setYAxis
setTheme
enquote()
mat <- as.data.frame(rbind(
  c(11975,  5871, 8916, 2868),
  c( 1951, 10048, 2060, 6171),
  c( 8010, 16145, 8090, 8045),
  c( 1013,   990,  940, 6907)
))
names(mat) <- c("group1", "group2", "group3", "group4")
mat$name <- names(mat)
echartr(mat, x=name, y=c(group1, group2, group3, group4), type="chord", 
        subtype='ribbon + asc + descsub + hidelab + scaletext') %>%
  setTitle("测试数据", subtitle="From d3.js", pos=5)->g
enquote(g)
library(recharts)
library(shiny)
app = shinyApp(
  ui = fluidPage(eChartOutput('myChart')),
  server = function(input, output) {
    chart = echartr(data.frame(x = rnorm(100), y = rnorm(100)), x, y)
    output$myChart = renderEChart(chart)
  }
)

if (interactive()) print(app)


####====gif=====
devtools::install_github("leonawicz/mapmate")
## mapmate 专门用于做地图动画的R软件包
library(mapmate)
library(dplyr)
library(purrr)
library(parallel)
library(magrittr)
## 下载数据(API:'http://earthquake.usgs.gov/earthquakes/search/')
start <-'2017-03-01'
end <- '2018-03-01'
mag <- 3.0
url <- paste0('http://earthquake.usgs.gov/fdsnws/event/1/query.csv?starttime=',start,
              '%2000:00:00&endtime=',end,'%2023:59:59&minmagnitude=',mag)
earthquake <- read.table(file=url,fill=T,sep=',',header=T)%>%
  select(lon=longitude,lat=latitude,z=mag)
head(earthquake)
## 将地震制作成一个list
n <- 360
eq <- map(1:n,~mutate(earthquake,frameID=.x))
data(bathymetry)# 海洋深度数据
bath <- map(1:n,~mutate(bathymetry,frameID=.x))
rng_bath <- range(bath[[1]]$z,na.rm=T)
pal_bath <- c('black','steelblue4')
data(borders)
borders <- map(1:n,~mutate(borders,frameID=.x))
## 制作帧
id <- 'frameID'
system('mkdir example')
mclapply(bath,save_map,z.name='z',id=id,n.frames=n,
         col=pal_bath,rotation.axis=0,type='maptiles',
         file='examples/earthquake/bath')         
mclapply(borders,save_map,id=id,n.frames=n,
         col='dodgerblue',rotation.axis=0,type='maplines',
         file='examples/earthquake/border') 
mclapply(eq,save_map,id=id,n.frames=n,
         col='red',rotation.axis=0,type='points',
         file='examples/earthquake/earthquake')
## 制作成一个gif
f1 <- grep('bath',dir('examples/earthquake/'),value=T)
f2 <- grep('border',dir('examples/earthquake/'),value=T)
f3 <- grep('earthquake',dir('examples/earthquake/'),value=T)
for(i in 1:n){
  system(paste0('convert/examples/',f1[i],
                'examples/earthquake/',f2[i],
                'examples/earthquake/',f3[i],
                '-background black -layers merge example/merge_',
                sprintf('%04d',i),'.png'))
  ffmpeg(pattern='examples/merge_%04d.png',output='earthquake.gif',rate = 10,overwrite = T)
}
