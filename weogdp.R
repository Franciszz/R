#############################################################################
##################Package Required###########################################
#############################################################################
library(rJava)
library(xlsx)
library(magrittr)
library(reshape2)
library(stringr)
library(recharts)
library(RColorBrewer)
rm(list=ls())
#############################################################################
##################Package Required###########################################
#############################################################################
data <- read.xlsx('examples/GDP/weogdp.xlsx',sheetIndex = 1,header=T)%>%
  set_colnames(c('country',2008:2015))
weo <- read.csv('examples/GDP/weogdp.csv',header=T,stringsAsFactors = F)%>%
  `[`(,c(3:7,9:49))%>%
  set_colnames(c('sub','country','sub_ds','unit','scale',1981:2020,'estimates'))
data_copy <- data; weo_copy <- weo
#################Country Name Transformation for Recharts####################
text <- '“Afghanistan”, “Angola”, “Albania”, “United Arab Emirates”, “Argentina”, “Armenia”, “French Southern and Antarctic Lands”, “Australia”, “Austria”, “Azerbaijan”, “Burundi”, “Belgium”, “Benin”, “Burkina Faso”, “Bangladesh”, “Bulgaria”, “The Bahamas”, “Bosnia and Herzegovina”, “Belarus”, “Belize”, “Bermuda”, “Bolivia”, “Brazil”, “Brunei”, “Bhutan”, “Botswana”, “Central African Republic”, “Canada”, “Switzerland”, “Chile”, “China”, “Ivory Coast”, “Cameroon”, “Democratic Republic of the Congo”, “Republic of the Congo”, “Colombia”, “Costa Rica”, “Cuba”, “Northern Cyprus”, “Cyprus”, “Czech Republic”, “Germany”, “Djibouti”, “Denmark”, “Dominican Republic”, “Algeria”, “Ecuador”, “Egypt”, “Eritrea”, “Spain”, “Estonia”, “Ethiopia”, “Finland”, “Fiji”, “Falkland Islands”, “France”, “Gabon”, “United Kingdom”, “Georgia”, “Ghana”, “Guinea”, “Gambia”, “Guinea Bissau”, “Equatorial Guinea”, “Greece”, “Greenland”, “Guatemala”, “French Guiana”, “Guyana”, “Honduras”, “Croatia”, “Haiti”, “Hungary”, “Indonesia”, “India”, “Ireland”, “Iran”, “Iraq”, “Iceland”, “Israel”, “Italy”, “Jamaica”, “Jordan”, “Japan”, “Kazakhstan”, “Kenya”, “Kyrgyzstan”, “Cambodia”, “South Korea”, “Kosovo”, “Kuwait”, “Laos”, “Lebanon”, “Liberia”, “Libya”, “Sri Lanka”, “Lesotho”, “Lithuania”, “Luxembourg”, “Latvia”, “Morocco”, “Moldova”, “Madagascar”, “Mexico”, “Macedonia”, “Mali”, “Myanmar”, “Montenegro”, “Mongolia”, “Mozambique”, “Mauritania”, “Malawi”, “Malaysia”, “Namibia”, “New Caledonia”, “Niger”, “Nigeria”, “Nicaragua”, “Netherlands”, “Norway”, “Nepal”, “New Zealand”, “Oman”, “Pakistan”, “Panama”, “Peru”, “Philippines”, “Papua New Guinea”, “Poland”, “Puerto Rico”, “North Korea”, “Portugal”, “Paraguay”, “Qatar”, “Romania”, “Russia”, “Rwanda”, “Western Sahara”, “Saudi Arabia”, “Sudan”, “South Sudan”, “Senegal”, “Solomon Islands”, “Sierra Leone”, “El Salvador”, “Somaliland”, “Somalia”, “Republic of Serbia”, “Suriname”, “Slovakia”, “Slovenia”, “Sweden”, “Swaziland”, “Syria”, “Chad”, “Togo”, “Thailand”, “Tajikistan”, “Turkmenistan”, “East Timor”, “Trinidad and Tobago”, “Tunisia”, “Turkey”, “United Republic of Tanzania”, “Uganda”, “Ukraine”, “Uruguay”, “United States of America”, “Uzbekistan”, “Venezuela”, “Vietnam”, “Vanuatu”, “West Bank”, “Yemen”, “South Africa”, “Zambia”, “Zimbabwe”'%>%
  str_replace_all(.,'“|”','')%>%str_split(.,',')%>%sapply(.,str_trim)

data$country <- as.character(data$country)
weo$country <- as.character(weo$country)
# invalid_country <-  data$country[!(data$country %in% text)]
# invalid_name <- c('Republic of Congo','United States','Tanzania','Brunei Darussalam',
#                   'Dominica','Guinea-Bissau','Korea','The Gambia','Serbia',
#                   'Kyrgyz Republic','Islamic Republic of Iran','Lao P.D.R.',
#                   'Timor-Leste','FYR Macedonia')
# valid_name <- c('Democratic Republic of the Congo','United States of America',
#                 'United Republic of Tanzania','Brunei','Dominican Republic',
#                 'Guinea Bissau','South Korea','Gambia', 'Republic of Serbia',
#                 'Kyrgyzstan','Iran','Laos','East Timor','Macedonia')
# data_country <- data_copy$country
# weo_country <- weo$Country
# data_country <- str_replace_all(data_country,invalid_name,valid_name)
data[data$country=='Republic of Congo',]$country = 'Democratic Republic of the Congo'
data[data$country=='United States',]$country = 'United States of America'
data[data$country=='Tanzania',]$country = 'United Republic of Tanzania'
data[data$country=='Brunei Darussalam',]$country = 'Brunei'
data[data$country=='Dominica',]$country = 'Dominican Republic'
data[data$country=='Guinea-Bissau',]$country = 'Guinea Bissau'
data[data$country=='Korea',]$country = 'North Korea'
data[data$country=='The Gambia',]$country = 'Gambia'
data[data$country=='Serbia',]$country = 'Republic of Serbia'
data[data$country=='Kyrgyz Republic',]$country = 'Kyrgyzstan'
data[data$country=='Islamic Republic of Iran',]$country = 'Iran'
data[data$country=='Lao P.D.R.',]$country = 'Laos'
data[data$country=='Timor-Leste',]$country = 'East Timor'
data[data$country=='FYR Macedonia',]$country = 'Macedonia'
#################weo country change
weo[weo$country=='Republic of Congo',]$country = 'Democratic Republic of the Congo'
weo[weo$country=='United States',]$country = 'United States of America'
weo[weo$country=='Tanzania',]$country = 'United Republic of Tanzania'
weo[weo$country=='Brunei Darussalam',]$country = 'Brunei'
weo[weo$country=='Dominica',]$country = 'Dominican Republic'
weo[weo$country=='Guinea-Bissau',]$country = 'Guinea Bissau'
weo[weo$country=='Korea',]$country = 'North Korea'
weo[weo$country=='The Gambia',]$country = 'Gambia'
weo[weo$country=='Serbia',]$country = 'Republic of Serbia'
weo[weo$country=='Kyrgyz Republic',]$country = 'Kyrgyzstan'
weo[weo$country=='Islamic Republic of Iran',]$country = 'Iran'
weo[weo$country=='Lao P.D.R.',]$country = 'Laos'
weo[weo$country=='Timor-Leste',]$country = 'East Timor'
weo[weo$country=='FYR Macedonia',]$country = 'Macedonia'
#############################################################################
##################GDP Changes################################################
#############################################################################
subject <- c('NGDP_RPCH','NGDPD','NID_NGDP','LUR','LP')
weo_1 <- weo[weo$country %in% text,]
#apply(weo_1,2,function(x) sum(complete.cases(x)))
weo_2 <- weo_1[weo_1$sub %in% subject,]%>%`[`(,c(1:3,6:45))
weo_gdp <- melt(weo_2[weo_2$sub=='NGDPD',],id.vars=c('country','sub','sub_ds'),
              variable.name='Year',value.name='GDP')
my_col <- brewer.pal(9,'Blues')[2:9]
pt_color <- brewer.pal(9,'GnBu')[5]
bg_color <- brewer.pal(9,'Blues')[1]
my_split <-list(
  list(start=0, end=100, label='0.0e2-1.0e2', color=my_col[1]),
  list(start=100, end=500, label='1.0e2-5.0e2', color=my_col[2]),
  list(start=500, end=1000, label='5.0e2-1.0e3', color=my_col[3]),
  list(start=1000, end=2000, label='1.0e3-2.0e3', color=my_col[4]),
  list(start=2000, end=5000, label='2.0e3-5.0e4', color=my_col[5]),
  list(start=5000, end=10000, label='5.0e3-1.0e4', color=my_col[6]),
  list(start=10000, end=15000, label='1.0e4-1.5e4', color=my_col[7]),
  list(start=15000, end=25000, label='1.5e4-2.0e4', color=my_col[8]))
echartr(weo_gdp,country,GDP,t=Year,type="map_world",
        subtype="move + scale",symbolList='none')%>%
  setSeries(showLegendSymbol=FALSE)%>%
  setDataRange(splitList=my_split,color=rev(my_col),itemGap=2,
               pos=c(8,'center','center')) %>%
  setTimeline(autoPlay=FALSE,playInterval=1000,symbolSize=3,
              label=labelStyle(textStyle=textStyle(color=my_col[8],
                                                   fontFamily='Carbon')),
              lineStyle=lineStyle(color=my_col[8],type='dotted'))%>%
  setLegend(show=FALSE)%>%
  setTitle(title='Global GDP',pos=10)%>%
  setTheme(palette=my_col[5],bgColor=bg_color)
#############################################################################
##################GDP Growth and Investment##################################
#############################################################################
weo_gdp_inv <- weo_2[complete.cases(weo_2),]%>%
  .[.$sub %in% c('NGDP_RPCH','NID_NGDP','NGDPD'),]%>%
  melt(.,id.vars=c('sub','country','sub_ds'),variable.name='Year',value.name='Value')%>%
  dcast(.,country+Year~sub,mean)%>%arrange(Year,NGDP_RPCH)
echartr(weo_gdp_inv,NGDPD,NID_NGDP,t=Year,
        type='bubble',subtype='scale+move')%>%
  setXAxis(min=-10,max=100)%>%
  setYAxis(min=0,max=100)%>%
  setTheme(palette=my_col[5],bgColor=bg_color)
#############################################################################
##################China and America##########################################
#############################################################################
weo_cnus <- weo[weo$country %in% c('China','United State of America'),]

#############################################################################
##################G20########################################################
#############################################################################
G20 <- c('China','Argentina','Australia','Brazil',
         'Canada','France','Germany','India',
         'Italy','Indonesia','Japan',
         'Mexico','Russia','Saudi Arabia','Turkey',
         'South Africa','United Kingdom',
         'United States of America')
brics <- G20[c(1,4,8,13,16)]
developing <- G20[c(10,12,14)]
developed <- G20[c(2,3,5,6,7,9,11,15,17,18)]
weo_g20 <- weo_1[weo_1$country %in% G20 & weo_1$sub %in% subject,]%>%
  cbind(.,Development=rep(NA,90))
weo_g20[weo_g20$country %in% brics,]$Development <- 'Brics'
weo_g20[weo_g20$country %in% developing,]$Development <- 'Developing'
weo_g20[weo_g20$country %in% developed,]$Development <- 'Developed'

g20_melt <-  melt(weo_g20[weo_g20$sub=='NGDPD',c(2,6:45,47)],
                  id.vars=c('country','Development'),
                  variable.name='Year',value.name='GDP')
library(plyr)
library(dplyr)
g20_agg <- ddply(g20_melt,.(Development,Year),summarize,GDP=sum(GDP,na.rm = T))%>%
  set_colnames(c('country','Year','GDP'))
g20_agg$Development <- NA
g20_agg <- g20_agg[,c(1,2,4,3)]
g20_data <- rbind(g20_melt,g20_agg)
g20_data$Type='Test'
g20_data$Development <- as.character(g20_data$Development)
knitr::kable(g20_data)
echartr(b5_data,x=c(country,NA),facet=sub,
        y=index,t=Year,type='treemap')
#############################################################################
##################Brics######################################################
#############################################################################
weo_b5 <- weo_1[weo_1$country %in% brics & weo_1$sub %in% subject,]
b5_data <- melt(weo_b5[weo_b5$sub%in%c('NGDPD','NGDP_RPCH','NID_NGDP','LP'),
                        c(1:2,6:45)],id.vars = c('sub','country'),
                variable.name = 'Year',value.name = 'index')%>%
  dcast(.,country+Year~sub,mean)
b5_data$Year <- as.numeric(as.character(b5_data$Year))
echartr(b5_data,Year,log(NGDPD),series=country,weight=NID_NGDP,type='bubble')%>%
  setSymbols(symbols=rep('circle',5),colors=my_col[1,3,5,6,8])%>%
  setXAxis(min=1980,max=2020)

