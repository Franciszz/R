##===read_html(url_d)%>%html_nodes('//div[@class="indent]//table"')%>%html_table(fill=TRUE)

#登入讲座系统
library("RCurl")
library("XML")
library("dplyr")
library("ggplot2")
library("ggimage")
formurl <- "http://open.xmu.edu.cn/Login?returnUrl=http%3A%2F%2Fopen.xmu.edu.cn%2Foauth2%2Fauthorize%3Fclient_id%3D1085%26response_type%3Dcode"
session <- html_session(formurl)
form <- html_form(session)
form <- form[[1]]
Username <- '15420171151976'
Password <- "family404xmu"
form <- set_values(form,'UserName'=Username,'Password'=Password)
out_url <- submit_form(session,form,submit=NULL)
out_url
class(out_url)
session_2 <- follow_link(out_url,'两院讲座预约系统')
follow_link(session_2,"Reserve this seminar")
getForm()
html_form(session_2)
html_nodes(session_2)
xmlParse(session_2)
#网络教学平台
formurl2 <- "http://open.xmu.edu.cn/oauth2/authorize?client_id=1010&response_type=code"
session2 <- html_session(formurl2) #创建会话
form2 <- html_form(session2) #得到网页内的所有表单，以list形式返回
#str(form)
form2 <- form2[[1]] #提取我们想要的表单
UserName2 <- "15420171151976" #这里填写你自己的学号
Password2 <- "family404xmu" #这里把password替换成你自己的密码
form2 <- set_values(form2,'UserName'=UserName2,'Password'=Password2) #填写表单内容
out_url2 <- submit_form(session2,form2,submit=NULL) #在会话中提交表单，实现表单穿越
out_url2;class(out_url2)
session2_2 <- follow_link(out_url2,'Advanced Econometrics')
session2_2
course.info <- session2_2 %>% html_nodes("ul.section") %>% html_text() #爬取属性为section的ul节点，获取其下面列表的所有文本内容
cat(course.info[2])

#zhihulive
library(httr)
library(jsonlite)
library("httr") 
library("jsonlite")
library("httr")
library("magrittr")
library("plyr")
library("rlist")
#all-pages
mylive <- function(){
  baseurl ='https://api.zhihu.com/lives/special_lists'
  header <- c(
    'Content-Type'='application/json; charset=utf-8', 
    'User-Agent'='Mozilla/5.0 (Windows NT 10.0; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/61.0.3163.79 Safari/537.36',
    'Referer'='https://www.zhihu.com/lives/specials',
    'Cookie' = "f0afde6ea82e2cc678ceee0639bcac65"
  )
  payload<-list(
    'limit'=10,
    'offset'=0,
    'subtype'='special_list'
  )
  i = 0
  myresult <- data.frame()
  while (TRUE){
    ###每次请求offset值偏移10个单位
    payload['offset'] = payload[['offset']] %>% `+`(10*i)
    tryCatch({
      r <- GET(baseurl,add_headers(.headers =header),query =payload, encode="json")
      myresult <-r %>% content(as="text") %>% fromJSON(flatten = TRUE) %>% `[[`(3) %>% rbind(myresult,.)
      cat(sprintf("正在处理第【%d】页！",i),sep = "\n")
    },error = function(e){
      cat(sprintf("第【%d】页抓取失败!",i),sep = "\n")
    })
    ###通过抓包返回值中的状态信息确定是否应该跳出循环
    if ( r %>% content(as="text") %>% fromJSON(flatten = TRUE) %>% `[[`(2) %>% `[[`(1) == TRUE) break     
    Sys.sleep(runif(1,0.5,1.5))
    i = i +1
  }
  cat("all page is OK!!!",sep = "\n")
  return (myresult)     
}
#execute
system.time( myresult <- mylive() )

outdata <- function(id){
  baseurl<-sprintf("https://api.zhihu.com/lives/special_lists/%s/lives",id)
  header <- c(
    'Content-Type'='application/json; charset=utf-8',
    'User-Agent'='Mozilla/5.0 (Windows NT 10.0; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/61.0.3163.79 Safari/537.36',
    'Referer'=sprintf('https://www.zhihu.com/lives/specials/%s',id),
    'Cookie' = Cookie
  )
  payload<-list(
    'limit'=10,
    'offset'=0,
    'subtype'='special_list'
  )
  
  myresult <- data.frame()
  i = 0
  while (TRUE){
    payload['offset'] = payload[['offset']] %>% `+`(10*i)
    tryCatch({
      r <- GET(baseurl,add_headers(.headers =header),query =payload, encode="json")
      myresult <-r %>% content(as="text") %>% fromJSON(flatten = TRUE) %>% `[[`(3) %>% rbind(myresult,.)
    })
    if ( r %>% content(as="text") %>% fromJSON(flatten = TRUE) %>% `[[`(2) %>% `[[`(1) == TRUE) break     
    Sys.sleep(runif(1,0.5,1.5))
    i = i +1
  }
  return (myresult)     
}

fulloutdata <- function(){
  mydatafull <- data.frame()
  i = 1
  for (id in ids){
    tryCatch({
      mydatafull <- outdata(id) %>% rbind(mydatafull,.)
      cat(sprintf("正在处理任务【%s】",id),sep = "\n")
      Sys.sleep(runif(1,0.5,1.5))   
    },error = function(e){
      cat(sprintf("任务【%s】处理失败!",i),sep = "\n")
      i = i +1
    })
  }
  cat("have done!",sep = "\n")
  cat(sprintf("一共有【i】个任务处理失败！",i),sep = "\n")
  return(mydatafull)
}

mydatalast
library("rmongodb")
mongo <- mongo.create(host = "localhost")
bson <- mongo.bson.from.list(mydatalast)
mongo.insert(mongo,"rmongo_test.",mydatalast)
list.save(mydatalast,"D:/R/File/liveinfo.json")