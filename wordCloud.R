library(wordcloud2)
data1 <- read.csv("C:\\Users\\Franc\\Desktop\\2017\\Regular\\it-Said 数据之星\\Notes.txt")
wordcloud2(data1,size=1,shape="star")
normalizePath("~")
list.files('~',all.files=T)
#配置R
file.edit('~/.Renviron')
dir.create('~/R')
#.Rprofile文件
file.edit('~/.Rprofile')
R.home("bin")
gpl = readLines(file.path(R.home(), "COPYING"))
head(gpl)
xie <- readline("Answer yea or no:")
nchar(gpl[1:10])
write.csv(iris,file="iris.csv",row.names=FALSE)
#查看文件
normalizePath("iris.csv")
strsplit(gpl[4:5]," ")
words <- unlist(strsplit(gpl,"\\W"))
words <- words[words != ""]
tail(sort(table(tolower(words))),10)
paste(1:3,"a",sep="-",collapse = "+")
xie <- readLines("https://yihui.name",encoding="UTF-8")
xie
gsub("<title>|</title>","",xie[8])
#stringr
library(stringr)
metaChar = c("$","*","+",".","?","[","^","{","|","(","\\")
grep(pattern="$", x=metaChar, value=TRUE)
grep(pattern="\\", x=metaChar, value=TRUE)
grep(pattern="(", x=metaChar, value=TRUE)
gsub(pattern="|", replacement=".", "gsub|uses|regular|expressions")
strsplit(x="strsplit.aslo.uses.regular.expressions", split=".")
states <- rownames(USArrests)
substr(states,1,4)
abbreviate(states,5)
hist(nchar(states),main="Histogram",
     xlab="number of characters in US State names",
     breaks=10)
states[which(nchar(states) ==max(nchar(states)))]
#匹配字符串
grep(pattern="w",x=states,value=T)
grep(pattern="[wW]",states,value=T)
grep(pattern="W",x=states,ignore.case=T,value=T)
#字符统计
library(stringr)
str_count(states,"a")
vowels <- c("a","e","i","o","u")
num_vowels <- vector(mode="integer",length=5)
for(i in seq_along(vowels)){
  num_aux <- str_count(tolower(states),vowels[i])
  num_vowels[i] <- sum(num_aux)
}
#[]
library(stringr)
grep(pattern="[wW]",x=states,value=TRUE)
#\ 对元字符进行转义。以\开头的特殊序列表达字符串组
str_extract_all(string="my credit card number:0109971314",pattern="\\d")
strsplit(x="strsplit.also.uses.regular.expression",split="\.")
strsplit(x="strsplit.also.uses.regular.expression",split="\\.")
#^:匹配字符串的开始。但若将^置于character class的首位表达的意思是取反义。
test_vector <- c("123","345","567")
str_extract_all(test_vector,"^3")
str_extract_all(test_vector,"3")
str_extract_all(test_vector,"[^3]")
#$:匹配字符串的结束
str_extract_all("test_vector","3$")
#$:匹配字符串的结束
str_extract_all(test_vector,"3$")
str_extract_all(test_vector,"[3$]")
#.：匹配换行符意外的任意字符
str_extract_all(string=c("regular.expression\n","\n"),pattern="\\.")
#|:或者
test_vector2<-c("AlphaGo实在厉害！","alphago是啥","阿尔法狗是一条很凶猛的狗。")
#?:前面的字符（组）是可有可无，并且最多匹配一次。
#*:前面的字符（组）将被匹配零次或多次
#+：前面的字符组将被匹配一次或多次
#（）：表示一个字符组，括号内的字符串将作为一个整体被匹配。
str_extract_all(string=c("abc","ac","ab"),pattern="ab?c")
#?:前面的字符（组）是可有可无，并且最多匹配一次。
#*:前面的字符（组）将被匹配零次或多次
#+：前面的字符组将被匹配一次或多次
#（）：表示一个字符组，括号内的字符串将作为一个整体被匹配。
str_extract_all(string=c("abc","ac","bc"),pattern="ab?c")
str_extract_all(string = c("abababab","abc","ac"),pattern = "(ab)*")
str_extract_all(string = c("abababab","abc","ac"),pattern = "(ab)+")
str_extract_all(string = c("ababc","ac","cde"),pattern = "(ab)?c")
str_extract_all(string = c("abc","ac","cde"),pattern = "ab?c")
str_extract_all(string = c("abababab","ababc","ababababc"),
                pattern = "(ab){2,3}")
str_extract_all(string = c("abababab","ababc","ababababc"),
                pattern = "(ab){2,}")
#转义\\
#字符组[:digit:]0-9[:lower:]a-z[:upper:]A-Z[:alpha:]a-zA-Z
#[:alnum:]所有数字及字母[:punct:]标点符号[:graph:]所有字符
#[:blank:]空字符[:space:]Space,tab,newline,space characters
#[print]可打印的字符
str_extract_all(string="my credit card number:0109971314",pattern="\\d")
str_extract_all(string="my credit card number:0109971314",pattern="[:digit:]")
str_extract_all(string="my credit card number:0109971314",pattern="[:alpha:]")
str_extract_all(string="my credit card number:0109971314",pattern="[[:alpha:]]")
#str_dup()丢弃指定位置的字符
#str_extract()=regmatches()提取首个匹配模式的字符
#str_extract_all()=regmatches提取所有匹配模式的字符
#str_locate()=regexpr()返回首个匹配模式字符的位置
#str_locate_all=gregexpr()
#str_replace()=sub()替换
#str_replace_all()=gsub()
#str_split()=strsplit()按照字符模式分割字符串
#str_split_fixed()分割成指定个数
#str_count()返回指定模式出现的位置
gpl = readLines(file.path(R.home(), "COPYING"))
gpl
anim <- readLines("examples/President Xi words.txt",encoding = "UTF-8")
anim[[1]]
library(wordcloud2)
wordcloud2(demoFreq,size=2,fontFamily = "微软雅黑",color="random-light",
           backgroundColor = "grey")
spl <- system.file("examples/spl.jpg",package="wordcloud2")
spl
wordcloud2(demoFreq,size=2,fontFamily = "微软雅黑",color="random-light",
           backgroundColor = "grey",figPath = spl)
letterCloud(demoFreq,size=2,color="random-light",
            backgroundColor = "snow",word="LOVE")
###词云jiebar
#install.packages("jiebaR")
library(jiebaR)
library(jiebaRD)
anim2 <- unlist(anim)
#设置分词引擎
cutter <- worker(stop_word = "examples/stop_words.utf8")
TL_content <- segment(code="examples/President Xi words.txt",cutter)
TL_content
#加载data.table
library(dplyr)
library(stringr)
library(data.table)
tyears <- fread(input=TL_content,
                sep=" ",
                header=F,
                encoding='UTF-8')
tyears <- readLines(con <- file(TL_content,encoding="utf-8"))%>% str_split(.,pattern=" ")
class(tyears);class(tyears2)
tyears2 <- as.matrix(unlist(tyears)) 
mytxt <- read.csv(file="tenyears.segment.2017-12-05_14_16_44.txt",
                  header = F,stringsAsFactors = F,sep=" ",
                  encoding = "UTF-8")
#去除停止词
data <- system.file("dict/stop_words.utf8",package="jiebaRD")
stop_words_list <- readLines(data,encoding = "UTF-8")
stopword <- as.vector(stop_words_list)
segWords <- filter_segment(tyears2,stopword)
#example
anim <- readLines("examples/President Xi words.txt",encoding = "UTF-8")
anim2 <- unlist(anim)
cutter <- worker(stop_word = "examples/stop_words.utf8")
TL_content <- segment(code="examples/President Xi words.txt",cutter)
tyears <- readLines(con <- file(TL_content,encoding="utf-8"))%>% str_split(.,pattern=" ")
tyears <- readLines(con <- file(TL_content,encoding="utf-8"))%>% str_split(.,pattern=" ")
class(tyears);class(tyears2)
tyears2 <- as.matrix(unlist(tyears)) 
#统计词频
ty <- subset(tyears2,nchar(as.character(tyears2))>1)
tyfreq <- table(ty)
tyfreq <- rev(sort(tyfreq))
myfreq <- as.data.frame(tyfreq)
#去掉出现3次以下的词语
myfreq2 <- subset(myfreq,myfreq$Freq >=10& nchar(as.character(myfreq$ty)))
#词云制作
library(RColorBrewer)
library(wordcloud2)
mycolors <- brewer.pal(8,"Dark2")#设置颜色系
windowsFonts(myFont=windowsFont("微软雅黑"))
wordcloud2(myfreq2,size=2,fontFamily = "myFont",color = mycolors )
letterCloud(myfreq2,size=2,fontFamily = "微软雅黑",color=mycolors)
letterCloud(myfreq2,word="文",size = 2,fontFamily = "myFont",color = mycolors)  
