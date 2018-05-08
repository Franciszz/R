R_LIBS_USER="~/R"

options(repos = c(CRAN = "http://mirrors.ustc.edu.cn/CRAN/",
                  CRANextra = "https://mirrors.tuna.tsinghua.edu.cn/CRAN/"))
#用户目录
normalizePath("~")
list.files("~",all.files=T)
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
#下载最新版本R
#http://cran.r-project.org/bin/windows/base/release.htm
#更新R
library(installr)
library(stringr)
install.R()
