#functions
dir_t <- list(desk="C:/Users/Franc/Desktop",
  f2017="C:/Users/Franc/Desktop/2017",
  work="C:/App/Rproject/Imp.1/Adv1")
dir.create(dir_t$desk)
filenames <- c("Coding","Reading","Report","Course","Regular","Material")
i=1:6
for (i in 1:length(filenames)){
  dirnames <- paste(dir_t$f2017,sort(filenames),sep="/")
  dir.create(dirnames[i])
}
gg()
help("if")
dir(dir_t$desk)
dir(paste(dir_t$desk,"Report/研究报告",sep="/"))
fileNames <- dir(paste(dir_t$desk,"Report/研究报告",sep="/"))
file.copy(paste(paste(dir_t$des,"Report/研究报告",sep="/"),
                fileNames,sep="/"),dir_t$f2017)

#base包中的所有函数列表
objs <- mget(ls("package:base"),inherits = TRUE)
count(objs)
funs <- Filter(is.function,objs)
funs
unlist(funs)
library(pryr)
library(stats4)
getGenerics()

##S4
setClass("Person",
         slots=list(name="character",age="numeric"))
setClass("Employee",
         slots=list(boss="Person"),
         contains="Person")
alice <- new("Person",name="Alice",age=40)
john <- new("Employee",name="John",age=20,boss=alice)
alice@age
alice
isS4(alice)
slot(john,"boss")
setClass("RangeNumeric",
         contains = "numeric",
         slots=list(min="numeric",max="numeric"))
rn <- new("RangeNumeric",1:10,min=1,max=10)
#rn@max
#setGeneric()创建一个新的泛型函数或将现有函数转换成一个泛型函数，
#setMethod()则需要将泛型函数的名称、方法应该关联的类别以及一个实现该函数的方法
#转换原有函数
getGeneric
setGeneric("union")
x <- data.frame(a=1:3,b=letters[1:3])
y <- data.frame(a=1:3,b=letters[4:6])
union(x,y)
setMethod("union",c(x="data.frame",y="data.frame"),
          function(x,y){
            unique(rbind(x,y))
          })
union(x,y)
union
#创建新的泛型函数standardGeneric()
setGeneric("myGeneric",function(x){
  standardGeneric("myGeneric")
})
#引用类reference class
#模拟银行账户
setRefClass
Account <- setRefClass("Account")
Account$new()
Account <- setRefClass("Account",fields = list(balance="numeric"))
a <- Account$new(balance=1000000)
a$balance;a$balance <- 2000;a
c <- a$copy();c$balance;a$balance <- 3000;c$balance
Acconnt <- setRefClass("Account",
                       fields=list(balance="numeric"),
                       methods=list(
                         withdraw=function(x){
                           balance <<- balance-x
                         },
                         deposit=function(x){
                           balance <<- balance+x
                         }
                       ))
a <- Account$new(balance=100)
a$deposit(250);a;a$withdraw(50);a
NoOverdraft <- setRefClass("NoOverdraft",
                           contains = "Account",
                           methods = list(
                             withdraw=function(x){
                               if(balance < x) stop("Not enough money")
                               balance <<- balance-x
                             }
                           ))
AccountJohn <- NoOverdraft$new(balance=100)
AccountJohn$deposit(50)
AccountJohn$withdraw(200)
pryr::otype(AccountJohn)
NoOverdraft
x %<a-% runif(1)

#函数式编程
set.seed(0997)
df <- data.frame(replicate(6,sample(c(1:10,-99),6,rep=TRUE)))
names(df) <- letters[1:6]
df
fix_missing <- function(x){
  x[x==-99] <- NA
  x
}
fix_missing(df)

df[] <- lapply(df,fix_missing)
missing_fixer <- function(na_value){
  function(x){
    x[x==na_value] <- NA
    x
  }
}
df3 <- df
df3[] <- lapply(as.data.frame(lapply(df,missing_fixer(8))),
                missing_fixer(-99))
summary <- function(x){
  c(mean(x),median(x),sd(x),mad(x),IQR(x))
}
dim(df)
df[7:11,] <- as.data.frame(lapply(df,summary))
summmary <- function(x){
  funs <- c(mean,median,sd,mad,IQR)
  lapply(funs,function(f)f(x,na.rm=TRUE))
}
lapply(mtcars,function(x)length(unique(x)))
Filter(function(x)!is.numeric(x),mtcars)
integrate(function(x)sin(x)^2,0,pi)
integrate(function(x)x^2-x,0,10)
power <- function(exponent){
  function(x){
    x^exponent
  }
}
square <- power(2)
square
as.list(environment(square))
unenclose(square)
power <- function(exponent){
  print(environment())
  function(x)x^exponent
}
zero <- power(0)
m1 <- moment(1)
m2 <- moment(2)
compute_mean <- list(base=function(x)mean(x),
                     sum=function(x) sum(x)/length(x),
                     manual=function(x){
                       total <- 0
                       n <- length(x)
                       for(i in seq_along(x)){
                         total <- total+x[i]/n
                       }
                       total
                     })
x <- runif(1e5)
x
options(digits = 8)
system.time(compute_mean$manual(x))
lapply(compute_mean,function(f) f(x))
call_fun <- function(f,...)f(...)
lapply(compute_mean,call_fun,x)
lapply(compute_mean,function(f) system.time(f(x)))
simple_tag <- function(tag){
  force(tag)
  function(...){
    paste0("<",tag,">",paste0(...),"</",tag,">")
  }
}
tags <- c("p","b","i")
html <- lapply(setNames(tags,tags),simple_tag)
html$p("This is",html$b("bold"),"text.")
midpoint <- function(f,a,b){
  (b-a)*f((a+b)/2)
}
trapezoid <- function(f,a,b){
  (b-a)/2*(f(a)+f(b))
}
midpoint(sin,0,pi)
trapezoid(sin,0,pi)
midpoint_composite <- function(f,a,b,n=10){
  points <- seq(a,b,length=n+1)
  h <- (b-a)/n
  area <- 0
  for(i in seq_len(n)){
    area <- area+h*f((points[i]+points[i+1])/2)
  }
  area
}
trapezoid_composite <- function(f,a,b,n=10){
  points <- seq(a,b,length=n+1)
  h <- (b-a)/n
  area <- 0
  for(i in seq_len(n)){
    area <- area+h*(f(points[i])+f(points[i+1]))/2
  }
  area
}
midpoint_composite(sin,0,pi,n=100)
trapezoid_composite(sin,0,pi,n=100)
composite <- function(f,a,b,n=10,rule){
  points <- seq(a,b,length=n+1)
  area <- 0
  for(i in seq_len(n)){
    area <- area+rule(f,points[i],points[i+1])
  }
  area
}
composite(sin,0,pi,rule=midpoint)
composite(sin,0,pi,rule=trapezoid)
simpson <- function(f,a,b){
  (b-a)/6*(f(a)+4*f((a+b)/2)+f(b))
}
boole <- function(f,a,b){
  pos <- function(i) a+i*(b-a)/4
  fi <- function(i)f(pos(i))
  (b-a)/90*(7*fi(0)+32*fi(1)+12*fi(2)+32*fi(3)+7*fi(4))
}
composite(sin,0,pi,n=10,rule=simpson)
newton_cotes <- function(coef,open=FALSE){
  n <- length(coef)+open
  function(f,a,b){
    pos <- function(i) a+i*(b-a)/n
    points <- pos(seq.int(0,length(coef)-1))
    (b-a)/sum(coef)*sum(f(points)*coef)
  }
}
boole <- newton_cotes(c(7,32,12,32,7))
milne <- newton_cotes(c(2,-1,2,open=TRUE))
trapezoid <- newton_cotes(c(1,1))
simpton <- newton_cotes(c(1,4,1))
composite(sin,0,pi,n=10,rule=milne)
funs_nc <- list(
  midpoint=function(f,a,b){
    (b-a)*f((a+b)/2)
  },
  trapezoid=function(f,a,b){
    (b-a)/2*(f(a)+f(b))
  },
  simpson=function(f,a,b){
    (b-a)/6*(f(a)+4*f((a+b)/2)+f(b))
  },
  boole=function(f,a,b){
    pos <- function(i) a+i*(b-a)/4
    fi <- function(i)f(pos(i))
    (b-a)/90*(7*fi(0)+32*fi(1)+12*fi(2)+32*fi(3)+7*fi(4))
  }
)
newton_cotes <- function(coef,open=FALSE){
  n <- length(coef)+open
  function(f,a,b){
    pos <- function(i) a+i*(b-a)/n
    points <- pos(seq.int(0,length(coef)-1))
    (b-a)/sum(coef)*sum(f(points)*coef)
  }
}
funs_nc2 <- list(
  midpoint <- newton_cotes(c(0,1)),
  boole <- newton_cotes(c(7,32,12,32,7)),
  milne <- newton_cotes(c(2,-1,2,open=TRUE)),
  trapezoid <- newton_cotes(c(1,0,1)),
  simpton <- newton_cotes(c(1,4,1))
)
composite(sin,0,pi,n=100,funs_nc2[[1]])
composite(sin,0,pi,n=100,funs_nc[[1]])
midpoint_composite(sin,0,pi,n=100)
composite(sin,0,pi,n=100,funs_nc2[[4]])
composite(sin,0,pi,n=100,rule=funs_nc[[2]])
unenclose(funs_nc2[[1]])
unenclose(midpoint_composite)
#泛函
randomise <- function(f)f(runif(1e3))
randomise(mean)
randomise(mean)
l <- replicate(20,runif(sample(1:10,1)),simplify=FALSE)
l
out <- vector("list",length(l))
for(i in seq_along(l)){
  out[[i]] <- length(l[[i]])
}
unlist(out)
unlist(lapply(l,length))
unlist(lapply(mtcars,class))
set.seed(0997)
x <- rcauchy(1000)
trims <- c(0,0.1,0.2,0.5)
unlist(lapply(trims,function(trim)mean(x,trim=trim)))
##循环模式
xs <- runif(1e3)
res <- numeric(length(xs))
for(i in seq_along(xs)){
  res[i] <- sqrt(xs[i])
}
res
for(i in xs)
for(i in seq_along(xs))
for(nm in names(xs))
lapply(xs,function(x){})
lapply(seq_along(xs),function(i){})
lapply(names(xs),function(nm){})
#Map多个输入
xs <- replicate(5,runif(10),simplify=FALSE)
ws <- replicate(5,rpois(10,5)+1,simplify = FALSE)
unlist(lapply(xs,mean))
unlist(lapply(seq_along(xs),function(i){
  weighted.mean(xs[[i]],ws[[i]])
}))
unlist(Map(weighted.mean,xs,ws))
mtmeans <- lapply(mtcars,mean)
mtmeans
mtmeans[] <- Map('/',mtcars,mtmeans)
mtmeans
##移动平均数
rollmean <- function(x,n){
  out <- rep(NA,length(x))
  offset <- trunc(n/2)
  for(i in (offset+1):(length(x)-n+offset-1)){
    out[i] <- mean(x[(i-offset):(i+offset+1)])
  }
  out
}
trunc(5/2)
x <- seq(1,3,length=1e2)+runif(1e2)
x
plot(x)
lines(rollmean(x,5),col="blue",lwd=2)
lines(rollmean(x,10),col="red",lwd=2)
#移动中位数
x <- seq(1,3,1e2)+rt(1e2,df=2)/3
plot(x)
lines(rollmean(x,5),col="red",lwd=2)
rollapply <- function(x,n,f,...){
  out <- rep(NA,length(x))
  offset <- trunc(n/2)
  for(i in (offset+1):(length(x)-n+offset+1)){
    out[i] <- f(x[(i-offset):(i+offset)],...)
  }
  out
}
plot(x)
lines(rollapply(x,10,median),col="red",lwd=3)
#用vapply重写
rollapply2 <-function(x,n,f,...){
  offset <- trunc(n/2)
  locs <- (offset+1):(length(x)-n+offset+1)
  num <- vapply(locs,
                function(i) f(x[(i-offset):(i+offset)],...),
  numeric(1))
  c(rep(NA,offset),num)
}
lapply3 <- function(x,f,...){
  out <- vector("list",length(x))
  for(i in sample(seq_along(x))){
    out[[i]] <- f(x[[i]],...)
  }
  out
}
unlist(lapply(1:10,sqrt))
library(parallel)
unlist(mclapply(1:10,sqrt,mc.cores=4))
boot_df <- function(x)x[sample(nrow(x),rep=10),]
rsquared <- function(mod)summary(mod)$r.square
boot_lm <- function(i){
  rsquared(lm(mpg~wt+disp,data=boot_df(mtcars)))
}
system.time(lapply(1:500,boot_lm))
apply
a <- matrix(1:20,nrow=5)
a1 <- apply(a,1,identity)
identical(a,a1)
x <- matrix(rnorm(20,0,10),nrow=4)
x1 <- sweep(x,1,apply(x,1,min),"-")
x2 <- sweep(x,1,apply(x1,1,max),"/")
#aperm高维数组还原
x  <- array(1:24, 2:4)
xt <- aperm(x, c(2,1,3))
xt2 <- aperm(x,c(1,2,3))
stopifnot(t(xt[,,2]) == x[,,2],
          t(xt[,,3]) == x[,,3],
          t(xt[,,4]) == x[,,4])

UCB <- aperm(UCBAdmissions, c(2,1,3))
UCB[1,,]
summary(UCB)
outer(1:3,1:10,"*")
#tapply
pulse<- round(rnorm(22,70,10/3))+rep(c(0,5),c(10,12))
group <- sample(rep(c("A","B"),c(10,12)),22,rep=TRUE)
tapply(pulse,group,length)
tapply(pulse,group,mean)
split(pulse,group)
str(split(pulse,group))

#plyr
Reduce(`*`,1:5)#函数递归
choose(5,5);
l <- replicate(5,sample(1:10,15,replace=T),simplify=FALSE)
l_2 <- replicate(5,sample(1:10,15,replace=T),simplify=TRUE)
l;l_2
Reduce(intersect,l)
where <- function(f,x){
  vapply(x,f,logical(l))
}
df <- data.frame(x=1:3,y=letters[1:3])
where(is.factor,df)
str(Filter(is.factor,df))
str(Find(is.factor,df))
Position(is.factor,df)

##最优化optim()(Rvmmin包)
#就地修改-递归函数-while循环不适宜泛函
trans <- list(disp=function(x)x*0.01638,
              am=function(x)factor(x,levels=c("auto","manual"))
)              
for(var in names(trans)){
  mtcars[[var]] <- trans[[var]](mtcars[[var]])
}
add <- function(x,y){
  stopifnot(length(x)==1,length(y)==1,
            is.numeric(x),is.numeric(y))
  x+y
}
rm_na <- function(x,y,identity){
  if(is.na(x)&&is.na(y)){
    identity
  } else if (is.na(x)){
    y
  }else{
      x
    }
}
add <- function(x,y,na.rm=FALSE){
  if(na.rm&&(is.na(x)||is.na(y))) rm_na(x,y,0)else x+y
}
add(10,NA)
add(10,NA,na.rm=T)
r_add <- function(xs,na.rm=T){
  Reduce(function(x,y)add(x,y,na.rm=na.rm),xs)
}
r_add(sample(1:10,6))
sum(sample(1:10,6))
sum(1,3)
sum(replicate(3,sample(1:10,6)))
c_add <- function(xs,na.rm=F){
  Reduce(function(x,y) add(x,y,na.rm=na.rm),xs,accumulate=TRUE)
}
Reduce
#行为函数运算符
download_file <- function(url,...){
  download.file(url,basename(url),...)
}
lapply(urls,download_file)#错误
i <- 1
for(url in urls){
  i <- i+1
  if(i %%10 ==0)cat(".")
  Sys.delay(1)
  download_file(url)
}
lapply(urls,dot_every(10,delay_by(1,download_file)))
delay_by <- function(delay,f){
  function(...){
  }
}
system.time(runif(100))
system.time(delay_by(0.1,runif)(100))
dot_every <- function(n,f){
  i <- 1
  function(...){
    if(i %%n ==0)cat(".")
    i <<- i+1
    f(...)
  }
}
x <- lapply(1:100,runif)
x <- lapply(1:100,dot_every(10,runif))
download <- dot_every(10,delay_by(1,download_file))
download <- dot_every(delay_by(download_file,1),10)
#explanation:delay by 1 second, print a dot every 10 invocations.
#备忘录
library(memoise)
slow_function <- function(x){
  Sys.sleep(1)
  10
}
system.time(slow_function())
fast_function <- memoise(slow_function)
fib <- function(n){
  if(n<2)return(1)
  fib(n-2)+fib(n-1)
}
fib2 <- memoise(function(n){
  if(n<2)return(1)
  fib2(n-2)+fib2(n-1)
})
options(digits=6)
system.time(fib(23))
system.time(fib(24))
system.time(fib2(23))
runifm <- memoise(runif)
runifm(5)
runifm(5)
runif(5)
download <- dot_every(10,memoise(delay_by(1,download_file)))
#捕获函数调用
ignore <- function(...)NULL
tee <- function(f,on_input=ignore,on_output=ignore){
  function(...){
    on_input(...)
    output <- f(...)
    on_output(output)
    output
  }
}
g <- function(x)cos(x)-x
zero <- uniroot(g,c(-5,5))
show_x <- function(x,...)cat(sprintf("%+0.8f",x),"\n")
zero <- uniroot(tee(g,on_input=show_x),c(-5,5))
#函数延迟
funs <- list(mean=mean,sum=sum)
funs_m <- lapply(funs,delay_by,delay=0.1)
funs_m$mean(1:10)
Negate <- function(f){
  force(f)
  function(...)!f(...)
}
(Negate(is.null))(NULL)
compact <- function(x)Filter(Negate(is.null),x)#删除所有空单元
plyr:fairwith()
library(pryr)
partial
f <- function(a)g(a,b=1)
compact <- function(x)Filter(Negate(is.null),x)
Map(function(x,y) f(x,y,zs),xs,ys)
sample2 <- Vectorize(sample,"size",SIMPLIFY=FALSE)
str(sample2(1:5,c(1,1,3)))
splat <- function(f){
  force(f)
  function(args){
    do.call(f,args)
  }
}
x <- c(NA,runif(100),1000)
args <- list(
  list(x),
  list(x,na.rm=TRUE),
  list(x,na.rm=TRUE,trim=0.1)
)
lapply(args,splat(mean))
#函数组合
sapply(mtcars,function(x)length(unique(x)))
compose <- function(f,g){
  function(...)f(g(...))
}
sapply(mtcars,compose(length,unique))
'%o%' <- compose
sapply(mtcars,length%o%unique)
##非标准计算(在指定数据框内运行)quote&eval
x <- seq(0,2*pi,length=200)
sinx <- sin(x)
plot(x,sinx,type="l")
sample_df <- data.frame(a=1:5,b=5:1,c=c(5,3,1,4,1))
sample_df
a <- 10
eval(quote(a),sample_df)
eval(a,sample_df)     
substitute
subset2 <- function(x,condition){
  condition_call <- substitute(condition)
  r <- eval(condition_call,x)
  x[r,]
}
subset2(sample_df,a>=4)
x <- 4;y <- 4;condition <- 4;condition_call <- 4
subset2(sample_df,a==4)
subset2(sample_df,a==condition)
subset3 <- function(x,condition){
  condition_call <- substitute(condition)
  r <- eval(condition_call,x,parent.frame())
  x[r,]
}
x <- 4
subset3(sample_df,a==condition)
subset2a <- function(x,condition){
  condition_call <- substitute(condition)
  env <- list2env(x,parent=parent.frame())
  r <- eval(condition_call,env)
  x[r,]
}
x <- 5
subset2a(sample_df,a==x)
scramble <- function(x)x[sample(nrow(x)),]
subscramble <- function(x,condition){
  scramble(subset2(x,condition))
}

#表达式
z <- quote(x*10)
z;eval(z)
eval(quote( x*10))
ast(y <- x*10)
f <- function(abc=1,def=2,ghi=3){
  list(sys=sys.call(),match=match.call())
}
mod <- lm(mpg~wt,data=mtcars)
update(mod,formula=.~.+cyl)
library(dplyr)
??with_html
with_html(body(
  h1("A Heading",id="first"),
  p("Some text &",b("some bold text.")),
  img(src="myimg.png",width=100,height=100)
))
html <- function(x) structure(x, class = "html")
print.html <- function(x, ...) {
  out <- paste0("<HTML> ", x)
  cat(paste(strwrap(out), collapse = "\n"), "\n", sep = "")
}
print.html
escape <- function(x) UseMethod("escape")
escape.html <- function(x) x
escape.character <- function(x) {
  x <- gsub("&", "&amp;", x)
  x <- gsub("<", "&lt;", x)
  x <- gsub(">", "&gt;", x)
  html(x)
}
escape.list <- function(x) {
  lapply(x, escape)
}
# 现在我们检查一下它能不能工作
escape("This is some text.")
#> <HTML> This is some text.
escape("x > 1 & y < 2")
#> <HTML> x &gt; 1 &amp; y &lt; 2
# 两层转义也没问题
escape(escape("This is some text. 1 > 2"))
#> <HTML> This is some text. 1 &gt; 2
# 我们知道的HTML 文本不会被转义。
escape(html("<hr />"))
#> <HTML> <hr />
p("Some text",b("some bold text"),class="mypara")
named <- function(x){
  if(is.null(names(x)))return(NULL)
  x[names(x)!=""]
}
unnamed <- function(x){
  if(is.null(names(x)))return(x)
  x[names(x)==""]
}
source("dsl-html-attributes.r",local=TRUE)
p <- function(...) {
  args <- list(...)
  attribs <- html_attributes(named(args))
  children <- unlist(escape(unnamed(args)))
  html(paste0(
    "<p", attribs, ">",
    paste(children, collapse = ""),
    "</p>"
  ))
}
to_math <- function(x) {
  expr <- substitute(x)
  eval(expr, latex_env(expr))
}
greek <- c(
  "alpha", "theta", "tau", "beta", "vartheta", "pi", "upsilon",
  "gamma", "gamma", "varpi", "phi", "delta", "kappa", "rho",
  "varphi", "epsilon", "lambda", "varrho", "chi", "varepsilon",
  "mu", "sigma", "psi", "zeta", "nu", "varsigma", "omega", "eta",
  "xi", "Gamma", "Lambda", "Sigma", "Psi", "Delta", "Xi",
  "Upsilon", "Omega", "Theta", "Pi", "Phi")
greek_list <- setNames(paste0("\\", greek), greek)
greek_env <- list2env(as.list(greek_list), parent = emptyenv())
latex_env <- function(expr) {
  greek_env
}
to_math(pi)
library(formatR)
tidy_source(text=c("{if(TRUE)1 else 2;if(FASLE){1+1","##comments","}else 2}"))
