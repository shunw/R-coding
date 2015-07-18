#5 ---advanced dataframe

#data created
sNames<-c("John Davis", "Angela Williams", "Bullwinkle Moose", "David Jones", 
          "Janice Markhammer", "Cheryl Cushing", "Reuven Ytzrhak", "Greg Knox", 
          "Joel England", "Mary Rayburn")

Math<-c(502, 600, 412, 358, 495, 512, 410, 625, 573, 522)
Sci<-c(95, 99, 80, 82, 75, 85, 80, 95, 89, 86)
Eng<-c(25, 22, 18, 15, 20, 28, 15, 30, 27, 18)

roster<-data.frame(sNames, Math, Sci, Eng, stringsAsFactors = FALSE)

#============================
# MATH FUNCTION
### abs(x)
### sqrt(x)
### ceiling(x) --- 不小于x的最小整数 --- ceiling(3.745)=4
### floor(x) --- 不大于x的最大整数 --- floor(3.745)=3
### trunc(x) ---- 向0的方向截取x中的整数部分---- trunc(5.99)=5
### round(x, digits=n)
### signif(x, digits=n) ---将x舍入为指定的有效数字位数--- signif(3.475, digits=2)=3.5
### log(x, base=n) --- 对x取以n为底的对数
### log(x) 
### log10(x) --- log(x)为自然对数/ log10(x)为常数对数/ log(10)返回值2.3026/ log10(10)返回值为1
### exp(x) --- 指数函数--- exp(2.3026)=10
#============================

#============================
# STAT FUNCTION
###  mean(x)
###  Median(x)
### sd(x)
### var(x)
### mad(x)  --- median absolute deviation--- mad(c(1, 2, 3, 4))=1.48
### quantile(x, probs) --- y<-quantile(x, c(.3, .84)) 求x的30％和84%分位点
### range(x)
### sum(x)
### diff(x, lag=n) ---滞后差分, lag用以指定滞后几项，默认点lag为1--- x<-c(1, 5, 23, 29), diff(x)=c(4, 18, 6)
### min(x)
### max(x)
### scale(x, center=TRUE, scale=TRUE)
#============================


#============================
# PROBABILITY SHORT NAME
### beta --->>> beta分布
### binom --->>> 二项分布
### cauchy --->>> 柯西分布
### chisq --->>> （非中心）卡方分布
### exp --->>> 指数分布
### f --->>> F 分布
### gamma --->>> Gamma 分布
### geom --->>> 几何分布
### hyper --->>> 超几何分布
### lnorm --->>> 对数分布
### logis --->>> logistic 分布
### multinom --->>> 多项分布
### nbinom --->>> 负多项分布
### norm --->>> 正态分布
### pois --->>> 泊松分布
### signrank --->>> Wilcoxon 符号秩分布
### t --->>> t 分布
### unif --->>> 均匀分布
### weibull --->>> weibull分布
### wilcox --->>> wilcoxon秩和分布
#============================


#============================
#NORMAL CHART
#============================
x<-pretty(c(-3, 3), 40)
y<-dnorm(x)
plot(x, y, 
     type="l",
     xlab="Normal Deviate",
     ylab="Density",
     yaxs="i"
     )

z<-pnorm(x)
plot(x, z,
     type="l", 
     xlab="Normal Deviate",
     ylab="Probability",
     yaxs="i"
     )


# d--->>> density 密度函数
# p--->>> distribution function 分布函数
# sample 位于z＝1.96左侧的标准正态曲线下方面积是多少？
pnorm(1.96)

# q --->>> quantile function分位数数函数
# sample  均值为500， 标准差为100的正态分布的.9分位点值是多少
qnorm(.9 mean=500, sd=100) 
#--->>>  628.16

# r --->>> random 生成随机数
#============================
#END ====  NORMAL CHART
#============================

#============================
# STRING RELATED FUNCTIONS
### nchar(x) --->>>计算x中的字符数量
### substr(x, start, stop) --->>> 提取或替换一个字符向量中的子串
### grep(pattern, x, ignore.case=FALSE, fixed=FALSE) 
### sub(pattern, replacement, x, ignore.case=FALSE, fixed=FALSE)
### strsplit(x, split, fixed=FALSE) --->>> 在split处分割字符向量x中的元素
### paste(...,sep="") --->>> 连接字符串，分隔符为sep
### toupper(x) --->>> 大���写转换
### tolower(x) --->>> 小写转换
#============================

#============================
# OTHER FUNCTIONS
### length(x)
### seq(from, to, by) --->>> 生成一个序列
### rep(x, n) --->>>将x重复n次
###cut(x, b=n) --->>> 将连续行变量x分割为有着n个水平的因子
### pretty(x, n) --->>>创建美观的分割点。通过选取n＋1个等间距的取整值，将一个连续型变量x分割为n个区间
### cat(..., file="myfile", append=FALSE) --->>> 连接...中的对象，并将其输出到屏幕上或者文件中
### \n --->>> 新行
### \t --->>> 制表符
### \' --->>> 单引号
### \b --->>> 退格
# sample
name<-"Bob"
cat("Hello", name, "\b.\n", "Isn\'t R", "\t", "GREAT?\n")
#============================

#============================
# APPLE TO  MATRIX/ DATAFRAME
# apply(x, margin, fun, ...)
# margin=1 --->>> 表示行
# margin=2 --->>> 表示列

# lapply/sapply 将函数应用到列表上
#============================
mydata<-matrix(rnorm(30), nrow=6)
apply(mydata, 1, FUN=mean)
apply(mydata, 2, FUN=mean)
apply(mydata, 2, FUN=mean, trim=.2)
# trim 最高和最低20%的值均被忽略
#============================
# END ==== APPLE TO MATRIX/ DATAFRAME
#============================


#============================
# SOLUTION   FOR PROBLEM 1
# 1. 将学生的各科考试成绩组合为单一的成绩衡量指标
# 2. 基于相对名次（前20%， 下20%， 等等）给出从A到F的评分
# 3. 根据学生姓氏和名字的首字母对花名册进行排序
#============================

roster
z
score
z<-scale(roster[, 2:4])
score<-apply(z, 1, mean)
roster<-cbind(roster, score)
y<-quantile(score, c(.8, .6, .4, .2))

roster$grade[score>=y[1]]<-"A"
roster$grade[score<y[1] & score>=y[2]]<-"B"
roster$grade[score<y[2] & score>=y[3]]<-"C"
roster$grade[score<y[3] & score>=y[4]]<-"D"
roster$grade[score<y[4]]<-"F"

name<-strsplit((roster$sNames), " ")
name
lastname<-sapply(name, "[", 2)
firstname<-sapply(name, "[", 1)
roster<-cbind(firstname, lastname, roster[, -1])
roster
roster<-roster[order(lastname, firstname),]

#============================
# END --- SOLUTION   FOR PROBLEM 1
#============================

#============================
# LOOP --- FOR & WHILE
#============================
#SAMPLE
for (i in 1:10) print("Hello")

#SAMPLE
i<-10
while (i>0) {
          print ("Hello");
          i=i-1
}
#============================
# END --- FOR & WHILE
#============================



#============================
# IF CONDITION --- IF/ IF-ELSE/ SWITCH
#============================
#SAMPLE 1-1
grade<-"a"
if (is.character(grade)) 
  grade<-as.factor(grade)
class(grade)
#SAMPLE 1-2
grade<-"a"
if (!is.factor(grade)) grade<-as.factor(grade) else print ("Grade already is a factor")

#SAMPLE 2
# 程序的行为是二元时，或者希望结构的输入和输出均为向量时，请用ifelse
ifelse (roster$score>.5, print("Passed"), print("Failed"))


#============================
# END---  IF CONDITION --- IF/ IF-ELSE/ SWITCH
#============================



#============================
# MY FUNCTION
# myfunction <-function(arg1, arg2, ...){
# statements
# return(object)
# }


# debug functions
# warining() --->>> 生成一条错误提示信息
# message() --->>> 生成一条诊断信息
# stop() --->>> 停止当前表达式的执行并提示错误信息
#============================
# SAMPLE1
mystats<-function(x, parametric=TRUE, print=FALSE) {
        if (parametric) {
                center<-mean(x); spread<-sd(x)
        } else {
                center<-median(x); spread<-mad(x)
        }
        if (print & parametric) {
                cat("Mean=", center, "\n", "SD=", spread, "\n")
        } else if (print & !parametric) {
                cat("Median=", center, "\n", "MAD=", spread, "\n")
        }
        result<-list(center=center, spread=spread)
        return(result)
}

set.seed(1234)
x<-rnorm(500)
y<-mystats(x)
y<-mystats(x, print = TRUE)

y$center
y$spread


# SAMPLE 2
mydate<-function(type="long") {
        switch(type,
               long=format(Sys.time(), "%A %B %d %Y"),
               short=format(Sys.time(),  "%m-%d-%y"),
               cat (type, "is not a recognized type\n")
               )
}

mydate()
mydate("long")
mydate("short")
mydate("abc")
#============================
#  END --- MY FUNCTION
#============================

#============================
# 转置
#============================
cars<-mtcars[1:5, 1:4]
cars
t(cars)
#============================
# END --- 转置
#============================


#============================
# 整合
# aggregate ()
# by中的变量必须在一个列表中（即使只有一个变量）
#============================
options(digits = 3)
mtcars
attach(mtcars)
aggdate<-aggregate(mtcars, by=list(cyl, gear), FUN=mean, na.rm=TRUE)
aggdate
detach(mtcars)
#============================
# END --- 整合
#============================


#============================
# RESHAPE
#============================
library(reshape2)

ID<-c(1, 1, 2, 2)
Time<-c(1, 2, 1, 2)
X1<-c(5,3, 6, 2)
X2<-c(6, 5, 1, 4)
mydata<-data.frame(ID, Time, X1, X2)
mydata


# 融合
# 注意必须指定可以唯一确定每个测量所需的变量 --->>> 这里是ID和Time
md<-melt(mydata, id=c("ID", "Time"))
md

#重铸
# newdata<-cast(md, formula, FUN)
# formula描述了想要的最后的结果； FUN是（可选的）数据整合函数
#samples with FUN
dcast(md, ID~variable, mean)
dcast(md, ID~Time, mean)
dcast(md, Time~variable, mean)

#samples wo FUN
dcast(md, ID+Time~variable)
dcast(md, ID+variable ~ Time)
dcast(md, ID~variable+Time)

#============================
# END --- RESHAPE
#============================
