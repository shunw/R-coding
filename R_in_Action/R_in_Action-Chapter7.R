#基本统计分析
#7.2.3-7.3.3 skip

vars<-c("mpg", "hp", "wt")
head(mtcars[vars])

#===========================
# SAPPLE ()
#===========================
mystats<-function(x, na.omit=FALSE) {
          if (na.omit)
                  x<-x[!is.na(x)]
          m<-mean(x)
          n<-length(x)
          s<-sd(x)
          skew<-sum((x-m)^3/s^3)/n
          kurt<-sum((x-m)^4/s^4)/n-3
          return(c(n=n, mean=m, stdev=s, skew=skew, kurtosis=kurt))
          }
sapply(mtcars[vars], mystats)

#===========================
# END --- SAPPLE ()
#===========================


#===========================
# AGGREGET()
# 无法一次返回若干个统计量
#===========================
#sample
aggregate(mtcars[vars], by=list(am=mtcars$am), mean)

#===========================
# BY()
# by(data, INDICES, FUN)
#===========================
dstats<-function(x){
                    m=sapply(x, mean) 
                    s=sapply(x, sd)
                    return(c(mean=m, sd=s))
                    }
by(mtcars[vars], mtcars$am, colMeans)
by(mtcars[vars], mtcars$am, dstats)

#===========================
# END --- BY()
#===========================


#===========================
# RESHAPE
#===========================
library(reshape)
dstats<-function(x) (c(n=length(x), mean=mean(x), sd=sd(x)))
dfm<-melt(mtcars, measure.vars = c("mpg", "hp", "wt"), 
          id.vars = c("am", "cyl"))

cast(dfm, am+cyl+variable~., dstats)
#===========================
# END --- RESHAPE
#===========================

#===========================
# 频数表
#===========================
library(vcd)
# ----------------一维----------------------
#sample 1
mytable<-with(Arthritis, table(Improved))

#sample 2
mytable<-table(Arthritis$Improved)

# --->>>转为比例值
prop.table(mytable)

# ----------------二维----------------------
#sample 1
mytable<-table(Arthritis$Treatment, Arthritis$Improved)
mytable

#sample 2 
mytable<-xtabs(~Treatment+Improved, data=Arthritis)

#--->>> 计算行总和＋行比例
margin.table(mytable, 1)
prop.table(mytable, 1)

#--->>> 计算列总和＋列比例
margin.table(mytable, 2)
prop.table(mytable, 2)

#--->>> 计算各单元格所占比例
prop.table(mytable)

# --->>> 添加边际和
addmargins(mytable)
addmargins(prop.table(mytable))

# --->>>others
addmargins(prop.table(mytable,1), 2)
addmargins(prop.table(mytable,2), 1)

# ----------------多维----------------------
#sample1
mytable<-xtabs(~Treatment+Sex+Improved, data=Arthritis)
mytable
ftable(mytable)

margin.table(mytable, c(1, 3))

ftable(prop.table(mytable, c(1, 2)))

ftable(addmargins(prop.table(mytable, c(1, 2)), 3))
#===========================
# END --- 频数表
#===========================

#===========================
# 独立性检验
#===========================

# -----------------卡方独立性检验------------------
library(vcd)
mytable<-xtabs(~Treatment+Improved, data=Arthritis)
chisq.test(mytable) #治疗情况和改善情况不独立

mytable<-xtabs(~Improved+Sex, data=Arthritis)
chisq.test(mytable) #性别和改善情况独立

mytable
# -------------Fisher精确检验----------------
# 原假设为：边界固定的列联表中行和列事相互独立的。。。
# 可以在任意行列数大于等于2的二维列联表上使用，但不能用于2*2的列联表
mytable<-xtabs(~Treatment+Improved, data=Arthritis)
fisher.test(mytable)
mytable

# -------------Cochran-Mantel-Haenszel检验----------------
# 原假设为：两个名义变量在第三个变量的每一层中都是条件独立的
# 此检验假设不存在三阶互交作用(治疗情况＊改善情况＊性别)
mytable<-xtabs(~Treatment+Improved+Sex, data=Arthritis)
mytable
mantelhaen.test(mytable)
# 结果表明，患者接受的治疗与得到的改善在性别的每一水平下并不独立

#===========================
# END--- 独立性检验
#===========================


#7.2.3-7.3.3 skip

#===========================
# t检验--- 关于变量为连续型妾假设其呈正态分布
# t 检验默认嘉定方差不相等，并使用Welsh的修正自由度。 
# ---------- 可以添加参数var.equal=TRUE以假定方差相等并使用合并方差估计
# 默认的备择假设事双侧的（即均值不相等，但大小的方向不确定）
# ----------可以添加一个参数alternative="less" 或 alternative="greater"来进行有方向的检验
#===========================

# -------------------独立样本的t检验--------------------------
library(MASS)
nrow(UScrime)
mean(UScrime[which(UScrime$So==1), ]$Prob)
t.test(Prob~So, data=UScrime)

# -------------------非独立样本的t检验------------------------
sapply(UScrime[c("U1", "U2")], function(x)(c(mean=mean(x), sd=sd(x))))
with(UScrime, t.test(U1, U2, paired = TRUE))

# -------------------多于两组的情况------------------------
#===========================
# ANOVA 方差分析
#===========================
# aov(formula, data=dataframe)
# ~ 左边为响应变量，右边为解释变量 y~a+b+c
# + 分隔解释变量
# : 表示变量的交互项 y~a+b+a:b
# * 表示所有可能交互项 y~a*b*c == y~a+b+c+a:b+a:c+b:c+a:b:c
# ^ 表示交互项达到某个次数 y~(a+b+c)^2 == y~a+b+c+a:b+a:c+b:c
# . 表示包含除因变量外的所有变量。如果一个数据框包含变量y, a, b, c， 代码y~. == y~a+b+c

# 单因素ANOVA ----- y~A
# 含单个协变量的但因素ANCOVA ----- y~x+A
# 双因素ANOVA ----- y~A*B
# 含两个协变量的双因素ANCOVA -----y~x1+x2+A*B
# 随机化区组 ----- y~B+A (B是区组因子)
# 单因素组内ANOVA ---y~A+Error (subject/A)
# 含单个组内因子(w) 和单个组间因子(B) 的重复测量ANOVA -----y~B*W+Error(Subject/W)

# =============顺序很重要===========
# y~A+B+A:B
# R 默认调用类型 I （序贯型）
# 效应根据表达式中先出现的效应做调整。A不做调整，B根据A调整，A：B交互项根据A和B调整
# 样本大小越不平衡，效应项的顺序对结果的影响越大。
# ----- 一般来说，越基础的效应越需要方在表达式前面。
# -----具体来讲，首先是协变量，然后是主效应，接着是双因素的交互项，再接着是三因素的交互项
# -----一个基本准则：若研究设计不是正交的（即因子和／或协变量相关），一定要谨慎设置效应的顺序

#============================
# -----------单因素方差分析--------------
#============================
library(multcomp)
str(cholesterol)
attach(cholesterol)
table(trt) #各组样本大小
aggregate(response, by=list(trt), FUN=mean)
aggregate(response, by=list(trt), FUN=sd)

fit<-aov(response~trt) #检验组间差异 --- p<.0001说明5种疗法的效果不同
summary(fit)

library(gplots)
plotmeans(response~trt, xlab="Treatment", ylab="Response", 
          main="Mean Plot\nwith 95% CI") #绘制各组均值及其置信区间的图形
detach(cholesterol)

# -------------多重比较---------------
# ----- 告诉你哪种疗法与其他疗法不同
# sample 1
TukeyHSD(fit)
par(las=2) #旋转轴标签
par(mar=c(5, 8, 4, 2)) # 增大左边界的面积
plot(TukeyHSD(fit))

# sample 2 
# glht()函数提供了多重均值比较更为全面的方法， 既适用于线性模型，也适用于广义线性模型
par(las=1)
par(mar=c(5, 4, 6, 2))
tuk<-glht(fit, linfct=mcp(trt="Tukey"))
plot(cld(tuk, level=.05), col="lightgrey")

# -------------评估检验的假设条件-------------
# 单因素方差分析中，我们假设因变量服从正态分布，各组方差相等。
library(car)

# 检验正态性假设
qqPlot(lm(response~trt, data=cholesterol), simulate=TRUE, main="Q-Q Plot", labels=FALSE)

# 检验方差齐性
bartlett.test(response~trt, data=cholesterol)
# p=.97表明五组方差米有明显不同
# 方差齐性分析对离群点非常敏感。可利用car包中的outlierTest()函数来检测离群点
outlierTest(fit)

#============================
# -----------单因素协方差分析--------------
#============================
data(litter, package="multcomp")
attach(litter)
table(dose)
aggregate(weight, by=list(dose), FUN=mean)
fit<-aov(weight~gesttime+dose)
summary(fit) # 怀孕时间和幼崽出生体重相关； 控制怀孕时间，药物剂量与出生体重相关

# 去除协变量效应后的组均值，可适用effects包中的effects函数来计算调整的均值
library(effects)
effect("dose", fit)

head(litter)
contrast<-rbind("no drug vs. drug"=c(3, -1, -1, -1))
summary(glht(fit, linfct=mcp(dose=contrast)))

# -------------评估检验的假设条件-------------
# 和ANOVA相同，需要正态性和同方差性假设；另外，ANCOVA还假定回归斜率相同
# ---------这里假定四个处理组通过怀孕时间来预测出生体重的回归斜率都相同
# ---------模型包含时间＊剂量的交互项时，可对回归斜率的同质性进行检验。交互效应若显著
#----------，则意味着时间和幼崽出生体重间的关系依赖与药物剂量的水平
fit2<-aov(weight~gesttime*dose, data=litter)
summary(fit2)
# ===>>> 可得交互效应补显著，支持了斜率相等的假设。
#------------若假设不成立，可以尝试变换协变量或因变量。
#-----------或使用能对每个斜率独立解释的模型
#-----------或使用不需要假设回归斜率同质性的非参数ANCOVA方法 如sm包中的sm.ancova()

# -------------结果可视化-------------
library(HH)
ancova(weight ~ gesttime + dose, data=litter) #----->>>> cannot show in the graph

# searched in the internet, ggplot to draw the same one
library(ggplot2)
mod<-ancova(weight~gesttime+dose, data=litter)
pred<-predict(mod)
ggplot(data=cbind(litter, pred),
       aes(gesttime, weight, color=dose))+geom_point()+facet_grid(.~dose)+geom_line(aes(y=pred))
ggplot(data=cbind(litter, pred),
       aes(gesttime, weight, color=dose))+geom_point()+geom_line(aes(y=pred))

mod<-ancova(weight~gesttime*dose, data=litter)
pred<-predict(mod)
ggplot(data=cbind(litter, pred),
       aes(gesttime, weight, color=dose))+geom_point()+facet_grid(.~dose)+geom_line(aes(y=pred))
ggplot(data=cbind(litter, pred),
       aes(gesttime, weight, color=dose))+geom_point()+geom_line(aes(y=pred))

#============================
# -----------双因素方差分析--------------
#============================
attach(ToothGrowth)
head(ToothGrowth)
table(supp, dose)
aggregate(len, by=list(supp, dose), FUN=mean)
aggregate(len, by=list(supp, dose), FUN=sd)
fit<-aov(len~supp*dose)
summary(fit)

interaction.plot(dose, supp, len, type="b", col=c("red", "blue"), pch=c(16, 18), 
                 main="Interaction between Dose and Supplement Type")

library(gplots)
plotmeans(len~interaction(supp, dose, sep=" "), 
          connect=list(c(1, 3, 5), c(2, 4, 6)), 
          col=c("red", "darkgreen"), 
          main="Interaction Plot with 95% CIs", 
          xlab="Treatment and Dose Combination")

library(HH)
interaction2wt(len~supp*dose)


#============================
# -----------重复测量方差分析--------------
#============================


#===========================
# END--- ANOVA
#===========================



#===========================
# END --- t检验
#===========================
