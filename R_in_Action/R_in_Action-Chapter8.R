# 回归

#===============================
# OLS 回归
#===============================

# OLS 回归的假设：
# 正态性： 对于固定的自变量值，因变量值成正态分布
# 独立性：Yi值之间相互独立
# 线性：因变量与自变量之间为线性关系
# 同方差性：因变量的方差不随自变量的水平不同而变化。也可称作不变方差。
# OLS回归还假设自变量是固定的且测量无误差，但在实践中通常都放松了这个假设

# OLS 常用的符号
# ~ ===>>> 分隔符，左边为响应量，右边为解释变量
# + ===>>>分隔预测变量
# : ===>>>表示预测变量的交互项。y~x+z+x:z
# * ===>>>表示所有可能交互项的简洁方式。 y~x*z*w == y~x+z+w+x:z+x:w+z:w+x:z:w
# ^ ===>>>表示交互项达到某个次数 y~(x+z+w)^2 ==y~x+z+w+x:z+x:w+z:w
# . ===>>>表示包含除因变量意外的所有变量 y~. ==y~x+z+w
# - ===>>> 表示从等式中移除某个变量。y~(x+z+w)^2-x:w == y~x+z+w+x:z+z:w
# -1 ===>>>删除截距项。 y~x-1 拟合y在x上的回归，并强制直线通过原点
# I( ) ===>>> 从算数的角度来解释括号中的元素。y~x+(z+w)^2 == y~x+z+w+z:w / 
# ========= y~x+I((z+w)^2)==y~x+h， h是由z和w的平方和创建的新变量
# function ===>>>可以在表达式中用的数据函数 log(y)~x+z+w 表示通过x, z,和w来预测log(y)

# 有用的其他函数
# summary() ===>>>展示拟合模型的详细结果
# coefficients() ===>>>列出拟合模型的模型参数（截距项和斜率）
# confint() ===>>>提供模型参数的置信区间(默认95%)
# fitted() ===>>>列出拟合模型的预测值
# residuals() ===>>>理出拟合模型的残差值
# anova() ===>>>生成一个拟合模型的方差分析表，或者比较两个或者更多拟合模型的方差分析表
# vcov() ===>>>列出模型参数的协方差矩阵
# AIC() ===>>>输出赤池信息统计量 Akaike Information Criterion
# plot()===>>>生成评价拟合模型的诊断图
# predict() ===>>> 用拟合模型对新的数据集预测响应变量值

# ===========简单线性回归============
fit<-lm(weight~height, data=women)
summary(fit)
# F统计量检验所有的预测变量预测响应变量是否都在某个几率水平之上
# 由于简单回归只有一个预测变量，此处F检验等同于身高回归系数的t检验

women$weight
fitted(fit)
residuals(fit)
plot(women$height, women$weight, 
     xlab="Height (in inches)", 
     ylab="Weight (in pounds)")
abline(fit)

# ==========多项式回归================
fit2<-lm(weight~height+I(height^2), data=women)
summary(fit2)
plot(women$height, women$weight, 
     xlab="Height (in inches)", 
     ylab="Weight (in inches)")
lines(women$height, fitted(fit2))

# ==========绘制二元关系图=============
library(car)
scatterplot(weight~height, 
            data=women, 
            spread=FALSE, lty.smooth=2, 
            pch=19, 
            main="Women Age 30-39", 
            xlab="Height (inches)", 
            ylab="Wegith (lbs)")


# ==========多元线性回归===============
states<-as.data.frame(state.x77[, c("Murder", "Population", "Illiteracy", "Income", "Frost")])
cor(states) #第一步检查一下变量间的相关性
library(car) 
scatterplotMatrix(states, spread = FALSE, lty.smooth=2, 
                  main="Scatter Plot Matrix")

fit<-lm(Murder~., data=states)
summary(fit)

# =========有交互的多元线性回归===========
fit<-lm(mpg~hp+wt+hp:wt, data=mtcars)
summary(fit)
fit<-lm(mpg~hp+wt+wt:hp, data=mtcars)

#通过effects包中的effect()函数，可以用图形展示交互项的结果
library(effects)
plot(effect("hp:wt", fit, 
      xlevels=list(wt=c(2.2, 3.2, 4.2))),
      multiline=TRUE)

# =================================
# ==========回归诊断=================
# =================================

# ==========基础诊断==================
fit<-lm(Murder~., data=states)
confint(fit)
summary(fit)
# 文盲率改变1% 谋杀率就是95% 的置信区间[2.38, 5.90]中变化
# Forst 的置信区间包含0，可以得出结论，当其他变量不变是，温度当改变与谋杀率无关。

fit<-lm(weight~height, data=women)
par(mfrow=c(2,2))
plot(fit)
#正态性 --->>> 当预测变量值固定时，因变量成正态分布，
# ---------------则残差值应该也是一个均值为0的正态分布。 
# ---------------Normal Q-Q 是在正态分布对应的值下，标准化残差的概率图。
# ---------------若满足正态假设，那么图上的点应该落在呈45度角的直线上。

# 独立性--->>> 无法从这些图中分辨出因变量是否相互独立，只能从收集的数据中来验证。

# 线性--->>> 若因变量与自变量线性相关，那么残差值和预测值就没有任何系统联系。
#-------------换句话说，除了白噪声，模型应该包含数据中所有的系统方差。
#------------- Risiduals vs Fitted中可以清楚的看到一个曲线关系，暗示着可能需要对回归模型加上一个二次项

# 同方差性 --->>> 若满足不变方差假设，那么在Scale-Location 中，水平线周围的点应该随机分布

# Residuals vs Leverage提供了可能关注点单个观测点的信息。
#-------------可以鉴别出离群点，高杠杆值点和强影响点
#-------------离群点：表明拟合回归模型对其预测效果不佳（产生巨大的残差）
#-------------高杠杆值点：表明异常的预测变量值的组合。即，在预测变量空间中，它是一个离群点。
#-------------强影响点：表明对模型参数的估计产生的影响过大，非常不成比例。

fit2<- lm(weight~height+I(height^2), data=women)
plot(fit2)

fit<-lm(Murder~., data=states)
plot(fit)

#===============改进诊断================
# car包中的回归诊断是用函数
# qqPlot() --->>>分位数比较图
# durbinWatsonTest() --->>> 对误差自相关性做Durbin-Watson检验
# crPlots() --->>>成分与残差图
# ncvTest() --->>> 对非恒定的误差方差做得分检验
# spreadLevelPlot() --->>> 分散水平检验
# outlierTest() --->>> Bonferroni离群点检验
# avPlots() --->>>添加的变量图形
# inluencePlot() --->>> 回归影响图
# scatterplot() --->>>增强的散点图
# scatterplotMatrix() --->>> 增强的散点图矩阵
# vif() --->>> 方差膨胀因子 variance inflation factor

#========正态性===========
library(car)
fit<-lm(Murder~., data=states)
qqPlot(fit, labels=row.names(states), id.method = "identify", 
       simulate=TRUE, main="Q-Q Plot")
# id.method="indentify" 能够交互式绘图---待图形绘制后，用鼠标单击图形内的点，将会标注函数中
# --------label选项的设定值。敲击esc键，从图形下拉菜单中选择stop，或者在图形上右击，都将关闭
#-------- 这种交互模式。

# 关注Nevada，它又一个很大的正残差值，表明模型低估了该州的自杀率
states["Nevada", ]
fitted(fit)["Nevada"]
residuals(fit)["Nevada"]
rstudent(fit)["Nevada"]

# 其他可视化误差
residplot<-function(fit, nbreaks=10) {
            z<-rstudent(fit)
            hist(z, breaks=nbreaks, freq=FALSE, 
                 xlab="Studentized Residual", 
                 main="Distribution of Errors")
            rug(jitter(z), col="brown")
            curve(dnorm(x, mean=mean(z), sd=sd(z)), 
                  add=TRUE, col="blue", lwd=2)
            lines(density(z)$x, density(z)$y, 
                  col="red", lwd=2, lty=2)
            legend("topright", 
                   legend=c("Normal Curve", "Kernel Density Curve"), 
                   lty=1:2, col=c("blue", "red"), cex=.7)
            }
residplot(fit)

#===============线性=============
library(car)
crPlots(fit)
# 若图形存在非线性，则说明对预测变量的函数形式建模不够充分
# -----此图可以看出，成分残差图证实了线性假设，线性模型形式对该数据集看似是合适的


# ===============同方差性============
# car包提供了两个有用的函数，可以判定误差方差是否恒定
# ncvTest()生成一个计分检验，零假设为误差方差不变，备择假设为误差方差随着拟合值水平的变化而变化
# -------若检验显著，则说明存在异方差性
# spreadLevelPlot() 创建一个添加了最佳拟合曲线的散点图，展示标准化残差绝对值和拟合值的关系

library(car)
ncvTest(fit)
# p=.19 说明满足方差不变假设
spreadLevelPlot(fit)
# 通过分布水平图，看到点在水平的最佳拟合曲线周围呈水平随机分布
# ----- 若违反了该假设，会看到一个非水平的曲线。
# -----代码结果建议幂次变化的含义是，经过p次幂，非恒定的误差方差将会平稳
# -----对于当前例子，异方差性很不明显，因此建议幂次接近1
# ----- 若建议幂次转换为.5，在回归等式中用y^(1/2)代替y
# -----若建议幂次为0, 则是用对数变换

# ===============综合验证==============
library(gvlma)
gvmodel<-gvlma(fit)
summary(gvmodel)
# 从输出项(Global Stat中的文字栏)，可以看到数据满足OLS 回归模型所有的统计假设


# ===============多重共线性==============
library(car)
vif(fit)
sqrt(vif(fit))>2 
# 一般情况下vif^(1/2)>2就表明存在多重共线性问题
# 多重共线性例子：假设你正在进行一项握力研究，自变量包括出生日期（DOB）和年龄。
# -----------你用握力对DOB和年龄进行回归，F检验显著，p<.001。
# -----------但是当你观察DOB和年龄的回归系数时，却发现他们都不显著
# -----------原因是DOB与年龄在四舍五入后相关性极大。回归系数测量的是当其他预测变量不变时，
#-----------某个预测变量对响应变量的影响。那么此处就相当与假定年龄不变，然后测握力与年龄的关系
# -----------这种问题称为多重共线性。它会导致模型参数的置信区间过大，使单个系数解释起来很困难。


#==============================================
# 异常观测值
#==============================================

# =============离群点===================
# 一种方法是根据Q-Q图 或根据标准化残差值大于2或者小于－2的点，可能是离群点
library(car)
outlierTest(fit)
# 该函数只是根据单个最大残差值的显著性来判断是否有离群点。
# 若不显著，说明数据集中，没有离群点
# 若显著，则必须删除该离群点，然后再检验是否还有其他离群点存在。

# =============高杠杆点==================
# 高杠杆值观测点，即是与其他预测变量有关的离群点。他们是由许多异常的预测变量值组合起来的
# ------和响应变量值没有关系
# 通过蛮子统计量(hat statistic)判断。对于一个给定的数据集，帽子均值为p/n，其中p是模型估计的参数数目
# (包含截距项)，n是样本量。一般来说，若观测点的帽子值大于帽子均值的2或3倍，即可认为是高杠杆值点。

hat.plot<-function(fit) {
          p<-length(coefficients(fit))
          n<-length(fitted(fit))
          plot(hatvalues(fit), main="Index Plot of Hat Values")
          abline(h=c(2, 3)*p/n, col="red", lty=2)
          identify(1:n, hatvalues(fit), names(hatvalues(fit)))
}
hat.plot(fit)
# 高杠杆值点可能会是强影响点，也可能不是，这个要看他们是否是离群点

# =============强影响点==================
# 强影响点，即对模型参数估计值影响有些比例失衡的点。例如，若移除模型点一个观测点时
# -----模型会发生巨大的改变。
# 有两种方法可以检测强影响点：cook距离，或称D统计量，以及变量添加图
# 一般来说，cook's D值大于4/(n-k-1), 则表明它是强影响点
# -----其中n为样本量大小，k是预测变量数目。

cutoff<-4/(nrow(states)-length(fit$coefficients)-2)
plot(fit, which=4, cook.levels=cutoff)
abline(h=cutoff, lty=2, col="red")
# NOTE: 虽然该图对搜索强影响点很有用，但我逐渐发现以1为分割点比4/(n-k-1)更具一般性。
# 若设定D=1为判别标准，则数据集中没有点看起来像是强影响点

# Cook's D图有助于鉴别强影响点，但是并不提供关于这些点如何影响模型的信息。
# 变量添加图弥补了这个缺陷。
# 变量添加图，即对于每个预测变量Xk， 绘制Xk在其他k-1个预测变量上回归的残差值相对于
# -----响应变量在其他k-1个预测变量上回归的残差值的关系图。
library(car)
avPlots(fit, ask=FALSE, onepage=TRUE, id.method="identify")

library(car)
influencePlot(fit, id.method = "identify", main="Influence Plot", 
              sub="Circle size is proportional to Cook's distance")
# 影响图。纵坐标超过+2或者小于-2的州可被认为是离群点。
# ---------水平轴超过.2 或 .3的州，有高杠杆值
# ---------圆圈很大的点可能是对模型参数的估计造成的不成比例影响的强影响点。

#==============================================
# 改进措施
#==============================================
# 删除观测点
# 变量变换
# 添加或删除变量
# 使用其他回归方法

# ============变量变换====================
# 常见的变换： y^(-2); y^(-1); y^(-.5); log(y); y^(.5); y^2
# 如果y是比例数：[ln(y/(1-y))]

# 当模型违反了正态假设时，通常可以对响应变量尝试某种变换
library(car)
summary(powerTransform(states$Murder))
# 结果表明，可以用Murder^.6来正态化变量Murder。 但在本例中，lambda=1的假设也无法拒绝(p=.145)
# -----因此没有强有力的证据表明本例需要变量变换

# 当违反了线性假设时，对预测变量进行变化会比较有用
library(car)
boxTidwell(Murder~Population+Illiteracy, data=states)
# 结果表明，使用变化Population^.87 和 Illiteracy^1.36能够改善线性关系
# -----但是p=.75 和p=.53的计分检验又表明变量不需要变换。

#==============================================
# 选择“最佳”的回归模型
#==============================================
# ===========模型比较================
fit1<-lm(Murder~Population+Illiteracy+Income+Frost, data=states)
fit2<-lm(Murder~Population+Illiteracy, data=states)
anova(fit2, fit1)
# 此处模型1嵌套在模型2中。anova() 函数同时还对是否应该添加Income和Frost到线性模型中进行了检验
# -----由于检验不显著(p=.994)，因此可得：不需要将这两个变量添加到线性模型中

fit1<-lm(Murder~Population+Illiteracy+Income+Frost, data=states)
fit2<-lm(Murder~Population+Illiteracy, data=states)
AIC(fit1, fit2)
#NOTE: ANOVA 需要嵌套模型，AIC不需要
# AIC值越小的模型要优先选择，它说明模型用较少的参数获得了足够的拟合度。

# ============变量选择================
# 逐步回归法: 模型会一次添加或者删除一个变量，直到达到某个判停准则为止。
library(MASS)
fit1<-lm(Murder~Population+Illiteracy+Income+Frost, data=states)
stepAIC(fit, direction = "backward")

# 全子集法: 所有可能的模型都会被检验。可以选择展示所有可能的结果，也可以展示n个不同子集大小的最佳模型。
# ----- 例如：若nbest=2, 先展示两个最佳的单预测变量模型，然后展示两个最佳单双预测变量模型。
# 
library(leaps)
leaps<-regsubsets(Murder~Population+Illiteracy+Income+Frost, data=states, nbest=4)
plot(leaps, scale="adjr2")
# 含预测变量越少的模型，调整R平方越大。图形表明，双预测变量模型(Popluation and Illiteracy)是最佳模型

library(car)
subsets(leaps, statistic = "cp", 
        main="Cp Plot for All Subsets Regression")
abline(1, 1, lty=2, col="red")
# 越好的模型离截距项和斜率均为1的直线越近。
# 图形表明，可以选择：P & IL; P & IL& F; P & IL& In;P & IL & In & F

# 一般来说，变量自动选择应该被看作是对模型选择对一种辅助方法，而非直接方法
# 拟合效果佳而没有意义的模型对你毫无帮助
# 主题背景知识的理解才能最终只因你获得理想的模型

#==============================================
# 深层次分析
#==============================================

#===============交叉验证==================
# k重交叉验证中，样本被分为k个子样本，轮流将k-1个子样本组合作为训练集，另外1个子样本作为保留集
# ----- 这样会获得k个预测方程，记录k个保留样本的预测表现结果，然后求平均值

shrinkage<-function(fit, k=10){
  require(bootstrap)
  
  theta.fit<-function(x, y) {lsfit(x,y)}
  theta.predict<-function(fit, x) {cbind(1, x)%*%fit$coef}
  
  x<-fit$model[, 2:ncol(fit$model)]
  y<-fit$model[,1]
  
  results<-crossval(x, y, theta.fit, theta.predict, ngroup=k)
  r2<-cor(y, fit$fitted.values)^2
  r2cv<-cor(y, results$cv.fit)^2
  cat("Original R-square =", r2, "\n")
  cat(k, "Fold Cross-Validated R-square =", r2cv, "\n")
  cat("Change = ", r2-r2cv, "\n")
  }
fit<-lm(Murder~., data=states)
shrinkage(fit)
# 可以看到基于出事样本的R平方(.567)过于乐观了，
# -----对于新数据更好的方差解释率估计是交叉验证后的R平方 0.448

fit2<-lm(Murder~Population+Illiteracy, data=states)
shrinkage(fit2)
# 含两个预测变量的模型，比全变量模型R平方减少的更少。R平方减少的越少，预测则越精确。

#=================相对重要性===============
# 比较标准化的回归系数：
# -----表示当其他预测变量不变时，该预测变量一个标准差的变化可引起的响应变量的预期变化
# -----在进行回归分析前，可用scale函数将数据标准化为均值为0，标准差为1的数据集
# -----这样用R回归即可获得标准化的回归系数
# -----scale返回的是一个矩阵，lm要求的是一个数据框，需要中间转换一下

zstates<-as.data.frame(scale(states))
zfit<-lm(Murder~., data=zstates)
coef(zfit)
# 可以看到，当其他因素不变时，文盲率一个标准差的变化将增加.68个标准差的谋杀率
# -----根据标准化的回归系数，我们可以认为文盲率是最重要的预测变量，而frost是最不重要的

#相对权重 relative weight
relweights<-function(fit, ...){
            R<-cor(fit$model)
            nvar<-ncol(R)
            rxx<-R[2:nvar, 2:nvar]
            rxy<-R[2:nvar, 1]
            svd<-eigen(rxx)
            evec<-svd$vectors
            ev<-svd$values
            delta<-diag(sqrt(ev))
            lambda<-evec %*% delta %*% t(evec)
            lambdasq<-lambda^2
            beta<-solve(lambda)%*% rxy
            rsquare<-colSums(beta^2)
            rawwgt<-lambdasq%*%beta^2
            import<-(rawwgt/rsquare)*100
            lbls<-names(fit$model[2:nvar])
            rownames(import)<-lbls
            colnames(import)<-"Weights"
            barplot(t(import), names.arg=lbls,
                    ylab="% of R-Wquare", 
                    xlab="Predictor Variable", 
                    main="Relative Importance of Predictor Variables", 
                    sub=paste("R-Square=", round(rsquare, digits=3)), 
                    ...)
            return (import)
            }
# note: 代码可参考Johnson(2000, Multivariate Behaviroal Research, 35, 1-19)

fit<-lm(Murder~., data=states)
relweights(fit, col="lightgrey")
# 通过图可以看到各个预测变量对模型方差的解释程度 R^2=.567
# Illiteracy解释了59%的R平方，Forst解释了20.79%