# 中级绘图

#============================
# 马赛克图
#============================
# 两个以上的类别型变量
ftable(Titanic)
library(vcd)
mosaic(Titanic, shade=TRUE, legend=TRUE)
mosaic(~Class+Sex+Age+Survived, data=Titanic, shade=TRUE, legend=TRUE)

#============================
# 散点图
#============================
# ============基本散点图=============
par(mfrow=c(1, 1))
attach(mtcars)
plot(wt, mpg,
     main="Basic Scatter plot of MPG vs. Weight",
     xlab="Car Weight (lbs/1000)", 
     ylab="Miles Per Gallon", pch=19)
abline(lm(mpg~wt), col="red", lwd=2, lty=1)
lines(lowess(wt, mpg), col="blue", lwd=2, lty=2)
detach(mtcars)
# R有两个平滑曲线拟合函数：lowesss() 和loess()。 loess是基于lowess表达式版本的更新和更强大的拟合函数
# 这两个函数的默认值不同，因此要小心使用

# car 包中的scatterplot函数增强了散点图的许多功能
library(car)
scatterplot(mpg~wt|cyl, data=mtcars, lwd=2, #按照cyl的水平分别绘制mpg和wt的关系图
            main="Scatter Plot of MPG vs. Weight by # Cylinders", 
            xlab="Weight of Car (lbs/1000)", 
            ylab="Miles Per GAllon", 
            legend.plot=TRUE, 
            id.method="identify", 
            labels=row.names(mtcars), 
            boxplots="xy")
# 平滑拟合默认需要5个单独的点，所以6缸车型的平滑曲线无法绘制。
# legend.plot表明在左上边界添加图例

#===============散点图矩阵=================
pairs(~mpg+disp+drat+wt, data=mtcars, 
      main="Basic Scatter Plot Matrix")

library(car)
scatterplotMatrix(~mpg+disp+drat+wt, data=mtcars, spread=FALSE, 
                  lty=2, main="Scatter Plot Matrix via car Package")

library(car)
scatterplotMatrix(~mpg+disp+drat+wt|cyl, data=mtcars, spread=FALSE, 
                  diagonal="histogram",
                  main="Scatter Plot Matrix via car Package")


cor(mtcars[c("mpg", "wt", "disp", "drat")])
library(gclus)
mydata<-mtcars[c(1, 3, 5, 6)]
mydata.corr<-abs(cor(mydata))
mycolors<-dmat.color(mydata.corr)
myorder<-order.single(mydata.corr)
cpairs(mydata,
       myorder,
       panel.colors=mycolors,
       gap=.5,
       main="Variable Ordered and Colored by Correlation")

#===================高密度散点图=========================
set.seed(1234)
n<-10000
c1<-matrix(rnorm(n, mean=0, sd=.5), ncol=2)
c2<-matrix(rnorm(n, mean=3, sd=2), ncol=2)
mydata<-rbind(c1, c2)
mydata<-as.data.frame(mydata)
names(mydata)<-c("x", "y")

with(mydata, 
     plot(x, y, pch=19, main="Scatter Plot with 10, 000 Observations"))

#smaple 1
with(mydata,
     smoothScatter(x, y, main="Scatterplot Colored by Smoothed Densities"))

#sample 2
library(hexbin)
with(mydata, {
      bin<-hexbin(x, y, xbins=50)
      plot(bin, main="Hexagonal Binning with 10,000 Observations")
      })

#====================三围散点图====================
library(scatterplot3d)
attach(mtcars)
scatterplot3d(wt, disp, mpg, 
              main="Basic 3D Scatter Plot")
detach(mtcars)

attach(mtcars)
scatterplot3d(wt, disp, mpg, 
              pch=16, 
              highlight.3d = TRUE,
              type="h", 
              main="3D Scatter Plot with Vertical Lines")
detach(mtcars)

attach(mtcars)
s3d<-scatterplot3d(wt, disp, mpg, 
                   pch=16, 
                   highlight.3d = TRUE,
                   type="h",
                   main="3D Scatter Plot with Vertical Lines and Regression Planes")
fit<-lm(mpg~wt+disp)
s3d$plane3d(fit)
detach(mtcars)

#================气泡图=====================
attach(mtcars)
r<-sqrt(disp/pi)
symbols(wt, mpg, circle=r, inches=.3, # inch是比例因子，控制圆圈大小，默认最大为1
        fg="white", bg="lightblue", 
        main="Bubble Plot with point size proportional to displacement", 
        ylab="Miles Per Gallon", 
        xlab="Weight of Car (lbs/1000)")
text(wt, mpg, rownames(mtcars), cex=.6)
detach(mtcars)


#============================
# 折线图
#============================
opar<-par(no.readonly=TRUE)
par(mfrow=c(1, 2))
t1<-subset(Orange, Tree==1)
plot(t1$age, t1$circumference, 
     xlab="Age (days)", 
     ylab="Circumference (mm)", 
     main="Orange Tree 1 Growth")
plot(t1$age, t1$circumference, 
     xlab="Age (days)", 
     ylab="Circumference (mm)", 
     main="Orange Tree 1 Growth", 
     type="b")
par(opar)

# p --->>> 只有点
# l --->>> 只有线
# o --->>> 实心点和线（即线覆盖在点上）
# b, c --->>>线连接点 （c时不绘制点）
# s, S --->>>阶梯线
# h --->>>直方图式的垂直线
# n --->>>不生成任何点和线（通常用来为后面的命令创建坐标轴）
#Note: plot和line原理不同，plot是被调用时即创建一副新图，
# ------line则是在已存在的图形上添加信息，并不能自己生成图形。

Orange$Tree<-as.numeric(Orange$Tree)
ntrees<-max(Orange$Tree)

xrange<-range(Orange$age)
yrange<-range(Orange$circumference)

plot(xrange, yrange, 
     type="n", 
     xlab="Age (days)", 
     ylab="Circumference (mm)"
     )

colors<-rainbow(ntrees)
linetype<-c(1:ntrees)
plotchar<-seq(18, 18+ntrees, 1)

for (i in 1:ntrees) {
          tree<-subset(Orange, Tree==i)
          lines(tree$age, tree$circumference, 
                type="b", 
                lwd=2,
                lty=linetype[i],
                col=colors[i],
                pch=plotchar[i]
                )
            }

title("Tree Growth", "example of line plot")

legend(xrange[1], yrange[1], 
       1:ntrees, 
       cex=.8,
       col=colors,
       pch=plotchar,
       lty=linetype,
       title="Tree")

#================相关图======================
options(digits = 2)
cor(mtcars)

#sample 1
library(corrgram)
corrgram(mtcars, order=TRUE, lower.panel=panel.shade, 
         upper.panel=panel.pie, text.panel=panel.txt, 
         main="Correlogram of mtcars intercorrelations")
#蓝色和从左下指向右上的斜杠表示两个变量呈正相关
# 红色和从左上指向右下的斜杠表示两个变量呈负相关
# 色彩越深，饱和度越高，说明变量相关性越大


#sample 2
library(corrgram)
corrgram(mtcars, order=TRUE, lower.panel = panel.ellipse, 
         upper.panel = panel.pts, text.panel = panel.txt,
         diag.panel = panel.minmax, 
         main="Correlogram of mtcars data using scatter plots and ellipses")

#sample 3
library(corrgram)
corrgram(mtcars, lower.panel = panel.shade, 
         upper.panel = NULL, text.panel = panel.txt,
         main="Car Mileage Data (unsorted")

#sample 4
# 以下号称可以改颜色，不过没有看出来，觉得是代码有问题。。。
library(corrgram)
col.corrgram<-function(ncol){
              colorRampPalette(c("darkgoldenrod4", "burlywood1", 
                                 "darkkhaki", "darkgreen"))(ncol)}

corrgram(mtcars, order=TRUE, lower.panel = panel.shade, 
         upper.panel = panel.pie, text.panel = panel.txt,
         main="A Corrgram( or Horse) of a Different Color")
