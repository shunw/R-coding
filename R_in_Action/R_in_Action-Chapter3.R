#图形初阶

#===========使用图形================

#保存图形
pdf("mygraph.pdf")
  attach(mtcars)
  plot(wt, mpg)
  abline(lm(mpg~wt))
  title("Regression of MPG on Weight")
  detach(mtcars)
dev.off()
getwd()

# pdf()/ win.metafile(), png(), jpeg(), bmp(), tiff(), xfig(), postscript()

#============图形参数================
# par(optionname=value,  optionname=name, ...)
# 不加参数的执行par将生成一个含有当前图形参数设置的列表
# 添加no.readyonly=TRUE可以生成一个可以修改的当前图形参数列表

dose<-c(20, 30, 40, 45, 60)
drugA<-c(16, 20, 27, 40, 60)
drugB<-c(15, 18, 25, 31, 40)

opar<-par(no.readonly=TRUE) # 复制了当前的图形参数设置
par(lty=2, pch=17) #将默认的线条类型修改为虚线，将点符号修改为实心三角
plot(dose, drugA, type="b") #绘制图形
par(opar) # 还原了原始设置

#==============颜色===================
# col --->>> 默认的绘图颜色
# col.axis--->>>坐标轴刻度文字的颜色
# col.lab --->>>坐标轴标签（名称）的颜色
# col.main --->>> 标题颜色
# col.sub --->>> 副标题颜色
# fg --->>>图形的前景颜色
# bg --->>> 图形的背景颜色

#==============文本属性=================
# cex --->>>表示相对于默认大小缩放倍数的数值
# cex.axis --->>> 坐标轴刻度文字的缩放倍数
# cex.lab --->>>坐标轴标签（名称）的缩放倍数
# cex.main --->>>标题的缩放倍数
# cex.sub --->>>副标题的缩放倍数

#==============图形尺寸和边界尺寸=========
# pin --->>>以英寸表示图形尺寸
# mai --->>>以数值向量表示的边界大小，顺序为“下，左，上，右”，单位为英寸
# mar --->>>以数值向量表示的边界大小，顺序同上，单位为英分，默认值为c(5, 4, 4, 2)+.1

opar<-par(no.readonly = TRUE)
par(pin=c(2, 3))
par(lwd=2, cex=1.5)
par(cex.axis=.75, font.axis=3)
plot(dose, drugA, type="b", pch=19, lty=2, col="red")
plot(dose, drugB, type="b", pch=23, lty=6, col="blue", bg="green")
par(opar)

#================标题===================
title(main="My Title", col.main="red", 
      sub="My Sub-title", col.sub="blue", 
      xlab="My X label", ylab="my Y label", 
      col.lab="green", cex.lab=.75)

#================坐标轴=================
# side --->>> 一个整数，表示在图形的哪边绘制坐标轴(1=下，2=左，3=上，4=下)
# at --->>>一个数值型向量，表示需要绘制刻度线的位置
# labels --->>> 一个字符型向量，表示置于刻度线旁边的文字标签
# pos --->>>坐标轴线绘制位置的坐标 （即与另一条坐标轴相交位置的值）
# lty --->>> 线条类型
# col --->>>线条和刻度线颜色
# las --->>>标签是否平行于(=0)或垂直于(=2)坐标轴
# tck --->>>刻度线的长度

x<-c(1:10)
y<-x
z<-10/x

opar<-par(no.readonly = TRUE)

par(mar=c(5, 4, 4, 8)+.1)

plot(x, y, type="b", 
     pch=21, col="red", 
     yaxt="n", lty=3, ann=FALSE)   # ann=FALSE 禁用所有标题和标签

lines(x, z, type="b", pch=22, col="blue", lty=2)

axis(2, at=x, labels=x, col.axis="red", las=2)

axis(4, at=z, labels=round(z, digits = 2), 
     col.axis="blue", las=2, cex.axis=.7, tck=-.01)

mtext("y=1/x", side=4, line=3, cex.lab=1, las=2, col="blue")

title("An Example of Creative Axes", 
     xlab="X values", 
     ylab="Y=X")
par(opar)

#===================图例===============
opar<-par(no.readonly = TRUE)

par(lwd=2, cex=1.5, font.lab=2)

plot(dose, drugA, type="b", 
     pch=15, lty=1, col="red", ylim=c(0,60), 
     main="Drug A vs. Drug B", 
     xlab="Drug Dosage", ylab="Drug Response")

lines(dose, drugB, type="b", 
      pch=17, lty=2, col="blue")

abline(h=c(30), lwd=1.5, lty=2, col="gray") # 绘制参考线

library(Hmisc)
minor.tick(nx=3, ny=3, tick.ratio = .5) #添加次要刻度线

legend("topleft", inset = .05, title="Drug Type", c("A", "B"),
       lty=c(1, 2), pch=c(15, 17), col=c("red", "blue"))
par(opar)


#====================图形的组合===============
# mfrow=c(nrows, ncols)
# nfcol=c(nrows, ncols) --->>>按列填充矩阵

attach(mtcars)
layout(matrix(c(1, 1, 2, 3), 2, 2, byrow=TRUE))
hist(wt)
hist(mpg)
hist(disp)
detach(mtcars)

par(mar=c(4,4, 2, 2))
attach(mtcars)
layout(matrix(c(1, 1, 2, 3), 2 ,2, byrow = TRUE), 
       widths = c(3, 1), heights=c(1, 2)) #widths各列宽度值组成的一个向量/ height各行高度值
hist(wt)
hist(mpg)
hist(disp)
detach(mtcars)

# FIG sample
opar<-par(no.readonly = TRUE)
par(fig=c(0, .8, 0, .8)) # c(x1, x2, y1, y2)
plot(mtcars$wt, mtcars$mpg, 
     xlab="Miles Per Gallon", 
     ylab="Car Weight")

par(fig=c(0, .8, .55, 1), new=TRUE)
boxplot(mtcars$wt, horizontal = TRUE, axes=FALSE)

par(fig=c(.65, 1, 0, .8), new=TRUE)
boxplot(mtcars$mpg, axes=FALSE)

mtext("Enhanced Scatterplot", side=3, outer=TRUE, line=-3)
par(opar)
