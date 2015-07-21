install.packages("vcd")
library(vcd)
head(Arthritis)

#============================
# BARPLOT
#============================
counts<-table(Arthritis$Improved)
counts

barplot(counts,
        main="Simple Bar Plot",
        xlab="Impovement", ylab="Frequency")

barplot(counts,
        main="Simple Bar Plot",
        xlab="Frequency", ylab="Improvement",
        horiz = TRUE)
#============================
# END --- BARPLOT
#============================


#============================
# STACKED -BARPLOT
#============================
counts<-table(Arthritis$Improved, Arthritis$Treatment)
counts

barplot(counts, 
        main="Stacked Bar Plot",
        xlab="Treatment", ylab="Frequency",
        col=c("red", "yellow", "green"),
        legend=rownames(counts))
        
barplot(counts, 
        main="Stacked Bar Plot",
        xlab="Treatment", ylab="Frequency",
        col=c("red", "yellow", "green"),
        legend=rownames(counts), beside=TRUE)


#============================
# END --- STACKED -BARPLOT
#============================


#============================
# 均值---BARPLOT
#============================
states<-data.frame(state.region, state.x77)
means<-aggregate(states$Illiteracy, by=list(state.region), FUN=mean)
means<-means[order(means$x), ]
means

barplot(means$x, names.arg = means$Group.1)
title("Mean illiteracy Rate")

#============================
# END --- 均值---BARPLOT
#============================

#============================
# BARPLOT --- adjustment
# cex.names --->>>减小字号
# names.arg --->>> 指定一个字符向量作为条形的标签名
#============================
par(mar=c(5,8,4,2))
par(las=2)
counts<-table(Arthritis$Improved)

barplot(counts, 
        main="Treatment Outcome",
        horiz=TRUE, cex.names = .8, 
        names.arg=c("No Improvement", "Some Improvement", "Marked Improvement"))
#============================
# END --- BARPLOT --- adjustment
#============================


#============================
# SPINOGRAM
#============================
attach(Arthritis)
counts<-table(Treatment, Improved)
spine(counts, main="Spinogram Example")
detach(Arthritis)
#============================
# END --- SPINOGRAM
#============================


#============================
# HIST
#============================
par(mfrow=c(2, 2))

hist(mtcars$mpg)

hist(mtcars$mpg,
     breaks=12,
     col="red",
     xlab="Miles Per Gallon",
     main="Colored hist with 12 bins")

hist(mtcars$mpg,
     breaks=12,
     freq = FALSE,
     col="red",
     xlab="Miles Per Gallon",
     main="Colored hist with 12 bins")
rug(jitter(mtcars$mpg)) #--->>> rug plot 轴须图
lines(density(mtcars$mpg), col="blue", lwd=2) #--->>>密度曲线

x<-mtcars$mpg
h<-hist(x,
        breaks=12,
        col="red",
        xlab="Miles Per Gallon", 
        main="Hist w normal curve and box")
xfit<-seq(min(x), max(x), length=40)
yfit<-dnorm(xfit, mean=mean(x), sd=sd(x))
yfit<-yfit*diff(h$mids[1:2])*length(x)
lines(xfit, yfit, col="blue", lwd=2) # --->>> 正态曲线
box()

#============================
# END --- HIST
#============================


#============================
# DENSITY
#============================
par(mfrow=c(2, 1))
d<-density(mtcars$mpg)
plot(d)

d<-density(mtcars$mpg)
plot(d, main="Kernel Density of Miles Per Gallon")
polygon(d, col="red", border="blue") #--->>>  根据顶点的x和y坐标绘制了多边形
rug(mtcars$mpg, col="brown")

par(lwd=2)
library(sm)
attach(mtcars)
cyl.f<-factor(cyl, levels=c(4, 6, 8), 
              labels=c("4 cylinder", "6 cylinder", "8 cylinder"))

sm.density.compare(mpg, cyl, xlab="Miles Per Gallon")
title(main="MPG Distribution by Car cylinders")

colfill<-c(2:(1+length(levels(cyl.f))))
legend(locator(1), levels(cyl.f), fill=colfill)
# --->>> locator(1) 表示用鼠标点击想让图例出现的位置来交互式的放置这个图例

detach(mtcars)
#============================
# END --- DENSITY
#============================
