#THIS CHAPTER IS FOR THE CHARTS

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



#============================
# BOXPLOT
#============================
#sample for one variable
boxplot(mtcars$mpg, main="Box Plot", ylab="Miles per Gallon")

#sample for compare variables
# boxplot(formula, data=dataframe)
boxplot(mpg~cyl, data=mtcars, 
        main="Car Mileage Data",
        xlab="Number of Cylinders",
        ylab="Miles Per Gallon",
        horizontal=TRUE)

boxplot(mpg~cyl*gear, data=mtcars, 
        main="Car Mileage Data",
        xlab="Number of Cylinders",
        ylab="Miles Per Gallon")

#sample --->>> notch=TRUE
boxplot(mpg~cyl, data=mtcars, 
        notch=TRUE,
        varwidth=TRUE,
        col="red", 
        main="Car Mileage Data",
        xlab="Number of Cylinders",
        ylab="Miles Per Gallon")

#sample --->>> with two factors
mtcars$cyl.f<-factor(mtcars$cyl,
                     levels=c(4, 6, 8),
                     labels=c("4", "6", "8"))

mtcars$am.f<-factor(mtcars$am, 
                    levels=c(0, 1), 
                    labels=c("auto", "standard"))

boxplot(mpg~am.f*cyl.f,
        data=mtcars,
        varwidth=TRUE,
        col=c("gold", "darkgreen"),
        main="MPG Distribution by Auto Type",
        xlab="Auto Type")
#============================
# END --- BOXPLOT
#============================

#============================
# VIOPLOT
#============================
library(vioplot)
x1<-mtcars$mpg[mtcars$cyl==4]
x2<-mtcars$mpg[mtcars$cyl==6]
x3<-mtcars$mpg[mtcars$cyl==8]

vioplot(x1, x2, x3,
        names=c("4 cyl", "6 cyl", "8 cyl"),
        col="gold")


#============================
# END --- VIOPLOT
#============================


#============================
# DOTCHART
#============================
#sample1
dotchart(mtcars$mpg, labels=row.names(mtcars), cex=.7, 
         main="Gas Mileage for Car Models", 
         xlab="Miles Per Gallon")

#sample2 --->>> 分组 排序 着色
x<-mtcars[order(mtcars$mpg),]
x$cyl<-factor(x$cyl)
x$color[x$cyl==4]<-"red"
x$color[x$cyl==6]<-"blue"
x$color[x$cyl==8]<-"darkgreen"

dotchart(x$mpg,
         labels=row.names(x),
         cex=.7,
         groups = x$cyl,
         gcolor="black",
         color=x$color,
         pch=19,
         main="Gas Mileage for Car Model\ngrouped by cylinder",
         xlab="Miles Per Gallon")

#============================
# END---DOTCHART
#============================
