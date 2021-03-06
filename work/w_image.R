<<<<<<< HEAD
#============ change the path here============
setwd("path")
fl<-read.csv("data.csv")
#============ end change the path here============

colnames(fl)
fl_clean<-fl[,-(98:168)]

#============ change to value/colname you want here============
col_name<-"colname_x_axis"
X_axis<-quote(colname_x_axis)
y_axis<-quote(colname_y_axis)
spec_l<-real spec low
spec_h<-real spec high
#============ end change to value/colname you want here============

#just select the leading edge deviation
LD<-subset(fl, select=c(eval(X_axis), eval(y_axis)))

#remove the na rows
attach(LD)
LD_clean<-LD[!is.na(eval(y_axis)), ]
detach(LD)

#restructure the data.frame
library(plyr)
LD_summary<-ddply(LD_clean, .(eval(X_axis)), 
                   summarize, mean=mean(eval(y_axis)), sd=sd(eval(y_axis)), 
                   meanp3sd=mean+3*sd, meanm3sd=mean-3*sd, 
                   spec_L=spec_l, spec_H=spec_h)
LD_summary<-rename(LD_summary, c("eval(X_axis)"=col_name))
head(LD_summary)
#remove the NA item
LD_sum_c<-LD_summary[!is.na(LD_summary$sd),]
tail(LD_sum_c)
=======
#function to get the right character
substrRight<-function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}
#function end

#debug tool
#check the list index of cha in the cha_1
which(cha%in%cha_1)
#end debug tool

#===========================================
#=================change here=================
#===========================================
setwd("path")
x_col_name<-"col name will show as the x axis"
y_col_name<-"col name will show as the y axis"
spec_l<--0.008
spec_h<-0.008
#===========================================
#=================END=======================
#===========================================

fl<-read.csv("data.csv")
fl_clean<-fl[,-(98:168)]

#just select the leading edge deviation
LD<-subset(fl, select=c(x_col_name, y_col_name))

#remove the na rows and move the "" row in x_axis col
LD_clean<-LD[!is.na(LD[, y_col_name]) & LD[, x_col_name]!="", ]

#==========data class convert in general formate=========
# change the character into numeric by col together
LD_clean[, y_col_name]<-as.character(LD_clean[, y_col_name])
# convert the % part

LD_clean[substrRight(LD_clean[, y_col_name], 1)=="%", y_col_name]<-
  as.numeric(sub("%", "", LD_clean[substrRight(LD_clean[, y_col_name], 1)=="%", y_col_name]))/100

LD_clean[, y_col_name]<-as.numeric(LD_clean[, y_col_name])

#==========end data class convert in general formate=========

#restructure the data.frame
library(plyr)
LD_summary<-ddply(LD_clean, .(tbd=LD_clean[, x_col_name]), 
                   summarize, mean=mean(LD_clean[, y_col_name]), sd=sd(LD_clean[, y_col_name]), 
                   meanp3sd=mean+3*sd, meanm3sd=mean-3*sd, 
                   spec_L=spec_l, spec_H=spec_h)
LD_summary<-rename(LD_summary, c("tbd"=col_name))

#remove the NA item
LD_sum_c<-LD_summary[!is.na(LD_summary$sd),]
>>>>>>> 709b90347a5c152bbac2b435ae4e415fcfa36b5a

#draw the dots with the spec
library(ggplot2)
ggplot(LD_sum_c, aes(eval(X_axis)))+
  geom_point(aes(y=mean, color="mean"))+
  geom_point(aes(y=meanp3sd, color="mean+3sd"))+
  geom_point(aes(y=meanm3sd, color="mean-3sd"))+
  geom_hline(aes(yintercept=spec_L, color="spec"))+
  geom_hline(aes(yintercept=spec_H, color="spec"))

