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

#draw the dots with the spec
library(ggplot2)
ggplot(LD_sum_c, aes(eval(X_axis)))+
  geom_point(aes(y=mean, color="mean"))+
  geom_point(aes(y=meanp3sd, color="mean+3sd"))+
  geom_point(aes(y=meanm3sd, color="mean-3sd"))+
  geom_hline(aes(yintercept=spec_L, color="spec"))+
  geom_hline(aes(yintercept=spec_H, color="spec"))

