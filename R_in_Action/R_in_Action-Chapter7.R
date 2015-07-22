#基本统计分析

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
                    m=mean(x) 
                    s=sd(x)
                    return(c(mean=m, sd=s))
                    }
by(mtcars[vars], mtcars$mpg, dstats)
tapply(mtcars[vars], as.factor(mtcars$mpg), dstats)
head(mtcars[vars])
class(mtcars$am)
?by
