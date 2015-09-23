setwd("C:/Users/lishunw.AUTH/My Work/program/github/R-coding/RE")
getwd()

#-------------------------------------------
#------------section 3 ---------------------
#-------------------------------------------

#--- function --- series system MIBF
series_sys_MIBF <- function (MIBF_list) 
  {
  #the MIBF_list is series system. MIBF_list <- c(a, b, c, ...)
  landa_sys=0
  for (i in MIBF_list)
    {
      landa_part<-(1/i)
      landa_sys=landa_sys+landa_part
    }
  MIBF_sys <- (1/landa_sys)
  return (MIBF_sys)
}

#--- function --- parallel system MIBF
parallel_sys_MIBF <- function(MIBF_list)
{
  #the MIBF_list is 2 parallel system. MIBF_list <- c(a, b)
  landa_1 <- 1/MIBF_list[1]
  landa_2 <- 1/MIBF_list[2]
  MIBF_sys <- (1/landa_1) + (1/landa_2) - 1/(landa_1+landa_2)
  return (MIBF_sys)
}
MIBF_P <- c(12500, 12500)
parallel_sys_MIBF(MIBF_P)


MIBF_list_sub_par <- c(12000, 13000)
sub_sys <- parallel_sys_MIBF(MIBF_list)

MIBF_list <- c(sub_sys, 80000, 100000)
series_sys_MIBF(MIBF_list)


n <- 2
k <- 1
MIBF <- 12500
#--- function --- not functioned--- K of N w/o repair
K_N_Rsys<-function(n, k, MIBF)
{
    #this is for the K of N Items required to work with and w/o repair
    #this function has some problem. 
    Ra <- 1/MIBF
    Total <- 0
    for (i in 0:(n-k))
    {
      Total = Total + (factorial(n)/(factorial(i)*factorial(n-i)))*(Ra^(n-i))*((1-Ra)^i)
      
    }
    return (Total)
}
1/K_N_Rsys(n, k, MIBF)



#-------------------------------------------
#------------section 5 ---------------------
#-------------------------------------------

# Exponential Expression
# CDF <- the cumulative fraction failing at time t
landa <- 10^(-6)
t <- 87600
F_exp<-function(landa, t)
{
    return (1-exp(-landa*t))
}
F_exp(landa, t)

# R(t) <- exp(-landa*t)
R_exp <- function(landa, t)
{
    return (exp(-landa*t))
}

# practice 1
#get the failure rate, etc
failure_rate_h <- 10/10^6
R_sixmonth <- 1-F_exp(failure_rate_h, 365/2*24)
R_sixmonth <- R_exp(failure_rate_h, 8760)
F_oneyear <- F_exp(failure_rate_h, 365*24)
failed_6month_1<- 1000*(1-R_sixmonth)
failed_6month_2<- 1000*R_sixmonth*(1-R_sixmonth)
year<-failed_6month_1+failed_6month_2
print (R_sixmonth)

# practice 2
#plotting position

#create the dataframe: get the failure number and the failure time
failure <- data.frame(Rank_X=c(1, 2, 3, 4, 5), Number_Failures=c(1, 1, 1, 1, 2), 
                      Fail_Time_Midpoint=c(880, 1000, 1300, 2000, 3000))
total_engine <- 7
#create the i cum failures
failure$i_cum <- cumsum(failure$Number_Failures)
#create the median (i-.3)/(n+.4)  
failure$median<-(failure$i_cum-.3)/(total_engine+.4)

#base plot
plot(x=failure$Fail_Time_Midpoint, y=failure$median, type='b')


# other test
tt<- read.table("M3073GeyserData.txt", header = TRUE)
Dorment<- tt$Dormant
Dorment
qqnorm(Dorment)
qqline(Dorment, col=2)
n<-length(Dorment)
Fi<-(1:n-.5)/n

SD<-sort(Dorment)
Qi<--log(1-Fi)

plot(SD, Qi) #this is for the exponential plot

Eta <- log(-log(1-Fi))
plot(Eta, log(SD), ylab="Log Dorment Quantiles", 
     xlab="Theoretical Weibull Quantiles", main="Weibull P-P Plot")
m<-(log(SD[37])-log(SD[4]))/(Eta[37]-Eta[4])
b<-log(SD[4])-m*Eta[4];b
abline(b, m, col=3)

survreg(Surv(futime, fustat) ~ ecog.ps+rx, ovarian, dist='weibull', scale=1)
Surv(ovarian$futime, ovarian$fustat)

head(lung)
nrow(lung)
Surv(lung$time, lung$status)

survreg(Surv(time, status) ~ ph.ecog + age + strata(sex), lung)
