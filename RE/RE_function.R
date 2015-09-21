setwd("C:/Users/lishunw.AUTH/My Work/program/github/R-coding/RE")
getwd()

# MIBF_list<-c(80000, 100000, 12000)
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
# series_sys_MIBF(MIBF_list)

parallel_sys_MIBF <- function(MIBF_list)
{
  #the MIBF_list is 2 parallel system. MIBF_list <- c(a, b)
  landa_1 <- 1/MIBF_list[1]
  landa_2 <- 1/MIBF_list[2]
  MIBF_sys <- (1/landa_1) + (1/landa_2) - 1/(landa_1+landa_2)
  return (MIBF_sys)
}

MIBF_list_sub_par <- c(12000, 13000)
sub_sys <- parallel_sys_MIBF(MIBF_list)

MIBF_list <- c(sub_sys, 80000, 100000)
series_sys_MIBF(MIBF_list)
