#4 ---- dataframe basic

#============================
# DATA CREATION
#============================
manager<-c(1:5)
date<-c("10/24/08", "10/28/08", "10/1/08", "10/12/08", "5/1/09")
country<-c("US", "US", "UK", "UK", "UK")
gender<-c("M", "F", "F", "M", "F")
age<-c(32, 45, 25, 39, 99)
q1<-c(5, 3, 3, 3, 2)
q2<-c(4, 5, 5, 3, 2)
q3<-c(5, 2, 5, 4, 1)
q4<-c(5, 5, 5, NA, 2)
q5<-c(5, 5, 2, NA, 1)

leadership<-data.frame(manager, date, country, gender, age, q1, q2, q3, q4, q5, stringsAsFactors=FALSE)

#============================
# END ===== DATA CREATION
#============================

#============================
# TRANSFORM ADD COL WITH CALCULATION
#============================
transform(mydata, 
          sumx=x1+x2,
          meanx=(x1+x2)/2)

#============================
#END ==== TRANSFORM ADD COL WITH CALCULATION
#============================


#============================
# ADD ONE COL WITH SOME CALCULATION
### ---->>>  variable[condition]<-expression
#============================
leadership$age[leadership$age==99]<-NA

#sample 1
leadership$agecat[leadership$age<55]<-"Young"
leadership$agecat[leadership$age>=55 & leadership$age<=75]<-"Middle Aged"
leadership$agecat[leadership$age>75]<-"Elder"

#sample 2
leadership<-within(leadership, {
                    agecat<-NA
                    agecat [age>75]<-"Elder"
                    agecat [age<=75 & age>=55]<-"Middle Aged"
                    agecat [age<55]<-"Young"})

#============================
#END ==== ADD ONE COL WITH SOME CALCULATION
#============================


#============================
# RENAME  VARIABLE
### ---->>>  RENAME IN <RESHAPE>
#============================
#SAMPLE 1
library(reshape2)
leadership<-rename(leadership, 
                   c(manager="managerID", date="testDate"))

#SAMPLE 2
names(leadership)[6:10]<-c("item1", "item2", "item3", "item4", "item5")

#============================
#END ==== RENAME VAIRABLE
#============================


#============================
# DEAL WITH NA ROWS
### ---->>>  NA.OMIT() --->>> THIS WILL DELETE ALL THE ROWS WITH NA DATA
#============================
newdata<-na.omit(leadership)
newdata
#============================
#END ==== DEAL  WITH ROWS
#============================

#============================
# DATES  FORMAT
###  %d ---->>>  数字表示的日期（0-30） --->>>  01-31
###  %a --->>>  缩写的星期名 --->>>  Mon
###  %A --->>>  非缩写的星期名 --->>>  Monday
###  %m --->>>  月份（00-12） --->>>  00-12
###  %b --->>>  缩写的月份 --->>>  Jan
###  %B --->>>  非缩写的月份 --->>>  January
###  %y --->>>  两位数的年份 --->>>  07
###  %Y --->>>  四位数的年份 --->>>  2007
### default Date format is yyy-mm-dd
### other Date format need to add the correct format in the function --->> as.Date(mydates, "%m%d%Y")

### USEFUL DATE FUNCTIONS
### Sys.Date() --->>> return today's date
###date()--->>> return current date and time
### format(x, format="output_format") --->>> the  output is show the format date
#============================
#sample
myformat<-"%m/%d/%y"
leadership$date<-as.Date(leadership$date, myformat)

#sample 
today<-Sys.Date()
format(today, format="%B-%d-%y")
format(today, format="%A")

#sample to calculate the date duration
today<-Sys.Date()
dob<-as.Date("1956-10-12")
difftime(today, dob, units = "weeks")
format(dob, format="%A")
#============================
#END ====  DATE
#============================

#============================
# TYPE CHANGE
### is.numeric() ---as.numeric()
### is.character() --- as.character()
### is.vector() --- as.vector()
### is.matrix() --- as.matrix()
### is.data.frame() --- as.data.frame()
### is.logical() --- as.logical()
#============================

#============================
# ORDER
#============================
newdata<-leadership[order(leadership$age),]

attach(leadership)
    newdata<-leadership[order(gender, age), ]
detach(leadership)
newdata

#============================
#END ==== ORDER
#============================

#============================
# COMBINE TWO DATAFRAMES ROWS 
### ---->>>  merge(dataframeA, dataframeB, by=c("colA", "colB")) ---->>> like the inner join
#============================

#============================
# SELECT VARIABLES
#============================
#sample
myvars<-paste("q", 1:5, sep = "")
leadership[myvars]
#============================
#END ==== SELECT VARIABLES
#============================

#============================
# DELETE VARIABLES
#============================
myvars<-names(leadership)%in%c("q3", "q4")
newdata<-leadership[!myvars]
newdata
#============================
#END ==== DELETE VARIABLES
#============================

#============================
# GET THE ROWS
# --->>> SUBSET()
#============================
#sample1
startdate<-as.Date("2009-1-1")
enddate<-as.Date("2009-12-31")
newdata<-leadership[which(leadership$date>=startdate&leadership$date<=enddate),]
newdata

#sample2
newdata<-subset(leadership, age>=35|age<24, select=c(q1, q2, q3, q4))

#sample3
newdata<-subset(leadership, gender=="M"&age>25, select=gender:q4)

#============================
#END ==== GET THE ROWS
#============================

#============================
# SAMPLE THE DATA
### ---->>>  SAMPLE()
#============================
mysample<-leadership[sample(1:nrow(leadership), 3, replace=FALSE), ]
mysample
#============================
#END ==== SAMPLE THE DATA
#============================
