#this is to cal the part1 and part2 sum. 
#first step: clean data ---> move the NA rows, and sort the data by"D" and "B.Pg"
#second step: add Totalsum ---> cumsum the X..of.pg by D
#third step: add the part1sum/ part2sum ---> cumsum the X..of.pg by D and by part1
#fourth step: add the part1beg/ part1 end/ part2 beg/ part2 end
#fifth step: write the csv file. 


#++++++++++++++++++++++++++++++++++++++++++++
# FIRST
#++++++++++++++++++++++++++++++++++++++++++++
#set the correct path
setwd("file_path")

#get the raw data from csv file. 
raw_0<-read.csv("souce_file.csv", fill=TRUE, header=TRUE, sep=",")

#clear the raw data from NA rows
raw<-raw_0[!is.na(raw_0$J.ID), ]

#sort the data
raw_ord<-raw[order(raw$"D", raw$"B.Pg"), ]

#++++++++++++++++++++++++++++++++++++++++++++
# SECOND
#++++++++++++++++++++++++++++++++++++++++++++
#calculate the total sum including part1 and part2
require(data.table)
raw_sum<-data.table(raw_ord)

raw_sum<-within(raw_sum, {
        Totalsum<-ave(X..of.Pg, D, FUN=cumsum)
})


#++++++++++++++++++++++++++++++++++++++++++++
# THIRD
#++++++++++++++++++++++++++++++++++++++++++++

#calculate the accumulated page by part1 and part2
raw_sum[raw_sum$Input=="part1", part1sum:=cumsum(X..of.Pg), by=c("D", "I")]
raw_sum[raw_sum$Input=="part2", part2sum:=cumsum(X..of.Pg), by=c("D", "I")]

#++++++++++++++++++++++++++++++++++++++++++++
# FOURTH
#++++++++++++++++++++++++++++++++++++++++++++
#Add part1/ part2 Begin/ End col
raw_sum$part1_Beg<-NA
raw_sum$part1_End<-NA
raw_sum$part1_Beg<-as.integer(raw_sum$part1_Beg)
raw_sum$part1_End<-as.integer(raw_sum$part1_End)

raw_sum$part2_Beg<-NA
raw_sum$part2_End<-NA
raw_sum$part2_Beg<-as.integer(raw_sum$part2_Beg)
raw_sum$part2_End<-as.integer(raw_sum$part2_End)

#part1 and part2 Begin/ End
raw_sum[raw_sum$Input=="part1", ]$part1_Beg=raw_sum[raw_sum$Input=="part1", ]$part1sum-raw_sum[raw_sum$Input=="part1", ]$X..of.Page+1
raw_sum[raw_sum$Input=="part1", ]$part1_End=raw_sum[raw_sum$Input=="part1", ]$part1sum

raw_sum[raw_sum$Input=="part2", ]$part2_Beg=raw_sum[raw_sum$Input=="part2", ]$part2sum-raw_sum[raw_sum$Input=="part2", ]$X..of.Page+1
raw_sum[raw_sum$Input=="part2", ]$part2_End=raw_sum[raw_sum$Input=="part2", ]$part2sum

#============================================================
# Fill the NA item in the sum col
#============================================================
raw_sum[is.na(raw_sum$part2sum), ]$part2sum=raw_sum[is.na(raw_sum$part2sum), ]$Totalsum-raw_sum[is.na(raw_sum$part2sum), ]$part1sum
raw_sum[is.na(raw_sum$part1sum), ]$part1sum=raw_sum[is.na(raw_sum$part1sum), ]$Totalsum-raw_sum[is.na(raw_sum$part1sum), ]$part2sum

#++++++++++++++++++++++++++++++++++++++++++++
# FIFTH
#++++++++++++++++++++++++++++++++++++++++++++
#============================================================
# write the output file
#============================================================
write.csv(raw_sum, file="test_output.csv")
