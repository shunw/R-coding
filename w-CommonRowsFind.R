#==========================================================================
#PURPOSE: to find the common contents in csv1 and csv2 with specific fields
#Step 1: find the lvls with Field1,2,3,4 with "unique" function in csv1 and csv2 separately.
#Step 2: make the common levels with these two lvls by merge function.
#Step 3: find the rows with these common level in csv1 and csv2 separately by merge function. 
#Step 4: combine these two dataframe, and make a output
#==========================================================================

setwd("path")
f_1<-read.csv("csv1.csv")
f_2<-read.csv("csv2.csv")
#=======================================================
# GET THE COMMON CONTEXT IN THE FOLLOWING FIELDS
#=======================================================
common_fields<-c("F1", "F2", "F3", "F4")
lvl_f1<-unique(f_1[common_fields])
lvl_f2<-unique(f_2[common_fields])

lvl_com<-merge.data.frame(x=lvl_f1, y=lvl_f2)

#=======================================================
# GET THE COMMON CONTEXT IN THE FOLLOWING FIELDS
#=======================================================
f_1_com<-merge(x=f_1, y=lvl_com)
f_2_com<-merge(x=f_2, y=lvl_com)
com_total<-rbind(f_1_com, f_2_com)

#=======================================================
# SORT THE COL NAMES AS ITS PREVIOUS ONES
#=======================================================
F_Name<-colnames(f_1)
com_total_ord<-com_total[, F_Name]

#=======================================================
# WRITE THE OUTPUT FILE
#=======================================================
write.csv(com_total, file="test_output.csv", row.names = FALSE)
