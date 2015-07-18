library(caret); library(kernlab); data("spam")
inTrain<-createDataPartition(y=spam$type,
                             p=.75, list=FALSE)
inTrain
training<-spam[inTrain,]
testing<-spam[-inTrain,]
modelFit<-train(type~., data = training, method="glm")
install.packages("e1071")
library(e1071)
