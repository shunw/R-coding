#========================================
# PREDICTING WITH TREES
#========================================
library(caret)
data(iris);library(ggplot2)
names(iris)
table(iris$Species)

inTrain<-createDataPartition(y=iris$Species, 
                             p=.7, list=FALSE)
training<-iris[inTrain, ]
testing<-iris[-inTrain, ]
dim(training); dim(testing)
qplot(Petal.Width, Sepal.Width, colour=Species, data=training)

modFit<-train(Species~., method="rpart", data=training)
print(modFit$finalModel)

plot(modFit$finalModel, uniform=TRUE, 
     main="Classification Tree")
text(modFit$finalModel, use.n=TRUE, 
     all=TRUE, cex=.8)

library(rattle)
fancyRpartPlot(modFit$finalModel)

predict(modFit, newdata=testing)


#========================================
# BAGGING
#========================================
library(ElemStatLearn); data(ozone, package="ElemStatLearn")



#========================================
# Quiz w3
#========================================
#1
library(AppliedPredictiveModeling)
data(segmentationOriginal)
library(caret)
head(segmentationOriginal)
names(segmentationOriginal)

#inTrain<-createDataPartition(y=segmentationOriginal$Case, p=.7, list=FALSE)
training<-segmentationOriginal[which(segmentationOriginal$Case=="Train"),]
testing<-segmentationOriginal[which(segmentationOriginal$Case=="Test"),]

set.seed(125)
fitMod1<-train(Class~., method="rpart", data=training)

# library(rpart)
# fitMod<-rpart(Class~., method="class", data=training)
# plot(fitMod1$finalModel, uniform=TRUE)
# text(fitMod1$finalModel, use.n=TRUE, 
#      all=TRUE, cex=.8)

testing1<-as.data.frame(lapply(testing, function(x) rep.int(NA, 1)), stringsAsFactors=FALSE)
testing1
 = 57,000; FiberWidthCh1 = 8;VarIntenCh4 = 100 

testing1$TotalIntench2<-57000
testing1$FiberWidthCh1<-8
testing1$VarIntenCh4<-100
predict(fitMod1, newdata=testing1)

fitMod1$finalModel

head(testing)
#3
install.packages("pgmm")
library(pgmm)
data(olive)
olive = olive[,-1]
head(olive)
newdata=as.data.frame(t(colMeans(olive)))
fitMod_3<-train(Area~., method="rpart", data=olive)
fitMod_3$finalModel
predict(fitMod_3, newdata)
olive$Area
class(olive$Area)
complete.cases(olive)


#4
install.packages("ElemStatLearn")
library(ElemStatLearn)
data(SAheart)
set.seed(8484)
train = sample(1:dim(SAheart)[1],size=dim(SAheart)[1]/2,replace=F)
trainSA = SAheart[train,]
testSA = SAheart[-train,]

set.seed(13234)
head(trainSA)
fitMod_4<-train(chd~age+alcohol+obesity+tobacco+typea+ldl, method="glm", data=trainSA, family="binomial")
missClass = function(values,prediction){sum(((prediction > 0.5)*1) != values)/length(values)}
fd<-c("age","alcohol","obesity","tobacco","typea","ldl")
testSA[, fd]

missClass(testSA$chd, predict(fitMod_4, newdata=testSA[,fd]))
missClass(trainSA$chd, predict(fitMod_4, newdata=trainSA))


#5
library(ElemStatLearn)
data(vowel.train)
data(vowel.test) 
set.seed(33833)
vowel.test$y<-as.factor(vowel.test$y)
vowel.train$y<-as.factor(vowel.train$y)
fitMod_5<-train(y~., data=vowel.train, method="rf", prox=TRUE)
