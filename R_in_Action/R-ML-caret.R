library(caret); library(kernlab); data("spam")
inTrain<-createDataPartition(y=spam$type,
                             p=.75, list=FALSE)
inTrain
training<-spam[inTrain,]
testing<-spam[-inTrain,]
modelFit<-train(type~., data = training, method="glm")
install.packages("e1071")
library(e1071)

#====================
# PREPROCESS   PCA
#====================
library(caret); library(kernlab); data(spam)
head(spam)
inTrain<-createDataPartition(y=spam$type, 
                             p=.75, list=FALSE)
training<-spam[inTrain, ]
testing<-spam[-inTrain, ]

ncol(spam)
ncol(M)
head(spam[, 58])
M<-abs(cor(training[, -58]))
head(M)
diag(M)<-0
diag(M)
?diag
which(M>.9, arr.ind = T)

names(spam)[c(34, 32)]
plot(spam$num415, spam$num857)
plot(spam[, 34], spam[, 32])

x<-training$num415+training$num857
y<-training$num415-training$num857
plot(x, y)

smallSpam<-spam[, c(34, 32)]
prComp<-prcomp(smallSpam)
summary(prComp)
str(prComp)
plot(prComp$x[,1], prComp$x[, 2])
prComp$rotation

typeColor<-((spam$type=="spam")*1+1)
prComp<-prcomp(log10(spam[,-58]+1))
head(prComp)
plot(prComp$x[, 1], prComp$x[, 2], col=typeColor, xlab="PC1", ylab="PC2")

preProc<-preProcess(log10(spam[, -58]+1), method="pca", pcaComp=2)
spamPC<-predict(preProc, log10(spam[, -58]+1))
plot(spamPC[, 1], spamPC[,2], col=typeColor)

str(spamPC)
head(spamPC)

#sample to use the preprocess
preProc<-preProcess(log10(training[, -58]+1), method="pca", pcaComp=2)
trainPC<-predict(preProc, log10(training[, -58]+1))
modelFit<-train(training$type~., method="glm", data=trainPC)
summary(trainPC)

testPC<-predict(preProc, log10(testing[, -58]+1))
confusionMatrix(testing$type, predict(modelFit, testPC))
#end --->>>sample to use the preprocess


#====================
# PREDICT  WITH REGRESSION
#====================
library(caret); data("faithful"); set.seed(333)
inTrain<-createDataPartition(y=faithful$waiting,
                             p=.5, list=FALSE)
nrow(faithful)
length(inTrain)

#separate the train data and the test data
trainFaith<-faithful[inTrain,]; testFaith<-faithful[-inTrain,]
nrow(faithful[-inTrain,])
head(trainFaith)

#plot
plot(trainFaith$waiting, trainFaith$eruptions, pch=19, col="blue", xlab="Waiting", ylab="Duration")

#make a  regression
lm1<-lm(eruptions~waiting, data=trainFaith)
summary(lm1)
lines(trainFaith$waiting, lm1$fitted, lwd=3)

#make a predict
coef(lm1)[1]+coef(lm1)[2]*80
newdata<-data.frame(waiting=80)
predict(lm1, newdata)
length(predict(lm1))


#check in the test set
par(mfrow=c(1,2))
plot(trainFaith$waiting, trainFaith$eruptions, pch=19, col="blue", xlab="waiting", ylab="duration")
lines(trainFaith$waiting, predict(lm1), lwd=3)
plot(testFaith$waiting, testFaith$eruptions, pch=19, col="blue", xlab="waiting", ylab="duration")
lines(testFaith$waiting, predict(lm1, newdata=testFaith), lwd=3)

#get training set/ test set errors
#w training set
sqrt(sum((lm1$fitted-trainFaith$eruptions)^2))

#w test set
sqrt(sum((predict(lm1, newdata=testFaith)-testFaith$eruptions)^2))

#prediction intervals
pred1<-predict(lm1, newdata=testFaith, interval="prediction")
ord<-order(testFaith$waiting)
plot(testFaith$waiting, testFaith$eruptions, pch=19, col="blue")
matlines(testFaith$waiting[ord], pred1[ord, ], type="l", col=c(1, 2, 2), lty=c(1, 1, 1), lwd=3)
head(pred1)
head(ord)
head(testFaith$waiting)


#w caret package
modFit<-train(eruptions~waiting, data=trainFaith, method="lm")
summary(modFit$finalModel)


#========================================
# PREDICTING WITH REGRESSION MULTIPLE COVARIATES
#========================================
library(ISLR); library(ggplot2); library(caret)
data(Wage); Wage<-subset(Wage, select = -c(logwage))
head(Wage)
summary(Wage)

#set training data
inTrain<-createDataPartition(y=Wage$wage,
                             p=.7, list=FALSE)
training<-Wage[inTrain, ]; testing<-Wage[-inTrain, ]
dim(training); dim(testing)

#feature plot
featurePlot(x=training[, c("age", "education", "jobclass")], 
            y=training$wage,
            plot="pairs")
qplot(age, wage, data=training)
qplot(age, wage, data=training, color=jobclass)
qplot(age, wage, data=training, color=education)


#fit a linear model
modFit<-train(wage~age+jobclass+education, 
        method="lm", data=training)
fitMod<-modFit$finalModel
print(modFit)
summary(fitMod)

#plot
par(mfrow=c(1,1))
plot(fitMod, 1, pch=19, col="#00000010")
qplot(fitMod$fitted, fitMod$residuals, colour=race, data=training)
plot(fitMod$residuals, pch=19)

pred<-predict(modFit, testing)
head(testing)
qplot(wage, pred, color=year, data=testing)

# fit w all convariates
modFitAll<-train(wage~., data=training, method="lm")
warnings()
pred<-predict(modFitAll, testing)
qplot(wage, pred, data=testing)

#========================================
# QUIZ 2
#========================================
library(AppliedPredictiveModeling)
install.packages("AppliedPredictiveModeling")
library(caret)
data(AlzheimerDisease)


library(AppliedPredictiveModeling)
data(concrete)
library(caret)
set.seed(1000)
inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = mixtures[ inTrain,]
testing = mixtures[-inTrain,]
head(training)
hist(training$Superplasticizer)
hist(log(training$Superplasticizer))



library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]
head(training)

var<-grep("^IL.*", colnames(training))
training_sub<-subset(training,select=var)
head(training_sub)
head(adData)

M<-abs(cor(training_sub))
diag(M)
diag(M)<-0
which(M>.8, arr.ind = T)

?preProcess
precomp<-preProcess(training_sub, method = "pca", thresh =.9 )
precomp

#4
#Create a training data set consisting of only the predictors with variable names beginning with IL
#    and the diagnosis.
#Build two predictive models,
#one using the predictors as they are
#and one using PCA 
#    with principal components explaining 80% of the variance in the predictors.
#    Use method="glm" in the train function. 
#    What is the accuracy of each method in the test set? Which is more accurate?
library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

#w pca 
var<-grep("^[dI][iL].*", colnames(training))
training_sub=subset(training, select=var)
testing_sub=subset(testing, select = var)

precomp<-preProcess(training_sub[,-1], method="pca", thresh=.8)

trainPC<-predict(precomp, training_sub[, -1])
modelFit<-train(training$diagnosis~., method="glm", data=trainPC)
summary(trainPC)
testPC<-predict(precomp, testing_sub[, -1])
confusionMatrix(testing$diagnosis, predict(modelFit, testPC))


#w all
modelFit2<-lm(diagnosis~., data=training_sub)
head(training_sub)
summary(modelFit2)
confusionMatrix(testing$diagnosis, predict(modelFit2, testing_sub))