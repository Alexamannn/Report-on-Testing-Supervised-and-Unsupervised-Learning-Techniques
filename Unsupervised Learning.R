library(caret)
library(AppliedPredictiveModeling)
library(pls)
library(e1071)
library(lattice)
library(pls)
library(MASS)
library(lars)
library(elasticnet)
library(car)
library(glmnet)
library(plyr)

###read data
data<-read.csv("cal-housing (1).csv")
head(data)
data$ocean_proximity<-revalue(data$ocean_proximity, c("NEAR BAY"="1", "INLAND"="2", "ISLAND"="3", "NEAR OCEAN"="4","<1H OCEAN"="5"))
data$ocean_proximity <- as.numeric(data$ocean_proximity)
data<-data[,!names(data) %in% c('X','X.1')]

###do a scatterplot matrix to  see linearilty
splom(data)
nrow(data)
head(data)


###create training and test data set
###set 75 percent of rows for training and rest for test
bound<-floor(0.75*nrow(data))
data.train <- data[1:bound, ]            
data.test <- data[(bound+1):nrow(data), ] 
nrow(data.test)
nrow(data.train)
dataTrainX<-data.train
dataTestX<-data.test


###apply box cox transformation
boxcox<-preProcess(dataTrainX,method ="BoxCox") 
dataTrainXtrans<-predict(boxcox,dataTrainX)
head(dataTrainXtrans)
hist(dataTrainXtrans$population)
hist(dataTrainX$population)

datatestXtrans<-predict(boxcox,dataTestX)
head(datatestXtrans)
hist(datatestXtrans$median_house_value)
hist(dataTestX$median_house_value)

###create training data
trainingData<-dataTrainXtrans
trainingData<-dataTrainX
head(trainingData)

###fit the model-OLS
model<-lm(median_house_value~.,data=trainingData)
summary(model)
par(mfrow=c(2,2))


###predict values
pred<-predict(model,datatestXtrans)
###create obs,pred data frame
df<-data.frame(obs=datatestXtrans$median_house_value,pred=pred)
df
defaultSummary(df)
###cross-validation
ctrl<-trainControl(method="cv",n=10)
set.seed(100)
tmp<-subset(dataTrainXtrans,select =-median_house_value)
head(tmp)
modcv<-train(x=tmp,y=dataTrainXtrans$median_house_value,method="lm",trControl =ctrl)


###check for multicollinearality
vif(model)
###vif levels shows collinearity in the dataset


###pca analysis 
pca<-data
###standardize independent variables
x<-subset(pca,select=-median_house_value)
head(x)
x<-scale(x)
###center the dependent variable
y<-pca$median_house_value
y<-scale(y,scale =F)
###do pca on indepenedent variables
comp<-prcomp(na.omit(x))
comp
plot(comp)
biplot(comp)
summary(comp)
#5 principal components explain 97% of the total variance


pcr<-pcr(median_house_value~.,data=trainingData,validation="CV")
summary(pcr)
###choose five components for prediction
xpcr=subset(datatestXtrans,select=-median_house_value)
pcrpred<-predict(pcr,xpcr,ncomp =5)
pcrdf1<-data.frame(obs=dataTestX$median_house_value,Predictions=pcrpred)
pcrdf1

###pls regression is a better variation of PCR.It accounts for the variation in response when selecting weights
###use pls package, plsr function
###default algorithm is Dayal and Mcgregor kernel algorithm
plsFit<-plsr(median_house_value~.,data=trainingData,validation="CV")
###predict first five median_house_value values using 1 and 2 components
pls.pred<-predict(plsFit,datatestXtrans[1:100,],ncomp=1:2)
summary(plsFit)
validationplot(plsFit,val.type ="RMSEP")
pls.RMSEP<-RMSEP(plsFit,estimate="CV")
plot(pls.RMSEP,main="RMSEP PLS",xlab="Components")
min<-which.min(pls.RMSEP$val)
points(min,min(pls.RMSEP$val),pch=1,col="red")
plot(plsFit, ncomp=4, asp=1)

###use 4 components
pls.pred2<-predict(plsFit,datatestXtrans,ncomp=5)
pls.eval<-data.frame(obs=dataTestX$median_house_value ,pred=pls.pred2[,1,1])
defaultSummary(pls.eval)
