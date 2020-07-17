library(readr)
dat <- read.csv("cal-housing (1).csv")
str(dat)
#marking different regions
plot(dat$longitude,dat$latitude)
points(dat$longitude[dat$ocean_proximity=="NEAR BAY"], dat$latitude[dat$ocean_proximity=="NEAR BAY"], col="blue", pch=19)
points(dat$longitude[dat$ocean_proximity=="ISLAND"], dat$latitude[dat$ocean_proximity=="ISLAND"], col="green", pch=19)
points(dat$longitude[dat$ocean_proximity=="NEAR OCEAN"], dat$latitude[dat$ocean_proximity=="NEAR OCEAN"], col="pink", pch=19)
points(dat$longitude[dat$ocean_proximity=="INLAND"], dat$latitude[dat$ocean_proximity=="INLAND"], col="black", pch=19)
points(dat$longitude[dat$ocean_proximity=="<1H OCEAN"], dat$latitude[dat$ocean_proximity=="<1H OCEAN"], col="purple", pch=19)
#looking for expensive houses
plot(dat$longitude,dat$latitude)
summary(dat$median_house_value)
points(dat$longitude[dat$median_house_value<=300000],dat$latitude[dat$median_house_value<=300000], col="red", pch=20)

# Linear Regression using LAT and LON
plot(dat$latitude, dat$median_house_value)
plot(dat$longitude, dat$median_house_value)

latlonlm = lm(median_house_value ~ latitude + longitude, data=dat)
summary(latlonlm)
plot(dat$latitude,dat$longitude)
points(dat$latitude[dat$median_house_value>=300000], dat$longitude[dat$median_house_value>=300000], col="red", pch=20)
latlonlm$fitted.values
points(dat$latitude[latlonlm$fitted.values >= 300000], dat$longitude[latlonlm$fitted.values >= 300000], col="blue", pch="€")

#Decision Tree analysis
library(rpart)
library(rpart.plot)
latlontree = rpart(median_house_value ~ latitude + longitude, data=dat)
prp(latlontree)

plot(dat$latitude,dat$longitude)
points( dat$latitude[dat$median_house_value>=300000],dat$longitude[dat$median_house_value>=300000], col="red", pch=20)
fittedvalues = predict(latlontree)
points(dat$latitude[fittedvalues >= 300000], dat$longitude[fittedvalues >= 300000], col="blue", pch="$")


#Regression tree analysis
library(caTools)
set.seed(123)
split = sample.split(dat$median_house_value, SplitRatio = 0.7)
train = subset(dat, split==TRUE)
test = subset(dat, split==FALSE)

# Create a CART model
tree = rpart(median_house_value ~ latitude + longitude + median_income + population , data=train)
prp(tree)


tree.pred = predict(tree, newdata=test)
tree.sse = sum((tree.pred - test$median_house_value)^2)
tree.sse
fittedvalues2 <-tree.pred
plot(dat$latitude,dat$longitude)
points( dat$latitude[dat$median_house_value>=300000],dat$longitude[dat$median_house_value>=300000], col="red", pch=20)
fittedvalues = predict(latlontree)
points(dat$latitude[fittedvalues2 >= 300000], dat$longitude[fittedvalues2 >= 300000], col="blue", pch="$")

#Regression tree overfitt the data