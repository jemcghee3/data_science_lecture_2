#### Regression on Auto dataset

## Prerequisites
#install.packages("ISLR")
library(ISLR)

## 1. Inspect the Data
Auto
# view some rows in the dataset
head(Auto)
# view the statistical summary of the dataset
summary(Auto)
# create a scatterplot of all features in the dataset and inspect it
plot(Auto)

## 2. Fit a regression model
# Tell R to remember the dataset we are using
attach(Auto)
# Start by using all the predictors in the dataset - backward selection
lm.fit <- lm(mpg~., data=Auto)
# inspect the model
summary(lm.fit)

# activate the dplyr library to use the select function (first install if needed)
library(dplyr)
# fit a model by excluding the qualitative feature - name
lm.fit1 <- lm(mpg~., data=select(Auto,-c(name)))
# again inspect the model - relationships between coefficients, metrics, significance
summary(lm.fit1)

## 3. Improve model
# check correlation between the quantitative predictors
cor(select(Auto,-c(name)))
# use the corrplot library to visualize the correlations 
# install.packages("corrplot")
library(corrplot)
corrplot(cor(select(Auto,-c(name))), type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)

# re-fit the model after selecting the important prpedictors
lm.fit2 <- lm(mpg~displacement+weight+year+origin, data=select(Auto,-c(name)))
# inspect the model - which metrics changed, significance
summary(lm.fit2)

# continue to re-fit after identifying improvements, here we excluded displacement             
lm.fit3 <- lm(mpg~weight+year+origin, data=select(Auto,-c(name)))
# inspect the model - any improvement, what else can be improved
summary(lm.fit3)

## 4. Check Residual plots
# further improvement can be identified by checking the residual plots of our model
plot(lm.fit3)
# check the residual vs. fitted plot - is there any non-linearity?
plot(lm.fit3, which=1)
# check the normal q-q plot - are the points following the line? skewedness in data? outliers?
plot(lm.fit3, which=2)
# plot the studentized residuals - outliers?
#rstudent(lm.fit3)
plot(rstudent(lm.fit3))
text(x=1:length(rstudent(lm.fit3)), y=rstudent(lm.fit3), labels=ifelse(rstudent(lm.fit3) > 3,names(rstudent(lm.fit3)),""), col="red", pos = 4)


## 5. Improve model based on diagnostic inspection
# fit a model using polynomial regression (weight to the degree 2)
lm.fit4.poly <- lm(mpg~weight+I(weight^2)+year+origin, data=select(Auto,-c(name)))
# inspect the summary and the effect on the metrics
summary(lm.fit4.poly)
# again check the residual plot - any improvement?
plot(lm.fit4.poly, which=1)

# identify outliers using studentized residuals
#rstudent(lm.fit4.poly)
plot(rstudent(lm.fit4.poly))
text(x=1:length(rstudent(lm.fit4.poly)), y=rstudent(lm.fit4.poly), labels=ifelse(rstudent(lm.fit4.poly) > 3,names(rstudent(lm.fit4.poly)),""), col="red", pos = 4)

# we need to be careful while removing the outliers, confirm the outliers using the outlierTest function
library(car)
outlierTest(lm.fit4.poly)

# let's remove the identified outliers
#first get the index of the rows that have the outliers - note that 323,327 and 387 correspond to the row names and not the indices
ind <- which(rownames(Auto) %in% c("323","327","387"))
# then copy the data without the outlier into another variable
auto_nooutlier <- Auto[-ind, ]
#re-fit the previous model to the new data
lm.fit4_nooutlier <- lm(mpg~weight+I(weight^2)+year+origin, data=select(auto_nooutlier,-c(name)))
# inspect the summary - any improvement?
summary(lm.fit4_nooutlier)

######Start - tried to improve further, but only minor or no improvement
lm.fit5_nooutlier<- lm(mpg~weight+I(weight^2)+I(weight^3)+year+origin, data=select(auto_nooutlier,-c(name)))
summary(lm.fit5_nooutlier)

#first get the index of the rows that have the outliers
ind <- which(rownames(Auto) %in% c("323","327","387","328","326"))
# then copy the data without the outlier into another variable
auto_nooutlier <- Auto[-ind, ]

lm.fit6_noorigin<- lm(mpg~weight+I(weight^2)+year, data=select(auto_nooutlier,-c(name)))
summary(lm.fit6_noorigin)
######End - tried to improve further, but only minor improvement

## 6. Find accuracy of the model
# Calculate MSE for all the models that we fit in this example, compare to see the improvement
mean(lm.fit$residuals^2)
mean(lm.fit1$residuals^2)
mean(lm.fit2$residuals^2)
mean(lm.fit3$residuals^2)
mean(lm.fit4.poly$residuals^2)
mean(lm.fit4_nooutlier$residuals^2)


