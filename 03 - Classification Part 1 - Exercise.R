#####################################################
# Data Science - Exercise
# Categorization Part 1: KNN, Logistic Regression
# Gwendolin Wilke
# gwendolin.wilke@fhnw.ch
#####################################################
# Adapted from ISLR chapter 4:
# Introduction to Statistical Learning, 
# by Witten, Hastie Tibshirani, Springer 2017
#####################################################




##############   PRELIMINARIES   ##########################

# install packages - if needed
# install.packages("ISLR")
# install.packages("MASS")
# install.packages("ggplot2")
# install.packages("gridExtra")
# install.packages("class")

# load libraries
library(ISLR)      # contains textbook data
library(ggplot2)   # for nice plots
library(gridExtra) # for convenient window arrangement in plotting
library(class)     # for using k-NN
library(MASS)      # for using lda

# get rid of old stuff
rm(list=ls()) # clear environment
par(mfrow=c(1,1)) # set plotting window to default

# set printing preferences
options(scipen=99) # penalty for displaying scientific notation
options(digits=4) # suggested number of digits to display

##############   Explore the "Default" Data Set  ##########################

str(Default)
summary(Default)
attach(Default)

# Display the distributions of balance and income
p1 <- ggplot(Default, aes(x=income)) + geom_histogram(color="black", fill="white")
p2 <- ggplot(Default, aes(x=balance)) + geom_histogram(color="black", fill="white")
grid.arrange(p1, p2, nrow = 1)
par(mfrow=c(1,1))

# Display the classes in the Default data
p1 <- ggplot(Default, aes(x=balance, y=income)) + geom_point(aes(col=default))
p2 <- ggplot(Default, aes(x=default, y=balance, fill=default)) + geom_boxplot()
p3 <- ggplot(Default, aes(x=default, y=income, fill=default)) + geom_boxplot()
grid.arrange(p1, p2, p3, nrow = 1, widths = c(2, 1, 1))
par(mfrow=c(1,1))

set.seed(1)
# Create training and test data
(indices <- sort(sample(1:length(balance), 100))) # select 100 random samples
(test.data <- Default[indices,])
training.data <- Default[-indices,]
p1 <- ggplot() + geom_point(data = training.data, aes(x=balance, y=default), color='steelblue3') + geom_point(data = test.data, aes(x=balance, y=default), color='darkred', size=4) 
p2 <- ggplot() + geom_point(data = training.data, aes(x=balance, y=income), color='steelblue3') + geom_point(data = test.data, aes(x=balance, y=income), color='darkred', size=4) 
grid.arrange(p1, p2, nrow = 1)


##############   K-Nearest Neighbors   ##########################

# Convert taining and test data to k-NN specific fomrat
(training.data.predictors <- cbind(training.data$balance,training.data$income))
(test.data.predictors <- cbind(test.data$balance,test.data$income))
training.data.class <- training.data$default

# Fit the k-NN model with k=1

set.seed (1)
(knn.pred <- knn(training.data.predictors,test.data.predictors, training.data.class,k=1))

# Confusion Matrix
table(knn.pred, test.data$default)

# Estimate the test error rate
mean(knn.pred != test.data$default)


# Fit the k-NN model with k=3
set.seed (1)
knn.pred <- knn(training.data.predictors,test.data.predictors, training.data.class,k=3)

# Confusion Matrix
table(knn.pred, test.data$default)

# Estimate the test error rate
mean(knn.pred != test.data$default)


# Fit the k-NN model with k=100
set.seed (1)
knn.pred <- knn(training.data.predictors,test.data.predictors, training.data.class,k=100)

# Confusion Matrix
table(knn.pred, test.data$default)

# Estimate the test error rate
mean(knn.pred != test.data$default)


##############   Logistic Regression   ##########################


# Logistic Regression with 1 PREDICTOR

# Fitting the Logistic Regression model to the training data 
glm.fit <- glm(default~balance,family="binomial", data=training.data)
summary(glm.fit)

# Making predictions

# "Predicting" the TRAINING data 
pred.train.lin = predict(glm.fit) # No data set is supplied to the predict() function: the probabilities are computed for the training data that was used to fit the logistic regression model. 
# Notice: Without the type option specified in predict we get the linear predictor scale (see plot below)
pred.train.lin.df <- data.frame(balance=training.data$balance,pred.train.lin=pred.train.lin) # make it a data frame fro plotting
ggplot() + geom_point(data = pred.train.lin.df, aes(x=balance, y=pred.train.lin, col=training.data$default)) + geom_hline(yintercept = 0) + geom_hline(yintercept = 1) + ylim(-15,2) # Plot. 

pred.train.probs = predict(glm.fit, type = "response") # With type = "response", we get the response variable scale, i.e., the probabilities.
pred.train.probs.df <- data.frame(balance=training.data$balance,pred.train.probs=pred.train.probs) # make it a data frame fro plotting
ggplot() + geom_point(data = pred.train.probs.df, aes(x=balance, y=pred.train.probs, col=training.data$default)) + geom_hline(yintercept = 0) + geom_hline(yintercept = 1) # Plot. 

# Predicting the TEST data PROBABILITIES
pred.test.probs = predict(glm.fit, test.data, type = "response")
pred.test.probs.df <- data.frame(balance=test.data$balance,pred.test.probs=pred.test.probs) # make it a data frame fro plotting
ggplot() + geom_point(data = pred.test.probs.df, aes(x=balance, y=pred.test.probs, col=test.data$default), size=5) + geom_hline(yintercept = 0) + geom_hline(yintercept = 1) + geom_hline(yintercept = 0.5, linetype="dashed") + ylim(0,1)

# Predicting the TEST data CLASSES
pred.test.classes = rep("No",nrow(test.data)) # In order to predict the classes, we must convert the predicted into class labels, Yes or No. We start by converting all to No.
pred.test.classes[pred.test.probs > 0.5] = "Yes"  # Now we set those to Yes whose proobability is greater than 0.5.
pred.test.classes.df <- data.frame(balance=test.data$balance,pred.test.classes=pred.test.classes) # make it a data frame for plotting
ggplot() + geom_point(data = pred.test.classes.df, aes(x=balance, y=pred.test.classes, col=test.data$default), size=5)

# Confusion matrix  
table(test.data$default, pred.test.classes)

# Calculating the validation error rate (percentage of incorrectly classified samples) as an estimate of the test error rate
mean(pred.test.classes != test.data$default)

# Predicting probabilities and classes for a balance of 1000 and 2000 Dollars:
new.data <- data.frame(student = c("No", "No"), balance= c(1000, 2000), income=c(1000, 2000)) # student and income are arbitrarily set, since they will not be used by predict
predict(glm.fit, newdata = new.data, type = "response")


# Logistic Regression with >1 PREDICTORS (including qualitative predictors)

# Fitting the model to the training data 
glm.fit <- glm(default~balance + student + income, family = "binomial", data = Default) 
summary(glm.fit)

# Predicting probabilities and classes for a balance of 1000 and 2000 Dollars:
new.data <- data.frame(student = c("No", "No"), balance= c(1000, 2000), income=c(1000, 2000)) # student and income are arbitrarily set, since they will not be used by predict
predict(glm.fit, newdata = new.data, type = "response")