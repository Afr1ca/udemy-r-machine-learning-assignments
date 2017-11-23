# remove all variables; start with a clean slate
rm(list=ls(all=TRUE))

# load libraries
library(dplyr)
library(ggplot2)
library(forcats)
library(caTools)
library(e1071)

# read in Lending Club loan data
loans_raw <- read.csv("data/loan_data.csv")
str(loans_raw)
summary(loans_raw)

# make certain columns factors
factor_list <- c("inq.last.6mths",
                 "delinq.2yrs",
                 "pub.rec",
                 "not.fully.paid",
                 "credit.policy")
loans <- loans_raw %>%
  mutate_at(factor_list, factor)

# create some exploratory charts
ggplot(loans, aes(fico, fill=not.fully.paid)) + 
  geom_histogram(color="black")
ggplot(loans, aes(fct_infreq(purpose), fill=not.fully.paid)) + 
  geom_bar(position="dodge")
ggplot(loans, aes(int.rate, fico, color=days.with.cr.line)) + 
  geom_point(alpha=0.5) +
  scale_color_distiller(palette="Spectral")

# split data into training and test data
sample_split <- sample.split(loans$not.fully.paid, 0.7)
loans_training <- subset(loans, sample_split==T)
loans_test <- subset(loans, sample_split==F)

# create a support vector machines model
model <- svm(not.fully.paid ~ ., data=loans_training)
summary(model)

# predict the outcomes of the test data
loans_predictions <- predict(model, select(loans_test, -not.fully.paid))

# check the accuracy of the model with a confusion matrix
table(loans_test$not.fully.paid, loans_predictions)

# tune results of the model to improve accuracy
tune_results <- tune(svm, 
                     train.x=not.fully.paid~., 
                     data=loans_training, 
                     kernal="radial", 
                     ranges=list(
                       cost=c(100, 200),
                       gamma=c(0.1)
                     ))
summary(tune_results)

# create a tuned model with params: cost=100 and gamma=0.1
tuned_model <- svm(not.fully.paid~., data=loans_training, cost=100, gamma=0.1)

# check tuned model accuracy on test data
loans_predictions_tuned <- predict(tuned_model, select(loans_test, -not.fully.paid))
table(loans_test$not.fully.paid, loans_predictions_tuned)



