rm(list=ls(all=TRUE))

library(ISLR)
library(dplyr)
library(tidyr)
library(ggplot2)
library(caTools)
library(rpart)
library(rpart.plot)
library(randomForest)

df <- College

ggplot(df, aes(Room.Board, Grad.Rate, color = Private)) + geom_point()
ggplot(df, aes(F.Undergrad, fill = Private)) + geom_histogram(color="black", binwidth=1000)
ggplot(df, aes(Grad.Rate, fill = Private)) + geom_histogram(color="black", binwidth=1)
df['Cazenovia College','Grad.Rate'] <- 100

set.seed(101)
sample_split <- sample.split(df$Private, 0.7)
df_training <- subset(df, sample_split==T)
df_test <- subset(df, sample_split==F)

tree <- rpart(Private ~ ., method="class", data=df_training)

printcp(tree)

df_test_predict_tree <- predict(tree, df_test)
table(df_test$Private, df_test_predict_tree[,'Yes'] > 0.5)

#options(scipen=999)
prp(tree)

forest <- randomForest(Private ~ ., data=df_training, importance=TRUE)
forest$confusion
forest$importance

df_test_predict_forest <- predict(forest, df_test)
table(df_test$Private, df_test_predict_forest)

data.frame(forest$err.rate) %>% 
  mutate(Trees = 1:500) %>%
  gather(Err.Measure, Error, -Trees) %>%
  ggplot(aes(Trees, Error, color=Err.Measure)) + 
  geom_point() +
  labs(title = "Random forest error is sufficiently low past 50 trees")


