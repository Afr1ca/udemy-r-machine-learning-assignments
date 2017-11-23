# remove all variables; start with a clean slate
rm(list=ls(all=TRUE))

# load libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(caTools)
library(neuralnet)
library(stringr)
library(randomForest)

# read in data
df <- read.csv("data/bank_note_data.csv")
summary(df)

# visualize data
df %>% 
  gather(Metric, Value, -Class) %>%
  ggplot(aes(Value, fill=factor(Class))) +
  geom_histogram() +
  facet_wrap(~Metric)
  
# split data into training and test sets
set.seed(101)
sample_split <- sample.split(df$Class, 0.7)
training <- subset(df, sample_split==T)
test <- subset(df, sample_split==F)

# create a neural net model
factors <- str_c(names(select(df, -Class)), collapse= "+")
net <- neuralnet(str_c("Class~",factors), 
                 data=training, 
                 hidden=10, 
                 linear.output=FALSE)
plot(net)

# create predictions and create a confusion matrix
predictions <- compute(net, select(test, -Class))
predictions <- sapply(predictions$net.result, round)
table(test$Class, predictions)

# create a random forest model
training$Class <- factor(training$Class)
test$Class <- factor(test$Class)
forest <- randomForest(Class~., data=training)

# create predictions and create a confusion matrix
predictions2 <- predict(forest, select(test, -Class))
table(test$Class, predictions2)

# both the neural net model and the random forest models are good predictors
