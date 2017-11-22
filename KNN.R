rm(list=ls(all=TRUE))

library(dplyr)
library(tidyr)
library(caTools)
library(class)
library(ggplot2)

iris <- iris

iris_scaled <- iris %>% mutate_if(is.numeric, scale)

iris_variance <- iris %>% gather(Measure, Value, -Species) %>% group_by(Measure) %>% summarize(Variance = var(Value))
iris_scaled_variance <- iris_scaled %>% gather(Measure, Value, -Species) %>% group_by(Measure) %>% summarize(Variance = var(Value))

set.seed(101)
sample_split <- sample.split(iris_scaled$Species, 0.7)
iris_train <- subset(iris_scaled, sample_split==T)
iris_test <- subset(iris_scaled, sample_split==F)

iris_test_predicted <- knn(select(iris_train, -Species),
                           select(iris_test, -Species),
                           pull(select(iris_train, Species)),
                           k=1)

error_rate <- mean(iris_test$Species != iris_test_predicted)

# choose best k
iris_test_predicted <- NULL
error_rate <- NULL

for (i in 1:10) {
  
  set.seed(101)
  
  iris_test_predicted <- knn(select(iris_train, -Species),
                             select(iris_test, -Species),
                             pull(select(iris_train, Species)),
                             k=i)
  
  error_rate[i] <- mean(iris_test$Species != iris_test_predicted)
  
}

data.frame(k.value = 1:10, error.rate = error_rate) %>%
  ggplot(aes(k.value, error.rate)) + 
  geom_point() +
  geom_line(lty="dotted", color="red") +
  scale_x_continuous(breaks=seq(1:10)) +
  labs(title="The best k value is 2 to 6")
