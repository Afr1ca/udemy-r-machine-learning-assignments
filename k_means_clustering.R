# remove all variables; start with a clean slate
rm(list=ls(all=TRUE))

# load libraries
library(dplyr)
library(ggplot2)
library(cluster)

# read in red and white wine data
red <- read.csv("data/winequality-red.csv", sep=";")
white <- read.csv("data/winequality-white.csv", sep=";")
colnames(red) == colnames(white)

# combine into one data frame
red <- mutate(red, color="red")
white <- mutate(white, color="white")
wine <- rbind(red, white)
wine <- mutate(wine, color = fct_rev(factor(color)))
summary(wine)

# add custom color palette
wine_palette = c("#f1f285", "#990012")

# create some exploratory charts
ggplot(wine, aes(residual.sugar, fill=color)) + 
  geom_histogram(color="black", bins=50) +
  scale_fill_manual(values=wine_palette)
ggplot(wine, aes(citric.acid, fill=color)) + 
  geom_histogram(color="black", bins=50) +
  scale_fill_manual(values=wine_palette)
ggplot(wine, aes(alcohol, fill=color)) + 
  geom_histogram(color="black", bins=50) +
  scale_fill_manual(values=wine_palette)
ggplot(wine, aes(citric.acid, residual.sugar, color=color)) + 
  geom_point(alpha=0.1) +
  scale_color_manual(values=wine_palette)
ggplot(wine, aes(volatile.acidity, residual.sugar, color=color)) + 
  geom_point(alpha=0.1) +
  scale_color_manual(values=wine_palette)

# seaparate out numeric values
cluster_data <- select(wine, -color)
head(cluster_data)

# cluster with two centers
wine_clusters <- kmeans(cluster_data, 2)
wine_clusters$centers

# check accuracy of results
# note that usually this is not possible with unsupervised learning
table(wine$color, wine_clusters$cluster)

# visualize the cluster results
clusplot(wine, wine_clusters$cluster, color=T, shade=T, labels=0, lines=0)

