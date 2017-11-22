rm(list=ls(all=TRUE))

# load libraries
library(dplyr)
library(lubridate)
library(ggplot2)
library(RColorBrewer)
library(magrittr)
library(modelr)

# import data
bike_raw <- read.csv("data/bikeshare.csv")

# clean data
bike <- bike_raw %>%
  mutate(datetime = ymd_hms(datetime),
         season = factor(season, c(1,2,3,4), c("Spring", "Summer", "Fall", "Winter")),
         weather = factor(weather, c(1,2,3,4), c("Clear", "Cloudy", "Light precip", "Heavy precip")),
         workingday = factor(workingday, c(0,1), c("Non-working day", "Working day")),
         holiday = factor(holiday, c(0,1), c("Non-holiday", "Holiday")))

ggplot(bike, aes(temp, count, color=temp)) + geom_point(alpha=0.3) + theme_minimal()

ggplot(bike, aes(datetime, count, color=temp)) + geom_point(alpha=0.3) + theme_minimal()

cor(bike$temp, bike$count)

ggplot(bike, aes(season, count, color=season)) + geom_boxplot() + theme_minimal()

bike %>% 
  mutate(hour=hour(datetime)) %>%
  ggplot(aes(hour, count, color=temp)) +
  geom_jitter(alpha=0.3) +
  theme_minimal() +
  facet_wrap(~workingday) +
  scale_x_continuous(breaks=seq(0,23)) +
  scale_color_distiller(palette="Spectral")

temp_model <- lm(count ~ temp, data=bike)
summary(temp_model)

temp_predictions <- data_grid(bike, temp)
temp_predictions$count <- predict(temp_model, temp_predictions)

ggplot(bike, aes(temp, count)) + geom_point() + geom_line(data=temp_predictions)

temp_single_prediction <- predict(temp_model, data.frame(temp = c(25)))

model <- lm(count ~ . - datetime - atemp - casual - registered, bike)
summary(model)