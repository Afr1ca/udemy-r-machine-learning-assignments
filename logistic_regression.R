rm(list=ls(all=TRUE))

library(dplyr)
library(ggplot2)
library(forcats)
library(Amelia)
library(caTools)

adult_raw <- read.csv("data/adult_sal.csv")
adult <- select(adult_raw,-X)

head(adult)
str(adult)
summary(adult)

adult$type_employer <- fct_collapse(adult$type_employer,
                            "Unemployed" = c("Without-pay", "Never-worked"),
                            "SL-gov" = c("State-gov", "Local-gov"),
                            "Self-emp" = c("Self-emp-inc", "Self-emp-not-inc"))
 
adult$marital <- fct_collapse(adult$marital,
                              "Married" = c("Married-civ-spouse", "Married-spouse-absent", "Married-AF-spouse"),
                              "Not-Married" = c("Divorced", "Separated", "Widowed"),
                              "Never-Married" = c("Never-married"))

country_continent <- read.csv("data/countries_continents.csv")
colnames(country_continent) <- c("continent", "country")

adult_countries <- data.frame(country=unique(adult$country)) %>% left_join(country_continent, by = "country")

adult_countries_NA <- adult_countries %>% filter(is.na(continent)==TRUE)
adult_countries_NA$continent <- c("North America", "?", "?", "North America", "Europe", "South America", "Asia", "North America", "North America", "Europe", "North America", "Europe", "North America", "Asia", "Europe")

adult_countries <- adult_countries %>% filter(is.na(continent) == FALSE) %>% rbind(adult_countries_NA)

adult <- adult %>% left_join(adult_countries, by = "country")
adult$continent <- fct_other(adult$continent, drop=c("?", "Africa", "Oceania"))

adult$country <- factor(adult$country)
duration(adult$age)

adult[adult=="?"] <- NA

adult <- na.omit(adult)

ggplot(adult, aes(age, fill=fct_rev(income))) + 
  geom_histogram(color="black", bins=diff(range(adult$age))) +
  scale_x_continuous(breaks=seq(0,100,10) , minor_breaks=seq(min(adult$age), max(adult$age))) +
  theme_minimal()

ggplot(adult, aes(fct_infreq(continent), fill=income)) + geom_bar()

set.seed(101)

sample <- sample.split(adult$income, SplitRatio=0.7)
train <- subset(adult, sample==T)
test <- subset(adult, sample==F)

model <- glm(income ~ . -country, family=binomial(link='logit'), data=train)
summary(model)

step_model <- step(model)

test$predicted.income <- predict(step_model, newdata=test, type="response")

table(test$income, test$predicted.income > 0.5)

accuracy <- (6287+1349) / length(test$income)

# how useful the search results are
precision <- 1349 / (1349+509)

# how complete the results are
recall <- 1349 / (1349+903)

