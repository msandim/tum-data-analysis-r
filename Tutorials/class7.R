#readRDS()
library(ggplot2)
library(data.table)
library(gridExtra)

# Question 4
data(mtcars)
mtcars

#IQR(mtcars$mpg)
#ggplot(mtcars, aes())

mtcars <- as.data.table(mtcars)
Quantiles <- mtcars[, .(lq = quantile(mpg, 0.25), uq = quantile(mpg, 0.75)), by = cyl]

iqr <- mtcars[, .(IQR = 1.5 * IQR(mpg)), by = cyl]
Quantiles <- merge(Quantiles, iqr, by = 'cyl')
Quantiles <- Quantiles[, up_IQR := uq + IQR]
Quantiles <- Quantiles[, down_IQR := lq - IQR]
#Quantiles <- Quantiles

# Question 5
titanic <- fread("Titanic.csv")

# ver os boxplots de sex e age para o survived
titanic$Survived <- as.factor(titanic$Survived)

ggplot(titanic, aes(Survived, Age)) + 
  geom_boxplot() +
  geom_jitter(width = 0.2)

ggplot(titanic, aes(Survived, Age)) + 
  geom_boxplot() + 
  geom_jitter(width = 0.2) +
  facet_wrap(~ Pclass)

ggplot(titanic, aes(Survived, fill = factor(SibSp))) + 
  geom_bar()

# o fill normaliza o count entre 0 e 1
ggplot(titanic, aes(Survived, fill = factor(SibSp))) + 
  geom_bar(position = "fill")

ggplot(titanic, aes(Survived, fill = factor(SibSp))) + 
  geom_bar(position = "fill") +
  facet_wrap(~ Sex)

# Question 6
library(tidyr)

election <- fread("US_election2016.csv")
states <- map_data("state") %>% as.data.table
election[, region := tolower(State)]
states <- merge(states, election, by = "region")
states[, Clinton_per := as.numeric(sub("%", "", Clinton_per))]
