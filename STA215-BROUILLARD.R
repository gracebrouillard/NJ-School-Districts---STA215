## Load packages
# NOTE: Run base.R if these commands return an error!
library(readr)
library(dplyr)
library(ggplot2)
library(haven)
library(psych)
library(ggplot2)
library(forcats)
library(tidyselect)

# set working directory
setwd("~/Desktop/STA215")

# Load data 
data <- read.csv("raw_data.csv")

#create table 2: contingency table
table(data$funding, data$salary)
chisq.test(table(data$funding, data$salary))

#create table 1: descriptive statistics 
table(data$population)
describe(data$population)

table(data$gradiation)
describe(data$graduation)

table(data$funding)
describe(data$funding)

table(data$salary)
describe(data$salary)


#create figure 1 - boxplot 
lm(salary ~ graduation, data = data)
aov(salary ~ graduation, data = data)
summary(salary ~ graduation, data = data)
boxplot(salary ~ graduation, data = data)


lm(funding ~ graduation, data = data)
aov(funding ~ graduation, data = data)
summary(funding ~ graduation, data = data)
boxplot(funding ~ graduation, data = data)


#create figure 2 - scatter plot
linear_plot <- plot(data$population, data$graduation)
print(linear_plot)
#add x line and y line for means 
meany <- mean(data$graduation, na.rm = TRUE)
meanx <- mean(data$population, na.rm = TRUE)

abline(v = meanx, col = "black")
abline(h = meany, col = "black")

linear_relationship <- lm(population ~ graduation, data = data)
summary(linear_relationship)
abline(linear_relationship, col = "red")

#create figure 3 - residual plot
# Plot the residuals
plot(data$graduation, na.rm = TRUE, residuals(linear_plot))
plot(data$population, residuals(linear_plot))

# Add a horizontal line at zero to indicate the baseline
abline(h = 0, col = "red")
hist(data$population)


