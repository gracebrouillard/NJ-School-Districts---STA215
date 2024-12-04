## Load packages
# NOTE: Run base.R if these commands return an error!
library(readr)
library(dplyr)
library(ggplot2)
library(haven)
library(psych)

# set working directory
setwd("~/Desktop/STA215")

# Load data 
data <- read.csv("raw_data.csv")

#create table 2 contingency table
table(data$funding, data$salary)

#create table 1 descriptive statistics 
table(data$population)
describe(data$population)

table(data$gradiation)
describe(data$graduation)

table(data$funding)
mean(data$funding)

table(data$salary)


#create figure 1 - boxplot 
lm(funding ~ salary, data = data)
aov(funding ~ salary, data = data)
summary(funding ~ salary, data = data)
boxplot(funding ~ salary, data = data)

lm(funding ~ graduation, data = data)
aov(funding ~ graduation, data = data)
summary(funding ~ graduation, data = data)

#create figure 2 - scatter plot
linear_plot <- plot(data$graduation, data$funding)
print(linear_plot)
meany <- mean(data$graduation)
meanx <- mean(data$funding)
abline(h = meanx, col = "black")
abline(v = meany, col = "black")
linear_relationship <- lm(funding ~ graduation, data = data)
summary(linear_relationship)
abline(linear_relationship, col = "red")

linear_plot <- plot(data$population, data$funding)
print(linear_plot)
meany <- mean(data$population)
meanx <- mean(data$funding)
abline(h = meanx, col = "black")
abline(v = meany, col = "black")
linear_relationship <- lm(funding~ population, data = data)
summary(linear_relationship)
abline(linear_relationship, col = "red")

#create figure 3 - residual plot 
linear_plot <- plot(data$population, data$gradiation)
print(linear_plot)

# add x line and y line for means
meany <- mean(data$population)
meanx <- mean(data$graduation)

linear_relationship <- lm(graduation ~ population, data = data)
summary(linear_relationship)

# Plot the residuals
plot(data$graduation, residuals(linear_plot))
plot(data$population, residuals(linear_plot))

# Add a horizontal line at zero to indicate the baseline
abline(h = 0, col = "red")
