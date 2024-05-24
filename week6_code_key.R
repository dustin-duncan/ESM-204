# ESM204 Week 6: estimating the demand
# Thank you to Vincent Thivierge for the dataset and basic code

## set up environment 

rm(list=ls()) #delete
options(scipen=999) # not scientific notation

#Packages
library(tidyverse)
library(here)

## Load data
demand_data <- read.csv("data/ESM204_week6_data.csv", stringsAsFactors = F) 

demand_data$price

str(demand_data) # data structure. Another option is glimpse
head(demand_data)# top values
tail(demand_data) # bottom values

## Let's plot the data 

plot_1 <- ggplot(data=demand_data, aes(x=quantity,y=price))+
  geom_point() +
  theme_bw()

plot_1

## Estimate linear model 
## the command has an implied intercept

model_demand <- lm(price ~ quantity, data=demand_data)

model_demand$coefficients[[2]]

a <- model_demand$coefficients[[1]]
b <- model_demand$coefficients[[2]]

## Add estimated curve to our plot

price_fitted <- a + b*demand_data$quantity

demand_data$price_fitted <- price_fitted 

plot_1 + geom_point(aes(y=price_fitted, x=quantity, colour = "price fitted"))

plot_1 + geom_smooth(formula = y ~ x, method = "lm", se = F)



