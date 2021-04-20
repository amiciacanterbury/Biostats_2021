# Day 2 
# Chapter 7 - ANOVA 
# Biostatistics
# 20 April 2021

#As t-tests, ANOVAs require that some assumptions are met:

#Normally distributed data
#Homogeneity of variances
#Independence of data
#In our case, we will encourage also that the data are balanced

library(lubridate)
library(plotly)
library(tidyverse)
library("ggpubr")

chicks <- as_tibble(ChickWeight)

chicks_sub1 <- chicks %>% 
  filter(Diet %in% c(1, 2), Time == 2)

compare_means(weight ~ Diet, data = chicks_sub1, method = "t.test")

chicks.aov1 <- aov(weight ~ Diet, data = filter(chicks, Time == 21))
summary(chicks.aov1)

#Multiple factors: 

summary(aov(weight ~ Diet, data = filter(chicks, Time %in% c(0))))

# Go through the rest by yourself
# Homework = Snakes - add a separate graph of your own 
# Also add extra things in between 

