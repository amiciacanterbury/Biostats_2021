# Chapter 8: Linear regressions
# 21 April 2021
# Author: Amicia Canterbury

library(tidyverse)

# Regressions test the statistical significance of the dependence of one 
# continuous variable on one or many independent continuous variables.

head(faithful)
tail(faithful)
summary(faithful)
glimpse(faithful)
unique(faithful)

# Here we fit the model in R. When we perform a linear regression in R, 
# it will output the model and the coefficients:
  
  eruption.lm <- lm(eruptions ~ waiting, data = faithful)
summary(eruption.lm)

# A graph of the linear regression

slope <- round(eruption.lm$coef[2], 3)

# p.val <- round(coefficients(summary(eruption.lm))[2, 4], 3) # it approx. 0, so...

p.val = 0.001
r2 <- round(summary(eruption.lm)$r.squared, 3)

# Make a plot: 

ggplot(data = faithful, aes(x = waiting, y = eruptions)) +
  geom_point(shape = 18) +
  annotate("text", x = 45, y = 5, label = paste0("slope == ", slope, "~(min/min)"), parse = TRUE, hjust = 0) +
  annotate("text", x = 45, y = 4.75, label = paste0("italic(p) < ", p.val), parse = TRUE, hjust = 0) +
  annotate("text", x = 45, y = 4.5, label = paste0("italic(r)^2 == ", r2), parse = TRUE, hjust = 0) +
  stat_smooth(method = "lm", colour = "blue") +
  labs(title = "Old Faithful eruption data",
       subtitle = expression(italic("Linear regression")),
       x = "Waiting time (minutes)",
       y = "Eruption duration (minutes)")+
  theme_bw()

#title = expression(paste("Relationship of variables between" , 
  #                       italic(" Ecklonia")
# Predicting from the linear model:

# use the accessor function to grab the coefficients:

erupt.coef <- coefficients(eruption.lm)
erupt.coef

# how long would an eruption last of we waited, say, 80 minutes?
waiting <- 80 

# the first and second coef. can be accessed using the 
# square bracket notation:

erupt.pred <- erupt.coef[1] + (erupt.coef[2] * waiting)
erupt.pred # the unit is minutes

# There is another way to do this. The predict() function takes a dataframe 
# of values for which we want to predict the duration of the eruption and 
# returns a vector with the waiting times:

pred.val <- data.frame(waiting = c(60, 80, 100))
predict(eruption.lm, pred.val) # returns waiting time in minutes

# The coefficient of determination, r2:

summary(eruption.lm)$r.squared

library(tidyverse)
n <- 100
set.seed(666)
rand.df <- data.frame(x = seq(1:n),
                      y = rnorm(n = n, mean = 20, sd = 3))

# Make a plot: 

ggplot(data = rand.df, aes(x = x, y = y)) +
  geom_point(colour = "yellow") +
  stat_smooth(method = "lm", colour = "black", size = 0.75, fill = "green", alpha = 0.3) +
  labs(title = "Temperature during winter",
       subtitle = "Linear regression",
       x = "Days",
       y = "Temperature (°F)")+
  theme_bw()

