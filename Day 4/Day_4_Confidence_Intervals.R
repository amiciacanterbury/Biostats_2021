# Day 4 - Confidence Intervals
# 22 April 2021
# Biostatistics 
# Author: Amicia Canterbury 

library(tidyverse)
library(corrplot)
library(ggpubr)
library(lubridate)
library(ggplot2)
library(Rmisc)
library(dplyr)
library(ggrepel)
library(dslabs)
library(reshape)
library(plotly)

library(rcompanion) # NEW NEW 

#--------------------CONFIDENCE INTERVALS---------------------------------------

# A confidence interval (CI) tells us within what range we may be certain to 
# find the true mean from which any sample has been taken. If we were to 
# repeatedly sample the same population over and over and calculated a mean 
# every time, the 95% CI indicates the range that 95% of those means would fall 
# into.

# Calculating the confidence intervals: 

Input <- ("
Student  Sex     Teacher  Steps  Rating
a        female  Jacob    8000   7
b        female  Jacob    9000  10
c        female  Jacob   10000   9
d        female  Jacob    7000   5
e        female  Jacob    6000   4
f        female  Jacob    8000   8
g        male    Jacob    7000   6
h        male    Jacob    5000   5
i        male    Jacob    9000  10
j        male    Jacob    7000   8
k        female  Sadam    8000   7
l        female  Sadam    9000   8
m        female  Sadam    9000   8
n        female  Sadam    8000   9
o        male    Sadam    6000   5
p        male    Sadam    8000   9
q        male    Sadam    7000   6
r        female  Donald   10000  10
s        female  Donald    9000  10
t        female  Donald    8000   8
u        female  Donald    8000   7
v        female  Donald    6000   7
w        male    Donald    6000   8
x        male    Donald    8000  10
y        male    Donald    7000   7
z        male    Donald    7000   7
")

data <- read.table(textConnection(Input),header = TRUE)
summary(data)

str(data) # When you want to look at the data the way it is in the textbook.

library(rcompanion)

# ungrouped data is indicated with a 1 on the right side of the formula, 
# or the group = NULL argument.

groupwiseMean(Steps ~ 1,data = data, conf = 0.95, digits = 3)

?groupwiseMean # Calculates the means & confidence intervals for groups. 

groupwiseMean(Steps ~ 1,data = data, conf = 0.95, digits = 3, 
              traditional = FALSE) # Used to see only the mean 


# one-way data
data_1 <-groupwiseMean(Steps ~ Sex, data = data, conf = 0.95,digits = 3)

# two-way data
 groupwiseMean(Steps ~ Teacher + Sex, data = data, conf = 0.95,digits = 3)
 
# Make a plot with one-way data 
 
ggplot(data = groupwiseMean(Steps ~ Sex, data = data, conf = 0.95,digits = 3))+
  geom_col()

ggplot(data = data_1)+
  geom_col(aes(x = Sex, y = Mean, fill = Sex), col = "black")+
  scale_fill_manual(values =c("seagreen3","yellow"))+
  geom_errorbar(aes(ymin = Trad.lower,
                    ymax = Trad.upper, 
                    x = Sex),
                col = "black", 
                width = 0.2 )+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  labs(x = "Sex", y = "Total steps (cm)", title = "The total amount of steps between males and females")+
  theme_grey()

# Plot number 2 

data_2 <- groupwiseMean(Steps ~ Teacher + Sex, data = data, confidence = 0.95, digits = 3)


ggplot(data = data_2)+
  geom_col(aes(x = Sex, y = Mean, fill = Sex))+
  geom_errorbar(aes(ymin = Trad.lower,
                    ymax = Trad.upper, 
                    x = Sex),
                col = "black", 
                width = 0.2) +
  facet_wrap(~ Teacher, ncol = 3)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  labs(x = "Sex", y = "Total steps (cm)", title = "Mean steps of different classes",
       subtitle = "Donald, Jacob, Sadam")+
  theme(legend.position = "remove")+
  theme_bw()+
  scale_fill_manual(values =c("navy blue", "cornflower blue"))
 
# Neither gender nor teachers influence has any effect on the total number 
# of steps they take 


# by bootstrapping:

groupwiseMean(Steps ~ Sex, data = data, conf = 0.95, digits = 3, R = 1000,
              boot = TRUE, traditional = FALSE, normal = FALSE, basic = FALSE,
              percentile = FALSE, bca = TRUE)

# One would do a two-way anova test:
anova <- aov(Steps ~ Sex * Teacher, data = data)
summary(anova)


# TukeyHSD anova

anova_Tukey <- TukeyHSD(anova)

plot(anova_Tukey, col = "darkorange")


