# Chapter 7 - Snakes Homework
# 21 April 2021
# Biostatistics
# Author: Amicia Canterbury 

library(readr)
library(tidyverse)
library(ggplot2)
library(plotly)
library(lubridate)



snakes <- read_csv("data/snakes.csv")
snakes$day = as.factor(snakes$day) # As factor changes the factorial data 
# When you want to change the data of a specific column - group_by

view(snakes)

#The first thing we do is to create some summaries of the data. Refer to the summary statistics Chapter.

snakes.summary <- snakes %>% 
  group_by(day, snake) %>%  # Average's of everything, if you group by day = more sense because it will group by the day
  summarise(mean_openings = mean(openings),
            sd_openings = sd(openings)) %>% 
  ungroup()
snakes.summary

#To fix this problem, let us ignore the grouping by both snake and day.

snakes.summary <- snakes %>% 
  group_by(day) %>% 
  summarise(mean_openings = mean(openings),
            sd_openings = sd(openings)) %>% 
  ungroup()
snakes.summary

library(Rmisc)
snakes.summary2 <- summarySE(data = snakes, measurevar = "openings", groupvars = c("day"))

snakes.summary2

# Make plots:

ggplot(data = snakes, aes(x = day, y = openings)) +
  geom_segment(data = snakes.summary2, aes(x = day, xend = day, y = openings - ci, yend = openings + ci, colour = day),
               size = 2.0, linetype = "solid", show.legend = F) +
  geom_boxplot(aes(fill = day), alpha = 0.6, show.legend = F) + 
  geom_jitter(width = 0.05)+
  labs(x = "Day", y = "Openings", title = "Boxplot representing the amount of releases that occur during openings")+
  theme_bw()


#What are our null hypotheses?

#H0: There is no difference between snakes with respect to the number of openings at which they habituate.
#: There is no difference between days in terms of the number of openings at which the snakes habituate.

#Fit the ANOVA model to test these hypotheses:

snakes.aov <- aov(openings ~ day + snake, data = snakes)
summary(snakes.aov)

#Now we need to test of the assumptions hold true (i.e. erros are normally distributed and heteroscedastic). Also, where are the differences?

par(mfrow = c(2, 2))
# Checking assumptions...
# make a histogram of the residuals;
# they must be normal

snakes.res <- residuals(snakes.aov)
hist(snakes.res, col = "red")

# make a plot of residuals and the fitted values;
# # they must be normal and homoscedastic
plot(fitted(snakes.aov), residuals(snakes.aov), col = "red")

snakes.tukey <- TukeyHSD(snakes.aov, which = "day", conf.level = 0.90)
plot(snakes.tukey, las = 1, col = "red")

# Own plot: 

ggplot(data = snakes, aes(x = openings, y = day, fill = day)) +
  geom_bar(stat = "identity") +
  labs(x = "Openings", y = "Day") +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 90)) +
  ggtitle("Bar graph representing the amount of releases that occur during openings")+
  theme_bw()+
  theme(panel.border = element_blank(), legend.position = "none")
  

# Second plot: 

ggplot(data = snakes, aes(x = day, y = openings, fill = snake))+
  geom_col(position = "dodge", col = "black")+
  labs(x = "Day", y = "Openings", title = "Showing the relationship between each snakes and the amount of releases",
       fill = "Snake")+
  theme_bw()+
  scale_fill_manual(values =c("navy blue", "cornflower blue", "blue", "cyan1", 
                                "cadetblue2", "skyblue"))


#scale_fill_brewer(pallet = "set3")

# scale_fill_gradient(low = "yellow", high = "red", na.value = NA)  - try it 
# on a temp. scale 

  
  
 
  


