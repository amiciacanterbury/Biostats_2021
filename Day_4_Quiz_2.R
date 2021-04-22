# Day 4 - Quiz 2 - Data manipulation, analyses & visualisation
# 22 April 2021
# Biostatistics
# Author: Amicia Canterbury 


#-------------------------QUESTION 1--------------------------------------------

# Load in the necessary packages and explore three built in dataset (see below) 
# by writing hypotheses and selecting the
# correct statistical tests to prove or disprove the hypotheses.

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
library(rcompanion)

ls("package:datasets") # Shows the different datasets

Orange <- datasets::Orange
ToothGrowth <- datasets::ToothGrowth
warpbreaks <- datasets::warpbreaks


# Orange dataset: 

summary(Orange)
unique(Orange)
head(Orange)
tail(Orange)
view(Orange)

# Hypothesis: The older the orange tree's are, the larger the circumference.

Orange %>%  
  summarise(min_circum = range(circumference)[1],
            max_cirum = range(circumference)[2]) 

Orange %>%  
  summarise(quantile(circumference))

view(Orange)


ggplot(data = Orange, aes(x = Tree, y = circumference))+
  geom_histogram(colour = "green", fill = "white")+
  labs(x = "Tree", y = "Circumference (cm)", title = "The relationship between age & circumference")
  theme_bw()


# filter the data
orange_age <- Orange %>% 
  

Orange %>% 
  summarise(mean_circum = mean(circumference))

Orange %>% 
  summarise(std_circum = sd(circumference))

Orange %>% 
  summarise(min_circum = min(circumference),
            qrt1_circum = quantile(circumference, p = 0.25),
            med_circum = median(circumference),
            qrt3_circum = median(circumference, p = 0.75),
            max_circum = max(circumference))
Orange %>% 
  summarise(mean_age = mean(age))

Orange %>% 
  summarise (std_age = sd(age))

Orange %>% 
  summarise(min_age = min(age),
            qrt1_age = quantile(age, p = 0.25),
            med_age = median(age),
            qrt3_age = median(age, p = 0.75),
            max_age = max(age))

orange_data <- data.frame(data = c(rnorm(n = 35, mean = 115.9, sd = 57.48818),
                            rnorm(n = 35, mean = 922.1429, sd = 491.8645)),
                    sample = c(rep("Circumference", 35), rep("Age", 35)))

orange_plot <- ggplot(data = orange_data, aes(x = data,fill = sample)) +
  scale_color_manual(values = c("Green", "Blue")) +
  geom_histogram(position = "dodge", binwidth = 1, alpha = 0.8) +
  geom_density(aes(y = 1*..count.., fill = sample), colour = NA, alpha = 0.4) +
  labs(x = "Age" , y = "Circumference", 
       title = "Histrogram representing Sample A and B")+
  theme_bw()

orange_plot

# Toothgrowth data: 

summary(ToothGrowth)
unique(ToothGrowth)
head(ToothGrowth)
tail(ToothGrowth)
view(ToothGrowth)

# Hypothesis: The higher the dosage of supplement, the longer the length of the tooth.


# Warp beaks data:
summary(warpbreaks)
unique(warpbreaks)
head(warpbreaks)
tail(warpbreaks)
view(warpbreaks)

?warpbreaks

# Hypothesis: the longer the length of the loom, the more breaks found

ggplot(data = warpbreaks, col = wool)+
  geom_boxplot(aes(x = tension, y = breaks))+
  labs(x = "Tension", y = "Breaks", title = "Wool breaks found in wool A & B")
     

#---------------------QUESTION 2------------------------------------------------

# Analyse the SACTN dataset to compare if monthly differences exist in 
# Sea Surface Temperature (SST) between each of the sites and source.

summary(SACTN_daily_v4.2)
unique(SACTN_daily_v4.2)
head(SACTN_daily_v4.2)
tail(SACTN_daily_v4.2)
view(SACTN_daily_v4.2)

SACTN_grp <- SACTN_daily_v4.2 %>%
  mutate(yr = year(date) , 
         mo = month(date))%>%
  group_by(index, mo) %>%
  summarise(mean_temp = mean(temp, na.r = TRUE)) %>%
  ungroup() 


ggplot(SACTN_grp, aes(x = mo, y = mean_temp)) +
  geom_line(aes(group = index), colour = "salmon") +  
  labs (x = "Month" , y = " Sea Surface Temperature (°C)" ,
        title = "Monthly Sea Surface Temperature")




