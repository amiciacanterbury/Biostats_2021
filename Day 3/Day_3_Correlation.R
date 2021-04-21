# Chapter 9: Correlation
# 21 April 2021
# Author: Amicia Canterbury

#-------------------------INTRODUCTION------------------------------------------

# Assumptions for correlation: 
#  .pair-wise data
#  .absence of outliers
#  .linearity
#  .normality of distribution
#  .homoscedasticity
#  .level (type) of measurement
#    .Continuous data (Pearson correlation)
#    .Ordinal data (Spearman correlation)

# Load libraries

library(tidyverse)
library(ggpubr)
library(corrplot)
library(readr)
library(lubridate)
library(plotly)

# Load data
ecklonia <- read_csv("data/ecklonia.csv")
View(ecklonia)

# Using the select function to exclude/prevent columns to be read in
ecklonia_sub <- ecklonia %>% 
  select(-species, - site, - ID)

#-------------------PEARSON CORRELATION-----------------------------------------

# We use this type of this correlation for continuous data. 
# You comparing variables between same data set.

# Dollar sign means you are specifying the column 

# Perform correlation analysis on two specific variables
# Note that we do not need the final two arguments in this function to be stated
# as they are the default settings.
# They are only shown here to illustrate that they exist.

cor.test(x = ecklonia$stipe_length, ecklonia$frond_length, # This is used for 2
         use = "everything", method = "pearson")

# You look for correlation value, closer to 1, the stronger the correlation.
# In the case of the ecklonia data, the correlation = 0.6524911.
# This means it is highly correlated due to it being higher than 0.5. 

# What if you want to correlate all the data? 
# In order to correlate you only need all the values.
# Pearson is the default, no need to install.

ecklonia_pearson <- cor(ecklonia_sub)
ecklonia_pearson

view(ecklonia_pearson)

#------------------------------SPEARMAN RANK CORRELATION------------------------

# Use Spearman rank correlation when using ordinal data.
# Data is always continuous so Spearman rank correlation is hardly used.

# Create ordinal data
ecklonia$length <- as.numeric(cut((ecklonia$stipe_length+ecklonia$frond_length), breaks = 3))

# Run test on any variable
cor.test(ecklonia$length, ecklonia$digits)

#-----------------------------KENDALL RANK CORRELATION--------------------------

# Use Kendall correlation for both continuous and ordinal data. 
# Main purpose = to perform a correlation on non-normally distributed data.

ecklonia_norm <- ecklonia_sub %>% 
  gather(key = "variable") %>% 
  group_by(variable) %>% 
  summarise(variable_norm = as.numeric(shapiro.test(value)[2]))
ecklonia_norm

# From this analysis we may see that the values for primary blade length are 
# not normally distributed. In order to make up for this violation of our 
# assumption of normality we may use the Spearman test.

cor.test(ecklonia$primary_blade_length, ecklonia$primary_blade_width, 
         method = "kendall")

#-----------------------ONE PANEL VISUAL----------------------------------------

# Calculate Pearson r beforehand for plotting
r_print <- paste0("r = ", 
                  round(cor(x = ecklonia$stipe_length, ecklonia$frond_length),2))

# Then create a single panel showing one correlation
ggplot(data = ecklonia, aes(x = stipe_length, y = frond_length)) +
  geom_smooth(method = "lm", colour = "royalblue", se = F) +
  geom_point(colour = "black", shape = 23, size = 3)+
  geom_label(x = 300, y = 240, label = r_print) +
  labs(x = "Stipe length (cm)", y = "Frond length (cm)",
       title = "Relationship between frond length & stipe length") +
  theme_grey()

#-------------------------MULTIPLE PANEL VISUAL---------------------------------

corrplot(ecklonia_pearson, method = "circle")

#-------------------------EXCERCISE---------------------------------------------

install.packages("reshape")
library(reshape)

ecklonia_melt <- melt(ecklonia_pearson)
head(ecklonia_melt)
ggp <- ggplot(ecklonia_melt, aes(X1, X2))+
  geom_tile(aes(fill = value))+
ggp
ggp + scale_fill_gradient(low = "forestgreen", high = "gold3")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  labs(x = "",y ="",title = expression(paste("Relationship of variables between" , 
                      italic(" Ecklonia"), paste(sp.))))
       

# Second method: 
heatmap(ecklonia_pearson)

heatmap(ecklonia_pearson, Rowv = NA, Colv = NA)
  
  


