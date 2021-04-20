# Day 2 - Quiz 
# Biostatistics
# Author: Amicia Canterbury
# Date: 20 April 2021

# Question 1.1: List the various data classes and give explanation and example of each 

# Numerical data - objects that are counted objectively in nature 
# Within Numerical data there are more classes: 
# 1: Nominal data (discrete data): used for integers or whole numbers when 
#                                   counting (E.g. counting how many trees
#                                             are in a certain area)
# 2: Continunous data: used when trying to explain measurements or rational numbers 
#                                   (E.g. temperature of heat, weight of dry mass etc.)
# 3: Dates: seen as a special class of data (E.g. years, months etc)

# Qualitative data - used when describing a category 
# Within Qualitative data: 
# 1. Categorical data: used when wanting to categorize data (E.g. male & female)
# 2. Ordinal data: used when wanting to put date according to specific order 
#                       (E.g. first place, second place, last place)

# Question 1.2: List some of the functions used to view your data in R: 
# summary() 
# head()
# tail()
# unique()
# col_names()

#Question 1.3: Discuss skewness and Kurtosis
# Skewness applies to how the  graph is data is placed. If the mean = median = 
# mode then a graph would be seen as being normally distributed. It is used when
# looking at the symmetry of the graph. Kurtosis looks at the tail-end of the data.

# Question 2: 

library(tidyverse)
library(lubridate)
library(ggpubr)
library(ggplot2)
library(plotly)
library(dplyr)

Orange <- datasets::Orange # This data-set falls within Numerical data
view(Orange)

head(Orange)
tail(Orange)
colnames(Orange)
summary(Orange)


# Mean
Orange %>% 
  group_by(age) %>% 
  summarise(mean_age = mean(age))

Orange %>% 
  group_by(circumference) %>% 
  summarise(mean_circum = mean(circumference))

# Median 
Orange %>%  
  group_by(age) %>% 
  summarise(median_age = median(age))

Orange %>% 
  group_by(circumference) %>% 
  summarise(median_circum = median(circumference))

# Standard deviation
Orange %>%  
  group_by(age) %>% 
  summarise(std_age = sd(age))

Orange %>%  
  group_by(circumference) %>% 
  summarise(std_circum = sd(circumference))

# Skewness and Kurtosis
install.packages("e1071")
library(e1071)

skewness()


# Min, max, first quantile, second quantile etc

Orange %>%  
  summarise(min_circum = range(circumference)[1],
            max_cirum = range(circumference)[2]) 

Orange %>%  
  summarise(quantile(circumference))

view(Orange)

# Two plots: 

plot_1 <- ggplot(data = Orange, aes(x = tree, y = circumference)+
                   geom_histogram(colour = "green", fill = "white"))





# Question 3: 

mutate() # The mutate function creates a new variable in the data set 
select() # The select function only collects the data that you have identified
         # or selected 
group_by() # Groups existing data according to how one wants it grouped
filter() # Creates a subset of rows that you want to be filtered 
separate() # Splits up columns according to how you want them split 
