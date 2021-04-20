# Biostatistics & Intro_R recap assignment: Data manipulation, analyses & visualisation
# 20 April 2021
# Author: Amicia Canterbury 

library(tidyverse)
library(lubridate)
library(ggpubr)
library(ggplot2)
library(dslabs)
library(plotly)
options(scipen = 100)
#-------------------------SECTION 1---------------------------------------------
data(package = .packages(all.available = TRUE))
view(BOD)

# C

#------------------------SECTION 2----------------------------------------------

library(dplyr)
data("murders")
view(murders)

glimpse(murders)
head(murders)
tail(murders)
unique(murders)
summary(murders)
colnames(murders)

# This data set comprises of the amount of murders that occurred in the United
# States of America collected by the FBI. The data set also shows the population
# found in each state. In the data set one can see the state with their abbreviation,
# the geographical region, the amount of gun murders that occurred in the state 
# during the year 2010 and the estimated population size of the state in 2010.

murders_sub1 <- murders %>% 
  select(state, population)

murders_sub2 <- murders_sub1 %>% 
  filter(state!= "Florida")

no_south <- murders %>% 
  filter(region!= "South")
unique(no_south) 

# There are 34 states in this category 

South_Pop <- murders %>% 
  filter(region == "South") 

view(South_Pop)

sum(South_Pop$population) # The population found in the South = 115 674 434

West_Pop <-  murders %>% 
  filter(region == "West")

view(West_Pop)

sum(West_Pop$population) # The population found in the West = 71 945 553

North_East <- murders %>% 
  select(region, population) %>% 
  filter(region == "Northeast")

 
# Creating the two plots for murder data set 

 ggplot(data = murders, aes(x = state, y = total, fill = state)) +
  geom_bar(stat = "identity") +
  labs(x = "State", y = "Total gun murders") +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 90)) +
  ggtitle("Bar graph showing the total gun murders per state in 2010")+
   theme(panel.border = element_blank(), legend.position = "none")
 
 library(ggrepel)

 
 South <- murders %>% 
   filter(region == "South")
 
 ggplot(data = South, aes(x = population, y = total))+
   geom_point(aes(colour = state))+
   labs(x = "Population size", y = "Total gun-related murders", 
        title = "Scatter plot representing the total gun-related murders in the South Region of USA",
        colour = "State", size = 2)+
   theme_bw()

# Comparing South & West population sizes 
 
murders %>% 
  filter(region %in% c("South", "West")) %>% 
  arrange(region) %>% 
  summarise(South_Pop = sum(population [c(1:17)]),
            West_Pop = sum(population [c(18:30)]))

# The South region population is 115 674 434 and the West region population is
# 71 945 553. 

# Create a new data frame where total>20 but <100 and exclude value 120

murder_20_to_100 <- murders %>% 
  filter(total %in% (20:100)) %>% 
  arrange(total)

head(murder_20_to_100)
tail(murder_20_to_100)
glimpse(murder_20_to_100)

# Value 120 is not included in the data set as it does not fall within the category


new_new <- murders %>% 
  slice(10:20,26)

view(murders)
view(new_new)

murders_tibble <- as_tibble(murders) 
view(murders_tibble) 

murders_tibble %>% 
  group_by(region) %>% 
  arrange(region)

view(murders_tibble)

#-----------------------SECTION 3-----------------------------------------------
library(dplyr)
library(dslabs)
data(heights) 

# It is the height difference between males and females. The data is also tidy.

summary(heights)
unique(heights)
colnames(heights)
head(heights)
tail(heights)
glimpse(heights)

heights_stats <- heights %>% 
  group_by(sex) %>% 
  summarise(height_mean = mean(height),
            height_std = sd(height),
            height_median = median(height),
            height_min = min(height),
            height_max = max(height))
view(heights_stats)

glimpse(heights_stats)
unique(heights_stats)
str(heights_stats)
summary(heights_stats)

#--------------------------SECTION 4--------------------------------------------

x <- c(1, 6, 21, 19, NA, 73, NA)
y <- c(NA, NA, 3, NA, 13, 24, NA)

sum(is.na(x)) #Number of elements missing in x = 2
sum(is.na(y)) # Number of elements missing in y = 4

fahrenheit_to_celsius <- function(temp_F) {
  temp_C <- (temp_F - 32) * 5 / 9
  return(temp_C)
}

missing_values <- function(x) {
  x <- (sum(is.na(x)))
  y <- (sum(is.na(y)))
  return(x)
}

missing_values(x)
missing_values(y)

w <- c(24, 35, 41, NA, NA, NA, 81)
z <- c(2, 5, NA, NA, 25, NA, 42, NA)

missing_values(w)
missing_values(z)

#-------------------------SECTION 5---------------------------------------------

Seasonal_data <- data.frame(year = c(2015, 2016, 2017, 2018),
                 winter = c(41, 39, 47, 40),
                 spring = c(41, 46, 57, 45),
                 summer = c(75, 52, 85, 66),
                 autumn = c(57, 66, 52, 56))
view(Seasonal_data)

# Hypothesis question

# The rainfall in South Africa is higher in summer than in winter due to the 
# positioning between two oceans and it's location in the sub-tropical zone.

Seasonal_data_1 <- Seasonal_data %>% 
gather(autumn, spring, summer, winter, key = "season", value = "rainfall") %>% 
  group_by(season)

Seasonal_data_1

view(Seasonal_data_1)


ggplot(data = Seasonal_data_1, aes(x = year, y = rainfall , fill = season))+
 geom_bar(stat = "identity")+
  labs(x = "Year", y = "Rainfall(mm)", 
          title ="Bar graph representing the seasonal rainfall of South Africa" ) +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(panel.border = element_blank())+
  theme_bw()

ggplot(data = Seasonal_data_1, aes(x = year, y = rainfall))+
  geom_point(aes(colour = season))+
  labs(x = "Year", y = "Rainfall (mm)", 
       title = "Scatter plot representing the the seasonal rainfall of South Africa" ,
       colour = "Season", size = 4)+
  theme_bw()

# The bar graph shows how the summer rainfall occurs more regularly than in winter.
# In 2017, the summer rainfall was the greatest. Winter rainfall remained the least,
# due to it mostly occurring in the Western Cape. The Western Cape is known to be
# an area with a Mediterranean climate thus more winter rainfall. 

cats_data <-  tibble(cats = c("A", "B", "C"),
              position = c("1-2-3", "3-2-1", "2-3-1"),
              minutes = c(3, 3, 3),
              seconds = c(12, 44, 15))
cats_data

# Splitting the position column into three new columns

cats_data_sub1 <- cats_data %>% 
  separate(position, c("first_place", "second_place", "third_place"))

# Uniting the minutes and seconds column into total_time

cats_data_sub2 <- cats_data_sub1 %>%
  mutate(total_time = seconds*0.0166667 + minutes)

cats_data_sub2


#--------------------------SECTION 6--------------------------------------------
data("Orange")
view(Orange)

head(Orange)
tail(Orange)
summary(Orange)

# I could not gather any information in my specific data set, however I did do it
# in a previous question so here it is: 

Seasonal_data_1 <- Seasonal_data %>% 
  gather(autumn, spring, summer, winter, key = "season", value = "rainfall") %>% 
  group_by(season)

Seasonal_data_1

# Separate

# joining()


#arrange()
#select()
#group_by()
#mutate()
