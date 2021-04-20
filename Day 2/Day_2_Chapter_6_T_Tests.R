# Day 2
# 20 April 2021
# Chapter 6
# Biostatistics 


library(tidyverse)
install.packages("plotly")

library(plotly) # Uses this packages for the T-tests used in biology

# Random normal data
set.seed(666)
r_dat <- data.frame(dat = c(rnorm(n = 1000, mean = 10, sd = 3),
                            rnorm(n = 1000, mean = 8, sd = 2)),
                    sample = c(rep("A", 1000), rep("B", 1000)))

# Create histogram
h <- ggplot(data = r_dat, aes(x = dat,fill = sample)) +
  scale_color_manual(values = c("Green", "Blue")) +
  geom_histogram(position = "dodge", binwidth = 1, alpha = 0.8) +
  geom_density(aes(y = 1*..count.., fill = sample), colour = NA, alpha = 0.4) +
  labs(x = "Value" , y = "Count", 
       title = "Histrogram representing Sample A and B")+
  theme_bw()
  
  
h 

shapiro.test(r_dat$dat) #The shapiro.test is to test the p-value 

#Have to check normality of the each dataset (aka A and B)

# we use the square bracket notation to select only the p-value;
# had we used `[1]` we'd have gotten W
r_dat %>% 
  group_by(sample) %>% 
  summarise(norm_dat = as.numeric(shapiro.test(dat)[2]))

#Homoscedasticity - checks if your variants are homogenous 
#In practical terms this means that the variance of the samples we are comparing
#should not be more than two to four times greater than one another. 
#In R, we use the function var() to check the variance in a sample:

r_dat %>% 
  group_by(sample) %>% 
  summarise(sample_var = var(dat))

# Check the assumptions in the beginning 

two_assum <- function(x) {
  x_var <- var(x)
  x_norm <- as.numeric(shapiro.test(x)[2])
  result <- c(x_var, x_norm)
  return(result)
} 

r_dat %>% 
  group_by(sample) %>% 
  summarise(sample_var = two_assum(dat)[1],
            sample_norm = two_assum(dat)[2])

# Excercise 
ecklonia <- read_csv("C:/Users/Samantha/Desktop/Honours/Semester 1 - Core Modules/BDC 744 - Biostatistics/Biostatistics - PART 2/Biostats_2021/data/ecklonia.csv") %>% 
gather(key = "variable", value = "value", -species, -site, -ID)

ggplot(data = ecklonia, aes(x = variable, y = value, fill = site)) +
  geom_boxplot() +
  coord_flip()
# filter the data
ecklonia_sub <- ecklonia %>% 
  filter(variable == "stipe_mass")

# then create a new figure
ggplot(data = ecklonia_sub, aes(x = variable, y = value, fill = site)) +
  geom_boxplot() +
  coord_flip() +
  labs(y = "stipe mass (kg)", x = "") +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

ecklonia_sub %>% 
  group_by(site) %>% 
  summarise(stipe_mass_var = two_assum(value)[1],
            stipe_mass_norm = two_assum(value)[2])
