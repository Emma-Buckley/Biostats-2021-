#Emma Buckley
#21 April 2021
#Correlations
#Day 3

install.packages("ggpubr")
install.packages("corrplot")

# Load libraries #activate the packages

library(tidyverse)
library(ggpubr)
library(corrplot)

# Load data

ecklonia <- read_csv("~/Biostatistics/Second part of R/Biostats-2021/data/ecklonia.csv")

#Removing categorical variables 
#create a subsetted version of our data by removing all of the categorical variables

ecklonia_sub <- ecklonia %>% 
  select(-species, - site, - ID)

# Perform correlation analysis on two specific variables
# Note that we do not need the final two arguments in this function to be stated
# as they are the default settings.
# They are only shown here to illustrate that they exist.
#comparing two variables

cor.test(x = ecklonia$stipe_length, ecklonia$frond_length, #specifying a column
         use = "everything", method = "pearson")

#Now we want to compare many variables
ecklonia_pearson <- cor(ecklonia_sub)
ecklonia_pearson

#Kendall rank correlation
ecklonia_norm <- ecklonia_sub %>% 
  gather(key = "variable") %>% 
  group_by(variable) %>% 
  summarise(variable_norm = as.numeric(shapiro.test(value)[2]))
ecklonia_norm

cor.test(ecklonia$primary_blade_length, ecklonia$primary_blade_width, method = "kendall")


#One panel visual

# Calculate Pearson r beforehand for plotting
#creating label of the r value
r_print <- paste0("r = ", 
                  round(cor(x = ecklonia$stipe_length, ecklonia$frond_length),2))

# Then create a single panel showing one correlation
ggplot(data = ecklonia, aes(x = stipe_length, y = frond_length)) +
  geom_smooth(method = "lm", colour = "grey90", se = F) +
  geom_point(colour = "mediumorchid4") +
  geom_label(x = 300, y = 240, label = r_print) +
  labs(x = "Stipe length (cm)", y = "Frond length (cm)") +
  theme_pubclean()

#creates a correlation plot

corrplot(ecklonia_pearson, method = "circle")

#If the colour is dark = strong correlation
#if the colour is lighter = weak correlation
