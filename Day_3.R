#Emma Buckley
#21 April 2021
#Linear Regressions
#Day 3

#Loading tidyverse

library(tidyverse)

#Getting data faithful

data("faithful")
load(faithful)

#Looking at the first six rows of faithful dataset:

head(faithful)

#Creating a sub dataframe called eruption.lm.
#fit the model in R. When we perform a linear regression in R, it will 
#output the model and the coefficients

eruption.lm <- lm(eruptions ~ waiting, data = faithful)

#summarising the dataframe

summary(eruption.lm)

#Using a value for the slope of the data: 

slope <- round(eruption.lm$coef[2], 3) #rounding off to three decimal places

#p.val <- round(coefficients(summary(eruption.lm))[2, 4], 3) # it approx. 0
#Using the p value for the data:

p.val = 0.001

#Using the value for the co-efficient of determination value:

r2 <- round(summary(eruption.lm)$r.squared, 3)

#Tells us the value for r^2:

summary(eruption.lm)$r.squared

#Creating a ggplot:

ggplot(data = faithful, aes(x = waiting, y = eruptions)) + #creating a plot with points
  geom_point(colour = "blue") +#point plot
  annotate("text", x = 45, y = 5, label = paste0("slope == ", slope, "~(min/min)"), parse = TRUE, hjust = 0) +
  annotate("text", x = 45, y = 4.75, label = paste0("italic(p) < ", p.val), parse = TRUE, hjust = 0) +
  annotate("text", x = 45, y = 4.5, label = paste0("italic(r)^2 == ", r2), parse = TRUE, hjust = 0) +#adding text to the graph
  stat_smooth(method = "lm", colour = "red") +
  labs(title = "Old Faithful eruption data", #adding labels
       subtitle = "Linear regression",
       x = "Waiting time (minutes)",
       y = "Eruption duration (minutes)")


#Another example:
#Creating random normal data:

n <- 100
set.seed(666)
rand.df <- data.frame(x = seq(1:n),
                      y = rnorm(n = n, mean = 20, sd = 3))

#Creating a graph showing the data:

ggplot(data = rand.df, aes(x = x, y = y)) +
  geom_point(colour = "black") +
  stat_smooth(method = "lm", colour = "red", size = 0.75, fill = "turquoise", alpha = 0.3) +
  labs(title = "Random normal data",
       subtitle = "Linear regression",
       x = "X (independent variable)",
       y = "Y (dependent variable)")



# Part 2 ------------------------------------------------------------------
#Emma Buckley
#21 April 2021
#Correlations
#Day 3

#Installing the necessary packages:

install.packages("ggpubr")
install.packages("corrplot")

# Load libraries #activate the packages

library(tidyverse)
library(ggpubr)
library(corrplot)

# Loading data:

ecklonia <- read_csv("~/Biostatistics/Second part of R/Biostats-2021/data/ecklonia.csv")

#Removing categorical variables(site, ID and species)
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

#If value is close to 1, strong correlation
#0.6 is closer to 1 so there is a strong correlation between 
#stipe length and frond length

#Now we want to compare many variables:
#Pearson correlation:

ecklonia_pearson <- cor(ecklonia_sub)
ecklonia_pearson

#Spearman rank correlation:

# Create ordinal data:

ecklonia$length <- as.numeric(cut((ecklonia$stipe_length+ecklonia$frond_length), breaks = 3))

# Run test on any variable:

cor.test(ecklonia$length, ecklonia$digits)

#Kendall rank correlation:

ecklonia_norm <- ecklonia_sub %>% 
  gather(key = "variable") %>% 
  group_by(variable) %>% 
  summarise(variable_norm = as.numeric(shapiro.test(value)[2]))
ecklonia_norm

cor.test(ecklonia$primary_blade_length, ecklonia$primary_blade_width, method = "kendall")


#One panel visual:

#Calculate Pearson r beforehand for plotting
#creating label of the r value
r_print <- paste0("r = ", 
                  round(cor(x = ecklonia$stipe_length, ecklonia$frond_length),2))

# Then create a single panel showing one correlation:

ggplot(data = ecklonia, aes(x = stipe_length, y = frond_length)) +
  geom_smooth(method = "lm", colour = "blue", se = F) +
  geom_point(colour = "black") +
  geom_label(x = 300, y = 240, label = r_print) + #this add r value to the graph
  labs(x = "Stipe length (cm)", y = "Frond length (cm)",
       title = "Scatterplot showing relationship between Ecklonia maxima 
stipe length (cm) and frond length (cm)") +
  theme_pubclean()

#creates a correlation plot:
#Multiple panel visual:

corrplot(ecklonia_pearson, method = "circle")
        
#If the colour is dark = strong correlation
#if the colour is lighter = weak correlation
