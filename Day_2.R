#Emma Buckley
#Day_2
#Chapter 6 t-tests
#20 April 2021

#Loading necessary libraries and installing plotly:

install.packages("plotly")
library(tidyverse)
library(plotly)
library(ggplot2)

#Randomly creating a dataset - normal data:
#creating a data frame - two samples

set.seed(666)
r_dat <- data.frame(dat = c(rnorm(n = 1000, mean = 10, sd = 3),
                            rnorm(n = 1000, mean = 8, sd = 2)),
                    sample = c(rep("A", 1000), rep("B", 1000)))

#Create histogram this gives an idea of what the data is about:

h <- ggplot(data = r_dat, aes(x = dat, fill = sample)) +
  geom_histogram(position = "dodge", binwidth = 1, alpha = 0.8) +
  geom_density(aes(y = 1*..count.., fill = sample), colour = NA, alpha = 0.4) +
  labs(x = "Value", y = "Count" ,
       title = "A histogram showing the different samples")
h

#To test the normality for all the samples:
#Shapiro-Wilk normality test

shapiro.test(r_dat$dat)

#Test normality for each sample separately:

r_dat %>% 
  group_by(sample) %>% 
  summarise(norm_dat = as.numeric(shapiro.test(dat)[2]))

#Sample A and B are normally distributed

#Homoscedasticity test:

r_dat %>% 
  group_by(sample) %>% 
  summarise(sample_var = var(dat))

#Therefore one sample is not four times greater than the other
#They are homoscedastic

#This creates a function to do the normality test and the homoscedasticity test
#This does the normality test and the Shapiro test at the same time:

two_assum <- function(x) {
  x_var <- var(x)
  x_norm <- as.numeric(shapiro.test(x)[2])
  result <- c(x_var, x_norm)
  return(result)
}

#Use the function to show the normality and homoscedasticity for each sample:

r_dat %>% 
  group_by(sample) %>% 
  summarise(sample_var = two_assum(dat)[1],
            sample_norm = two_assum(dat)[2])

#One-sample t-test this compares a sample to the mean
#create a single sample of random normal data:

set.seed(666)
r_one <- data.frame(dat = rnorm(n = 20, mean = 20, sd = 5),
                    sample = "A")

#Check normality:

shapiro.test(r_one$dat)

#one sample t-test
#compare random data against a population mean of 20:

t.test(r_one$dat, mu = 20)

#compare random data against a population mean of 30:

t.test(r_one$dat, mu = 30)

#Creating a box and whisker diagram to visualise the one sample t-test:

ggplot(data = r_one, aes(y = dat, x = sample)) +
  geom_boxplot(fill = "lightblue") +
  # population  mean (mu) = 20
  geom_hline(yintercept = 20, colour = "darkblue", 
             size = 1, linetype = "dashed") +
  # population  mean (mu) = 30
  geom_hline(yintercept = 30, colour = "red", 
             size = 1, linetype = "dashed") +
  labs(y = "Value", x = NULL, 
       title = "A box and whisker visualising the one sample t-test") +
  coord_flip() #axis is flipped

#Two sampled t-tests:

#Creates a random dataset - normal data:
set.seed(666)
r_two <- data.frame(dat = c(rnorm(n = 20, mean = 4, sd = 1),
                            rnorm(n = 20, mean = 5, sd = 1)),
                    sample = c(rep("A", 20), rep("B", 20)))

#perform t-test
#note how we set the `var.equal` argument to TRUE because we know 
#our data has the same SD (they are simulated as such!)
#To compare the differences between the sample:

t.test(dat ~ sample, data = r_two, var.equal = TRUE)

#t-test workflow:
#Load the necessary data:
#Removing species, site and ID from the dataset

ecklonia <- read_csv("~/Biostatistics/Second part of R/Biostats-2021/data/ecklonia.csv") %>% 
  gather(key = "variable", value = "value", -species, -site, -ID)

#Creating a ggplot of the data to help formulate a hypothesis:

ggplot(data = ecklonia, aes(x = variable, y = value, fill = site)) +
  geom_boxplot() +
  coord_flip() +
  labs(title = "Boxplots showing differences in morphometric 
properties of the kelp Ecklonia maxima at two 
sites in False Bay")


#Filtering the data:

ecklonia_sub <- ecklonia %>% 
  filter(variable == "stipe_mass")

#Create a new figure:

ggplot(data = ecklonia_sub, aes(x = variable, y = value, fill = site)) +
  geom_boxplot() +
  coord_flip() + #flips axes
  labs(y = "stipe mass (kg)", x = "", 
       title = "Boxplots showing the difference in stipe mass (kg) of the kelp
Ecklonia maxima at two sites in False Bay.") +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

#Checking the assumptions:
#Doing the normality test and the Shapiro test

ecklonia_sub %>% 
  group_by(site) %>% #there are two sites and we want to compare 2 diff.sites
  summarise(stipe_mass_var = two_assum(value)[1],
            stipe_mass_norm = two_assum(value)[2])

#It is normally distributed and homoscedastic

#running the t-test
#traditional output

t.test(value ~ site, data = ecklonia_sub, var.equal = TRUE, alternative = "greater")

#Reject null hypothesis which means that the the stipe mass (kg) of 
#the kelp Ecklonia maxima was found to be significantly greater at 
#Batsata Rock than at Boulders Beach (p = 0.03, t = 1.87, df = 24).

# Part 2 ------------------------------------------------------------------
#Emma Buckley
#Day_2
#Chapter 6 ANOVA test
#20 April 2021

#Single factor ANOVA test:

#Installing necessary packages and loading them:

install.packages("ggpubr")
library(ggpubr)

#First grab the data:

chicks <- as_tibble(ChickWeight)

#Subset out only the sample sets to be compared:

chicks_sub <- chicks %>% 
  filter(Diet %in% c(1, 2), Time == 21) #filtering diets 1,2 and time 21

#Uses compare means to do a t-test:

compare_means(weight ~ Diet, data = chicks_sub, method = "t.test")


#Doing an Anova test:

chicks.aov1 <- aov(weight ~ Diet, data = filter(chicks, Time == 21))
summary(chicks.aov1)

#Run a Tukey HSD test:

TukeyHSD(chicks.aov1)

#This shows us the pairwise comparisons of all of the groups we are comparing.

#Multiple factors:

#Instead of comparing each time separately:

summary(aov(weight ~ Diet, data = filter(chicks, Time %in% c(0))))
summary(aov(weight ~ Diet, data = filter(chicks, Time %in% c(2))))
summary(aov(weight ~ Diet, data = filter(chicks, Time %in% c(10))))
summary(aov(weight ~ Diet, data = filter(chicks, Time %in% c(21))))

#Compare each time in one line:

chicks.aov2 <- aov(weight ~ as.factor(Time), data = filter(chicks, Time %in% c(0, 2, 10, 21)))
summary(chicks.aov2)

#OR save time and do it another way:

summary(aov(weight ~ Diet + as.factor(Time), data = filter(chicks, Time %in% c(0, 21))))

#Run a Tukey HSD test:

TukeyHSD(aov(weight ~ Diet * as.factor(Time), data = filter(chicks, Time %in% c(20, 21))))



