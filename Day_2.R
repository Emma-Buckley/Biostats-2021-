#Emma Buckley
#Day_2
#Chapter 6 t-tests
#20 April 2021

library(tidyverse)
install.packages("plotly")
library(plotly)

# Randomly creating a dataset- normal data
set.seed(666)
#creating a data frame- two samples
r_dat <- data.frame(dat = c(rnorm(n = 1000, mean = 10, sd = 3),
                            rnorm(n = 1000, mean = 8, sd = 2)),
                    sample = c(rep("A", 1000), rep("B", 1000)))

# Create histogram-gives idea of what the data is about
h <- ggplot(data = r_dat, aes(x = dat, fill = sample)) +
  geom_histogram(position = "dodge", binwidth = 1, alpha = 0.8) +
  geom_density(aes(y = 1*..count.., fill = sample), colour = NA, alpha = 0.4) +
  labs(x = "Value", y = "Count" ,
       title = "A histogram showing the different samples")
h

#To test the normality for all the samples
#Shapiro-Wilk normality test
shapiro.test(r_dat$dat)

#To group samples A and B seperately
r_dat %>% 
  group_by(sample) %>% 
  summarise(norm_dat = as.numeric(shapiro.test(dat)[2]))

#homoscedasity
r_dat %>% 
  group_by(sample) %>% 
  summarise(sample_var = var(dat))

#This does the normality test and the Shapiro test at the same time
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

#One-sample test-compare a sample to the mean
# create a single sample of random normal data
set.seed(666)
r_one <- data.frame(dat = rnorm(n = 20, mean = 20, sd = 5),
                    sample = "A")

# check normality
shapiro.test(r_one$dat)

#one sample t-test
#compare random data against a population mean of 20
t.test(r_one$dat, mu = 20)


#Two sampled tests

# creates a random dataset normal data
set.seed(666)
r_two <- data.frame(dat = c(rnorm(n = 20, mean = 4, sd = 1),
                            rnorm(n = 20, mean = 5, sd = 1)),
                    sample = c(rep("A", 20), rep("B", 20)))

#perform t-test
#note how we set the `var.equal` argument to TRUE because we know 
#our data has the same SD (they are simulated as such!)
#To compare the differences between the sample
t.test(dat ~ sample, data = r_two, var.equal = TRUE)

#t-test workflow
ecklonia <- read_csv("~/Biostatistics/Second part of R/Biostats-2021/data/ecklonia.csv") %>% 
  gather(key = "variable", value = "value", -species, -site, -ID)

ggplot(data = ecklonia, aes(x = variable, y = value, fill = site)) +
  geom_boxplot() +
  coord_flip()

#Second exercise
# filter the data
ecklonia_sub <- ecklonia %>% 
  filter(variable == "stipe_mass")

# then create a new figure
ggplot(data = ecklonia_sub, aes(x = variable, y = value, fill = site)) +
  geom_boxplot() +
  coord_flip() + #flips axes
  labs(y = "stipe mass (kg)", x = "") +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

#doing the normality test and the Shapiro test
ecklonia_sub %>% 
  group_by(site) %>% #there are two sites and we want to compare 2 diff.sites
  summarise(stipe_mass_var = two_assum(value)[1],
            stipe_mass_norm = two_assum(value)[2])

#running the t-test
# traditional output
t.test(value ~ site, data = ecklonia_sub, var.equal = TRUE, alternative = "greater")

#Part 2
#Emma Buckley
#Day_2
#Chapter 6 ANOVA test
#20 April 2021

install.packages("ggpubr")
library(ggpubr)
# First grab the data
chicks <- as_tibble(ChickWeight)

# Then subset out only the sample sets to be compared
chicks_sub <- chicks %>% 
  filter(Diet %in% c(1, 2), Time == 21) #filtering diets 1,2 and time 21

compare_means(weight ~ Diet, data = chicks_sub, method = "t.test")

chicks.aov1 <- aov(weight ~ Diet, data = filter(chicks, Time == 21))
summary(chicks.aov1)

TukeyHSD(chicks.aov1)

summary(aov(weight ~ Diet, data = filter(chicks, Time %in% c(0))))
summary(aov(weight ~ Diet, data = filter(chicks, Time %in% c(2))))

chicks.aov2 <- aov(weight ~ as.factor(Time), data = filter(chicks, Time %in% c(0, 2, 10, 21)))
summary(chicks.aov2)

summary(aov(weight ~ Diet + as.factor(Time), data = filter(chicks, Time %in% c(0, 21))))

TukeyHSD(aov(weight ~ Diet * as.factor(Time), data = filter(chicks, Time %in% c(20, 21))))


