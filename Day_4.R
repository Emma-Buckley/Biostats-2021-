#Emma Buckley
#Day 4
#22 April 2021
#Confidence intervals

#installing packages and loading libraries:
install.packages("rcompanion")
library(rcompanion)
library(ggplot2)
library(tidyverse)

#Inputting data:

Input <- ("
Student  Sex     Teacher  Steps  Rating
a        female  Jacob    8000   7
b        female  Jacob    9000  10
c        female  Jacob   10000   9
d        female  Jacob    7000   5
e        female  Jacob    6000   4
f        female  Jacob    8000   8
g        male    Jacob    7000   6
h        male    Jacob    5000   5
i        male    Jacob    9000  10
j        male    Jacob    7000   8
k        female  Sadam    8000   7
l        female  Sadam    9000   8
m        female  Sadam    9000   8
n        female  Sadam    8000   9
o        male    Sadam    6000   5
p        male    Sadam    8000   9
q        male    Sadam    7000   6
r        female  Donald   10000  10
s        female  Donald    9000  10
t        female  Donald    8000   8
u        female  Donald    8000   7
v        female  Donald    6000   7
w        male    Donald    6000   8
x        male    Donald    8000  10
y        male    Donald    7000   7
z        male    Donald    7000   7
")

#Creating dataframe:

data <- read.table(textConnection(Input),header = TRUE)

#Getting a summary of the data:

summary(data)

#Getting structure of the data:

str(data)

# ungrouped data is indicated with a 1 on the right side of the formula, 
#or the group = NULL argument.
#Calculating mean, confidence level for all data:

groupwiseMean(Steps ~ 1,data = data, conf = 0.95, digits = 3)


# one-way data:
##Calculating mean, confidence level for male and female:

groupwiseMean(Steps ~ Sex, data = data, conf = 0.95,digits = 3)

#Plotting a graph:

#one way data: Just looking at sex 
#Seeing if sex has effect on the number of steps:

out <- groupwiseMean(Steps ~ Sex, data = data, conf = 0.95,digits = 3)

ggplot(data = out) +
  geom_col(aes(x = Sex, y = Mean, fill = Sex), col = "black") +
  geom_errorbar(aes(ymin = Trad.lower,
                    ymax = Trad.upper,
                    x = Sex),
                col = "black",
                width = 0.2) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(y = "Steps",
       title = "The number of steps for males and female students")

#two way data: Looking at sex and teacher

groupwiseMean(Steps ~ Teacher + Sex, data = data, conf = 0.95,digits = 3)

out_2 <-groupwiseMean(Steps ~ Teacher + Sex, data = data, conf = 0.95,digits = 3)

ggplot(data = out_2) +
  geom_col(aes(x = Sex, y = Mean, fill = Sex), col = "black") +
  geom_errorbar(aes(ymin = Trad.lower,
                    ymax = Trad.upper,
                    x = Sex),
                col = "black",
                width = 0.2) +
  facet_wrap(~Teacher, ncol = 3) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(y = "Steps",
       title = "Number of steps of students with different teacher")
#Neither gender or teacher has effect on steps


#Use bootstrapping:

groupwiseMean(Steps ~ Sex,
              data = data,
              conf = 0.95,
              digits = 3,
              R = 10000,
              boot = TRUE,
              traditional = FALSE,
              normal = FALSE,
              basic = FALSE,
              percentile = FALSE,
              bca = TRUE)
#ANOVA test

anova <- aov(Steps~Sex*Teacher, data = data)
summary(anova)

#Tukey to look pairwise comparisons

anova_Tukey <- TukeyHSD(anova)
plot(anova_Tukey) #plotting the anova test results
