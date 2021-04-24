#Biostats
#Emma Buckley
#Day 1
#Viewing data and Basic Stats
#19 April 2021

#Part 1:

library(tidyverse)
?BOD #to see info about the data
BOD #view the data
str(BOD) #to see the structure of the data and the types of data
summary(BOD) #gives you a summary of the data


str(InsectSprays)#to see the structure of the data and the types of data
View(InsectSprays)#view the dataset
unique(InsectSprays$spray)# to see the different types of spray

?Loblolly #to see info about the data
str(Loblolly)#to see the structure of the data and the types of data

?HairEyeColor #to see information about the dataset
HairEyeColor #view the data
str(HairEyeColor) #to see the structure of the data and the types of data

Seatbelts #view the data in the console
str(Seatbelts) #to see the structure of the data and the types of data
View(Seatbelts) #view the data in a separate tab
?Seatbelts  #to see information about the dataset

cars #view the data in the console
str(cars) #to see the structure of the data and the types of data
View(cars) #view the data in a separate tab

?esoph #to see information about the dataset
str(esoph) #to see the structure of the data and the types of data
View(esoph) #view the data in a separate tab

?JohnsonJohnson #to see information about the dataset
str(JohnsonJohnson) #to see the structure of the data and the types of data
View(JohnsonJohnson) #view the data in a separate tab

?volcano #to see information about the dataset
str(volcano) #to see the structure of the data and the types of data
View(volcano) #view the data in a separate tab


#Load built-in data
pines <- Loblolly
str(pines) #structure of the data
class(pines$height)#what type of class a particular column is

#Part 2:

#calculate sample size

library(tidyverse)
chicks <- as_tibble(ChickWeight)
chicks #Viewing the chickweight data
nrow(chicks) #calculate the number of rows
?ChickWeight #Tells us what the data is about
unique(chicks$Chick) #tells you the sample size of the chickweight data

#note the distinction between 'nrows' and the 'true' sample size
nrow(chicks)
unique(chicks$Chick)#counts no. of unique levels

#calculate the mean weight of all chickens at day 20

view(chicks)
chicks %>% 
  filter(Time == 20) %>% 
  summarise(chicks = mean(weight))

#calculate the mean of the chicks weight for each diets:

view(chicks)
chicks %>% 
  filter(Time == 20) %>% 
  group_by(Diet) %>% 
  summarise(chicks = mean(weight))

#calculate the standard deviation  of the chicks weight for each diets:

view(chicks)
chicks %>% 
  filter(Time == 20) %>% 
  group_by(Diet) %>% 
  summarise(chicks = mean(weight),
            chicks_sd = sd(weight))

#mean, median, std per diet

chicks %>% 
  filter(Time == 20) %>% 
  group_by(Diet) %>% 
  summarise(chicks_mean = mean(weight),
            chicks_sd = sd(weight),
            chicks_median = median(weight))

#Kurtosis
install.packages("e1071")
library(e1071)

#Kutosis for each diet at time 20
chicks %>% 
  group_by(Diet) %>%
  filter(Time == 20) %>% 
  summarise(kurtosis = kurtosis(weight))

#The mean of the weight of the chicks
chicks %>% 
  summarise(mean_wt = mean(weight))            

#The quartiles of the chick for each diet at time 20
chicks %>% 
  group_by(Diet) %>%
  filter(Time == 20) %>% 
  summarise(min_wt = min(weight),
            qrt1_wt = quantile(weight, p = 0.25),
            med_wt = median(weight),
            qrt3_wt = quantile(weight, p = 0.75),
            max_wt = max(weight))

#range of the chicks
chicks %>% 
  summarise(lower_wt = range(weight)[1],
            upper_wt = range(weight)[2])

#Option 2 for finding the range:

chicks %>% 
  group_by(Diet) %>%
  filter(Time == 20) %>% 
  summarise(lower_wt = range(weight)[1],
            upper_wt = range(weight)[2])

#Missing values:

dat1 <- c(NA, 12, 76, 34, 23)
mean(dat1) #give a NA value
mean(dat1, na.rm = TRUE)#to remove the missing value

#Plots:
library(ggpubr) # needed for arranging multi-panel plots

grp_stat <- chicks %>%
  filter(Time == 21) %>% 
  group_by(Diet, Time) %>% 
  summarise(mean_wt = round(mean(weight, na.rm = TRUE), 2),
            med_wt = median(weight, na.rm = TRUE),
            sd_wt = round(sd(weight, na.rm = TRUE), 2),
            sum_wt = sum(weight),
            min_wt = min(weight),
            qrt1_wt = quantile(weight, p = 0.25),
            med_wt = median(weight),
            qrt3_wt = median(weight, p = 0.75),
            max_wt = max(weight),
            n_wt = n())
grp_stat

plt1 <- chicks %>%
  filter(Time == 21) %>% 
  ggplot(aes(x = Diet, y = weight)) +
  geom_point(data = grp_stat, aes(x = Diet, y = mean_wt), 
             col = "black", fill = "red", shape = 23, size = 3) +
  geom_jitter(width = 0.05) + # geom_point() if jitter not required
  labs(y = "Chicken mass (g)" ,
       title = "A scatterplot showing the mean weights of the chicks 
                              and raw chick mass values") +  
  theme_pubr()

plt1

plt2 <- ggplot(data = grp_stat, aes(x = Diet, y = mean_wt)) +
  geom_bar(position = position_dodge(), stat = "identity", 
           col = NA, fill = "salmon") +
  geom_errorbar(aes(ymin = mean_wt - sd_wt, ymax = mean_wt + sd_wt),
                width = .2) +
  labs(y = "Chicken mass (g)",
       title = "A bar graph showing the chicken mass values and showing
                                whiskers of one SD") + 
  theme_pubr()
plt2
# position_dodge() places bars side-by-side
# stat = "identity" prevents the default count from being plotted

# a description of the components of a boxplot is provided in the help file
# geom_boxplot()
plt3 <- chicks %>%
  filter(Time == 21) %>% 
  ggplot(aes(x = Diet, y = weight)) +
  geom_boxplot(fill = "salmon") +
  geom_jitter(width = 0.05, fill = "white", col = "blue", shape = 21) +
  labs(y = "Chicken mass (g)", 
       title = "A box and whisker showing chicken mass data for the four diets") + 
  theme_pubr()
plt3

plt4 <- chicks %>%
  filter(Time %in% c(10, 21)) %>% 
  ggplot(aes(x = Diet, y = weight, fill = as.factor(Time))) +
  geom_boxplot() +
  geom_jitter(shape = 21, width = 0.1) +
  labs(y = "Chicken mass (g)", fill = "Time", 
       title = "A box and whisker showing showing chicken mass data at times
                                      10 and 21") +
  theme_pubr()
plt4

ggarrange(plt1, plt2, plt3, plt4, ncol = 2, nrow = 2, labels = "AUTO")
