#Emma Buckley
#Snakes Exercise

library(tidyverse)
snakes <- read_csv("~/Biostatistics/Second part of R/Biostats-2021/data/snakes.csv")
snakes$day = as.factor(snakes$day) #data needs to be independant and changes day to factorial data

#The first thing we do is to create some summaries of the data, this gives us 
#NA for SD.

snakes.summary <- snakes %>% 
  group_by(day, snake) %>% #you will have a group for every day
  summarise(mean_openings = mean(openings),
            sd_openings = sd(openings)) %>% 
  ungroup()
snakes.summary 

#To fix the NA problem, let us ignore the grouping by both snake and day
snakes.summary <- snakes %>% 
  group_by(day) %>% 
  summarise(mean_openings = mean(openings),
            sd_openings = sd(openings)) %>% 
  ungroup()
snakes.summary

install.packages("Rmisc")
library(Rmisc)
#create a new summary
snakes.summary2 <- summarySE(data = snakes, measurevar = "openings", groupvars = c("day"))

#Now we turn to some visual data summaries.

ggplot(data = snakes, aes(x = day, y = openings)) +
  geom_segment(data = snakes.summary2, aes(x = day, xend = day, y = openings - ci, yend = openings + ci, colour = day),#colour strip
               size = 2.0, linetype = "solid", show.legend = F) +
  geom_boxplot(aes(fill = day), alpha = 0.6, show.legend = F) + 
  geom_jitter(width = 0.05)

#ANOVA model to test these hypotheses
snakes.aov <- aov(openings ~ day + snake, data = snakes)
summary(snakes.aov)


par(mfrow = c(2, 2))
# Checking assumptions...
# make a histogram of the residuals;
# they must be normal
snakes.res <- residuals(snakes.aov)
hist(snakes.res, col = "red")

# make a plot of residuals and the fitted values;
# # they must be normal and homoscedastic
plot(fitted(snakes.aov), residuals(snakes.aov), col = "red")

snakes.tukey <- TukeyHSD(snakes.aov, which = "day", conf.level = 0.90)
plot(snakes.tukey, las = 1, col = "red")

#Own plot:This graph shows the amount of snakes released over four days

ggplot(data = snakes, aes(x = day, y = openings, fill = "red")) +
  geom_bar(stat ="identity") +
  labs(x = "Day", y = "Openings",
       title = "Amount of releases per day") +
  theme(panel.border = element_blank(),
        legend.position = "none")

#The second plot:
ggplot(snakes, aes(x = day, y = openings, fill =snake)) +
  geom_col(position ="dodge") +
  scale_fill_manual(values = c("purple", "salmon", "green", "blue", "red", "orange")) +
  labs(x = "Day", y = "Openings",
       title = "The number of snakes released per day",
       fill = "Snake") 


#add scale_fill_brewer(palette = "") instead of using  the scale fill manual
#Look up the different palettes for brewer
