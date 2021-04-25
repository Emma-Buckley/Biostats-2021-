#Emma Buckley
#21 April 2021
#Correlations - Example
#Day 3

#method 1: Create a heatmap using 
ecklonia_sub <- ecklonia %>% 
  select(-species, - site, - ID)

heatmap(ecklonia_sub, Rowv = NA, Colv = NA)

ecklonia_pearson <- cor(ecklonia_sub)
ecklonia_pearson

heatmap(ecklonia_pearson, Rowv = NA, Colv = NA)

#method 2: Using ggplot 2

install.packages("reshape")
library(reshape)
data_melt <- melt(ecklonia_pearson)

ggplot(data_melt, aes(X1, X2)) + 
  geom_tile(aes(fill = value)) +
  scale_fill_gradient(low = "lightblue", high = "royalblue") +
  labs(x = "Variables", y = "Variables",
       title = "A heatmap of the ecklonia data") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.border = element_blank(),
        legend.position = "none")


#Dlply

install.packages("plyr")
library(plyr)

dlply(ecklonia, .(site, ID))
View(dlply(ecklonia, .(species, ID)))
