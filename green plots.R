---
# Title: "green plots.R"
# Author: "hannah ho"
# Date: "8/18/2019"
---
library(tidyverse)
library(knitr)
library(reshape2)

green = read.csv('C:/Users/Hannah/Desktop/UT Austin/Analytics - MSBA/summer 19/predictive modeling/STA380-1-master/STA380-1-master/data/greenbuildings.csv')
#detach(green)
attach(green)
green = na.omit(green)

###
# Plot rent of green Status by class
###

# Merge class_a and class_b into one column
green_class = green[,c(5,10:11,14)]
green_class$class_b[green_class$class_b == 1] <- 2
merge_class = max.col(green_class[,c(2:3)], ties.method ="first")
green_class = cbind(green_class, merge_class)
table(merge_class) #note that there are no class c buildings

# Convert numberical indicators to string indicators for better readability
green_class$merge_class[green_class$merge_class == 1] <- 'a'
green_class$merge_class[green_class$merge_class == 2] <- 'b'
green_class$green_rating[green_class$green_rating == 1] <- 'Y'
green_class$green_rating[green_class$green_rating == 0] <- 'N'

green_class$merge_class_fac = as.factor(green_class$merge_class)
green_class$green_rating_fac = as.factor(green_class$green_rating)

ggplot(data = green_class) + 
  geom_bar(mapping = aes(x=merge_class_fac, y=Rent, fill=green_rating_fac),
           position="dodge", stat='identity') + 
  labs(title="Rent of Green Status Buildings by Class", 
       y="Rent per SqFt ($)",
       x = "Class",
       fill="Green Status")
###
# Plot number of buildings per Green Status per Class
###

# grouping by two variables and spreading one across the columns
TitanicSurvival %>%
  group_by(passengerClass, survived) %>%
  summarize(mean_age = mean(age, na.rm=TRUE)) %>%
  spread(survived, mean_age)
         
d1 = green_class %>%
  group_by(green_class$merge_class, green_class$green_rating) %>%
  summarize(buildings = count(green_class, 'merge_class_fac'))
d1
