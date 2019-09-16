---
# Title: "green plots.R"
# Date: "8/18/2019"
---
rm(list=ls())
library(tidyverse)
library(knitr)
library(reshape2)
library(plyr)
library(readxl)
library(glmnet)
library(coefplot)
library(formattable)

green = read.csv('C:/Users/Hannah/Desktop/UT Austin/Analytics - MSBA/summer 19/predictive modeling/part 2 exercises/green data/greenbuildings.csv')
#detach(green)
attach(green)
green = na.omit(green)

###
# Plot MEAN RENT of green Status by class
###

# Groupby preprocessing was performed in python and the resulting dataframe was imported into R
green_groups = read_excel('C:/Users/Hannah/Desktop/UT Austin/Analytics - MSBA/summer 19/predictive modeling/part 2 exercises/green data/class_green_rent_groups.xlsx')
green_groups = green_groups[,-1]
green_groups$Rent = round(green_groups$Rent,digits = 2)
green_groups$green_rating[green_groups$green_rating == 1] <- 'Y'
green_groups$green_rating[green_groups$green_rating == 0] <- 'N'

ggplot(data = green_groups) +
  geom_bar(mapping = aes(x=green_groups$class, y=green_groups$Rent, fill=as.factor(green_groups$green_rating)),
           position="dodge", stat='identity') + 
  labs(title="Mean Rent of Green Status Buildings by Class", 
       y="Mean Rent per SqFt ($)",
       x = "Class",
       fill="Green Status")

###
# Plot NUMBER of buildings per green status by class to visualize sample sizes per category
###

# Groupby preprocessing was performed in python and the resulting dataframe was imported into R
green_num = read_csv('C:/Users/Hannah/Desktop/UT Austin/Analytics - MSBA/summer 19/predictive modeling/part 2 exercises/green data/class_green_rent_counts.csv')
green_num$green_rating[green_num$green_rating == 1] <- 'Y'
green_num$green_rating[green_num$green_rating == 0] <- 'N'

ggplot(data = green_num) +
  geom_bar(mapping = aes(x=green_num$class, y=green_num$CS_PropertyID, fill=as.factor(green_num$green_rating)),
           position="dodge", stat='identity') + 
  labs(title="Number of Green Status Buildings by Class", 
       y="Number of Buildings",
       x = "Class",
       fill="Green Status")

# Class 'A' green status buildings charge lower mean rents per square foot, 
# implying that green status is not correlated with premium rates when
# compared with buildings of a similar class.
# It might look like class c green buldings can charge a higher rent.
# However, there are so few class b and c green buildings in the dataset that
# the premium is unlikely to carry statistical significance.

# Given that there is no premium for green status after holding class constant
# and most green buildings are class 'A' buildings,  we will investigate
# whether there is a premium charged for class 'A' buildings.

###
# Plot number of class 'A' buildings per binned cluster rent
###
green_cluster = read_csv('C:/Users/Hannah/Desktop/UT Austin/Analytics - MSBA/summer 19/predictive modeling/part 2 exercises/green data/classA_clusterRent.csv')

ggplot(data = green_cluster) +
  geom_bar(mapping = aes(x=green_cluster$cluster_bin, y=green_cluster$class),
           position="dodge", stat='identity', fill = 'pink') + 
  labs(title="Number of Class A Buildings per Cluster Rent",
       y="Number of Class A Buildings",
       x = "Cluster Rent")

# Most class 'A' buildings are not located in particularly expensive cluster rents.
# This means that the apparent premium on green buildings are not
# coming from being located in nicer areas as signalled by having higher rents
# overall.

###
# Plot Rent of Green Status Buildings by no. of cooling days
###

pretty_green = read.csv('C:/Users/Hannah/Desktop/UT Austin/Analytics - MSBA/summer 19/predictive modeling/part 2 exercises/green data/rentColdDays.csv')
pretty_green$green_rating_fac <- as.factor(pretty_green$green_rating)

ggplot(data = pretty_green, aes(x = pretty_green$cd_total_07_quantiles, y = pretty_green$Rent_cooling, fill = pretty_green$green_rating_fac))+
  geom_bar(stat = 'identity', position = position_dodge())+ 
  labs(title="Rent of Green Status Buildings by no. of cooling days", 
       y="Rent per SqFt ($)",
       x = "No. of cooling days",
       fill="Green Status")
# green buildings have a higher rent across all kinds of cooling days (barring 1000-2000 days),
# so we can conclude that no of cooling days does not contribute to the rent of the building

ggplot(data = pretty_green, aes(x = pretty_green$hd_total07_quantiles, 
                                y = pretty_green$Rent_heating, 
                                fill = pretty_green$green_rating_fac))+
  geom_bar(stat = 'identity', position = position_dodge())+
  labs(title="Rent of Green Status Buildings by no. of heating days", 
       y="Rent per SqFt ($)",
       x = "No. of Heating days",
       fill="Green Status")
# From the graph we see that green_rated buildings charga a higher rent when:
# 1. The no of heating degree days are low, implying that the region is hot, or warm enough to not require heating.
# 2. The no of heating degree days are high, implying that there is a need for heating on most days. This implies that the 
# savings in energy costs are higher than the rent of a green building
# In places with heating degree days between 2000-5000, green buildings are in fact commanding a lower rent
# One reason for this could be that for places which are moderately cold, the savings in energy costs are higher 
# than the rent of a green building. We need to check with the total no of degree days to further understand the correlation
# betweem these phenomenon

ggplot(data = pretty_green, aes(x = pretty_green$total_dd_07_quantiles, 
                                y = pretty_green$Rent_total, 
                                fill = pretty_green$green_rating_fac))+
  geom_bar(stat = 'identity', position = position_dodge())+
  labs(title="Rent of Green Status Buildings by no. of degree days", 
       y="Rent per SqFt ($)",
       x = "No. of degree",
       fill="Green Status")
# We cann now confidently say that (a) in places that have less than 5000 degree days (moderate temperatures) and (b) places that 
# have more than 8000 degree days (extreme temperatures), green buildings charge a higher rent. For (b), we can hypothesise
# the reason for higher rent is the savings in energy costs. We will not hold the degree days constant and check if there
# is another feature that can attributed to the variation in rent.

# Feature 1: class of the building
# It is possible that because buildings with degree days > 2000 are better built, and hence charge a premium rent

# holding degree days > 8000 constant
l <- pretty_green[which(pretty_green$total_dd_07_quantiles == ">8000"),]
ggplot(data = l, aes(x = l$class, fill = l$green_rating_fac)) +
  geom_bar(aes(col = l$green_rating_fac), position = position_dodge())
# We see that the number of buildings is too low forclass_b and class_c, to understand if the green premium
# significant enough. So from the next graph, we take a look at only the class A buildings
ggplot(data = l, aes(x = l$class,  y = l$Rent_total,
                     fill = l$green_rating_fac))+
  geom_bar(stat = 'identity', position = position_dodge())+
  labs(title="Rent of Green Status Buildings by class of buildings , for >8000 degree days", 
       y="Rent per SqFt ($)",
       x = "Class",
       fill="Green Status")
# This is telling us for sure that there is a higher rent asscociated with a green building, if it is class_a and in an 
# area with degree days > 8000

#holding degree days between 2000-3000 constant
l <- pretty_green[which(pretty_green$total_dd_07_quantiles == "2000-3000"),]
ggplot(data = l, aes(x = l$class, fill = l$green_rating_fac)) +
  geom_bar(aes(col = l$green_rating_fac), position = position_dodge())
# Once again, we see that the number of buildings is too low class_c, to understand if the green premium
# significant enough. So from the next graph, we take a look at only the class A and B buildings
ggplot(data = l, aes(x = l$class,  y = l$Rent_total,
                     fill = l$green_rating_fac))+
  geom_bar(stat = 'identity', position = position_dodge())+
  labs(title="Rent of Green Status Buildings by class of buildings for degree days between 2000-3000", 
       y="Rent per SqFt ($)",
       x = "Class",
       fill="Green Status")
# This too assures us that there is a green premium for areas with 2000-3000 degree days

#holding degree days between 3000-4000 constant
l <- pretty_green[which(pretty_green$total_dd_07_quantiles == "3000-4000"),]
# by number of buildings in each class
ggplot(data = l, aes(x = l$class, fill = l$green_rating_fac)) +
  geom_bar(aes(col = l$green_rating_fac), position = position_dodge())
# mean rent vs green_rating
ggplot(data = l, aes(x = l$class,  y = l$Rent_total,
                     fill = l$green_rating_fac))+
  geom_bar(stat = 'identity', position = position_dodge())+
  labs(title="Rent of Green Status Buildings by class of buildings for degree days between 3000-4000", 
       y="Rent per SqFt ($)",
       x = "Class",
       fill="Green Status")

#holding degree days between 4000-5000 constant
l <- pretty_green[which(pretty_green$total_dd_07_quantiles == "4000-5000"),]
# by number of buildings in each class
ggplot(data = l, aes(x = l$class, fill = l$green_rating_fac)) +
  geom_bar(aes(col = l$green_rating_fac), position = position_dodge())
# mean rent vs green_rating
ggplot(data = l, aes(x = l$class,  y = l$Rent_total,
                     fill = l$green_rating_fac))+
  geom_bar(stat = 'identity', position = position_dodge())+
  labs(title="Rent of Green Status Buildings by class of buildings for degree days between 4000-5000", 
       y="Rent per SqFt ($)",
       x = "Class",
       fill="Green Status")

# Within class 'A' buildings, investing in a green building is not associated
# with being able to charge higher rents. 
# However, we see that the no. of degree days is correlated with the green premium on the rent. In all areas with greater than
# > 2000 degree days, we see that across the class of the buildings (wherever significant/relevant), 
# the rent is higher for green_rated buildings. Hence we should invest in a green building if they are going to be 
# built in areas with a high number of degree days (>2000), ie areas with extremes of temperature.
# Conversely, do not invest in green buildings if they are located in areas with moderate weather.

