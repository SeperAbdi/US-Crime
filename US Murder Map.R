#a)Load USArrest dataset
library(ggplot2)
library(MASS)

data(USArrests)

# Lets look at what we have in this data frame. Click on the USArrests data in the
# workspace to bring up a pseudo "data browser"
# What are the mean, median, standard deviation, minimum, and maximum of
# the variable "Murder"?

mean(USArrests$Murder)

median(USArrests$Murder)

sd(USArrests$Murder)

min(USArrests$Murder)

max(USArrests$Murder)

# Or do this to get most of the info, except the standard deviation:

summary(USArrests$Murder)

# Which state has the lowest murder rate?

min(USArrests$Murder)

USArrests[which(USArrests$Murder==0.8), ]

# Which states are in the top quartile for Murder rate?

summary(USArrests$Murder)

USArrests[which(USArrests$Murder >= 11.25), ]

# Let's create a dichotomous factor variable, using the "ifelse" function,
# for states that have more than the median percentage of their population
# residing in urban areas.

summary(USArrests$UrbanPop)

hiUrban <- ifelse(USArrests$UrbanPop>=66, "Hi.Urban", "Lo.Urban")

hiUrbanFactor <- factor(hiUrban)

# Notice this only created a vector in our workspace, not a variable in the data
# frame. Let's create a variable in the data frame and fill it with our vector now.

USArrests$hiUrbanFactor <- hiUrbanFactor

# To keep our workspace tidy, lets get rid of those new vectors.

rm(hiUrban, hiUrbanFactor)

# We can check which number (1 or 2) corresponds with which factor.

as.integer(USArrests$hiUrbanFactor)

# Now that we have this dichotomous measure, lets get rid of the continuous
# one in our data frame just for fun.

USArrests$UrbanPop <- NULL

# Now let's recode murder rates into 3 categories: low, with the first quartile of
# states; medium, with the 2nd and 3rd quartiles; and high, with the last quartile.
# Notice that by including the "USArrests$" before our new variable name, we
# are bypassing the above step to bring a vector into the data frame.

USArrests$murderCat[USArrests$Murder<4.075] <- "Low"

USArrests$murderCat[USArrests$Murder>= 4.075 & USArrests$Murder<11.250] <- "Mid"

USArrests$murderCat[USArrests$Murder>= 11.250] <- "Hi"

# The data browser will not show the new column until we click on the updated
# data frame in the workplace.
# Finally, lets specify that this is a factor variable so we can use it correctly.

USArrests$murderCat <- factor(USArrests$murderCat)

# Let's try plotting the murder and assault rates against each other to see if there
# is a correlation.

plot(USArrests$Assault, USArrests$Murder)

# There appears to be a strong, positive correlation. Let's test it.

cor.test(USArrests$Assault, USArrests$Murder)

# I wonder how hi vs. low urban factors into this? Let's try making the same plot
# again with separate symbols for low and hi urban %, and a legend in the
# bottom-right corner of the graph.

plot(USArrests$Assault, USArrests$Murder, pch=as.integer(
  
  USArrests$hiUrbanFactor))

legend("bottomright", c("Low Urban", "Hi Urban"), pch=1:2)

# Now, let's fit a very simple linear model to this relationship and view a graph
# with the predicted line.

model1 <- lm(USArrests$Murder ~ USArrests$Assault)

summary(model1)

abline(model1)

# Now, let's include an interaction between assaults and our urban factor
# variable to see if what appeared to be different slopes in the plot are sig.

model2 <- lm(USArrests$Murder ~ USArrests$Assault*USArrests$hiUrbanFactor)

summary(model2)

#Nope, it doesn't look like it!
#Please now try to create a factor variable with categories (Low, Med, Hi)
# for "Rape" to see whether the association between Murder and Assault differs
# by states with hi and lo rates of rape. Follow the same steps as above,
# creating a factor variable, plotting the association, create a legend, and test
# the interaction effect.
# Finally, export the data frame (now with 7 variables) into .CSV format.
# Note that you will have to specify a useful file path after "file=" .

write.csv(USArrests, file= "arrests.csv")

# Now, lets bring data of another format into R. The data we just saved will work!
# Remember to give a name to this data frame that will be useful.

USAssaultsData <- read.csv("arrests.csv")

# Now, to clean up your workspace completely, type:

rm(list=ls())

library(MASS)

data(USArrests)

# Lets look at what we have in this data frame. Click on the USArrests data in the
# workspace to bring up a pseudo "data browser"
# What are the mean, median, standard deviation, minimum, and maximum of
# the variable "Murder"?

mean(USArrests$Murder)

median(USArrests$Murder)

sd(USArrests$Murder)

min(USArrests$Murder)

max(USArrests$Murder)

# Or do this to get most of the info, except the standard deviation:

summary(USArrests$Murder)

# Which state has the lowest murder rate?

min(USArrests$Murder)

USArrests[which(USArrests$Murder==0.8), ]

# Which states are in the top quartile for Murder rate?

summary(USArrests$Murder)

USArrests[which(USArrests$Murder >= 11.25), ]

# Let's create a dichotomous factor variable, using the "ifelse" function,
# for states that have more than the median percentage of their population
# residing in urban areas.

summary(USArrests$UrbanPop)

hiUrban <- ifelse(USArrests$UrbanPop>=66, "Hi.Urban", "Lo.Urban")

hiUrbanFactor <- factor(hiUrban)

# Notice this only created a vector in our workspace, not a variable in the data
# frame. Let's create a variable in the data frame and fill it with our vector now.

USArrests$hiUrbanFactor <- hiUrbanFactor

# To keep our workspace tidy, lets get rid of those new vectors.

rm(hiUrban, hiUrbanFactor)

# We can check which number (1 or 2) corresponds with which factor.

as.integer(USArrests$hiUrbanFactor)

# Now that we have this dichotomous measure, lets get rid of the continuous
# one in our data frame just for fun.

USArrests$UrbanPop <- NULL

# Now let's recode murder rates into 3 categories: low, with the first quartile of
# states; medium, with the 2nd and 3rd quartiles; and high, with the last quartile.
# Notice that by including the "USArrests$" before our new variable name, we
# are bypassing the above step to bring a vector into the data frame.

USArrests$murderCat[USArrests$Murder<4.075] <- "Low"

USArrests$murderCat[USArrests$Murder>= 4.075 & USArrests$Murder<11.250] <- "Mid"

USArrests$murderCat[USArrests$Murder>= 11.250] <- "Hi"

# The data browser will not show the new column until we click on the updated
# data frame in the workplace.
# Finally, lets specify that this is a factor variable so we can use it correctly.

USArrests$murderCat <- factor(USArrests$murderCat)

# Let's try plotting the murder and assault rates against each other to see if there
# is a correlation.

plot(USArrests$Assault, USArrests$Murder)

# There appears to be a strong, positive correlation. Let's test it.

cor.test(USArrests$Assault, USArrests$Murder)

# I wonder how hi vs. low urban factors into this? Let's try making the same plot
# again with separate symbols for low and hi urban %, and a legend in the
# bottom-right corner of the graph.

plot(USArrests$Assault, USArrests$Murder, pch=as.integer(
  
  USArrests$hiUrbanFactor))

legend("bottomright", c("Low Urban", "Hi Urban"), pch=1:2)

# Now, let's fit a very simple linear model to this relationship and view a graph
# with the predicted line.

model1 <- lm(USArrests$Murder ~ USArrests$Assault)

summary(model1)

abline(model1)

# Now, let's include an interaction between assaults and our urban factor
# variable to see if what appeared to be different slopes in the plot are sig.

model2 <- lm(USArrests$Murder ~ USArrests$Assault*USArrests$hiUrbanFactor)

summary(model2)


#Please now try to create a factor variable with categories (Low, Med, Hi)
# for "Rape" to see whether the association between Murder and Assault differs
# by states with hi and lo rates of rape. Follow the same steps as above,
# creating a factor variable, plotting the association, create a legend, and test
# the interaction effect.
# Finally, export the data frame (now with 7 variables) into .CSV format.
# Note that you will have to specify a useful file path after "file=" .

write.csv(USArrests, file= "arrests.csv")

# Now, lets bring data of another format into R. The data we just saved will work!

# Remember to give a name to this data frame that will be useful.

USAssaultsData <- read.csv("arrests.csv")

# Prepare the USArrests data
library(dplyr)
arrests <- USArrests 
arrests$region <- tolower(rownames(USArrests))
head(arrests)


if (!"maps" %in% installed.packages()) install.packages("maps")
library(maps)


# Retrieve the states map data and merge with crime data
states_map <- map_data("state")
arrests_map <- left_join(states_map, arrests, by = "region")

# Create the map
ggplot(arrests_map, aes(long, lat, group = group))+
  geom_polygon(aes(fill = Assault), color = "white")+
  scale_fill_viridis_c(option = "C")

