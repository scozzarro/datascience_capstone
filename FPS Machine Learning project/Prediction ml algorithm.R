#######################################################################
# FPS Machine learning project
# 24 May 2020
# Gabrielmaria Scozzarro
#    This project born from a suggestion from my company partner who is 
#    a teacher at the Italian VideoGames Accademy as I need a project for my
#    exam during HardvardX: Capstone.
#    The final aim is to built a machine learning algorythm to predict the 
#    winners of a game starting from the players game stats.
#    It's composed by 2 parts: the first one is Exploration and Data Analisys (EDA)
#    the second part is the the creation of the machine learning algorythm,
#    winners prediction and calculate MSE as KPI.
#######################################################################

#######################################################################
#PART 2: Prediction algorithm usign machine learning
#######################################################################

#####################################################
# load data sets (train_V2, test_V2) and libraries (tidyverse,data.table,corrplot)

#####################################################

#Load packages.

library(tidyverse)
library(data.table)
library(caret)


#Load Data if needed

train_set <- fread("C:/Users/elekt/OneDrive/Documenti/datascience_capstone/FPS Machine Learning project/Data input/train_V2.csv")
test_set <- fread("C:/Users/elekt/OneDrive/Documenti/datascience_capstone/FPS Machine Learning project/Data input/test_V2.csv")

#Remove NAs if needed

train_set$winPlacePerc[is.na(train_set$winPlacePerc)]<- 0


#Prepare Data for the model


#To fully take in consideration the team work inside the game,
#the exploration of the map even when the game time is short and 
#the boost and perks found in the game we can create 3 different feature 
#that take all of that and insert them in the train data set

FPS <- function(x){
  x <- x %>%
    mutate(teamwork = (assists*0.2)+(revives*0.8),
           avgDistPerMinute = (walkDistance+rideDistance+swimDistance)/(matchDuration/60),
           itemsFound = (weaponsAcquired+heals+boosts))
  return(x)
}

train_set <- FPS(train_set)

#Drop less important feature and games

less_imp <- c("vehicleDestroys","roadKills ","teamKills", "maxPlace")

train_set <- train_set[, -which(names(train_set) %in% less_imp)]

#Remove games with numGroup == 1 

train_set <- train_set %>% 
  filter(numGroups > 1)

#Splitting Data in train , validation , test

val_index<- createDataPartition(train_set$winPlacePerc, p = 0.2, list = FALSE)
validation<- train_set[val_index,]
train<- train_set[-val_index,]


#since the target value can be any valu from 0 to 1 we can try
#a liner model to predict it 

lm_fit <- train(winPlacePerc ~)
