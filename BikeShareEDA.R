##Bike Share EDA Code

library(tidyverse)
library(vroom)
library(ggplot2)
library(gridExtra)

#Read data
bike <- read.csv("./Stat348/Kaggle-Bike-Share/train.csv")

#Overview
dplyr::glimpse(bike)
skimr::skim(bike)

#plots
bike$season <- as.character(bike$season)
bike$weather <- as.character(bike$weather)

seasonplot <- ggplot(data=bike, aes(x=season, y=count))+geom_boxplot()
weatherplot <- ggplot(bike, aes(weather, casual))+geom_boxplot()
tempplot <- ggplot(bike,aes(temp, count))+geom_point()+geom_smooth(se=FALSE)
atempplot <- ggplot(bike, aes(atemp, registered))+geom_point()+geom_smooth(se=FALSE)
grid.arrange(seasonplot,weatherplot,tempplot,atempplot, nrow=2,ncol=2)
