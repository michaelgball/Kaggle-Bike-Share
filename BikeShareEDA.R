##bikeTrain Share EDA Code

library(tidyverse)
library(vroom)
library(ggplot2)
library(gridExtra)
library(tidymodels)

#Read data
bikeTrain <- read.csv("./Stat348/Kaggle-Bike-Share/train.csv")
bikeTest <- read.csv("./Stat348/Kaggle-Bike-Share/test.csv")

#Overview
dplyr::glimpse(bikeTrain)
skimr::skim(bikeTrain)

#plots
bikeTrain$season <- as.character(bikeTrain$season)
bikeTrain$weather <- as.character(bikeTrain$weather)

seasonplot <- ggplot(data=bikeTrain, aes(x=season, y=count))+geom_boxplot()
weatherplot <- ggplot(bikeTrain, aes(weather, casual))+geom_boxplot()
tempplot <- ggplot(bikeTrain,aes(temp, count))+geom_point()+geom_smooth(se=FALSE)
atempplot <- ggplot(bikeTrain, aes(atemp, registered))+geom_point()+geom_smooth(se=FALSE)
grid.arrange(seasonplot,weatherplot,tempplot,atempplot, nrow=2,ncol=2)
