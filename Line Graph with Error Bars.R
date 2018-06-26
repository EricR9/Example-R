# This file creates a line graph with error bars for each level

library(tidyverse)
library(plyr)

# Set the working directory and read in the data
setwd("C:\\Users\\Eric Rannenberg\\Desktop\\KC-130\\DIT Files and R")
full_dataset<-read.csv("HH_trim.csv")

# Isolate each phase
detect<- filter(full_dataset, DIT == "Detect")
ID<- filter(full_dataset, DIT == "ID")
track<- filter(full_dataset, DIT == "Track")

# Data Exploration (Means and Error) ------------------------------------------

std<-function(x){
    sd(x)/sqrt(length(x))
}

xbar_rng<- ddply(full_dataset, .(DIT), summarise, mean = mean(Range)) # mean of DIT
se_rng<- ddply(full_dataset, .(DIT), summarise, se = std(Range)) # std err of mean of DIT

plot_points<- merge(xbar_rng, se_rng, by="DIT") # Assemble mean and std err dataframes

ggplot(plot_points, aes(x=DIT, y=mean))+
    geom_line(aes(group=1))+
    geom_point(size=3)+
    geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.2)+
    ggtitle("Mean Values & Standard Error")+
    theme(plot.title = element_text(hjust = .5))+
    xlab("Phase")+ylab("Range (km)")