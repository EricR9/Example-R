# This file creates boxplots and density plots of the data in order to understand 
# the underlying distribution.

library(tidyverse)
library(plyr)

# Set the working directory and read in the data
setwd("C:\\Users\\Eric Rannenberg\\Desktop\\KC-130\\DIT Files and R")
full_dataset<-read.csv("HH_trim.csv")

# Isolate each phase
detect<- filter(full_dataset, DIT == "Detect")
ID<- filter(full_dataset, DIT == "ID")
track<- filter(full_dataset, DIT == "Track")

# Histogram with boxplot and normal curve plus Shapiro_Wilk Test -----------------
# Creates a horizontal boxplot 
layout(mat = matrix(c(1,2),2,1, byrow=TRUE), height = c(1,8))
par(mar=c(0,3.9,1.1,2.9))
boxplot(track$Range, horizontal=TRUE, col= "gold2", ylim=c(0,25), xaxt="n", frame=FALSE)

# Creates the histogram
par(mar=c(4,3.9,1.1,2.9))
h<-hist(track$Range, xlim=c(0,25), breaks=20, col= "darkslategray4", 
    main="", xlab="Track Range (km)")

# Creates the normal curve
xfit<-with(track, seq(min(Range), max(Range), length=50))
yfit<-with(track, dnorm(xfit, mean(Range), sd(Range)))
yfit<-yfit*diff(h$mids[1:2])*length(track$Range)
lines(xfit,yfit, col="red",lwd=2)
legend("topright",legend="Normal Curve",lty = 1, lwd=2, col="red", cex=.8)

# Test for normality
norm<-shapiro.test(track$Range) # test for normality
normp<-round(norm$p.value,7)
text(25,1.7, paste("Shapiro-Wilk Test:\np-value= ", normp), cex=.8)


# Density plots ----------------------------------------------------------------
median_range<- ddply(full_dataset, .(DIT), summarize, median=median(Range)) #uses plyr

# Density plot with median values overlaid with threshold
ggplot(full_dataset, aes(x=Range, fill=DIT))+
    geom_density(alpha=.3)+
    xlab("Slant Range (km)")+
    ylab("Density")+
    geom_vline(xintercept = 9, color="red", size=1)+ # threshold value
    geom_vline(data= median_range, aes(xintercept = median, color = DIT, linetype = DIT),
        size=1.2)+ # overlaid individual median values from different dataframe
    ggtitle("Density Plot With Median Values Relative to Threshold")+
    theme(plot.title = element_text(hjust = .5))

# This plot combines a histogram with density plot
ggplot(detect, aes(x=Range, y=..density..))+
    geom_histogram(binwidth= 4, fill="cornsilk", color="grey60", size=.2)+
    geom_density()+
    geom_vline(xintercept = 9, color="red", size=1)+
    ggtitle("Detection Density/Histogram")+
    theme(plot.title = element_text(hjust = .5))+
    xlab("Slant Range (km)")+
    ylab("Density")


