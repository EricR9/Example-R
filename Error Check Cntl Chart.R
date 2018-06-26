# Author: Eric Rannenberg

# This builds a control chart for individual observations (sample size = 1)
# through the use of the moving range of two successive observations to 
# measure process variability

library(tidyverse)
library(gridExtra) 

# Set working directory and read in data
setwd("C:\\Users\\Eric Rannenberg\\Desktop\\KC-130\\DIT Files and R")
full_dataset<-read.csv("HH_trim.csv")

# Isolate run number and range value for evaluation
detect<- filter(full_dataset, DIT == "Detect")
det_run<- detect$Run
det_rng<- detect$Range  

ID<- filter(full_dataset, DIT == "ID")
ID_run<- ID$Run 
ID_rng<- ID$Range  

track<- filter(full_dataset, DIT == "Track")
trk_run<- track$Run
trk_rng<- track$Range  

# Calculate the moving range(s)
moving_rng<- function (x) {
    mr<-c()
    for (i in 1:length(x)) {
        mr_calc<- abs(x[i-1] - x[i])
        mr<-c(mr, mr_calc)
    }
    return(c(NA,mr)) # Need to add an NA to slide the values down by one
}

det_mov_rng<- moving_rng(det_rng)
id_mov_rng<- moving_rng(ID_rng)
trk_mov_rng<- moving_rng(trk_rng)

# Combine observations and moving ranges into data frame(s)
det_df<-data.frame(Run=det_run, Value=det_rng, MR=det_mov_rng)
ID_df<-data.frame(Run=ID_run, Value=ID_rng, MR=id_mov_rng)
trk_df<-data.frame(Run=trk_run, Value=trk_rng, MR=trk_mov_rng)

# Calculate the average range and standard deviation of moving range
# UCL and LCL are computed by (xbar +/- 3*MR_bar/d2) 
# where s = std dev of MR, c4 = value for n=2

d2<- 1.128 # value extracted from appendix VI, Intro Statistical Quality Control, Montgomery

# Function to calculate LCL but only return values larger than zero, otherwise 0
LCL<- function (Value, MR){
    lcl<- mean(Value) - 3*(mean(MR, na.rm = TRUE)/d2)
    ifelse(lcl >= 0, lcl, 0)
}

# Detect
det_UCL<- mean(det_df$Value) + 3*(mean(det_df$MR, na.rm = TRUE)/d2)
det_LCL<- LCL(det_df$Value, det_df$MR) #Function call
det_xbar<- mean(det_df$Value)

det_cntl<-round(c(det_LCL, det_xbar, det_UCL),1)
names(det_cntl)<-c("LCL", "Mean", "UCL"); det_cntl

# ID
ID_UCL<- mean(ID_df$Value) + 3*(mean(ID_df$MR, na.rm = TRUE)/d2)
ID_LCL<- LCL(ID_df$Value, ID_df$MR) #Function call
ID_xbar<- mean(ID_df$Value)

ID_cntl<-round(c(ID_LCL, ID_xbar, ID_UCL),1)
names(ID_cntl)<-c("LCL", "Mean", "UCL"); ID_cntl

# Track
trk_UCL<- mean(trk_df$Value) + 3*(mean(trk_df$MR, na.rm = TRUE)/d2)
trk_LCL<- LCL(trk_df$Value, trk_df$MR)
trk_xbar<- mean(trk_df$Value)

trk_cntl<-round(c(trk_LCL, trk_xbar, trk_UCL),1)
names(trk_cntl)<-c("LCL", "Mean", "UCL"); trk_cntl

# Construct the control chart(s)
det_chart <- ggplot(det_df, aes(x=Run,y=Value))+
    geom_point()+
    geom_hline(yintercept = det_LCL, color = "red", lwd=1.2)+
    geom_hline(yintercept = det_UCL, color = "red", lwd=1.2)+
    geom_hline(yintercept = det_xbar, color = "darkgreen")+
    geom_line()+
    ylab("Observed Value")+
    scale_x_continuous(breaks = seq(1, 60, 1))+
    scale_y_continuous(limits=c(0,60), breaks = seq(0, 60, 5))+
    annotate("text",x=52, y=det_cntl[3]-2, label="UCL = 58.7")+
    annotate("text",x=52, y=det_cntl[1]+2, label="LCL = 0")+
    annotate("text",x=52, y=det_cntl[2]+2, label="Mean = 23.1")+
    ggtitle("Detection Control Chart")+
    theme(plot.title = element_text(hjust=.5))

ID_chart <- ggplot(ID_df, aes(x=Run,y=Value))+
    geom_point()+
    geom_hline(yintercept = ID_LCL, color = "red", lwd=1.2)+
    geom_hline(yintercept = ID_UCL, color = "red", lwd=1.2)+
    geom_hline(yintercept = ID_xbar, color = "darkgreen")+
    geom_line()+
    ylab("Observed Value")+
    scale_x_continuous(breaks = seq(1, 60, 1))+
    scale_y_continuous(limits=c(0,60), breaks = seq(0, 60, 5))+
    annotate("text",x=52, y=ID_cntl[3]-2, label="UCL = 34.6")+
    annotate("text",x=52, y=ID_cntl[1]+2, label="LCL = 0")+
    annotate("text",x=52, y=ID_cntl[2]+2, label="Mean = 14.1")+
    ggtitle("Identification Control Chart")+
    theme(plot.title = element_text(hjust=.5))


trk_chart <- ggplot(trk_df, aes(x=Run,y=Value))+
    geom_point()+
    geom_hline(yintercept = trk_LCL, color = "red", lwd=1.2)+
    geom_hline(yintercept = trk_UCL, color = "red", lwd=1.2)+
    geom_hline(yintercept = trk_xbar, color = "darkgreen")+
    geom_line()+
    ylab("Observed Value")+
    scale_x_continuous(breaks = seq(1, 60, 1))+
    scale_y_continuous(limits=c(0,60), breaks = seq(0, 60, 5))+    
    annotate("text",x=52, y=trk_cntl[3]-2, label="UCL = 21.0")+
    annotate("text",x=52, y=trk_cntl[1]+2, label="LCL = 1.8")+
    annotate("text",x=52, y=trk_cntl[2]+2, label="Mean = 11.4")+
    ggtitle("Tracking Control Chart")+
    theme(plot.title = element_text(hjust=.5))


grid.arrange(det_chart, ID_chart, trk_chart, nrow=3)







