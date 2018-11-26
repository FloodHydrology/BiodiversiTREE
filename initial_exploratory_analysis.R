###################################################################################
#Name: Exploratory Analysis
#Coder: C. Nathan Jones
#Date: 11/7/2018
#Purpose: Explore  ecosystem water use at BiodiversiTREE sites
##################################################################################

##################################################################################
#Step 1:  Setup Workspace---------------------------------------------------------
##################################################################################
#Clear Memory
rm(list=ls(all=TRUE))

#Set Working Directory
setwd("/nfs/njones-data/Research Projects/BiodiversiTREE/data/BDT_Sensor_Data/Initital_Data/Biodiversitree Soil Network Data/")

#add appropriate libarary
library(tidyr)
library(dplyr)
library(data.table)
library(ggplot2)

#download data
df<-fread("BDT_soil_17_2018.csv")
df<-data.frame(df)

##################################################################################
#Step 2:  Ecosystem Water Use-----------------------------------------------------
##################################################################################
#Convert time to ESD 
df$time2<-as.POSIXct(df$time2, "%Y-%m-%d %H:%M:%S")
df$time2<-df$time2+3600*5

#create table to describe soil layer depths
layers<-data.frame(height = c(-2, -12, -22, -32, -42, -52, -62, -72, -82), 
                   dz    =  c(7 ,  10,  10,  10,  10,  10,  10,  10,  10))
df <- left_join(df, layers, by='height')

#List rain days

#Estimate ET for 1 Day-------------------------
output<-df %>% 
  #Estimate time metrics
  mutate(jday= strptime(time2, "%Y-%m-%d %H:%M:%S")$yday, 
         hour= strptime(time2, "%Y-%m-%d %H:%M:%S")$h,
         min = strptime(time2, "%Y-%m-%d %H:%M:%S")$min) %>%
  filter(jday!=112,
         jday!=119) %>%
  #Estimate hourly average
  group_by(plot,jday, hour, height) %>%
  summarise(vwc=mean(vwc), 
            dz =mean(dz)) %>%
  mutate(time=jday + hour/24,
         tsm =vwc*dz/100) %>%
  #Estimate tsm for enitre soil profile
  group_by(plot, jday, hour) %>%
  summarise(tsm=sum(tsm)) %>%
  # #Estimate hourly stats
  group_by(plot,jday) %>%
  summarise(tsm0 = tsm[hour==0],
            s    = (tsm[hour==6]-tsm[hour==0])/6) %>%
  mutate(tsm1 = lead(tsm0)) %>%
  mutate(ET=tsm0-tsm1+(24*s)) %>%
  filter(jday==117) %>%
  filter(ET<0, ET>-0.5) %>%
  mutate(ET=ET*-1)

#plot---------------------
par(ps=12)
par(cex.lab=14/12)
par(cex.axis=10/12)
hist(output$ET, breaks=12, 
     ylab="Number of Plots", xlab= "Ecosystem Water Use [mm/day]", main=NA, 
     yaxs="i", col="grey70")
box(bty="l")
