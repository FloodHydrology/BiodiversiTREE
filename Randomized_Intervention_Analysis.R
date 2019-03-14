##############################################################
#Title: Randomized Intervention Analysis
#Coder: Nate Jones
#Date: 3/13/2019
#Purpose: Conduct RIA analysis to examine impact of forest restoration 
##############################################################

##############################################################
#Step 1: Setup workspace--------------------------------------
##############################################################
#Download required libraries
library('dygraphs')
library('xts')
library('EcoHydRology')
library('tidyverse')

#Define directories of interest
data_dir<-"/nfs/njones-data/Research Projects/BiodiversiTREE/data/flow_data/"
output_dir<-"/nfs/njones-data/Research Projects/BiodiversiTREE/plots/"

#Download streamflow data~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ws109<-read.csv(paste0(data_dir,'dflo109_07_18.csv'))
ws110<-read.csv(paste0(data_dir,'dflo110_07_18.csv'))

#Convert date format to POSIX
ws109$DAY<-as.POSIXct(ws109$DAY, format="%d-%b-%y")
ws110$DAY<-as.POSIXct(ws110$DAY, format="%d-%b-%y")

#Convert from ft3/d to mm/hr
ws109 %<>% mutate(mm_day= FLOW__CF/167498.6*(0.3048^3)*1000) %>% select(DAY,mm_day)
ws110 %<>% mutate(mm_day= FLOW__CF/58382.48*(0.3048^3)*1000) %>% select(DAY,mm_day)

#Combine flow data
df<-left_join(ws109, ws110, by="DAY")
colnames(df)<-c("day", "q109", "q110")
df<-na.omit(df)
df<-as_tibble(df) %>%
  mutate(day  = as.POSIXct(day, format="%d-%b-%y"))

#Precip data~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#install precip 
precip<-read_csv(paste0(data_dir, "BWI_precip.csv")) %>%
  select(DATE, PRCP) %>%
  mutate(PRCP=PRCP*25.4) %>%
  rename(day=DATE) %>%
  mutate(day=as.POSIXct(day,format="%y-%m-%d")) %>%
  mutate(day=floor_date(day, unit="day"))
df<-left_join(df, precip, by="day")

#Cleanup workspace~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Limit data to 2008 to 2018
df<-df %>% filter(year(day)>=2007)

##############################################################
#Step 2: Baseflow seperation----------------------------------
##############################################################
#Conduct Baseflow Seperation Nathan and McMahon, 1990
df$bf109<-BaseflowSeparation(df$q109, passes=3)[,1]
df$ro109<-BaseflowSeparation(df$q109, passes=3)[,2]
df$bf110<-BaseflowSeparation(df$q110, passes=3)[,1]
df$ro110<-BaseflowSeparation(df$q110, passes=3)[,2]

##############################################################
#Step 3: Monthly Analysis-------------------------------------
##############################################################
#Sum by month
df<-df %>% 
  group_by(date=floor_date(day, "month")) %>%
  mutate(q109=q109, 
         q110=q110) %>%
  summarize(PRCP  = sum(PRCP),
            q109  = sum(q109), 
            q110  = sum(q110), 
            ro109 = sum(ro109), 
            ro110 = sum(ro110), 
            bf109 = sum(bf109), 
            bf110 = sum(bf110)) %>%
  mutate(q_diff=q109-q110,
         ro_diff=ro109-ro110,
         bf_diff=bf109-bf110, 
         month=month(date),
         year=year(date))

#Seperate before and after
b<-df %>% 
  filter(year<2013) %>%
  group_by(month) %>%
  summarise(q       = mean(q_diff)/sum(PRCP)*100,
            q_se    = sd(q_diff)/sqrt(5)/sum(PRCP)*100,
            ro      = mean(ro_diff)/sum(PRCP)*100,
            ro_se   = sd(ro_diff)/sqrt(5)/sum(PRCP)*100,
            bf      = mean(bf_diff)/sum(PRCP)*100, 
            bf_se   = sd(bf_diff)/sqrt(5)/sum(PRCP)*100)

a<-df %>% 
  filter(year>=2013) %>%
  group_by(month) %>%
  summarise(q       = mean(q_diff)/sum(PRCP)*100,
            q_se    = sd(q_diff)/sqrt(5)/sum(PRCP)*100,
            ro      = mean(ro_diff)/sum(PRCP)*100,
            ro_se   = sd(ro_diff)/sqrt(5)/sum(PRCP)*100,
            bf      = mean(bf_diff)/sum(PRCP)*100, 
            bf_se   = sd(bf_diff)/sqrt(5)/sum(PRCP)*100)

#Correct for water year
a$month<-c(4,5,6,7,8,9,10,11,12,1,2,3)
b$month<-c(4,5,6,7,8,9,10,11,12,1,2,3)
a<-a[order(a$month),]
b<-b[order(b$month),]

##############################################################
#Step 4: Monthly Analysis Plots-------------------------------
##############################################################

#Inital plots~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Start emf device
png(paste0(output_dir,"flow_diff.png"), w=5,h=3.5,units="in", res=300)

#Plotting Parameters
par(mar=c(3.3,4.5,0.5,0.5))
par(mgp=c(1.5,0.6,0)) 
par(ps=12)
par(cex.lab=12/12)
par(cex.axis=10/12)

#Plot empty plot
plot(a$month,a$q, type="n", 
     #Axes limits
     ylim=c(-9,6),
     #Axes labels
     xlab="Month", ylab="Mean Monthly Stremflow Difference\n[% of Monthly Precip]", 
     #xaxis labels
     xaxt="n"
     )

#Add axis
axis(1, at=c(1,4,8,12), labels=c("Oct","Jan", "May", "Sept"))

#Add Zero Line
abline(h=0, lty=2, lwd=2, col="grey30")

#Add error bars
for(i in 1:12){
  arrows(a$month[i], a$q[i]+a$q_se[i], a$month[i], a$q[i]-a$q_se[i], length = 0)
  arrows(b$month[i], b$q[i]+b$q_se[i], b$month[i], b$q[i]-b$q_se[i], length = 0)
}

#Add points 
points(a$month, a$q, type="l", col="grey30", lty=2, lwd=1.3)
points(a$month, a$q, type="p", bg="dark green", pch=21, cex=1.3)

#Add points 
points(b$month, b$q, type="l", col="grey30", lty=1, lwd=1.3)
points(b$month, b$q, type="p", bg="orange", pch=21, cex=1.3)

#Add Legend
legend("topright", cex=1.15,
       legend=c("Before Restoration", "After Restoration"), 
       pch = 21, 
       pt.bg=c("orange", "dark green"), 
       lty  = c(1,2), 
       bty="n")
       

#Turn Device Off
dev.off()

#--------------------
#Hydrograph Plots
png(paste0(output_dir,"monthly_hydrograph.png"), w=5,h=3.5,units="in", res=300)

#Plotting Parameters
par(mar=c(3.3,4.5,0.5,0.5))
par(mgp=c(1.5,0.6,0)) 
par(ps=12)
par(cex.lab=12/12)
par(cex.axis=10/12)

h<-df %>% 
  group_by(month, year) %>%
  summarise(date,
            q109= sum(q109),
            q110= sum(q110)) %>%
  arrange(date)

#Plot empty plot
plot(h$date,h$q109, type="n", 
     #Axes limits
     ylim=c(0,175),
     #Axes labels
     xlab="", ylab="Monthly Streamflow Totals\n[mm/month]"
)

abline(v=as.POSIXct("01-01-2013", format="%m-%d-%Y"), 
       lty=2, lwd=2, col="grey30")

points(h$date,h$q109, type="l", col="#b2182b", lwd=1.3)
points(h$date,h$q110, type="l", col="#2b8cbe")
legend("topright", cex=1.15,
       legend=c("Forested", "Restored"), 
       col=c("#2b8cbe", "#b2182b"), 
       lty  = c(1,1), 
       bty="n")
dev.off()

#--------------------
#Difference Plots
png(paste0(output_dir,"monthly_diff.png"), w=5,h=3.5,units="in", res=300)

#Plotting Parameters
par(mar=c(3.3,4.5,0.5,0.5))
par(mgp=c(1.5,0.6,0)) 
par(ps=12)
par(cex.lab=12/12)
par(cex.axis=10/12)

h<-df %>% 
  group_by(month, year) %>%
  summarise(date,
            diff= (sum(q109)-sum(q110))/sum(PRCP)*10) %>%
  arrange(date)

#Plot empty plot
plot(h$date,h$diff, type="n", 
     #Axes limits
     ylim=c(-7.5,6),
     #Axes labels
     xlab="", ylab="Monthly Stremflow Difference\n[% of Monthly Precip]"
)

abline(v=as.POSIXct("01-01-2013", format="%m-%d-%Y"), 
       lty=2, lwd=2, col="grey30")
abline(h=0, lty=2, lwd=2, col="grey30")
points(h$date,h$diff, type="l", col="#542788", lwd=1.3)
dev.off()
##############################################################
#Step 4: Randomized Intervention Analysis --------------------
##############################################################
#Use methods from: https://doi.org/10.1579/0044-7447-38.7.357