#assuming that Central Park South & 6th Ave” is == Central Park S & 6th Ave”
#assuming that fee is collected at the end of the trip.

install.packages("mice")
install.packages("geosphere")
install.packages("forecast")
install.packages("xts")
library(xts)
library(mice)
library(geosphere)
library(dplyr)
library(data.table)
library(lubridate)
library(PerformanceAnalytics)
library(plyr)
round_any(61,15,f = ceiling)

ctrip04 <- read.csv("C:/Users/vpran/Desktop/Jaggu DC/201604-citibike-tripdata.csv")
ctrip05 <- read.csv("C:/Users/vpran/Desktop/Jaggu DC/201605-citibike-tripdata.csv")
ctrip06 <- read.csv("C:/Users/vpran/Desktop/Jaggu DC/201606-citibike-tripdata.csv")
weather <- read.csv("C:/Users/vpran/Google Drive/Jaggu DC/central_park_weather.csv")

ctrip <- rbind(ctrip04,ctrip05,ctrip06)
head(ctrip)
summary(ctrip)
pattern <- md.pattern(ctrip)

ctrip_mastersub <- ctrip[(ctrip$end.station.name == "Central Park S & 6 Ave") & ctrip$usertype == "Customer",]
ctrip_subs <- ctrip_mastersub
colnames(ctrip_subs)
summary(ctrip)
pattern <- md.pattern(ctrip)
ctrip_subs <- ctrip_subs[,c(1,2,3,6,7,10,11)] #for the subsetted data gender and age are NA, verified through data table.

ctrip_subs_tmp <- ctrip_subs

#distance between lat longs are calculated but it may not be applied to customer and may only be aplicable to customers
#since customers may take random paths for sightseeing
setDT(ctrip_subs_tmp)
ctrip_subs_tmp[,distance := distHaversine(matrix(c(start.station.longitude, start.station.latitude),ncol = 2),
                                          matrix(c(end.station.longitude, end.station.latitude),ncol = 2))]

ctrip_subs <- cbind(ctrip_subs,ctrip_subs_tmp$distance)
colnames(ctrip_subs)
ctrip_subs <- ctrip_subs[,c(1,2,3,7)]
ctrip_subs_tmp <- ctrip_subs

ctrip_subs_tmp$startdate <- as.Date(ctrip_subs_tmp$starttime, format = "%m/%d/%Y %H:%M:%S")
ctrip_subs_tmp$starttime <- as.POSIXct(ctrip_subs_tmp$starttime, format = "%m/%d/%Y %H:%M:%S")

ctrip_subs_tmp$stopdate <- as.Date(ctrip_subs_tmp$stoptime, format = "%m/%d/%Y %H:%M:%S")
ctrip_subs_tmp$stoptime <- as.POSIXct(ctrip_subs_tmp$stoptime, format = "%m/%d/%Y %H:%M:%S")

#no of returns on different days
ctrip_subs_tmp$startdayofmonth <- as.numeric(format(ctrip_subs_tmp$startdate,'%d'))
ctrip_subs_tmp$stopdayofmonth <- as.numeric(format(ctrip_subs_tmp$stoptime,'%d'))
total_returns = nrow(ctrip_subs_tmp)
total_returns_sameday = sum(ctrip_subs_tmp[,ctrip_subs_tmp$startdayofmonth==ctrip_subs_tmp$stopdayofmonth])
total_returns_diffday = total_returns - total_returns_sameday
#since these 29 cases constitute 0.2% of the data, we will not be handling them separately although they do contribute to revenue across 2 days
#Assumption here that a bike is taken and returned on the same day and the revenue will be calculated and added to the day the bike is returned hence endtime will be considered for forecasting
#per tableau insight the peak for "endtime or bike returns" happen between 10AM-7PM for customer returns at CPS6A

ctrip_subs_tmp$hour <- as.numeric(format(ctrip_subs_tmp$stoptime,'%H'))

ctrip_subs_tmp$weekday <- as.numeric(format(ctrip_subs_tmp$stoptime,'%u'))

holidays_date <- as.Date(c("2016-05-30","2016-06-07"),format = "%Y-%m-%d")
holidays_name <- c("memorial day","ramadan start")
holiday_table <- as.data.frame(holidays_date,holidays_name)

ctrip_subs_tmp$ishol <- ifelse(ctrip_subs_tmp$stopdate %in% holidays_date,1,0)
ctrip_subs_tmp$isweekend <- ifelse(ctrip_subs_tmp$weekday %in% c(6,7),1,0)  #capturing insign on customer activity since most customer activity is on weekends
ctrip_subs_tmp$ispeaktime <- ifelse(ctrip_subs_tmp$hour>=10 & ctrip_subs_tmp$hour<=19,1,0)  #per insights from tableau customer peak usage peaks betwwen 9-9
colnames(ctrip_subs_tmp)

#Aggregating dataset for modelling
ctrip_subs_mod <- ctrip_subs_tmp[,c(1,6,8,10,11,12,13)]
head(ctrip_subs_mod)
ctrip_subs_mod <- ctrip_subs_mod[,c(2,1,3,4,5,6,7)]
colnames(ctrip_subs_mod)
ctrip_subs_mod <- as.data.frame(ctrip_subs_mod)
ctrip_subs_mod$revenue = ifelse((ctrip_subs_mod$tripduration/60)<30,4,4+((round_any(ctrip_subs_mod$tripduration/60,15,f = ceiling)-30)*(4/15)))
s1 <- aggregate(cbind(tripduration,ispeaktime,revenue)~ stopdate, data = ctrip_subs_mod, FUN = sum)
s2 <- aggregate(cbind(stopdayofmonth,weekday,ishol,isweekend)~ stopdate, data = ctrip_subs_mod, FUN = max)
s3 <- aggregate(tripduration ~ stopdate, data = ctrip_subs_mod, FUN = NROW)
ctrip_subs_mod <- cbind.data.frame(s1,s2,s3)
colnames(ctrip_subs_mod)
ctrip_subs_mod <- ctrip_subs_mod[,c(1,2,3,4,6,7,8,9,11)]
colnames(ctrip_subs_mod) <- c("date","total_trip_duration","no_peak_customers","revenue","day_of_month","weekday","is_hol","is_weekend","total_customers")
head(ctrip_subs_mod)

#Imported weather for the three months and mapped the date inthe exact format
weather$DATE <- ctrip_subs_mod$date

#Adding weather data to the final model
ctrip_subs_mod <- cbind.data.frame(ctrip_subs_mod,weather$PRCP,weather$TMAX,weather$TMIN)
colnames(ctrip_subs_mod) <- c("date","total_trip_duration","no_peak_customers","revenue","day_of_month","weekday","is_hol","is_weekend","total_customers","precip","tmax","tmin")

#Creating a time series model
triprevenue <- ts(ctrip_subs_mod$revenue, start=c(2016,4),end=c(2016,6),frequency=24)
plot.ts(triprevenue)

