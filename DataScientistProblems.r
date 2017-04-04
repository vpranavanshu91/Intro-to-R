#Assumptions
#1 Assuming that Central Park South & 6th Ave" signifies Central Park S & 6th Ave"
#2 The revenue will be calculated and added to the day the bike is returned hence endtime will be considered for forecasting
#3 Assumption here is that a bike is taken and returned on the same day 

#loading libraries
library(lmtest)
library(rmarkdown)
library(astsa)
library(xts)
library(mice)
library(geosphere)
library(dplyr)
library(data.table)
library(plyr)
library(forecast)
library(lubridate)
library(tseries)

#Reading csvs from desktop
ctrip04 <- read.csv("/201604-citibike-tripdata.csv")
ctrip05 <- read.csv("/201605-citibike-tripdata.csv")
ctrip06 <- read.csv("/201606-citibike-tripdata.csv")
weather <- read.csv("/central_park_weather.csv")

#binding csvs into a single df and removing the rest
ctrip <- rbind(ctrip04,ctrip05,ctrip06)
rm(list = c("ctrip04","ctrip05","ctrip06"))

#checking basic outlook of data
head(ctrip)
summary(ctrip)
md.pattern(ctrip)

#Subsetting the data based on the problem statement and looking into summary statistics
ctrip_subs <- ctrip[(ctrip$end.station.name == "Central Park S & 6 Ave") & ctrip$usertype == "Customer",]
colnames(ctrip_subs)
summary(ctrip)
pattern <- md.pattern(ctrip)
ctrip_subs <- ctrip_subs[,c(1,2,3,6,7,10,11)] 
#For the above subsetted data 'gender' and 'age' are NA, verified through data table  
#distance between lat longs are calculated but it may not be applied to customer 
#and may only be aplicable to subscribers since customers may take random paths for sightseeing

##############Feature Engineering#########################

#Distance calculation between latlongs (Unused in the model)
setDT(ctrip_subs)
ctrip_subs[,distance := distHaversine(matrix(c(start.station.longitude, start.station.latitude),ncol = 2),
                                      matrix(c(end.station.longitude, end.station.latitude),ncol = 2))]

colnames(ctrip_subs)
ctrip_subs <- ctrip_subs[,c(1,2,3,8)]

#Preprocessing dates into required formats for feature extraction
ctrip_subs$startdate <- as_date(mdy_hms(ctrip_subs$starttime))
ctrip_subs$starttime <- as_datetime(mdy_hms(ctrip_subs$starttime))
ctrip_subs$stopdate <- as_date(mdy_hms(ctrip_subs$stoptime))
ctrip_subs$stoptime <- as_datetime(mdy_hms(ctrip_subs$stoptime))

#####Testing if all bikes taken are returned on the same day################
ctrip_subs$startdayofmonth <- as.numeric(format(ctrip_subs$startdate,'%d'))
ctrip_subs$stopdayofmonth <- as.numeric(format(ctrip_subs$stoptime,'%d'))
total_returns = nrow(ctrip_subs)
total_returns_sameday = sum(ctrip_subs[,ctrip_subs$startdayofmonth==ctrip_subs$stopdayofmonth])
total_returns_diffday = total_returns - total_returns_sameday
#There are a total of 29 cases with different return dates, that constitute 0.2% of the data,
#we will not be handling them separately although they do contribute to revenue across 2 days
#Per tableau insight the peak for "endtime or bike returns" happen between 10AM-7PM for customer returns at CPS6A

#Extracting features from the date, features can be used as regressors for ts modeling
ctrip_subs$hour <- as.numeric(format(ctrip_subs$stoptime,'%H'))
ctrip_subs$weekday <- as.numeric(format(ctrip_subs$stoptime,'%u'))
holidays_date <- as.Date(c("2016-05-30","2016-06-07"),format = "%Y-%m-%d")
holidays_name <- c("memorial day","ramadan start")
holiday_table <- as.data.frame(holidays_date,holidays_name)
ctrip_subs$ishol <- ifelse(ctrip_subs$stopdate %in% holidays_date,1,0)
ctrip_subs$isweekend <- ifelse(ctrip_subs$weekday %in% c(6,7),1,0)  #capturing insign on customer activity since most customer activity is on weekends
ctrip_subs$ispeaktime <- ifelse(ctrip_subs$hour>=10 & ctrip_subs$hour<=19,1,0)  #per insights from tableau customer peak usage peaks betwwen 9-9
colnames(ctrip_subs)

################Aggregating dataset for modelling####################
#Aggregating dataset to obtain daily statistics

#Some preprocessing before aggregating
ctrip_subs <- ctrip_subs[,c(1,6,8,10,11,12,13)]
colnames(ctrip_subs)
ctrip_subs <- ctrip_subs[,c(2,1,3,4,5,6,7)]
colnames(ctrip_subs)
ctrip_subs <- as.data.frame(ctrip_subs)

#Creating a derieved revenue column for each trip prior to aggregating
ctrip_subs$revenue = ifelse((ctrip_subs$tripduration/60)<30,4,4+((round_any(ctrip_subs$tripduration/60,15,f = ceiling)-30)*(4/15)))

#Aggregating the sets of variables with appropriate functions
# tripduration,ispeaktime,revenue < - Sum - will give total trip duration, total customers availing peak time service, total revenue
# tripduration <- nrow -  will give no. of customers
# stopdayofmonth,weekday,ishol,isweekend <- MAX - These will be same for all trips in a day, hence using MAX
s1 <- aggregate(cbind(tripduration,ispeaktime,revenue)~ stopdate, data = ctrip_subs, FUN = sum)
s2 <- aggregate(cbind(stopdayofmonth,weekday,ishol,isweekend)~ stopdate, data = ctrip_subs, FUN = max)
s3 <- aggregate(tripduration ~ stopdate, data = ctrip_subs, FUN = NROW)

#Aggregating the columns and pre processing
ctrip_subs_mod <- cbind.data.frame(s1,s2,s3)
rm(list = c("s1","s2","s3"))
colnames(ctrip_subs_mod)
ctrip_subs_mod <- ctrip_subs_mod[,c(1,2,3,4,6,7,8,9,11)]
colnames(ctrip_subs_mod) <- c("date","total_trip_duration","no_peak_customers","revenue","day_of_month","weekday","is_hol","is_weekend","total_customers")
head(ctrip_subs_mod)

#Imported weather for the three months and mapped the date inthe exact format
weather$DATE <- ctrip_subs_mod$date

#Adding weather data to the final model
ctrip_subs_mod <- cbind.data.frame(ctrip_subs_mod,weather$PRCP,weather$TMAX,weather$TMIN)
colnames(ctrip_subs_mod) <- c("date","total_trip_duration","no_peak_customers","revenue","day_of_month","weekday","is_hol","is_weekend","total_customers","precip","tmax","tmin")

##################################################################################

#creating model ready dataset  train and test

ctrip_subs_mod <- as.data.table(ctrip_subs_mod)
head(ctrip_subs_mod)
ctrip_subs_mod$is_hol_wek_int <- ctrip_subs_mod$is_hol + ctrip_subs_mod$is_weekend
ctrip_subs_mod <- ctrip_subs_mod[,c(1,2,4,9,12,13,6)]
ctrip_subs_mod_train <- ctrip_subs_mod[1:84,]
ctrip_subs_mod_test <-ctrip_subs_mod[84:91,c(3)]
train <- as.vector(ctrip_subs_mod_train$revenue)
test <- as.vector(ctrip_subs_mod_test$revenue)

##################################################################################
######################Creating a time series model##########################
# We have created ts and xts objects
# ts objects have better compatibility with forecasting models but fail to take fragmented data 
# into account, it look for a full year of data and non daily dates to add as index 
# These show day numbers in their plots which is alright and also show proper lags in
# ACF and PACF plots

# xts objects are used because they have great compatibility with fragmented data
# and also show dates in the plots

#DATES have been converted to POSIXct formats

##################creating xts object###################################
cbtrips <- as.xts.data.table(ctrip_subs_mod_train)
index(cbtrips)
graphics.off()
plot(cbtrips[,"revenue"],major.ticks= "weeks",main = "Revenue")

#############creating TS object for modelling with regressors##############

ctrip_subs_mod_ts <- cbind(ctrip_subs_mod_train,ymd(ctrip_subs_mod_train$date))
ctrip_subs_mod_ts <- ctrip_subs_mod_ts[,c(6,2,3,4,5)]

#created ts objects for all 4 variables, 3 of which will be tested in regressors
obj_ts_rev <- ts(ctrip_subs_mod_ts$revenue,frequency =  1)
obj_ts_tmin <- ts(ctrip_subs_mod_ts$tmin,frequency =  1)
obj_ts_tc <- ts(ctrip_subs_mod_ts$total_customers,frequency =  1)
obj_ts_ttd <- ts(ctrip_subs_mod_ts$total_trip_duration,frequency =  1)
obj_ts_ihwi <- ts(ctrip_subs_mod_ts$is_hol_wek_int,frequency =  1)
#dec_obj <- decompose(obj_ts_rev)  #cant decompose the object to look at separate trend, seasonal and error terms since freq is 1 and is aggregated daily.

#################ACF & PACF plots###############################
graphics.off()
#using xts object for plotting since ts has format problems

acf(cbtrips[,"revenue"])    # q maybe 1,4 or 7
pacf(na.omit(cbtrips[,"revenue"]))  # p maybe 1,4 or 7

acf(cbtrips[,"total_customers"])  # q maybe 1,4 or 7   
pacf(cbtrips[,"total_customers"])  # p maybe 1 or 7

acf(cbtrips[,"total_trip_duration"])  # q maybe 1,4 or 7    
pacf(cbtrips[,"total_trip_duration"])   # p maybe 1,4 or 7

acf(cbtrips[,"tmin"])  #You see an evident trend in tmin hence differencing below to remove trend
acf(na.omit(diff(cbtrips[,"tmin"])))   # No q
pacf(cbtrips[,"tmin"])     # # No p

#stationary test
adf.test(cbtrips[,"revenue"], alternative="stationary")

adf.test(cbtrips[,"tmin"], alternative="stationary") # as noted above the staionarity test fails
adf.test(cbtrips[,"tmin"], alternative="stationary",k=1)  #after differencing the series is now stationary and passes the test

################creating seasonal dummies to model with dummy regrerssors#############

#here weekly seasonal dumies are created and tested in regressors 
start_ts <- dmy_hms("01/04/2016 00:00:00")
index_ts <- start_ts + (c(0:90)*days(1))
week_ts <-  wday(index_ts)
xreg_w <- seasonaldummy(ts(ctrip_subs_mod_ts$revenue,f=5))


##################ARIMA Modelling###############################
# Here plain arma modelling without regressors but with seasonal dummies will be tried

arma_ts <- Arima(obj_ts_rev, order=c(0,0,1),seasonal = list(order=c(0,1,0),period=5))
summary(arma_ts)
plot(resid(arma_ts))
coeftest(arma_ts)
for_ar <- forecast(arma_ts,h = 8)
arima_ac <- accuracy(for_ar,test)       #Best model accuracy of simple Arima
arima_ac
#seasonal dummies at all periods ranging from 5-7 are insignificant hence not considering them in the model

##################### Regressors ##################################
# Here after trying all the regressors the best would be kept and explained

reg_rev <- Arima(obj_ts_rev, xreg=obj_ts_tmin,order = c(0,0,1),seasonal = list(order=c(0,1,0),period=5))
ar_tc <- Arima(obj_ts_tc,order = c(0,1,0))
reg1_for <- forecast(reg_rev,h=7,xreg=forecast(ar_tc,h=8)$mean)
summary(reg1_for)
plot.ts(reg1_for$x)
points(reg1_for$fitted)
coeftest(reg_rev)
Reg_ac <- accuracy(reg1_for,test)   #Best model accuracy of Arima with Tmin as regressor
Reg_ac
######################Sarima implementation#############################
graphics.off()
par("mar")
par(mar=c(1,1,1,1))
x_sar <- sarima(cbtrips[,"revenue"],p = 0,d = 0,q = 1,P = 0,D = 1,Q = 0,S = 5,xreg = cbtrips[,"tmin"])   #to estimate pdqPDQS values by understanding plots
x_sar$ttable
sarima_fore <- sarima.for(cbtrips[,"revenue"],n.ahead = 8,p = 0,d = 0,q = 1,P = 0,D = 1,Q = 0,S = 5)
residuals<- x_sar$fit$residuals
x_sar$ttable
plot(cbtrips[,"revenue"],major.ticks= "weeks")
plot(resid(x_sar$fit))

#train accuracy
sar_train_ac <- accuracy(train+residuals,train)
row.names(sar_train_ac)<-c("Train Set")
sar_train_ac

#test accuracy
sar_pred <- sarima_fore$pred
sar_test_ac <- accuracy(sar_pred,test)
sar_test_ac

#######################ARIMAX implementation############################################
#seasonal = list(order=c(0,1,0),freq=5)
regx_rev <- arima(obj_ts_rev,order = c(1,0,0),xreg = obj_ts_ihwi)
coeftest(regx_rev)
reg1x <- predict(regx_rev,n.ahead = 8,newxreg = c(0,0,1,1,0,0,0,0))   #passing list of holidays and weekends as a regressor

#train accuracy
Regx_train_ac <- accuracy(fitted.Arima(regx_rev),train)
row.names(Regx_train_ac)<-c("Train Set")
Regx_train_ac

#test accuracy
Regx_test_ac <- accuracy(reg1x$se,test)   #Best model accuracy of Arimax with holidays
Regx_test_ac

####################Model Selection#############
#Final seleted model is Arimax
#Arimax has the least RMSE of all the models and the best parameter significanse
Regx_train_ac
Regx_test_ac

#Parameter significance as below
coeftest(regx_rev)

#Now forecasting using this model for the 7 days
full_data <- ctrip_subs_mod[,c(3,6)]
obj_ts_ihwi_fd <- ts(full_data$is_hol_wek_int,frequency =  1)
obj_ts_rev_fd <- ts(full_data$revenue,frequency =  1)

regx_rev_fd <- arima(obj_ts_rev_fd,order = c(0,0,1),xreg = obj_ts_ihwi_fd)
coeftest(regx_rev_fd)
reg1x_fd <- predict(regx_rev_fd,n.ahead = 7,newxreg = c(0,1,1,1,0,0,0))   #passing list of holidays and weekends as a regressor

Regx_fd_ac <- accuracy(fitted.Arima(regx_rev_fd),full_data$revenue)
row.names(Regx_fd_ac)<-c("Test Data")
Regx_fd_ac

FINAL_PREDICTIONS <- as.vector(reg1x_fd$pred)
names(FINAL_PREDICTIONS) <- c("Friday","Saturday","Sunday","Monday","Tuesday","Wednesday","Thursday")
graphics.off()
plot(FINAL_PREDICTIONS,type = 'l')
FINAL_PREDICTIONS

#############################################
#To analyse outliers the data is too limited, there needs to be more data
#however on examining the values you see that saturday, sunday and monday
#are evident outliers
#saturday and sunday because weekend rush and Monday because its the 4th of july.

boxplot(FINAL_PREDICTIONS,boxwex = 0.3,range = 1)
