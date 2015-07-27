x<-1:10
mean(x)
sum(x)
mode
nchar(x)

mean(x,trim = 0.1) #gets rid of 10% of observations on the either side to calculate mean
mean(x,.1)

x[c(2,5,6)]<-NA   #replacing some values of array with NA
mean(x)         #mean does not return, because array now has NA
mean(x,na.rm = TRUE)    #use na.rm=TRUE to neglect NA values and return mean for the rest 
