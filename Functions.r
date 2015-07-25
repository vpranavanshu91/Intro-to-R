x<-1:10
mean(x)
sum(x)
mode
nchar(x)

mean(x,trim = 0.1) #gets rid of 10% of observations on the either side to calculate mean
mean(x,.1)

x[c(2,5,6)]<-NA
mean(x)
mean(x,na.rm = TRUE)
