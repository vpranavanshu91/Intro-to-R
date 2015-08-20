# Using the Aggregate function

require(ggplot2)
data(diamonds)
head(diamonds)
mean(diamonds$price)

aggregate(price~cut,diamonds,mean) #Aggregates Price and segment factor is cut
aggregate(price~cut,diamonds,mean, na.rm=TRUE)

aggregate(price~cut+color,data = diamonds,FUN = mean)  #Aggregates Price with unique combo of cut and color
aggregate(cbind(price,carat)~cut,data = diamonds,FUN = mean) #Aggeegates Price and Carat with unique combo of cut and color
aggregate(cbind(price,carat)~cut+color,data = diamonds,FUN = mean)
