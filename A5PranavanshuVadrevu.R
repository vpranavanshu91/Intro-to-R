#Pranavanshu Vadrevu

library(MASS)       #loading MASS package
library(dplyr)      #loading dplyr package
data("survey")      #loading survey data
head(survey)        # viewing the top part of the file

one <- filter(survey,survey$Age>21 & survey$Sex == "Female")    #subseting the data required to plot the first graph in variable one
two <- filter(survey,survey$Exer == "Freq" & survey$Sex == "Male")  #subseting the data required to plot the second graph in variable two

plot(survey$Age,survey$Pulse,pch =19, main = "Assignment 5")    #plotting Age and Pulse with pch(point type) and title as parameters
points(one$Age,one$Pulse,col="red",pch = 19,cex = 1.5)          #plotting points with the above subseted one data with red color given the pch (point type) parameters,col red and cex (for size)
points(two$Age,two$Pulse,col="blue",pch = 19,cex = 1.5)         #plotting points with the above subseted two data with red color given the pch (point type) parameters,col blue and cex (for size)