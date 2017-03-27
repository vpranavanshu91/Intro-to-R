#Pranavanshu Vadrevu

contribution <- read.csv("C:/Users/vpran/OneDrive - University of Connecticut School of Business/MSBAPM/B13 - OPIM 5503 - Data Analytics using R/Session 2/contribution.csv")

#----------------------------------------------------------------------------------------------

# a. Create a frequency table between Gender and Marital Status. 
# What percentage of divorcees (denoted as D) are male? 
# What percentage of the total are single (denoted as S) females? 
# What is the most common marital status of females?

a <- table(contribution$Gender,contribution$Marital.Status) 
a                                               #frequency table between Gender and Marital Status

a1 <- prop.table(x,margin = 2)*100 
paste(round(a1[2,1],2),"%")                      #percentage of male divorcees

a2 <- prop.table(a)*100  
paste(round(a2[1,3],2),"%")                      #total percentage of single females

a3 <- prop.table(a,margin = 1)*100
names(which.max(a3[1,]))                         #most common marital status of females

#----------------------------------------------------------------------------------------------

# b. Compute the median value of FY04Giving based on Gender and Marital Status.
# Which two groups had the highest median giving?

b <- tapply(contribution$FY04Giving,list(contribution$Marital.Status,contribution$Gender),median)
b                       #Male Widows and Female Widows have the highest median FY04 giving.

#----------------------------------------------------------------------------------------------

# c. Cut ‘Class Year’ into 2 groups with 3 break points –Inf, 1980, Inf. Save it as cyear.
# Calculate the average FY03Giving grouping by cyear and Gender. Which group gave the lowest?

cyear = cut(contribution$Class.Year,breaks = c(-Inf,1980,Inf))
as.data.frame(cyear)
c1 <- tapply(contribution$FY03Giving,list(cyear,contribution$Gender),mean)
c1
# Women in class year (1980,Inf) gave the lowest

#-----------------------------------------------------------------------------------------------

# d.       Install and explore package dplyr.
# 1.         Create a subset with individuals whose Next Degree is either MS or PHD.  Save it as S1.
# 2.         Sort the subset S1 on Gender and Next Degree. Save it as S2.

install.packages("dplyr")
library(dplyr)

# Created a subset with individuals whose Next Degree is either MS or PHD
S1 <- filter(contribution,contribution$Next.Degree == "MS" | contribution$Next.Degree == "PHD")

# Sorted the subset S1 on Gender and Next Degree.
S2 <- arrange(S1,Gender,Next.Degree)

#----------------------------------------------------------------------------------------------
# Using the dataset quakes in the datasets library to complete the following 2 tasks

# e.Split the plotting region into 1 row and 3 columns with grey background and blue color
# plot the following graphs: lat and long, depth and mag, stations and mag.

data(quakes)

par(bg = "grey", mfrow = c(1,3), las =2, col = "blue")

attach(quakes)
plot(lat,long)
plot(depth,mag)
plot(stations,mag)


#-----------------------------------------------------------------------------------------

# f. Draw a histogram for mag with density on the y-axis. 
# Add a vertical line to indicate the mean of mag in red with line width 3. Give the line an appropriate label.

hist(mag,freq = FALSE)
abline(v=mean(mag),lwd = 3, col="red")
label1 = paste("Mean Mag = ", round(mean(mag),2))
text(4.5,0.6,label1,srt=90)
detach(quakes)
