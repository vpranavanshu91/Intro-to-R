car1<-read.csv("C:/Users/vpran/OneDrive - University of Connecticut School of Business/MSBAPM/B13 - OPIM 5503 - Data Analytics using R/Session 2/Data/car insurance.csv")

colnames(car1)[5] <- "amount"
names(car1) <- c("veh_value","exposure","clm","numclaims","amount","veh_body","veh_age","gender","area","agecat")

attach(car1)
mean(veh_value)
detach(car1)
View(car1)

#Summarizing Factor Data

library(MASS)
library(vcd)
library(car)
library(dplyr)
getwd()
setwd("C:/Users/vpran/Documents/GitHub")

s1 = whiteside[whiteside$Temp<7,]
View(s1)
mean(s1$Gas)

s2 = whiteside[whiteside$Insul=="Before",]
View(s2)
mean(s2$Gas)

View(Arthritis)
table(Arthritis$Treatment)   #gives COunt summary
unique(Arthritis$Treatment)  #gives only values
x = table(Arthritis$Treatment,Arthritis$Improved)

prop.table(x)*100  #proportions of the cases overall

prop.table(x,margin = 1)*100    #get row proportions
prop.table(x,margin = 2)*100    #get column proportions

y = table(Arthritis$Sex,Arthritis$Treatment,Arthritis$Improved)
ftable(y)     #used to view more than 2 components in the contingency table

range(Arthritis$Age)
agegroups = cut(Arthritis$Age,breaks = c(-Inf,40,60,74))
agegroups = cut(Arthritis$Age,breaks = quantile(Arthritis$Age),include.lowest = TRUE)

as.data.frame(agegroups)
table(agegroups)
table(agegroups,Arthritis$Improved)

#Summarizing Numeric Data
View(Salaries)
tapply(Salaries$salary,Salaries$sex,mean)
tapply(Salaries$yrs.since.phd,Salaries$sex,mean)
y = tapply(Salaries$salary,list(Salaries$sex,Salaries$rank),mean)     # puts into a Matrix
y = aggregate(Salaries$salary,list(Salaries$sex,Salaries$rank),mean)   #Puts into a DF

rm(list=ls())

#dplyr Package

data(whiteside)
attach(whiteside)
detach(whiteside)
x = select(whiteside,c(Insul,Gas))

attach(Salaries)
detach(Salaries)
formula1 = (sex=="Male" & rank=="Prof")|salary>100000 
f1 = filter(Salaries,formula1) 
View(f1)

Salaries = arrange(Salaries,sex,rank) 
Salaries = arrange(Salaries,sex,desc(rank)) 
View(Salaries)

data(Salaries)
attach(Salaries)
detach(Salaries)
x = group_by(Salaries,discipline,sex,rank) 
summarize(x,median(salary)) 
summarize(x,mean(salary),mean(yrs.service),median(yrs.since.phd))

#Graphing and Visualization

data(whiteside)
attach(whiteside)
plot(Insul)
plot(Insul,Gas)
plot(Temp,Gas,main = "Graphing",pch = 19,cex=2,type = 'l')   #type can be h , l etc

data("Salaries")
attach(Salaries)
plot(Salaries)

## Plotting Exercise

help(par)
opar = par()
par(bg = "grey", mfrow = c(1,1), las =2, col = "white") #sets base cancas settings that are applied to all

plot(Temp,Gas,pch = 19,cex=1)
abline(v=mean(Temp),lwd = 3, col="turquoise1")
label1 = paste("Avg Temp = ", mean(Temp))
text(5,6,label1,srt = 90)

abline(h=mean(Gas),lwd = 3, col = "Orange")
label2 = paste("Avg Temp = ", round(mean(Gas),3))
text(4,4,label2,srt = 0)

s1 = whiteside[Insul == "Before",]
s1
points(s1$Temp,s1$Gas,pch = 19 , col = "Green")

s2 = whiteside[Temp<mean(Temp) & Gas < mean(Gas),]
s2
points(s2$Temp,s2$Gas,pch = 19 ,cex = 1.2, col = "Red")


#Other graph types

boxplot(Gas~Insul,horizontal = F,col = c("green","yellow"),notch = T)

stripchart(Gas~Insul,col = c("red","blue"),pch = 19, method = "stack")
stripchart(Gas~Insul,col = c("red","blue"),pch = 19, method = "jitter")

x = aggregate(Salaries$salary,list(Salaries$rank),mean) 
View(x) 
pie(x$x,labels=x$Group.1,col=rainbow(3))  
pie(x$x,labels=x$Group.1,col=c("red","blue","green"))

dotchart(x$x,labels = x$Group.1,col = rainbow(3))
