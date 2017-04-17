library(MASS)
library(vcd)
library(car)
library(dplyr)

View(whiteside)
hist(whiteside$Gas)
plot(density(whiteside$Gas))
polygon(density(whiteside$Gas),col='red')
head(Salaries)
symbols(Salaries$yrs.service,Salaries$salary,circles = Salaries$yrs.since.phd/10,inches = 0.1,bg = 'green')

head(UScereal)
x=as.matrix(UScereal[,c(3,4)])
symbols(UScereal$sugars,UScereal$calories,rectangles = x, inches = 0.4 ,bg = 'blue')

x1 = rnorm(1000)
y1 = rnorm(1000)

plot(x1,y1)
smoothScatter(x1,y1)

x2 = runif(1000)
y2 = runif(1000)
smoothScatter(x2,y2)

head(Boston)
?Boston
plot(Boston$ptratio,Boston$tax)
sunflowerplot(Boston$ptratio,Boston$tax)

library(rgl)
plot3d(Boston$crim,Boston$nox,Boston$ptratio)

l1 = c("USA","Germany","Spain","UK")
v1 = c(12,16,5,23)
pie(v1,labels=l1,col=rainbow(4))

library(plotrix)
fan.plot(v1,labels = l1)

install.packages("vioplot")
library(vioplot)
vioplot(whiteside$Gas)
?vioplot

cor(Boston)
install.packages("dendextend",dependencies = TRUE)
library(corrgram)

corrgram(Boston,order = T)
corrgram(Boston,order = T,upper.panel = panel.pie)

#-----------------------------------------------------------------
NumericPred <- read.csv("C:/Users/vpran/OneDrive - University of Connecticut School of Business/MSBAPM/B13 - OPIM 5503 - Data Analytics using R/Session 3/data/NumericPred.csv")
colnames(NumericPred) <- c("target","model1","model2")
head(NumericPred)

a = NumericPred$target
m = NumericPred$model2
#metrics like MAD MSE MAPE MPSE and R2



numetrics2 = function(a,m)
{
    metrics <- c(MAD=0,MSE=0,MAPE=0,MPSE=0,R2=0,TMAD=0)
    metrics["MAD"] = mean(abs(a-m))
    metrics["MSE"] = mean((a-m)^2)
    metrics["MAPE"] = mean(abs((a-m)/a))
    metrics["MPSE"] = mean(((a-m)/a)^2)
    
    SST = sum((a-mean(a))^2)
    SSE =   sum((a-m)^2)
    metrics["R2"] = 1 - (SSE/SST)
    
    metrics["TMAD"] = mean(abs(a-m),trim = 0.05)
    return(metrics)
}

numetrics(NumericPred$target,NumericPred$model2)

#save the function
dump('numetrics2', file = "Mymodelfunctions.R",append = TRUE)
getwd()

#testing the sourcing fucntion
x1 = c(12,56,32,56,56,57)
y1 = c(15,78,45,65,42,48)

source(file.choose())
source("Mymodelfunctions.R")
args(numetrics)
numetrics(x1,y1)

cor(NumericPred$model1,NumericPred$model2)

ensemble = lm(NumericPred$target~NumericPred$model1+NumericPred$model2)
summary(ensemble)

NumericPred$model3 <- -1.11362+(0.29938*NumericPred$model1)+(0.75044*NumericPred$model2)
#or use predict function
head(NumericPred)

NumericPred$baseline <- mean(NumericPred$target)
numetrics(NumericPred$target,NumericPred$baseline)
plot(NumericPred$target,NumericPred$target,col = "blue",pch=19)
points(NumericPred$target,NumericPred$model3)

s1 = NumericPred[abs(NumericPred$target-NumericPred$model3)>5,]
points(s1$target,s1$model3,pch=19, col = "red")

a = NumericPred$target
m = NumericPred$model3

tcost = sum(ifelse(abs(a-m)<5,0,2*abs(a-m)))

tcost = sum(ifelse(abs(a-mean(a))<5,0,2*abs(a-mean(a)))) # for baseline COst


