library(bbmle)
student <- read.csv("C:/Users/vpran/OneDrive - University of Connecticut School of Business/MSBAPM/B13 - OPIM 5503 - Data Analytics using R/Session 5/Data/student.csv")
head(student)
attach(student)

#if using 0's as default values dont work then we see what the average of count data is and try to set lambda to mean of count data 
#and reverse estimate x and other starting values

#poisson regression
#pi = dpois(x,lambda)
# x is  a0 + a1*gender + a2*math +a3*prog

f1 = function(a0,a1,a2,a3)
{
    x = a0+a1*gender+a2*math+a3*prog
    l = exp(x)
    LLsum = sum(dpois(daysabs,lambda = l, log = T)) 
    return(-1*LLsum)
}
f1(0,0,0,0)
res = mle2(minuslogl = f1, start = list(a0=0,a1=0,a2=0,a3=0))
summary(res)
detach(student)
#if holding everything else constatnt, 
#if gender goes up by 0.23 then lambda or mean of days absent increases by exp(0.235) = 1.264 days
#a0 a2 a2 have coonfidence intervals that come from resampling cia bootstrapping and estimation from the population
#try training  and obtaining a0 a1 a2 estimates and obtain the 95% CI by doing above

#this is overfitting, but the impact can be estimated
#then when applying this over the test dataset use these estimates to optimize with 95% CI limits as limits

#if 0 does not fall in the 95%CI for the a0 a1 a2 estimate then the variable is significant and has an impact

#is bootstrapping a baysian way of loking at stats since it makes no assumtion son the population ?

#heteroskedascticity
head(cars)
attach(cars)
plot(speed,dist)

f1 = function(a1,s)
{
    err = dist - a1*speed
    LLsum = sum(dnorm(err, mean=0,sd =s, log = T))
    return(-1*LLsum)
}

f1(0,sd(dist))
res = mle2(minuslogl = f1,start = list(a1=0,s=sd(dist)))
summary(res)  

#assumption of homoskedasticity fails, so for speeds under 15 homo assumtion holds but above 15 cant predict properly
#therefore we go for separate data generation process
#we will get to know the above by looking at the residual plot

f1 = function(a1,s1,s2)
{
    err = dist - a1*speed
    L = ifelse(speed<15,dnorm(err,0,sd=s1),dnorm(err,mean = 0,sd=s2))
    LLsum = sum(log(L))
    return(-1*LLsum)
}

f1(0,sd(dist),sd(dist))
res = mle2(minuslogl = f1,start = list(a1=0,s1=sd(dist),s2=sd(dist)))
summary(res)  
detach(cars)
##############################################################################
#forecasting
series <- read.csv("C:/Users/vpran/OneDrive - University of Connecticut School of Business/MSBAPM/B13 - OPIM 5503 - Data Analytics using R/Session 6/Data/timeseries.csv")
attach(timeseries)
plot(x,type = "l")

f1 = function(u,s)
{
    err = x - u
    LLsum = sum(dnorm(err,mean = 0, sd = s, log=T ))
    return(-1*LLsum)
}

f1(mean(x),sd(x))
res = mle2(minuslogl = f1,start = list(u = mean(x),s = sd(x)))
summary(res)  

################################################################################3
#time series with autocorelation

#possibly an auto regression model (AR1) , the lag is selected as 1 and hardcoded while subsetting

f1 = function(u,rho,s)
{
    err = x[2:10000] - u -rho*(x[1:9999]-u)
    LLsum = sum(dnorm(err,mean = 0, sd = s, log=T ))
    return(-1*LLsum)
}

f1(mean(x),0,sd(x))
res = mle2(minuslogl = f1,start = list(u = mean(x),rho = 0,s = sd(x)))
summary(res)  
detach(timeseries)
############################################################################
#two stage model

clickbuy <- read.csv("C:/Users/vpran/OneDrive - University of Connecticut School of Business/MSBAPM/B13 - OPIM 5503 - Data Analytics using R/Session 6/Data/clickbuy.csv")
attach(clickbuy)
f1 <- function(a0,a1,a2,b0,b1,b2)
{
    xc = a0+a1*gender+a2*age
    xb = b0+b1*gender+b2*income
    p1 = exp(xc)/ (1+ exp(xc))
    p2 = exp(xb)/ (1+ exp(xb))
    L = ifelse(click==0,1-p1,ifelse(buy==0,p1*(1-p2),p1*p2))
    LLsum = sum(log(L))
    return(-1*LLsum)
}

f1(0,0,0,0,0,0)
res = mle2(minuslogl = f1,start = list(a0=0,a1=0,a2=0,b0=0,b1=0,b2=0))
summary(res)
#########################################################################
#single staged for above

f1 <- function(b0,b1,b2)
{
    xb = b0+b1*gender+b2*income
    p2 = exp(xb)/ (1+ exp(xb))
    L = ifelse(buy==0,1-p2,p2)
    LLsum = sum(log(L))
    return(-1*LLsum)
}

f1(0,0,0)
res = mle2(minuslogl = f1,start = list(b0=0,b1=0,b2=0))
summary(res)
detach(clickbuy)
#########################################################################
#censored data & survival models

aml <- read.csv("C:/Users/vpran/OneDrive - University of Connecticut School of Business/MSBAPM/B13 - OPIM 5503 - Data Analytics using R/Session 6/Data/aml.csv")
x <- rexp(1000,rate = 1/15)   #random exp distribution generator
mean(x)     #rate of 1/15 shows that the mean is similar to rate, basically rate is 1/average 
plot(density(x))
attach(aml)

f2 = function(l)
{
    LLsum = sum(dexp(time,rate = 1/l,log = T))
    return(-1*LLsum)
}

res = mle2(minuslogl = f2,start = list(l=10))
summary(res)
