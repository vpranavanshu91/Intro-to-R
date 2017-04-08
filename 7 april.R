library(bbmle)
aml <- read.csv("C:/Users/vpran/OneDrive - University of Connecticut School of Business/MSBAPM/B13 - OPIM 5503 - Data Analytics using R/Session 6/Data/aml.csv")

x = rexp(1000,rate = 1/5)
mean(x)
plot(density(x))

f1 = function(l)
{
    LLsum = sum(dexp(aml$time,rate = 1/l,log = T))
    return (-1*LLsum)
}

f1(0)
res = mle2(minuslogl = f1, start = list(l=5))
summary(res)

#Censored

f1 = function(l)
{
    L = ifelse(aml$status==0,1-pexp(aml$time,rate = 1/l),dexp(aml$time,rate = 1/l))
    LLsum = sum(log(L))
    return (-1*LLsum)
}

f1(0)
res = mle2(minuslogl = f1, start = list(l=5))
summary(res)

###Survival Model
as.numeric(aml$x)
mt = ifelse(aml$x=="Maintained",1,0)

f1 = function(a0,a1)
{
    X = a0+a1*mt
    l = exp(X)
    L = ifelse(aml$status==0,1-pexp(aml$time,rate = 1/l),dexp(aml$time,rate = 1/l))
    LLsum = sum(log(L))
    return (-1*LLsum)
}

f1(0,0)
res = mle2(minuslogl = f1, start = list(a0=5,a1=0))

x= 10
A = dexp(x,rate = 1/15)
B = 1- pexp(x,rate = 1/15)
A/B          # hazard rate

#######################################
#seemingly unrelated regression
hsb2 <- read.csv("C:/Users/vpran/OneDrive - University of Connecticut School of Business/MSBAPM/B13 - OPIM 5503 - Data Analytics using R/Session 6/Data/hsb2.csv")

reg1 = lm(hsb2$read~as.numeric(hsb2$female)+as.numeric(hsb2$ses)+hsb2$socst)
reg2 = lm(hsb2$math~as.numeric(hsb2$female)+as.numeric(hsb2$ses)+hsb2$science)
summary(reg1)
summary(reg2)
##check pdf for code

##########################Linear Regression#################################
library(MASS)
attach(Boston)
attach(women)

View(Boston)
plot(height,weight,type = 'l')
