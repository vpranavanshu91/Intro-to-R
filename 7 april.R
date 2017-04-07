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

