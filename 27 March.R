library(bbmle)
corn <- read.csv("C:/Users/vpran/OneDrive - University of Connecticut School of Business/MSBAPM/B13 - OPIM 5503 - Data Analytics using R/Session 6/Corn.csv")
attach(corn)
plot(nitrate,yield)

#regression estimation

f1 = function(a0,a1,sig)
{
    err = yield - a0 - a1*nitrate
    LLsum = sum(dnorm(err,mean = 0,sd = sig,log = T))
    return (-1*LLsum)
}

#baseline assumptions
a0s = mean(yield)
a1s = 0
sigs = sd(yield)

f1(a0s,a1s,sigs)

res = mle2(minuslogl = f1,start = list(a0=a0s,a1=a1s,sig=sigs),method = "L-BFGS-B",lower = c(sig=0))
summary(res)

lm(yield~nitrate)
detach(corn)
########################################################################################exp(1)
#logistic regression estimation
admit <- read.csv("C:/Users/vpran/OneDrive - University of Connecticut School of Business/MSBAPM/B13 - OPIM 5503 - Data Analytics using R/Session 5/Data/admit.csv")
attach(admit)
# pi = e^x / (1+e^x)   pi is probability of success (1-pi) is probability of failure
# pi(1+e^x) = e^x
# pi = e^x(1-pi)
# e^x = pi / (1-pi)
# x = log(pi / (1-pi))
# x is  a0 + a1*gre + a2*gpa +a3*rank

f1 = function(a0,a1,a2,a3)
{
    x = a0+a1*gre+a2*gpa+a3*rank
    p=exp(x)/(1+exp(x))
    L = ifelse(admit==0,1-p,p)
    LLsum = sum(log(L))
    return(-1*LLsum)
}

res = mle2(minuslogl = f1, start = list(a0=0,a1=0,a2=0,a3=0))
summary(res)
##################################################################################3
#probit regression
#pi = pnorm(x,0,1)
# x is  a0 + a1*gre + a2*gpa +a3*rank

f1 = function(a0,a1,a2,a3)
{
    x = a0+a1*gre+a2*gpa+a3*rank
    p = pnorm(x,mean = 0,sd = 1)
    L = ifelse(admit==0,1-p,p)
    LLsum = sum(log(L))
    return(-1*LLsum)
}

res = mle2(minuslogl = f1, start = list(a0=0,a1=0,a2=0,a3=0))
summary(res)