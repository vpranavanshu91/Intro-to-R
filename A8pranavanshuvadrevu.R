library(bbmle)
corn <- read.csv("C:/Users/vpran/OneDrive - University of Connecticut School of Business/MSBAPM/B13 - OPIM 5503 - Data Analytics using R/corn.csv")
health <- read.csv("C:/Users/vpran/OneDrive - University of Connecticut School of Business/MSBAPM/B13 - OPIM 5503 - Data Analytics using R/health.csv")
library(devtools)
install_github(username = "vpranavanshu91",repo = "gscom")
library(gscom)
attach(corn)
reg1 = lm(yield~nitrate)
summary(reg1)

linear = reg1$fitted.values
numetrics(yield,linear)


f1 = function(a,b,s)
{
    e = yield - a*(1- exp(1 - (b*nitrate)))
    LLsum = sum(dnorm(e,mean = 0,sd = s,log = T))
    return (-1*LLsum)
}

f1(0)
res = mle2(minuslogl = f1, start = list(a=0,b=0,s=0), method = "L-BFGS-B")
summary(res)

#####################################################################################

# Zero Inflated Poisson
x = data2$x2
x
plot(table(x))

#DGP
"""
v = (sick, not sick)
p = (pi, 1-pi)

# 0 can be due to two reasons, either not sick, or sick but choose not to go to the hospital
0 =  pi + (1-pi)*dpois(0,lambda)
1 = (1-pi) * dpois(1,lambda)
3 = (1-pi) * dpois(3,lambda)
4 = (1-pi) * dpois(4,lambda)
0
0
1
"""
f1 = function(lambda,p)
{
    L = ifelse(x==0,p+(1-p)*dpois(0,lambda),(1-p)*dpois(x,lambda))
    LLsum=sum(log(L))
    return(-1*LLsum)
}

f1(5,p = 0.5)
res = mle2(minuslogl = f1,start = list(lambda=1,p=0.5),method = "L-BFGS-B",lower = c(p=0,lambda=0),upper = c(p=1,lambda=Inf))
summary(res)
