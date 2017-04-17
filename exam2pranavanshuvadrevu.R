wrkh <- read.csv("C:/Users/vpran/Desktop/Workinghours.csv")
growth <- read.csv("C:/Users/vpran/Desktop/growth.csv")
library(bbmle)
attach(growth)

f1 = function(b0,b1,sig)
{
    err1 = height - b0 - b1*age
    err2 = height - b0 - b1*20
    L = ifelse(age<=15,dnorm(err,mean = 0,sd=sig),dnorm(err2,mean = 0,sd=sig))
    LLsum = sum(log(L))
    return(-1*LLsum)
}

f1(0,0,sd(height))
res = mle2(minuslogl = f1, start = list(b0=mean(height), b1=0, sig=sd(height)))
summary(res)  
#"L-BFGS-B",lower = c(sig=0)