library(bbmle)
library(car)

growth <- read.csv("C:/Users/vpran/Desktop/growth.csv")

attach(growth)

plot(growth$age,growth$height)

f1 = function(b0,b1,sig)
{
    err1 = height - b0 - b1*age
    err2 = height - b0 - b1*20
    L = ifelse(age<=15,dnorm(err1,mean = 0,sd=sig),dnorm(err2,mean = 0,sd=sig))
    LLsum = sum(log(L))
    return(-1*LLsum)
}

f1(0,0,sd(height))
res = mle2(minuslogl = f1, start = list(b0=mean(height), b1=0, sig=sd(height)))
summary(res)  
#"L-BFGS-B",lower = c(sig=0)

##################################################################################

wrkh <- read.csv("C:/Users/vpran/Desktop/Workinghours.csv")
attach(wrkh)

plot(income)

reg <- lm(income~.-child5-child13-child17,data=wrkh)
summary(reg)

boxCox(reg,family = "yjPower", plotit = T)

reg2 <- lm(log(income)~.-child5-child13-child17,data=wrkh)
summary(reg2)

z = cooks.distance(reg2)
plot(reg2,which = 4, cook.levels = cutoff)
abline(h = cutoff,col='red')
cutoff = 4 / length(income)
z[z>cutoff]
length(z[z>cutoff])
