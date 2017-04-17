library(bbmle)   #Loading bbmle package
library(car)     #loading car package

growth <- read.csv("C:/Users/vpran/Desktop/growth.csv")    #reading growth file

attach(growth)  #attaching growth

plot(growth$age,growth$height)   #plotting age and height

f1 = function(b0,b1,sig)    # writing a function that takes b0,b1,sig as input
{
    err1 = height - b0 - b1*age             #defining err for when age is less than equal to 15
    err2 = height - b0 - b1*20             #defining err for when age is above 15
    L = ifelse(age<=20,dnorm(err1,mean = 0,sd=sig),dnorm(err2,mean = 0,sd=sig))     
    LLsum = sum(log(L))    #sum of log of probabilities
    return(-1*LLsum)    #returns -llsum to minimize
}

f1(0,0,sd(height))
res = mle2(minuslogl = f1, start = list(b0=mean(height), b1=0, sig=sd(height)))  #giving baseline model starting values
summary(res)  


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
cutoff = 4 / length(income)

plot(reg2,which = 4, cook.levels = cutoff)
abline(h = cutoff,col='red')
z[z>cutoff]
length(z[z>cutoff])
