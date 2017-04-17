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

wrkh <- read.csv("C:/Users/vpran/Desktop/Workinghours.csv")  #reading work hours file
attach(wrkh)  #attaching work hours file

plot(income)

reg <- lm(income~.-child5-child13-child17,data=wrkh)   #running first regression without child 5, 13 and 17
summary(reg)  #viewing summary

boxCox(reg,family = "yjPower", plotit = T)  #using boxcox to ecaluate the right transformation

reg2 <- lm(log(income)~.-child5-child13-child17,data=wrkh)   #since the boxcox lambda is between 0 and 0.5 , its close to 0 hence considering log
summary(reg2)

z = cooks.distance(reg2)   #calculating z using cooks distance
cutoff = 4 / length(income)    #calculating cutoff value

plot(reg2,which = 4, cook.levels = cutoff)   #plotting the cooks distance and the cutoff line
abline(h = cutoff,col='red')

z[z>cutoff]    #all influential observations above the cutoffline
length(z[z>cutoff])   #the no, of influential observations


#check lambda value peak and decide what transformation to apply
#
#Table 1: Common Box-Cox Transformations
#  l 	    Y'
# -2  	  Y-2 = 1/Y2
# -1	    Y-1 = 1/Y1
# -0.5  	Y-0.5 = 1/(Sqrt(Y))
#  0	    log(Y)
#  0.5	  Y0.5 = Sqrt(Y)
#  1	    Y1 = Y
#  2	    Y2
