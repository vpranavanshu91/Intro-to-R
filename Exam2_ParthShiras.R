# Exam 2
# Parth Shiras

library(bbmle)

# Question 1
growth <- read.csv("~/Downloads/growth.csv")

plot(growth$age,growth$height)

f1 = function(B0,B1,sig){
    LLsum = sum(ifelse(growth$age <= 20, dnorm((growth$height - B0 -B1*growth$age),mean = 0,sd = sig,log = T),
                   dnorm((growth$height - B0 - B1*20),mean = 0,sd = sig,log = T)))
    return(-1*LLsum)
}

f1(1,1,1)

res = mle2(minuslogl = f1, start = list(B0=1,B1=1,sig=1))

summary(res)

#Answer -
#Coefficients:
#    Estimate Std. Error z value     Pr(z)    
#B0  49.713210   1.098685  45.248 < 2.2e-16 ***
#B1   6.163427   0.065726  93.775 < 2.2e-16 ***
#sig 12.366699   0.285216  43.359 < 2.2e-16 ***

# -------------------------------------

# Question 2

