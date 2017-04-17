admission <- read.csv("C:/Users/vpran/OneDrive - University of Connecticut School of Business/MSBAPM/B13 - OPIM 5503 - Data Analytics using R/Session 4/Data/admission.csv")
GMAT = admission$GMAT
GPA = admission$GPA

install.packages("mvtnorm")
library(mvtnorm)

M = c(10,1000)
S = matrix(c(2,1,1,2),nrow = 2,ncol = 2)
x = rmvnorm(1000,mean = M,sigma = S)
cor(x[,1],x[,2])


#hypothesis is corr is 0.6
tstat = cor(GPA,GMAT)

f1 = function()
{
  M = c(mean(GPA),mean(GMAT))
  S = matrix(c(var(GPA),0.6*sd(GPA)*sd(GMAT),0.6*sd(GPA)*sd(GMAT),var(GMAT)),nrow = 2,ncol = 2)
  x = rmvnorm(length(GMAT),mean = M, sigma = S)
  return(cor(x[,1],x[,2]))
}

#create the sampling distribution
sdist <- replicate(10000,f1())
sdist

#draw and evaluate the hypothesis
plot(density(sdist))
abline(v = tstat,col= "red")
gap = abs(mean(sdist)-tstat)
s1 = sdist[sdist<mean(sdist)-gap|sdist>mean(sdist)+gap]
pvalue = length(s1)/length(sdist)
pvalue

##############################################################
"""
sample = (5.6,7.1,2.8,4.9)
mean = 6 (Estimation)and SD = 2 (Known)

p1 = dnorm(5.6,6,2)
p2 = dnorm(7.1,6,2)
p3 = dnorm(2.8,6,2)
p4 = dnorm(4.9,6,2)

IID: Independent and identically distributed

L = p1*p2*p3*p4    (Likelihood)
Whatever maximises the likelihood is my maximum likelihood.

because increasingly smal values of L due to small p creates computational problems.
We take Log
LL = Sum log(Pi)   Log Likelihood
"""
###########################################################

data1 <- read.csv("C:/Users/vpran/OneDrive - University of Connecticut School of Business/MSBAPM/B13 - OPIM 5503 - Data Analytics using R/Session 5/data1.csv")
x = data1$x1
x

# DGP N(M,2)  

sum(dnorm(x,mean = M,sd = 2,log = T))

f1 = function(M)
{
  LLsum = sum(dnorm(x,mean = M,sd = 2,log = T))
  return(LLsum)
}

mseq = seq(0,10,by=0.05)

LLres  = sapply(mseq,f1)
plot(LLres)
max(LLres)

i = which.max(LLres)   #gives index number
mseq[i]    # gives the element at the 116 index with max LL

####################################################################################

install.packages("bbmle")
library(bbmle)

#DGP - N(M,2)

f1 = function(M)
{
 LLsum = sum(dnorm(x,mean = M,sd = 2,log = T))
 return(-1*LLsum)
 }

res = mle2(minuslogl = f1,start = list(M=10))

summary(res)


#####################################################################################
#DGP - N(M,s)

f1 = function(M,s)
{
  LLsum = sum(dnorm(x,mean = M,sd = s,log = T))
  return(-1*LLsum)
}

res = mle2(minuslogl = f1,start = list(M=10,s=3),method = "L-BFGS-B",lower = c(s=0))
summary(res)

####################################################################################

data2 <- read.csv("C:/Users/vpran/OneDrive - University of Connecticut School of Business/MSBAPM/B13 - OPIM 5503 - Data Analytics using R/Session 5/data2.csv")
x = data2$x1
x
#DGP - N(lambda)

f1 = function(lambda)
{
  LLsum = sum(dpois(x,lambda = lambda,log = T))
  return(-1*LLsum)
}

res = mle2(minuslogl = f1,start = list(lambda = 1),method = "L-BFGS-B",lower = c(s=0))
summary(res)

#################################################################################
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
