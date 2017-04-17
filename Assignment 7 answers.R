# Assignment 7 answers

#1
t1 = runif(10000,min = 5,max = 9)
t2 = rexp(10000,rate = 0.1)
t3 = rpois(10000,lambda = 4)
t4 = runif(10000,min = 3,max = 10)
t= ifelse(t1>t2,ifelse(t1>t3,t1,t3),ifelse(t2>t3,t2,t3))+t4
mean(t)
median(t)
length(t[t<15])/length(t)
plot(density(t))

#2
diabetes <- read.csv("C:/Users/chint/Downloads/R Assignments/diabetes.csv")
preg = diabetes$Pregnancies
plot(table(preg),type="h")
tstat = mean(preg)
f1 = function()
  {
    x = rpois(n = length(preg),lambda = 3.7)
    return((mean(x)))
  }
sdist = replicate(10000,f1())
plot(density(sdist))
gap = abs(mean(sdist)-tstat)
abline(v=mean(sdist)-gap)
abline(v=mean(sdist)+gap)
length(sdist[sdist<mean(sdist)-gap|sdist>mean(sdist)+gap])/length(sdist)

#3
Insulin = diabetes$Insulin
tstat = median(Insulin)
tstat
f1 = function()
{
  x = rnorm(length(Insulin),mean = 80,sd = sd(Insulin))
  return(median(x))
}
sdist = replicate(10000,f1())
plot(density(sdist))
gap = abs(mean(sdist)-tstat)
abline(v=mean(sdist)-gap)
abline(v=mean(sdist)+gap)
length(sdist[sdist<mean(sdist)-gap|sdist>mean(sdist)+gap])/length(sdist)

tstat = sum(ifelse(Insulin>80,1,0))
f1 = function()
{
  v= c(0,1)
  p = c(0.5,0.5)
  x = sample(size = length(Insulin),x = v,prob = p,replace = T)
  return((sum(x)))
}
sdist = replicate(10000,f1())
plot(density(sdist))
gap = abs(mean(sdist)-tstat)
abline(v=mean(sdist)-gap)
abline(v=mean(sdist)+gap)
length(sdist[sdist<mean(sdist)-gap|sdist>mean(sdist)+gap])/length(sdist)

# non-parametric is preferable as normality is not appropriate for Insulin. Plot and see


#4
out0= (diabetes[diabetes$Outcome==0,6])
out1= (diabetes[diabetes$Outcome==1,6])

tstat = sd(out0)-sd(out1)
f1 = function()
{
  out= c(out0,out1)
  x = sample(out)
  o1 = sd(x[1:length(out0)])
  o2 = sd(x[length(out0):length(x)])
  return(o1-o2)
}

sdist = replicate(10000,f1())
plot(density(sdist))
gap = abs(mean(sdist)-tstat)
abline(v=mean(sdist)-gap)
abline(v=mean(sdist)+gap)
length(sdist[sdist<mean(sdist)-gap|sdist>mean(sdist)+gap])/length(sdist)

#5
g = (diabetes$Glucose-mean(diabetes$Glucose))/sd(diabetes$Glucose)
b = (diabetes$BloodPressure-mean(diabetes$BloodPressure))/sd(diabetes$BloodPressure)

plot(density(g))
plot(density(b))
q = c(0.1,0.3,0.5,0.7,0.9)
tstat = sum(abs(quantile(g,probs = q)-quantile(b,probs = q)))

f1 = function()
{
  x = sample(g,replace = T)
  return(sum(abs(quantile(g,probs = q)-quantile(x,probs = q))))
}

sdist = replicate(10000,f1())
plot(density(sdist))
abline(v=tstat)
# p value is very small. Reject that the shapes are the same



#######################################################################


#clear the objects present in the environment
rm(list=ls())
#set working directory
setwd("F://Fall 2016/R - Ramgopal/Assignment 9")


################################################
#Question 1
#################################################
#read the Corn.csv and assign it to the object Corn
Corn <- read.csv("Corn.csv")
#View the Corn 
View(Corn)

#apply linear reression
reg1 = lm(yield ~ nitrate,data=Corn)
reg1
#Add the predicted values of the linear regression to the column "linear" in Corn
Corn$linear <- 6148.50+51.52*Corn$nitrate
#Plot the predicted and actual to see how the values are spreaded
plot(Corn$yield,Corn$linear)

#von Bertalanffy growth model log likelihood values
LLreg = function(A, beta, sig) {
  #error value between actual and predicted
  x = Corn$yield - A * (1 - exp(1 - beta * Corn$nitrate))
  #sum of loglikelihood values using error
  ll = sum(dnorm(x, mean = 0, sd = sig, log = T))
  #return negative of loglikelihood values to maximize 
  return(-ll)
}
#add bbmle library for mle2 function
library(bbmle)

#plot the yield values and nitrate values to set start values
plot(Corn$yield,Corn$nitrate)

#mle2 function is used to minimize the log likelihood function
reg2 =mle2(minuslogl = LLreg,start = list(A=mean(Corn$yield),beta=1,sig=1000))
#providing start values so that the mle2 function returns the base model
#start value for the A is mean of the yield for the base model
#And the part exp(1-beta*nitrate) should be 0 which means the beta*nitrate should be large negative

summary(reg2)
#########################################################
#results
#Coefficients:
#       Estimate Std. Error z value     Pr(z)    
#  A    7.8735e+03 3.1838e+01 247.297 < 2.2e-16 ***
#  beta 2.4987e-01 1.9837e-02  12.596 < 2.2e-16 ***
#  sig  1.9551e+03 1.3892e+02  14.073 < 2.2e-16 ***
######################################################
#see the structure ofthe reg2 object to access the coefficient values  
str(reg2)
A=reg2@fullcoef[1]
beta=reg2@fullcoef[2]  
#new column grownth
Corn$growth = A * (1 - exp(1 - beta * Corn$nitrate))
#save data frame as Corn-MithraChintha.csv
write.csv(file =  "Corn-Mithrachintha.csv",x=Corn)

#Model metrics functions created in class to compare two models
nummetrics = function(a,m){
  metrics <- c(MAD=0,MSE=0,MAPE=0,MPSE=0,R2=0,TMAD=0,p90=0)
  metrics["MAD"] <- mean(abs(a-m))
  metrics["MSE"] <- mean((a-m)^2)
  metrics["MAPE"] <- mean(abs((a-m)/a))
  metrics["MPSE"] <- mean(((a-m)/a)^2)
  #How much variation in a(Target) is explained by the m(model)
  #1- (sse/sst)
  #sse - a-mean(a) sqaure and sum
  #SST - a- m square and sum
  SST= sum((a-mean(a))^2)
  SSE= sum((a-m)^2)
  metrics["R2"]= 1- (SSE/SST)
  metrics["TMAD"]= mean(abs(a-m),tri=0.05)
  metrics["p90"]= quantile(abs(a-m),probs = 0.9)
  return(metrics)
}
#comparision metrics for actual and linear model
nummetrics(Corn$yield,Corn$linear)
#Metrics Output for linear model
#MAD          MSE         MAPE         MPSE           R2         TMAD          p90 
#1.516556e+03 3.766379e+06 3.415468e-01 6.135397e-01 1.930015e-01 1.413326e+03 3.318559e+03 

#comparision metrics for actual and growth model
nummetrics(Corn$yield,Corn$growth)
#Metrics Output fdr the growth model 
#MAD          MSE         MAPE         MPSE           R2         TMAD          p90 
#1.511175e+03 3.588147e+06 3.085782e-01 3.713885e-01 2.311902e-01 1.438807e+03 3.368975e+03 

#All the metrics other than MPSE are similar to both linear model and von Bertalanffy growth model
#Mean Squared prediction is hign for linear model when compared to the growth model
#Considering lesser MPSE growth model is better


################################################
#Question 2
#################################################
#read the Health.csv and assign it to "Health" variable
Health <- read.csv("Health.csv")
View(Health)

#to check the data frame values 
head(Health)
summary(Health$ofp)

#loglikelihood function
LLPois=function(alpha0,alpha1,alpha2,beta0,beta1,beta2,beta3){
  
  x=alpha0+alpha1*Health$numchron+alpha2*Health$male
  #calculate probability of being healthy by given equation
  p=exp(x)/(1+exp(x))
  y=beta0+beta1*Health$numchron+beta2*Health$employed+beta3*Health$married
  #caculate count of physician visits using poison distribution
  lambda=exp(y)
  #Calculate loglikelihood values
  L=ifelse(Health$ofp==0,p+((1-p)*dpois(0,lambda)),(1-p)*dpois(Health$ofp,lambda))
  #loglikelihood function sum
  LLsum=sum(log(L))
  #return negative of loglikelihood function as we need to maximize the value
  return(-LLsum)
  
}

# minimizing the negative likelihood using mle2 function
reg1= mle2(minuslogl = LLPois,start = list(alpha0=0,alpha1=0,alpha2=0,beta0=1.6,beta1=0,beta2=0,beta3=0))
# Intializing the fucntion with the values such that it represents base model
# SO p(healthy) = 0.5  => x= 0 so alpha0=0,alpha1=0,alpha2=0
# now for the frequency of visits be equal to mean(health$ofnp[health$ofnp >0]) = 5
# so lambda =5 => y = log5 = 1.6  so beta0=1.6 and beta1=0,beta2=0,beta3=0

#gives the coefficient values
reg1@fullcoef

summary(reg1)
#########################################################
#results
#Coefficients:
#          Estimate Std. Error  z value     Pr(z)    
#  alpha0 -1.1971291  0.0730850 -16.3800 < 2.2e-16 ***
#  alpha1 -0.5429473  0.0427495 -12.7007 < 2.2e-16 ***
#  alpha2  0.3734854  0.0866193   4.3118 1.619e-05 ***
#  beta0   1.6770592  0.0129344 129.6593 < 2.2e-16 ***
#  beta1   0.1494984  0.0042856  34.8840 < 2.2e-16 ***
#  beta2   0.0490150  0.0213313   2.2978   0.02157 *  
#  beta3  -0.0579694  0.0126693  -4.5756 4.749e-06 ***
#########################################################

#frequency of visits depends on all the coefficients
#All are significant values as per the probability



