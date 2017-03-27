#Pranavanshu Vadrevu

#Q1 Task 1 follows a random uniform distribution between 5 and 9
#the completion time for Task 2 follows a random exponential distribution with a rate of 0.1
#Task 3 follows a Poisson distribution with a lambda of 4
#Task 4 follows uniform distribution between 3 and 10. Compute the following.
#a. What are the mean and median times to complete all the tasks?
#b. What is the probability that all the tasks are completed in 15 hours?
#c. Create a plot of the density of the total completion time.

rm(list = ls())   #Clearing Environment

t1 <- runif(n = 10000,min = 5,max = 9) #Task 1 follows a random uniform distribution between 5 and 9
t2 <- rexp(n = 10000,rate = 0.1)#Task 2 follows a random exponential distribution with a rate of 0.1
t3 <- rpois(n = 10000,lambda = 4)#Task 3 follows a Poisson distribution with a lambda of 4.
t4 <- runif(n = 10000,min = 3,max = 10)#Task 4 follows uniform distribution between 3 and 10.


# a. What are the mean and median times to complete all the tasks?
# Task 1,2,3 --- NO PREDECESSOR
# Task 4 --- Task 1,2,3 AS PREDECESSOR

totaltime <- apply(cbind(t1,t2,t3),1,FUN = max)+t4   #the total time is calculated by taking the max time of the first 3 tasks together and the time for the 4th task added to it.
paste("Mean is: ",mean(totaltime))   #calculates the mean totaltime
paste("Median is:",median(totaltime))  #calculates the median totaltime

#b. What is the probability that all the tasks are completed in 15 hours?
paste("The probability is:",sum(totaltime<=15)/10000)     #calculate the no. of times when the tasks finish before 15 hours divided by the total no. samples

#c. Create a plot of the density of the total completion time.
plot(density(totaltime))  #plotting the densities of total time

###################################################################################
#Q2 There is a variable called Pregnancies, which indicates the number of pregnancies. 
#Assuming this follows a poisson distribution,test the hypothesis that the mean number of pregnancies is 3.7

rm(list = ls())   #Clearing Environment
diabetes <- read.csv("C:/Users/vpran/OneDrive - University of Connecticut School of Business/MSBAPM/B13 - OPIM 5503 - Data Analytics using R/Session 6/diabetes.csv")

#NULL HYPOTHESIS : THE MEAN OF PREGNANCIES IS 3.7
#ALTERNATE HYPOTHESIS: THE MEAN OF PREGNANCIES IS NOT 3.7
#test statistic 1

# there might be sampliong error thus im hypothesising that my test statistic will be a value and checking the p value of my test statistic being that number

attach(diabetes)  #attaching the diabetes dataset
tstat = mean(Pregnancies)   #our tstat here is mean of pregnancies

# draw a sample and compute the statistic

f1 = function()
{
  x <- rpois(n= length(Pregnancies), lambda = 3.7)   #to generate a random poison distribution with given lambda
  return(mean(x))  #returns mean of the above distribution
}

#create the sampling distribution
sdist <- replicate(10000,f1())    #replicates the above function 10000 times

#draw and evaluate the hypothesis
plot(density(sdist))  #plots the density of sdist
abline(v = tstat)     #line plots the tstat over the sdist distribution
gap = abs(mean(sdist)-tstat)   #calculates gap between the mean(sdist) and our tstat
s1 = sdist[sdist<mean(sdist)-gap|sdist>mean(sdist)+gap]  #Adds gap to the mean on both sides and calculates the points lying outside the boundaries
pvalue = length(s1)/length(sdist)  #calculates pvalue as a ratio of the no. of points obtained above and the no. of points sdist
paste("The p vaue is:",pvalue)  #prints pvalue
#We Reject the Null hypothesis
detach(diabetes)
####################################################################################################
#Q3 Read the file diabetes.csv. There is a variable called Insulin. 
#Conduct both a parametric and a non-parametric test for the median value of 80. 
#Are the results from both the tests similar? If not, explain why and which test you would trust more

#Parametric
rm(list = ls())   #Clearing Environment
diabetes <- read.csv("C:/Users/vpran/OneDrive - University of Connecticut School of Business/MSBAPM/B13 - OPIM 5503 - Data Analytics using R/Session 6/diabetes.csv")
attach(diabetes)
#NULL HYPOTHESIS : THE MEDIAN OF INSULIN IS 80
#ALTERNATE HYPOTHESIS: THE MEDIAN OF INSULIN IS NOT 80
tstat = median(Insulin)   # takes median of insulin to be the tstat

# draw a sample and compute the statistic

f1 = function()
{
  x <- rnorm(n= length(Insulin), mean = 80, sd = sd(Insulin))  #to generate a random normal distribution with hypothesised mean and sample SD
  return(median(x))
}

#create the sampling distribution
sdist <- replicate(10000,f1())    #replicates the above function 10000 times

#draw and evaluate the hypothesis
plot(density(sdist))   #plots the density of sdist
abline(v = tstat)    #line plots the tstat over the sdist distribution
gap = abs(mean(sdist)-tstat)     #calculates gap between the mean(sdist) and our tstat
s1 = sdist[sdist<mean(sdist)-gap|sdist>mean(sdist)+gap]     #Adds gap to the mean on both sides and calculates the points lying outside the boundaries
pvalue = length(s1)/length(sdist)          #calculates pvalue as a ratio of the no. of points obtained above and the no. of points sdist
paste("The p vaue is:",pvalue)                #prints pvalue
#We Reject the Null hypothesis
detach(diabetes)

#Non - Parametric

rm(list = ls())   #Clearing Environment
diabetes <- read.csv("C:/Users/vpran/OneDrive - University of Connecticut School of Business/MSBAPM/B13 - OPIM 5503 - Data Analytics using R/Session 6/diabetes.csv")
attach(diabetes)
#NULL HYPOTHESIS : THE MEDIAN OF INSULIN IS 80
#ALTERNATE HYPOTHESIS: THE MEDIAN OF INSULIN IS NOT 80
# non parametric test - median = 80
#test statistic
tstat = sum(ifelse(Insulin>80,1,0))     #change all insulin elements >80 to 1 and rest to 0

f1 = function()
{
  v = c(0,1)                   #creating a set of 0,1
  p = c(0.5,0.5)               #creating a set of equal probabilities
  x = sample(size = length(Insulin), x= v, prob = p, replace = T)        #drawing samples with replacement from above given set and associated probabilities
  return(sum(x))
}
#create the sampling distribution
sdist <- replicate(10000,f1())          #replicates the above function 10000 times

#draw and evaluate the hypothesis
plot(density(sdist))            #plots the density of sdist
abline(v = tstat,col= "red")     #line plots the tstat over the sdist distribution
gap = abs(mean(sdist)-tstat)      #calculates gap between the mean(sdist) and our tstat
s1 = sdist[sdist<mean(sdist)-gap|sdist>mean(sdist)+gap]      #Adds gap to the mean on both sides and calculates the points lying outside the boundaries
pvalue = length(s1)/length(sdist)       #calculates pvalue as a ratio of the no. of points obtained above and the no. of points sdist
paste("The p vaue is:",pvalue)       #prints pvalue

#We Reject the Null hypothesis
#The results from both tests are same and we are rejecting null in both cases, but I would prefer non-parametric 
#because it does not make assumptions about the population
detach(diabetes)
######################################################################################################
#Q4Read the file diabetes.csv. There are two variables called BMI and Outcome. 
#The variable Outcome takes on only two values: 0 and 1. 
#Conduct a non-parametric two sample test for the hypothesis that the standard deviation of BMI is the same for both Outcome values
# two sample non paramteric test

rm(list = ls())   #Clearing Environment
diabetes <- read.csv("C:/Users/vpran/OneDrive - University of Connecticut School of Business/MSBAPM/B13 - OPIM 5503 - Data Analytics using R/Session 6/diabetes.csv")
attach(diabetes)

#NULL HYPOTHESIS : THE STANDARD DEVIATION OF BMI (WITH OUTCOME=0) IS SAME AS THE STANDARD DEVIATION OF BMI (WITH OUTCOME=1) 
#ALTERNATE HYPOTHESIS: THE STANDARD DEVIATION OF BMI (WITH OUTCOME=0) IS NOT SAME AS THE STANDARD DEVIATION OF BMI (WITH OUTCOME=1)

n = length(BMI[Outcome==0])   #taking the length to be BMI elements with outcome 0
tstat = abs(sd(BMI[Outcome==0])-sd(BMI[Outcome==1]))    #tstat to be the absolute difference between both std deviations

f1 = function()
{
  x = c(BMI[Outcome==0],BMI[Outcome==1])      #creating a collective sample with both samples
  x = sample(x)                       #drawing samples without replacement
  m1 = sd(x[1:n])                      #calculating standard deviation for 1st n samples
  m2 = sd(x[(n+1):length(x)])          #calculating standard deviation for next n samples
  return(abs(m1-m2))                   #returning absolute difference of both the standard deviations
}

#create the sampling distribution
sdist <- replicate(10000,f1())           #replicates the above function 10000 times

#draw and evaluate the hypothesis
plot(density(sdist))                       #plots the density of sdist
abline(v = tstat,col= "blue")              #line plots the tstat over the sdist distribution
s1 = sdist[sdist>tstat]                    #calculates the points above the tstat for p value calculation
pvalue = length(s1)/length(sdist)          #calculates pvalue as a ratio of the no. of points obtained above and the no. of points sdist
paste("The p vaue is:",pvalue)                                     #prints pvalue
#We fail to reject the Null hypothesis
detach(diabetes)
#######################################################################################
#Q5 Read the file diabetes.csv. There are two variables called Glucose and BloodPressure. 
#Conduct a non-parametric test for the shapes of the two distributions are identical

#NULL HYPOTHESIS : The shape for glucose and bloodpressure is the same
#ALTERNATE HYPOTHESIS: The shape for glucose and bloodpressure is not the same

rm(list = ls())   #Clearing Environment
diabetes <- read.csv("C:/Users/vpran/OneDrive - University of Connecticut School of Business/MSBAPM/B13 - OPIM 5503 - Data Analytics using R/Session 6/diabetes.csv")
attach(diabetes)

plot(density(Glucose))        #plots distribution of glucose
plot(density(BloodPressure))  #plots distribution of bloodpressure
Glucose1 = (Glucose - mean(Glucose))/sd(Glucose)    #standardizing glucose
BloodPressure1 = (BloodPressure - mean(BloodPressure))/sd(BloodPressure)   #standardizing bloodpressure
plot(density(BloodPressure1))     #plot distribution of standardized bloodpressure
lines(density(Glucose1),col = "blue")    #plots the standardized glucose over the above plot

q = c(0.1,0.3,0.5,0.7,0.95)     #listing down quantiles for the test in a variable
q1= quantile(Glucose1,probs = q)      #calculating calues at the above mentioned quantiles for glucose
q2 = quantile(BloodPressure1,probs = q)   #calculating calues at the above mentioned quantiles for bloodpressure


n = length(Glucose)          #taking length variable as length of glucose
tstat = sum(abs(q1-q2))      #tstat to be our sum of absolute difference between quantiles of both distributions

f1 = function()
{
  
  xGlucose <- sample(size = n, x=Glucose1,replace = T)    #bootstrapping and drawing samples from glucose
  xBloodPressure <- sample(size = n, x=Glucose1,replace = T)     #bootstrapping and drawing samples from glucose
  q = c(0.1,0.3,0.5,0.7,0.95)        #listing down quantiles for the test in a variable
  q1 = quantile(xGlucose,probs = q)   #calculating calues at the above mentioned quantiles for glucose
  q2 = quantile(xBloodPressure,probs = q) #calculating calues at the above mentioned quantiles for bloodpressure
  return(sum(abs(q1-q2)))            #returns sum of absolute difference between quantiles of both distributions
}

#create the sampling distribution
sdist <- replicate(10000,f1())          #replicates the above function 10000 times

#draw and evaluate the hypothesis
plot(density(sdist))               #plots the density of sdist
abline(v = tstat,col= "red")        #line plots the tstat over the sdist distribution
s1 = sdist[sdist>tstat]             #calculates the points above the tstat for p value calculation
pvalue = length(s1)/length(sdist)   #calculates pvalue as a ratio of the no. of points obtained above and the no. of points sdist
paste("The p vaue is:",pvalue)                                     #prints pvalue
#We reject the Null hypothesis
detach(diabetes)
rm(list = ls())
