admission <- read.csv("C:/Users/vpran/OneDrive - University of Connecticut School of Business/MSBAPM/B13 - OPIM 5503 - Data Analytics using R/Session 4/Data/admission.csv")
GMAT = admission$GMAT
GPA = admission$GPA

#75th percentile is 600

qnorm(0.75,mean = 532.55125,sd = 100)

tstat = quantile(GMAT, probs = 0.75)

f1 = function()
{
    x <- rnorm(n= length(GMAT), mean = 532.55215, sd = 100)
    return(quantile(x, probs = 0.75))
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
############################################################################


#test statistic
tstat = cor(GPA,GMAT)
tstat
tion
f1 = func()
{
    xGPA = rnorm(n= length(GMAT), mean = mean(GPA), sd = sd(GPA))
    xGMAT = rnorm(n= length(GMAT), mean = mean(GMAT), sd = sd(GMAT))
    return(cor(xGMAT,xGPA))
}
f1()

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

##########################################################

#shape test
plot(density(GMAT))
plot(density(GPA))
GPA1 = (GPA - mean(GPA))/sd(GPA)
GMAT1 = (GMAT - mean(GMAT))/sd(GMAT)
plot(density(GPA1))
lines(density(GMAT1),col = "blue")

q = c(0.1,0.3,0.5,0.7,0.95)
q1= quantile(GPA1,probs = q)
q2 = quantile(GMAT1,probs = q)
tstat=sum(abs(q1-q2))

n = length(GMAT)

f1 = function()
{
    
    xGPA <- rnorm(n=n, mean = 0, sd = 1)
    xGMAT <- rnorm(n=n, mean = 0, sd = 1)
    q = c(0.1,0.3,0.5,0.7,0.95)
    q1 = quantile(xGPA,probs = q) 
    q2 = quantile(xGMAT,probs = q)
    return(sum(abs(q1-q2)))
}

#create the sampling distribution
sdist <- replicate(10000,f1())

sdist

#draw and evaluate the hypothesis
plot(density(sdist))
abline(v = tstat,col= "red")
gap = abs(mean(sdist)-tstat)
length(sdist[sdist>tstat])/length(sdist)

#########################################################
x <- rnorm(n= 100, mean = 500, sd = 100)
y <- rnorm(n= 100, mean = 500, sd = 100)

x = sort(x)
y = sort(y)
cor(x,y)
#########################################################
#create the sampling distribution
sdist <- replicate(10000,f1())
sdist

#draw and evaluate the hypothesis
plot(density(sdist))
abline(v = tstat,col= "red")
gap = abs(mean(sdist)-tstat)
s1 = sdist[sdist<mean(sdist)-gap|sdist>mean(sdist)+gap]
pvalue = length(s1)/length(sdist)

pvalue#create the sampling distribution
sdist <- replicate(10000,f1())
sdist

#draw and evaluate the hypothesis
plot(density(sdist))
abline(v = tstat,col= "red")
gap = abs(mean(sdist)-tstat)
s1 = sdist[sdist<mean(sdist)-gap|sdist>mean(sdist)+gap]
pvalue = length(s1)/length(sdist)

pvalue#create the sampling distribution
sdist <- replicate(10000,f1())
sdist

#draw and evaluate the hypothesis
plot(density(sdist))
abline(v = tstat,col= "red")
gap = abs(mean(sdist)-tstat)
s1 = sdist[sdist<mean(sdist)-gap|sdist>mean(sdist)+gap]
pvalue = length(s1)/length(sdist)

pvalue
#####################################################################
#factor data - admission rate - 40%

tstat = prop.table(table(admission$De))[1]

#draw a sample and computer statistic

f1 = function()
{
    v = c("admit","other")
    p = c(0.4,0.6)
    x = sample(size = n, x=v, prob = p, replace = T)
    return(prop.table(table(x))[1])
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

########################################################################################
# non parametric test - median = 500
#test statistic
tstat = sum(ifelse(GMAT>500,1,0))

f1 = function()
{
    v = c(0,1)
    p = c(0.5,0.5)
    x = sample(size = n, x= v, prob = p, replace = T)
    return(sum(x))
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
##################################################################################################

#confidence intervals
#parametric

f1 = function()
{
    x = rnorm(n=n, mean = mean(GMAT), sd = sd(GMAT))
    return(median(x))
}

#create the sampling distribution
sdist <- replicate(10000,f1())
sdist

#plot and compute the confidence intervals
plot(density(sdist))
quantile(sdist,c(0.025,0.975))

### Bootstrapped confidence interval

f1 = function()
{
    x = sample(size = n, x=GMAT,replace = T)    
    return(median(x))
}

#create the sampling distribution
sdist <- replicate(10000,f1())
sdist

#plot and compute the confidence intervals
plot(density(sdist))
quantile(sdist,c(0.025,0.975))

###########################################################################

x = c("A","B","C","D")
sample(x)

# two sample non paramteric test
twosample <- read.csv("C:/Users/vpran/OneDrive - University of Connecticut School of Business/MSBAPM/B13 - OPIM 5503 - Data Analytics using R/Session 4/Data/twosample.csv")
treatment <- twosample[twosample$group=="Treatment",2]
control <- twosample[twosample$group=="Control",2]

n = length(treatment)
tstat = mean(treatment)-mean(control)

f1 = function()
{
    x = c(treatment,control)
    x = sample(x)
    m1 = mean(x[1:n])
    m2 = mean(x[(n+1):length(x)])
    return(m1-m2)
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
###############################################################################################
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
