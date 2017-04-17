admission <- read.csv("C:/Users/rgopal/Desktop/Session 4/Data/admission.csv")
GMAT = admission$GMAT
GMAT

# 75th percentile = 600

qnorm(mean = 532.5,sd = 100,p = 0.75)

# test statistic
tstat= quantile(GMAT,probs = 0.75)

# draw a sample and compute the statistic
f1 = function()
{
  x= rnorm(n = length(GMAT),mean = 532.5,sd = 100)
  return(quantile(x,probs = 0.75))
}

# Create the sampling distribution
sdist=replicate(10000,f1())

# draw and evaluate the hyopthesis
plot(density(sdist))
abline(v=tstat,col="red")
gap = abs(mean(sdist)-tstat)
s1 = sdist[sdist<mean(sdist)-gap|sdist>mean(sdist)+gap]
pvalue = length(s1)/length(sdist)
pvalue

# cor test
GPA = admission$GPA

# test statistic
tstat= cor(GPA,GMAT)
tstat

n = length(GMAT)
# draw a sample and compute the statistic
f1 = function()
{
  xGPA= rnorm(n = n,mean = mean(GPA),sd = sd(GPA))
  xGMAT= rnorm(n = n,mean = mean(GMAT),sd = sd(GMAT))
  return(cor(xGPA,xGMAT))
}

# Create the sampling distribution
sdist=replicate(10000,f1())

# draw and evaluate the hyopthesis
plot(density(sdist))
abline(v=tstat,col="red")
gap = abs(mean(sdist)-tstat)
s1 = sdist[sdist<mean(sdist)-gap|sdist>mean(sdist)+gap]
pvalue = length(s1)/length(sdist)
pvalue


# Shape test
plot(density(GMAT))
plot(density(GPA))
GPA1 = (GPA-mean(GPA))/sd(GPA)
GMAT1 = (GMAT-mean(GMAT))/sd(GMAT)
plot(density(GPA1))
lines(density(GMAT1),col="blue")

q = c(0.1,0.3,0.5,0.7,0.95)
q1= quantile(GPA1,probs = q)
q2 = quantile(GMAT1,probs = q)
tstat=sum(abs(q1-q2))

n = length(GMAT)
# draw a sample and compute the statistic
f1 = function()
{
  xGPA= rnorm(n = n,mean = 0,sd = 1)
  xGMAT= rnorm(n = n,mean = 0,sd=1)
  q = c(0.1,0.3,0.5,0.7,0.95)
  q1= quantile(xGPA,probs = q)
  q2 = quantile(xGMAT,probs = q)
  return(sum(abs(q1-q2)))
}

# Create the sampling distribution
sdist=replicate(10000,f1())

# draw and evaluate the hyopthesis
plot(density(sdist))
abline(v=tstat,col="red")
length(sdist[sdist>tstat])/length(sdist)

# Factor data - admission rate = 40%
View(admission)



# test statistic
tstat = prop.table(table(admission$De))[1]

# draw a sample and compute the statistic
f1 = function()
{
  v = c("admit","other")
  p = c(0.4,0.6)
  x = sample(size = n,x = v,prob = p,replace = T)
  return(prop.table(table(x))[1])
}

# Create the sampling distribution
sdist=replicate(10000,f1())

# draw and evaluate the hyopthesis
plot(density(sdist))
abline(v=tstat,col="red")
gap = abs(mean(sdist)-tstat)
s1 = sdist[sdist<mean(sdist)-gap|sdist>mean(sdist)+gap]
pvalue = length(s1)/length(sdist)
pvalue

# Non parametric test - median = 500

# test statistic
tstat = sum(ifelse(GMAT>500,1,0))

# draw a sample and compute the statistic
f1 = function()
{
  v = c(0,1)
  p = c(0.5,0.5)
  x = sample(size = n,x = v,prob = p,replace = T)
  return(sum(x))
}

# Create the sampling distribution
sdist=replicate(10000,f1())

# draw and evaluate the hyopthesis
plot(density(sdist))
abline(v=tstat,col="red")
gap = abs(mean(sdist)-tstat)
s1 = sdist[sdist<mean(sdist)-gap|sdist>mean(sdist)+gap]
pvalue = length(s1)/length(sdist)
pvalue


# Confidence Intervals

# Parametric

f1 = function()
{
  x = rnorm(n = n,mean = mean(GMAT),sd = sd(GMAT))
  return(median(x))
}

# Create the sampling distribution
sdist=replicate(10000,f1())

# plot and compute confidence interval
plot(density(sdist))
quantile(sdist,probs = c(0.025,1-0.025))

# Bootstrapped confidence interval

f1 = function()
{
  x = sample(size = n,x = GMAT,replace = T)
  return(median(x))
}
# Create the sampling distribution
sdist=replicate(10000,f1())

# plot and compute confidence interval
plot(density(sdist))
quantile(sdist,probs = c(0.025,1-0.025))

x = c("A","B","C","D")
sample(x)

# two sample non-parametric test
twosample <- read.csv("C:/Users/rgopal/Desktop/Session 4/Data/twosample.csv")
treatment = twosample[twosample$group=="Treatment",2] 
control = twosample[twosample$group=="Control",2]
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

f1()
# Create the sampling distribution
sdist=replicate(10000,f1())
sdist
# draw and evaluate the hyopthesis
plot(density(sdist))
abline(v=tstat,col="red")
gap = abs(mean(sdist)-tstat)
s1 = sdist[sdist<mean(sdist)-gap|sdist>mean(sdist)+gap]
pvalue = length(s1)/length(sdist)
pvalue 
  
  









