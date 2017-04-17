#Version1.1
r1 = rnorm(3000)
u1 = runif(3000)

r2 = rnorm(3000)
u2 = runif(3000)

plot(density(u1+u2))

shapiro.test(r1+r2)



t1 = rnorm(10000, mean = 5, sd = 4)
t2 = runif(10000,min =3,max=7 )
t = t1+t2

plot(density(t))

shapiro.test(t)
nortest::ad.test(t)

pnorm(7,mean = 10,sd = 4.235108)  ## to get a p value more realistic or closer use the same to calculate probability
summary(t)
sd(t)


# sampling error

x <- rnorm(2000,mean = 12, sd = 7)
summary(x)

# Standard error of the mean simulation
x<- 0
for(i in 1:1000)
{
k <- rnorm(2000,mean = 12, sd = 7)
x[i] <- mean(k)
}
plot(density(x))

# to give confidence interval of the mean 90% taking 5% of both tails
qnorm(0.95,mean = 12.00137,sd = 0.1520076)
qnorm(0.05,mean = 12.00137,sd = 0.1520076)


# Statistical testing 

admission <- read.csv("C:/Users/vpran/OneDrive - University of Connecticut School of Business/MSBAPM/B13 - OPIM 5503 - Data Analytics using R/Session 4/Data/admission.csv")
GMAT <- admission$GMAT
head(GMAT)
##################################################################################
#test statistic 1

# there might be sampliong error thus im hypothesising that my test statistic will be a value and checking the p value of my test statistic being that number

tstat = mean(GMAT)

# draw a sample and compute the statistic

f1 = function()
{
x <- rnorm(n= length(GMAT), mean = 510, sd = sd(GMAT))
return(mean(x))
}

#create the sampling distribution
sdist <- replicate(10000,f1())

#draw and evaluate the hypothesis
plot(density(sdist))
abline(v = tstat)
abline(v = mean(sdist),col = "blue")
abline(v = mean(sdist)+gap,col = "red")
gap = abs(mean(sdist)-tstat)
s1 = sdist[sdist<mean(sdist)-gap|sdist>mean(sdist)+gap]
pvalue = length(s1)/length(sdist)


##########################################################################
#test statistic 2

# there might be sampliong error thus im hypothesising that my test statistic will be a value and checking the p value of my test statistic being that number

tstat = sd(GMAT)

# draw a sample and compute the statistic

f1 = function()
{
  x <- rnorm(n= length(GMAT), mean = mean(GMAT), sd = 78)
  return(sd(x))
}

#create the sampling distribution
sdist <- replicate(10000,f1())

#draw and evaluate the hypothesis
plot(density(sdist))
abline(v = tstat)
gap = abs(mean(sdist)-tstat)
abline(v = mean(sdist),col = "blue")
abline(v = mean(sdist)-gap,col = "red")
s1 = sdist[sdist<mean(sdist)-gap|sdist>mean(sdist)+gap]
pvalue = length(s1)/length(sdist)

##############################################################################

#test statistic 3

# there might be sampliong error thus im hypothesising that my test statistic will be a value and checking the p value of my test statistic being that number

tstat = median(GMAT)

# draw a sample and compute the statistic

f1 = function()
{
  x <- rnorm(n= length(GMAT), mean = 500, sd = sd(GMAT))
  return(median(x))
}

#create the sampling distribution
sdist <- replicate(10000,f1())

#draw and evaluate the hypothesis
plot(density(sdist))
abline(v = tstat)
gap = abs(mean(sdist)-tstat)
s1 = sdist[sdist<mean(sdist)-gap|sdist>mean(sdist)+gap]
pvalue = length(s1)/length(sdist)
##############################################################################
# The above one with mean talks about central limit theorem and we can find out what confidence interval our hypothesis lies in and the corresponding p value tells us if the hypothesised test statistic is representative of the population 
######################################################################################
#test statistic 4

# there might be sampliong error thus im hypothesising that my test statistic will be a value and checking the p value of my test statistic being that number

tstat = mean(GMAT)/sd(GMAT)

# draw a sample and compute the statistic

f1 = function()
{
  x <- rnorm(n= length(GMAT), mean = 6.5, sd = 1)
  return(mean(x)/sd(x))
}

#create the sampling distribution
sdist <- replicate(10000,f1())

#draw and evaluate the hypothesis
plot(density(sdist))
abline(v = tstat)
abline(v = mean(sdist)+gap,col = 'green')
gap = abs(mean(sdist)-tstat)
s1 = sdist[sdist<mean(sdist)-gap|sdist>mean(sdist)+gap]
pvalue = length(s1)/length(sdist)

#############################################################################
#We have a sample with a known mean, median and sd. I hypothesise that the population has median of x.
#Now population is normally distributed so mean=median so im hypothesising the mean now. We draw synthetic samples of the estimations and plot them.
#The distribution of the samples is again normal. We check where our hypothesis lies in that distribution. GAP is the distance between the  test statistic and that of the mean of hypothesized population sampling distribution.
#The value other than the gap is the p value. hence the larger the p value he smaller the gap, the better our hypothesis.