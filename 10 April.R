opar = par() 
par(mfrow=c(2,2))
library(car)

###Reg Diagnostics
#Scenario 1: Simple regression 
x = runif(500,1,100) 
y = 250 + x + rnorm(500,0,10) 
reg1 = lm(y~x)
summary(reg1)
plot(reg1)
err = reg1$residuals
shapiro.test(err)
ncvTest(reg1)   #homoskedasticity test

#Scenario 2: Heteroscedasticity 
x = runif(500,1,20) 
y = 100+2*x + x*rnorm(500) 
reg1 = lm(y~x)
ncvTest(reg1)
summary(reg1)
plot(reg1)
err = reg1$residuals
shapiro.test(err)

#Scenario 3: Nonlinear 1 
x = runif(500,1,20) 
y = ifelse(x<15,100+2*x +rnorm(500),100+5*x+rnorm(500)) 
reg1 = lm(y~x)
summary(reg1)
plot(reg1)
err = reg1$residuals
shapiro.test(err)

#Scenario 4: Nonlinear 2 
x = runif(500,1,20) 
y = 100+2*x +0.5*x^2 + rnorm(500) 
reg1 = lm(y~x)
summary(reg1)
plot(reg1)
err = reg1$residuals
shapiro.test(err)

#Scenario 5: Interaction 
x1 = runif(500,1,20) 
x2 = runif(500,1,20) 
y = x1+4*x2+0.5*x1*x2 + rnorm(500) 
reg1 = lm(y~x1+x2)
plot(reg1)
err = reg1$residuals
shapiro.test(err)

#Scenario 6: Extreme Values (x) 
x = runif(500,1,100) 
y = 250 + x + rnorm(500,0,10) 
x[499] = 860 
reg1 = lm(y~x)
summary(reg1)
plot(reg1)
err = reg1$residuals
shapiro.test(err)

#Scenario 7: Extreme Values (y) 
x = runif(500,1,100) 
y = 250 + x + rnorm(500,0,10) 
y[499] = 2000 
reg1 = lm(y~x)
summary(reg1)
plot(reg1)
err = reg1$residuals
shapiro.test(err)
#larger the cooks distance larger the influence, it runs regression with and without the data point and
#estimated distance between the coefficients
z = cooks.distance(reg1)
plot(reg1,which = 4, cook.levels = cutoff)
abline(h = cutoff,col='red')
round(z,4)
cutoff = 4 / length(x)
z[z>cutoff]

#Scenario 8: Multicollinearity 
x1 = runif(500,1,10) 
lambda = 0.1 
x2 = (lambda*x1) + (1-lambda)*runif(500,1,10) 
cor(x1,x2) 
y = 2*x1 + x2 + rnorm(500,0,10) 
reg1 = lm(y~x1+x2)
summary(reg1)
plot(reg1)
err = reg1$residuals
shapiro.test(err)
vif(reg1)
spread.level.plot()

#Boston data test for influential obs
library(MASS)
attach(Boston)

reg2 = lm(medv~.,data=Boston)
summary(reg2)
ncvTest(reg2) #homoskedasticity test
z = cooks.distance(reg2)
plot(reg2,which = 4, cook.levels = cutoff)
abline(h = cutoff,col='red')
round(z,4)
cutoff = 4 / length(x)
z[z>cutoff]

reg3 = lm(medv~.,data=Boston[-c(369,373),])
summary(reg3)

cbind(round(reg2$coefficients,4),round(reg3$coefficients,4))

#VIF test for multicolinearity

attach(women)
reg1 = lm(weight~height,data=women)
summary(reg1)
ncvTest(reg1) #homoskedasticity test

z = cooks.distance(reg1)
plot(reg1,which = 4, cook.levels = cutoff)
abline(h = cutoff,col='red')
reg2 = lm(weight~.,data=women[-c(1,14,15),])
summary(reg2)
cutoff = 4 / length(weight)
z[z>cutoff]
cbind(round(reg1$coefficients,4),round(reg2$coefficients,4))
detach(women)
#why is it called OLS regression?

library(bbmle)

f1 <- function(a0,a1) {

  err = y -a1 - a1*x
  totalerr = sum(err^2)
  return(totalerr)
}

res = mle2(minuslogl = f1, start = list(a0=mean(y),a1=0))
summary(res)

#weighted least squares and robust regression
library(MASS)

x = runif(500,1,100) 
y = 250 + x + rnorm(500,0,10) 
x[499] = 860 
reg1 = lm(y~x)
summary(reg1)

z = cooks.distance(reg1)
cutoff = 4/length(x)
w = ifelse(z<cutoff,1,cutoff/z)
w
reg2 = lm(y~x,weights = w)
summary(reg2)

reg3 = rlm(y~x)
summary(reg3)

#################################################
x = runif(500,1,20) 
y = 100 + 2*x + 0.5*(x^2) + rnorm(500)
reg1 = lm(y~x)
summary(reg1)
plot(reg1)

reg2 = lm(y~x+I(x^2))
anova(reg1,reg2)
AIC(reg1,reg2)
opar = par() 
par(mfrow=c(2,2))
plot(reg2)

################################################
attach(women)
reg1 = lm(weight~height+I(height^2),data=women)
summary(reg1)

reg2 = lm(weight~height+I(height^2)+I(height^3),data=women)
summary(reg2)

anova(reg1,reg2)
AIC(reg1,reg2)
detach(women)
#############################################
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

reg1 = lm(medv~.,data=Boston)
boxCox(reg1,family = "yjPower", plotit = T) 
reg2 = lm(log(medv)~.,data = Boston)
summary(reg1)
summary(reg2)
############################################
#box tidwell
x = runif(500,1,100) 
y = 250 + x + rnorm(500,0,10)
reg1 = lm(y~x)
summary(reg1)
boxTidwell(y~x)

x = runif(500,1,20) 
y = 100 + 2*x + 0.5*(x^2) + rnorm(500)
reg2 = lm(y~x)
summary(reg2)
boxTidwell(y~x)
###########################################
#We do not do ANOVA and AIC when Ys are different as in regular y and log(y), hence only r^2 should be seen
attach(women)
reg1 = lm(weight~height,data=women)
summary(reg1)
boxTidwell(weight~height)

reg2 = lm(weight~height+I(height^4),data=women)
summary(reg2)

anova(reg1,reg2)
AIC(reg1,reg2)
detach(women)
#########################################
attach(Boston)
boxTidwell(medv~age++dis+rad, data = Boston)
detach(Boston)

########################################
#to check for interactions between 2 independent variables use STEP function.
#we add levels of interactions
attach(Boston)
x1 = runif(500,1,20)
x2 = runif(500,1,20)

reg1 = lm(medv~., data = Boston)
res = step(reg1,~.^2)
res$anova    #get addition and subtraction flow and AIC comparison
