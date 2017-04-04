fit_ets <- ets(triprevenue[,"revenue"], model="ZZZ", damped=TRUE, alpha=NULL, beta=NULL, gamma=NULL, 
    phi=NULL, additive.only=FALSE, lambda=TRUE, 
    lower=c(0.0001,0.0001,0.0001,0.8),upper=c(0.9999,0.9999,0.9999,0.98), 
    opt.crit=c("lik","amse","mse","sigma","mae"), nmse=3, 
    bounds=c("both","usual","admissible"), ic=c("aicc","aic","bic"),
    restrict=TRUE)

fr<-forecast.ets(fit_ets)
forecast(fit_ets,h=7,method ='ets')   
plot.ets(fit_ets)
plot(fit_ets$residuals)

###############rgv ts fitting
rev <- ctrip_subs_mod$revenue
library(bbmle)
f1 = function(u,rho,s)
{
    err = rev[2:91] - u -rho*(rev[1:90]-u)
    LLsum = sum(dnorm(err,mean = 0, sd = s, log=T ))
    return(-1*LLsum)
}

f1(mean(rev),0,sd(rev))
res = mle2(minuslogl = f1,start = list(u = mean(rev),rho = 0,s = sd(rev)))
summary(res)

err = rev[2:91] - 1219.18925 - 0.28699*(rev[1:90]-1219.18925)

pred <- rev[2:91] + err

plot(pred,type = 'l')
points(rev)

accuracy(rev,pred)

##############revenue maximization
precip <- ctrip_subs_mod$precip
tmin <- ctrip_subs_mod$tmin
rev <- ctrip_subs_mod$revenue

f1 = function(a0,a1,a2,x1,x2,sig)
{
    err = rev - (a0 + (a1 * ((tmin)^x1)) + (a2 * ((precip)^x2)))
    LLsum = sum(dnorm(err,mean = 0,sd = sig,log = T))
    return(-1*LLsum)
}

f1(mean(rev),0,0,0,0,sd(rev))

sd_rev = sd(rev)
m_rev <- mean(rev)
res = mle2(minuslogl = f1,start = list(a0=m_rev,a1=0,a2=0,x1=0,x2=0,sig=sd_rev),method = "L-BFGS-B",lower = c(sig=0,x1=0,x2=0),upper = c(x1=2,x2=2))
summary(res)

err = rev[2:91] - 1219.18925 - 0.28699*(rev[1:90]-1219.18925)

pred <- rev[2:91] + err

plot(pred,type = 'l')
points(rev)

accuracy(rev,pred)
