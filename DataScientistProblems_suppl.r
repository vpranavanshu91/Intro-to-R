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
