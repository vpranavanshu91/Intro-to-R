numetrics <-
function(a,m)
{
    metrics <- c(MAD=0,MSE=0,MAPE=0,MPSE=0,R2=0,TMAD=0,p90=0)
    metrics["MAD"] = format(mean(abs(a-m)),nsmall = 2)
    metrics["MSE"] = format(mean((a-m)^2),nsmall = 2)
    metrics["MAPE"] = format(mean(abs((a-m)/a)),nsmall = 2)
    metrics["MPSE"] = format(mean(((a-m)/a)^2),nsmall = 2)
    
    SST = sum((a-mean(a))^2)
    SSE =   sum((a-m)^2)
    metrics["R2"] = format(1 - (SSE/SST),nsmall = 2)
    
    metrics["TMAD"] = format(mean(abs(a-m),trim = 0.05),nsmall = 2)
    metrics["p90"]= quantile(abs(a-m),probs = 0.9)
    return(metrics)
}