baseball = read.csv("C:/Users/vpran/Desktop/baseball.csv")
attach(baseball) 

#Q1

# Bootstrapped confidence interval


f1 = function()
{
    x = sample(size = length(Average),x = Average,replace = T)
    return(IQR(x))
}
# Create the sampling distribution
sdist=replicate(10000,f1())

# plot and compute confidence interval
plot(density(sdist),main = "sdist")
polygon(density(sdist),col="green")
quantile(sdist,probs = c(0.025,1-0.025))


#Q2
AgeLessT60 <- c(75,77,80,69,73,76,78,74,75,81,75,80,79,80)
AgeGreaterT60 <- c(68,74,77,71,73,75,80,77,78,72,69,71,75,78)

tstat = abs(mean(AgeLessT60)-mean(AgeGreaterT60))
n = length(AgeLessT60)

f5 = function() 
{ 
    x = sample(c(AgeLessT60,AgeGreaterT60)) 
    m1 = mean(x[1:n]) 
    m2 = mean(x[(n+1):length(x)]) 
    abs(m1-m2) 
    return(abs(m1-m2)) 
}

sdist = replicate(10000,f5()) 
plot(density(sdist)) 
gap = abs(mean(sdist)-tstat) 
abline(v=mean(sdist)-gap) 
abline(v=mean(sdist)+gap) 
pvalue = length(sdist[sdist<mean(sdist)-gap|sdist>mean(sdist)+gap])/length(sdist)
pvalue 

#Q3
num <- c(-400,75,77,80,69,73,76,77,78,74,75,81,75,80,79,80,150,200,400,500,551)

f1 = function(numvec)
{
    return(length(numvec[numvec>(mean(numvec)+(2*sd(numvec)))|numvec<(mean(numvec)-(2*sd(numvec)))]))
}

f1(num)

#Q4
num2 <- c(5,10,NA)

f1 = function(input)
{
 return(ifelse(is.na(num2), mean(num2, na.rm = TRUE), num2))
}

f1(num2)

