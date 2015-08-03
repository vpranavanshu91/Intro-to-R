a <- c(1,0,1,1,0,1,0)
a[4]<-NA

ifelse(a==1,a*7,"FALSE")

#########################################
q <- c(1,1,0,1)
w <- c(2,1,0,1)

ifelse(q==1 & w==1,TRUE,FALSE)  #Single & checks all the values in the matrix
ifelse(q==1 && w==1,TRUE,FALSE) #Double & checks only the first value

ifelse(q==1 | w==1,TRUE,FALSE)
ifelse(q==1 || w==1,TRUE,FALSE)
########################################
x <- 1
y <- 2
if(x==0 && y==3)    #Using a double && reduces computational time, since if the first condition is not met, 
                        #it does not go further, with single & it goes ahead even if the case fails.
{
    print("Hello")
}

if(x==0 || y==3)
{
    print("Hello")
}