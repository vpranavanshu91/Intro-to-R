x <- seq(10,1000,1)
y <- sqrt(log(x))
z <- 50/x

main <- expression( paste("y = ",alpha^2+gamma+sqrt(beta)))


plot(x,z,col = 'red',pch = 19,ylab = "y and z",xlab = "x",main = main)
points(y,col = 'blue',pch = 19)