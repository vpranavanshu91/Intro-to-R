# 1.       Create a vector called x with a sequence of numbers from -10 to 10 incrementing by 0.1. 
# Create another vector called y which computes the sine (which is a trigonometric function; the syntax is sin()) of the values of x. 
# Create a plot of x and y. Use pch value of 19, add labels to the x and y axis – “x value” and “sine”, add the title “Assignment Graph”. Plot the points in color orange.

x <- seq(-10,10,by=0.1)  #vector called x with a sequence of numbers from -10 to 10 incrementing by 0.1
y <- sin(x)  # Created another vector called y which computes the sine, which is a trigonometric function of the values of x
plot(x,y,pch=19,main = "Assignment Graph",xlab = "x value" ,ylab = "sine" ,col = "orange") #Created a plot of x and y. where pch value of 19, labels to the x and y axis, and the title are added. points are in orange color.

#---------------------------------------------------------------------------------------------

# 2.       Write the expression in R to repeat x, y, and z as follows
# a.       Repeat 7 times as follows
# b.       Repeat x, y 4 times and z twice as follows.

rep(c("x","y","z"),7)
c(rep(c("x","Y"),4),rep("z",2))
