shell('C: & cd C:/Users/NeerajSubhedar/Desktop & batch.bat')
cat("\014")

rm(list = ls())
cat("\014")

#####################
## Statistical tests
#####################

## t-test
t.test(data)

## two-tailed test
sdist <- replicate(10000,func)
plot(density(sdist))
abline(v=mean(sdist), col = "red")
abline(v = mean(sdist)-gap, col = "blue")
abline(v = mean(sdist)+gap, col = "blue")
gap <- abs(mean(sdist)-tstat)
# obtaining subset
s1 <- sdist[sdist<mean(sdist)-gap|sdist>mean(sdist)+gap]
pvalue <- length(s1)/length(sdist)
pvalue

## one-tailed test
sdist <- replicate(10000, func)

plot(density(sdist))
abline(v=mean(sdist), col = "red")
abline(v = tstat, col = "blue")
pvalue <- length(sdist[sdist>tstat])/length(sdist)
pvalue

#########
## plots
#########

## base plot
plot(x, y, pch, cex, main, xlab, ylab, col, xlim, ylim, type)

## matrix to create multiple plots
opar = par()
par(bg = "white", mfrow = c(row,col), las = 2, col = "red", bg = "grey")
par(opar)

#######Overlays
## abline
abline(h, v, col, lwd)

## label
text(x,y,label)

# expression as label
label <- expression(paste("y=",alpha^2,"+",gamma,"+",sqrt(beta)))

## highlighting
## create subset of data based on condition and then overlay points on existing graph
## using the same x and y axis
## refer assignment 5
points(x,y,pch,col,cex)

## other plots

#boxplot
boxplot(Gas~Insul, horizontal = TRUE, col = "green")
boxplot(Gas~Insul, horizontal = TRUE, col = c("green","red"))
# notch shows 95% confidence interval of median
boxplot(Gas~Insul, horizontal = TRUE, col = c("green","red"), notch = TRUE)

# stripchart
stripchart(Gas~Insul, col=c("red","blue"), pch = 19, method = "stack")
stripchart(Gas~Insul, col=c("red","blue"), pch = 19, method = "jitter")

#pie chart
whiteside_agg <- aggregate(Salaries$salary, list(Salaries$rank), mean)
pie(whiteside_agg$x,labels = whiteside_agg$Group.1, col = rainbow(3))

#dotchart
dotchart(whiteside_agg$x,labels = whiteside_agg$Group.1, color = rainbow(3))

##########
## Others
##########

## Basic
seq(to,from,by,length.out)
rep(vector, each, len/times)
# if include.lowest is not specified then values more than 'a' are considered, take -Inf instead of 'a' to fix
# similarly d can be set as Inf if upper limit is not specified
cut(vector/df, breaks = c(a,b,c,d),include.lowest = )

## Aggregate
aggregate(x, by = vec/df/list, func)

## proportion table
prop.table(table())
## frequency table
ftable(table())

## packages
ISwR, MASS
dplyr - arrange, filter

## data
data(package = "name") ## to list data available in a package
data("name")
attach()/ detach()

#############
## Functions
#############

## functions in Assignment 6
# 1. running mean after every 'i'th value
# 2. exponential smoothing
# 3. number of primes
# 4. roll a dice
# 5. missing value summary for data frame

#################
## Distributions
#################

runif(num, a, b)
dunif # density
punif # distribution function
qunif # quantile function

rexp(num, rate)
rpois(num, lambda)


###############
## PDF summary
###############

# Session 1
# plots, vectors, missing values, arrays, subsetting, sorting, edit, fix

# Session 2
# table, prop.table, ftable, cut, aggregate, apply functions, dplyr
# selection function and related calls [contains, ends_with, starts_with, matches, num_range, one_of]
# mutate, filter, arrange, summarize
# plots, boxplot, stripchart, piechart
# corrgram

# Session 4
# distributions, coins, dice
# Shapiro-wilk test
# Statistical testing