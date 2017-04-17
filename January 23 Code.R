# Our First R Code
8*56
exp(75)
rnorm(10)
# packages
library()
install.packages("ISwR")
library(ISwR)
library(MASS)
library(help=MASS)
help("whiteside")
data()
getwd()


# Data Objects
weight = c(60,72,57,90,95,72)
height = c(1.75,1.8,1.65,1.9,1.74,1.91)
mean(weight)
sd(height)
median(height)
quantile(weight,probs = 0.75)
quantile(weight,probs = c(0.2,0.75))
length(height)
range(weight)
t.test(weight,mu = 80)
bmi = weight/height^2
bmi
plot(height,weight,pch=7,col="red",
     main="First Graph",ylim=c(0,100))
colors()

weight = c(weight,86)
height = c(height,NA)
mean(weight)
mean(height,na.rm = T)
gender = c("M","F","M","F","F","M","F")
gender
names(gender)=c("Bob","Susan","Jim","Mary","Jane","Tim","Nicole")
names(gender)
plot(factor(gender),weight)

# creating vectors
x = seq(1,15,by = 3)
x
x = seq(1,15,length.out = 4)
x
rep("A",5)
rep(c("A","B"),5)
rep(c("A","B"),c(5,7))

# Indexing and Subsetting
weight[c(1,5)]
weight[-3]
height[height>1.7]
height[height>1.7 & is.na(height)==F]

# dataframe
ghw = data.frame(gender,height,weight)
ghw
ghw[2,2]
ghw[1,]
ghw[c(3,5),]
# viewing and editing
edit(ghw)
fix(ghw)
View(ghw)

# Indexing data frames
ghw$height
s1 = ghw[ghw$gender=="F",]
edit(s1)

View(whiteside)
plot(whiteside$Insul,whiteside$Gas)

# data frames
str(whiteside)
str(ghw)
summary(ghw)
dim(ghw)
row.names(whiteside)
row.names(ghw)
colnames(ghw)
colnames(whiteside)





























