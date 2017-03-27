install.packages("ISwR")    #Installing the ISwR package
library(ISwR)               #loading the ISwR package
data("stroke")              #loading the stroke dataset
help("stroke")              #Finding Column Information on the stroke dataset
stroke                      #displaying the stroke dataset


# 1.     Mean, median and 75th percentile values of age

mean(stroke$age,na.rm = T)  #Mean of "age" column in "Stroke" dataset, with NA removed
# 69.8854

median(stroke$age,na.rm = T)    #Median of "age" column in "Stroke" dataset, with NA removed
# 71

quantile(stroke$age,probs = 0.75,na.rm = T) #75th percentile values of "age" column in "Stroke" dataset, with NA removed
# 81

#--------------------------------------------------------------------------------------------------------------------------

# 2.     Conduct a t-test to evaluate the hypothesis for the population age of 71 for age variable.
t.test(stroke$age,mu = 71,conf.level = 0.95) #t-test to evaluate the hypothesis for the population age of 71 for "age" column in "stroke" dataset

# One Sample t-test
# 
# data:  stroke$age
# t = -2.3232, df = 828, p-value = 0.02041
# alternative hypothesis: true mean is not equal to 71
# 95 percent confidence interval:
#     68.94369 70.82711
# sample estimates:
#     mean of x 
# 69.8854 

# Since p-value is less than 0.05(our alpha), we reject null hypothesis and accept alternate.
# i.e. alternative hypothesis: true mean is not equal to 71

#--------------------------------------------------------------------------------------------------------------------------

# 3.     Create a subset data frame called s1 of all patients who are not dead. 
# Draw a plot of sex and age for these individuals. What is your takeaway from the visual?

s1 <- stroke[stroke$dead== FALSE,]  #created subset data frame called "s1" of all patients who are not dead
unique(s1$dead)             #Reconfirm the values in "dead" column in "s1" dataset

plot(s1$sex,s1$age)         #a plot of sex and age for all patients who are not dead
# Takeaways
# 1) Median age of women is higher when compared to that of men
# 2) The oldest living person is a female
# 3) The youngest living person is a male
# 4) SD of males is greater than that of females

#numerical proof for point 4 above
sd(s1[s1$sex == "Male","age"])      
sd(s1[s1$sex == "Female","age"])

#--------------------------------------------------------------------------------------------------------------------------

# 4.     What is the mean age of all patients who have diabetes and are not dead?
mean(stroke[stroke$dead == FALSE & stroke$diab=='Yes',"age"],na.rm = T)
# 65.11429

#--------------------------------------------------------------------------------------------------------------------------

# 5.     What is the mean age of all patients who have diabetes and are dead?
mean(stroke[stroke$dead== TRUE & stroke$diab=='Yes',"age"],na.rm = T)
# 71.37097