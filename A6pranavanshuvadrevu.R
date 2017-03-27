#Pranavanshu Vadrevu

#Q1
run_mean <- function(vec_in)
{
    sum = 0     #sum variable set to 0
    no = 1      #no to keep a count of the lementsin the vector
    vec_out = 0     #final output created and set to 0
    for (element in vec_in)     #using the input vector as an interable 
    {
        sum = element + sum     #taking the ith elments and adding them to sum
        mean = sum/no           #taking mean for summation with every ith element when count is updating nelow
        no = no + 1             #the number counter updating after every ith element
        vec_out <- c(vec_out,mean)  #appending the final output vector with the current mean value
    }
        
    return(vec_out[-1])     #returning the mean values except the 1st element, which was a default value 
}

#sample
run_mean(c(1,3,5,7))
run_mean(c(3,5,7,9))
#######################################################################################
#Q2
exp_smoothing <- function(vec_in, alpha=0.8)    #setting default for alpha parameter and vec_in as the mandatory input 
{
    s = 0
    for (i in 1:length(vec_in))      #using the input vector as an interable 
        {
        if (i == 1)         #setting the smoothing value for the 1st element as itself, will be entered only once
            { 
            s[i] = vec_in[i]
            } 
        
        else                #will be TRUE after the first iteration
            {
            s[i] = s[i-1] + alpha*(vec_in[i-1]-s[i-1])    #using the exponential smoothing formulae
            }
    }
    result <- cbind.data.frame(vec_in,s)          #making the data frame using cbind by binding columns
    colnames(result) <- c("Actual","Predicted")
   return(result)        #returning the vector with the smoothing values
} 

#sample
exp_smoothing(c(1,3,5,7))
exp_smoothing(c(3,5,7,9))
#######################################################################################
#Q3
install.packages("schoolmath")  #used this package instead of school math  since that was erenuous 
library(schoolmath)

num_prime <- function(a=1,b=1)  #setting default parameters to the arguments
{
        return(ifelse(a==b,0,sum(is.prim(seq.int(a+1,b-1))))) #using seq.int to generate nos between the input numbers, wrapping them in the prime num function and summin all the TRUES returned by it.
}

#sample
num_prime(2,6)
num_prime(2,9)
################################################################################
#Q4
die_throw <- function()
{
    v = c(1,2,3,4,5,6)          #created a vector with 6 possible values
    p = rep(1/6,6)          #storing the prob of each no. in a vector

    throw <- sample(size = 2, x = v, prob = p, replace = T)     #using sample command to generate dice throws using prob
    
    res = ifelse(throw[1]==throw[2],"You WIN","You LOSE")  #checking if the first and second throw are same for a win
    
    return(res)
}

#sample
die_throw()
die_throw()
###############################################################################
#Q5
library(MASS)
k <- survey					#loading the dataset survey
l <- whiteside				#loading the dataset whiteside

missing <- function(s)
{
    x <- data.frame()           #creating a blank data frame
    counter <- ncol(s)          #creating a counter for the loop with storing the no. of columns
    name <- colnames(s)         #stored column names in a variable name
    
    for (i in seq(counter))     #running iterations using seq function on counter
    {
        col1 <- as.character(name[i])       #derieving the column name for the ith column
        col2 <- sum(is.na(s[,i]))           #summig all the TRUEs in the is.na to get the na count for every ith column
        col3 <- formatC(col2/length(s[,i]) * 100,9,format = 'f')  #calculating % of missing valuesfor every ith column
        col4 <- length(unique(s[,i])) #using unique to view different values in the ith column and then using length to give unique elements
        
        x = rbind(x,cbind(col1,col2,col3,col4))  #using cbind to combine the columns for ith rows together and appending using rbind to the data set for every ith column
    }
    
    colnames(x) <- c("Column Name","# Missing Values","% Missing Values","# Unique Values")  #setting column names
    return(x)   #returning the data frame
} 

#sample
missing(k)
missing(l)
#########################################################################################

