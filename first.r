1+1                             #simple math
install.packages("useful")      #install packages
require("useful")               #Loading packages
x<-3                            #Assigning Values
assign("r",8)                   #Assigning Values
remove("r")                     #Removing Assigned Values
x<-2L                           #Assigning Value to X as an Integer 'L' denotes an integer
class(x)                        #outputs data type of the variable
is.numeric(x)                   #checks if the variavble is numeric
y<- "pranav"                    #Assigning strings
f<-factor("Pen")                #Assigning Factors____Concept Unclear *******
nchar(y)                        #No. of characters in the string or number
nchar(6789)
dateprana <- date()             #ASSIGNING TIME STAMP
remove(dateprana)
date1 <- as.Date("1991-05-13")
as.numeric(date1)               #Different from IS.NUMERIC. 'AS.Numeric' outputs the date as a number 
                                #this gives the no of days passed since 1970

date2 <- as.POSIXct("1991-05-13 03:50") #Required date tobe entered with time-zone displayed Automatically
nchar(date2)
as.numeric(date2)               #this gives the no of seconds passed since 1970 
                                #because of using posixCT(better and simpler than posixLT)
TRUE
x<-TRUE                         #Assigning logical value to X , true translates to 1.
TRUE*5
FALSE
FALSE*5
k<-FALSE
k
k*x
k+x
class(k)
class(x)
is.logical(x)                   #Checks if a variable is initialized with logical data
T
F
class(T)
T<-7L
class(T)
T<-7
class(T)
3==3                            #to check for equality
2!=3                            #to check for inequality
o <- 2!=3                       #Assigning the resultant logical output to a variable
o
"data" < "stats"                #quantifying strings and comparing them
"data" = "bata"                 #Assigning one string to a variable which looks like a striing
remove("data")
"data"="bata"
q<- (nchar("data") > nchar("kei"))  #compares the nchar and assigns the ouput logical result to q
q <-c(1,2,3,4,5,6,7,8,9,10)         #assigns q as a array
o<- q-T                 #subtracting the element T from every element of the vector q and assigning it to O
sqrt(q)                 #Mathematical Operation on every element of Q
o<-5:-4                 #Assigning O array which has decremental elements from 5 to -7
o/q                     #divides corresponding array elements with each other only if the size/length is same
o+q
o^q
length(o)               #Gives no. or array elements or length
length(q+o)             #Adds the array elements first , then gives the no. of elements in the array, 
                        #which always remains the same or length
o<-o+ c(5,3)            #Adds the elements 5 and 3 alternatively to existing elments of o and, stores in o
o<=5                    #checks if all elements of o are <=5, returns logical putut
o<=q                    #compares corresponding elements of o and q for logical output
any(o>q)                #checks if atleast 1 of the results satisfy the condition for TRUE
all(q>o)                #checks if all of the results satisfy the condition for TRUE
p <- c("hockey", "cricket","Soccer", 
       "footbmall")     #assigns strings in an array
p
nchar(p)                #returns the no of characters in each string
p[q]                    #returns positional values of elements in P, depending upon the values in q
                        #returns NA if there is no element present at that position
p[c(1,3)]               #returns the first and the third value in the array
a<-c('a','y','r')       #assigning letter to array a
b<-c(one='a', Two='y',Last='r') #assigning labels for the letters assigned 
b
all(a==b)                   #since a,y,r are contents checking if a equals b, inspite of b having labels
w<-1:3                      #initializing w with array of elements first 
names(w)<-c('a','b','c')    #alternative way to add labelsto the array w
i<-c(p,"cricket","lacrosse",
     "water polo","hockey","Soccer")  #initializes variable I by adding addional elements to the array P
i
factor(i)   #ignores repeated elements in array I , and returns only the unique values as LEVELS
as.numeric(factor(i)) #assigns numeric value to the strings and displays posiitoning inarray,including repeats

m<-c(1,NA,3)        #assigning a NA in a array, it displays NA and length shows 3 elements
                    #reprsents missing information
m
length(m)

n<-c(1,NULL,3)      #assigning a NULL in a array shows absense, 
                    #NULL does not get included in length and is not displayed in the array
n
length(n)