x<-10:1
y<--4:5
q<-c("Hockey","Football","Curling","Soccer","Cricket","BasketBall","Kabaddi","lacrosse","Ping Pong","Sprinting")
thedf<-data.frame(x,y,q)                #TO create a Data Frame
thedf<-data.frame(First=x,Second=y,Sport=q,stringsAsFactors = FALSE)    #Assigning Column names and storing sports column as string and not factors
thedf

class(thedf$Sport) #Checking class of the sports column in the data frame

nrow(thedf)         #No. of rows in the data frame
ncol(thedf)         #No. of Columns in the data frame
dim(thedf)          #Returns the dimension of the data frame, i.e. both the columns and rows

nrow(a)
NROW(a)             #Capital nrow can be used to return the no. of rows/elements even in a vector

names(thedf[3])     #name of the third column in the data frame

rownames(thedf)<-c("One","Two","Three","Four","Five","Six","Seven","Eight","Nine","Ten")    #assigning row names
rownames(thedf)                 #checking rownames of the data frame
rownames(thedf)<-NULL           #CHanging the rownames to default in a data frame

head(thedf)         #returns firt few records from the top
head(thedf,n=7)     #returns first 7 records from the top

tail(thedf)         #returns firt few records from the bottom
tail(thedf,n=4)         #returns firt 4 records from the bottom

class(thedf)

thedf$Sport
thedf[3,3]      #Return element from 3rd row and 3rd column
thedf[2:5,2:3]  #return elements from row 2 to 5 and column from 2 to 3
thedf[5,1:3]    #Returns the 5th row and columns 1 to 3
thedf[c(1,10),2:3]  #Returns Row 1 and Row 10 with 2nd and 3rd columns
thedf[,2:3]     #Returns all records for column 2 and 3

thedf[,3]       #returns only Column 3 as a vector
class(thedf[,3])

thedf[,3,drop= FALSE]   #returns only column, but as a data frame
class(thedf[,3,drop= FALSE])   #checks class of the above

thedf[2,]                   #returns 2nd record and all columns
class(thedf[2,])

thedf[,c("First","Sport")]  #returns all rows for the columns specified in that order  
thedf[,c("Sport","First")]  #returns all rows for the columns specified in that order

thedf[,"Sport"]             #returns only Column "Sports" as a vector
thedf[,"Sport", drop=FALSE] #returns only column, but as a data frame
thedf["Sport"]
thedf[["Sport"]]            #Double square brackets ensure that output
thedf[c("Sport","First")]
