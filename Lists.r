thedf2 <- data.frame(Sports=i,No=1:9)
list1 <- list(TheDataFrame2=thedf, TheVector2=m)    #Creates a list with a DFrame and a vector
list1[[1]]$Sport                                    #outputs the SPORTS columns as a vector
list1[[1]][,"Sport", drop=FALSE]                    #outputs the SPORTS Column as a Dframe

list2 <- list(TheDataFrame=thedf2,TheList=list1,TheVector=a)    #A list within A List
list2[[2]]                                          #3 ways to call elements of a list
list2[[2]][1]           
list2[["TheList"]]["TheDataFrame2"]

list2[[4]]<-c(1:4)                              #Assigning a vector to a list element
names(list2)[[4]]<- c("AddedList")              #Assigning a name to the 4th element of the list
names(list2[[4]]) <- c("a","b","c","d")         #assigning the column names of the dframe in the 4th element


length(list2)
NROW(list2)
names(list2)

emptylist <- vector(mode = "list",length=4)     #Creating an empty list and then filling with elements
emptylist[[1]]<-c(1:6)
emptylist[[2]]<-thedf
emptylist[[3]]<-c(-3:6)
emptylist[[4]]<- vector(mode="list",length = 2)
emptylist[[4]][[1]][2]<-thedf2
emptylist[[4]][2]<-NA
