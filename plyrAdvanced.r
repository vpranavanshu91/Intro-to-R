theList <- list(A=matrix(1:9,3),B=1:5,C=matrix(1:4,2),D=2)


identical(lapply(theList, sum),llply(theList,sum))

llply(theList,sum)   #plyr package
lapply(theList, sum)

sapply(theList,sum)
laply(theList,sum)   #plyr package

head(diamonds)
aggregate(price ~ cut,diamonds,each(mean, median))


numcolwise(sum,na.rm=TRUE)(diamonds)

sapply(diamonds[,sapply(diamonds, FUN = is.numeric)],FUN = sum)