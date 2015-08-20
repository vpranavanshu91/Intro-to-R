theMatrix <- matrix(1:9, nrow = 3)
theMatrix

apply(theMatrix,MARGIN = 2,FUN = sum)
apply(theMatrix,MARGIN = 1,FUN = sum)

colSums(theMatrix)
rowSums(theMatrix)

theMatrix[2,1]<-NA
theMatrix
apply(theMatrix,MARGIN = 2,FUN = sum)

apply(theMatrix,MARGIN = 2,FUN = sum,na.rm=TRUE)
colSums(theMatrix,na.rm = TRUE)

#lapply and sapply in R

Thelist<- list(A=matrix(1:9,nrow = 3),B=1:5,C=matrix(1:4,2),D=2)
Thelist
lapply(Thelist,FUN = sum)
sapply(Thelist, FUN = sum)

theNames<- c("Jared","Deb","Paul")
lapply(theNames,nchar)
sapply(theNames,nchar)
nchar(theNames)

#############################################

firstList<- list(A=matrix(1:16,nrow = 4),B=matrix(1:16,2),C=1:5)
secondList<- list(A=matrix(1:16,nrow = 4),B=matrix(1:16,8),C=15:1)
firstList
secondList

mapply(FUN = identical,firstList,secondList)


simplefunc <- function(x,y)
{
    NROW(x) + NROW(y)
}

mapply(FUN = simplefunc,firstList,secondList)

# No. of rows elements from the list are being added and outputted above
# NROW(firstList$B)
# NROW(secondList$B)
