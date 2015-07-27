c<-matrix(data = 1:25,nrow = 5)
d<-matrix(data = 1:5,ncol = 5)

LETTERS
letters

colnames(c)
rownames(c)

colnames(c)<- c("1ST","2ND","3RD","4TH","5TH")
rownames(c)<-c("one","two","three","four","five")

colnames(d)<-LETTERS[1:5]   #assigning col names using letters function
rownames(d)<-c("THE ONE")


d%*%c                   #dot product of matrices
t(c)%*%t(d)             #transposed dot product
t(d%*%c)


all(t(d%*%c)==t(c)%*%t(d))

thearray<- array(1:27,dim = c(3,3,3))   #creation of array i.e. they are multi dimensional. In this case 3x3x3
thearray[2,1,3]         #IMAGINE A CUBE of 3X3 
                        #2 is the layer from top to bottom ,
                        #1 is the part from left to right and 3 is the face from front to back
                            
