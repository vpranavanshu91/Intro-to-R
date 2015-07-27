"http://www.jaredlander.com/data/Tomato%20First.csv"
tomato<- read.table(file = "C:/Users/CVO/Documents/R/Tomato First.csv",header=TRUE,sep=",")
head(tomato)
class(tomato$Tomato)

tomato<- read.table(file = "C:/Users/CVO/Documents/R/Tomato First.csv",header=TRUE,sep=",",stringsAsFactors = FALSE)
class(tomato$Tomato)

#sep="\t"
#sep=";"



