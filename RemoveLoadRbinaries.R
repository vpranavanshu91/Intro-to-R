tomato<- read.table(file = "C:/Users/CVO/Documents/R/TomatoFirst.csv",header=TRUE,sep=",")
head(tomato)
class(tomato$Tomato)

tomato<- read.table(file = "C:/Users/CVO/Documents/R/TomatoFirst.csv",header=TRUE,sep=",",stringsAsFactors = FALSE)
class(tomato$Tomato)

save(tomato,file = "C:/Users/CVO/Documents/R/Tutorial/TomatoFirst.Rdata")
rm(tomato)


load("C:/Users/CVO/Documents/R/Tutorial/TomatoFirst.Rdata")

tomato