require(plyr)

head(baseball)

baseball$sf[baseball$year<1954]<-0
any(is.na(baseball$sf))

baseball$hbp[is.na(baseball$hbp)] <-0
any(is.na(baseball$hbp))

baseball <- baseball[baseball$ab>=50,]

baseball$OBP <- with(baseball,(h+bb+hbp)/(ab+bb+hbp+sf))
tail(baseball)

            
OBP <- function(data)
{
    c(OBP=with(data,sum(h+bb+hbp)/sum(ab+bb+hbp+sf)))
}

careerOBP <-ddply(baseball,.variables = "id",OBP)
careerOBP <- careerOBP[order(careerOBP$OBP, decreasing=TRUE),]
head(careerOBP)

test<-baseball[baseball$id=="willite01",]  #fetching only records of ted williams
remove(test)