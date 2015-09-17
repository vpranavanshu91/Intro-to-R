//switching storage paradigms b/w wide and long
install.packages("reshape2")
require(reshape2)
head(airquality)

airMelt<- melt(airquality,id=c("Month", "Day"),value.name = "value",variable.name = "Metric")

head(airMelt,10)

dim(airMelt)
dim(airquality)

airCast<- dcast(airMelt,Month + Day ~ Metric, value.var = "value")

head(airCast,10)

airCast<-airCast[,c("Ozone","Solar.R","Wind","Temp","Month","Day")]
head(airCast,10)
