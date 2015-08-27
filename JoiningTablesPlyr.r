codes <- read.table("C:/Users/CVO/Documents/R/countryCodes.csv",header = TRUE,sep = ",",stringsAsFactors = FALSE)
countries<-read.table("C:/Users/CVO/Documents/R/GovType.csv",header = TRUE,sep = ",",stringsAsFactors = FALSE) 
View(codes)

countryMerged<-merge(x=codes,y=countries,by.x = "Country.name",by.y = "Country")
View(countryMerged)

require(plyr)
codes <- rename (codes, c(Country.name = "Country"))

countryJoined<-join(x = codes,y = countries,by="Country")
View(countryJoined)