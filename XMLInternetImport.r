require(XML)                    #Loading XML package
theURL<-"http://techbus.safaribooksonline.com/9780133578867/35-2013-12-05?percentage=&reader=pf"
Hrs <-readHTMLTable(theURL, which = 1,header = FALSE,StringsAsFactors= FALSE)   #imported data in Hrs DF
class(Hrs)  #checked class of DF

Calc <- Hrs[60:115,3,drop=FALSE]    #Copied records 60 to 115 and column 3 as a DF
names(Calc) <- "time"               #named column

Calc2<-str_split_fixed(Calc$time, ":", 3)       #used : delimetor to store time into 3 separate columns
colnames(Calc2) <- c("Hours","Minutes","Sec")   #since Calc2 is a matrix, renamed the columns
Calc2<-as.data.frame(Calc2,stringsAsFactors = FALSE)    #Converted matrix to data frame

is.null(Calc2[9,"Hours"])       #just checked if blank values were actually NULL

Calc2$Hours <- as.character(Calc2$Hours)        #Converting columns to character to find empty fields and change to NA
Calc2$Hours[Calc2$Hours == ""] <- NA

Calc2$Minutes <- as.character(Calc2$Minutes)    #Converting columns to character to find empty fields and change to NA
Calc2$Minutes[Calc2$Minutes == ""] <- NA

Calc2$Sec <- as.character(Calc2$Sec)            #Converting columns to character to find empty fields and change to NA
Calc2$Sec[Calc2$Sec == ""] <- NA

Calc2<-Calc2[complete.cases(Calc2),]            #updating DF by taking evrything excluding NA rows

Calc2$Hours <- as.numeric(Calc2$Hours)          #converting all columns back to numeric
Calc2$Minutes <- as.numeric(Calc2$Minutes)      #converting all columns back to numeric
Calc2$Sec <- as.numeric(Calc2$Sec)              #converting all columns back to numeric

Time<-(sum(Calc2$Minutes)+(sum(Calc2$Sec)/60))/60   #calculating total time in Hrs

TimeF<- paste(floor(Time), round((Time-floor(Time))*60), sep=":")   #formatting time to show hours:minutes
