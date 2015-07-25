1+1
install.packages("useful")
require("useful")
x<-3
assign("r",8)
remove("r")
x<-2L
class(x)
is.numeric(x)
y<- "pranav"
f<-factor("Pen")
nchar(y)
nchar(6789)
dateprana <- date()
remove(dateprana)
date1 <- as.Date("1991-05-13")
as.numeric(date1)
date2 <- as.POSIXct("1991-05-13 03:50")
nchar(date2)
as.numeric(date2)
TRUE
x<-TRUE
TRUE*5
FALSE
FALSE*5
k<-FALSE
k
k*x
k+x
class(k)
class(x)
is.logical(x)
T
F
class(T)
T<-7L
class(T)
T<-7
class(T)
3==3
2!=3
o <- 2!=3
o
"data" < "stats"
"data" = "bata"
remove("data")
"data"=="bata"
q<- (nchar("data") > nchar("kei"))
q <-c(1,2,3,4,5,6,7,8,9,10)
o<- q-T
sqrt(q)
5:-7
o/q
o+q
o^q
length(o)
length(q+o)
o<-o+ c(5,3)
o<=5
o<=q
any(o>q)
all(q>o)
p <- c("hockey", "cricket","Soccer", 
       "footbmall")
p
nchar(p)
p[q]
p[1]
