require(ggplot2)
data("economics")
head(economics)
ggplot(economics,aes(x=date,y=pop)) + geom_line()
#pranav v1.1

install.packages("lubridate")
require(lubridate)
economics$year <- year(economics$date)
economics$month <- month(economics$date)


econ2000 <- economics[which(economics$year >= 2000),]
nrow(econ2000)

head(econ2000)
econ2000$month <- month(econ2000$date, label = TRUE)

require(scales)

g <- ggplot(econ2000,aes(x=month,y=pop))
g <- g+geom_line(aes(color=factor(year),group=year))

g <- g + labs(title="Population Growth",x="Month",y="Population")
g <- g + scale_color_discrete(name="Year")
g <- g + scale_y_continuous(labels=comma)
g <- g + theme(axis.text.x=element_text(angle = 90,hjust = 0,vjust = 0))

