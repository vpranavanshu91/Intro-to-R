require(ggplot2)
data(diamonds)                                               #Loading the Diamonds Dataset
head(diamonds)
hist(diamonds$carat, main = "Carrot Histogram",xlab = "Carrot")
plot(diamonds$carat,diamonds$price)                         #Scatter plotting caratVSprice using basic func
plot(price~carat,data = diamonds,main="Price Vs Carat")     #Another way to do the above


boxplot(diamonds$carat,main="Boxplot")                      #box plot for carats



#below we always layers and points, both have to present to display a graph, aes can be stored within any of the two


ggplot(diamonds) + geom_histogram(aes(x=carat))      #binwidth defaulted to range/30


ggplot(diamonds) + geom_histogram(aes(x=carat),binwidth=0.1)  #binwidth is width of interval


ggplot(diamonds) + geom_density(aes(x=carat),fill="grey50")     #Filling Color

ggplot(diamonds) + geom_point(aes(x=carat, y=price))
ggplot(data=diamonds, aes(x=carat,y=price)) + geom_point()      #aes func moved to ggplot

g<-ggplot(diamonds,aes(x=carat,y=price))    #you can assign layer 1 to a variable and then add other functions
g
g+geom_point(aes(color=color))      #Segragates the scatter plot by color
g+geom_point(aes(color=color,shape=cut))    #Segragates the scatter plot by color and shape

ggplot(diamonds, aes(y=carat,x=1)) + geom_boxplot()
ggplot(data = diamonds) + geom_boxplot(aes(y=carat,x=1))
ggplot(diamonds,aes(y=carat,x=cut)) + geom_boxplot()
ggplot(diamonds,aes(y=carat,x=color)) + geom_boxplot()   

ggplot(diamonds,aes(y=carat,x=cut)) + geom_violin()

g1<-ggplot(diamonds,aes(y=carat,x=cut))     #assigns the ggplot layer to a variable

g1 + geom_violin() + geom_point()   #multiple layers of points are being added in ORDER
g1 +geom_point() + geom_violin()
g1 + geom_jitter() + geom_violin()