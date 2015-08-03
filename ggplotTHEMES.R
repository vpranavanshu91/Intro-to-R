install.packages("ggthemes")
require(ggthemes)

g <- ggplot(diamonds,aes(x=carat, y=price))
g + geom_point(aes(color=color, shape=cut, size=depth))  #Using 3 Legends/dsicriminators


G1 <- g + geom_point(aes(color=color, shape=cut)) 
G1 + theme_wsj()    #Using the wall street Journal theme
G1 + theme_economist() + scale_color_economist()    #uses the economist color pallete
