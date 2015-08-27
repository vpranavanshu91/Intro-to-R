sport <- c("hockey","Football","baseball")
league <- c("NHL","NFL","MLB")
trophy <- c("stanley Cup","Vincent Lombard Trophy","Commisioners trophy")

sports <- cbind(sport,league,trophy)

sports2<- data.frame(sport=c("BasketBall","Golf"),
                            league=c("NBA","PGA"),
                            trophy=c("LOB Champ trophy","PGA tour Trophy"))

SPORTS<- rbind(sports,sports2)

#cbind is used to combine individual columns from matrices, vectors to form a matrix
#rbind is used to combine rows, with same column names, returns a data frame