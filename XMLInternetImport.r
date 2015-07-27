require(XML)
theURL<-"http://www.jaredlander.com/2012/02/another-kind-of-super-bowl-pool/"
bowlgame <-readHTMLTable(theURL, which = 1,header = FALSE,StringsAsFactors= FALSE)
bowlgame
