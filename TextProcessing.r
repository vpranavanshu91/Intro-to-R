Needed <- c("tm", "SnowballCC", "RColorBrewer", "ggplot2", "wordcloud", "biclust", "cluster", "igraph", "fpc")
install.packages(Needed, dependencies=TRUE)
install.packages("Rcampdf", repos = "http://datacube.wu.ac.at/", type = "source") 

cname <- file.path("C:", "texts")   
cname   
dir(cname) 

library(tm)   
docs <- Corpus(DirSource(cname))   

summary(docs)   
inspect(docs[1])
docs <- tm_map(docs, removePunctuation) 
docs <- tm_map(docs, removeNumbers) 
docs <- tm_map(docs, tolower) 
docs <- tm_map(docs, removeWords, stopwords("english"))  

library(SnowballC)   
docs <- tm_map(docs, stemDocument)   

docs <- tm_map(docs, stripWhitespace) 

docs <- tm_map(docs, PlainTextDocument) 


#############################

dtm <- DocumentTermMatrix(docs)   
dtm
inspect(dtm)
tdm <- TermDocumentMatrix(docs)  

freq <- colSums(as.matrix(dtm))   
length(freq)   
ord <- order(freq) 

m <- as.matrix(dtm)   
dim(m)   
write.csv(m, file="dtm.csv") 

dtms <- removeSparseTerms(dtm, 0.1) # This makes a matrix that is 10% empty space, maximum.   
inspect(dtms)  

freq[head(ord)]  

freq[tail(ord)]  

head(table(freq), 20)   
tail(table(freq), 20) 

freq <- colSums(as.matrix(dtms))   
freq   
findFreqTerms(dtm, lowfreq=50)

wf <- data.frame(word=names(freq), freq=freq)   
head(wf)  
###################################

library(ggplot2)   
p <- ggplot(subset(wf, freq>10), aes(word, freq))    
p <- p + geom_bar(stat="identity")   
p <- p + theme(axis.text.x=element_text(angle=45, hjust=1))   
p   

library(wordcloud)  
set.seed(142)   
wordcloud(names(freq), freq, min.freq=5) 
