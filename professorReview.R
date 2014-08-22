library(ggplot2)
library(plyr)
library(wordcloud)
library(tm)

#Testing playground

View(prof)
p1<-prof[which(prof$id==1),]
class(unlist(p1$review_ids))
reviewsOnProf = review[which(review$id %in% unlist(p1$review_ids)),]
strsplit(reviewsOnProf$review[[1]], " ")[[1]]

p<-prof[which(prof$nugget=="gold"),]
unlist(p$review_ids)
review[which(review$id %in% unlist(p$review_ids)),]

stopwords("english")

###########################################################################

# FUNCTION THAT RETURNS A LIST OF ALL THE WORDS IN REVIEWS FOR THE Professor
getFreqCount = function(x){
  p = prof[which(prof$id==x),]
  df = review[which(review$id %in% unlist(p$review_ids)),]
  x = ""
  for(i in 1:dim(df)[1]){
    y = strsplit(df$review[[i]], " ")[[1]]
    x = c(x, y)
  }
  return(x)
}

# FUNCTION THAT RETURNS A LIST OF ALL THE WORDS IN REVIEWS FOR Nugget type
getFreqCountNugget = function(x){
  p = prof[which(prof$nugget==x),]
  df = review[which(review$id %in% unlist(p$review_ids)),]
  x = strsplit(df$review[[1]], " ")[[1]]
  for(i in 2:dim(df)[1]){
    y = strsplit(df$review[[i]], " ")[[1]]
    x = c(x, y)
  }
  return(x)
}


#CREATE AND FORMAT LIST OF MOST USED WORDS for Prof==1 for example
words = getFreqCount(1)
words = sapply(words, function(x) gsub("[[:punct:]]+\\\\n", "", x))
x = count(words)
x = x[order(-x$freq),]
x$f = x$freq/length(words)
head(x[which(nchar(as.character(x$x)) > 6),], n = 40)

+#load b, which is the sortedFreqList of all words
load("final/sortedFreqList")

#find the common words in the b-list
common = head(b, n = 250)
common = common$x
common = as.character(common)
common <- sapply(common, function(x) gsub("[^[:alnum:][:space:]']+", "", x))
common <- sapply(common, function(x) gsub("[\r\n]", "", x))
common <- tolower(common)

#creating word count for gold, silver, and none
gold = getFreqCountNugget("gold")
silver = getFreqCountNugget("silver")
none = getFreqCountNugget("none")

#lower case
gold<-tolower(gold)
silver<-tolower(silver)
none<-tolower(none)

#remove punctuation, then remove whitespace
gold <- sapply(gold, function(x) gsub("[^[:alnum:][:space:]']+", "", x))
gold <- sapply(gold, function(x) gsub("[\r\n]", "", x))
silver <- sapply(silver, function(x) gsub("[^[:alnum:][:space:]']", "", x))
silver <- sapply(silver, function(x) gsub("[\r\n]", "", x))
none <- sapply(none, function(x) gsub("[^[:alnum:][:space:]']", "", x))
none <- sapply(none, function(x) gsub("[\r\n]", "", x))

#find the intersect of gold, silver, and none
intersect <- intersect(intersect(gold, silver),none)

#count the words
goldx <- count(gold)
silverx <- count(silver)
nonex <- count(none)

#get rid of intersect
##NOTE DOES NOT WORK, what remains are just professors' names lol
#goldx2<-goldx[which(goldx$x %in% setdiff(goldx$x, intersect)),]
#silverx2<-silverx[which(silverx$x %in% setdiff(silverx$x, intersect)),]
#nonex2<-nonex[which(nonex$x %in% setdiff(nonex$x, intersect)),]

#get rid of common words set at 250
goldx2<-goldx[which(goldx$x %in% setdiff(goldx$x, common)),]
silverx2<-silverx[which(silverx$x %in% setdiff(silverx$x, common)),]
nonex2<-nonex[which(nonex$x %in% setdiff(nonex$x, common)),]

#get rid of stopwords
stopWords <- stopwords("english")

goldx3<-goldx2[which(goldx2$x %in% setdiff(goldx2$x, stopWords)),]
silverx3<-silverx2[which(silverx2$x %in% setdiff(silverx2$x, stopWords)),]
nonex3<-nonex2[which(nonex2$x %in% setdiff(nonex2$x, stopWords)),]

goldx4 <- goldx3[order(-goldx3$freq),]
silverx4 <- silverx3[order(-silverx3$freq),]
nonex4 <- nonex3[order(-nonex3$freq),]

#wordcount
wcPlot = function(y, a, b){
  wordcloud(tail(head(y[which(nchar(as.character(y$x)) > 1),], n = b),n=b-a)$x,
            tail(head(y[which(nchar(as.character(y$x)) > 1),], n = b),n=b-a)$freq,
            colors=brewer.pal(8, "Dark2"))  
class(goldx4$x)
fq = getFreqCount(1)
fq<-tolower(fq)
fq <- sapply(fq, function(x) gsub("[^[:alnum:][:space:]']+", "", x))
fq <- sapply(fq, function(x) gsub("[\r\n]", "", x))
fqx <- count(fq)
1:dim(fqx)[1]
#function
score = function(id){
  fq = getFreqCount(id)
  fq<-tolower(fq)
  fq <- sapply(fq, function(x) gsub("[^[:alnum:][:space:]']+", "", x))
  fq <- sapply(fq, function(x) gsub("[\r\n]", "", x))
  fqx <- count(fq)
  x <- 0
  for(i in 1:dim(fqx)[1]){
    if(fqx[i,]$x %in% goldx4$x){x<-x+2}
    else if(fqx[i,]$x %in% silverx4$x){x<-x+1}
  }
  return(x)
}

prof$score<-sapply(prof$id, score)
save(prof, file="final/profScore")
View(prof)
median(prof[which(prof$nugget=="silver"),]$score)
s<-sapply(prof$score, function(x) if(x>0){return(1)} else{return(0)})
c<-sapply(s, function(x,y) if((x==1 && y!="none") || (x==0 && y=="none")){return(1)} else{return(0)},y=prof$nugget)
sum(c)/length(c)
#lookup function to return t/f word in intersect list
lookup = function(word){
  if(word %in% intersect) {return(1)}
  else if(word %in% stopwords("english")) {return(1)}
  else {return(0)}
}




#remove intersect from each
goldx <- count(gold)
goldx <- goldx[order(-goldx$freq),]
goldx$of = sapply(goldx$x, function(x) lookup(as.character(x)))

silverx <- count(silver)
silverx <- silverx[order(-silverx$freq),]
silverx$of = sapply(tolower(silverx$x), function(x) lookup(as.character(x)))

nonex <- count(none)
nonex <- nonex[order(-nonex$freq),]
nonex$of = sapply(nonex$x, function(x) lookup(as.character(x)))


# CREATING THE WORDCLOUD
words = getFreqCountNugget("silver")
words = sapply(words, function(x) gsub("[[:punct:]]+\\\\n", "", x))
x = count(words)
x = x[order(-x$freq),]
newx = head(x, n = 300)
newx$of = sapply(tolower(newx$x), function(x) lookup(as.character(x)))
View(newx)

wordcloud(head(x[which(newx$of < 1 & nchar(as.character(x$x)) > 1),], n = 40)$x,
            head(x[which(newx$of < 1 & nchar(as.character(x$x)) > 1),], n = 40)$freq,
            colors=brewer.pal(8, "Dark2"))
