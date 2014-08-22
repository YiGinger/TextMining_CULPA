require(wordcloud)
#GET NUMBER OF WORDS PER REVIEW
x = unlist(review$review)
y = sapply(x, function(x) sapply(gregexpr("\\W+", x), length) + 1)
y = as.numeric(y)


#GETTING AVERAGE AND MAX NUMBER OF REVIEWS BY DEPARTMENT
averages = tapply(y, review$department, mean)

bymax = tapply(y, review$department, max)
namesInBymax = names(bymax)
values = c(1,2,3,4)
for (i in 1:length(namesInBymax)){values[i] = bymax[[i]]}
maxes = cbind(namesInBymax, values)
maxes[,2] = as.numeric(maxes[,2])
maxes = data.frame(maxes)
maxes[order(values),]

#GET A LIST OF MOST REPRESENTED DEPARTMENTS
summary = summary(review$department)
names = names(summary)
values = c(1,2,3,4)
for (i in 1:length(summary)){values[i] = summary[[i]]}
summary = cbind(names, values)
summary[,2] = as.numeric(summary[,2])
summary = data.frame(summary)
mostRepresented = head(summary[order(-values),], n = 20)
mostRepresented = mostRepresented[-10,]

#GET DATA ONLY FOR MOST REPRESENTED DEPARTMENTS
reviewMostRepresented = review[which(review$department %in% mostRepresented$names),]
reviewMostRepresented$department = factor(reviewMostRepresented$department)

#GET NUMBER OF WORDS PER SENTENCE FOR EACH REVIEW
x = unlist(reviewMostRepresented$review)
wordNum = sapply(x, function(x) sapply(gregexpr("\\W+", x), length) + 1)
wordNum = as.numeric(wordNum)
reviewMostRepresented$wordLength = wordNum
#GET AVERAGE WORD LENGTH
x = unlist(reviewMostRepresented$review)
sentLength = c(1,2,3,4)
for (i in 1:length(x)){sentLength[i] = nchar(x[i])}
reviewMostRepresented$avWord = (sentLength /reviewMostRepresented$wordLength)
#PLOTTING IT
library(ggplot2)
qplot(reorder(department, avWord), avWord, data = reviewMostRepresented, geom=c("boxplot")) + 
  theme(axis.text.x=element_text(angle=-45)) + ylim(4.5,6.5) + xlab("department") + 
  ylab("average word length")

for(i in 1:length(reviewMostRepresented$review)){ maxWordLength[i] = 
    max(nchar(strsplit(reviewMostRepresented$review[[i]], " ")[[1]]))}
maxWordLength[maxWordLength==-Inf] <- 0

maxWords = tapply(reviewMostRepresented$maxWord, reviewMostRepresented$department, mean)
names = names(maxWords)
values = c(1,2,3)
for (i in 1:length(names)){values[i] = maxWords[[i]]}
maxWords = cbind(names, values)
maxWords = data.frame(maxWords)
maxWords$values = as.character(maxWords$values)
maxWords$values = as.numeric(maxWords$values)
maxWords[order(values),]

#ACTUALLY GET THE LARGEST WORD OF EACH REVIEW
largestWord = c('a', 'b')
for(i in 1:length(reviewMostRepresented$review)){ largestWord[i] = 
   max(strsplit(reviewMostRepresented$review[[i]], " ")[[1]]
   [which.max(nchar(strsplit(reviewMostRepresented$review[[i]], " ")[[1]]))], 0)
}
largestWord = sapply(largestWord, function(x) gsub("[[:punct:]]+\\\\n", "", x))
reviewMostRepresented$largestWord = largestWord

x = count(reviewMostRepresented[which(reviewMostRepresented$department=="Anthropology"),]$largestWord)
x = x[order(-x$freq),]
x = x[1:40,]
x = x[-which(x=="0"),]

wordcloud(x$x, x$freq)





# FUNCTION THAT RETURNS A LIST OF ALL THE WORDS IN REVIEWS FOR THE DEPARTMENT
getFreqCount = function(x){
  df = reviewMostRepresented[which(reviewMostRepresented$department==x),]
  x = strsplit(df$review[[1]], " ")[[1]]
  for(i in 2:dim(df)[1]){
    y = strsplit(df$review[[i]], " ")[[1]]
    x = c(x, y)
  }
  return(x)
}

#CREATE AND FORMAT LIST OF MOST USED WORDS IN ANTHRO for example
words = getFreqCount("Anthropology")
words = sapply(words, function(x) gsub("[[:punct:]]+\\\\n", "", x))
x = count(words)
x = x[order(-x$freq),]
x$f = x$freq/length(words)
head(x[which(nchar(as.character(x$x)) > 6),], n = 40)

#NOW FOR ALL OF THE REVIEWS, SO ALL WORDS USED ON CULPA
x = strsplit(reviewMostRepresented$review[[1]], " ")[[1]]
 for(i in 2:dim(reviewMostRepresented)[1]){
       y = strsplit(reviewMostRepresented$review[[i]], " ")[[1]]
       x = c(x, y)
 }
b = x
b = sapply(b, function(x) gsub("[[:punct:]]+\\\\n", "", x))
b = count(b)
b = b[order(-b$freq),]
b$f = b$freq/2868190

common = head(b, n = 250)
common = common$x
common = as.character(common)

lookup = function(word){
     if(word %in% common) {return(1)}
     else {return(0)}
  }

# CREATING THE WORDCLOUD
words = getFreqCount("Anthropology")
words = sapply(words, function(x) gsub("[[:punct:]]+\\\\n", "", x))
x = count(words)
x = x[order(-x$freq),]
newx = head(x, n = 200)
newx$of = sapply(newx$x, function(x) lookup(as.character(x)))

wordcloud(head(x[which(newx$of < 1 & nchar(as.character(x$x)) > 1),], n = 40)$x,
      head(x[which(newx$of < 1 & nchar(as.character(x$x)) > 1),], n = 40)$freq,
            colors=brewer.pal(8, "Dark2"))
