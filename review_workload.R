
#Require Packages#
library(tm)
library(RWeka)
library(SnowballC)
library(tau)
library(wordcloud)
library(RColorBrewer)
library(proxy)
library(ggplot2)
#Workload Text Data and Cleaning#
workload<-review$workload
head(workload)
class(workload)
workload<-unlist(workload)

workload<-tolower(workload)  
workload<-removePunctuation(workload)
workload<-sub(pattern = "([[:digit:]])(\\s)([[:alpha:]])", replacement ="\\1\\3", x=workload) #remove space between digit and alpha
workload<-gsub(pattern="final\\sexams",replacement="final",x=workload)
workload<-gsub(pattern="final\\sexam",replacement="final",x=workload)
workload<-gsub(pattern="not\\s",replacement="not",x=workload)
workload<-gsub(pattern="no\\s",replacement="no",x=workload)   
workload<-removeWords(workload, stopwords("english")) #remove stopwords
workload<-stripWhitespace(workload)  
head(workload)
#Term Document Matrix#
corpus<-Corpus(VectorSource(workload))
BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 1, max = 1))
tdm <- TermDocumentMatrix(corpus, control = list(tokenize = BigramTokenizer))

wordfreq50<-findFreqTerms(tdm,lowfreq=50) #delete words with frequencies less than 50
tdm50<-tdm[wordfreq50,]
tdm50.m<-as.matrix(tdm50)
termFrequency=rowSums(tdm50.m)

#Wordcloud#
wordcloud(words=names(termFrequency),freq=termFrequency,min.freq=5,max.words=100,
          random.order=F,rot.per=.15, colors=brewer.pal(8,"Dark2"))

#Generate Dictionary#
freq.m<-data.frame(termFrequency)
freq.m$word<-row.names(freq.m)
freq.m$freq<-freq.m[,1]
freq.m<-freq.m[,2:3]
freq.sort<-freq.m[order(-freq.m[,2]),]
dictionary<-c("final","midterm","papers","reading","sets","easy","short","weekly",
"readings","midterms","pages","essays","quizzes","assignments","homework","exams",
"essay","lot","hard","long","read","tests","fair","bad","reserch","presentation",
"difficult","discussion","light","good","lots","writing","project","heavy",
"worth","exam","standard","bad","fairly","nottoo","material","manageable","takehome",
"interesting","little","2papers","topic","assignment","extra","reasonable","inclass",
"group","quiz","review","optional","lab","2midterms","oral","written","dropped","homeworks",
"sometimes","notbad","test","lowest","notes","fine","nothing","exercises","least","doable",
"tough","compositions","3papers","multiple","moderate","report","reports","many","postings",
"fun","articles","nomidterm","better","recitation","helpful","challenging","practice",
"cumulative","analysis","projects","easier","normal","small","concert","ofen","5page",
"mandatory","typical","medium","harder","presentations","drafts","best","prepared",
"texts","posts","annoying","big","quizes","impossible","nocumulative","beginning","biweekly",
"easily","seminar","decent","3midterms","simple","assigns","finals","memorize","harshly","tons",
"regular","1midterm","consuming","article","nofinal","journal","comments","nograded","graded",
"grades","notgraded","labs","reviews","midtermfinal","post","museum","discussion","concert",
"nothard","passage","discussed","large","film","large","creative","intense","boring","huge",
"notmuch","2page","3essays","2essays","timeconsuming","poems","rewrite","shorter",
"notnecessary","nohomework","posting","4essays","poem","3tests","stupid","harsh","rewrites",
"movies","conversation","noteasy","managable","proposal","notdifficult","unnecessary",
"composition","poetry","translation","brief","notvery","novels","2exams","notreally",
"experiment","hws","heavily","literature","materials","love","lighter","worst","heavier",
"not impossible","worthwhile")


tdm.d <- DocumentTermMatrix(corpus, list(dictionary = dictionary))


#clustering#
tmd.d.m<-as.matrix(tdm.d)
train<-tmd.d.m[1:7540,]
dstance <- dist(train, method = "euclidean")
fit <- hclust(dstance, method="ward")
tree<-cutree(fit,3)
t1<-subset(train,tree==1)
t2<-subset(train,tree==2)
t3<-subset(train,tree==3)

freq.t1<-colSums(t1)
freq.t2<-colSums(t2)
freq.t3<-colSums(t3)

p1<-wordcloud(words=names(freq.t1),freq=freq.t1,min.freq=5,max.words=100,
          random.order=F,rot.per=.15, colors=brewer.pal(8,"Dark2"))
p2<-wordcloud(words=names(freq.t2),freq=freq.t2,min.freq=5,max.words=100,
          random.order=F,rot.per=.15, colors=brewer.pal(8,"Dark2"))
#Generate Score of Workload#
dictionary.lowwl<-c("easy","short","essay","fair","discussion","light","good","worth","standard",
                    "fairly","nottoo","manageable","interesting","little","reasonable","optional","notbad",
                    "lowest","fine","nothing","least","doable","tough","moderate","fun","nomidterm","helpful",
                    "normal","small","medium","nocumulative","easily","decent","simple","regular","nofinal",
                    "nograded","notgraded","nothard","notmuch","shorter","notnecessary","nohomework",
                    "managable","notdifficult","brief","notvery","notreally","not impossible","worthwhile")
                    
dictionary.highwl <-c( "final","midterm","papers","reading","sets","weekly","readings",
                    "midterms","pages","essays","quizzes","assignments","homework","exams","lot","hard",
                    "long","read","tests","bad","reserch","presentation","difficult","lots","writing",
                    "project","heavy","exam","bad","material","2papers","topic","assignment","extra",
                    "quiz","review","lab","2midterms","oral","written","homeworks","test","exercises",
                    "compositions","3papers","multiple","report","reports","many","postings","articles",
                    "recitation","challenging","practice","cumulative","analysis","projects","concert","ofen",
                    "mandatory","presentations","drafts","texts","posts","annoying","big","quizes",
                    "impossible","biweekly","seminar","3midterms","assigns","finals","memorize","harshly","tons",
                    "1midterm","consuming","article","journal","comments","graded",
                    "grades","labs","reviews","midtermfinal","post","museum","discussion",
                    "passage","discussed","film","large","intense","boring","huge","2page","3essays","2essays",
                    "timeconsuming","poems","rewrite","posting","4essays","poem","3tests","stupid","harsh","rewrites",
                    "movies","conversation","noteasy","proposal","unnecessary","composition","poetry","translation",
                    "novels","2exams","experiment","hws","heavily","literature","materials","worst")


score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
{
  require(plyr)
  require(stringr)
  # we got a vector of sentences. plyr will handle a list or a vector as an "l" for us
  # we want a simple array of scores back, so we use "l" + "a" + "ply" = laply:
  scores = laply(sentences, function(sentence, pos.words, neg.words) {
    # split into words. str_split is in the stringr package
    word.list = str_split(sentence, '\\s+')
    # sometimes a list() is one level of hierarchy too much
    words = unlist(word.list)
    # compare our words to the dictionaries of positive & negative terms
    pos.matches = match(words, pos.words)
    neg.matches = match(words, neg.words)
    # match() returns the position of the matched term or NA
    # we just want a TRUE/FALSE:
    pos.matches = !is.na(pos.matches)
    neg.matches = !is.na(neg.matches)
    # and conveniently enough, TRUE/FALSE will be treated as 1/0 by sum():
    score = sum(pos.matches) - sum(neg.matches)
    return(score)
  }, pos.words, neg.words, .progress=.progress )
  scores.df = data.frame(score=scores, text=sentences)
  return(scores.df)
}
  

result <- score.sentiment(workload, dictionary.highwl, dictionary.lowwl)
colnames(result)<-c("wl.score","text")

#Workload Score Analysis#
 #histogram of workload
review<-cbind(result$wl.score,review)
colnames(review)<-c("wl.score", colnames(review[2:10]))
ggplot(review, aes(x=wl.score))+geom_density()
ggplot(review, aes(x=wl.score))+geom_histogram()



getworkloadscore<-function(x) {
  p=prof[which(prof$nugget==x),]
  df = review[which(review$id %in% unlist(p$review_ids)),1]
  return(df)
}
a<-rep("gold",894)
gold <-data.frame(getworkloadscore("gold"))
gold<-cbind(gold,a)
colnames(gold)<-c("score","nugget")
b<-rep("silver",8827)
silver <-data.frame(getworkloadscore("silver"))
silver<-cbind(silver,b)
colnames(silver)<-c("score","nugget")
c<-rep("none",10078)
none <- data.frame(getworkloadscore("none"))
none<-cbind(none,c)
colnames(none)<-c("score","nugget")
total <- rbind(gold,silver,none)
ggplot(total,aes(x=score,group=factor(nugget)))+geom_density(aes(col=factor(nugget)))
ggplot(total,aes(x=score,group=factor(nugget)))+geom_histogram(aes(fill=factor(nugget), alpha=.5))
