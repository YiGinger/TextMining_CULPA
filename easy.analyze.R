frequency <- tapply(review.df$id,review.df$pr,length)
hist(frequency)
sort.freq<-sort(frequency,decreasing=TRUE)
top.pop.professor.id<-names(sort.freq)[1:5]
firstn<-as.character(prof.df$f.name)
lastn <- as.character(prof.df$l.name)
nug<-as.character(prof.df$nug)
get.prof.info<-function(id){
  index<-which(prof.df$p.id==id)
  if(length(index)!=1) return(rep(NA,4))
  re<-c(firstn[index],lastn[index],frequency[id],nug[index])
  names(re)<-c("first name","last name","review number","nug")
  return (re)
}

dept.name<-as.character(course.df$dept.name)

get.dept.name<-function(course.id){
  index<-which(course.df$id==course.id)
  if(length(index)!=1) return(NA)
  return (dept.name[index])
}
top.prof<-sapply(top.pop.professor.id,get.prof.name)
print(top.prof)
top.prof<-data.frame(t(top.prof))
pro.info<-sapply(review.df$pr,get.prof.info)
firstn.pro<-pro.info[1,]
lastn.pro<-pro.info[2,]
nug.pro<-pro.info[4,]
review.new<-transform(review.df,prof.first.name=firstn.pro,prof.last.name=lastn.pro,nug=nug.pro)
dept<-sapply(review.new$courses,get.dept.name)
review.new<-transform(review.new,dept.name=dept)

## try to use google to visualize it
library(googleVis)
Motion=gvisMotionChart(Fruits, idvar="Fruit", timevar="Year", options=list(height=350, width=400))
# Display chart
plot(Motion) 
# Create Google Gadget
cat(createGoogleGadget(Motion), file="motionchart.xml")

##plot my babble
require(plyr)
nug.info<-review.new[,c("dept.name","nug")]
get.nug.tab<-function(x) table(x$nug)
nug.tab<-ddply(nug.info,.(dept.name),get.nug.tab)
nug.tab<-nug.tab[!is.na(nug.tab$dept.name),]
nug.tab<-nug.tab[nug.tab$dept.name!=0,]
nug.tab$size=rep(0.1,nrow(nug.tab))


####-----google-------
require(googleVis)
Bubble <- gvisBubbleChart(nug.tab[-12,], xvar="none", yvar="gold",
                          colorvar="dept.name",
                          options=list(hAxis='{minValue:75, maxValue:125}',
                                       hAxis.textPosition='none',
                                       width=800, height=600))
plot(Bubble)


######-----rCharts-------
library(rCharts)
r1 <- rPlot(gold ~ none, 
            data = nug.tab[-12,],  
            type = "point",
            size = list(const= 4),
            tooltip='#! function(item) { return item.dept_name } !#',
)
r1
r1$save("rCharts1.html",cdn=TRUE)
# Standalone


##-----------------Stemming------------
require(SnowballC)
require(stringr)
texts<-as.character(review.df$review)
texts.only<- str_replace_all(texts,"[[:punct:]]","")
## delete all the punctuation charaters
sen<-texts.only[1]

doc2words <- function(document) str_split(document," ")
text.words<-sapply(texts.only,doc2words)

text.stem<-sapply(text.words,wordStem)
dict<-unlist(text.words)


