# extract data
index<-!is.na(review.new$nug)
 dat<-review.new[index,c("nug","review")]
 head(dat)
######### make dictionary
texts<-dat$review
## vector, each element is a review

## try tm pakage
require(tm)
require(Snowball)

corpus3<-Corpus(VectorSource(texts))
## do some standard processes of cleaning data

corpus3 <- tm_map(corpus3,tolower)
corpus3 <- tm_map(corpus3, removeWords, stopwords("english"))

corpus3 <- tm_map(corpus3,stripWhitespace)
corpus3 <- tm_map(corpus3,removePunctuation)

# generate the document-term matrix
dtm3 <-DocumentTermMatrix(corpus3)
term3<-colnames(dtm3)

tabu3<- tm_term_score(dtm3,term,slam::col_sums)

# generate the count for each words in dict

sort.dict3<-sort(tabu3,decreasing=TRUE)
dict3 <- names(sort.dict3)

sort.dict3[1:5]



######### classifier
attach(dat)
nug0<-nug=="none"
nug1<-nug=="gold"
set.seed(80)
nug0.mat <- dtm3[nug0,]
nug1.mat<-dtm3[nug1,]

#doc.weight<-sqrt(r.nug+1)
# doc.weight<-doc.weight[nug1]


n<-length(nug)
log.prior0<-log(sum(nug0)/n)
log.prior1<-log(sum(nug1)/n)


train0<-sample(1:nrow(nug0.mat),floor(nrow(nug0.mat)/2))
train1<-sample(1:nrow(nug1.mat),floor(nrow(nug1.mat)/2))


nug0.mat.train<-nug0.mat[train0,]
nug0.mat.test<-nug0.mat[-train0,]

nug1.mat.train<-nug1.mat[train1,]
nug1.mat.test<-nug1.mat[-train1,]

# doc.weight<-doc.weight[train1] # change weight



nug0.log.p<-make.log.pvec(nug0.mat.train,mu)
nug0.log.pmat <- as.simple_triplet_matrix(nug0.log.p)

nug1.log.p<-make.log.pvec(nug1.mat.train,mu)
nug1.log.pmat <- as.simple_triplet_matrix(nug1.log.p)


## try the naive bayes algorithm

get.nug.label<-function(dtm.test){
  nr <- nrow(dtm.test)
  
  log.prior0.mat <- as.simple_triplet_matrix(rep(log.prior0,nr))
  log.prior1.mat <- as.simple_triplet_matrix(rep(log.prior1,nr))
  
  log.p0<-matprod_simple_triplet_matrix(dtm.test,nug0.log.pmat)+log.prior0.mat
  log.p1<-matprod_simple_triplet_matrix(dtm.test,nug1.log.pmat)+log.prior1.mat
  
  log.p<-cbind(log.p0,log.p1)
  label<-rowapply_simple_triplet_matrix(log.p, which.max)
}

train0<-get.nug.label(nug0.mat.train)
train1<-get.nug.label(nug1.mat.train)
test0<-get.nug.label(nug0.mat.test)
test1<-get.nug.label(nug1.mat.test)

table(test0)["1"]/length(test0)
table(test1)["2"]/length(test1)
table(train0)["1"]/length(train0)
table(train1)["2"]/length(train1)

(false.positive<-table(train0)["2"]/(table(train1)["2"]+table(train0)["2"]))
(false.negative<-table(train1)["1"]/(table(train1)["1"]+table(train0)["1"]))
(true.positive <-table(train1)["2"]/(table(train1)["2"]+table(train0)["2"]))
(true.negative <- table(train0)["1"]/(table(train1)["1"]+table(train0)["1"]))


(false.positive<-table(test0)["2"]/(table(test1)["2"]+table(test0)["2"]))
(false.negative<-table(test1)["1"]/(table(test1)["1"]+table(test0)["1"]))
(true.positive <-table(test1)["2"]/(table(test1)["2"]+table(test0)["2"]))
(true.negative <- table(test0)["1"]/(table(test1)["1"]+table(test0)["1"]))

