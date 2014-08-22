texts<-r.review
## vector, each element is a review

## try tm pakage
require(tm)
require(Snowball)
corpus<-Corpus(VectorSource(texts))
## do some standard processes of cleaning data
corpus <- tm_map(corpus,tolower)
corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus <- tm_map(corpus,stripWhitespace)
corpus <- tm_map(corpus,removePunctuation)
corpus3 <- tm_map(corpus, removeWords, local.stopwords[1:50])
# generate the document-term matrix
dtm3 <-DocumentTermMatrix(corpus3)
term3<-colnames(dtm3)

tabu3<- tm_term_score(dtm3,term3,slam::col_sums)

# generate the count for each words in dict

sort.dict3<-sort(tabu3,decreasing=TRUE)
dict3 <- names(sort.dict3)

sort.dict3[1:5]
# class    really professor      will  students 
# 58005     14543     13975     12257     12039 

a<-tm_term_score(fun.mat.train, term, FUN = slam::col_sums)
b<-tm_term_score(notfun.mat.train,term,FUN=slam::col_sums)
## the dict is in decresing order

##### Naive Bayes Classifier
make.log.pvec <- function(dtm,mu){
  # Sum up the number of instances per word
  term <- colnames(dtm)
  pvec.no.mu <- tm_term_score(dtm, term, FUN = slam::col_sums)
  # Sum up number of words
  n.words <- sum(pvec.no.mu)
  # Get dictionary size
  dic.len <- length(pvec.no.mu)
  # Incorporate mu and normalize
  log.pvec <- log(pvec.no.mu + mu) - log(mu*dic.len + n.words)
  return(log.pvec)
}

mu <- 1/length(dict2)

## try to classify to three classes: not fun, fun, very fun

## Since according to calculation the 96.8% reviews get 0-4 fun, 
## so we regard the left 3.2%,which get more than 4 fun, as the group"Very Fun"



#-----------------------three classes result-----
fun0<-r.fun<1
fun1<-(r.fun>0)&(r.fun<5)
fun2<-r.fun>4
set.seed(60)
fun0.mat <- dtm[fun0,]
fun1.mat<-dtm[fun1,]
fun2.mat <- dtm[fun2,]

n<-length(r.fun)
log.prior0<-log(sum(fun0)/n)
log.prior1<-log(sum(fun1)/n)
log.prior2<-log(sum(fun2)/n)

train0<-sample(1:nrow(fun0.mat),floor(nrow(fun0.mat)/2))
train1<-sample(1:nrow(fun1.mat),floor(nrow(fun1.mat)/2))
train2<-sample(1:nrow(fun2.mat),floor(nrow(fun2.mat)/2))

fun0.mat.train<-fun0.mat[train0,]
fun0.mat.test<-fun0.mat[-train0,]

fun1.mat.train<-fun1.mat[train1,]
fun1.mat.test<-fun1.mat[-train1,]

fun2.mat.train<-fun2.mat[train2,]
fun2.mat.test<-fun2.mat[-train2,]


fun0.log.p<-make.log.pvec(fun0.mat.train,mu)
fun0.log.pmat <- as.simple_triplet_matrix(fun0.log.p)

fun1.log.p<-make.log.pvec(fun1.mat.train,mu)
fun1.log.pmat <- as.simple_triplet_matrix(fun1.log.p)

fun2.log.p<-make.log.pvec(fun2.mat.train,mu)
fun2.log.pmat <- as.simple_triplet_matrix(fun2.log.p)


#log.pmat.fun<-fun.logp.mat
#log.pmat.notfun<-notfun.logp.mat
## try the naive bayes algorithm
dtm.test <- fun0.mat.test

nr <- nrow(dtm.test)

log.prior0.mat <- as.simple_triplet_matrix(rep(log.prior0,nr))
log.prior1.mat <- as.simple_triplet_matrix(rep(log.prior1,nr))
log.prior2.mat <- as.simple_triplet_matrix(rep(log.prior2,nr))

log.p0<-matprod_simple_triplet_matrix(dtm.test,fun0.log.pmat)+log.prior0.mat
log.p1<-matprod_simple_triplet_matrix(dtm.test,fun1.log.pmat)+log.prior1.mat
log.p2<-matprod_simple_triplet_matrix(dtm.test,fun2.log.pmat)+log.prior2.mat

log.p<-cbind(log.p0,log.p1,log.p2)
label<-rowapply_simple_triplet_matrix(log.p, which.max)

#> predict "not fun": 0.9152399
# prior probability for "not fun": 0.8365236
# predict "very fun" :[1] 0.02240896
# prior probability for "very fun":0.03151938
# predict "fun" : 0.1942398
# prior probability for "fun": 0.131957


##---------two classes result------
fun0<-r.fun<1
fun1<-r.fun>0
set.seed(60)
fun0.mat <- dtm2[fun0,]
fun1.mat<-dtm2[fun1,]


n<-length(r.fun)
log.prior0<-log(sum(fun0)/n)
log.prior1<-log(sum(fun1)/n)


train0<-sample(1:nrow(fun0.mat),floor(nrow(fun0.mat)/2))
train1<-sample(1:nrow(fun1.mat),floor(nrow(fun1.mat)/2))


fun0.mat.train<-fun0.mat[train0,]
fun0.mat.test<-fun0.mat[-train0,]

fun1.mat.train<-fun1.mat[train1,]
fun1.mat.test<-fun1.mat[-train1,]



fun0.log.p<-make.log.pvec(fun0.mat.train,mu)
fun0.log.pmat <- as.simple_triplet_matrix(fun0.log.p)

fun1.log.p<-make.log.pvec(fun1.mat.train,mu)
fun1.log.pmat <- as.simple_triplet_matrix(fun1.log.p)


#log.pmat.fun<-fun.logp.mat
#log.pmat.notfun<-notfun.logp.mat
## try the naive bayes algorithm
dtm.test <- fun1.mat.test

nr <- nrow(dtm.test)

log.prior0.mat <- as.simple_triplet_matrix(rep(log.prior0,nr))
log.prior1.mat <- as.simple_triplet_matrix(rep(log.prior1,nr))


log.p0<-matprod_simple_triplet_matrix(dtm.test,fun0.log.pmat)+log.prior0.mat
log.p1<-matprod_simple_triplet_matrix(dtm.test,fun1.log.pmat)+log.prior1.mat

log.p<-cbind(log.p0,log.p1)
label<-rowapply_simple_triplet_matrix(log.p, which.max)
table(label)
#> predict "not fun":  0.8987529
# prior probability for "not fun": 0.8365236

#> predict " fun":  0.2336398
# prior probability for "fun":  0.1634764


##---------Use different cut------
fun0<-r.fun<5
fun1<-r.fun>4
set.seed(60)
fun0.mat <- dtm2[fun0,]
fun1.mat<-dtm2[fun1,]


n<-length(r.fun)
log.prior0<-log(sum(fun0)/n)
log.prior1<-log(sum(fun1)/n)


train0<-sample(1:nrow(fun0.mat),floor(nrow(fun0.mat)/2))
train1<-sample(1:nrow(fun1.mat),floor(nrow(fun1.mat)/2))


fun0.mat.train<-fun0.mat[train0,]
fun0.mat.test<-fun0.mat[-train0,]

fun1.mat.train<-fun1.mat[train1,]
fun1.mat.test<-fun1.mat[-train1,]



fun0.log.p<-make.log.pvec(fun0.mat.train,mu)
fun0.log.pmat <- as.simple_triplet_matrix(fun0.log.p)

fun1.log.p<-make.log.pvec(fun1.mat.train,mu)
fun1.log.pmat <- as.simple_triplet_matrix(fun1.log.p)


#log.pmat.fun<-fun.logp.mat
#log.pmat.notfun<-notfun.logp.mat
## try the naive bayes algorithm
dtm.test <- fun1.mat.test

nr <- nrow(dtm.test)

log.prior0.mat <- as.simple_triplet_matrix(rep(log.prior0,nr))
log.prior1.mat <- as.simple_triplet_matrix(rep(log.prior1,nr))


log.p0<-matprod_simple_triplet_matrix(dtm.test,fun0.log.pmat)+log.prior0.mat
log.p1<-matprod_simple_triplet_matrix(dtm.test,fun1.log.pmat)+log.prior1.mat

log.p<-cbind(log.p0,log.p1)
label<-rowapply_simple_triplet_matrix(log.p, which.max)
table(label)
#> predict "not fun":  0.8987529
# prior probability for "not fun": 0.8365236

#> predict " fun":  0.2336398
# prior probability for "fun":  0.1634764

