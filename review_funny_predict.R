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

# generate the document-term matrix
dtm <-DocumentTermMatrix(corpus)
term<-colnames(dtm)

tabu<- tm_term_score(dtm,term,slam::col_sums)

# generate the count for each words in dict

sort.dict<-sort(tabu,decreasing=TRUE)
dict <- names(sort.dict)

sort.dict[1:5]
# class    really professor      will  students 
# 58005     14543     13975     12257     12039 


dummy.fun<-(r.fun>0)
fun.mat<-dtm[dummy.fun,]
set.seed(3)
train<-sample(1:nrow(fun.mat),floor(nrow(fun.mat)/2))
fun.mat.train<-fun.mat[train,]
fun.mat.test<-fun.mat[-train,]
notfun.mat <-dtm[!dummy.fun,]
train2 <- sample(1:nrow(notfun.mat),floor(nrow(notfun.mat)/2))
notfun.mat.train<-notfun.mat[train2,]
notfun.mat.test<-notfun.mat[-train2,]
# use fun.mat.train and notfun.mat.train to train

a<-tm_term_score(fun.mat.train, term, FUN = slam::col_sums)
b<-tm_term_score(notfun.mat.train,term,FUN=slam::col_sums)
## the dict is in decresing order
aa<-sort(a,decreasing=TRUE)
bb<- sort(b,decreasing=TRUE)
names(aa[201:240])

names(bb[201:240])
term.temp <-dict[-(1:200)]



##### Naive Bayes Classifier
require(tm)
make.log.pvec <- function(dtm,mu.doc.weight){
  # Sum up the number of instances per word
  term <- colnames(dtm)
  doc.weight <- as.simple_triplet_matrix(doc.weight) 
  crossdtm
  pvec.no.mu <- tm_term_score(dtm, term, FUN = slam::col_sums)
  # Sum up number of words
  n.words <- sum(pvec.no.mu)
  # Get dictionary size
  dic.len <- length(pvec.no.mu)
  # Incorporate mu and normalize
  log.pvec <- log(pvec.no.mu + mu) - log(mu*dic.len + n.words)
  return(log.pvec)
}

mu <- 1/50870
fun.log.p<-make.log.pvec(fun.mat.train,mu)
fun.logp.mat <- as.simple_triplet_matrix(fun.log.p)
notfun.log.p <- make.log.pvec(notfun.mat.train,mu)
notfun.logp.mat <- as.simple_triplet_matrix(notfun.log.p)


log.pmat.fun<-fun.logp.mat
log.pmat.notfun<-notfun.logp.mat
 

## make the log.pvec as simple_triplet_matrix

#### rowapply_simple_triplet_matrix(dtm.test, FUN, ...)
dtm.test<-fun.mat.test
log.prior.fun<-log(sum(r.fun>0)/length(r.fun))
log.prior.notfun<-log(sum(r.fun==0)/length(r.fun))


## try the naive bayes algorithm
dtm.test <- notfun.mat.test

nr <- nrow(dtm.test)
log.p.diff<-matprod_simple_triplet_matrix(dtm.test,log.pmat.fun)-matprod_simple_triplet_matrix(dtm.test,log.pmat.notfun)
log.prior.fun.mat <- as.simple_triplet_matrix(rep(log.prior.fun,nr))
log.prior.notfun.mat <- as.simple_triplet_matrix(rep(log.prior.notfun,nr))
log.p<-log.p.diff+log.prior.fun.mat-log.prior.notfun.mat

#for fun.test
#> sum(log.p>0)
#[1] 398
#> sum(log.p<0)
#[1] 1451
#> 398/nr
# [1] 0.2152515

# for notfun.test
#> sum(log.p>0)
#[1] 870
#> sum(log.p<0)
#[1] 8592
#> 870/nr
#[1] 0.09194673

#> sum(r.fun>0)/length(r.fun)
#The average ratio of fun against all :0.1634764



## try to classify to three classes: not fun, fun, very fun

## Since according to calculation the 96.8% reviews get 0-4 fun, 
## so we regard the left 3.2%,which get more than 4 fun, as the group"Very Fun"
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

get.fun.label<-function(dtm.test){
nr <- nrow(dtm.test)
log.prior0.mat <- as.simple_triplet_matrix(rep(log.prior0,nr))
log.prior1.mat <- as.simple_triplet_matrix(rep(log.prior1,nr))
log.prior2.mat <- as.simple_triplet_matrix(rep(log.prior2,nr))
log.p0<-matprod_simple_triplet_matrix(dtm.test,fun0.log.pmat)+log.prior0.mat
log.p1<-matprod_simple_triplet_matrix(dtm.test,fun1.log.pmat)+log.prior1.mat
log.p2<-matprod_simple_triplet_matrix(dtm.test,fun2.log.pmat)+log.prior2.mat
log.p<-cbind(log.p0,log.p1,log.p2)
label<-rowapply_simple_triplet_matrix(log.p, which.max)
}

train0<-get.fun.label(fun0.mat.train)
train1<-get.fun.label(fun1.mat.train)
train2<-get.fun.label(fun2.mat.train)

test0<-get.fun.label(fun0.mat.test)
test1<-get.fun.label(fun1.mat.test)
test2<-get.fun.label(fun2.mat.test)

(false.positive<-table(test0)["3"]/(table(test1)["3"]+table(test0)["3"]+table(test2)["3"]))
(false.negative<-table(test2)["1"]/(table(test1)["1"]+table(test0)["1"]+table(test2)["1"]))
(true.positive <-table(test2)["3"]/(table(test1)["3"]+table(test0)["3"]+table(test2)["3"]))
(true.negative <- table(test0)["1"]/(table(test1)["1"]+table(test0)["1"]+table(test2)["1"]))




