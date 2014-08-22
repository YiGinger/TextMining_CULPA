
require(tm)
require(Snowball)

make.log.pvec <- function(dtm, mu, doc.weight=rep(1, nrow(dtm)) ){
  # Sum up the number of instances per word
  term <- colnames(dtm)
  dtm<-dtm*doc.weight
  pvec.no.mu <- tm_term_score(dtm, term, FUN = slam::col_sums)
  # Sum up number of words
  n.words <- sum(pvec.no.mu)
  # Get dictionary size
  dic.len <- length(pvec.no.mu)
  # Incorporate mu and normalize
  log.pvec <- log(pvec.no.mu + mu) - log(mu*dic.len + n.words)
  return(log.pvec)
}

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

doc.weight <-r.fun+1

##---------two classes result------
fun0<-r.fun<1
fun1<-r.fun>0
set.seed(80)
fun0.mat <- dtm[fun0,]
fun1.mat<-dtm[fun1,]

 doc.weight<-sqrt(r.fun+1)
 doc.weight<-doc.weight[fun1]


n<-length(r.fun)
log.prior0<-log(sum(fun0)/n)
log.prior1<-log(sum(fun1)/n)


train0<-sample(1:nrow(fun0.mat),floor(nrow(fun0.mat)/2))
train1<-sample(1:nrow(fun1.mat),floor(nrow(fun1.mat)/2))


fun0.mat.train<-fun0.mat[train0,]
fun0.mat.test<-fun0.mat[-train0,]

fun1.mat.train<-fun1.mat[train1,]
fun1.mat.test<-fun1.mat[-train1,]

doc.weight<-doc.weight[train1] # change weight



fun0.log.p<-make.log.pvec(fun0.mat.train,mu)
fun0.log.pmat <- as.simple_triplet_matrix(fun0.log.p)

fun1.log.p<-make.log.pvec(fun1.mat.train,mu)
fun1.log.pmat <- as.simple_triplet_matrix(fun1.log.p)


## try the naive bayes algorithm

get.fun.label<-function(dtm.test){
nr <- nrow(dtm.test)

log.prior0.mat <- as.simple_triplet_matrix(rep(log.prior0,nr))
log.prior1.mat <- as.simple_triplet_matrix(rep(log.prior1,nr))

log.p0<-matprod_simple_triplet_matrix(dtm.test,fun0.log.pmat)+log.prior0.mat
log.p1<-matprod_simple_triplet_matrix(dtm.test,fun1.log.pmat)+log.prior1.mat

log.p<-cbind(log.p0,log.p1)
label<-rowapply_simple_triplet_matrix(log.p, which.max)
}

train0<-get.fun.label(fun0.mat.train)
train1<-get.fun.label(fun1.mat.train)
test0<-get.fun.label(fun0.mat.test)
test1<-get.fun.label(fun1.mat.test)

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


# ---no weight-----------
 
  > (false.positive<-table(test0)["2"]/(table(test1)["2"]+table(test0)["2"]))
2 
0.1977753 
> (false.negative<-table(test1)["1"]/(table(test1)["1"]+table(test0)["1"]))
1 
0.6708315 
> (true.positive <-table(test1)["2"]/(table(test1)["2"]+table(test0)["2"]))
2 
0.8022247 
> (true.negative <- table(test0)["1"]/(table(test1)["1"]+table(test0)["1"]))
1 
0.3291685 


##---- weight = sqrt +1, not delete words--------------------
> (false.positive<-table(train0)["2"]/(table(train1)["2"]+table(train0)["2"]))
2 
0.07950192 
> (false.negative<-table(train1)["1"]/(table(train1)["1"]+table(train0)["1"]))
1 
0.08649912 
> (true.positive <-table(train1)["2"]/(table(train1)["2"]+table(train0)["2"]))
2 
0.9204981 
> (true.negative <- table(train0)["1"]/(table(train1)["1"]+table(train0)["1"]))
1 
0.9135009 

> (false.positive<-table(test0)["2"]/(table(test1)["2"]+table(test0)["2"]))
2 
0.4458945 
> (false.negative<-table(test1)["1"]/(table(test1)["1"]+table(test0)["1"]))
1 
0.08762669 
> (true.positive <-table(test1)["2"]/(table(test1)["2"]+table(test0)["2"]))
2 
0.5541055 
> (true.negative <- table(test0)["1"]/(table(test1)["1"]+table(test0)["1"]))
1 
0.9123733 

####-------------  with weight=2*sqrt-1--------------
> (false.positive<-table(test0)["2"]/(table(test1)["2"]+table(test0)["2"]))
2 
0.6746699 
> (false.negative<-table(test1)["1"]/(table(test1)["1"]+table(test0)["1"]))
1 
0.1506013 
> (true.positive <-table(test1)["2"]/(table(test1)["2"]+table(test0)["2"]))
2 
0.3253301 
> (true.negative <- table(test0)["1"]/(table(test1)["1"]+table(test0)["1"]))
1 
0.8493987 

##---with weight = log------\
> (false.negative<-table(test1)["1"]/(table(test1)["1"]+table(test0)["1"]))
1 
0.1489423 
> (true.positive <-table(test1)["2"]/(table(test1)["2"]+table(test0)["2"]))
2 
0.3204593 
> (true.negative <- table(test0)["1"]/(table(test1)["1"]+table(test0)["1"]))
1 
0.8510577 
####-------with weight= sqrt/2----
> (false.positive<-table(test0)["2"]/(table(test1)["2"]+table(test0)["2"]))
2 
0.6455026 
> (false.negative<-table(test1)["1"]/(table(test1)["1"]+table(test0)["1"]))
1 
0.1460544 
> (true.positive <-table(test1)["2"]/(table(test1)["2"]+table(test0)["2"]))
2 
0.3544974 
> (true.negative <- table(test0)["1"]/(table(test1)["1"]+table(test0)["1"]))
1 
0.8539456 

###------not much differnece about words frequency between fun and not fun reviews
## Here are respectively the most frequent words in "fun reviews" and "not fun reviews"

> logp1[1:20]
class professor      will    really  students       one      take    course      just      like       can 
-3.424887 -4.888033 -4.916035 -5.012867 -5.048118 -5.055087 -5.083458 -5.118849 -5.141463 -5.164600 -5.182973 
get      time      good  lectures      also      even      much  material       way 
-5.204393 -5.248660 -5.371940 -5.381586 -5.409434 -5.455321 -5.469332 -5.550110 -5.648484 
> logp0[1:20]
class    really professor  students      will       one      take    course       can      just       get 
-3.384236 -4.741065 -4.791620 -4.932991 -4.951382 -4.973484 -5.099126 -5.107528 -5.168153 -5.187290 -5.190952 
time      good      also      like  lectures  material      much     great      well 
-5.215893 -5.238171 -5.286000 -5.301684 -5.327482 -5.349348 -5.387525 -5.411413 -5.458228 
