
require(tm)
require(Snowball)

make.log.pvec <- function(dtm,mu,doc.weight=rep(1,nrow(dtm))){
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

##---------two classes result------
fun0<-r.g==0
fun1<-r.g>0
set.seed(80)
fun0.mat <- dtm3[fun0,]
fun1.mat<-dtm3[fun1,]

#doc.weight<-sqrt(r.fun+1)
# doc.weight<-doc.weight[fun1]


n<-length(r.fun)
log.prior0<-log(sum(fun0)/n)
log.prior1<-log(sum(fun1)/n)


train0<-sample(1:nrow(fun0.mat),floor(nrow(fun0.mat)/2))
train1<-sample(1:nrow(fun1.mat),floor(nrow(fun1.mat)/2))


fun0.mat.train<-fun0.mat[train0,]
fun0.mat.test<-fun0.mat[-train0,]

fun1.mat.train<-fun1.mat[train1,]
fun1.mat.test<-fun1.mat[-train1,]

# doc.weight<-doc.weight[train1] # change weight



fun0.log.p<-make.log.pvec(fun0.mat.train,mu)
fun0.log.pmat <- as.simple_triplet_matrix(fun0.log.p)

fun1.log.p<-make.log.pvec(fun1.mat.train,mu)
fun1.log.pmat <- as.simple_triplet_matrix(fun1.log.p)


#log.pmat.fun<-fun.logp.mat
#log.pmat.notfun<-notfun.logp.mat
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


# the error in training set is very small, it means that this model is seriously overfitting.

> (false.positive<-table(train0)["2"]/(table(train1)["2"]+table(train0)["2"]))
2 
0.02953874 
> (false.negative<-table(train1)["1"]/(table(train1)["1"]+table(train0)["1"]))
1 
0.09051037 
> (true.positive <-table(train1)["2"]/(table(train1)["2"]+table(train0)["2"]))
2 
0.9704613 
> (true.negative <- table(train0)["1"]/(table(train1)["1"]+table(train0)["1"]))
1 
0.9094896 
> 
  > 
  > (false.positive<-table(test0)["2"]/(table(test1)["2"]+table(test0)["2"]))
2 
0.2003827 
> (false.negative<-table(test1)["1"]/(table(test1)["1"]+table(test0)["1"]))
1 
0.654937 
> (true.positive <-table(test1)["2"]/(table(test1)["2"]+table(test0)["2"]))
2 
0.7996173 
> (true.negative <- table(test0)["1"]/(table(test1)["1"]+table(test0)["1"]))
1 
0.345063 


###--------weight=sqrt+1, delete 50 words-----
> (false.positive<-table(test0)["2"]/(table(test1)["2"]+table(test0)["2"]))
2 
0.1996585 
> (false.negative<-table(test1)["1"]/(table(test1)["1"]+table(test0)["1"]))
1 
0.6541237 
> (true.positive <-table(test1)["2"]/(table(test1)["2"]+table(test0)["2"]))
2 
0.8003415 
> (true.negative <- table(test0)["1"]/(table(test1)["1"]+table(test0)["1"]))
1 
0.3458763 
############----weight =sqrt+1, change the way to cut vector fun---

