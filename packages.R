# Some code to investigate number of dependencies for R packages
# Joseph Rickert
# Get package infromation from CRAN
require(stringr)
data<- as.data.frame(available.packages(),stringsAsFactors=FALSE)
head(p)

pkgs <- data.frame(data[,c(1,4)])                  # Pick out Package names and Depends

#pkgs <- pkgs[complete.cases(pkgs[,2]),]         # Remove NAs
pkgs.nod<-pkgs[is.na(pkgs[,2]),]

pkgs$Depends2 <-strsplit(pkgs$Depends,",")      # split list of Depends

######-------------my version------------
pp<-mat.or.vec(nrow(pkgs),length(d.pkg))
rownames(pp)<-pkgs$Packagesao
colnames(pp)<-d.pkg
rownames(pkgs)<-pkgs$Package
#sparse.model.matrix(matrix(0,nrow(pkgs),nrow(pkgs)))
dep<-unlist(pkgs$Depends2)
dep<-str_extract(dep,"[[:alpha:]][[:alnum:]]+")
dep<-dep[is.na(dep)]
freq<-table(dep)
freq.d<-sort(freq,decreasing=TRUE)
  

d.pkg<-names(freq.d)
for(i in 1:nrow(pkgs)){
  #for(i in 1:10){
  pkg.name<-pkgs$Package[i];print(pkg.name)
  BY<-unlist(pkgs[pkg.name,"Depends2"])
  BY<-str_extract(BY,"[[:alpha:]][[:alnum:]]+")
  BY<-BY[!is.na(BY)]
  if (length(BY)>0){
    print(BY)
  pp[pkg.name,BY]<-1
  }
}
obj<-d.pkg[1:20]
no.dep<-pkgs$Package[!is.na(pkgs$Package)]
nod.obj<-setdiff(obj,pkgs$Package)# be depended but not included in the Package 
chord<-as.data.frame(matrix(0,20,20)) ## if pkg i are depended on pkg j, then chord[i,j]=1
colnames(chord)<-obj 
rownames(chord)<-obj
chord[nod.obj,]

