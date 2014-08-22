require(rjson)
#prof <- fromJSON(file="/Users/apple/Downloads/professors.json")
#review <- fromJSON(file="/Users/apple/Downloads/reviews.json") 

prof_url <- "http://s3.amazonaws.com/culpa/professors.json"
review_url <- "http://s3.amazonaws.com/culpa/reviews.json"
# prof <- fromJSON(file=prof_url, method='C')
# review <- fromJSON(file=review_url, method='C')
courses <- fromJSON(file="/volumes/bootcamp/documents/team20/final/dump/courses new.json")
prof <- fromJSON(file="/volumes/bootcamp/documents/team20/final/dump/professors new.json")
review <- fromJSON(file="/volumes/bootcamp/documents/team20/final/dump/reviews new.json")
## Can someone change it into url? Or just change the path locally to read the data
require(plyr)


dep <- llply(courses,function(x) x$departments  )
ff<- function(x){
  if (length(x)==0) {return (0) 
  }else return (x[[1]]$name)
}
dept.name <- sapply(dep,ff)
c.id <- sapply(courses,function(x) x$id)
course.df <- data.frame(id=c.id,dept.name=dept.name)
d.name <- sapply(review,function(x) course.df[which(course.df$id==x$courses),"dept.name"])
d <- sapply(d.name,function(x) as.character(x))
r.dept.name <- unlist(d)
# review.df <- cbind(review.df,r.dept.name)


nugget <- sapply(prof,function(x) x$nugget)
f.name <- sapply(prof, function(x) x$first_name)
l.name <- sapply(prof, function(x) x$last_name)
m.name <- sapply(prof,function(x) x$middle_name)
p.id <- sapply(prof, function(x) x$id)
prof.df <- data.frame(nug=nugget,f.name=f.name,l.name=l.name,p.id=p.id)

r.id <- unlist(sapply(review,function(x) x$id))
r.pr <- unlist(sapply(review,function(x) x$professors))
r.fun <- unlist(sapply(review,function(x) x$funny))
r.wl <- unlist(sapply(review, function(x) x$workload))
r.disg<-unlist(sapply(review, function(x) x$disagree))
r.g <- unlist(sapply(review, function(x) x$agree))
r.review <- unlist(sapply(review, function(x) x$review))
r.courses <- unlist(sapply(review, function(x) x$courses))
r.date<- unlist(sapply(review, function(x) x$date))

review.df <- cbind(id=r.id,pr=r.pr,fun=r.fun,disagree=r.disg,agree=r.g,workload=r.wl,
                   review=r.review,courses=r.courses,date=r.date)
review.df <- data.frame(review.df)

