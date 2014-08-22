library(stringr)

reviewMostRepresented$funny = as.numeric(reviewMostRepresented$funny)
numberOfPapers = reviewMostRepresented$funny

papers <- "((([[:digit:]]+)([[:space:]]+))(paper))"

function(review){
  toReturn = NA;
  str = str_extract(review, papers)
  if(!is.na(str)){
    toReturn = substr(str, 0, 1)
    toReturn = as.numeric(toReturn)
  }   
  return(toReturn)    
}

numberOfPapers = sapply(reviewMostRepresented$workload, function(x) getNumPapers(x))
reviewMostRepresented$papers = numberOfPapers

averages = tapply(numberOfPapers, reviewMostRepresented$department, function(x) mean(x, na.rm = TRUE))
names = names(averages)
values = c(1,2,3)
for (i in 1:length(names)){values[i] = averages[[i]]}
averages = cbind(names, values)
averages = data.frame(averages)
averages = averages[order(values),]

averages$values = as.character(averages$values)
averages$values = as.numeric(averages$values)

qplot(reorder(names, values), values, data = averages, geom=c("bar")) + 
  theme(axis.text.x=element_text(angle=-45)) + xlab("department") + 
  ylab("average number of papers")







function(review){
  toReturn = NA;
  str = str_extract(review, midterms)
  if(!is.na(str)){
    toReturn = substr(str, 0, 1)
    toReturn = as.numeric(toReturn)
  }   
  return(toReturn)    
}

qplot(reorder(department, papers), papers, data = reviewMostRepresented, geom=c("jitter")) + 
     theme(axis.text.x=element_text(angle=-45)) + xlab("department") + 
      ylab("average word length")
