# First get the department name from the nested list 
nickname = c("new", "character", "vector")         # Create a new character vector
for(i in 1:length(course[,4])){
  if(class(course[i,4][[1]][1][[1]])!="NULL"){     #Check if a department name is specified
      nickname[i] = course[i,4][[1]][[1]]$name}    #Get it from the list
  else{nickname[i] = "NULL"}                       #Set to NULL if not specified
}
course[,4] = nickname                              #put it in the dataframe
course$departments = as.factor(course$departments) #turn it into a factor

# A function that returns the name of the department when given the id
getDep <- function(id){
  x = unlist(course$id)
  index = match(id, x)
  return(as.character(course$departments[index]))
}

# Now for each review, get the department name from the ID (if it exists)
x <- rep(NA,length(review$courses))
for(i in 1:length(review$courses))              # Create a vector of course IDs
   {tmp = review$courses[i]
    tmp = tmp[[1]][1]
    if(class(tmp)=="list") {x[i] = -1}
    else {x[i]  =tmp}
   }

x = unlist(x) 
deps = sapply(x, getDep)         # Apply the function to get the department name
deps = as.factor(deps)                          # Add it as a collumn

