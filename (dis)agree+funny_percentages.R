# 61 percent of reviews are funny
> Reduce("+", review.df$fun) / length(review.df$fun)
[1] 0.614851

#On average 2 people disagree with a review
> Reduce("+", review.df$disagree) / length(review.df$disagree)
[1] 2.065037

#On average ~4 people agree
> Reduce("+", review.df$agree) / length(review.df$agree)
[1] 3.768808

#Percentage of professors with nuggets
> nuggets = unlist(prof.df$nug)
> length(nuggets)
[1] 3027
> summary(nuggets)
gold   none silver 
109   2049    869 

f <- function(a,b) b/a+b
