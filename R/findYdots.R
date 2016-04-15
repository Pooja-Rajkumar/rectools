findYdots <- function(ratingsIn){
  users = ratingsIn[,1]
  effect = ratingsIn[,2]
  value = ratingsIn[,3]
  Y.. = mean(value) # this is Y..
  Yi. = tapply(value,users,mean) # this is the mean of all ratings per user
  Y.j = tapply(value,effect,mean) #this is the mean of all ratings per movie
  means <- structure(list(Y..,Yi.,Y.j), class= "means")
  names(means) = c("Overall Mean", "Mean Values per user", "Mean Values per Effect")
  return(means)
} 



