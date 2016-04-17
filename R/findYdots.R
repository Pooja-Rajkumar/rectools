
# arguments:

#   ratingsIn: raw data, with each row of the form (user, item, rating)
#   keepUsrs: if TRUE, keep split by users
#   keepItms: if TRUE, keep split by items

# value:

#   S3 class of type "ydots", with components:

#      Y..: grand mean
#      Yi.: vector of mean ratings for each user
#      Y.j: vector of mean ratings for each item
#      grpByUsrs: a list showing all the items rated by each user
#      grpByItms: a list showing all the users who  rated each item

findYdots <- function(ratingsIn,keepUsrs=FALSE,keepItms=FALSE){
  users = ratingsIn[,1]
  items = ratingsIn[,2]
  ratings = ratingsIn[,3]
  Y.. = mean(ratings) # this is Y..
  if (keepUsrs) {
     grpByUsrs = split(items,users)
     Yi. = sapply(grpByUsrs,mean)
  } else
     Yi. = tapply(ratings,users,mean) # means of all ratings per user
  if (keepItms) {
     grpByItms = split(users,items)
     Y.j = sapply(grpByItms,mean)
  } else
     Y.j = tapply(ratings,items,mean) # means of all ratings per item
  ydots = list(GrandMean=Y..,UsrMeans=Yi.,ItmMeans=Y.j)
  if (keepUsrs) ydots$grpByUsrs = grpByUsrs
  if (keepItms) ydots$grpByItms = grpByItms
  class(ydots) = 'ydots'
  ydots
} 

# check
checkyd <- function() {
   check <- 
      data.frame(userID = c(1,3,2,1,2),itemID = c(1,1,3,2,3),ratings=6:10)
   print(check)
   print(findYdots(check))
   print(findYdots(check,keepUsrs=T))
   print(findYdots(check,keepItms=T))
}


