
# arguments:

#   ratingsIn: raw data, with each row of the form (user, item,
#      rating,...), where the ellipsis indicates optional covariates;
#      later prediction presumes the same input format, minus the
#      ratings column

# value:

#   S3 class of type "ydots", with components:

#      Y..: grand mean (0 if have covariates)
#      Yi.: vector of mean ratings for each user
#      Y.j: vector of mean ratings for each item
#      regOjb: if have covariates, object of class 'lm' from
#              the regression op

findYdots <- function(ratingsIn) {
  users = ratingsIn[,1]
  items = ratingsIn[,2]
  ratings = ratingsIn[,3]
  nms <- names(ratingsIn)
  haveCovs = ncol(ratingsIn) > 3
  if (haveCovs) {
     frml = as.formula(paste(nms[3],'~ .'))
     lmout = lm(frml,data=ratingsIn[,-(1:2)])
     fits = lmout$fitted.values
     ratings = ratings - fits
     Y.. = 0
  } else Y.. = mean(ratings) 
  Yi. = tapply(ratings,users,mean) # means of all ratings per user
  Y.j = tapply(ratings,items,mean) # means of all ratings per item
  ydots = list(grandMean=Y..,usrMeans=Yi.,itmMeans=Y.j)
  if (haveCovs) ydots$regObj = lmout
  class(ydots) = 'ydots'
  ydots
} 

# check
checkyd <- function() {
   check <- 
      data.frame(userID = c(1,3,2,1,2),itemID = c(1,1,3,2,3),ratings=6:10)
   print(check)
   print(findYdots(check))
   check$cv <- c(1,4,6,2,10)
   print(check)
   print(findYdots(check))
}


