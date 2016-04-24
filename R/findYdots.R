
# arguments:

#   ratingsIn: input data, with first cols (userID,itemID,rating,
#              covariates); data frame, unless cls is non-null, in which
#              case this argument is the quoted name of the distributed 
#              data frame
#   cls: an R 'parallel' cluster

# value:

#   S3 class of type "ydots", with components:

#      Y..: grand mean (0 if have covariates)
#      Yi.: vector of mean ratings for each user
#      Y.j: vector of mean ratings for each item
#      regOjb: if have covariates, object of class 'lm' from
#              the regression op

findYdots <- function(ratingsIn,cls=NULL) {
  require(partools)
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
  if (is.null(cls)) {
     Yi. = tapply(ratings,users,mean) # means of all ratings per user
     Y.j = tapply(ratings,items,mean) # means of all ratings per item
  } else {
     cmd = paste('names(',ratingsIn,') = c("uID","iID","rating")',sep="")
browser()
     clusterExport(cls,cmd,env=environment())
     clusterEvalQ(cls,docmd(cmd))
     Yi. = distribmeans(cls,'rating','uID',ratingsIn)
     Y.j = distribmeans(cls,'rating','iID',ratingsIn)
  }
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


