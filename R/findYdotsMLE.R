
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

findYdotsMLE <- function(ratingsIn,cls=NULL) {
  require(partools)
  if (is.null(cls)) {
     users = ratingsIn[,1]
     items = ratingsIn[,2]
     ratings = ratingsIn[,3]
     nms <- names(ratingsIn)
     haveCovs = ncol(ratingsIn) > 3
     if (!haveCovs) {
        tmp = sprintf('%s ~ (1|%s) + (1|%s)',
           nms[3],nms[1],nms[2])
        frml = as.formula(paste(tmp))
        lmerout = lmer(frml,data=ratingsIn)
        Y.. = fixef(lmerout)
        clm = coef(lmerout)
        Yi. = clm[[nms[1]]][,1]
        Y.j = clm[[nms[2]]][,1]
     } 
  } else {
     stop('parallel feature not implemented yet')
  }
  ydots = list(grandMean=Y..,usrMeans=Yi.,itmMeans=Y.j)
  if (haveCovs) ydots$regObj = lmout
  class(ydots) = 'ydots'
  invisible(ydots)
} 

# check
checkyd <- function() {
   check <- 
      data.frame(userID = c(1,3,2,1,2),itemID = c(1,1,3,2,3),ratings=6:10)
   print(check)
   print(findYdotsMLE(check))
        browser()
   # check$cv <- c(1,4,6,2,10)
   # print(check)
   # print(findYdotsMLE(check))
}


