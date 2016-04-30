
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
  if (is.null(cls)) {
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
  } else {
     stop('parallel version under construction')
     # find haveCovs
     cmd = sprintf('ncol(%s)',ratingsIn)
     # clusterExport(cls,'cmd',envir=environment())
     # haveCovs = clusterEvalQ(cls,docmd(cmd))[[1]] > 3
     haveCovs = evalq(cmd) > 3
     # set names at workers
     nms = c('uID','iID','rating')
     cmd = paste('names(',ratingsIn,') <<- nms',sep="")
     clusterExport(cls,c('nms','cmd'),envir=environment())
     clusterEvalQ(cls,docmd(cmd))
     # get means
     cmd = sprintf('tapply(%s[,3],%s[,1],sumnum)',ratingsIn,ratingsIn)
     clusterExport(cls,c('sumnum','cmd'),envir=environment())
     tmp = clusterEvalQ(cls,docmd(cmd))
     tmpcb = Reduce(cbind,tmp)
     Y.. = sum(tmp[,2]*tmp[,3]) / sum(tmp[,3])
     Yi. = tmp[,2]
     Y.j = distribmeans(cls,'rating','iID',ratingsIn)[,2]
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
   print(findYdots(check))
   check$cv <- c(1,4,6,2,10)
   print(check)
   print(findYdots(check))
}

# predict() method for the 'ydots' class
#
# testSet in same form as ratingsIn in findYdots(), except that there 
# is no ratings column; regObj is as in the output of findYdots()
#
# returns vector of predicted values for testSet
predict.ydots <- function(ydotsObj,testSet) {
   testSet$pred = ydotsObj$usrMeans[as.character(testSet[,1])] + 
      ydotsObj$itmMeans[as.character(testSet[,2])] - ydotsObj$grandMean
   if (!is.null(ydotsObj$regObj))
      testSet$pred = testSet$pred + predict(ydotsObj$regObj,testSet[,-(1:2)])
   testSet$pred
}

