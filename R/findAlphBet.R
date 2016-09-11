
# arguments:

#   ratingsIn: input data, with first cols (userID,itemID,rating,
#              covariates); data frame, unless cls is non-null, in which
#              case this argument is the quoted name of the distributed 
#              data frame

# value:

#   S3 class of type "alphbet", with components:

#      alph: the alpha_i
#      bet: the beta_j

findYdotsAlphBet <- function(ratingsIn) {
   users = ratingsIn[,1]
   items = ratingsIn[,2]
   ratings = ratingsIn[,3]
   nms <- names(ratingsIn)
   # find 
   userrated  <- split(items,users)
   itemratedby <- split(users,items)
   class(ydots) = 'ydotsAlphBet'
   invisible(ydots)
} 

# predict() method for the 'ydots' class
#
# testSet in same form as ratingsIn in findYdots(), except that there 
# is no ratings column; regObj is as in the output of findYdots()
#
# returns vector of predicted values for testSet
predict.ydotsMM = function(ydotsObj,testSet) {
   testSet$pred = ydotsObj$usrMeans[as.character(testSet[,1])] + 
      ydotsObj$itmMeans[as.character(testSet[,2])] - ydotsObj$grandMean
   if (!is.null(ydotsObj$regObj))
      testSet$pred = testSet$pred + predict(ydotsObj$regObj,testSet[,-(1:2)])
   testSet$pred
}

