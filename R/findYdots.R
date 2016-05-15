
# arguments:

#   ratingsIn: input data, with first cols (userID,itemID,rating,
#              covariates); data frame, unless cls is non-null, in which
#              case this argument is the quoted name of the distributed 
#              data frame
#   regressYdots; if TRUE, apply lm() to the estimated latent factors
#                 (and their product), enabling rating prediction from the
#                 resulting linear function of the factors; currently
#                 only implemented if have no covariates
#   cls: an R 'parallel' cluster

# value:

#   S3 class of type "ydots", with components:

#      Y..: grand mean (0 if have covariates)
#      Yi.: vector of mean ratings for each user
#      Y.j: vector of mean ratings for each item
#      regOjb: if have covariates, regression output, e.g. coefs

findYdotsMM <- function(ratingsIn,regressYdots=FALSE,cls=NULL) {
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
  }
  ydots = list(grandMean=Y..,usrMeans=Yi.,itmMeans=Y.j)
  if (haveCovs) {
     ydots$regObj = lmout
  } 
  if (regressYdots && !haveCovs) {
       yi. = Yi.[ratingsIn[,1]]
       y.j = Y.j[ratingsIn[,2]]
       # yij = yi. * y.j
       # ydots$regressYdots = coef(lm(ratings ~ yi. + y.j + yij))
       ydots$regressYdots = coef(lm(ratings ~ yi. + y.j))
  }
  
  class(ydots) = 'ydotsMM'
  invisible(ydots)
} 

# check
checkyd <- function() {
   check <- 
      data.frame(userID = c(1,3,2,1,2),itemID = c(1,1,3,2,3),ratings=6:10)
   print(check)
   print(findYdotsMM(check))
   check$cv <- c(1,4,6,2,10)
   print(check)
   print(findYdotsMM(check))
}

# predict from output from coef(lm()); not used currently
predict.lmcoef = function(coefs,testSet) {
   cbind(1,as.matrix(testSet)) %*% as.vector(coefs)
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

