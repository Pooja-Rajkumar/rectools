
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
  require(lme4)
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
     } else {
        frml <- paste(nms[3],'~ ')
        for (i in 4:ncol(ratingsIn)) {
           frml <- paste(frml,nms[i])
           # if (i < ncol(ratingsIn)) 
           frml <- paste(frml,'+')
        }
        frml <- paste(frml,'(1|',nms[1],')',
                         '+ (1|',nms[2],')')
        frml <- as.formula(frml)
    }
    lmerout = lmer(frml,data=ratingsIn)
    ydots = list()
    if (!haveCovs) {
       Y.. = fixef(lmerout)
       ydots$Y.. = Y..
       clm = coef(lmerout)
       Yi. = clm[[nms[1]]][,1]
       names(Yi.) = as.character(unique(ratingsIn[,1]))
       ydots$Yi. = Yi.
       Y.j = clm[[nms[2]]][,1]
       names(Y.j) = as.character(unique(ratingsIn[,2]))
       ydots$Y.j = Y.j
    } else {
       ydots$Y.. = fixef(lmerout)
       ydots$Yi. = ranef(lmerout)
    }
  } else {
     stop('parallel feature not implemented yet')
  }
  class(ydots) = 'ydotsMLE'
  invisible(ydots)
}

# predict() method for the 'ydotsMLE' class
#
# testSet in same form as ratingsIn in findYdots(), except that there 
# is no ratings column; regObj is as in the output of findYdots()
#
# returns vector of predicted values for testSet
predict.ydotsMLE <- function(ydotsObj,testSet) {
   testSet$pred = ydotsObj$usrMeans[as.character(testSet[,1])] +
      ydotsObj$itmMeans[as.character(testSet[,2])] - ydotsObj$grandMean
   if (!is.null(ydotsObj$regObj))
      testSet$pred = testSet$pred +
         predict(ydotsObj$regObj,testSet[,-(1:2)])
   testSet$pred
}

# check
checkydmle <- function() {
   check <- 
      data.frame(userID = c(1,3,2,1,2),itemID = c(1,1,3,2,3),ratings=6:10)
   print(check)
   print(findYdotsMLE(check))
   check$cv <- c(1,4,6,2,10)
   print(check)
   print(findYdotsMLE(check))
}


