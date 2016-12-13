 
# arguments:

#   ratingsIn: input data, with cols (userID,itemID,rating,
#              covariates); data frame
#   cls: an R 'parallel' cluster

# value:

#   ydotsMLE (if NULL cls): S3 class with components:

#      Y..: grand mean (fixef() if have covariates)
#      Yi.: vector of mean ratings for each user
#           (part of ranef() if have covariates)
#      Y.j: vector of mean ratings for each item
#           (part of ranef() if have covariates)

#   ydotsMLEpar (if non-NULL cls): S3 class with components of class ydotsMLE

findYdotsMLE <- function(ratingsIn,cls=NULL) {
  require(lme4)
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
        frml <- paste(frml,'+')
     }
     frml <- paste(frml,'(1|',nms[1],')',
                      '+ (1|',nms[2],')')
     frml <- as.formula(frml)
  }
  if (is.null(cls)) {
     lmerout = lmer(frml,data=ratingsIn)
     ydots = formYdots(ratingsIn,nms,haveCovs,lmerout)
  } else {
     require(partools)
     clusterEvalQ(cls,require(lme4))
     distribsplit(cls,'ratingsIn')
     clusterExport(cls,c('frml','formYdots'),envir=environment())
     clusterExport(cls,c('nms','haveCovs'),envir=environment())
     clusterEvalQ(cls,lmerout <- lmer(frml,data=ratingsIn))
     ydots = clusterEvalQ(cls,formYdots(ratingsIn,nms,haveCovs,lmerout))
     class(ydots) = 'ydotsMLEpar'
  }
  invisible(ydots)
}

# alias
trainMLE <- findYdotsMLE

formYdots = function(ratingsIn,nms,haveCovs,lmerout) {
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
     tmp = ranef(lmerout)
     ydots$Yi. = tmp[[2]][1][,1]
     ydots$Y.j = tmp[[1]][1][,1]
  }
  class(ydots) = 'ydotsMLE'
  ydots
}

# predict() method for the 'ydotsMLE' class
#
# testSet in same form as ratingsIn in findYdots(), except that there 
# is no ratings column
#
# returns vector of predicted values for testSet
predict.ydotsMLE <- function(ydotsObj,testSet) {
   if (ncol(testSet) == 2) Y.. = ydotsObj$Y.. else {
      Y.. = - as.matrix(cbind(1,testSet[,-(1:2)])) %*% ydotsObj$Y..
   }
   testSet$pred = ydotsObj$Yi.[testSet[,1]] +
                  ydotsObj$Y.j[testSet[,2]] - Y..
   testSet$pred
}

# predict() method for the 'ydotsMLE' class
predict.ydotsMLEpar <- function(ydotsMLEparObj,testSet) {
   predict.testSet <- 
      function(ydotsMLEObj) predict(ydotsMLEObj,testSet)
   tmp = lapply(ydotsMLEparObj,predict.testSet)
   tmp = Reduce(cbind,tmp)
   rowMeans(tmp,na.rm=TRUE)
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


