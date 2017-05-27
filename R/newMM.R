
# EXPERIMENTAL

#  modified version of findYdotsMM() etc.
#  
#  in the original model 
#  
#     Y_ij = mu + alpha_i + beta_j + eps 
#     
#  break down alpha_i:
#  
#     alpha_i = sum_k phi_i x_ik + gamma_i

#  that means

#      E(Y_ij | X_ik, k = 1,...,p) = sum_k phi_i x_ik

#  so the phi can be estimated by lm() without a constant term

# arguments:

#   ratingsIn: input data, with first cols (userID,itemID,rating,
#              covariates); data frame, unless cls is non-null, in which
#              case this argument is the quoted name of the distributed 
#              data frame

# value:

#   S3 class of type "ydots", with components:

#      Y..: grand mean (0 if have covariates)
#      Yi.: vector of mean ratings for each user
#      Y.j: vector of mean ratings for each item
#      regOjb: if have covariates, regression output, e.g. coefs

findNewMM <- function(ratingsIn,regressYdots=FALSE) {
  users <- ratingsIn[,1]
  items <- ratingsIn[,2]
  ratings <- ratingsIn[,3]
  nms <- names(ratingsIn)
  Y.. <- mean(ratings) 
  Yi. <- tapply(ratings,users,mean) # means of all ratings per user
  Y.j <- tapply(ratings,items,mean) # means of all ratings per item
  ydots <- list(grandMean=Y..,usrMeans=Yi.,itmMeans=Y.j)
  haveCovs <- ncol(ratingsIn) > 3
  if (haveCovs) {
     # regress, no constant term; could do a weighted least squares,
     # using the Ni, but since the latter is random too, not needed
     frml <- as.formula(paste(nms[3],'~ .-1'))
     lmout <- lm(frml,data=ratingsIn[,-(1:2)])
     ydots$lmout <- lmout
     # fits = lmout$fitted.values
     # ratings = ratings - fits
     # Y.. = 0
     Ni <- tapply(ratings,users,length) # number of ratings per user
     ydots$Ni <- Ni
  } 
  class(ydots) = 'newMM'
  invisible(ydots)
}

# alias
trainNewMM <- findNewMM 

# predict() method for the 'ydots' class
#
# arguments
#
#    testSet: data frame in same form as ratingsIn above except that there 
#             is no ratings column; thus covariates, if any, are shifted
#             leftward one slot, i.e. userID, itemID, cov1, cov2...
#    newMMObj: the output of newMM()
#    minN:  if Ni < minN and have covariates, use the latter
#
# returns vector of predicted values for testSet
predict.newMM = function(newMMObj,testSet,minN=0) {
   haveCovs <- ncol(testSet) > 2
   # use of as.character() is to take advantage of row names, in case of
   # future gaps in consecutive user IDs; not implemented yet
   ts1 <- as.character(testSet[,1])
   if (!haveCovs) tmp <- newMMObj$usrMeans[ts1] else {
      tmp <- ifelse (
                newMMObj$Ni[ts1] >= minN,
                newMMObj$usrMeans[ts1],
                predict(newMMObj$lmout,testSet[,-(1:2),drop=FALSE]))
   }
   testSet$pred <- tmp +
      newMMObj$itmMeans[as.character(testSet[,2])] - newMMObj$grandMean
   testSet$pred
}

# test case
checkydNew <- function() {
   check <- data.frame(
      userID <- c(1,3,2,1,2),
      itemID <- c(1,1,3,2,3),
      ratings <- 6:10)
   names(check) <- c('u','i','r')
   print(check)
   print(findNewMM(check))
   check$cv <- c(1,4,6,2,10)
   names(check)[4] <- 'x'
   print(check)
   cout <- findNewMM(check)
   print(cout)
   testset <- check[1:2,-3]
   testset$x <- c(5,8)
   print(predict(cout,testset,2))
}

