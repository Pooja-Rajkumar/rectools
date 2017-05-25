
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

findYdotsMM <- function(ratingsIn,regressYdots=FALSE) {
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
     # regress, no constant term
     frml <- as.formula(paste(nms[3],'~ .-1'))
     lmout <- lm(frml,data=ratingsIn[,-(1:2)])
     # fits = lmout$fitted.values
     # ratings = ratings - fits
     # Y.. = 0
     Ni <- tapply(ratings,users,length) # means of all ratings per user
     ydots$phi <- coef(lmout)
     ydots$Ni <- coef(lmout)
  } 
  class(ydots) = 'newMM'
  invisible(ydots)
}

# alias
trainNewMM <- newMM 

# test case
checkyd <- function() {
   check <- data.frame(
      userID <- c(1,3,2,1,2),
      itemID <- c(1,1,3,2,3),
      ratings <- 6:10)
   print(check)
   print(newMM(check))
   check$cv <- c(1,4,6,2,10)
   print(check)
   print(fnewMM(check))
}

# predict() method for the 'ydots' class
#
# arguments:
#
#    testSet: data frame in same form as ratingsIn above except that there 
#             is no ratings column; thus covariates, if any, are shifted
#             leftward one slot
#    newMMObj: the output of newMM()
#    minN:  if Ni < minN and have covariates, use the latter
#
# returns vector of predicted values for testSet
predict.newMM = function(newMMObj,testSet,minN=NULL) {
   haveCovs <- ncol(testSet) > 2
   # use of as.character() is to take advantage of row names, in case of
   # future gaps in consecutive user IDs; not implemented yet
   ts1 <- as.character(testSet[,1])
   tmp <- ifelse (
      newMMObj$Ni[ts1] >= minN,
      newMMObj$usrMeans[ts1],
   testSet$pred = newMMObj$usrMeans[as.character(testSet[,1])] + 
      newMMObj$itmMeans[as.character(testSet[,2])] - newMMObj$grandMean
   testSet$pred
}


