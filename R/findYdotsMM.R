
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

# alias
trainMM <- findYdotsMM 

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
# buildMatrix(): A function that changes ratingsIn into a mostly empty matrix upon which we fill 
buildMatrix <- function(ratingsIn,NAval=NA){
  # deal with possible factors
  dmax <- function(d) {
    if (is.factor(d)) return(length(levels(d)))
    max(d)
  }
  users = ratingsIn[,1]
  movies = ratingsIn[,2]
  rating = ratingsIn[,3]
  newMatrix = matrix(NA, 
                     nrow = dmax(users), ncol = dmax(movies))
  for(rows in 1:nrow(ratingsIn)){
    newMatrix[ratingsIn[rows,1],ratingsIn[rows,2]] = ratingsIn[rows,3]
  }
  return (newMatrix)
}

# Wrapper for recosystem 
# TrainNM will check if recosystem is called, if so, call recosystem function 
# If not, build ratingsIn into a mostly matrix which would be users x items dimensions
# Approximate the unknown ratings using findYdotsMM 
# Substitute those values and call NMF()
trainNM <- function(ratingsIn, trainprop = 0.5,cls = NULL,
                    rnk = 10, recosystem = FALSE,regressYdots=FALSE){
  require(NMF)
  library(NMF)
  if(recosystem == TRUE){
    source((paste(system.file(package = 'rectools'),
                  'recosys/xvalRecoParallel.R', sep = "")))
    res <- xvalReco(ratingsIn,trainprop,cls,rnk)
  }
  else {
    fullMatrix <- buildMatrix(ratingsIn) # Matrix A (Step 1)
    fullMatrix[which(fullMatrix == 0)] = NA 
    approxMatrix <- findYdotsMM(ratingsIn) # Matrix V (Step 2)
    
    naMatrix <- as.data.frame(which(is.na(fullMatrix) == TRUE, arr.ind = TRUE))
    naMatrix$ratings <- NA
  
     
    preds <- predict.ydotsMM(approxMatrix,naMatrix) # Step 3
  
    
    
    fullMatrix[which(is.na(fullMatrix))] <- preds 
    fullMatrix[fullMatrix < 0] <- 0
    require(NMF)
    res <- nmf(as.matrix(fullMatrix),10) # Step 4
  }
  res
}

