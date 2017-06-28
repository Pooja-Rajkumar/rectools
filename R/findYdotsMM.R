
#  modified version of earlier findYdotsMM() etc., May 27, 2017, with
#  new approach to use of covariates U_ijk for user i, item j;
#  NOTE: the covariates will be centered

#  basic model:
#  
#     Y_ij = 
#        mu + sum_k gamma_k U_ijk + alpha_i + beta_j + eps 
#     
#  that means

#      E(Y_ij | U_ik, V_jk) = sum_k gamma_k U_ik + sum_k delta_k V_jk

# and the coefficients can be estimated via lm() without a const term

# arguments:

#   ratingsIn: input data, with cols (userID,itemID,rating,
#              covariates); data framee

# value:

#   S3 class of type "ydots", with components:

#      Y..: grand mean, est of mu 
#      Yi.: vector of mean ratings for each user, ests. of alpha_i
#      Y.j: vector of mean ratings for each item, ests. of betaa_j
#      regObj: if have covariates, regression output, e.g. coefs

findYdotsMM <- function(ratingsIn,regressYdots=FALSE) {
  users <- ratingsIn[,1]
  items <- ratingsIn[,2]
  # IMPORTANT NOTE:
  # user and item IDs may not be consecutive; even if they are
  # consecutive in the original, if we do cross-validation, this 
  # may not be the case; so switch to character IDs
  users <- as.character(users)
  items <- as.character(items)
  ratings <- ratingsIn[,3]
  nms <- names(ratingsIn)
  Y.. <- mean(ratings) 
  Yi. <- tapply(ratings,users,mean) # means of all ratings per user
  Y.j <- tapply(ratings,items,mean) # means of all ratings per item
  ydots <- list(grandMean=Y..,usrMeans=Yi.,itmMeans=Y.j)
  haveCovs <- ncol(ratingsIn) > 3
  if (haveCovs) {
     # center the covs
     tmp <- scale(ratingsIn[,-(1:3)],scale=FALSE)
     ratingsIn[,-(1:3)] <- tmp
     ydots$covmeans <- attr(tmp,'scaled:center')
     # regress, no constant term; could do a weighted least squares,
     # using the Ni, but since the latter is random too, not needed
     frml <- as.formula(paste(nms[3],'~ .-1'))
     lmout <- lm(frml,data=ratingsIn[,-(1:2)])
     ydots$lmout <- lmout
     Ni <- tapply(ratings,users,length) # number of ratings per user
     ydots$Ni <- Ni
  } 
  class(ydots) = 'ydotsMM'
  invisible(ydots)
}

# alias
trainMM <- findYdotsMM 

# predict() method for the 'ydotsMM' class

# in predicting for user i, the code looks at N_i, the number of ratings
# by user i; if that number is below minN, the prediction comes from
# user i's covariate information instead of from Yi.

# arguments

#    testSet: data frame in same form as ratingsIn above except that there 
#             is no ratings column; thus covariates, if any, are shifted
#             leftward one slot, i.e. userID, itemID, cov1, cov2...
#    ydotsObj: the output of findYdotsMM()
#    minN:  if Ni < minN and have covariates, use the latter instead of Yi;
#           see above

# returns vector of predicted values for testSet
predict.ydotsMM = function(ydotsObj,testSet,minN=0) {
   haveCovs <- ncol(testSet) > 2
   # see comment on as.character() above
   ts1 <- as.character(testSet[,1])  # user IDs, char form
   # tmp will basically consist of the user means, except that in the
   # covariate case some will be replaced by predict.lm() values + Y..
   if (!haveCovs) {
      tmp <- ydotsObj$usrMeans[ts1] 
   }
   else {
      colmeans <- ydotsObj$covmeans
      testSet[,-(1:2)] <- 
         scale(testSet[,-(1:2)],center=colmeans,scale=FALSE)
      tmp <- 
         ifelse (ydotsObj$Ni[ts1] >= minN,
             ydotsObj$usrMeans[ts1],
             predict(ydotsObj$lmout,testSet[,-(1:2),drop=FALSE]) +
                ydotsObj$grandMean
         )
   }
   testSet$pred <- tmp +
      ydotsObj$itmMeans[as.character(testSet[,2])] - ydotsObj$grandMean
   testSet$pred
}

# test case
checkyd <- function() {
   check <- data.frame(
      userID <- c(1,3,2,1,2),
      itemID <- c(1,1,3,2,3),
      ratings <- 6:10)
   names(check) <- c('u','i','r')
   print(check)
   print(findYdotsMM(check))
   check$cv <- c(1,4,6,2,10)
   names(check)[4] <- 'x'
   print(check)
   cout <- findYdotsMM(check)
   print(cout)
   testset <- check[1:2,-3]
   testset$x <- c(5,8)
   print(predict(cout,testset,2))
}



predict.NM = function(ydotsObj,testSet) {
  testSet$pred = ydotsObj$usrMeans[as.character(testSet[,1])] + 
    ydotsObj$itmMeans[as.character(testSet[,2])] - ydotsObj$grandMean
#  if (!is.null(ydotsObj$regObj))
 #   testSet$pred = testSet$pred + predict(ydotsObj$regObj,testSet[,-(1:2)])
  testSet$pred
}

getDiff <- function(fullMatrix, filledNMF){
  indicies <- which(!is.na(fullMatrix), arr.ind = TRUE)
  diff <- matrix(data = NA,nrow = nrow(indicies), 1)
  for(i in 1:nrow(indicies))
    for(j in 1:ncol(indicies)){
      diff[i,1] <- round(fullMatrix[indicies[i,1],indicies[j,2]]) - filledNMF[indicies[i,1],indicies[j,2]]
    }
  
  diff
}


getAcc <- function(fullMatrix,filledNMF, threshold = 0.5) {
  diff <- abs(getDiff(fullMatrix, filledNMF))
  avgDiff <- colMeans(abs(diff), na.rm = TRUE, dims =1) # average difference is 1.19 
  # Remove the na's from diff 
  # Calculate the values less than the threshold 
  count = 0;
  tester <- na.exclude(diff)
  for(i in 1:nrow(diff))
  {
    if(diff[i] < 0.5 )
      count = count + 1
  }
  acc <- count/ nrow(diff)
  acc
  
}

train.NM <- function(ratingsIn, trainprop = 0.5,cls = NULL,
                    rnk = 10, recosystem = FALSE,regressYdots=FALSE)
  {
  require(NMF)
  library(NMF)
  if(recosystem == TRUE){
    source((paste(system.file(package = 'rectools'),
                  'recosys/xvalRecoParallel.R', sep = "")))
    res <- xvalReco(ratingsIn,trainprop,cls,rnk)
  }
  else {
    origMatrix <- buildMatrix(ratingsIn) # Matrix A (Step 1)
    fullMatrix <- origMatrix
    approxMatrix <- findYdotsMM(ratingsIn) # Matrix V (Step 2)
    
    naMatrix <- as.data.frame(which(is.na(fullMatrix) == TRUE, arr.ind = TRUE))
    naMatrix$ratings <- NA
    
    preds <- predict.NM(approxMatrix,naMatrix) # Step 3

    fullMatrix[which(is.na(fullMatrix))] <- preds 
    fullMatrix[fullMatrix < 0] <- 0
    require(NMF)
    result <- nmf(as.matrix(fullMatrix),10) # Step 4
    filledNMF <- as.matrix(result@fit@W) %*% as.matrix(result@fit@H)
    avgd <- getAcc(fullMatrix, filledNMF) # 1.149 for the movielens 100k data 
    print(avgd)
    
  }
  filledNMF
}


buildMatrix <- function(ratingsIn,NAval=NA){
  # deal with possible factors
  dmax <- function(d) {
    if (is.factor(d)) return(length(levels(d)))
    max(d)
  }
  users = ratingsIn[,1]
  movies = ratingsIn[,2]
  rating = ratingsIn[,3]
  newMatrix = matrix(NAval, 
                     nrow = dmax(users), ncol = dmax(movies))
  for(rows in 1:nrow(ratingsIn)){
    newMatrix[ratingsIn[rows,1],ratingsIn[rows,2]] = ratingsIn[rows,3]
  }
  return (newMatrix)
}

