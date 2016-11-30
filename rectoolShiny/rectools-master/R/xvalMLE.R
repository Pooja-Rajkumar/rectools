
# splits input data into training and test sets, fits "lme4" model on
# the former, then predicts the latter

# arguments:

#   ratingsIn: input data, with first cols (userID,itemID,rating,
#              covariates)
#   trainprop: proportion of data for the training set
#   cls: if non-null, do this in parallel

# value:

#    accuracy value

xvalMLE <- function(ratingsIn, trainprop=0.5,cls=NULL){
  ratIn = ratingsIn 
  # split into random training and validation sets 
  nrowRatIn = nrow(ratIn)
  rowNum = floor(trainprop * nrowRatIn)
  trainIdxs = sample(1:nrowRatIn,rowNum)
  trainingSet = ratIn[trainIdxs, ]
  # get means
  means = findYdotsMLE(trainingSet,cls)
  testIdxs = setdiff(1:nrowRatIn,trainIdxs)
  testSet = ratIn[testIdxs,]
  testSet$pred = predict(means,testSet[,-3])
  numpredna = sum(is.na(testSet$pred))
  # calculate accuracy 
  result = list(ndata=nrowRatIn,trainprop=trainprop,numpredna=numpredna)
  # accuracy measures
  exact <- mean(round(testSet$pred) == testSet[,3],na.rm=TRUE)
  mad <- mean(abs(testSet$pred-testSet[,3]),na.rm=TRUE)
  rms= sqrt(mean((testSet$pred-testSet[,3])^2,na.rm=TRUE))
  # if just guess mean
  meanRat <- mean(testSet[,3],na.rm=TRUE)
  overallexact <-
     mean(round(meanRat) == testSet[,3],na.rm=TRUE)
  overallmad <- mean(abs(meanRat-testSet[,3]),na.rm=TRUE)
  overallrms <- sd(testSet[,3],na.rm=TRUE)
  result$acc <- list(exact=exact,mad=mad,rms=rms,
     overallexact=overallexact,
     overallmad=overallmad,
     overallrms=overallrms)
  result$idxs <- testIdxs
  result$preds <- testSet$pred
  result$actuals <- testSet[,3]
  class(result) <- 'xvalb'
  result
}

# check
checkxv <- function(trainprop=0.5,acc='mad') {
   check <- 
      data.frame(userID = c(1,3,2,1,2,3),itemID = c(1,1,3,2,3,3),ratings=5:10)
   print(check)
   print(xvalMLE(check,trainprop,acc))
   check$cv = c(1,2,8,6,3,3)
   print(check)
   print(xvalMLE(check,trainprop,acc))
}


