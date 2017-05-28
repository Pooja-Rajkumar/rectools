
# splits input data into training and test sets, fits "ydots" model on
# the former, then predicts the latter

# arguments:

#   ratingsIn: input data, with first cols (userID,itemID,rating,
#              covariates); data frame, unless cls is non-null, in which
#              case this argument is the quoted name of the distributed 
#              data frame
#   trainprop: proportion of data for the training set
#   accmeasure: accuracy measure; 'exact', 'mad', 'rms' for
#               prop of exact matches, mean absolute error, and
#               root-mean square error
#   regressYdots; if TRUE, apply lm() to the estimated latent factors
#                 (and their product), enabling rating prediction from the
#                 resulting linear function of the factors; currently
#                 only implemented if have no covariates
#   minN: see findYdotsMM.R

# value:

#    accuracy value

xvalMM <- function(ratingsIn, trainprop=0.5,
    regressYdots=FALSE,minN=0){
  ratIn = ratingsIn 
  # split into random training and validation sets 
  nrowRatIn = nrow(ratIn)
  rowNum = floor(trainprop * nrowRatIn)
  trainIdxs = sample(1:nrowRatIn,rowNum)
  trainingSet = ratIn[trainIdxs, ]
  trainRatings = trainingSet[,3]
  trainItems = trainingSet[,2]
  trainUsers = trainingSet[,1]
  # get means
  means = findYdotsMM(trainingSet,regressYdots)
  # Y.. = means$grandMean
  # Yi. = means$usrMeans
  # Y.j = means$itmMeans
  testIdxs = setdiff(1:nrowRatIn,trainIdxs)
  testA = ratIn[testIdxs,]
  testA$pred = predict(means,testA[,-3],minN)  # predict.ydotsMM
  # need to integrate the following into predict.ydotsMM
  if (regressYdots) {
     yi. = means$usrMeans[testA[,1]]
     y.j = means$itmMeans[testA[,2]]
     testA$pred = cbind(1,yi.,y.j) %*% means$regressYdots 
  }
  numpredna = sum(is.na(testA$pred))
  # calculate accuracy 
  result = list(ndata=nrowRatIn,trainprop=trainprop,numpredna=numpredna)
     testA$pred = round(testA$pred)
  # accuracy measures
  exact <- mean(round(testA$pred) == testA[,3],na.rm=TRUE)
  mad <- mean(abs(testA$pred-testA[,3]),na.rm=TRUE)
  rms= sqrt(mean((testA$pred-testA[,3])^2,na.rm=TRUE))
  # if just guess mean
  meanRat <- mean(testA[,3],na.rm=TRUE)
  overallexact <- 
     mean(round(meanRat) == testA[,3],na.rm=TRUE)
  overallmad <- mean(abs(meanRat-testA[,3]),na.rm=TRUE)
  overallrms <- sd(testA[,3],na.rm=TRUE)  
  result$acc <- list(exact=exact,mad=mad,rms=rms,
     overallexact=overallexact,
     overallmad=overallmad,
     overallrms=overallrms)
  result$idxs <- testIdxs
  result$preds <- testA$pred
  result$actuals <- testA[,3]
  class(result) <- 'xvalb'
  result
}

# check
checkxv <- function(trainprop=0.5,acc='mad') {
   check <- 
      data.frame(userID = c(1,3,2,1,2,3),itemID = c(1,1,3,2,3,3),ratings=5:10)
   print(check)
   print(xvalMM(check,trainprop,acc))
   check$cv = c(1,2,8,6,3,3)  # covariate
   print(check)
   print(xvalMM(check,trainprop,acc))
}


