
# splits input data into training and test sets, fits "ydots" model on
# the former, then predicts the latter

# arguments:

#   ratingsIn: input data, with first cols (userID,itemID,rating,
#              covariates); data frame, unless cls is non-null, in which
#              case this argument is the quoted name of the distributed 
#              data frame
#   trainprop: proportion of data for the training set
#   k: number of nearest neighbors
#   wtcovs: as in predict.usrData
#   wtcats: as in predict.usrData

# value:

#    accuracy value

xvalCos <- function(ratingsIn,k,usrCovs=NULL,itmCats=NULL,
   wtcovs=NULL,wtcats=NULL,
   trainprop=0.5)
{
   # split into random training and validation sets 
   nrowRatIn = nrow(ratingsIn)
   numRows = floor(trainprop * nrowRatIn)
   trainIdxs = sample(1:nrowRatIn,numRows)
   trainingSet = ratingsIn[trainIdxs, ]
   trainRatings = trainingSet[,3]
   trainItems = trainingSet[,2]
   trainUsers = trainingSet[,1]
   testSet = ratingsIn[setdiff(1:nrowRatIn,trainIdxs),]
   # now set up training set for cosine analysis
   trainData <- formUserData(trainingSet,usrCovs,itmCats)
   # for each user i in the test data, find the items rated by user i in
   # the test data, then "predict" them
   testData <- formUserData(testSet,usrCovs,itmCats)
   preds <- c(NULL,NULL)
   for (l in 1:length(testData)) {
      oneNewDatum <- testData[[l]]
      for (j in 1:length(oneNewDatum$ratings)) {
         saveRat <- oneNewDatum$ratings[j]
         oneNewDatum$ratings[j] <- 0
         predVal <- predict(trainData,oneNewDatum,saveRat,k)
         preds <- rbind(preds,c(predVal,saveRat))
         oneNewDatum$ratings[j] <- saveRat
      }
   }
  numpredna = sum(is.na(preds[,1])) 
  # calculate accuracy 
  result = list(ndata=nrowRatIn,trainprop=trainprop,numpredna=numpredna)
  roundpreds = round(preds[,1])
  exact = mean(preds[,1] == preds[,2],na.rm=TRUE)
  mad = mean(abs(preds[,1] - preds[,2]),na.rm=TRUE)
  rms = sqrt(mean((preds[,1] - preds[,2])^2,na.rm=TRUE))
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


