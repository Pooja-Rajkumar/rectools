
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
#   k: number of nearest neighbors
#   wtcovs: as in predict.usrData
#   wtcats: as in predict.usrData

# value:

#    accuracy value

xvalCos <- function(ratingsIn,usrCovs=NULL,itmCats=NULL,
   k,wtcovs=NULL,wtcats=NULL,
   trainprop=0.5,accmeasure=c('exact','mad','rms'))
{
   # split into random training and validation sets 
   nrowRatIn = nrow(ratIn)
   numRows = floor(trainprop * nrowRatIn)
   trainIdxs = sample(1:nrowRatIn,numRows)
   trainingSet = ratIn[trainIdxs, ]
   trainRatings = trainingSet[,3]
   trainItems = trainingSet[,2]
   trainUsers = trainingSet[,1]
   testSet = ratIn[setdiff(1:nrowRatIn,trainIdxs),]
   # now set up training set for cosine analysis
   trainData <- formUserData(trainingSet,usrCovs,itmCats)
   # for each user i in the test data, find the items rated by user i in
   # the test data, then "predict" them
   testData <- formUserData(testSet,usrCovs,itmCats)
   preds <- c(NULL,NULL)
   for (l in 1:length(testData)) {
      oneNewDatum <- testData[[l]]
      i <- ond$userID
      itms <- ond$itms
      for (j in 1:length(itms)) {
         saveRat <- itms[j]
         itms[j] <- 0
         predVal <- predict(trainData,oneNewDatum,saveRat)
         preds <- rbind(preds,c(predVal,saveRat)
         itms[j]
      }
   }



   predOneNewDatum <- function(oneNewDatum) 
      predict(oneNewDatum)   

  
  numpredna = sum(is.na(testSet$pred)) # calculate accuracy 
  accmeasure = match.arg(accmeasure)
  result = list(ndata=nrowRatIn,trainprop=trainprop,
     accmeasure=accmeasure,numpredna=numpredna)
  if (accmeasure == 'exact') {
     testSet$pred = round(testSet$pred)
     acc = mean(testSet$pred == testSet[,3],na.rm=TRUE)
  } else if (accmeasure == 'mad') {
     acc = mean(abs(testSet$pred-testSet[,3]),na.rm=TRUE)
  } else if (accmeasure == 'rms') {
     acc = sqrt(mean((testSet$pred-testSet[,3])^2,na.rm=TRUE))
  }
  result$acc = acc
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


