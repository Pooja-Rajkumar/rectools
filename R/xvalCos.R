
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
  # split into random training and validation sets 
  nrowRatIn = nrow(ratIn)
  numRows = floor(trainprop * nrowRatIn)
  trainIdxs = sample(1:nrowRatIn,numRows)
  trainingSet = ratIn[trainIdxs, ]
  trainRatings = trainingSet[,3]
  trainItems = trainingSet[,2]
  trainUsers = trainingSet[,1]
  testA = ratIn[setdiff(1:nrowRatIn,trainIdxs),]
  # now set up training set for cosine analysis
  trainData <- formUserData(trainingSet,usrCovs,itmCats)
  getOneNewDatum <- function() {

  }
  testA$pred = sapply(trainData,
  
  predict(means,testA[,-3])  # predict.ydotsMM
  numpredna = sum(is.na(testA$pred))
  # calculate accuracy 
  accmeasure = match.arg(accmeasure)
  result = list(ndata=nrowRatIn,trainprop=trainprop,
     accmeasure=accmeasure,numpredna=numpredna)
  if (accmeasure == 'exact') {
     testA$pred = round(testA$pred)
     acc = mean(testA$pred == testA[,3],na.rm=TRUE)
  } else if (accmeasure == 'mad') {
     acc = mean(abs(testA$pred-testA[,3]),na.rm=TRUE)
  } else if (accmeasure == 'rms') {
     acc = sqrt(mean((testA$pred-testA[,3])^2,na.rm=TRUE))
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


