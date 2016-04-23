
# splits input data into training and test sets, fits "ydots" model on
# the former, then predicts the latter

# arguments:

#   ratingsIn: input data frame, with first cols (userID,itemID,rating)
#   trainprop: proportion of data for the training set
#   accmeasure: accuracy measure

# value:

#    accuracy value

### temporarily call our method the Additive method

xValAdd <- function(ratingsIn, trainprop=0.5,
    accmeasure=c('exact','mad','rms')){
  # split into random training and validation sets 
  nrowRatin = nrow(ratingsIn)
  rowNum = floor(trainprop * nrowRatin)
  trainIdxs = sample(1:nrow(ratingsIn),rowNum)
  trainingSet = ratingsIn[trainIdxs, ]
  trainRatings = trainingSet[,3]
  trainItems = trainingSet[,2]
  trainUsers = trainingSet[,1]
  # get means
  means = findYdots(trainingSet)
  # Y.. = means$grandMean
  # Yi. = means$usrMeans
  # Y.j = means$itmMeans
  testA = ratingsIn[setdiff(1:nrowRatin,trainIdxs),]
  testA$pred = predict(means,testA[,-3])  # predict.ydots
  numpredna = sum(is.na(testA$pred))
  # calculate accuracy 
  accmeasure = match.arg(accmeasure)
  result = list(ndata=nrow(ratingsIn),trainprop=trainprop,
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

# predict() method for the 'ydots' class
#
# testSet in same form as ratingsIn in findYdots(), except that there 
# is no ratings column; regObj is as in the output of findYdots()
#
# returns vector of predicted values for testSet
predict.ydots <- function(ydotsObj,testSet) {
   testSet$pred = ydotsObj$usrMeans[as.character(testSet[,1])] + 
      ydotsObj$itmMeans[as.character(testSet[,2])] - ydotsObj$grandMean
   if (!is.null(ydotsObj$regObj))
      testSet$pred = testSet$pred + predict(ydotsObj$regObj,testSet[,-(1:2)])
   testSet$pred
}

# check
checkxv <- function(trainprop=0.5,acc='mad') {
   check <- 
      data.frame(userID = c(1,3,2,1,2,3),itemID = c(1,1,3,2,3,3),ratings=5:10)
   print(check)
   print(xValAdd(check,trainprop,acc))
   check$cv = c(1,2,8,6,3,3)
   print(check)
   print(xvalAdd(check,trainprop,acc))
}


