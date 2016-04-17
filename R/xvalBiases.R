
# splits input data into training and test sets, fits "ydots" model on
# the former, then predicts the latter

# arguments:

#   ratingsIn: input data frame, with first cols (userID,itemID,rating)
#   trainprop: proportion of data for the training set
#   accmeasure: accuracy measure

# value:

#    accuracy value

xvalBiases <- function(ratingsIn, trainprop=0.5,accmeasure='exact'){
  # Split into training and validation sets 
  rowNum = floor(trainprop * nrow(ratingsIn))
  trainingSet = ratingsIn[1:rowNum ,]
  trainRatings = trainingSet[,3]
  trainItems = trainingSet[,2]
  trainUsers = trainingSet[,1]
  # get means
  means = findYdots(trainingSet)
  Y.. = means$GrandMean
  Yi. = means$UsrMeans
  Y.j = means$ItmMeans
  testA = ratingsIn[(rowNum+1):nrow(ratingsIn),]
  # predict the cases in the test set
  testA$pred = 
     Yi.[as.character(testA[,1])] + Y.j[as.character(testA[,2])] - Y..
print(testA)
  # calculate accuracy 
  if (accmeasure == 'exact') {
     testA$pred = round(testA$pred)
     return(mean(testA$pred == testA[,3],na.rm=TRUE))
  } else if (accmeasure == 'mad') {
     return(mean(abs(testA$pred-testA[,3]),na.rm=TRUE))
  }
}

# check
checkxv <- function(trainprop=0.5,acc='mad') {
   check <- 
      data.frame(userID = c(1,3,2,1,2,3),itemID = c(1,1,3,2,3,3),ratings=5:10)
   print(check)
   print(xvalBiases(check,trainprop,acc))
}


