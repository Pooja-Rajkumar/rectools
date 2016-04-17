
xvalBiases <- function(ratingsIn, accmeasure='exact'){
  # Split into training and validation sets 
  rowNum = nrow(ratingsIn)/2
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
  # calculate accuracy 
  if (accmeasure == 'exact') {
     testA$pred = round(testA$pred)
     return(mean(testA$pred == testA[,3],na.rm=TRUE))
  } else if (accmeasure == 'mad') {
     return(mean(abs(testA$pred-testA[,3]),na.rm=TRUE))
  }
}

# check
checkxv <- function(acc='mad') {
   check <- 
      data.frame(userID = c(1,3,2,1,2,3),itemID = c(1,1,3,2,3,3),ratings=5:10)
   print(check)
   print(xvalBiases(check,acc))
}


