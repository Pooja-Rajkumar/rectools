xvalBiases <- function(ratingsIn, accmeasure){
  # Split into training and validation sets 
  rowNum = nrow(ratingsIn)/2
  trainingSet = as.matrix(ratingsIn[1:rowNum ,]) 
  trainRatings = trainingSet[,3]
  trainMovies = trainingSet[,2]
  trainUsers = trainingSet[,1]
  means = findYdots(trainingSet)
  # Calculate all the means
  Y.. = means$`Overall Mean`
  Yi. = means$`Mean Values per user`
  Yj. = means$`Mean Values per Effect`
  
  testA = as.matrix(ratingsIn[rowNum:nrow(ratingsIn),])
  
  for(i in 1:nrow(trainingA)){
    testA[i,3] = Yi.[testA[i,1]] + Y.j[testA[i,2]] - Y..
  }
  testA = round(testA)
  # Lets calculate the accuracy 
  counter = 0
  for(row in 1:nrow(testA))
    if(testA[row,3] == ratingsIn[row,3]){
      counter = counter + 1
    }
  accuracy = counter/length(trainA)
  if(accuracy < accmeasure)
    accurate = FALSE
  else(accuracy >= accmeasure)
    accurate = TRUE
  acc <- structure(list(accurate,testA))
  names(acc) = c("Accuracy", "NewSet")
  return(acc)
}

