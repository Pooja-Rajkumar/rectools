xvalReco <- function(ratingsIn, trainprop = 0.5,
                     accmeasure = c('exact','mad','rms'))
{
  library(recosystem)
  r <- Reco()
  rownew = nrow(ratingsIn)
  trainRow = floor(trainprop*rownew)
  trainidxs = sample(1:rownew,trainRow)
  trainSet = ratingsIn[trainidxs,]
  testSet = ratingsIn[setdiff(1:nrow(ratingsIn),trainidxs),]
  write.table(trainSet,file = "train.txt", row.names = FALSE, col.names = FALSE)
  r$train('train.txt')
  res <- r$output(NULL,NULL)
  testSet$pred <- vector(length=nrow(testSet))
  for(i in 1:nrow(testSet)){
    j = ratingsIn[i,1]
    k = ratingsIn[i,2]
    testSet$pred[i] = res$P[j,] %*% res$Q[k,]
  }
  numpredna = sum(is.na(testSet$pred))
  accmeasure = match.arg(accmeasure)
  result = list(ndata =nrow(ratingsIn),trainprop = trainprop, 
                accmeasure = accmeasure, numpredna = numpredna)
  if(accmeasure == 'exact'){
    testSet$pred = round(testSet$pred)
    acc = mean(testSet$pred == testSet[,3],na.rm = TRUE)
    
  } else if (accmeasure == 'mad'){
    acc = mean(abs(testSet$pred-testSet[,3]), na.rm = TRUE)
  } else if (accmeasure == 'rms'){
    acc = sqrt(mean((testSet$pred-testSet[,3])^2,na.rm = TRUE))
  }
  result$acc = acc 
  result$pred = testSet$pred
  result$testVals = testSet[,3]
  class(result) <- 'xvalreco'
  result
}
