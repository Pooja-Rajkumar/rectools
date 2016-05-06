xvalReco <- function(newdata, trainprop = 0.5,
                     accmeasure = c('exact','mad','rms'))
{
  library(recosystem)
  r <- Reco()
  rownew = nrow(newdata)
  trainRow = floor(.5*rownew)
  trainidxs = sample(1:rownew,trainRow)
  trainSet = newdata[trainidxs,]
  testSet = newdata[setdiff(1:nrow(newdata),trainidxs),]
  write.table(trainSet,file = "train.txt", row.names = FALSE, col.names = FALSE)
  r$train('train.txt')
  res <- r$output(NULL,NULL)
  testSet$pred <- vector(length=nrow(testSet))
  for(i in 1:nrow(testSet)){
    j = newdata[i,1]
    k = newdata[i,2]
    testSet$pred[i] = res$P[j,] %*% res$Q[k,]
  }
  numpredna = sum(is.na(apred))
  accmeasure = match.arg(accmeasure)
  result = list(ndata =nrow(newdata),trainprop = trainprop, 
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
  class(result) <- 'xvalreco'
  result
}
