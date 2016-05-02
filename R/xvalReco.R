# args: takes in the data set with 3 columns (user, movie, rating respectively), a specific user to test, and a specific movie to test
# return: apred, a predicted data set from transposing recosystems P and Q
#
#
xvalReco <- function(newdata, trainprop = 0.5,
                     accmeasure = c('exact','mad','rms'))
{
  library(recosystem)
  r <- Reco()
  rownew = nrow(newdata)
  trainRow = floor(trainprop*rownew)
  trainidxs = sample(1:rownew,trainRow)
  trainSet = newdata[trainidxs,]
  testSet = newdata[setdiff(1:nrow(newdata),trainidxs),]
  write.table(trainSet,file = "train.txt", row.names = FALSE, col.names = FALSE)
  write.table(testSet,file = "test.txt", row.names = FALSE, col.names = FALSE)
  r$train('train.txt')
  res <- r$output(NULL,NULL)
  p <- res$P
  q <- res$Q
  apred <- p %*% t(q)
  print(head(apred))
  numpredna = sum(is.na(apred))
  accmeasure = match.arg(accmeasure)
  result = list(ndata =nrow(newdata),trainprop = trainprop, 
                accmeasure = accmeasure, numpredna = numpredna)
  if(accmeasure == 'exact'){
    apred = round(apred)
    acc = mean(apred == testSet[,3],na.rm = TRUE)
    
  } else if (accmeasure == 'mad'){
    acc = mean(abs(apred-testSet[,3]), na.rm = TRUE)
  } else if (accmeasure == 'rms'){
    acc = sqrt(mean((apred-testSet[,3])^2,na.rm = TRUE))
  }
  result$acc = acc 
  class(result) <- 'xvalreco'
  result
}
