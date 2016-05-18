training <- function(trainSet,
                    trainprop = 0.5,
                    rank = 10){
  library(recosystem)
  r <- Reco()
  write.table(trainSet,file = "train.txt", row.names = FALSE, col.names = FALSE)
  r$train('train.txt',opts = list(dim=rank))
  res <- r$output(NULL,NULL)
  res
}

prediction <- function(ratingsIn,res,testSet){
  p = res$P
  q = res$Q
  testSet$pred <- vector(length=nrow(testSet))
  for(i in 1:nrow(testSet)){
    j = ratingsIn[i,1]
    k = ratingsIn[i,2]
    testSet$pred[i] = p[j,] %*% q[k,]
  }
  testSet$pred
}


getTrainSet <- function(ratingsIn,trainprop = 0.5){
  rownew = nrow(ratingsIn)
  trainRow = floor(trainprop*rownew)
  trainidxs = sample(1:rownew,trainRow)
  trainSet = ratingsIn[trainidxs,]
  trainSet$trainidxs = trainidxs
  trainSet
}

getTestSet <- function(ratingsIn, trainSet){
  testSet = ratingsIn[setdiff(1:nrow(ratingsIn),trainSet$trainidxs),]
  testSet
}


xvalReco <- function(ratingsIn, trainprop = 0.5,
                     accmeasure = c('exact','mad','rms'),
                     cls = NULL,
                     rank = 10)
{
  library(recosystem)
  library(parallel)
  if(is.null(cls)){
    trainSet = getTrainSet(ratingsIn, trainprop)
    testSet= getTestSet(ratingsIn, trainSet)
    res = training(trainSet)
    totalPreds = prediction(ratingsIn,res,testSet)
  
  }else {
    require(partools)
    clusterEvalQ(cls,require(partools))
    distribsplit(cls, 'ratingsIn')
    clusterExport(cls,c('training','prediction','getTestSet','getTrainSet'))
    clusterEvalQ(cls, trainSet<- getTrainSet(ratingsIn,trainprop=0.5))
    testSet= clusterEvalQ(cls, testSet<- getTestSet(ratingsIn,trainSet))
    testSet = mapply(c,testSet$ratings[1],testSet$ratings[2],SIMPLIFY = FALSE)
    clusterEvalQ(cls,resu <- training(trainSet,trainprop=0.5,rank=10))
    allPreds = clusterEvalQ(cls, pred <- prediction(ratingsIn,resu,testSet))
    totalPreds = mapply(c,totalPreds[1],totalPreds[2],SIMPLIFY = FALSE)
  }
  numpredna = sum(is.na(totalPreds))
  accmeasure = match.arg(accmeasure)
  result = list(ndata =nrow(ratingsIn),trainprop = trainprop, 
                accmeasure = accmeasure, numpredna = numpredna)
  if(accmeasure == 'exact'){
    totalPreds = round(totalPreds)
    acc = mean(totalPreds == testSet[,3],na.rm = TRUE)
  } else if (accmeasure == 'mad'){
    acc = mean(abs(totalPreds-testSet[,3]), na.rm = TRUE)
  } else if (accmeasure == 'rms'){
    acc = sqrt(mean((totalPreds-testSet[,3])^2,na.rm = TRUE))
  }
  result$acc = acc 
  class(result) <- 'xvalreco'
  result
 
}

