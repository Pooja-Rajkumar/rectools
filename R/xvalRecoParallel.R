xvalReco <- function(newdata, trainprop = 0.5,
                     accmeasure = c('exact','mad','rms'),
                     cls = NULL)
{
  library(recosystem)
  library(parallel)
  if(is.null(cls))
    {
      r <- Reco()
      rownew = nrow(newdata)
      trainRow = floor(trainprop*rownew)
      trainidxs = sample(1:rownew,trainRow)
      trainSet = newdata[trainidxs,]
      testSet = newdata[setdiff(1:nrow(newdata),trainidxs),]
      write.table(trainSet,file = "train.txt", row.names = FALSE, col.names = FALSE)
      r$train('train.txt')
      res <- r$output(NULL,NULL)
      p = res$P
      q = res$Q
      testSet$pred <- vector(length=nrow(testSet))
      for(i in 1:nrow(testSet)){
        j = newdata[i,1]
        k = newdata[i,2]
        testSet$pred[i] = p[j,] %*% q[k,]
      }
  }else {
    #stop("parallel under construction")
    for(i in 1:length(cls)){
    rowgrps = splitIndices(nrow(newdata),length(cls))
    grpall = train(newdata,rowgrps)
    mout = clusterApply(cls,rowgrps,grpall)
    Reduce(c,mout)
    }
  }
  #testSet$pred = round(testSet$pred)
  #acc = mean(testSet$pred == testSet[,3],na.rm = TRUE)
  
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
  class(result) <- 'xvalreco'
  result
}

train <- function(newdata,group){
  trainSet = newdata[group,]
  testSet = newdata[setdiff(1:nrow(newdata),group),]
  p = res$P
  q = res$Q
  testSet$pred <- vector(length=nrow(testSet))
  for(i in 1:nrow(testSet)){
    j = newdata[i,1]
    k = newdata[i,2]
    testSet$pred[i] = p[j,] %*% q[k,]
  }
  testSet$pred
}

xvalReco(newdata,.5,'exact')

library(parallel)
c2 <- makePSOCKcluster(rep("localhost",2))
xvalReco(newdata,.5,'exact',c2)
check(newdata,298,474)
