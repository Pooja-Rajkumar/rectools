
### all commented out for now, until code fixed

### training <- function(trainSet,
###                     trainprop = 0.5,
###                     rank = 10){
###   library(recosystem)
###   r <- Reco()
###   write.table(trainSet,file = "train.txt", row.names = FALSE, col.names = FALSE)
###   r$train('train.txt',opts = list(dim=rank))
###   res <- r$output(NULL,NULL)
###   res
### }
### xvalReco <- function(ratingsIn, trainprop = 0.5,
###                      accmeasure = c('exact','mad','rms'),
###                      cls = NULL,
###                      rank = 10)
### {
###   library(recosystem)
###   library(parallel)
###   if(is.null(cls)){
###     rownew = nrow(ratingsIn)
###     trainRow = floor(trainprop*rownew)
###     trainidxs = sample(1:rownew,trainRow)
###     trainSet = ratingsIn[trainidxs,]
###     testSet = ratingsIn[setdiff(1:nrow(ratingsIn),trainidxs),]
###     res = training(ratingsIn)
###     p = res$P
###     q = res$Q
###     testSet$pred <- vector(length=nrow(testSet))
###     for(i in 1:nrow(testSet)){
###       j = ratingsIn[i,1]
###       k = ratingsIn[i,2]
###       testSet$pred[i] = p[j,] %*% q[k,]
###     }
###   }else {
###     }
###   numpredna = sum(is.na(testSet$pred))
###   accmeasure = match.arg(accmeasure)
###   result = list(ndata =nrow(ratingsIn),trainprop = trainprop, 
###                 accmeasure = accmeasure, numpredna = numpredna)
###   if(accmeasure == 'exact'){
###     testSet$pred = round(testSet$pred)
###     acc = mean(testSet$pred == testSet[,3],na.rm = TRUE)
###     
###   } else if (accmeasure == 'mad'){
###     acc = mean(abs(testSet$pred-testSet[,3]), na.rm = TRUE)
###   } else if (accmeasure == 'rms'){
###     acc = sqrt(mean((testSet$pred-testSet[,3])^2,na.rm = TRUE))
###   }
###   result$acc = acc 
###   class(result) <- 'xvalreco'
###   result
### }
### 
