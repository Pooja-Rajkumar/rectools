
# splits input data into training and test sets, fits multiplicative
# effects model on the former, then predicts the latter

# arguments:

#   ratingsIn: input data, with first cols (userID,itemID,rating,
#              covariates); data frame
#   trainprop: proportion of data for the training set

# value:

#    accuracy value

xvalMultiplic <- function(ratingsIn, trainprop=0.5) {
  ratIn = ratingsIn 
  # split into random training and validation sets 
  nrowRatIn = nrow(ratIn)
  rowNum = floor(trainprop * nrowRatIn)
  trainIdxs = sample(1:nrowRatIn,rowNum)
  trainSet = ratIn[trainIdxs, ]
  trnout = trainMultiplic(trainSet)
  testSet = ratIn[-trainIdxs,]
  pred = round(predict(trnout,testSet[,-3]))
  numpredna = sum(is.na(pred))
  result = list(ndata=nrowRatIn,trainprop=trainprop,numpredna=numpredna)
  result$acc <- mean(pred == testSet[,3],na.rm=TRUE)
  result$preds <- pred
  result$actuals <- testSet[,3]
  class(result) <- 'xvalmultiplic'
  result
}

