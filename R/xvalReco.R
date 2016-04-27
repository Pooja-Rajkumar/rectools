# args: takes in the data set with 3 columns (user, movie, rating respectively), a specific user to test, and a specific movie to test
# return: apred, a predicted data set from transposing recosystems P and Q
#
#
xvalReco <- function(newdata,testx,testy)
{
  library(recosystem)
  r <- Reco()
  trainRow =  floor(nrow(newdata))/2
  trainidxs = newdata[1:trainRow,1:3]
  testRow = nrow(newdata) - trainRow
  testidxs = newdata[1:testRow,1:3]
  write.table(trainidxs,file = "train.txt", row.names = FALSE, col.names = FALSE)
  write.table(testidxs,file = "test.txt", row.names = FALSE, col.names = FALSE)
  r$train('train.txt')
  res <- r$output(NULL,NULL)
  p <- res$P
  q <- res$Q
  apred <- p %*% t(q) 
  print(head(testidxs))
  print(head(apred[testx,testy])) 
  apred
}
