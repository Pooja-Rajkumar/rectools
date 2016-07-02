
# various plots of the estimated alpha_i and beta_j

plot.ydotsMM <- function(ydotsObj,ratingsIn) {
   ydo <- ydotsObj
   rin <- ratingsIn
   names(rin) <- c('user','item','rating')
   ydoalph <- ydo$usrMeans - ydo$grandMean
   ydobeta <- ydo$itmMeans - ydo$grandMean
   plot(density(ydoalph),xlab='est. alpha',main='')
   readline('hit Enter for next graph')
   plot(density(ydobeta),xlab='est. beta',main='')
   rin$alph <- ydoalph[rin[,1]] 
   rin$beta <- ydobeta[rin[,2]] 
   readline('hit Enter for next graph')
   main <- 'smoothed scatter plot'
   smoothScatter(rin$alph,rin$beta,main=main)
}

plot.xvalb <- function(xvalObj) {
   plot(density(xvalObj$preds,na.rm=TRUE),xlab='predicted ratings',main='')
   readline('hit Enter for next graph')
   main <- 'smoothed scatter plot'
   smoothScatter(xvalObj$actuals,xvalObj$preds,main=main)
}
