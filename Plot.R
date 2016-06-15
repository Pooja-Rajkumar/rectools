
# various plots of the estimated alpha_i and beta_j

plot.ydotsMM <- function(ydotsObj,ratingsIn) {
   ydo <- ydotsObj
   rin <- ratingsIn
   names(rin) <- c('user','item','rating')
   rin$alph <- ydo$usrMeans[rin[,1]] - ydo$grandMean
   rin$beta <- ydo$itmMeans[rin[,2]] - ydo$grandMean
   smoothScatter(rin$alph,rin$beta)
}

