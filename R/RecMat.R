
newrecmat <- function(mat,mattype=c('mat','sparsemat','distmat') {
   tmp <- list()
   tmp$recmat <- mat
   tmp$mattype <- mattype
   class(tmp) <- 'recmat'
   tmp
}

