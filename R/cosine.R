
# rating prediction via nearest neighbors, via "cosine" (inner product

# covariates and item type preferences may be used

# cosprep() does the initial nearest-neighbor analysis, after which use
# predict(); finds the k nearest-neighbors of the points in the original
# input data

# arguments:

#    usrDataObj: object of class 'usrData'
#    nitms: number of items
#    wtcovs: weight to put on covariates; NULL if no covs
#    wtcats: weight to put on item categories; NULL if no cats
#    k: number of nearest neigbhors
#    fileOut: name of file to save return value to

# value:

#    object of class 'cosknn'

cosprep <- 
   function(usrDataObj,iitms,wtcovs=NULL,wtcats=NULL,k=50,fileOut='') {

}

# cosDist() find cosine distance between x and y, elements of an object
# of 'usrData' class; wtcovs, wtcats as in cosprep()
cosDist <- function(x,y,nitems,wtcovs,wtcats) {
   xrow <- rep(0,nitems)
   xrow[x$itms] <- xrow[x$ratings]
   yrow <- rep(0,nitems)
   yrow[y$itms] <- yrow[y$ratings]
   browser()
   cosTot <- xrow %*% yrow
   if (!is.null(wtcovs)) {
      cosTot <- cosTot + wtcovs * x$cvrs %*% y$cvrs
   }
   if (!is.null(wtcats)) {
      cosTot <- cosTot + wtcats * x$cats %*% t(y$cats)
   }
   cosTot
}
