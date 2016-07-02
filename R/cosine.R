
# rating prediction via nearest neighbors, via "cosine" (inner product

# covariates and item type preferences may be used

# arguments:

#    origData: training set, object of class 'usrData'
#    newData: data point (just one for now) to be predicted, object of
#             class 'usrData'
#    newItem: ID of the item rating to be predicted for the user in
#             newData
#    wtcovs: weight to put on covariates; NULL if no covs
#    wtcats: weight to put on item categories; NULL if no cats
#    k: number of nearest neigbhors (code could be modifed to obtain
#       values for several k in one call)

# value:

#    predicted rating

predict.usrData <- function(origData,newData,newItem,
      k,wtcovs=NULL,wtcats=NULL) {
   checkNewItem <- function(oneUsr) {
      tmp <- match(oneUsr$itms,newItem)
      if (all(is.na(tmp))) return(c(NA,NA))
      whichOne <- which(!is.na(tmp))
      c(whichOne,oneUsr$ratings[whichOne])
   }
   found <- as.matrix(sapply(origData,checkNewItem))
   # found is a vector whose i-th element, j, is such that itms[j] of
   # origData[[i]] is newItem, with NA if newItem wasn't rated by user i
   whoHasIt <- which(!is.na(found[1,]))
   # whoHasIt[i] is the index, i.e. user ID, of the i-th non-NA value in
   # tmp
   if (is.null(whoHasIt)) return(NA)  # no one rated this item
   origData <- origData[whoHasIt]
   found <- found[,whoHasIt]
   onecos <- function(y) cosDist(newData,y,wtcovs,wtcats)
   dists <- sapply(origData,onecos) 
   k1 <- min(k,sum(!is.na(dists)))
   ksmall <- order(dists)[1:k1]
   # ksmall is a vector containing the indices (with respect to
   # origData) of the k closest users to newData
   mean(found[2,ksmall])
}

# cosDist() find cosine distance between x and y, elements of an object
# of 'usrData' class; wtcovs, wtcats as in cosprep()
cosDist <- function(x,y,wtcovs,wtcats) {
   # rated items in common
   commItms <- intersect(x$itms,y$itms)
   # where are they in x and y?
   xwhere <- which(!is.na(match(x$itms,commItms)))
   ywhere <- which(!is.na(match(y$itms,commItms)))
   cosTot <- x$ratings[xwhere] %*% y$ratings[ywhere]
   if (!is.null(wtcovs)) {
      cosTot <- cosTot + wtcovs * x$cvrs %*% y$cvrs
   }
   if (!is.null(wtcats)) {
      cosTot <- cosTot + wtcats * x$cats %*% t(y$cats)
   }
   cosTot
}
