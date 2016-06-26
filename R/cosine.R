
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

# origData, newData of class usrData, with the latter having only one
# element; predict latter from former, for the item newNA
predict.usrData <- function(origData,newData,newItem,k,wtcovs,wtcats) {
   checkNewItem <- function(oneUsr) 
      match(oneUsr$items,newItem)
   tmp <- sapply(origData,checkNewItem)
   whereNewItem <- which(!is.na(tmp))
   # whereNewItem[i] tells us, for each user u in origData, the index of
   # newItem in u$items
   if (is.null(tmp)) return(NA)
   origData <- origData[tmp]
   onecos <- function(y) cosDist(newData,y,wtcovs,wtcats)
   dists <- sapply(origData,onecos) 
   ksmall <- order(dists)[1:k]
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
