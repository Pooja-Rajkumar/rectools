
# rating prediction via nearest neighbors, via "cosine" (inner product

# covariates and item type preferences may be used

# arguments:

#    origData: training set, object of class 'usrData'
#    newData: data point (just one for now) to be predicted, object of
#             class 'usrData'
#    newItem: ID of the item rating to be predicted for the user in
#             newData
#    k: numbers of desired nearest neigbhors, increasing
#    wtcovs: weight to put on covariates; NULL if no covs
#    wtcats: weight to put on item categories; NULL if no cats

# value:

#    predicted ratings

predict.usrData <- function(origData,newData,newItem,
      k,wtcovs=NULL,wtcats=NULL) {
   # need to determine which users in origData rated newItem; the
   # following function does this for oneUser, a single user in origData 
   checkNewItem <- function(oneUsr) { tmp <- match(oneUsr$itms,newItem)
   if (all(is.na(tmp))) return(NA) whichOne <- which(!is.na(tmp))
   c(oneUser$userID,oneUsr$ratings[whichOne]) } found <-
   as.matrix(sapply(origData,checkNewItem))
   # found is a 2-row matrix; if element found[1,i] is not NA (it will
   # be i), then user i has rated newItem, with the rating in found[2,i]
   whoHasIt <- which(!is.na(found[1,]))
   # whoHasIt[i] is the user ID, say j, of the i-th user in origData who
   # has reviewed newItem; we will now restrict origData to those users
   if (is.null(whoHasIt)) return(rep(NA,length(k)))  # no one rated this item
   origData <- origData[whoHasIt]
   found <- found[,whoHasIt,drop=FALSE]
   onecos <- function(y) cosDist(newData,y,wtcovs,wtcats)
   cosines <- sapply(origData,onecos) 
   k1 <- min(k,sum(!is.na(cosines)))
   klarge <- order(cosines,decreasing=TRUE)[1:k1]
   # klarge is a vector containing the indices (with respect to
   # origData) of the k closest users to newData
   mean(found[2,klarge])
}

