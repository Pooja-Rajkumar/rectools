
# rating prediction via nearest neighbors, via "cosine" (inner product);
# the latter, though standard, has certain problems, and other choices
# for distance measure will be added

# covariates and item type preferences may be used

########################## predict() function ##########################

# predict newData argument from origData argument

# arguments:

#    origData: training set, object of class 'usrData' (see file
#    findUsrData.R)
#    newData: data point (just one for now) to be predicted, object of
#             class 'usrData'
#    newItem: ID of the item rating to be predicted for the user in
#             newData
#    wtcovs: weight to put on covariates; NULL if no covs
#    wtcats: weight to put on item categories; NULL if no cats
#    k: number of nearest neigbhors (code could be modifed to obtain
#       values for several k in one call)

# value:

#    predicted rating for newData

predict.usrData <- function(origData,newData,newItem,
      k,wtcovs=NULL,wtcats=NULL) {
   # we need to narrow origData down to the users who have rated newItem
   #
   # action of checkNewItem(): here oneUsr is one user record in
   # origData; the function will check which item in the record, if any,
   # equals newItem
   checkNewItem <- function(oneUsr) {
      tmp <- match(oneUsr$itms,newItem)
      if (all(is.na(tmp))) return(c(NA,NA))
      whichOne <- which(!is.na(tmp))
      c(whichOne,oneUsr$ratings[whichOne])
   }
   found <- as.matrix(sapply(origData,checkNewItem))
   # found[i] = j means origData[[i]]$itms[j] = newItem;
   # found[i] = NA means newItem wasn't rated by user i;
   # we need to get rid of the users who have this as NA
   whoHasIt <- which(!is.na(found[1,]))
   # whoHasIt[i] is the index, i.e. user ID, of the i-th user who has
   # rated newData
   if (is.null(whoHasIt)) return(NA)  # no one rated this item
   origData <- origData[whoHasIt]  
   # now origData only has the relevant users, the ones who have rated
   # newItem
   found <- found[,whoHasIt,drop=FALSE]
   onecos <- function(y) cosDist(newData,y,wtcovs,wtcats)
   cosines <- sapply(origData,onecos) 
   k1 <- min(k,sum(!is.na(cosines)))
   klarge <- order(cosines,decreasing=TRUE)[1:k1]
   # klarge is a vector containing the indices (with respect to
   # origData) of the k closest users to newData
   mean(found[2,klarge])
}

# cosDist() find cosine distance between x and y, elements of an object
# of 'usrData' class; wtcovs, wtcats as in cosprep()
cosDist <- function(x,y,wtcovs,wtcats) {
   # rated items in common
   commItms <- intersect(x$itms,y$itms)
   # where are they in x and y?
   xwhere <- which(!is.na(match(x$itms,commItms)))
   ywhere <- which(!is.na(match(y$itms,commItms)))
   xrats <- x$ratings[xwhere]
   yrats <- y$ratings[ywhere]
   cosTot <- xrats %*% yrats
   xl2 <- sum(xrats^2)
   yl2 <- sum(yrats^2)
   if (!is.null(wtcovs)) {
      cosTot <- cosTot + wtcovs * x$cvrs %*% y$cvrs
      xl2 <- xl2 + sum((wtcovs*x$cvrs)^2)
      yl2 <- yl2 + sum((wtcovs*y$cvrs)^2)
   }
   if (!is.null(wtcats)) {
      cosTot <- cosTot + wtcats * x$cats %*% t(y$cats)
      xl2 <- xl2 + sum((wtcovs*x$cats)^2)
      yl2 <- yl2 + sum((wtcovs*y$cats)^2)
   }
   cosTot / sqrt(xl2 * yl2)
}
