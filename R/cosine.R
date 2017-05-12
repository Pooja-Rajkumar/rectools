
########################## predict() function ##########################


#'  @title predict newData argument from origData argument
#'
#'  rating prediction via nearest neighbors, via "cosine" (inner product);
#'  the latter, though standard, has certain problems (e.g., its
#'  scale-free nature), and other choices for distance measure will be added.
#'  covariates (e.g. age, gender) and item type preferences (e.g.
#'  preferred movie genres) are allowed
#'
#' @param origData: training set, object of class 'usrData' (see file findUsrItmData.R)
#' @param newData: data point (just one for now) to be predicted, object of class 'usrDatum'
#' @param newItem: ID of the item rating to be predicted for the user in newData
#' @param wtcovs: weight to put on covariates; NULL if no covs
#' @param wtcats: weight to put on item categories; NULL if no cats
#' @k: a vector of the numbers of nearest neigbhors used for predicting
#'
#' @return predicted ratings for newData


predict.usrData <- function(origData,newData,newItem,
      k,wtcovs=NULL,wtcats=NULL) {

   ### # return NA if newData is NULL (no ratings provided for the rater)
   ### if (is.na(newData)) return(NA)

   # we need to narrow origData down to the users who have rated newItem
   
   # action of checkNewItem(): here oneUsr is one user record in
   # origData; the function will look for a j such that element j in the
   # items list for this user matches the item of interest, newItem; if
   # such a j exists, then (j,rating) will be returned, otherwise
   # (NA,NA); defined for use by sapply() below
   checkNewItem <- function(oneUsr) {
     tmp <- match(oneUsr$itms, newItem)
     #message(oneUsr)
     if (all(is.na(tmp))) {
       c(NA,NA)
     }
     else{
       whichOne <- which(!is.na(tmp))
       ### NM comment c(whichOne,oneUsr$ratings[whichOne,1])
       c(whichOne,oneUsr$ratings[whichOne])
     }
   }

   found <- as.matrix(sapply(origData,checkNewItem))
   # found is of dimensions 2 x number of users;
   # found[1,i] = j means origData[[i]]$itms[j] = newItem;
   # found[1,i] = NA means newItem wasn't rated by user i;ß
   # found[2,i] will be the rating in the non-NA case
   
   # we need to get rid of the NA users
   whoHasIt <- which(!is.na(found[1,]))
   # whoHasIt[i] is the index, i.e. user ID, of the i-th user who has
   # rated newData
   
   if (is.null(whoHasIt) | length(whoHasIt) == 0) 
      return(NA)  # no one rated this item

   origData <- origData[whoHasIt]
   # now origData only has the relevant users, the ones who have rated
   # newItem, so select only those columns of the found matrix
   found <- found[,whoHasIt,drop=FALSE]
   #found <- found[!is.na(found)]

   # find the distance from newData to one user y of origData; defined for
   # use in sapply() below
   onecos <- function(y) cosDist(newData,y,wtcovs,wtcats)
   cosines <- sapply(origData,onecos)
   # the vector cosines contains the distances from newData to all the
   # original data points

   # action of findKnghbourRtng(): predict rating based on each k[i] neighbours
   # x = k[i]
   # if x > neighbours present in the dataset, then the maximum 
   # number of neighbours is used
   findKnghbourRtng <- function(x){
     # x can be at most the number of neighbours in the dataset
     x <- min(x, length(cosines))
     # klarge is a vector containing the indices of the x closest neighbours
     klarge <- order(cosines,decreasing=TRUE)[1:x]
     mean(as.numeric(found[2, klarge]))
   }
   sapply(k, findKnghbourRtng)
}

#'find cosine distance between x and y, elements of an object
#' of 'usrData' class
#'
#'  \code{cosDist} find cosine distance between x and y, elements of an object
#'   of 'usrData' class; only items rated in both x and y are used; if none
#'   exist, then return NaN
#'   @x: object of usrData class
#'   @y: second object of usrData class
#'  @wtcovs: weight to put on covariates; NULL if no covs
#'  @wtcats: weight to put on item categories; NULL if no cats
cosDist <- function(x,y,wtcovs,wtcats) {
  # rated items in common
   commItms <- intersect(x$itms,y$itms)
   if (is.null(commItms)| length(commItms)==0) return(NaN)
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
