
# finds "representative" users, i.e. the users whose ratings correlate
# the highest with the average ratings of the items they rate

# specifically, for each user i, vectors u and v will be formed; u will
# be the vector of ratings set by user i; for v[j], the code will find
# the item ID m of the j-th component of u, then set v[j] to the average
# rating of all users for item ID m; then the mean abolute error will be
# computed, using u to predict v; this will be done for each user, and
# the k users with the lowest MAEs will be chosen

# arguments:

#    ydotsObj: output of findYdotsMM() or findYdotsMLE()
#    ratingsIn: input data, 3-column
#    k: the IDs of the most "accurate" k users will be returned
#    minn: minimum number of ratings for a user to be considered

# value:  see k above

focusGrp <- function(ydotsObj,ratingsIn,k=10,minn=50) {
   rin <- ratingsIn
   # form list, of which element [[i]] is items-ratings submatrix for
   # user i
   ugrps <- split(rin[,2:3],rin[,1]) 
   # remove users with too few ratings
   nvals <- sapply(ugrps,nrow)
   manyrats <- which(nvals >= minn)
   ugrps <- ugrps[manyrats]
   itmMeans <- ydotsObj$itmMeans
   nugrps <- length(ugrps)
   mads <- vector(length=nugrps)
   names(mads) <- as.character(1:nugrps)
   for (i in 1:nugrps) {
      tmp <- ugrps[[i]][,2] - itmMeans[ugrps[[i]][,1]]
      mads[i] <- mean(abs(tmp))
   }
   as.numeric(names(sort(mads)))[1:k]
}
