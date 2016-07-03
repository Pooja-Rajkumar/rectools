
# finds "representative" users, i.e. the users whose ratings correlate
# the highest with the average ratings of the items they rate

# specifically, for each user i, vectors u and v will be formed; u will
# be the vector of ratings set by user i; for v[j], the code will find
# the item ID of the j-th component of u, then set v[j] to the average
# rating of all users for that item ID; then the correlation between u
# and v will be computed for each user, and the k users with the highest
# correlations will be chosen

# arguments:

#    ydotsObj: output of findYdotsMM() or findYdotsMLE()
#    ratingsIn: input data, 3-column
#    k: the IDs of the most "accurate" k users will be returned
#    minn: minimum number of ratings for a user to be considered

# value:  see k above

focusGrp <- function(ydotsObj,ratingsIn,k=10,minn=50) {
   itmms <- ydotsObj$itmMeans
   rin <- ratingsIn
   # form list, of which element [[i]] is items-ratings submatrix for
   # user i
   ugrps <- split(rin[,2:3],rin[,1]) 
   # remove users with too few ratings
   nvals <- sapply(ugrps,nrow)
   manyrats <- which(nvals >= minn)
   ugrps <- ugrps[manyrats]
   # inputs cols 2:3 of a matrix like rin; replace vector of item IDs by
   # the average ratings of those items
   f <- function(itmsrats) 
      cbind(itmms[itmsrats[,1]],itmsrats[,2]) 
   ugrps1 <- lapply(ugrps,f) 
   cor12 <- function(m2) cor(m2)[1,2] 
   cors <- sapply(ugrps1,cor12) 
   cors <- sort(cors,decreasing=TRUE)
   as.numeric(names(cors[1:k]))
}
