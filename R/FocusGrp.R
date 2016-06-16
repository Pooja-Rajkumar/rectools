
# finds the users whose ratings correlate the highest with the average
# ratings of the items they rate

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
   as.numeric(names(cors))
}
