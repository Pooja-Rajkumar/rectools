
buildMatrix <- function(ratingsIn,NAval=0){
   # deal with possible factors
   dmax <- function(d) {
      if (is.factor(d)) return(length(levels(d)))
      max(d)
   }
   users = ratingsIn[,1]
   movies = ratingsIn[,2]
   rating = ratingsIn[,3]
   newMatrix = matrix(NAval, 
          nrow = dmax(users), ncol = dmax(movies))
   for(rows in 1:nrow(ratingsIn)){
       newMatrix[ratingsIn[rows,1],ratingsIn[rows,2]] = ratingsIn[rows,3]
   }
   return (newMatrix)
}
