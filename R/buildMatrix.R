
buildMatrix <- function(data,naVal=0){
   # deal with possible factors
   dmax <- function(d) {
      if (is.factor(d)) return(length(levels(d)))
      max(d)
   }
   users = data[,1]
   movies = data[,2]
   rating = data[,3]
   newMatrix = matrix(naVal, 
          nrow = dmax(users), ncol = dmax(movies))
   for(rows in 1:nrow(data)){
       newMatrix[data[rows,1],data[rows,2]] = data[rows,3]
   }
   return (newMatrix)
}
