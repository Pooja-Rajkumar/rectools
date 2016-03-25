
buildMatrix <- function(data){
users = data[,1]
movies = data[,2]
rating = data[,3]
newMatrix = matrix(0, nrow = max(users), ncol = max(movies))
for(rows in 1:nrow(data)){
    newMatrix[data[rows,1],data[rows,2]] = data[rows,3]
}
return (newMatrix)
}
