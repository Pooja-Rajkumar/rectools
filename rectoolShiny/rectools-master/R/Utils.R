
# finds and returns the number of ratings for each user (splitCol = 1)
# or each item (splitCol = 2), for the input data frame ratingsIn, in
# UserID | ItemID | Rating ...  format

ratingness <- function(ratingsIn,splitCol) {
   tapply(ratingsIn[,3],ratingsIn[,splitCol],length)
}


# forms a column 'nrats', intended to be appended to ratingsIn, with 
# nrats[i] = number of ratings of user i or item i; ratingsIn is as in
# ratingness() above

covratingness <- function(ratingsIn,splitCol) {
   tmp <- ratingness(ratingsIn,splitCol)
   tmp[ratingsIn[,splitCol]]
}

