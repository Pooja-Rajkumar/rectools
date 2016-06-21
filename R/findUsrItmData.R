
# utilities to read in raw data and form two R lists, one for users and
# one for items, of classes 'usrData' and 'itmData'

# arguments:

#    ratingsIn: input data, whose first 3 cols are user ID, item ID
#               and rating 
#    usrCovs: data frame of user covariates, e.g. gender and age, one
#             row per user
#    itmCatCols: numbers of columns in ratingsIn containing item
#                categories, e.g. movie genre; data values are 0 or 1
#    fileOut: if specified, save the value returned by the function
#             using R save(), with file name fileOut

# value:

#    object of class 'usrData': an R list with one element per user;
#    each such element is itself an R list, with these components:
#
#       ratings: ratings set by this user
#       covars:  covariate data for this user, if any

formUserData <- function(ratingsIn,usrCovs=NULL,itmCatCols,fileOut='') {
   usrs <- split(ratingsIn[,3],ratingsIn[,1])
   retval <- list()
   class(retval) <- 'usrData'
   for (i in 1:length(usrs)) {
      retval[[i]] <- list(ratings=usrs[[i]])
      if (!is.null(covCols))
   }

}

