
# note: it is assumed that user IDs are consecutive numbers starting at
# 1, and the same for the item numbers

# utility to read in raw data in standard format,
#    (user ID, item ID, rating)
# and form an R list for user data, with class 'usrData'; each
# element of the list will be of class 'usrDatum', and will have
# components as see in 'value' below 

# arguments:

#    ratingsIn: input data, whose first 3 cols are user ID, item ID
#               and rating 
#    usrCovs: data frame of user covariates, e.g. gender and age, one
#             row per user
#    itmCats: data frame of item categories, e.g. genre for movies, one
#             row of booleans per item; categories need not be 
#             mutually exclusive
#    fileOut: if specified, save the value returned by the function
#             using R save(), with file name fileOut
# value:

#    object of class 'usrData': an R list with one element per user;
#    each such element is itself an R list, an object of class
#    'usrDatum', with these components:
#
#       userID: the ID of this user
#       ratings: vector of ratings made by this user
#       itms: IDs of items rated by this user
#       cvrs:  covariate data for this user, if any
#       cats:  item category data for this user, if any; i-th element
#              is proportion of items rated by this user that are 
#              in category i

formUserData <- function(ratingsIn,usrCovs=NULL,itmCats=NULL,fileOut='') {
   if (ncol(ratingsIn) > 3)
      stop('have you included covariates?')
   # rownums[[i]] will be the row numbers in ratingsIn belonging to user i
   rownums <- split(1:nrow(ratingsIn),ratingsIn[,1])
   nusers <- length(rownums)
   userrange <- range(as.numeric(names(rownums)))
   usermin <- userrange[1]
   usermax <- userrange[2]
   if (usermin != 1) {
      stop('user IDs must start at 1')
   }
   if (usermax - usermin + 1 != nusers) {
      stop('some user IDs missing')
   }
   # should add check for item IDs too
   retval <- list()  # start building return value
   if (!is.null(itmCats)) {
      itmCats <- as.matrix(itmCats)
      nitems <- nrow(itmCats)
   }
   for (i in 1:nusers) {
      whichrows <- rownums[[i]]
      retval[[i]] <- list(userID=i)  # usrDatum object for user i
      retval[[i]]$itms <- ratingsIn[whichrows,2]
      retval[[i]]$ratings <- ratingsIn[whichrows,3]
      if (!is.null(usrCovs))
         retval[[i]]$cvrs <- as.numeric(usrCovs[i,])
      if (!is.null(itmCats)) {
         tmp <- rep(0,nitems)
         tmp[retval[[i]]$itms] <- 1
         retval[[i]]$cats <- tmp %*% itmCats / sum(tmp)
      }
      class(retval[[i]]) <- 'usrDatum'
   }
   class(retval) <- 'usrData'
   if (fileOut != '') save(retval,file=fileOut)
   retval
}

# construct a new object of class 'usrDatum'

formUserDatum <- function(itms,ratings,userID=NULL) {
   obj <- list(itms = itms, ratings=ratings,userID=userID)
   class(obj) <- 'usrDatum'
   obj
}

# utility:  find input row for a given user, item
findInputRow <- function(ratingsIn,usrID,itmID) {
   ratingsIn[ratingsIn[,1]==usrID & ratingsIn[,2]==itmID,]
}

