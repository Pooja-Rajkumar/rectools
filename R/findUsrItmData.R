
# utility to read in raw data in standard format,
#    (user ID, item ID, rating)
# and form an R list for user data, with class 'usrData'; each
# element of the list will be of class 'usrDatum', and will have
# components as seen in 'value' below 

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
   # covariates, if any, should be in usrCovs; check to see if user has
   # included them
   if (ncol(ratingsIn) > 3)
      stop('ratingsIn more than 3 columns')

   ## IMPORTANT NOTE: in order to work in xval, etc. we need to abandon
   ## the idea of having the user IDs start at 1 and be consecutive;
   ## instead, we will just use the ID numbers as list indices; e.g. if we
   ## have users numbered 2,8,85 then retval below will consist of
   ## retval[[2]], retval[[8]] and retval[[85]]

   # rownums[[i]] will be the row numbers in ratingsIn belonging to user i
   ## rownums <- split(1:nrow(ratingsIn),ratingsIn[,1])
   rownums <- split(1:nrow(ratingsIn),ratingsIn[,1])
   nusers <- length(rownums)
   if (!is.null(itmCats)) {
      itmCats <- as.matrix(itmCats)
      nitems <- nrow(itmCats)
   }
   retval <- vector('list',length(rownums))
   for (i in 1:nusers) {
      whichrows <- rownums[[i]]
      userID <- ratingsIn[whichrows[1],1]
      # start building usrDatum object for this user
      retval[[userID]] <- list()
      retval[[userID]]$userID <- userID
      retval[[userID]]$itms <- ratingsIn[whichrows,2]
      retval[[userID]]$ratings <- ratingsIn[whichrows,3]
      if (!is.null(usrCovs))
         retval[[userID]]$cvrs <- as.numeric(usrCovs[userID,])
      if (!is.null(itmCats)) {
         tmp <- rep(0,nitems)
         tmp[retval[[userID]]$itms] <- 1
         # form vector whose j-th element will be the count of items
         # rated by this user that are in category j
         catcounts <- tmp %*% itmCats
         # convert to proportions and record
         retval[[userID]]$cats <- catcounts / sum(tmp)
      }
      class(retval[[userID]]) <- 'usrDatum'
   }
   class(retval) <- 'usrData'
   if (fileOut != '') save(retval,file=fileOut)
   retval
}

### # construct a new object of class 'usrDatum'
### 
### formUserDatum <- function(itms,ratings,userID=NULL) {
###    obj <- list(itms = itms, ratings=ratings,userID=userID)
###    class(obj) <- 'usrDatum'
###    obj
### }

# utility:  find input row for a given user, item
findInputRow <- function(ratingsIn,usrID,itmID) {
   ratingsIn[ratingsIn[,1]==usrID & ratingsIn[,2]==itmID,]
}

