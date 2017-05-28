
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

# get the instructor evaluation data, and set it up, including
# covariates

getInstEval <- function()  {
   data(InstEval)
   ivl <- InstEval
   # convert from factors
   ivl$s <- as.numeric(ivl$s)
   ivl$d <- as.numeric(ivl$d)
   ivl$studage <- as.numeric(ivl$studage)
   ivl$lectage <- as.numeric(ivl$lectage)
   ivl$service <- as.numeric(ivl$service)
   # make correct format, choose 
   ivl <- ivl[,c(1,2,7,3:6)]
   # create dummy variables in place of dept
   library(dummies)
   dms <- dummy(ivl$dept)
   dms <- as.data.frame(dms)
   dms$dept2 <- NULL
   ivl$dept <- NULL
   ivl <- cbind(ivl,dms)
   gassign('ivl','ivl')
}

# get the MovieLense 100K evaluation data, and set it up, including
# covariates

# if needDownload, then download will be done; datadir is the directory
# containing the data, assumed by default to be in the current working
# directory

getML <- function(needDownload=FALSE,datadir='./ml-100k')  {
   if (needDownload) {
      # 5 Mb
      download.file(
         'http://files.grouplens.org/datasets/movielens/ml-100k.zip',
         'ml-100k.zip')
      unzip('ml-100k.zip')
   }
   currdir <- getwd()  # leave a trail of bread crumbs
   setwd(datadir)
   ud <- read.table('u.data',header=F,sep='\t')  
   uu <- read.table('u.user',header=F,sep='|')  
   ui <- read.table('u.item',header=F,sep='|')  
   setwd(currdir) # follow the trail back 
   ud <- ud[,-4]   # remove timestamp, leaving user, item, rating  
   uu <- uu[,1:3]  # user, age, gender  
   ui <- ui[,c(1,6:24)]  # item num, genres  
   names(ud) <- c('user','item','rating')  
   names(uu) <- c('user','age','gender')  
   names(ui)[1] <- 'item'  
   names(ui)[-1] <- gsub('V','GNR',names(ui)[-1]) # GNR = genre  
   uu$gender <- as.integer(uu$gender == 'M')  
   uduu <- merge(ud,uu)
   uduuui <- merge(uduu,ui)
   gassign('uduu','uduu')
   gassign('uduuui','uduuui')
}

# not-so-stealth global assignment
gassign <- function(gname,rhsname) {
   cmd <- paste(gname,'<<-',rhsname)
   eval(parse(text=cmd),envir=parent.frame())
}

