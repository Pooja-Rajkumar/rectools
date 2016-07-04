\name{cosDist,formUserData,predict.usrData}
\alias{cosDist}
\alias{formUserData}
\alias{predict.usrData}

\title{Cosine-Based Analysis}

\description{
Similarity-based analysis via inner products of user ratings, and
possibly other variables.
}


\usage{
formUserData(ratingsIn, usrCovs = NULL, itmCats = NULL, fileOut = "") 
cosDist(x, y, wtcovs, wtcats)
predict.usrData(origData, newData, newItem, k, wtcovs = NULL, wtcats = NULL)
}

\arguments{
	\item{ratingsIn}{Input data frame, training set. Within-row format 
           is (UserID, ItemID, rating).}
        \item{usrCovs}(Data frame of user covariates, e.g. gender and
           age, one row per user.}
        \item{itmCats}(Data frame of item categories, e.g. gender and
           age, one row per user.}
        \item{x}{Object of class \code{usrDatum}.}
        \item{y}{Object of class \code{usrDatum}.}
        \item{wtcovs}{Weight to be placed on covariates, relative to
           ratings variables.}
        \item{wtcats}{Weight to be placed on categories, relative to
           ratings variables.}
        \item{origData}{Object of class \code{'usrData'}, serving as the
           training set.}
        \item{newData}{Object of class \code{'usrDatum'}, to be
           predicted.}  
	\item{newItem}{The item ID of the rating to be predicted.}
	\item{k}{Number of nearest neighbors.}
        \item{fileOut}{A file name.}
}

\details{

The function \code{formUserData} inputs the usual (user ID,item ID,rating)
data, and outputs an R list, of class \code{usrData} that has one 
element per user ID.  That element, of class \code{usrDatum}, has the
following components:

\itemize{

   \item \code{userID:} User ID.
   \item \code{itms:} Vector of IDs for items rated by this user.
   \item \code{ratings:} Vector of ratings for those items.
   \item \code{usrCovs:} Vector of values of covariates, e.g. gender and
      age, for this user.
   \item \code{itmCats:} Vector of proportions for the item categories
      (need not sum to 1) for this user.  The j-th one is the proportion
      of items rating by this user in item category j.
}

In \code{cosDist}, the "distance" (not actually a metric) between
numeric vectors u and v is defined to be (u,v) / sqrt((u,u) (v,v)),
where ( , ) means inner product.  The function \code{cosDist} finds this
for two objects of class \code{'usrDatum'}, with the inner product being
taken on the ratings contained in each of these objects (zeroes are used
for the ratings that are unknown), as well as the covariates and
category data if any. 

The function \code{predict.usrData} uses a training set, of class
\code{usrData}, to predict one new data point, of class \code{usrDatum}.

}

\examples{
ivl <- InstEval 
ivl$s <- as.numeric(ivl$s) 
ivl$d <- as.numeric(ivl$d) 
ivl3 <- ivl[,c(1,2,7)]
ydo <- findYdotsMM(ivl3)
focusGrp(ydo,ivl3)  # users 1181, 1462, ... seem to be representative
}

\author{
	Pooja Rajkumar and Norm Matloff
}
