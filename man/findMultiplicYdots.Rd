\name{trainMultiplic,predict.MMmultiplic}
\alias{trainMultiplic}
\alias{predict.MMmultiplic}

\title{Multiplicative Model for Collaborative Filtering}

\description{
Multiplicative model for binary Y, analogous to the usual additive model
for general Y.
}

\usage{
findsMultiplicYdots(ratingsIn) 
predict.MMmultiplic(multiplicObj, testSet) 
}

\arguments{
  \item{ratingsIn}{Input data frame.  Within-row format is UserID,
     ItemID, Rating and optional covariates.}
  \item{multiplicObj}{An object of class \code{'MMmultiplic'}.}
  \item{testSet}{A data frame consisting of cases to be predicted.
     Format is the same as \code{ratingsIn}, in the first two columns.
     Further columns, if any, are ignored.}
}

\details{

   Note:  This software assumes that user and item ID number are
   consecutive, starting with 1.

   The basic model is 

   probability of a 1 rating = b0 + b1 * alpha * beta

   where alpha and beta are effects for the given user and item,
   calculated as the mean Y values for the given user and given item,
   respectively.

   Currently the code works only on the original data set, to predict
   the missing values.  Covariates are not yet allowed.

}

\value{

   The function \code{findMultiplicYdots} returns an object of class
   \code{'MMmultiplic'}, consisting of the alpha and beta vectors, the
   vector (b0,b1) and the mean Y value nu.
   
   The function \code{predict.MMmultiplic} returns a 2-column matrix,
   consisting of the estimated probabilities of 1 and the rounded
   version of the latter. 

}

\examples{
# lme4 data set, needs some prep
ivl <- InstEval
ivl <- ivl[,c(1,2,7)]
# convert from factors
ivl$s <- as.numeric(ivl$s)
ivl$d <- as.numeric(ivl$d)
ivl$y <- as.numeric(ivl$y)
# make it binary
ivl$y <- as.integer(ivl$y >= 4)
# run the training data
ydout <- trainMultiplic(ivl[,1:3]) 
# form a test set to illustrate prediction; make template first
testSet <- ivl[c(3,8),]
# say want to predict whether students 3 and 8 would rate instructor 12
# at least a 4
testSet[1,2] <- 12
testSet[2,2] <- 12
predict(ydout,testSet)
}

\author{
Norm Matloff 
}

