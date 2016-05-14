\name{findYdotsMM,findYdotsMLE, predict.ydotsMM, predict.ydotsMLE}k
\alias{findYdotsMM}
\alias{findYdotsMLE}
\alias{predict.ydotsMM}}
\alias{predict.ydotsMLE}

\title{Covariate-Based Recommender Systems}

\description{
Tools to incorporate user and item information into recommender system
methodology.
}

\usage{
findYdotsMM(ratingsIn,cls=NULL) 
predict.ydotsMM(ydotsObj,testSet) 
findYdotsMLE(ratingsIn,cls=NULL) 
predict.ydotsMLE(ydotsObj,testSet) 
}

\arguments{
  \item{ratingsIn}{Input data frame.  Within-row format is UserID,
     ItemID, Rating and optional covariates.}
  \item{cls}{An R \code{parallel} cluster.}
}

\details{

   The \code{covNMF} function inputs a user ratings matrix, and if
   covariate information is present, i.e. there are more than three
   columns, regresses the ratings against the covariates.  This is used
   to compute predicted values of the ratings, given the covariates.
   The predicted values are subtracted from the actual ratings,
   truncating below at 0 if necessary.  (If no covariate information is
   present, then the ratings do not change.)

   Then the nonnegative matrix factorization is performed.  First, the
   full matrix is constructed from (the first three columns of)
   \code{narrowrat}.  NMF is applied to the result, by calling
   \code{getNMF}.

   If several values of \code{k} will be tried, one should form the
   predicted values separately, by calling \code{getPreds}, then adjusting
   the input matrix, then calling \code{getNMF}.

}

\value{

The functions \code{covNMF} and \code{getNMF} return the 
matrix approximating the one built by \code{buildMatrix}, thus providing
estimated values for the missing ratings.

}

\author{
Norm Matloff
}

