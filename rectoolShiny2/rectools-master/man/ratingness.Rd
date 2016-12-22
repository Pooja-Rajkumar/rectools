\name{ratingness,covratingness}
\alias{ratingness}
\alias{covratingness}

\title{Degrees of Rating Activity}

\description{
In recommender system data, some users are more active raters than
others, i.e. they rate more items than do others.  These simple functions
facilitate taking "ratingness" into account, say as a covariate.
}

\usage{
ratingness(ratingsIn,splitCol)
covratingness(ratingsIn,splitCol)
}

\arguments{
  \item{ratingsIn}{Input data frame.  Within-row format is UserID,
     ItemID, Rating and optional covariates.}
  \item{splitCol}{Column to count ratings for -- 1 for user ratings, 2
     for item ratings.}
}

\details{

   The fact that some users rate more items may be useful, say as a
   predictor.  A typical usage of the latter is to call
   \code{covratingness} and then append the result to the input data
   frame.

}

\examples{
rtin <- data.frame(uid = c(1,3,2,1,2),itemid = c(1,2,1,2,2), rats=c(2,5,3,5,1))
ratingness(rtin,1)  
# returns (2,2,1), meaning that user 1 submitted 2 ratings, etc.
> covratingness(rtin,1)
# returns a 5-element vector, corresponding to the 5 rows of rtin
rtin$nrats <- covratingness(rtin,1)
# now usable as a predictor
}

\author{
Norm Matloff 
}

