\name{cosine,predict.usrData}
\alias{cosine}
\alias{predict.usrData}

\title{@title predict newData argument from origData argument}

\usage{
  predict.usrData(origData, newData, newItem, k, wtcovs = NULL, wtcats = NULL)
}

\arguments{
  \item{origData:}{training set, object of class 'usrData' (see file findUsrItmData.R)}

  \item{newData:}{data point (just one for now) to be predicted, object of class 'usrDatum'}

  \item{newItem:}{ID of the item rating to be predicted for the user in newData}

  \item{wtcovs:}{weight to put on covariates; NULL if no covs}

  \item{wtcats:}{weight to put on item categories; NULL if no cats}
}

\value{
  predicted ratings for newData
}

\description{
  rating prediction via nearest neighbors, via \code{cosDist} (inner product);
  the latter, though standard, has certain problems (e.g., its
  scale-free nature), and other choices for distance measure will be added.
  covariates (e.g. age, gender) and item type preferences (e.g.
  preferred movie genres) are allowed
}

\author{
	Vishal Chakraborty
}

\examples{
     ivl <- InstEval 
     ivl$s <- as.numeric(ivl$s) 
     ivl$d <- as.numeric(ivl$d) 
     ivl3 <- ivl[,c(1,2,7)]
     usrdata <- formUserData(ivl) 
     predict(usrdata,3,8,10) 
}
