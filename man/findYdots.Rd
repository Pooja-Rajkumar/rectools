\name{findYdotsMM,findYdotsMLE, predict.ydotsMM, predict.ydotsMLE}
\alias{findYdotsMM}
\alias{findYdotsMLE}
\alias{predict.ydotsMM}
\alias{predict.ydotsMLE}

\title{Covariate-Based, Latent-Factor Recommender Systems}

\description{
Tools to incorporate user and item information into latent-factor 
recommender system methodology, and to add parallel computation
capability.
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
  \item{ydotsObj}{An object of class \code{'ydotsMM'} or \code{'ydotsMLE'}.}
  \item{testSet}{A data frame consisting of cases to be predicted.
     Format is the same as \code{ratingsIn}, except that there is no
     ratings column.}
}

\details{

   All functions here implement latent-factor models for recommender
   systems.  They add the capability of using covariates, and in some
   cases enable parallel computation.

   The functions \code{findYdotsMM} and \code{findYdotsMLE} work on a
   training set, returning objects that later can be used to predict new
   cases.

}

\value{

   The functions \code{findYdotsMM} and \code{findYdotsMLE} return
   objects of class \code{} \code{'ydotsMM'} and \code{'ydotsMLE'},
   respectively.
   
   The functions \code{predict.ydotsMM} and \code{predict.ydotsMLE}
   return a vector of predicted ratings.
}

\author{
Norm Matloff and Pooja Rajkumar
}

