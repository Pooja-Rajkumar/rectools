\name{xvalMM,xvalMLE,xvalReco,xvalCos,getTestSet,getTrainSet}
\alias{xvalMM}
\alias{xvalMLE}
\alias{xvalReco}
\alias{xvalCos}
\alias{getTestSet}
\alias{getTrainSet}

\title{Cross Validation}

\description{
Cross-validation for the various methods in this package, with parallel
computation capability in some cases.
}

\usage{
        xvalMM(ratingsIn, trainprop = 0.5, regressYdots = FALSE, cls = NULL)
        xvalMLE(ratingsIn, trainprop = 0.5, cls = NULL) 
	xvalReco(ratingsIn, trainprop = 0.5,cls = NULL,rnk = 10)
        xvalCos(ratingsIn, k, usrCovs = NULL, itmCats = NULL, wtcovs = NULL, 
           wtcats = NULL, trainprop = 0.5)
	getTrainSet(ratingsIn,trainprop)
	getTestSet(ratingsIn,trainSet)
}

\arguments{
	\item{ratingsIn}{Input data frame. Within-row format is UserID,
	ItemID, rating}
	\item{trainprop}{The fraction of ratingsIn we want to use for our 
	training set}
	\item{cls}{R \code{paralell} cluster.
        \item{regressYdots}{If TRUE, allow for different weights on
           alpha and beta; see documentation for \code{findYdotsMM}.}
	\item{rnk}{Desired rank for \code{recosystem} analysis.}
}

\details{

These functions perform cross-validation using the various methods in
this package. A number of measures of prediction accuracy are output
(see Value), including comparison to accuracy obtained by simply
predicting by a constant, thus enabling one to ask the question, Are we
predicting better with our model than by chance?
		
The functions \code{getTrainSet} and \code{getTestSet} are helper
functions to generate the training and test sets.

}

\value{

The \code{xval} functions return an object of class \code{'xvalb'}, with
the following components:

   \itemize{

   \item \code{ndata:} Number of rows in the original input data
   \item \code{trainprop:} As above.
   \item \code{numpredna:} Number of rows in the test set for which
      prediction was not possible.
   \item \code{acc:} Accuracy measures; see below.
   \item \code{idxs:} Indices in the original input data selected for
      the test set.
   \item \code{actuals:} The actual ratings in the test set.
   \item \code{preds:} The predicted ratings in the test set.
   }

The \code{acc} component is an R list with these elements:

   \itemize{
   \item \code{exact:}{Proportion of ratings predicted exactly
      correctly.}
   \item \code{mad}{Mean absolute prediction error.}
   \item \code{rms}{L2 ("root mean squared") prediction error.}
   \item \code{overallexact:}{Proportion of ratings predicted exactly
      correctly by simply taking our guess to be the (rounded) 
      overall mean item rating.}
   \item \code{overallmad:}{Mean absolute prediction error resulting
      from simply taking our guess to be the overall mean item rating.}
   \item \code{overallmad:}{L2 prediction error resulting
      from simply taking our guess to be the overall mean item rating.}
   }

\examples{
       ivl <- InstEval 
       ivl$s <- as.numeric(ivl$s) 
       ivl$d <- as.numeric(ivl$d) 
       xvalReco(ivl[,c(1,2,7)]) 
}

\author{
	Pooja Rajkumar and Norm Matloff
}
