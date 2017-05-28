\name{xvalMM,xvalMLE,xvalReco,xvalCos,xvalMultiplic,getTestSet,getTrainSet,plot.xvalb}
\alias{xvalMM}
\alias{xvalMLE}
\alias{xvalReco}
\alias{xvalCos}
\alias{xvalMultiplic}
\alias{plot.xvalb}
\alias{getTestSet}
\alias{getTrainSet}

\title{Cross Validation}

\description{
Cross-validation for the various methods in this package, with parallel
computation capability in some cases.
}

\usage{
        xvalMM(ratingsIn, trainprop = 0.5, regressYdots = FALSE, minN = 0)
        xvalMLE(ratingsIn, trainprop = 0.5, cls = NULL) 
	xvalReco(ratingsIn, trainprop = 0.5,cls = NULL,rnk = 10)
        xvalCos(ratingsIn, k, usrCovs = NULL, itmCats = NULL, wtcovs = NULL, 
           wtcats = NULL, trainprop = 0.5)
        xvalMultiplic(ratingsIn) 
	getTrainSet(ratingsIn,trainprop)
	getTestSet(ratingsIn,trainSet)
        plot.xvalb(xvalObj,whichIdxs=NULL)
}

\arguments{
	\item{ratingsIn}{Input data frame. Within-row format is UserID,
	ItemID, rating}
        \item{minN}{Applies to situations in which covariates are
        present. In predicting for user i, then either Yi. or the
        regression-based prediction will be used, depending on whether
        N_i >= minN.}
	\item{trainprop}{The fraction of ratingsIn we want to use for our 
	training set}
	\item{cls}{R \code{paralell} cluster.}
        \item{regressYdots}{If TRUE, allow for different weights on
           alpha and beta; see documentation for \code{findYdotsMM}.}
	\item{rnk}{Desired rank for \code{recosystem} analysis.}
        \item{xvalObj}{An object of class \code{'xvalb'}.}
        \item{whichIdxs}{A vector of indices of rows of the test set to
           be used in plotting.}
}

\details{

These functions perform cross-validation using the various methods in
this package. A number of measures of prediction accuracy are output
(see Value), including comparison to accuracy obtained by simply
predicting by a constant, thus enabling one to ask the question, Are we
predicting better with our model than by chance?
		
The functions \code{getTrainSet} and \code{getTestSet} are helper
functions to generate the training and test sets.

The function \code{plot.xvalb} is a method for the generic function
\code{plot}.  It plots the estimated density of the predicted ratings,
and a smoothed scatter plot of the predicted ratings against the actual
ones.  If \code{whichIdxs} is specified, the user can choose to plot
only a subset of the data, say rows corresponding to large values of a
covariate.

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
}

\examples{
       ivl <- InstEval 
       ivl$s <- as.numeric(ivl$s) 
       ivl$d <- as.numeric(ivl$d) 
       ivlsdy <- ivl[,c(1,2,7)]
       # Test for xvalReco
       res <- xvalReco(ivlsdy)
      # Get accuracy of xvalReco test
       res$acc	
       xvoutmm <- xvalMM(ivlsdy) 
       xvoutmm$acc
       # plot(xvoutmm)  
       xvoutcos5 <- xvalCos(ivlsdy,5)  # takes time
       xvoutcos5$acc
}

\author{
	Pooja Rajkumar and Norm Matloff
}
