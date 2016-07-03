\name{trainReco,predict.Reco}
\alias{trainReco}
\alias{predict.Reco}

\title{Parallelized Recosystem}

\description{
Tools to utilize specific aspects of the recosystem package 
as well as add a parallel computation capability.
}

\usage{
	trainReco(ratingsIn,rnk = 10)
	predict.Reco(recoObj,predSet)
}

\arguments{
	\item{ratingsIn}{Input data frame, training set. Within-row format 
           is (UserID, ItemID, rating).}
        \item{recoObj}{Object of type \code{'Reco'}, output of
           \code{trainReco}.
        \item{predSet}{Data to be predicted, with within-row format
           (UserID, ItemID).
        in the formt of \code{rating}
	\item{rnk}{Desired rank for the matrix factors}
}

\details{
The function \code{trainReco} simply calls \code{r$train} on the input
data.

The latter function returns several key components, including:

   \itemize{
   
   \item P: This is the user score matrix, in which a row 
   represents a user, and each column is a latent factor. 
   
   \item Q: This is the item score matrix, in which a row represents 
   an item, and each column is a latent factor.  
   }

The product of these two matricies consists of the predicted ratings of
all users on all items.
	
}

\value{
The functions getTestSet and getTrainSet return the testing set and trainReco set respectively. trainReco returns the result of calling train on r, and predict.Reco multiplies the predicted values within the result(P and Q) to get the predicted rating/


		preference. xValReco returns a class called result, which contains the following components: 
			- ndata: the row number of data
			- trainprop: the trainReco proportion
			- accmeasure: the selected measurement for accuracy (default is exact)
			- numpredna: the number of NA predicted values 
			- acc: the accuracy of your predicted values from recosystem 
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
