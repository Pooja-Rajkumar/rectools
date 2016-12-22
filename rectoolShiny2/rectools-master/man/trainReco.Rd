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
           \code{trainReco}.}
        \item{predSet}{Data to be predicted, having within-row format
           (UserID, ItemID).}
	\item{rnk}{Desired rank for the matrix factors.}
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

The function \code{predict.Reco} is a method for the generic function
\code{predict}.
	
}

\examples{
       ivl <- InstEval 
       ivl$s <- as.numeric(ivl$s) 
       ivl$d <- as.numeric(ivl$d) 
       ivl3 <- ivl[,c(1,2,7)]
       trn <- trainReco(ivl3)
       onerec <- ivl3[1,]  # form dummy 1-rec data frame
       # how would student 788 would like lecturer 28
       onerec$s <- 788  
       onerec$d <- 28  
       onerec <- onerec[,-3]
       predict(trn,onerec)  # 2.23
       ############################
       # Assume ratingsIn is the 200 K movielens data (found online)
       trainSet = getTrainSet(ratingsIn) 
       testSet = getTestSet(ratingsIn,trainSet) # Suppose testSet has user 196 and item 242 in first row and user 186 and item 302 in the second row
       result = trainReco(ratingsIn) 
       preds = predict(result,testSet) # Returns a vector of all the predictions 
       firstItem <- preds[1] # 3.537769 is the predicted value for user 196 and item 242
       secondItem <- preds[2] # 3.305844 is the predicted value for user 186 and item 302 
}

\author{
	Pooja Rajkumar and Norm Matloff
}
