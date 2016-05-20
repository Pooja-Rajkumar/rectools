\name{trainReco,predict.Reco,getTestSet,xvalReco,getTrainSet}
\alias{trainReco}
\alias{predicition}
\alias{getTestSet}
\alias{getTrainSet}
\alias{xvalReco}

\title{Parallelized Recosystem}

\description{
	Tools to utilize specific aspects of the recosystem package as well as add a parallel computation capability
}

\usage{
	xvalReco(ratingsIn,trainprop = 0.5,accmeasure = c('exact','mad',
		'rms'),cls = NULL,rank = 10)
	trainReco(trainSet,trainprop = 0.5,rank = 10)
	predict.Reco(recoObj,testSet)
	getTrainSet(ratingsIn,trainprop)
	getTestSet(ratingsIn,trainSet)
}

\arguments{
	\item{ratingsIn}{Input data frame. Within row format is UserID,
	ItemID, rating}
	\item{trainprop}{The fraction of ratingsIn we want to use for our 
	trainRecoSet. This is optional.}
	\item{accmeasure}{The measure of our accuracy. We can test for exact one to one accuracy, or calculate the mean of the absolute value of the difference between the predicted and actual value, or square this difference, take the mean, and then square that. Each refers to exact, mad, and rms respectively. These are optional.}
	\item{cls}{If the user wishes to parallelize, then cls is the clusters. This is optional.}
	\item{rank}{The optional rank the user wants to use for recosystems}
}

\details{
	All the functions here are used by our main function, xvalReco, in order to perform a trainReco, predict.Reco, and accuracy measure for our data set. This function is also enabled for parallelization.
	The function trainReco simply calls r$train on our test set. 
	r$train returns several key components:
		- P: This is the output user score matrix in which the row in the file stands for a user, and each column stands for a latent factor. 
		- Q: This is the output item score matrix in which the row in the file stands for an item, and each column stands for a latent factor. 
	Prediction multiplies these two matricies in order to get the true predicted rating of users. 
	
	getTrainSet partitions the set based on our trainprop into a trainReco set, and returns this trainReco set. getTestSet does the same for the test set. 
	
	xValReco calls these helper functions to partition the sets, train them, predict them, and then calculates the accuracy based on the accuracy measure the user asks for. xValReco also has a parallelization capability, if the user sends a cluster to the function, making it much faster. It is highly recommended to use the parallelized version of recosystems as multiplying the P and Q matricies is very time expensive. 
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
