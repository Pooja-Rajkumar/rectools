
\name{rectools-package}
\alias{rectools-package}
\alias{rectools}
\docType{package}

\title{
Advanced Recommender System
}

\description{

Toolbox for recommender systems, including novel methods,
plotting, parallel computation, and so on.

}

\details{
\tabular{ll}{
Package: \tab rectools\cr
Type: \tab Package\cr
Version: \tab 1.0.0\cr
Date: \tab 2016-10-29\cr
License: \tab GPL (>= 2)\cr
}

Raw input data:  Standard form, one row per rating, with first three
columns being user ID, item ID and rating.

List of functions:

   \preformatted{

   matrix factorization model (MFM):

      trainReco():  fit MFM
      predict.Reco():  predict from MFM 
      xvalReco():  cross-validation evaluation of MFM

   statistical random effects model:

      findYdotsMLE():  fit Max. Likelihood model
      findYdotsMM():  fit Method of Moments model
      predict.ydotsMLE():  predict from MLE
      predict.ydotsMM():  predict from MM
      xvalMLE):  cross-validation of MLE
      xvalMM):  cross-validation of MM
      findMultiplicYdots():  fit multiplicative MM model, binary Y

   k-nearest neighbor model:

      formUserData():  aggregate items for each user, etc.
      cosDist():  calculates cosine distance on users with certain 
         item patterns
      predict.usrData():  predict using kNN

   misc.:

      ratingness():  find number of ratings for each user or 
         number of users per rating
      covratingness():  for each row of the raw input data
   
      

   }



}


\examples{
library(parallel)

# Try lme4 data set, needs some prep first.
data(InstEval)
ivl <- InstEval
# Convert factors to numeric:
ivl$s <- as.numeric(ivl$s)
ivl$d <- as.numeric(ivl$d)
ivl$studage <- as.numeric(ivl$studage)
ivl$lectage <- as.numeric(ivl$lectage)
ivl$service <- as.numeric(ivl$service)
# make correct format, choose covs:
ivl <- ivl[,c(1,2,7,3:6)]
# create dummy variables in place of dept:
library(dummies)
dms <- dummy(ivl$dept)
dms <- as.data.frame(dms)
dms$dept2 <- NULL
ivl$dept <- NULL
ivl <- cbind(ivl,dms)
# run the training data, no covariates:
# form a test set to illustrate prediction:
testSet <- ivl[c(3,8),]  # these happen to be students 1, 3
# say want to predict how well students 1 and 3 would like instructor 12
testSet[1,2] <- 12
testSet[2,2] <- 12
# predict:
predict(ydout,testSet[,1:2])  
# Try using the covariates:
ydout <- findYdotsMLE(ivl)
predict(ydout,testSet[,-3])  

}

