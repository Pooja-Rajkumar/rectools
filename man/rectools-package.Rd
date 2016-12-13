
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

\bold{Raw input data:}  

Standard form, one row per rating, with first three columns being user
ID, item ID and rating, followed by optional covariates.  \bold{Note:}
Most functions assume that user and item IDs are numbered consecutively,
starting at 1.

\bold{List of major functions:}

   \preformatted{
   matrix factorization model (MFM):

      trainReco():  fit MFM
      predict.Reco():  predict from MFM 
      xvalReco():  cross-validation evaluation of MFM

   statistical random effects additive model:

      findYdotsMLE():  fit Max. Likelihood model
      findYdotsMM():  fit Method of Moments model
      trainMLE():  alias for findYdotsMLE()
      trainMM():  alias for findYdotsMM()
      predict.ydotsMLE():  predict from MLE
      predict.ydotsMM():  predict from MM
      xvalMLE):  cross-validation of MLE
      xvalMM):  cross-validation of MM
      findMultiplicYdots():  fit multiplicative MM model, binary Y

   statistical random effects multiplicative model:

      findYdotsMultiplicYdots(): fit mult. model for binary Y 
      predict.MMmultiplic(): predict from that model 

   k-nearest neighbor model:

      formUserData():  aggregate items for each user, etc.
      cosDist():  calculates cosine distance on users with certain 
         item patterns
      predict.usrData():  predict using kNN

   misc.:

      focusGrp():  finds a "focus group," i.e. some "representative" users
      ratingness():  find number of ratings for each user or 
         number of users per rating
      covratingness():  for each row of the raw input data

   }

}


\examples{

# try lme4 data set InstEval, needs some prep first.
data(InstEval)
ivl <- InstEval
# convert factors to numeric:
ivl$s <- as.numeric(ivl$s)
ivl$d <- as.numeric(ivl$d)
ivl$studage <- as.numeric(ivl$studage)
ivl$lectage <- as.numeric(ivl$lectage)
ivl$service <- as.numeric(ivl$service)
# make correct format, and choose covs:
ivl <- ivl[,c(1,2,7,3:6)]
# create dummy variables in place of dept:
library(dummies)
dms <- dummy(ivl$dept)
dms <- as.data.frame(dms)
dms$dept2 <- NULL
ivl$dept <- NULL
ivl <- cbind(ivl,dms)
# run the training data, no covariates:
# ydout <- findYdotsMM(ivl[,1:3])
ydout <- trainMM(ivl[,1:3])
# form a test set to illustrate prediction:
testSet <- ivl[c(3,8),]  # these happen to be students 1, 3
# say want to predict how well students 1 and 3 would like instructor 12
testSet[1,2] <- 12
testSet[2,2] <- 12
# predict:
predict(ydout,testSet[,1:2])  
# try using the covariates:
# ydout <- findYdotsMM(ivl)
ydout <- trainMM(ivl)
predict(ydout,testSet[,-3])  

}

