\name{findYdotsMM,findYdotsMLE,predict.ydotsMM,predict.ydotsMLE,plot.ydotsMM,trainMM,trainMLE}
\alias{findYdotsMM}
\alias{findYdotsMLE}
\alias{trainMM}
\alias{trainMLE}
\alias{predict.ydotsMM}
\alias{plot.ydotsMM}
\alias{predict.ydotsMLE}

\title{Covariate-Based, Latent-Factor Recommender Systems}

\description{
Tools to incorporate user and item information into latent-factor 
recommender system methodology, and to add parallel computation
capability.  Various plots can be displayed.
}

\usage{
findYdotsMM(ratingsIn,regressYdots=FALSE,minN) 
trainMM(ratingsIn,regressYdots=FALSE,cls=NULL) 
predict.ydotsMM(ydotsObj,testSet) 
plot.ydotsMM(ydotsObj,ratingsIn) 
findYdotsMLE(ratingsIn,cls=NULL) 
trainMLE(ratingsIn,cls=NULL) 
predict.ydotsMLE(ydotsObj,testSet) 
}

\arguments{
  \item{ratingsIn}{Input data frame.  Within-row format is UserID,
     ItemID, Rating and optional covariates.}
  \item{regressYdots}{If TRUE, apply linear regression to the latent
     factors.}
  \item{cls}{An R \code{parallel} cluster.}
  \item{minN}{If a prediction is to be made on a user with fewer than
     this number of ratings and if there are covariates, predict from
     the covariates.}
  \item{ydotsObj}{An object of class \code{'ydotsMM'} or \code{'ydotsMLE'}.}
  \item{testSet}{A data frame consisting of cases to be predicted.
     Format is the same as \code{ratingsIn}, except that there is no
     ratings column.}
}

\details{

   Note:  This software assumes that user and item ID number are
   consecutive, starting with 1.

   All functions here implement latent-factor models for recommender
   systems.  They add the capability of using covariates, and in some
   cases enable parallel computation.

   The basic model is 

   mean rating = overall mean + user effect + item effect

   Adding covariates, this becomes

   mean rating = linear covariates effect + user effect + item effect

   The functions \code{findYdotsMM} and \code{findYdotsMLE} work on a
   training set, returning objects that later can be used to predict new
   cases.

   The \code{findYdotsMLE} function is primarily a wrapper that sets up
   Maximum Likelihood Estimation (assuming normal user and item effects)
   for a crossed-effects model in the \code{lme4} function \code{lmer}.
   As the computation for that function can be lengthy,
   \code{findYdotsMLE} also enables parallelizing the computation.

   The \code{findYdotsMM} function uses the Methods of Moments instead
   of MLE.  It is much faster, and thus at present does not have a
   parallel computation capability.

   In order to accommodate possibility that the user latent factor is a
   stronger predictor than the one for items, or vice versa, the option
   \code{regressYdots = TRUE} for \code{findYdotsMM} regresses ratings
   against user and item latent factors, enabling later prediction using
   the resulting coefficients.  This is not needed for
   \code{findYdotsMLE}, since \code{lmer} calculates the Best Linear
   Unbiased Predictors, thus indirectly assigning weights to the user
   and item effects.

   Plotting:  Calling \code{plot(ydotsObj,ratingsIn)} invokes
   \code{plot.ydotsMM}.  Several plots are displayed, including density
   estimates for the user and item random effects, and a smoothed
   scatter plot for the joint density of those effects.

}

\value{

   The functions \code{findYdotsMM} and \code{findYdotsMLE} return
   objects of class \code{} \code{'ydotsMM'} and \code{'ydotsMLE'},
   respectively.
   
   The functions \code{predict.ydotsMM} and \code{predict.ydotsMLE}
   return a vector of predicted ratings.
}

\examples{
# lme4 data set, needs some prep
data(InstEval)
ivl <- InstEval
# convert from factors
ivl$s <- as.numeric(ivl$s)
ivl$d <- as.numeric(ivl$d)
ivl$studage <- as.numeric(ivl$studage)
ivl$lectage <- as.numeric(ivl$lectage)
ivl$service <- as.numeric(ivl$service)
# make correct format, choose 
ivl <- ivl[,c(1,2,7,3:6)]
# create dummy variables in place of dept
library(dummies)
dms <- dummy(ivl$dept)
dms <- as.data.frame(dms)
# numeric names won't work, so change
dnms <- names(dms)
for (i in 1:length(dnms)) 
   dnms[i] <- paste('x',dnms[i],sep='') 
names(dms) <- dnms
dms$dept2 <- NULL
ivl$dept <- NULL
ivl <- cbind(ivl,dms)
# run the training data, no covariates
ydout <- trainMLE(ivl[,1:3]) 
# form a test set to illustrate prediction
testSet <- ivl[c(3,8),]
head(testSet)
# say want to predict how well students 1 and 3 would like instructor 12
testSet[1,2] <- 12
testSet[2,2] <- 12
# predict
predict(ydout,testSet[,1:2])  # 4.272660 4.410612
# try using the covariates
ydout <- trainMM(ivl)
predict(ydout,testSet[,-3])  # 5.141009 5.137111 
}

\author{
Norm Matloff and Pooja Rajkumar
}

