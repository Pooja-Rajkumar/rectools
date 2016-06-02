# rectools
Advanced package for recommender systems.

We allow the user to incorporate user and item covariate information,
and offer other capabilities not seen in other packages, as well as
adding parallel computation to some popular methods.

QUICK FIRST EXAMPLES:

A simple latent factor model is

E(Yij) =  mu + alphai + betaj

where Yij is the rating of item j by user i, with alphai and betaj
being specific latent effects for user i and item j.

A simple Method of Moments approach would estimate alphai by
Yi. - Y.., where the first term is the mean of all observed ratings by
user i and the second is the overall mean of all ratings.  We estimate
betaj similarly.  The predicted value of Yij is then

Yi. + Y.j - Y..

Under a Maximum Likelihood approach, the alphai and betaj are assumed to
have a normal distribution.  

Let's try it out (some output has been omitted for clarity):

> # Try lme4 data set, needs some prep first.
> data(InstEval)
> ivl <- InstEval
> # Convert factors to numeric:
> ivl$s <- as.numeric(ivl$s)
> ivl$d <- as.numeric(ivl$d)
> ivl$studage <- as.numeric(ivl$studage)
> ivl$lectage <- as.numeric(ivl$lectage)
> ivl$service <- as.numeric(ivl$service)
> # Make correct format, choose covs:
> ivl <- ivl[,c(1,2,7,3:6)]
> # Create dummy variables in place of dept:
> library(dummies)
...
> dms <- dummy(ivl$dept)
> dms <- as.data.frame(dms)
> dms$dept2 <- NULL
> ivl$dept <- NULL
> ivl <- cbind(ivl,dms)
# Run the training data, no covariates:
# Form a test set to illustrate prediction:
> testSet <- ivl[c(3,8),]  # these happen to be students 1, 3
# Say want to predict how well students 1 and 3 would like instructor 12
> testSet[1,2] <- 12
> testSet[2,2] <- 12
> # Predict:
> predict(ydout,testSet[,1:2])  
[1] 4.272660 4.410612
> # Try using the covariates:
> ydout <- findYdotsMLE(ivl)
> predict(ydout,testSet[,-3])  
      [,1]
3 3.286828
8 3.551587

