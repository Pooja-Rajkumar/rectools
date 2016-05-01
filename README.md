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

> library(lme4)
> data(InstEval)
> ivl <- InstEval
> ivl$s <- as.numeric(ivl$s)
> ivl$d <- as.numeric(ivl$d)
> ivl$studage <- as.numeric(ivl$studage)
> ivl$lectage <- as.numeric(ivl$lectage)
> ivlnocovs <- ivl[,c(1,2,7)]
> ivlcovs <- ivl[,c(1,2,7,3:6)]
> xva <- xvalMM(ivlnocovs)
> xva
$ndata
[1] 73421

$trainprop
[1] 0.5

$accmeasure
[1] "exact"

$numpredna
[1] 48

$acc
[1] 0.2732182

attr(,"class")
[1] "xvalb"

# in cross-validation, the simple latent factor model predicted 27% of
# the ratings exactly correctly

> xvalMLE(ivlnocovs)
$ndata
[1] 73421

$trainprop
[1] 0.5

$accmeasure
[1] "exact"

$numpredna
[1] 29

$acc
[1] 0.2315305

attr(,"class")
[1] "xvalb"
# the accuracy was somewhat less for MLE

# here is how to predict individual cases
# simple latent factors
> fyd <- findYdots(ivlnocovs)
> predict(fyd,matrix(c(3,88),nrow=1))
4.109381 
# MLE
> fydmle <- findYdotsMLE(ivlnocovs)
> predict(fydmle,matrix(c(3,88),nrow=1))
3.787764 

INCORPORATING COVARIATES:

The package allows the user to incorporate covariate data, and to run
these and other methods in parallel.

> xvalMM(ivlcovs)
...
$acc
[1] 0.2726306

Actually, using the covariates in this case didn't help.

