# rectools

## Advanced Package for Recommender Systems

## FEATURES:

* Incorporate user and item covariate information, including item
  category preferences.

* Parallel computation.

* Novel variations on latent factor model.

* Plotting.

* Focus group finder.

* NMF, ANOVA, cosine models all in one package.

* Some functions new, others enhancements of existing libraries.

## Overview and Examples

### Random effects ANOVA model:

A simple latent factor model is

E(Y) =  &mu; + &alpha;<sub>i</sub> + &beta;<sub>j</sub>

where Y<sub>ij</sub> is the rating, &alpha; and &beta;<sub>j</sub> being
specific latent effects for the given user and item.

A simple Method of Moments approach would estimate &alpha; for user i by
Yi. - Y.., where the first term is the mean of all observed ratings by
user i and the second is the overall mean of all ratings.  We estimate
&beta;<sub>j</sub> for item j similarly, and estimate &mu; by the overall
mean Y..  The predicted value of Y<sub>ij</sub> is then

Yi. + Y.j - Y..

Computation is simple, conducted by our function **findYdotsMM**;
prediction is done on the output by our function **predict.ydotsMM**.

A novel enhancement in the package is to allow for different weights to
be given to the &alpha;<sub>i</sub> and &beta;<sub>j</sub> components in
the MM version.

Under a Maximum Likelihood approach, &alpha;<sub>i</sub> and
&beta;<sub>j</sub> are assumed to have independent normal distributions
with different variances.  Here we piggyback R's **lme4** package,
forming a wrapper for our application, and adding our function
**predict.ydotsMLE**, also a wrapper suited for our context.  Since MLE
computation can be voluminous, our package offers a parallel version.

Covariates are allowed for both the MM and MLE versions.

### Let's try it:

(Some output has been omitted for clarity.)

```
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
```

## Matrix factorization model:

Let A denote the matrix of ratings, with Y<sub>ij</sub> in row i, column
j.  Most of A is unknown, and we wish to predict the unknown values.
*Nonnegative matrix factorization* does this as follows:

We find nonnegative matrices W and H, each of rank k, such that A is
approximately equal to the product WH.  Here k is typically much smaller
than the number of rows and columns of A, and is a tuning parameter,
kept small to avoid overfitting but large enough to capture most of the
structure of the data.

Here we piggyback on the R package **recosystem**, adding convenient
wrappers and adding a parallel computation capability.  See the
functions **trainReco**, **predictReco** and so on.

## Cosine model:

The basic idea here is as follows.  The predict the rating user i would
give to item j, find some users who are similar to user i and who have
rated item j, and average their ratings of that item.  The CRAN package
**recommderlab** is one implementation of this idea, but our
**rectools** package uses its own implementation, including novel
enhancements.

One such enhancement is to do item category matching, an example of
categories being movie genres.  For each user, our code calculates the
proportion of items in each category rated by this user, and
incorporates this into the calculation of similarity between any pair of
users.  See the functions **cosDist**, **formUserData** and
**predictUsrData**.

## Cross validation:

The MM, MLE, NMF and cosine methods all have wrappers to do
cross-validation, reporting the accuracy measures exact prediction, mean
absolute deviation and l2.  In our experiments so far, MM seems to give
the best accuracy (and the greatest speed).

## Plotting:

Some plotting capability is provided, currently in the functions
**plot.ydotsMM** and **plot.xvalb**.  The former, for instance, can be
used to assess the normality and independence assumptions of the MLE
model, and to identify a possible need to consider separate analyses for
subpopulations.


# REFERENCES

K. Gao and A. Owen, *Efficient Moment Calculations for Variance
Components in Large Unbalanced Crossed Random Effects Models*, 2016.

M. Hahsler, **recommderlab**, CRAN vignette.

Y. Koren et al, Matrix Factorization Techniques for Recommender 
Systems, *IEEE Computer*, 2009.

P. Perry, *Fast Moment-Based Estimation for Hierarchical Models*, 2015.

