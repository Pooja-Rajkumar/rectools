# rectools

## Advanced Package for Recommender Systems

## FEATURES:

* Incorporate user and item covariate information.

* Parallel computation.

* Novel variations on latent factor model.

* Plotting.

* Focus group finder.

## QUICK FIRST EXAMPLES:

### Random effects ANOVA model:

A simple latent factor model is

E(Y) =  mu + alpha + beta

where Y is the rating, alpha and beta being specific latent effects for
the given user and item.

A simple Method of Moments approach would estimate alpha for user i by
Yi. - Y.., where the first term is the mean of all observed ratings by
user i and the second is the overall mean of all ratings.  We estimate
beta for item j similarly, and estimate mu by the overall mean Y..  The
predicted value of Yij is then

Yi. + Y.j - Y..

Under a Maximum Likelihood approach, alpha and beta are assumed to
have independent normal distributions with different variances.  

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

# REFERENCES

K. Gao and A. Owen, *Efficient Moment Calculations for Variance
Components in Large Unbalanced Crossed Random Effects Models*, 2016.

Y. Koren et al, Matrix Factorization Techniques for Recommender 
Systems, *IEEE Computer*, 2009.

P. Perry, *Fast Moment-Based Estimation for Hierarchical Models*, 2015.

