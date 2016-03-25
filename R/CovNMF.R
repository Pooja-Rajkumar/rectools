
###################    covnmf()   ##############################

# applies covariate information to NMF

# arguments:

#    narrowrat: 3-column df in (userID, itemID, rating) format, 
#       plus covariate columns, if any
#    k: desired rank in factorization

# value:

#    full predicted ratings matrix; later, object of class 'fullrat'

# if already have the predictive vals, call getnmf() directory

covNMF <- function(narrowrat,k) {
   nc <- ncol(narrowrat)
   if (nc > 3) {
      narrowrat[,3] <- narrowrat[,3] - getpreds(narrowrat)

   }
   getNMF(narrowrat,k)
}

getPreds <- function(narrowrat) {
      nrw <- as.matrix(narrowrat)
      tmp <- lm(nrw[,3],nrw[,4:nc])
      predict(tmp,nrw[,4:nc])
}

###################    getnmf()   ##############################

getNMF <- function(narrowrat,k) {
   require(NMF)
   a <- buildMatrix(narrowrat)
   nmfout <- nmf(a,k)
   w <- nmfout@fit@W
   h <- nmfout@fit@H
   w %*% h
}

