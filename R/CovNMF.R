
###################    covnmf()   ##############################

# applies covariate information to NMF

# arguments:

#    narrowrat: 3-column df in (userID, itemID, rating) format, 
#       plus covariate columns, if any
#    k: desired rank in factorization

# value:

#    full predicted ratings matrix; later, object of class 'fullrat'

# if already have the predictive vals, call getnmf() directory

covnmf <- function(narrowrat,k) {
   nc <- ncol(narrowrat)
   if (nc > 3) {
      narrowrat[,3] <- narrowrat[,3] - getpreds(narrowrat)

   }
   getnmf(narrowrat,k)
}

getpreds <- function(narrowrat) {
      nrw <- as.matrix(narrowrat)
      tmp <- lm(nrw[,3],nrw[,4:nc])
      predict(tmp,nrw[,4:nc])
}

###################    getnmf()   ##############################

getnmf <- function(narrowrat,k) {
   require(NMF)
   a <- formfullmat(narrowrat)
   nmfout <- nmf(a,k)
   w <- nmfout@fit@W
   h <- nmfout@fit@H
   w %*% h
}

formfullmat <- function(narrowrat) {
   nuser <- length(table(narrowrat[,1]))
   nitem <- length(table(narrowrat[,2]))
   tmp <- matrix(rep(0,nuser*nitem),nrow=nuser)
   for (m in 1:nrow(narrowrat)) {
      i <- narrowrat[m,1]
      j <- narrowrat[m,2]
      tmp[i,j] <- narrowrat[m,3]
   }
   tmp
}

