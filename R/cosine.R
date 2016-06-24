
# rating prediction via nearest neighbors, via "cosine" (inner product

# covariates and item type preferences may be used

# cosprep() does the initial nearest-neighbor analysis, after which use
# predict(); finds the k nearest-neighbors of the points in the original
# input data

# arguments:

#    usrDataObj: object of class 'usrData'
#    wtcovs: weight to put on covariates
#    wtcats: weight to put on item categories
#    k: number of nearest neigbhors
#    fileOut: name of file to save return value to

# value:

#    object of class 'cosknn'

cosprep <- function() {usrDataObj,wtcovs=1.0,wtcats=1.0,k=50,fileOut='')

}
