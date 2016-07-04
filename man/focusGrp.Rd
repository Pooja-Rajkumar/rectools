\name{focusGrp}
\alias{focusGrp}

\title{Find Focus Group}

\description{
Finds "representative" users, i.e. the users whose ratings correlate
the highest with the average ratings of the items they rate.

}


\usage{
focusGrp(ydotsObj, ratingsIn, k = 10, minn = 50) 
}

\arguments{
        \item{ydotsObj}{Object of type \code{'ydotsMM'} or \code{ydotsMLE}.}
	\item{ratingsIn}{Input data frame, training set. Within-row format 
           is (UserID, ItemID, rating).}
	\item{k}{Desired size of the focus group.}
	\item{minn}{Minimum number of ratings for a user to be
           considered for the group.}
}

\details{

For each user i, vectors u and v will be formed; u will be the vector of
ratings set by user i; for v[j], the code will find the item ID of the
j-th component of u, then set v[j] to the average rating of all users
for that item ID.  Then the correlation between u and v will be computed
for each user, and the k users with the highest correlations will be
chosen.
	
}

\examples{
ivl <- InstEval 
ivl$s <- as.numeric(ivl$s) 
ivl$d <- as.numeric(ivl$d) 
ivl3 <- ivl[,c(1,2,7)]
ydo <- findYdotsMM(ivl3)
focusGrp(ydo,ivl3)  # users 1181, 1462, ... seem to be representative
}

\author{
	Pooja Rajkumar and Norm Matloff
}
