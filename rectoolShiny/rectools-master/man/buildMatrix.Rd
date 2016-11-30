\name{buildMatrix}
\alias{buildMatrix}
\title{Build Ratings Matrix}

\description{
Create a new, specified matrix given a data set with three columns in which the rows are the values in the first column of the data set, the columns are the values of the second column of the data set, and the new data is the values of the 3rd column of the data set 
}

\usage{
buildMatrix(ratingsIn)
}

\arguments{
	\item{ratingsIn}{Raw data, in (user ID, item ID, rating) format.}
}
\details{
The \code{buildMatrix} function inputs a user ratings matrix, and
returns the ratings matrix, with element (i,j) containing the rating of
item j by user i


a new matrix 
	in which the rows, columns, and values of the new matrix are the \code{data} first, second, and third column respectively. 
}
\value{
A new matrix given the first three columns of \code{data}
}

\examples{
d <- rbind(c(1,3,8),c(2,1,3),c(1,4,5))
buildMatrix(d)  # non-0s at (1,3), (1,4), (2,1)
}

\author{
	Pooja Rajkumar
}
