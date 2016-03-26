\name{buildMatrix}
\alias{buildMatrix}
\title{Build a new matrix}

\description{
Create a new, specified matrix given a data set with three columns in which the rows are the values in the first column of the data set, the columns are the values of the second column of the data set, and the new data is the values of the 3rd column of the data set 
}

\usage{
buildMatrix(data)
}

\arguments{
	\item{data}{any given data set with minimum 3 columns}
}
\details{
	The \code{buildMatrix} function inputs a user ratings matrix, and returns a new matrix 
	in which the rows, columns, and values of the new matrix are the \code{data} first, second, and third column respectively. 
}
\value{
A new matrix given the first three columns of \code{data}
}

\author{
	Pooja Rajkumar
}
