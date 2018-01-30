\name{SparseToFull}
\alias{SparseToFull}
\title{
Create matrix lrn from sparse
}
\description{
Interactive function to convert a named sparse matrix to a normal matrix in lrn format. This requires the sparse matrix to be saved in the NSM format, written by \code{\link{WriteNSM}}.
}
\usage{
SparseToFull()
}

\value{
This function results in a new file containing the matrix in the current working directory with the name of the named sparse matrix file and the extension '.lrn'. 
}
\author{
Felix Pape
}
\keyword{ file }% __ONLY ONE__ keyword per line
