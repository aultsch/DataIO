\name{indexmerge}
\alias{indexmerge}
\title{ sort dbt files by given index }
\description{
  \code{indexmerge} sorts data of .lrn, .cls and .bm files by their common index
}
\usage{
indexmerge(lrndata, bm = NULL, cls = NULL)
}
\arguments{
  \item{lrndata}{ data.frame with column names = index }
  \item{bm}{ data.frame with two columns, colnames = index }
  \item{cls}{ data.frame with one column, colnames = index }
}
\details{
  lrndata, bm and cls should have the same index, i.e. column names. Only the order
  of the data can differ.
}
\value{

  a list
  \item{lrndata}{same data as above}
  \item{bm}{same bm data as above, only in the same index-order as lrndata}
  \item{cls}{same cls data as above, only in the same index-order as lrndata}
}

\author{ databionics working group, university of marburg, germany }

\seealso{ \code{ReadLRN},\code{ReadBM},\code{ReadCLS} }
\keyword{ file }
