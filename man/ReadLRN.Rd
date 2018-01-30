\name{ReadLRN}
\alias{ReadLRN}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ load *.lrn files }
\description{
  read data files in the LRN format
  see /dbt/ZFileFormatDocuments/lrn.html
}
\usage{
ReadLRN(FileName, InDirectory=getwd())
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{FileName}{name of *.lrn file}
\item{InDirectory}{directory where *.lrn file is  (default ==  getwd() )}
}
\details{
  \code{ReadLRN} is the primary resource to read a *.lrn file
  For the corresponding write function see \code{WriteLRN}
}
\value{
  a list with following elements:
  \item{Data}{ a numeric matrix containing n rows and m columns}
  \item{Key}{  a numeric vector of length(n)  }
  \item{Header}{ n column names for Data  or n+1 if a special name for Key is wanted}
  \item{DataDefined}{ the Defined line of *.lrn for the data see ZFileFormatDocuments}
%  \item{Comment}{comments}
}
\references{
	\url{www.uni-marburg.de/fb12/datenbionik}
	\email{databionics@mathematik.uni-marburg.de}
}
\author{Michael Thrun}

\seealso{ \code{WriteLRN}, \code{addext}, \code{indexmerge} }

\keyword{ file, Lrn, LRN }
