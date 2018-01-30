\name{ReadCLS}
\alias{ReadCLS}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ load *.cls files }
\description{
  Load key-class assignments from *.cls file
}
\usage{
ReadCLS(FileName, InDirectory=getwd())
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{FileName}{ string, name of the *.cls file }
  \item{InDirectory}{ string of the directory containing the *.cls file }
}
\details{
  \code{ReadCLS} is the primary resouce to read a *.cls file
  For the corresponding write function see \code{WriteCLS}

}
\value{
  a list of length 3 containing the following:
  \item{ClsKey}{an n-by-1 matrix}
  \item{Cls}{an n-by-1 matrix}
  \item{UniqueClasses}{an m-by-1 matrix, m indicating the number of different classes}


}
\references{
	\url{www.uni-marburg.de/fb12/datenbionik}
	\email{databionics@mathematik.uni-marburg.de}
}
\author{Alfred Ultsch  }

\seealso{ \code{WriteCLS}, \code{addext}, \code{indexmerge} }

\keyword{ file, Cls, CLS }