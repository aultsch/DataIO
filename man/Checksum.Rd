\name{Checksum}
\alias{Checksum}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Calculates the checksums for *.data files}
\description{
  Processes the inputted data.frame and outputs a list with a checksum for each column in the inputted data.frame.
}
\usage{
Checksum(INData)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{INData}{ A data.frame containing the data.}
}
\details{
  \code{Checksum} is implemented for the use with \code{ReadData} and \code{WriteData}. The file format *.data is saved with checksums for each column to prevent failure. The algorithm used for calculating the checksum is CRC-32 provided by the package \code{digest}. 
}
\value{
  A list with checksums for each column in \code{INData}.}
\references{
  \url{www.uni-marburg.de/fb12/datenbionik}
	\email{databionics@mathematik.uni-marburg.de}
}
\author{Tim Schneider}

\seealso{ \code{WriteData}, \code{ReadData}, \code{addext} }
\examples{
  Data<-data.frame(c(1,2,3,4,5))
	# returns a list with CRC32 checksums for c(1,2,3,4,5)
  Checksum(Data) #"76909375"
}

\keyword{file, DataIO,Checksum, Read, Write}
