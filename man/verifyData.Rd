\name{verifyData}
\alias{verifyData}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Verify *.data files
}
\description{
*.data files contain checksums. 
To avoid warning messages the checksums must be new calculated every time the data is intentionally altered.
}
\usage{
verifyData(DataFilename, DataDirectory = getwd())
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{DataFilename}{ string, name of file the data will be loaded/saved in }
  \item{DataDirectory}{ string, name of directory the data will be saved in, default \code{getwd()} }
}

\references{
	\url{www.uni-marburg.de/fb12/datenbionik}
	\email{databionics@mathematik.uni-marburg.de}
}
\author{Tim Schneider}
\seealso{ \code{ReadData}, \code{WriteData} }
\keyword{ReadData}
\keyword{WriteData}
\examples{
	#assume the is a file called test.data
	#verifyData("test.data")
}
