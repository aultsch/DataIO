\name{dataToDataframe}
\alias{dataToDataframe}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ load a *.data file into a data.frame }
\description{
  Converts the outputted List from \code{ReadData} into a data.frame.
}
\usage{
dataToDataframe(V)
}
\arguments{
  \item{V}{ list from \code{ReadData} call}
}
\value{
	\item{data.frame}{ data.frame with named columns and *.data file's key for row names. 
		If names or description is available, it will be put as a separate column to the data.frame. }
}
\author{Tim Schneider}
\seealso{ \code{addext}, \code{WriteData},\code{ReadData} }
\examples{
	#needs a *.data file "test.data" in current directory (getwd())
	#dataToDataFrame( ReadData("test.data") )
}
\keyword{dataToDataframe,file}
