\name{addext}
\alias{addext}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ add file extensions to file names }
\description{
  \code{addext} intern \code{DataIO} function, checks for the right extension and adds it if necessary.
}
\usage{
addext(filename, extension)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{filename}{string, to be checked for correct extension}
  \item{extension}{Correct extension for filename.}
}
\details{
  Files have special formats and therefor need to be saved with the correct
  extensions. \code{addext} is simply an auxiliary function to guarantee correct file
  name extensions
}
\value{
  Function returns a string containing the filename and the correct extension. 
}

\author{Michael Thrun}

\examples{
addext('hallo.data','data') #  'hallo.data'
addext('hallo','data')     #  'hallo.data'

}
\keyword{ file }

