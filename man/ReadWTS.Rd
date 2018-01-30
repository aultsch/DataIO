\name{ReadWTS}
\alias{ReadWTS}

\title{ load .wts files }
\description{
  Load ESOM weights from a *.wts file 
}
\usage{
ReadWTS(FileName, InDirectory=getwd())
}
\arguments{
\item{FileName}{name of the file}
\item{InDirectory}{the directrory of the *.wts file is}
}
\details{
  \code{WtsFileName} may have a .wts extension, if not, it will be added automatically
  and only files with a .wts extension can be loaded
}
\value{
  a list
  \item{wts}{[1:Lines*Columns,1:weights]   Information stored as a List of weights in a 2D matrix, Note: Make this a  3 dimensional array through ListAsEsomNeurons \code{ListAsEsomNeurons}
   
   Information represents a 2D grid with a weight  for every neuron of the length of the data set (number of variables)      
  }
  \item{rows}{number of ESOM rows from header in *.wts file}
  \item{columns}{number of ESOM columns from header in *.wts file}
  \item{Keys}{ the Keys for the data dimensions, usually just ones }
}
\author{Michael Thrun}

\seealso{ \code{WriteWTS}, \code{addext} }
\keyword{ file }

