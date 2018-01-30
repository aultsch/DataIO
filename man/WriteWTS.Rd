\name{WriteWTS}
\alias{WriteWTS}
\title{ save .wts files  }
\description{
  save weight vectors of an ESOM to a file with .wts extension
}
\usage{
WriteWTS(FileName,wts, OutDirectory=getwd(),Lines = NULL,
                 Columns = NULL, IsToroid = 0, Comment)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{FileName}{ string, name of file the data will be saved in }
  \item{wts}{[1:Lines*Columns,1:weights]   Information stored as a List of weights in a 2D matrix, Note: Make this a  3 dimensional array through ListAsEsomNeurons \code{ListAsEsomNeurons}
   
   Information represents a 2D grid with a weight  for every neuron of the length of the data set (number of variables)      
  }
  \item{OutDirectory}{ string, name of directory the data will be saved in, default \code{getwd()} }
  \item{Lines}{Number, Defines Grid Size (y-axis) of the corresponding U-matrix 
NOTE: One of Lines starts at the top, y-axis starts at bottom}
  \item{Columns}{Number, Defines Grid Size (x-axis) of the corresponding U-matrix}
  \item{IsToroid}{used topology, either 0=planar or 1=toroid}
  \item{Comment}{array of characters to be written in the first line of the file, it will be marked with \code{'\#'}, more than one line, use \code{'\n#'}
Not More than 3 seperate Lines of Comments allowed}
}
\details{
  \code{FileName} may have a .wts extension, if not, it will be added automatically
  The array will be broken down into a rectangular form like a matrix.
  The lines will be organized as follows\cr
  som[1,1,1], ... , som[1,1,m]\cr
  som[1,2,1], ... , som[1,2,m]\cr
  .                      .    \cr
  .                      .    \cr
  .                      .    \cr
  som[1,l,1], ... , som[1,l,m]\cr
  som[2,1,1], ... , som[2,1,m]\cr
  .                      .    \cr
  .                      .    \cr
  .                      .    \cr
  som[k,l,1], ... , som[k,l,m]
}

\author{Michael Thrun, Florian Lerch}

\seealso{ \code{ReadWTS}, \code{addext} }

\keyword{ file }

