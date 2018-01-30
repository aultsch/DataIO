\name{WriteUMX}
\alias{WriteUMX}
\title{ save .umx files }
\description{
  save a U-matrix as a file with .umx extension
}
\usage{
WriteUMX(FileName,UMatrix, OutDirectory=getwd(),Comment)
}
\arguments{
  \item{FileName}{ string, name of the resulting *.umx file }
  \item{UMatrix}{ U-Matrix, i.e. a numeric matrix or data.frame }
  \item{OutDirectory}{ string, name of directory the data will be saved in, default \code{getwd()} }
  \item{Comment}{array of characters to be written in the first line of the file, it will be marked with \code{'\#'}, more than one line, use \code{'\n#'}
Not More than 3 seperate Lines of Comments allowed}
	}
\details{
  \code{FileName} may have a .umx extension, if not, it will be added automatically
}

\author{Alfred Ultsch}

\seealso{ \code{ReadUMX}, \code{addext} }
\keyword{ file }
