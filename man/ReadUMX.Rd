\name{ReadUMX}
\alias{ReadUMX}

\title{ load .umx files }
\description{
  Load height matrix from *.umx file  
}
\usage{
ReadUMX(FileName, InDirectory=getwd())
}
\arguments{
  \item{FileName}{ string, name of the .umx file  }
  \item{InDirectory}{ string of the directory containing the .umx file }
}
\details{
  \code{UmxFileName} may have a .umx extension, if not, it will be added automatically
  and only files with a .umx extension can be loaded.
}
\value{
  a numeric matrix, the so called U-Matrix
}


\references{
	\url{www.uni-marburg.de/fb12/datenbionik}
	\email{databionics@mathematik.uni-marburg.de}
}
\author{Michael Thrun}

\seealso{ \code{WriteUMX}, \code{addext} }
