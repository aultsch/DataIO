\name{ReadIMX}
\alias{ReadIMX}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ load .imx files }
\description{
	 read a mask for a tiled UMX to display a ESOM-map
}
\usage{
ReadIMX(FileName, InDirectory = getwd())
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{FileName}{ string, name of the .imx file  }
  \item{InDirectory}{ string of the directory containing the .imx file }
}
\details{
  \code{ImxFileName} may have a .imx extension, if not, it will be added automatically,
  and only files with a .imx extension can be loaded
}
\value{
  a data frame containing only ones and zeros (if the corresponding .imx file
  is correct)
}

\author{Michael Thrun}

\seealso{ \code{addext} }

\keyword{ file }