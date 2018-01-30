\name{WriteCLS}
\alias{WriteCLS}
\title{ save *.cls files }
\description{
  write classification to a *.cls file.
}
\usage{
WriteCLS(FileName, Cls, OutDirectory = getwd(), Key = c(), Comment=0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{FileName}{ string, name of file the classifiction will be saved in }
  \item{Cls}{ classifiction of the data, a numeric vector }
  \item{Key}{ Optional: index of the data, possibly empty numeric vector }
  \item{OutDirectory}{ Optional: string, name of directory the data will be saved in, default \code{getwd()} }
  \item{Comment}{ Optional: String to be written in the first line as # Comment}                    
}
\details{
    \code{WriteCLS} is the primary resouce to save a *.cls file
  For the corresponding read function see \code{ReadCLS}

}
\references{
	\url{www.uni-marburg.de/fb12/datenbionik}
	\email{databionics@mathematik.uni-marburg.de}
}

\author{Michael Thrun, Alfred Ultsch}

\seealso{ \code{ReadCLS}, \code{addext} }

\keyword{ file, Cls, CLS }
