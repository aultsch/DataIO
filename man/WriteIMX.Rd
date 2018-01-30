\name{WriteIMX}
\alias{WriteIMX}
\title{ save to .imx files }
\usage{WriteIMX(FileName,MapMask,OutDirectory,Comment)}
\arguments{
\item{FileName}{The name of the file to write}
\item{MapMask}{A binary array with 0 where the map is displayed  and 1 otherwise}
\item{OutDirectory}{Optional: the directory to write into; if not given: current dir}
  \item{Comment}{array of characters to be written in the first line of the file, it will be marked with \code{'\#'}, more than one line, use \code{'\n#'}
Not More than 3 seperate Lines of Comments allowed}
}
\value{FileName         the name of the file to write}
\description{
   saves a mask for a tiled U-matrix to display a ESOM-map-Isle }
\author{Michael Thrun}
