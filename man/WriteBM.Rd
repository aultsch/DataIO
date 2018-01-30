\name{WriteBM}
\alias{WriteBM}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ save .bm files }
\description{
  Save bestmatches to a .bm file.
}
\usage{
WriteBM(FileName, BestMatches, OutDirectory = getwd(), Lines, Columns, IsToroid,Comment )
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{FileName}{Filename of .bm file}
\item{BestMatches}{2-3 column integer matrix with key, row, column. If there are 2 columns, these should be only bestmatches and key is generated. If there are 3 columns, it is assumed that the first column is key, then:

Bestmatches = [BMkey, BMLineCoords, BMColCoords]


In cartesian coordinate system the y-axis equals GridHeight - BMLineCoords
and the x-axis BMColCoords-1, if only integer values are used

}
\item{OutDirectory}{Optional: directory where the file is being written, default is the current working directory}
\item{Lines}{Optional: number of ESOM Rows, Lines=    max(BestMatches(:,2) if not given}
\item{Columns}{Optional: number of ESOM columns, Columns=  max(BestMatches(:,3) if not given}
\item{IsToroid}{used topology, either 0=planar or 1=toroid}
  \item{Comment}{array of characters to be written in the first line of the file, it will be marked with \code{'\#'}, more than one line, use \code{'\n#'}
Not More than 3 seperate Lines of Comments allowed}
}

\author{ Michael Thrun }

\seealso{ \code{ReadBM}, \code{addext} }

\keyword{BestMatches}
\keyword{BMU}
\keyword{bmu}
\keyword{BestMatchingUnit}
\keyword{Best Matching Unit}
\keyword{Best Matches}
\keyword{best matching unit}
\keyword{best matches}
