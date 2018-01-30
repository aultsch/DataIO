\name{ReadBM}
\alias{ReadBM}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{  Load bestmatches from *.bm files }
\description{
   Load bestmatches from *.bm files
}
\usage{
ReadBM(FileName, InDirectory = getwd(), ESOMjavaTool=F)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{FileName}{ string, name of the .bm file }
  \item{InDirectory}{Optional: directory where *.bm file is, default: current dir}
  \item{ESOMjavaTool}{Optional: bool, Default(FALSE), falls TRUE: Datei stammt aus ESOM Java Tool}
}
\details{
  \code{BestMatchFileName} may have a .bm extension, if not, it will be added automatically
  and only files with a .bm extension can be loaded. If you want to load corresponding
  .bm or .cls files use \code{indexmerge}.
}
\value{
  A List containing:
\item{BestMatches[1:n,1:3]}{Bestmatches = [BMkey, BMLineCoords, BMColCoords]

In cartesian coordinate system the y-axis equals GridHeight - BMLineCoords
and the x-axis BMColCoords-1, if only integer values are used

}
\item{Lines}{number of ESOM rows    from header in *.bm file}
\item{Rows}{number of ESOM rows    from header in *.bm file  (historisch) }
\item{Columns}{number of ESOM columns from header in *.bm file}
\item{Size}{number of data points placed into lattice from header in *.bm file }
\item{IsToroid}{used topology, either 0=planar or 1=toroid}
}
\author{Michael Thrun}

\seealso{ \code{WriteBM}, \code{addext}, \code{indexmerge} }
\keyword{BestMatches}
\keyword{BMU}
\keyword{bmu}
\keyword{BestMatchingUnit}
\keyword{Best Matching Unit}
\keyword{Best Matches}
\keyword{best matching unit}
\keyword{best matches}
