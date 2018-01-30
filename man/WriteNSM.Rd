\name{WriteNSM}
\alias{WriteNSM}
\title{
Write named sparse matrix
}
\description{
Saves a named sparese matrix to disk using ".NSMrow.names", ".NSMcol.names" and ".NSMmat.lrn"
}
\usage{
WriteNSM(FilenameWithoutExt, Matrix, LineKey, LineHeader,

 ColumnKey, ColumnHeader, OutDirectory, Comments)
}
\arguments{
  \item{FilenameWithoutExt}{Filename for the saved matrix without extension}
  \item{Matrix}{The sparse matrix. Must be a sparse matrix from the Matrix package}
  \item{LineKey}{Optional: Vector of keys for all rows. Defaults to the vector \code{1:n} for a number of lines \code{n}}
  \item{LineHeader}{Optional: Vector of strings. Names for all rows. Defaults to: \code{paste0("R",LineKey)} }
  \item{ColumnKey}{Optional: Vector of keys for all colums. If not given and matrix dimensions are eqal, LineKey is taken as ColumnKey. Otherwise defaults to the vector \code{1:n} for a number of lines \code{n}}
  \item{ColumnHeader}{Optional: Vector of strings. Names for all columns. If not given and matrix dimensions are eqal, LineHeader is taken as Columnheader. Otherweise defaults to \code{paste0("C",LineKey)} }
  \item{OutDirectory}{Directory where the files should be saved}
  \item{Comments}{String containing comments for the sparse matrix}
}
\author{
Felix Pape
}
\seealso{
 \code{\link{WriteLRN}, \link{WriteNAMES}}
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ file }% use one of  RShowDoc("KEYWORDS")
