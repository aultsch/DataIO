\name{ReadNSM}
\alias{ReadNSM}
\title{
Read named sparse matrix
}
\description{
Loads a named sparese matrix from disk using ".NSMrow.names", ".NSMcol.names" and ".NSMmat.lrn"
}
\usage{
ReadNSM(FilenameWithoutExt, InDirectory)
}
\arguments{
  \item{FilenameWithoutExt}{Filename without extension}
  \item{InDirectory}{Optional: Directory of the file. Defaults to \code{getwd()}}
}
\value{
  A list containing:
  \item{matrix}{The sparse matrix read from the disk as dgCMatrix S4 object}
  \item{LineKey}{Vector of keys for all rows}
  \item{LineHeader}{Vector of strings. Names for all rows}
  \item{ColumnKey}{Vector of keys for all colums}
  \item{ColumnHeader}{Vector of strings. Names for all columns}
  \item{Comments}{String containing comments for the sparse matrix}
}
\author{
Felix Pape
}
\note{
Due to restrictions of the lrn format precision losses for floating point numbers can occur. The maximum precision is 13 digits.
}
\seealso{
 \code{\link{ReadLRN}, \link{ReadNAMES}}
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ file }% use one of  RShowDoc("KEYWORDS")
