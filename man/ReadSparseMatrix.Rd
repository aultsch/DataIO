\name{ReadSparseMatrix}
\alias{ReadSparseMatrix}
\title{
Reads sparse matrixes from *.lrn files.
}
\description{
Zum Lesen von duenn besetzten Matrizen, die in *.lrn-Files gespeichert sind. (Gegebenenfalls
 mit ergaenzender *.names-Datei.)
}
\usage{
ReadSparseMatrix(FileNameWithoutExt, InDirectory)
}
\arguments{
  \item{FileNameWithoutExt}{
  String, name of the  file to be written. Without extention!
}
  \item{InDirectory}{
  the directory where to read from; if not given: current dir.
}
}
\value{
A list of:
  \item{ZeilenInd }{numeric vector. index of rows that contain non-zero entries}
  \item{SpaltenInd }{numeric vector. index of columns that contain non-zero entries corresponding to ZeilenInd such that Matrix[ZeilenInd, SpaltenInd]==non-zero}
  \item{Inhalt }{numeric vector. Containing the values of the sparseMatrix at corresponding ZeilenInd and SpaltenInd.}
  \item{Dimension }{numeric vector. Dimensions of the complete sparseMatrix. Default: c(max(ZeilenInd),max(SpaltenInd))}
  \item{Header }{string vector. Header for columns (including Key).}
  \item{Key }{numeric vector: unique key for each line}
  \item{DimNames }{OPTIONAL OUTPUT: If FileNameWithoutExt.names exists: string list. Names of the complete sparseMatrix fitting Dimension.
Must not be quadratic. Must not be of the same length as ZeilenInd
(iff there are rows containing only zeros).
First list element are rownames, second columnnames.}
}
\author{
CL
}
\seealso{
\code{WriteSparseMatrix()}
}
