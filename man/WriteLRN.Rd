\name{WriteLRN}
\alias{WriteLRN}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ save *.lrn files }
\description{
  save data, column Header, and column type definition to a *.lrn file
}
\usage{
WriteLRN(FileName, Data, Header, Key, DataDefined, OutDirectory, CommentOrDigits)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{FileName}{ string, name of the  file to be written }
  \item{Data}{ matrix of data , cases in rows , variables in columns, may contain NaN, NA or any other missing number symbol }
  \item{Header}{ Optional: cell array or string matrix with column Header (d data columns) plus eventual header for Key as first entry }
  \item{Key}{ Optional: a numeric vector, unique number for each line, by default 1:n, n being the number of data points }
  \item{DataDefined}{ Optional: vector of column type: keys (0=ignore,1=data,2,3=class,9=key),
                     default is only 1's with leading 9 }
  \item{OutDirectory}{ Optional: string, name of directory the data will be saved in, default \code{getwd()} }
  \item{CommentOrDigits}{either a string which is inserted as CommentOrDigits in the first line in the file. if it is a number then it is the number of significant digits (after the "."), default 6}
}
\details{
  Comfortably writes data to a file in the LRN Format.
  See  dbt/ZFileFormatDocuments/lrn.html for a documentation of the format.
  The length of \code{Key} has to be \code{ncol(Data)} or \code{ncol(Data[[1]])} respectively.
  If \code{Key} is empty it is replaced by a vector \code{1:ncol(Data)}
  If \code{Names} is empty, it is replaced by  default \code{names}:
  The default name for a \code{DataDefined==9} column is 'Key'.
  The default name for a \code{DataDefined==3} column is 'Cls'.
  The default name for \code{DataDefined==1 or ==2} is 'C1', 'C2', etc.
  \code{DataDefined may} only contain numbers 0,1,2,3 and one 9.
  If \code{DataDefined} it is replaced by  c(9,1,..).
}
\references{
	\url{www.uni-marburg.de/fb12/datenbionik}
	\email{databionics@mathematik.uni-marburg.de}
}

\author{Michael Thrun}

\seealso{ \code{ReadLRN}, \code{addext} }

\keyword{ file, LRN, Lrn }