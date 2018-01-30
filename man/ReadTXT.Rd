\name{ReadTXT}
\alias{ReadTXT}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ load text to a file }
\description{
  Read \code{*.txt} files. The files containing matrices separated by \code{" "} (white space). 
}
\usage{
ReadTXT(FileName,InDirectory=getwd(),NrLinesToSkip,HasNamesInCols,EmptySymbol)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{FileName,}{name of *.txt file}
\item{InDirectory}{InDirectory where *.txt file is}
\item{NrLinesToSkip}{Optinal: so many lines are skipped bevore reading default:0}
\item{HasNamesInCols}{Optional: ==TRUE means Colum 1. is a name column default: FALSE}
\item{EmptySymbol}{Optional: Symbol used for NaN  default: EmptySymbol='NaN'}
}
\details{
  Each string has to be a single line in the \code{*.txt} file.
  \code{filename} may have a .txt extension, if not, it will be added automatically.
}
\value{Data(1:n,1:d)        data i.e. all numeric colums of *.txt}
\author{Michael Thrun}

\seealso{ \code{addext}, \code{DataIO}, \code{WriteTXT} }

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ file }
