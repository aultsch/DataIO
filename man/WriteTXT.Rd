\name{WriteTXT}
\alias{WriteTXT}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ save text to a file }
\description{
  This function saves string or character matrices into a plain text file *.txt.
  Each row in the matrix will be saved as separate line. 
  Each column will be saved by separating with a white space. 
}
\usage{
WriteTXT(FileName,Names,OutDirectory=getwd(),StrTrim=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{FileName}{ string, name of file the text will be saved in }
  \item{Names}{ a string or character matrix }
  \item{OutDirectory}{ string, name of directory the data will be saved in, default \code{getwd()} }
  \item{StrTrim}{==TRUE no leading and trailing blanks are written, not implemented yet, default FALSE}
}
\details{
  \code{filename} may have a *.txt extension, if not, it will be added automatically
}

\author{ databionics working group, university of marburg, germany }

\seealso{ \code{addext}, \code{ReadTXT} }
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ file }
