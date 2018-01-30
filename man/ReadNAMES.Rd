\name{ReadNAMES}
\alias{ReadNAMES}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ load *.names files}
\description{
ACHTUNG: Neu programiert, Doku in Header der Funktion lesen!
  easily load dbt files in the NAMES format
  see /dbt/ZFileFormatDocuments/names.html
}
\usage{
ReadNAMES(FileName, InDirectory = getwd())
}
\arguments{
\item{FileName}{filename of *.names file}
\item{InDirectory}{InDirectory where *.names file is, default: current dir}
}
\details{
  \code{ReadNAMES} may have a *.names extension, if not, it will be added automatically,
  and only files with a .names extension can be loaded
}
\value{
  a list of length 3 containing the following:
  \item{NamesKey}{the unique key of the names}
  \item{RowNames}{the names themselves}
  \item{RowRest}{additional info, optional}

}
\references{
	\url{www.uni-marburg.de/fb12/datenbionik}
	\email{databionics@mathematik.uni-marburg.de}
}
\author{Michael Thrun}

\seealso{ \code{addext}, \code{WriteNAMES}, }

\keyword{ file, Names, NAMES }