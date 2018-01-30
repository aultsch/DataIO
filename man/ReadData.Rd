\name{ReadData}
\alias{ReadData}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ reads *.data files }
\description{
  This function reads files in the *.data format.
  
  The *.data file format enables the user to import data to Microsoft Excel,  
  Mathworks Matlab and allows to inspect and edit data within every text editor. 
  *.data file format is a tab separated csv file with specific header. 
  To prevent failures each column is saved with a CRC-32 checksum. 
  Every *.data file contains a DataDefined line which describes the data in each column. 
  The key column is a unique column of integer values. One value codes exactly one data record, where this one line of data equals one observation.
  Numbers are stored under \code{Data} inside the file Strings, using \code{DataDefined} = 9,0,1. Strings are saved as Names using \code{DataDefined} = 6. \code{DataDefined} 2,3 and 8 are   used for internal purposes. 
}
\usage{
ReadData(FileName, InDirectory=getwd())
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{FileName}{ string, name of the *.data file }
  \item{InDirectory}{ string of the directory containing the *.data file }
}
\details{
  \code{ReadData} is the primary resource to read a *.data file.
  For the corresponding write function see \code{WriteData}.
}
\value{
  \item{Data}{ a \code{[n,k]} matrix containing the data}
  \item{Header}{string vector with the names for the key and data columns,
                by default 'Key' for \code{DataDefined==9}, 'Cls' for \code{DataDefined==3}, 'C1', 'C2', 'C3', 'C4',  etc. for \code{DataDefined==1 or ==2}. Note: must not contain whitespace
                 }
  \item{DataDefined}{vector of column type: entry (0='ignore', 1='data'(numeric), 2='other'(numeric), 3='class'(numeric), 6='Names'(character), 8='class'(character), 9='key'(numeric)),
                     default is only 1's or 6's with leading 9 }
  \item{Key}{a numeric vector, unique number for each line, by default \code{1:n}, \code{n} being the number of data points }
  \item{Names}{string matrix of short identifiers for each line to be put as last column. If not available value is NULL }
}
\references{
	\url{www.uni-marburg.de/fb12/datenbionik}
	\email{databionics@mathematik.uni-marburg.de}
}
\author{Tim Schneider, Michael Thrun}

\seealso{ \code{WriteData}}
\examples{
#Read the file "test.data" from the current directory
#ReadData("test.data") 

#Read the file "test.data" from given path
#ReadData("test.data", DataDirectory = "~/Desktop/") 
}

\keyword{ file, Data, dbt }
