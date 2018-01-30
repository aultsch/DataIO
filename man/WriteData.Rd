\name{WriteData}
\alias{WriteData}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ write *.data files }
\description{
  Writes Data, \code{DataDefined}, Key and Names in the \code{*.data} file format.
  WriteData is the function for writing *.data files.
  
  The *.data file format enables the user to import data to Microsoft Excel,  
  Mathworks Matlab and allows to inspect and edit data within every text editor. 
  *.data file format is a tab separated csv file with specific header. 
  To prevent failures each column is saved with a CRC-32 checksum. 
  Every *.data file contains a \code{DataDefined} line which describes the data in each column. 
  The key column is a unique column of integer values. One value codes exactly one data record, where this single line of data equals one observation.
  Numbers are stored under \code{Data} inside the file strings, using \code{DataDefined} = 9,0,1.  Strings are saved as Names using \code{DataDefined} = 6. \code{DataDefined} 2,3 and 8 are used for internal purposes. 

}
\usage{
WriteData(FileName, Data, OutDirectory = getwd(), 
			Header =c(), DataDefined, Key, Comment, Names)
}
\arguments{
  \item{FileName}{ string, name of file where the data will be stored }
  \item{Data}{\code{[n,k]} matrix or a \code{data.frame} containing the data }
  \item{OutDirectory}{optional,  string, name of directory the data will be saved in, default \code{getwd()} }
  \item{Header}{optional, string vector with the names for the key and data columns,
                by default 'Key' for \code{DataDefined}==9, 'Cls' for \code{DataDefined}==3, 'C1', 'C2', 'C3', 'C4' etc. for \code{DataDefined}==1 or ==2,
								 must not contain whitespace
                 }
  \item{DataDefined}{optional,vector of column type: keys (0=ignore, 1=data(numeric), 2(numeric), 3=class(numeric), 
	 6=Names(character), 8=class(character), 9=key),
                     default is only 1's or 6's with leading 9 for the key column }
  \item{Key}{optional, a numeric vector with unique number for each line, by default 1:n, n being the number of rows }
  \item{Comment}{optional, array of characters to be written in the first line of the file, it will be marked with '\%'}
  \item{Names}{optional, list or string matrix with text for each line to be put as last column, usually a short identifier }
}
\details{
  composition of the *.data file:
	
	all comment lines start with '\%', all header lines with '#'
	
	After none or one comment line the header starts with the following information:
	
	1.line: number of rows in \code{Key} respective data
	
	2.line: number of columns in file, including \code{Key} column
	
	3.line: checksums for each column
	
	4.line: names of columns defined by input variable \code{Names}, if not defined see below
	
	After this the key column and the data follows, each column separated by a white space, each row separated by new line.
	
  The length of \code{Key} has to be \code{ncol(Data)} or \code{ncol(Data[[1]])} respectively.
  If \code{Key} is empty it is replaced by a vector \code{1:ncol(Data)}
	NOTE: there can be only ONE column with \code{dataDefined == 9} i.e. the column containing the \code{Key}
	
  If \code{Names} is empty, it is replaced by default names:
  The default name for a \code{DataDefined==9} column is 'Key'.
  The default name for a \code{DataDefined==3} column is 'Cls'.
  The default name for \code{DataDefined==1 or ==2} is 'C1', 'C2', 'C3', 'C4' etc.
  \code{DataDefined} may only contain numbers 0,1,2,3,6,8 and 9.
	Please use \code{DataDefined} == 6 only once.
  If \code{DataDefined} is empty or wrong it is replaced by  \code{c(9,1,..)}.
}
\references{
	\url{www.uni-marburg.de/fb12/datenbionik}
	\email{mthrun@mathematik.uni-marburg.de}
}
\examples{
#create a matrix
#mat = matrix( 
	#c(1,0,0,0,1,0,0,0,1), 
	#nrow=3, 
	#ncol=3) 
	
#Write this matrix into the current directory
#WriteData(FileName = "test.data",Data = mat)
	
#Write this matrix into a different directory, for example under MacOS X
#WriteData(FileName = "test.data",Data = mat, InDirectory = '~/Desktop/')

}
\author{Tim Schneider, Michael Thrun}

\seealso{ \code{ReadData} }
\keyword{ReadData, WriteData}