\name{ReadTXT2DATA}
\alias{ReadTXT2DATA}
\title{Creates data object from .txt tables}
\arguments{
\item{TxtFilename}{name of .txt file. Including file extension '*.txt'}
\item{TxtDirectoy}{Directory where *.txt file is (default ==  getwd() )}
\item{NamesInd}{integer, column with names of cases, genes etc.}
\item{SkipLines}{integer, the number of lines of the data file to skip before beginning to read data}
}
\value{V$Data(1:n,1:d)          matrix, numeric data with n lines and d columns}
\description{
Reads *.txt tables and creates data format object, see dbt \\ZFileFormatDocuments\\data.html
allowed format of *.txt file:
  - table separated with tabs               
  - comment character is '', defined by default read.table() function }
\author{
Sabine Puels 3/2015}
