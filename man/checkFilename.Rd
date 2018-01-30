\name{checkFilename}
\alias{checkFilename}
\title{ checkFilename(FileName,Directory,Extension,ReadOrWrite,NameOfFunctionCalled) Searching for Filename & Directory existence }
\usage{ checkFilename(FileName,Directory,Extension,ReadOrWrite,NameOfFunctionCalled) }
\arguments{
\item{FileName}{string, name for file without extension}
\item{Extension}{string, extension of filename (everything after the ".")}
\item{ReadOrWrite}{=TRUE: ReadX =FALSE: WriteX}
\item{NameOfFunctionCalled}{Fuer Fehlermeldung: Welche Funktion hat checkFilename aufgerufen}
\item{Directory}{string, complete path of file, default is current path}
}
\value{Bricht mit Fehlermeldung die Ein/AusleseProzedure ab, wenn mit Dateiname/Directory etwas nicht stimmt}
\description{
 checkFilename(FileName,Directory,Extension,ReadOrWrite,NameOfFunctionCalled) Searching for Filename & Directory existence }
\author{Michael Thrun}
