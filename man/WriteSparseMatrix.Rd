\name{WriteSparseMatrix}
\alias{WriteSparseMatrix}
\title{
Writes sparse matrixes in *.lrn format to the filesystem
}
\description{
Writes sparse matrixes in *.lrn format to the filesystem
}
\usage{
WriteSparseMatrix(FileNameWithoutExt, ZeilenInd, SpaltenInd, 

Inhalt, Dimension, DimNames, OutDirectory, Header, Key, Comment)
}
\arguments{
  \item{FileNameWithoutExt}{string. name of the  file to be written without extention!}
  \item{ZeilenInd}{numeric vector. index of rows that contain non-zero entries}
  \item{SpaltenInd}{numeric vector. index of columns that contain non-zero entries corresponding to ZeilenInd such that Matrix[ZeilenInd, SpaltenInd]==non-zero}
  \item{Inhalt}{numeric vector or just one number that will be repeated n-times.
        Containing the values of the sparseMatrix at corresponding ZeilenInd and SpaltenInd.}
  \item{Dimension}{Optional: numeric vector. If any row or column has no non-zero entries, one can specify by Dimension the original dimensions that are wanted/expected. 
  Default: c(max(ZeilenInd),max(SpaltenInd))}
  \item{DimNames}{Optional: string list. Names of the complete sparseMatrix fitting Dimension.
        Must not be quadratic. Must not be of the same length as ZeilenInd (iff there are rows containing only zeros).
        First list element are rownames, second columnnames. }
  \item{OutDirectory}{Optional: the directory to write into; if not given: current dir.}
  \item{Header}{Optional: string vector. Header for columns (including OR excluding Key).
        Default: c('Key, ZeilenInd, SpaltenInd, Inhalt')}
  \item{Key}{Optional: numeric vector: unique key for each line, if not given Key<-1:n} 
  \item{Comment}{Optional: string which is inserted as comment in the first line(s) of the file}
}
\details{
Erzeugt eine *.lrn der Form:
 	Kommentar (ganz normal wie bei lrn),
 	Groesse der lrn (ganz normal wie bei lrn),
 	Header (ganz normal wie bei lrn),
 	Zeile 1 ist fuer zusaetzliche Informationen ueber Matrixdimension reserviert:
 	In der Key-Spalte steht: 0 
 	In der ZeilenInd-Spalte steht: Zeilenanzahl der Matrix (da es leer-Zeilen (Zeilen ohne nicht-Null-Eintraege) geben koennte)
 	In der SpaltenInd-Spalte steht: Spaltenanzahl der Matrix (da es leer-Spalten geben koennte) 
 	In der Inhalt-Spalte steht: Anzahl der nicht-Null-Eintraege in der gesamten Matrix.

 Falls DimNames gegeben wird auch noch eine *.names erzeugt, in der die Namen der gesamten Matrix
 gespeichert werden in einer Spalte. In einer zweiten Spalte wird gespeichert ob es row- oder
 columnname ist. Die Keys der lrn und der names stimmen dann selbstverstaendlich
 NICHT ueberein!!!
}
\author{
CL
}
\seealso{
\code{ReadSparseMatrix()}
}
