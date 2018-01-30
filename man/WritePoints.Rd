\name{WritePoints}
\alias{WritePoints}
\title{Save bestmatches (bm) of ESOM and/or ProjectedPoints (xy) to a *.pbm file.}
\arguments{
\item{FileName}{FileName of *.bm file}
\item{BestMatches[1:n,1:3]}{2-5 column numeric matrix with key, row, column}
\item{or [1:n,1:2]}{if 2 columns, assuming only bestmatches ("Rueckwaertscompatibel") if 3 columns, assumes, that first columns is key("Rueckwaertscompatibel") if more than 3 Columns than Also ProjectedPoints are give if you want to save only ProjectedPoints (not rectangular grid based points) please define DataDefined with c(5,5) NOTE: ProctedPoints[1:n,Key,X,Y] but BestMatchingUnits[1:n,Key,Y,X]=BestMatchingUnits[1:n,Key,Lines,Columns] BestMatchingUnits are defined such that the begin at [1,1] not [0,0]}
\item{Lines}{Defines Grid Size (y-axis) of the corresponding U-matrix NOTE: One of Lines starts at the top, y-axis starts at bottom}
\item{Columns}{Defines Grid Size (x-axis) of the corresponding U-matrix Beware: Lines and Columns has to be defined, if BestMatches and ProjectedPoints are given! Lines=    ceiling(max(BestMatches(:,2)) if not given Columns=  ceiling(max(BestMatches(:,3)) if not given}
\item{DataDefined(1:2) or (1:3)}{vector of column type: 9 For Key, 4 for Points on rectangular quad grid, 5 for hexagonal grid or other projections if not specified BestMatchingUnits will be saved if c(5,5) or c(9,5,5) given, both the ProjectedPoints and the Grid Coonversion will be saved The GridPoints will always be the the second and third Column}
\item{topology}{used topology, either "planar" or "toroid" if not given, assuming for ProjectedPoints planar and for BestMatches toroid}
\item{OutDirectory}{where to write, default = current dir}
\item{Comment}{array of characters to be written in the first line of the file, it will be marked with '\\', more than one line, use '\\n' Not More than 3 seperate Lines of Comments allowed}
}
\description{
Save bestmatches (bm) of ESOM and/or ProjectedPoints (xy) to a *.pbm file. }
\author{
MT 07/2015
}
