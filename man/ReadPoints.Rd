\name{ReadPoints}
\alias{ReadPoints}
\title{  Load bestmatches (bm) and/or ProjectedPoints (xy) from *.bm file   }
\usage{ ReadPoints(FileName, InDirectory = getwd(),ESOMjavaTool=FALSE)}
\arguments{
\item{FileName}{FileName of *.bm file}
\item{InDirectory}{directory where *.bm file is, default: current dir}
\item{ESOMjavaTool}{bool, Default(FALSE), falls TRUE: Datei stammt aus ESOM Java Tool}
}
\value{A List with:
\item{BestMatches[1:n,1:3]}{if exists,Bestmatches = [BMkey, BMLineCoords, BMColCoords]}
\item{Lines}{Defines Grid Size (y-axis) of the corresponding U-matrix}
\item{Columns}{Defines Grid Size (x-axis) of the corresponding U-matrix.
               Beware: Lines and Columns has to be defined, if BestMatches and ProjectedPoints are given!}
\item{Rows}{=Lines, historical}
\item{Size}{number of data points placed into lattice from header in *.bm file}
\item{ProjectedPoints[1:n,1:3]}{if exists, n by OutputDimension matrix containing coordinates of the Projection}
\item{topology}{'toroid' or 'planar'}
}

\description{
  Load bestmatches (bm) and/or ProjectedPoints (xy) from *.bm file   }
\author{
MT 07/2015}
