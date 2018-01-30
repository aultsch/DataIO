\name{WriteLRN2Data}
\alias{WriteLRN2Data}
\title{ WriteLRN2Data(LRNFileName) reads *.lrn file and writes *.Data File }

\usage{
WriteLRN2Data(LRNFileName,LRNDirectory=getwd(),NamesFileName,NamesDirectory=LRNDirectory)
}

\arguments{
\item{LRNFileName}{name of *.lrn file WITHOUT *.lrn!}
\item{LRNDirectory}{directory where *.lrn file is  (default ==  getwd() )}
\item{NamesFileName}{name of *.names file, The both will be read out, WITHOUT *.names!}
\item{NamesDirectory}{directory where *.names file is default ist LRNDirectory}
}
\value{*.data file in same Directory as LRNDirectory with same filename as LRNFileName}
\description{
 WriteLRN2Data(LRNFileName) reads *.lrn file and writes *.Data File }
