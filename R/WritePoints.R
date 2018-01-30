`WritePoints` <-
function(FileName,BestMatches,OutDirectory=getwd(),Lines=NULL,Columns=NULL,DataDefined=NULL, topology=NULL,Comment){
# WritePoints(FileName, BestMatches)
# WritePoints(FileName, BestMatches,OutDirectory)
# WritePoints(FileName, BestMatches,OutDirectory,Lines,Columns,c(4,4,5,5), topology,Comment)
#
# Save bestmatches (bm) of ESOM and/or ProjectedPoints (xy) to a *.pbm file.
#
# INPUT
# FileName              FileName of *.bm file
# BestMatches[1:n,1:3]  2-5 column numeric matrix with key, row, column
# or [1:n,1:2]          if 2 columns, assuming only bestmatches ("Rueckwaertscompatibel")
#                       if 3 columns, assumes, that first columns is key("Rueckwaertscompatibel")
#                       if more than 3 Columns than Also ProjectedPoints are give
#                       if you want to save only ProjectedPoints (not rectangular grid based points)
#                       please define DataDefined with c(5,5)
#                       NOTE: ProctedPoints[1:n,Key,X,Y] but 
#                       BestMatchingUnits[1:n,Key,Y,X]=BestMatchingUnits[1:n,Key,Lines,Columns]
#                       BestMatchingUnits are defined such that the begin at [1,1] not [0,0]
#
# Lines                 Defines Grid Size (y-axis) of the corresponding U-matrix 
#                       NOTE: One of Lines starts at the top, y-axis starts at bottom
# Columns               Defines Grid Size (x-axis) of the corresponding U-matrix 
#                       Beware: Lines and Columns has to be defined, if BestMatches and ProjectedPoints are given!
#                       Lines=    ceiling(max(BestMatches(:,2)) if not given
#                       Columns=  ceiling(max(BestMatches(:,3)) if not given
# OPTIONAL
# DataDefined(1:2) or (1:3)   vector of column type: 9 For Key, 4 for Points on rectangular quad grid, 5 for hexagonal grid or other projections
#                             if not specified BestMatchingUnits will be saved
#                             if c(5,5) or c(9,5,5) given, both the ProjectedPoints and the Grid Coonversion will be saved
#                             The GridPoints will always be the the second and third Column
#
# topology                    used topology, either "planar" or "toroid"
#                             if not given, assuming for ProjectedPoints planar and
#                             for BestMatches toroid
#
#
# OutDirectory                where to write, default = current dir
#
# Comment                   array of characters to be written in the first line of the file, it 
#    									      will be marked with '\#', more than one line, use '\n#'
#                           Not More than 3 seperate Lines of Comments allowed
# author: MT 07/2015
  checkFilename(FileName,Directory=OutDirectory,Extension='xybm',ReadOrWrite=FALSE,NameOfFunctionCalled='WritePoints()')
filename=addext(FileName,'xybm')
CurrentDir = getwd()
setwd(OutDirectory)
# Check for Key Kolumns
n = nrow(BestMatches) # number of data points
c=ncol(BestMatches)
if(is.null(DataDefined)){
    DataDefined=c(9,rep(4,c))
}else{
  if(length(DataDefined) == 2 || length(DataDefined)==4){
    DataDefined = c(9,DataDefined)           
  } #KeyColumn nicht angegeben
  if(length(DataDefined) == (c+1)){ # KeyColumns angegeben aber kein Key mitgeliefert
     warning('KeyColumn is missing, generating new key')
    BestMatches=cbind(1:n,BestMatches)
    c=c+1
  }
  if(length(DataDefined) != c) stop(paste0('Wrong length of DataDefined: ',DataDefined,'. It should be of length ',c))
}
# Kein DataDefined und Keine KeyColumn
if(c==2){
  warning('KeyColumn is missing, generating new key')
  BestMatches=cbind(1:n,BestMatches)
  if(length(DataDefined) == 2){DataDefined = c(9,DataDefined)}
}
if(c==4){
  warning('KeyColumn is missing, generating new key')
  BestMatches=cbind(1:n,BestMatches)
  if(length(DataDefined) == 2){DataDefined = c(9,DataDefined)}
}
# check if Lines and Columns set
if(is.null(Lines)){
  Lines=max(BestMatches[,2])
  warning(paste0('Lines not set, Assuming Lines = ',max(BestMatches[,2])))
}else{
  if(max(BestMatches[,2])>Lines)
      stop(paste0('Maximum in BestMatches[,2] ',max(BestMatches[,2]), 'higher than grid size (Lines) ',Lines))
}
if(is.null(Columns)){
  Columns=max(BestMatches[,3])
  warning(paste0('Columns not set, Assuming Columns = ',max(BestMatches[,3])))
}else{
  if(max(BestMatches[,3])>Columns)
      stop(paste0('Maximum in BestMatches[,3] ',max(BestMatches[,3]), 'higher than grid size (Columns) ',Columns))
}
if(Lines %%1 !=0)
  if(DataDefined[2]==4){
    stop(paste0('You defined, that your points lie on a grid(BestMatches), but Lines is not an Integer:', Lines))
  }
if(Columns %%1!=0)
  if(DataDefined[2]==4){
    stop(paste0('You defined, that your points lie on a grid(BestMatches), but Columns is not an Integer:', Columns))
  }

if(is.null(topology)){
  ind=which(DataDefined==5)
  if(length(ind)!=0){
    topology='planar'
    print('topology not set, Assuming topology = "planar"')
  }else{
    topology='toroid'
    print('topology not set, Assuming topology = "toroid"')
  }
}

header1 <- paste0('%',ceiling(Lines),"\t",ceiling(Columns)) # size of the matrix that was used
header2 <- paste0('%',nrow(BestMatches), "\t", topology) # dimensions of bestmatches/projections and the used topology
header3 <- paste0('%', paste(DataDefined, collapse="\t")) # Headers; every header is just a 1
header = c(header1,header2,header3)
    if(missing(Comment)){
        write.table(header, file=filename, quote=FALSE, row.names=FALSE, col.names=FALSE, na='NaN')
    }else{
       write.table(paste0('# ',Comment), file=filename, quote=FALSE, sep='\t',row.names=FALSE, col.names=FALSE, na='NaN')
       write.table(header, append=TRUE, file=filename, quote=FALSE, row.names=FALSE, col.names=FALSE, na='NaN')
    }

#header = c(paste('%',Lines,Columns),paste('%',n))
#write.table(header, filename, quote=FALSE, row.names=FALSE, col.names=FALSE)


write.table(BestMatches, file=filename, append=TRUE, quote=FALSE, sep='\t', row.names=FALSE, col.names=FALSE, na='NaN')
setwd(CurrentDir)

}

