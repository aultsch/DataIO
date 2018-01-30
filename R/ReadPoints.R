ReadPoints <- function(FileName, InDirectory = getwd(),ESOMjavaTool=FALSE) { 
# Liste = ReadPoints(FileName,InDirectory);
#  Load bestmatches (bm) and/or ProjectedPoints (xy) from *.bm file
#   
# INPUT
#   FileName                FileName of *.bm file
# OPTIONAL
#  InDirectory                 directory where *.bm file is, default: current dir
#  ESOMjavaTool                 bool, Default(FALSE), falls TRUE: Datei stammt aus ESOM Java Tool
#                               Anahme: das Tool zaehlt Punkte ab 0 und nicht ab 1, es wird also +1 gerechnet
# OUTPUT list with:
# BestMatches[1:n,1:3]          if exists,Bestmatches = [BMkey, BMLineCoords, BMColCoords]
# Lines                         Defines Grid Size (y-axis) of the corresponding U-matrix 
#                               NOTE: One of Lines starts at the top, y-axis starts at bottom
# Columns                       Defines Grid Size (x-axis) of the corresponding U-matrix 
#                               Beware: Lines and Columns has to be defined, if BestMatches and ProjectedPoints are given!
# Rows                          =Lines, historical 
#   Size                        number of data points placed into lattice from header in *.bm file
#   ProjectedPoints[1:n,1:3]    if exists, n by OutputDimension matrix containing coordinates of the Projection

#   topology                    'toroid' or 'planar' 
#
# author: MT 07/2015
# 1.Editor: Kim Schuster 07/2015
# 2.Editor: MT 07/2015
# NOTE:                 ProctedPoints[1:n,Key,X,Y] but 
#                       BestMatches[1:n,Key,Y,X]=BestMatches[1:n,Key,Lines,Columns]
#                       BestMatches are defined such that the begin at [1,1] not [0,0]
#
checkFilename(FileName,Directory=InDirectory,Extension='xybm',ReadOrWrite=TRUE,NameOfFunctionCalled='ReadPoints()')
  # add filename extension if it wasn't supplied
  BmFileName = addext(FileName, 'xybm')

  CurrentDir = getwd()
  setwd(InDirectory)

  Z = read.table(BmFileName, comment.char = "#",
                 header = FALSE, as.is = TRUE, fill=TRUE,
                 na.strings=c('NA','NaN')
      )

  # Extract header information. Header should look like:
  # %50 82   <- size of neutral lattice
  # %1000    <- number of data points

  # Extract size of lattice

if (Z[1,1] == '%') {
  r <- Z[1,2]
  c <- Z[1,3]
} else {
  r <- as.integer(substring(Z[1,1], 2)) 
  c <- as.integer(Z[1,2])
}


  if (Z[2,1] == '%') {
    size <- Z[2,2]
    topology <- Z[2,3]
  } else {
    size <- as.integer(substring(Z[2,1], 2)) 
    topology <- Z[2,2]
}

info = Z[3,]
if(info[1,1] == '9%'){ #Ergaenzung da excel %9 gerne in 9 Prozent umwandelt
  info[1,1] <- '%9'
}

  # extract non-header data
  data<- Z[substring(Z[,1], 1, 1) != '%',]

m=ncol(as.matrix(data))
s=nrow(as.matrix(data))
if(s!=size)
  warning(paste('Size of Data',s,'doest not equal size stored in header',size))
if (info[1,1]=='%'){  

  INdefined=as.numeric(info[1,2:(m+1)])
  }else if(info[1,1]=='%9'){ #MT: Weiterer Fehlerabfang keine Pause zwischen 9 und %
    INdefined=c(9,as.numeric(info[1,2:(m)]))
    cceck=F
	}else{ info[1,1]=substring(info[1,1],2)
       INdefined=as.numeric(info[1,1:m])
       warning('ReadBM: Missing "%" before Description of columns, where 9 describes the Key-Column and 1 stands for a valid data column') #MT: Abfangen fehlender % Zeichen
  }  
bmind=which(INdefined==4)
keyind=which(INdefined==9)
projectedpind=which(INdefined==5)
if(length(bmind)>0){
  bm=data[,c(keyind,bmind)]


  # reset row numbering
  row.names(bm) <- c()

  # convert first column to integers, was character do to header information...
  bm[,1] <- as.integer(bm[,1])

  # output from ESOM-Tool starts at index 0 for Line- and ColCoords,
  # add +1 for R
if(ESOMjavaTool){ 
  bm[,2] <- bm[,2] + 1
  bm[,3] <- bm[,3] + 1
} 
  # remove rows with NaN vales
  bm <- bm[complete.cases(bm),]

  names(bm) = c('BMkey', 'BMLineCoords', 'BMColCoords')

  setwd(CurrentDir)


bm=as.matrix(bm)
mode(bm)='numeric'


if(length(projectedpind)==0)
  return(list(BestMatches =bm, Rows = r, Columns = c, Size = s,topology=topology))
}

if(length(projectedpind)>0){
  ProjectedPoints=as.matrix(data[,projectedpind])
  
  colnames(ProjectedPoints)=c('X','Y')
  rownames(ProjectedPoints)=NULL
  ProjectedPoints=cbind(Key=data[,keyind],ProjectedPoints)
  mode(ProjectedPoints)='numeric'
  if(length(bmind)==0)
    return(list(ProjectedPoints = ProjectedPoints, Rows = r, Columns = c, Size = size,topology=topology))
}

return(list(BestMatches =bm, Rows = r, Columns = c, Size = size,Lines=r,ProjectedPoints=ProjectedPoints,topology=topology))


} # end function ReadBM

