ReadBM <- function(FileName = NULL, InDirectory = getwd(),ESOMjavaTool=F) { 
# BestMatches = ReadBM(FileName,InDirectory);
#  Load bestmatches from *.bm file
#   
# INPUT
#   FileName     FileName of *.bm file
# OPTIONAL
#   InDirectory   directory where *.bm file is, default: current dir
#  ESOMjavaTool   bool, Default(FALSE), falls TRUE: Datei stammt aus ESOM Java Tool
#                 Anahme: das Tool zaehlt Punkte ab 0 und nicht ab 1, es wird also +1 gerechnet
# OUTPUT list with:
#   BestMatches[1:n,1:3]  Bestmatches = [BMkey, BMLineCoords, BMColCoords]
#   Lines         number of ESOM rows    from header in *.bm file
#   Rows         number of ESOM rows    from header in *.bm file  (historisch) 
#   Columns        number of ESOM columns from header in *.bm file
#   Size           number of data points placed into lattice from header in *.bm file
# IsToroid        used topology, either 0=planar" or 1=toroid
# author: MT 07/2015 - neu geschrieben
  
  
  
  if (is.null(FileName)) {
    res <- ask2loadFile(".bm")
    if (is.null(res))
      stop("no file selected")
    FileName = res$FileName
    InDirectory = res$InDirectory
  }
  
  BmFileName = addext(FileName, 'bm')
  #checkFilename(BmFileName,Directory=InDirectory,Extension='bm',ReadOrWrite=TRUE,NameOfFunctionCalled='ReadBM()')
  # add filename extension if it wasn't supplied
  
  
  CurrentDir = getwd()
  setwd(InDirectory)
  
  Z = read.table(
    BmFileName,
    comment.char = "#",
    header = FALSE,
    as.is = TRUE,
    fill = TRUE,
    na.strings = c('NA', 'NaN')
  )
  
  # Extract header information. Header should look like:
  # %50 82   <- size of neutral lattice
  # %1000    <- number of data points
  
  
  # Extract size of lattice
  ZahlPattern = "[0-9]+"
  Line = as.vector(Z[1, ])
  atomicVectorInd = regexpr(ZahlPattern, Line)
  
  Inds = which(atomicVectorInd != -1)
  if (length(Inds == 2)) {
    StartRow = atomicVectorInd[Inds[1]]
    StartCol = atomicVectorInd[Inds[2]]
    EndRow = StartRow + attributes(atomicVectorInd)$match.length[Inds[1]] -
      1
    EndCol = StartCol + attributes(atomicVectorInd)$match.length[Inds[2]] -
      1
    r = as.numeric(substr(Line[Inds[1]], StartRow, EndRow))
    c = as.numeric(substr(Line[Inds[2]], StartCol, EndCol))
    ind = which(Z[1, ] == c)
    topology <- as.logical(Z[1, ind + 1])
  } else{
    #Extract size of lattice
    if (Z[1, 1] == '%') {
      r <- Z[1, 2]
      c <- Z[1, 3]
      topology <- as.logical(as.numeric(Z[1, 4]))
    } else {
      r <- as.integer(substring(Z[1, 1], 2))
      c <- as.integer(Z[1, 2])
      topology <- as.logical(as.numeric(Z[1, 3]))
    }
  }
  # Pattern = "[TF]"
  # atomicVectorInd=regexpr(Pattern, Line)
  # Start = atomicVectorInd[3]
  # End = StartRow+attributes(atomicVectorInd)$match.length[3]-1
  # topology=as.numeric(substr(Line[3],StartRow,EndRow))
  
  # Extract size of data
  Line2 = Z[2, ]
  atomicVectorInd = regexpr(ZahlPattern, Line2)
  StartRow = atomicVectorInd[1]
  StartCol = atomicVectorInd[2]
  EndRow = StartRow + attributes(atomicVectorInd)$match.length[1] - 1
  EndCol = StartCol + attributes(atomicVectorInd)$match.length[2] - 1
  s = as.numeric(substr(Line2[1], StartRow, EndRow))
  
  
  # extract non-header data
  bm <- Z[substring(Z[, 1], 1, 1) != '%', 1:3]
  
  # reset row numbering
  row.names(bm) <- c()
  
  # convert first column to integers, was character do to header information...
  bm[, 1] <- as.integer(bm[, 1])
  bm[, 2] <- as.numeric(bm[, 2])
  bm[, 3] <- as.numeric(bm[, 3])
  # output from ESOM-Tool starts at index 0 for Line- and ColCoords,
  # add +1 for R
  if (ESOMjavaTool) {
    bm[, 2] <- bm[, 2] + 1
    bm[, 3] <- bm[, 3] + 1
  }
  # remove rows with NaN vales
  bm <- bm[complete.cases(bm), ]
  
  names(bm) = c('BMkey', 'BMLineCoords', 'BMColCoords')
  
  setwd(CurrentDir)
  
  return(list(
    BestMatches = as.matrix(bm),
    Lines = r,
    Columns = c,
    Size = s,
    IsToroid = topology,
    Rows = r
  ))
  
} # end function ReadBM

