ReadUMX <-function(FileName = NULL,InDirectory=getwd()){ # value: data.frame of the U-matrix umx
# UMatrix <-    ReadUMX(FileName,InDirectory);
#   
#   Load height matrix from *.umx file
#   
#   INPUT
#   FileName                        name of the *.umx file
#   OPTIONAL
#   InDirectory                       directory where *.umx file is, default: current dir
#   
#   OUTPUT
#   UMatrix[1:Nrlines,1:NrCols]        matrix with U-Matrix height values
# author: Michael Thrun
  
  
  
  if (is.null(FileName)) {
    res <- ask2loadFile(".umx")
    if (is.null(res))
      stop("no file selected")
    FileName = res$FileName
    InDirectory = res$InDirectory
  }
  
  FileName = addext(FileName, 'umx')
  
  CurrentDir = getwd()
  setwd(InDirectory)
  
  Z = read.table(
    FileName,
    comment.char = "#",
    header = FALSE,
    fill = TRUE,
    na.strings = c('NA', 'NaN'),
    stringsAsFactors = FALSE
  )
  
  ZahlPattern = "[0-9]+"
  Line = as.vector(Z[1,])
  atomicVectorInd = regexpr(ZahlPattern, Line)
  Inds = which(atomicVectorInd != -1)
  if (length(Inds) == 2) {
    StartRow = atomicVectorInd[Inds[1]]
    StartCol = atomicVectorInd[Inds[2]]
    EndRow = StartRow + attributes(atomicVectorInd)$match.length[Inds[1]] -
      1
    EndCol = StartCol + attributes(atomicVectorInd)$match.length[Inds[2]] -
      1
    k = as.numeric(substr(Line[Inds[1]], StartRow, EndRow))
    l = as.numeric(substr(Line[Inds[2]], StartCol, EndCol))
  } else{
    if (nchar(Z[1, 1]) == 1) {
      k = Z[1, 2]
      l = Z[1, 3]
    }
    else {
      k = as.numeric(substring(Z[1, 1], 2, nchar(Z[1, 1])))
      l = Z[1, 2]
    }
  }
  Data = as.matrix(Z[2:(k + 1), 1:l])
  mode(Data) = 'numeric'
  rownames(Data) = 1:nrow(Data)
  if (ncol(Data) == 1)
    colnames(Data) = 'V1'
  Data[which(is.na(Data))] = NaN
  setwd(CurrentDir)
  return(Data)
}

