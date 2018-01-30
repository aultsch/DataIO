ReadIMX <- function(FileName = NULL,InDirectory=getwd()){
#   MapMask <- ReadIMX(FileName,InDirectory);
#   read a mask for a tiled UMX to display a ESOM-map
#
# INPUT
#   FileName         the name of the file to read 
#
# OPTIONAL
#   InDirectory        the directory where *.imx is, default: current dir
#
# OUTPUT        
#   MapMask             a binary array 0 where the map is displayed 1 otherwise
# author Michael Thrun  
  
  if (is.null(FileName)) {
    res <- ask2loadFile(".imx")
    if (is.null(res))
      stop("no file selected")
    FileName = res$FileName
    InDirectory = res$InDirectory
  }
  
  FileName = addext(FileName, 'imx')
  
  CurrentDir = getwd()
  setwd(InDirectory)
  
  Z = read.table(
    FileName,
    comment.char = "#",
    header = FALSE,
    stringsAsFactors = TRUE,
    fill = TRUE,
    na.strings = c('NA', 'NaN')
  )  # vorher: stringsAsFactors = FALSE
  Data = Z[2:nrow(Z),]
  Data = as.matrix(Data)
  mode(Data) = 'numeric'
  Data[which(is.na(Data))] = NaN
  rownames(Data) = 1:nrow(Data)
  setwd(CurrentDir)
  return(Data)
}# end function ReadIMX
