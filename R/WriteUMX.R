WriteUMX = function(FileName,UMatrix, OutDirectory=getwd(),Comment){
# WriteUMX(FileName,UMatrix)
# WriteUMX(FileName,UMatrix,OutDirectory)
#
# write U-matrix to *.umx file
# 
# INPUT
# FileName                      name of the *.umx file
# UMatrix(1:NrClines,1:NrCols)     matrix with U-Matrix height values
#
# OPTIONAL
# OutDirectory                     directory where *.umx file is written, 
#																	 default: current directory
#
  # Comment                   array of characters to be written in the first line of the file, it 
  #  										      will be marked with '\#', more than one line, use '\n#'
  #                           Not More than 3 seperate Lines of Comments allowed
  #
# OUTPUT
#
# author: Alfred Ultsch, Michael thrun
  
  
  filename = addext(FileName, 'umx')
  umx = UMatrix
  
  CurrentDir = getwd()
  setwd(OutDirectory)
  
  if (is.vector(umx)) {
    k = length(umx)
    l = 1
  }
  else {
    k = nrow(umx)
    l = ncol(umx)
  }
  
  append=F
  if (!missing(Comment)) {
    write(paste0('#\t',Comment), file = filename,append=F)
    append=T
  }

  header = paste0('%', '\t', k, '\t', l)

  write(header, file = filename,append = append)
  write.table(
    umx,
    file = filename,
    append = TRUE,
    quote = FALSE,
    sep = '\t',
    row.names = FALSE,
    col.names = FALSE,
    na = 'NaN'
  )
  setwd(CurrentDir)
  
}

