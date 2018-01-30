WriteBM = function(FileName,BestMatches,OutDirectory=getwd(),Lines,Columns,IsToroid=1,Comment){
# WriteBM(FileName, BestMatches);
# WriteBM(FileName, BestMatches,OutDirectory);
#
# Save bestmatches to a *.bm file.
#
# INPUT
# FileName              FileName of *.bm file
# BestMatches           2-3 column integer matrix with key, row, column
#                       if 2 columns, only bestmatches
#                       if 3 columns, assumes, that first columns is key
# OPTIONAL
# OutDirectory          where to write, default = current dir
#   Lines         number of ESOM rows    from header in *.bm file
#   Columns        number of ESOM columns from header in *.bm file
#                       Lines=    max(BestMatches(:,2) if not given
#                       Columns=  max(BestMatches(:,3) if not given
# IsToroid        used topology, either 0=planar" or 1=toroid
  # Comment                   array of characters to be written in the first line of the file, it 
  #  										      will be marked with '\#', more than one line, use '\n#'
  #                           Not More than 3 seperate Lines of Comments allowed
  #
# author: MT 08/2015
  
  
  filename = addext(FileName, 'bm')
  CurrentDir = getwd()
  setwd(OutDirectory)
  n = nrow(BestMatches) # number of data points
  ccc = ncol(BestMatches)
  if (ccc > 3)
    stop('Wrong number of columns')
  if (ccc == 2) {
    warning('KeyColumn is missing, generating new key')
    BestMatches = cbind(1:n, BestMatches)
  }
  
  if (missing(Lines))
    Lines = max(BestMatches[, 2])
  if (missing(Columns))
    Columns = max(BestMatches[, 3])
  
  
  #header = c(paste('%',UmxSize[1],UmxSize[2]),paste('%',n))
  header1 <-
    paste0('%', "\t", Lines, "\t", Columns, "\t", IsToroid) # size
  header2 <- paste0('%', "\t", n)
  header = rbind(header1, header2)
  
  if (missing(Comment)) {
    write.table(
      header,
      filename,
      quote = FALSE,
      row.names = FALSE,
      col.names = FALSE
    )
    
  } else{
    write.table(
      paste0('# ', Comment),
      file = filename,
      quote = FALSE,
      sep = '\t',
      row.names = FALSE,
      col.names = FALSE,
      na = 'NaN'
    )
    write.table(
      header,
      filename,
      quote = FALSE,
      row.names = FALSE,
      col.names = FALSE
    )
  }
  write.table(
    BestMatches,
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
