SparseToFull <- function() {
  # SparseToFull()
  # Fragt nach einer Named Sparse Matrix, konvertiert diese
  # zu einer normalen Matrix und speichert diese als LRN.
  # INPUT
  # (interaktiv) Dateiname der named sparse Matrix die konvertiert werden soll
  #
  # OUTPUT
  # Datei im aktullen working directory die die non-sparse Matrix im lrn Format enthält
  #
  # AUTHOR
  # FP 07/2016
  
  endung = '.NSMmat.lrn'
  
  parm = ask2loadFile(endung, multipleFileExtensions = T) 
  
  # Endung von Dateinamen abtrennen
  name = substr(parm$FileName, 1, nchar(parm$FileName) - nchar(endung))
  
  sparse = ReadNSM(FilenameWithoutExt = name,
                   InDirectory = parm$InDirectory)
  
  WriteLRN(FileName = name, Data = as.matrix(sparse$matrix))
}