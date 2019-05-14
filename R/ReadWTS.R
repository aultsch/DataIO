`ReadWTS` <-
function(FileName,InDirectory=getwd()){  
# V = ReadWTS(FileName,InDirectory)
#
# Load ESOM weights from a *.wts file  
# 
# INPUT
# FileName      name of the file
#
# OPTIONAL 
# InDirectory     the directrory where the *.wts file is
#
# OUTPUT
# wts[1:Lines*Columns,1:weights]  Information stored as a List of weights in a 2D matrix, 
#                                 Note: Make this a 3 dimensional array through ListAsEsomNeurons
#                                       then wts3darray[Lines,Columns,weights]
#                                 Information represents a 2D grid with a weight  for every neuron of the length of the data set (number of variables)                        
# Lines                           Number, Defines Grid Size (y-axis) of the corresponding U-matrix 
#                                 NOTE: One of Lines starts at the top, y-axis starts at bottom
# Columns                         Number, Defines Grid Size (x-axis) of the corresponding U-matrix 
#
# IsToroid                        TRUE='toroid' or FALSE='planar'   from header in *.wts file
# Header                        Names of variables represented by weights
#
# Author: MT 07/2015
# alles Neu: FL 2019

  FileName = addext(FileName, 'wts')
  
  CurrentDir = getwd()
  setwd(InDirectory)
  
  ##############
  # lese header
  ##############
  con = file(FileName, "r", blocking = FALSE)
  fileContent = readLines(con)
  # finde die letzte Zeile die mit %, # oder \t beginnt. (ende des headers)
  headerLength = tail(grep("^(%|#|\\t)", fileContent),1)
  fullHeader = fileContent[1:headerLength]
  close(con)
  
  ##############
  # Werte den Header aus
  ##############
  # finde die Kommentarzeilen und entferne sie
  commentLines = grep("^#", fullHeader)
  header = fullHeader[-commentLines]
  comments = gsub("#", '', fullHeader[commentLines])
  
  # gsub entfernt hier alle tabs und leerzeichen direkt vor und nach %
  # gesplittet wird per tab und leerzeichen.
  # Proportionen
  V = strsplit(gsub("( |\t)*%( |\t)*", "", header[1]),"(\t| )")[[1]]
  rows = as.numeric(V[1])
  cols = as.numeric(V[2])
  toroid = as.logical(as.numeric(V[3]))
  # Anzahl Daten Spalten
  V = strsplit(gsub("( |\t)*%( |\t)*", "", header[2]),"(\t| )")[[1]]
  nrOfDataCols = as.numeric(V[1])
  # Data Defined
  V = strsplit(gsub("( |\t)*%( |\t)*", "", header[3]),"(\t| )")[[1]]
  dataDefined = as.numeric(V)
  
  # variable names  
  colNames = paste0("X", 1:nrOfDataCols)
  if(length(header)==4){
    V = strsplit(gsub("( |\t)*%( |\t)*", "", header[4]),"(\t| )")[[1]]
    colNames = V
  }
  
  ##############
  # lese die Daten der Tabelle
  ##############
  Data = read.table(
    FileName,
    sep = '\t',
    header = FALSE,
    comment.char = '',
    fill = TRUE,
    stringsAsFactors = FALSE,
    na.strings = c('NaN', 'NA'),
    skip = headerLength
  ) 
  
  colnames(Data) = colNames
  
  #############
  # check auf konsistenz
  #############
  if (ncol(Data) != nrOfDataCols)
    stop("Header Information of Dimensionality disagrees with data")
  if (nrow(Data)!=rows*cols)
    stop("Header Information of Rows and Columns does not match the amount of weights")
  
  ############
  # rueckgabe
  ############
  setwd(CurrentDir) # get back
  return(list(
    wts = Data,
    Lines = rows,
    Columns = cols,
    IsToroid = toroid,
    Comments = comments,
    Header = colNames))
}

