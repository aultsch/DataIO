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
#
# Author: MT 07/2015

  FileName = addext(FileName, 'wts')
  
  CurrentDir = getwd()
  setwd(InDirectory)
  # Header Einlesen
  # Beispiel Aufbau Header:
  # %50 82
  # %2 toroid
  # %col1 col2
  Z = read.table(
    FileName,
    sep = '\t',
    header = FALSE,
    comment.char = '',
    fill = TRUE,
    stringsAsFactors = FALSE,
    na.strings = c('NaN', 'NA'),
    nrows = 10
  ) #Achtung begrenzt auch die Anzahl an Kommentarzeichen
  HEADERSIZE = 3
  skiplines = 3
  i = 1
  while (substring(Z[i, 1], 1, 1) == '#') {
    skiplines = skiplines + 1
    i = i + 1
  }
  #print(skiplines)
  
  
  Header = read.table(
    FileName,
    sep = '\t',
    comment.char = "#",
    header = FALSE,
    fill = TRUE,
    stringsAsFactors = FALSE,
    na.strings = c('NaN', 'NA'),
    nrows = HEADERSIZE
  )
  if (is.null(Header[1, 2])) {
    #seperator ist nicht \t
    Header = read.table(
      FileName,
      sep = ' ',
      comment.char = "#",
      header = FALSE,
      fill = TRUE,
      stringsAsFactors = FALSE,
      na.strings = c('NaN', 'NA'),
      nrows = HEADERSIZE
    )
  }
  if (is.na(Header[1, 2])) {
    Header = read.table(
      FileName,
      sep = ' ',
      comment.char = "#",
      header = FALSE,
      fill = TRUE,
      stringsAsFactors = FALSE,
      na.strings = c('NaN', 'NA'),
      nrows = HEADERSIZE
    )
  }
  
  
  # HeaderLines=readLines(FileName,n=skiplines)
  # ZahlPattern = "[0-9]+"
  # atomicVectorInd=regexpr(ZahlPattern, HeaderLines[i])
  #
  # StartRow = atomicVectorInd[1]
  # StartCol = atomicVectorInd[2]
  # EndRow = StartRow+attributes(atomicVectorInd)$match.length[1]-1
  # EndCol = StartCol+attributes(atomicVectorInd)$match.length[2]-1
  # rows=as.numeric(substr(HeaderLines[i],StartRow,EndRow))
  # cols=as.numeric(substr(HeaderLines[i],StartCol,EndCol))
  
  
  
  if (Header[1, 1] == '%') {
    # Moeglicher Spezialfall, zwischen % und erster Zahl ein tab
    rows = as.numeric(Header[1, 2])
    cols = as.numeric(Header[1, 3])
    topology = as.logical(Header[1, 4])
  } else{
    #Standardfall
    rows = as.numeric(substring(Header[1, 1], 2))
    cols = as.numeric(Header[1, 2])
    topology = as.logical(Header[1, 3])
    
  } # rows: number of data points (rows)
  
  if (Header[2, 1] == '%') {
    # Moeglicher Spezialfall, zwischen % und  Zahl ein tab
    weigthlength = as.numeric(Header[2, 2])
    #topology=as.logical(Header[2,3])
  } else{
    #Standardfall
    weigthlength = as.numeric(substring(Header[2, 1], 2))
    #topology=as.character(Header[2,2])
  } # rows: number of data points (rows)
  
  # Datei lesen ohne Kommentar und ohne Header, es koennen NA-Spalten auftreten!!!
  #Z = read.table(FileName, comment.char = "#",header = FALSE, stringsAsFactors = FALSE, fill=TRUE, na.strings=c('NA','NaN'))  # korrigiert am 05.08.09: stringsAsFactors
  #colClasses erhoehen die lesegweschwindigkeit
  Data = read.table(
    FileName,
    sep = '\t',
    quote = "",
    #To disable quoting altogether
    comment.char = '%',
    #Kommentarzeichen
    header = FALSE,
    #Wenn die erste Zeile der Header ist. zB. bei csv
    fill = TRUE,
    stringsAsFactors = FALSE,
    na.strings = c('NaN', 'NA'),
    #Anzahl der Zeilen zum lesen
    skip = skiplines,
    #Die ersten HEADERSIZE zeilen werden nicht nochmal gelesen
    colClasses = 'numeric' #DatenTypen der einzlenen Spalten numeric oder character
  )
  Data = as.matrix(Data)
  n = ncol(Data)
  r = nrow(Data)
  if (n != weigthlength) {
    stop(
      paste0(
        'Length of wts vector (weights) ',
        n,
        ' does not equal Header Information ',
        weigthlength
      )
    )
  }
  if (r != rows * cols) {
    stop(
      paste0(
        'Lines ',
        rows,
        ' or Columns ',
        cols,
        ' is not equal to Length of list(number of rows in R matrix) ',
        r
      )
    )
  }
  
  # load height and width of the som its based on
  # if (Z[1,1]=='%'){# if space after %-sign, the data is in the next col
  #   height = as.numeric( Z[1,2])
  #   width = as.numeric(Z[1,3])
  # }
  # else{
  #   height = as.numeric(substring(Z[1,1], 2))
  #   width = as.numeric(Z[1,2])
  # }
  #
  # # load nr of columns and topology
  # if (Z[2,1]=='%'){
  #   ncols = as.numeric(Z[2,2])
  #   topology = Z[2,3]
  # }
  # else{
  #   ncols = as.numeric(substring(Z[2,1],2))
  #   topology = Z[2,2]
  # }
  #
  # # skip 3rd row with columnheaders (they don't make sense for bm anyway)
  #
  # # count dataentries
  # nrows <- nrow(Z)-3
  #
  # # convert dataframe to matrix and extract datarows
  # data = as.matrix(Z)[4:(nrows+3),]
  
  # convert strings to numbers in the matrix
  #data = matrix(as.numeric(data), ncol=ncols)
  
  
  
  ############## this part is probably redundant and will be deleted later
  #kl = Z[1,]
  #kl=kl[which(!is.na(kl))]
  #if (length(kl)==3) kl = kl[2:length(kl)] else kl[1] = substring(kl[1],2)
  #kl = as.numeric(kl)
  #kl = kl[which(!is.na(kl))]   # Added 28.07.14 Onno Hansen-Goos
  #k=kl[1]
  #l=kl[2]
  
  #Zzeilen =nrow(Z) # wieviele Zeilen
  
  #Keys = Z[3,] # hier stehen die Keys
  #Keys = Keys[which(!is.na(Keys))] # NA entfernen
  
  # unterscheide: erste Zeichen '%1' oder '%' 1 -> '%' entfernen -> erstes Zeichen 1
  #if (Keys[1]=='%') Keys = Keys[2:length(Keys)] else Keys[1]=substr(Keys[1],2,nchar(Keys[1]))
  #names(Keys) = 1:length(Keys)
  #rownames(Keys) = c()
  # extract Data, discard NA columns
  #Data = Z[4:Zzeilen,1:m]
  
  #Data = as.matrix(Data)
  #mode(Data)='numeric'
  #Data[which(is.na(Data))]=NaN
  
  #wts = array(0,c(k,l,m))
  
  #for (i in 1:k)
  #   for (j in 1:l)
  #    wts[i,j,]= Data[(i-1)*l+j,]
  ################   end of redundant part
  if (skiplines - 3 > 0) {
    Comments = read.table(
      FileName,
      sep = '\n',
      comment.char = "",
      quote = "",
      header = FALSE,
      fill = T,
      strip.white = TRUE,
      stringsAsFactors = F,
      nrows = skiplines - 3,
      blank.lines.skip = TRUE,
      skip = 0
    )
    Comments = as.matrix(Comments)
    ind = grep('#', Comments)
    if (length(ind) > 0) {
      Comments = as.matrix(Comments[ind, ])
      Comments = sub('# ', '', Comments)
      Comments = sub('#', '', Comments)
      Comments = sub('\t', '', Comments)
    }
  } else{
    ind = NULL
  }
  setwd(CurrentDir) # get back
  if (length(ind) > 0) {
    return(list(
      wts = Data,
      Lines = rows,
      Columns = cols,
      IsToroid = topology,
      Comments = Comments
    ))
  } else{
    return(list(
      wts = Data,
      Lines = rows,
      Columns = cols,
      IsToroid = topology
    ))
  }
}

