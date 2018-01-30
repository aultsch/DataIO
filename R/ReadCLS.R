ReadCLS <- function(FileName = NULL,InDirectory=getwd()){
#   V <- ReadCLS(FileName,InDirectory);
#   ClsKey = V$ClsKey
#   Cls    = V$Cls  
#   Load key-class assignments from *.cls file
#   
# INPUT
#   FileName     filename of *.cls file
#   
# OPTIONAL
#   InDirectory    directory where *.cls file is, default is current directory
#   
# OUTPUT
# a list with:
#   ClsKey(1:d)       1 column integer matrix of Keys
#   Cls(1:d)          1 column integer matrix of ClassIntegers
#   UniqueClasses     unique class numbers in Cls or (prefreably from header
#   ClassColors       Vector of hexadecimal color values for classes from header
# author: Alfred Ultsch             
  
  getClassColor <- function(id, colorheaders){
    n = which(strtoi(substr(colorheaders[,1], 2, nchar(colorheaders[,1]))) == id)
    if(length(n) == 0){
      col = sample(colours(),1)
      warning(paste("Class", id, "has no color information. Setting random:", col))
      return(col)
    } else if(length(n) > 1) {
      n  = n[1]
      warning(paste("Class", id, "has multiple color informations. Using first entry only"))
    } 
    toHex <- function(number){
      n = as.hexmode(number %% 256)
      if(nchar(toString(n)) == 1)
        n = paste0('0',n)
      return(n)
    }
    r = toHex(colorheaders[n,2])
    g = toHex(colorheaders[n,3])
    b = toHex(colorheaders[n,4])
    
    col = paste0('#',r,g,b)
    
    return(col)
  }
  
  if(is.null(FileName)){
    res <- ask2loadFile(".cls")
    if(is.null(res)) stop("no file selected")
    FileName = res$FileName
    InDirectory = res$InDirectory
  }
  
  FileName = addext(FileName,'cls')
  
  CurrentDir = getwd()
  setwd(InDirectory)
  
  Z = read.table(FileName, comment.char = "#",header = FALSE, stringsAsFactors = FALSE, fill=TRUE,na.strings=c('NA','NaN'))
  
  # Achtung: es koennen mehrere %-Zeilen auftreten, laut Beschreibung nur eine, aber siehe dbt/DataIo/test/hexa.cls (Farbcodierung)
  # feststellen, wieviele %-Zeilen, die erste kann entfernt werden
  DataInd = which(substr(Z[,1],1,1)!='%')
  #ColorInd = which( (substr(Z[,1],1,1)=='%') & !is.na(Z[,2]+Z[,3]+Z[,4])) 
  
  # one or two columns
  if (prod(is.na(Z[DataInd,2]))==1) {Cls = as.data.frame(Z[DataInd,1]) # 2nd col == NA -> read only first column
  }else { Cls = as.matrix(Z[DataInd,2])
         mode(Cls) = 'numeric'
         ClsKey = as.matrix(Z[DataInd,1])
         mode(ClsKey) = 'numeric'
  }
  colnames(Cls) = 'Cls'
  Cls[which(is.na(Cls))]=NaN
  UniqueClasses = as.matrix(as.numeric(levels(factor(Cls))))
  #if(length(ColorInd) > 0){
 #   ClassColors = sapply(X = UniqueClasses,FUN = getClassColor, colorheaders = Z[ColorInd,] )
 # }
  setwd(CurrentDir)
  ClassColors=NULL
  return(list(ClsKey=ClsKey, Cls=as.vector(Cls), UniqueClasses = UniqueClasses, ClassColors = ClassColors))
}# end function ReadCLS

