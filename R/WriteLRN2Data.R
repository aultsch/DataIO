WriteLRN2Data=function(LRNFileName,LRNDirectory=getwd(),NamesFileName,NamesDirectory=LRNDirectory){
# WriteLRN2Data(LRNFileName)
# reads *.lrn file and writes *.Data File
#
# INPUT
# LRNFileName           name of *.lrn file WITHOUT *.lrn!
#
# OPTIONAL
# LRNDirectory          directory where *.lrn file is  (default ==  getwd() )
# NamesFileName  	    name of *.names file, The both will be read out, WITHOUT *.names!
# NamesDirectory      directory where *.names file is 
#                     default ist LRNDirectory
# Output
# *.data file in same Directory as LRNDirectory with same filename as LRNFileName
#
  if(!missing(LRNFileName)){
    
  V<- ReadLRN(LRNFileName,LRNDirectory)
  Data=    V$Data
  Key =    V$Key
  columnnames =  V$Header
  DataDefined=V$DataDefined
  if(missing(NamesFileName)){
     WriteData(FileName=LRNFileName,Data=Data,OutDirectory=LRNDirectory,Key=Key,DataDefined=DataDefined,Header=columnnames)
      return('Only *.lrn dataname was defined. Data written out.')
  }
  }else{
    Data=    c()
    Key =    c()
    Names =  c()
    DataDefined=c()
    columnnames=c()
  }
  
  if(!missing(NamesFileName)){
    Out = ReadNAMES(NamesFileName,NamesDirectory);
    NamesKey <- Out$Key
    Names    <- Out$Names
    if(missing(LRNFileName)){
      DataDefined=c(9,6,7)
      WriteData(FileName=NamesFileName,Data=Names,OutDirectory=NamesDirectory,Key=NamesKey,DataDefined=DataDefined,Header=columnnames)
    return('Only *.names dataname was defined. Data written out.')      
    }else{
      
      #DataDefinedNames <-c(6,7)
      #columnnames=c(columnnames,'Names','Description')
    }
  }
  
  if(!missing(NamesFileName) && !missing(LRNFileName)){
  TheSameKey  <- TheSameKey(Key, NamesKey)
  if(!TheSameKey){stop('Key of *.names and *.lrn is not the same')}
  
  #DataDefined=c(DataDefined,DataDefinedNames)
  }

  WriteData(FileName=LRNFileName,Data=Data,OutDirectory=LRNDirectory,Key=Key,Header=columnnames,Names=Names)
}