WriteData <- function(FileName, Data, OutDirectory=getwd(), Header=c(), DataDefined,Key,Comment,Names){
# WriteData(FileName, Data, OutDirectory)
# WriteData(FileName, Data, OutDirectory, Header, DataDefined,Key,Comment,Names)
#
# VERSION: 0.9.1
#
#  Writes Data, \code{DataDefined}, Key and Names in the \code{*.data} file format.
#  WriteData is the function for writing *.data files.
#  
#  The *.data file format enables the user to import data to Microsoft Excel,  
#  Mathworks Matlab and allows to inspect and edit data within every text editor. 
#  *.data file format is a tab separated csv file with specific header. 
#  To prevent failures each column is saved with a CRC-32 checksum. 
#  Every *.data file contains a \code{DataDefined} line which describes the data in each column. 
#  The key column is a unique column of integer values. One value codes exactly one data record, 
#  where this single line of data equals one observation.
#  Numbers are stored under \code{Data} inside the file strings, using \code{DataDefined} = 
#  9,0,1.  Strings are saved as Names using \code{DataDefined} = 6,7. \code{DataDefined} 2,3 and 
#  8 are used for internal purposes. e
#
#
# INPUT:  
# FileName        string, name of file where the data will be written
# Data(1:n,1:d)       \code{[n,d]} matrix or a \code{data.frame} containing the data 
#         
# Optional:
# OutDirectory       string, name of directory the data will be saved in, default #										 \code{getwd()
# Header(1:d)         string vector with the names for the key and data columns,
# OR Header(1:d+1)    by default 'Key' for \code{DataDefined}==9, 'Cls' for
#											\code{DataDefined}==3, #'C1', 'C2', 'C3', 'C4' etc. for
#											\code{DataDefined}==1 or ==2, must not contain whitespace
#                     Beware: If Header(1:d) the Key Column will be named with 'Key'
# DataDefined(1:d')   vector of column type: keys (0=ignore,1=data,3=class,6=names,9=key)
#                                                         4 -> for BestMatchingUnits=Points on rectangular quad grid, 
#                                                         5 -> for Points on hexagonal grid or other projections
#                     Beware: Please define Name column only with 6
#                     Beware: if Names used d'=d+1 f not: d'=d
#                     Beware: Header and Datadefined have to have the same length minus Names
# Key(1:n)           vector of row type: unique key for each line, by default: [1:n]' ; Unique identifier for rows
# Comment             array of characters to be written in the first line of the file, it 
#											will be marked with '\%', more than one line, use '\n%'
#
# Names[1:n,:]             list or string matrix with text for each line to be put as last 
#													column, usually a short identifier 
#
# OUTPUT: 
# 
# WARNING: Data which exceed more than 13 digits will be round bevor writing.
#           
# AUTHOR: Tim Schneider 06/2014 <schnei5t@students.uni-marburg.de>
# 1.Editor: MT 10/2014    *.lrna der BA entnommen und in *.data umdefiniert/Ansprueche angepasst
#           MT 11/2014 		In Format analog von LRN/Names gebracht, Kommentare ergaenzt
#						MT 01/2015		Interface erneut angepasst
#						MT 02/2015    diverse bugfixes
#						MT 04/2015		Dokumentation korrigiert und verbessert
#           MT 06/2015    Neues Interface eingefuert
#           MT 11/2016    FurtherText entfernt
# Notes/Details:
# composition of the *.data file:
#	all comment lines start with '\%', all header lines with '#'
#	After none or one comment line the header starts with the following information:
#	1.line: number of rows in \code{Key} respective data
#	2.line: number of columns in file, including \code{Key} column
#	3.line: checksums for each column
#	4.line: names of columns defined by input variable \code{Names}, if not defined see below
#	After this the key column and the data follows, each column separated by a white space, each 
# row separated by new line.
# The length of \code{Key} has to be \code{ncol(Data)} or \code{ncol(Data[[1]])} respectively.
# If \code{Key} is empty it is replaced by a vector \code{1:ncol(Data)}
# NOTE: there can be only ONE column with \code{dataDefined == 9} i.e. the column containing the # \code{Key}
# If \code{Names} is empty, it is replaced by default names:
#  The default name for a \code{DataDefined==9} column is 'Key'.
#  The default name for a \code{DataDefined==3} column is 'Cls'.
#  The default name for \code{DataDefined==1 or ==2} is 'C1', 'C2', 'C3', 'C4' etc.
#  \code{DataDefined} may only contain numbers 0,1,2,3,6,7,8 and 9.
#	 Please use \code{DataDefined} == 6 and \code{DataDefined} ==7 only once.
#  If \code{DataDefined} is empty or wrong it is replaced by  \code{c(9,1,..)}.
checkFilename(FileName,Directory=OutDirectory,Extension='data',ReadOrWrite=FALSE,NameOfFunctionCalled='WriteData()')

#  requireRpackage('digest')
################################################################################################################
## Kontrolle des Inputflows 
################################################################################################################
  if(!missing(DataDefined)&!missing(Header))
  if(!is.null(DataDefined)&!is.null(Header)){
    ind=which(DataDefined==6)
    DataDefined2=DataDefined[-c(ind)]
    if(length(DataDefined2)!=length(Header))
      stop(paste0('Length of Header ',length(Header),' does not equal length of DataDefined without Names',length(DataDefined2)))
  rm(ind)
  rm(DataDefined2)
  } 
  
  
  ColumnNames=Header #Historisch aus ReadLRNA
keynotindata=TRUE
  colladd=1 #Anzahl die abhaengig vom Input die Anzahl an Spalten ergaenzt, abhaengig ob key als variable angeben oder in Data mit Datadefined
# if(!missing(ForNamesHeader)){
#   if(length(ForNamesHeader)>1){
#     warning('character/string ForNamesHeader is too long, using first element')
#     ForNamesHeader=ForNamesHeader[1]
#   }
# }
# intern arbeiten wir mit einem data.frame
  if (is.vector(Data) || is.matrix(Data)) {
    Data=data.frame(Data)
    m=ncol(Data) #Fuer Header
  }

# Pruefung welche Inputvariablen angegeben wurden
  if(!missing(Names)){ 
    if(!is.null(Names)){
    Names=data.frame(Names)  
    Data=cbind(Data,Names)
    }
  }
  auxcolnames=names(Data) #Fuer automatische Header Generierung

#Zeilen anzahl bestimmen
if(ncol(Data) == 0 || nrow(Data) == 0 || typeof(Data) == 'NULL'){
  stop('WriteData(): No Data to write') #stop wenn Data NULL ist
}
else{
  rows = nrow(Data)
  columns = ncol(Data)
}  

  
#TS: wenn Data aus ReadData Funktion kommt -> Rekursive aufrufen
#MT: Muss noch getestetn werden
#   if(class(Data)=="list" && length(names(Data))>0 && names(Data)==c("Data","Header","KeyName","Key","DataDefined","Checksums")){
#     WriteData(FileName,
#               Data=Data$Data,
#               ColumnNames=Data$Header,
#               #KeyName=Data$KeyName,
#               Key=Data$Key,
#               DataDefined=Data$DataDefined,
#               OutDirectory)
#   }
#   else{
    
    #richtige Datei endung
    FileName  = addext(FileName,'data')
    suppressWarnings((OutDirectory = normalizePath(OutDirectory))) 
    
    currentWD=getwd() #Aktuelles verzeichnis merken
    setwd(OutDirectory) #Ins Verzeichnis wo sich Datei befindet wechseln

################################################################################################################
## Abfang diverser Fehler bezueglich DataDefined und Key
################################################################################################################
    #datadefined selbst erstellen 
    #DataDefined wird aus Data erstellt
    if(missing(DataDefined)){
        #funktioniert nicht bei Matrizen, vektoren
        DataDefined=c()
        DataDefined[(which(unname(unlist(lapply(Data,class)))=='character'))]=6
        DataDefined[(which(unname(unlist(lapply(Data,class)))=='factor'))]=6
        
        DataDefined[which(unname(unlist(lapply(Data,class)))=='numeric')]=1
        DataDefined[which(unname(unlist(lapply(Data,class)))=='integer')]=1
#print(DataDefined)
    }else if(is.null(DataDefined)){
         #funktioniert nicht bei Matrizen, vektoren
        DataDefined=c()
        DataDefined[(which(unname(unlist(lapply(Data,class)))=='character'))]=6
        DataDefined[(which(unname(unlist(lapply(Data,class)))=='factor'))]=6
        
        DataDefined[which(unname(unlist(lapply(Data,class)))=='numeric')]=1
        DataDefined[which(unname(unlist(lapply(Data,class)))=='integer')]=1
        
    }else{ #MT:Fehlerabfang
      ddeflen=length(DataDefined)
      vergleichslaenge=columns
#       if(missing(Key)){
#         vergleichslaenge=vergleichslaenge+1
#       }
      #if(!missing(Names)){
      #  vergleichslaenge=vergleichslaenge+1
      #}

      if(vergleichslaenge!=ddeflen){
        if(!is.null(DataDefined))
        warning(paste0('Length of DataDefined ',ddeflen,' does not match number of columns of data ',vergleichslaenge,' (with or without Key, Names)'))
      }
    }

    if (sum(DataDefined==9)>1) stop('do not indicate more than one column as key') #Da hier gestopt wird (*)
    

    if (sum(DataDefined==9)==0) {   #Key ist nicht in Data
      DataDefined = c(9,DataDefined)
      if(missing(Key)){ #Keyvariable nicht angegeben
        Key = 1:rows #erstellt neuen Key wenn eingebener falsch ist
        Key=as.numeric(Key)
        keynotindata=TRUE
      }else{ #Keyvariable angegeben, pruefe laenge
        if(length(Key)!=rows)stop('Length of Key isnt equal to the length of rows')
      }
    }else{ # (*) wird das nicht ausgefuert
      indkey=which(DataDefined==9,arr.ind=TRUE)
      Key=as.vector(Data[,indkey])
      keynotindata=FALSE
      colladd=0
    }
    
    if(!missing(Key)){Keytmp = unique(Key)
                      if(length(Keytmp)!=length(Key)){warning(paste0('Key with length ',length(Key),' is not unique: ',length(Keytmp)))}else{Key=Keytmp}
    } #make key uniqe
    

#     if( length(ColumnNames) != (columns+colladd) && length(ColumnNames) > 0){ #+x f?r bis zu drei zusaetzliche spalten
#       stop(paste0("wrong number of ColumnNames: is",length(ColumnNames),', but has to be ',columns+colladd))
#     }
nokeyname=T
    if(length(ColumnNames)==0){
      if(length(names(Data))==columns){
        ColumnNamesNew=names(Data)
        
        ColumnNames=auxcolnames
        if(length(ColumnNamesNew)>length(ColumnNames)){
            for(i in (length(ColumnNames)+1):length(ColumnNamesNew)){
            ColumnNames[i]=paste0('Col',i+1)
          } 
        }
      }
      else{
        for(i in 1:columns){
          ColumnNames[i]=paste0('Col',i)
        } 
      }
      if(keynotindata){#MT: Fehlerabfang
        ColumnNames=c('Key',ColumnNames) #Key ergaenzen
      }

    }else{ #length(ColumnNames)!=0
      if(length(ColumnNames)==m-1){
          aux = Header
          aux[DataDefined!=9] = ColumnNames
          aux[DataDefined==9] = 'Key'
          ColumnNames = aux
      }
      if(length(ColumnNames) == m+1){
        nokeyname=F
      }else if(length(ColumnNames) != m){ 
         #if(!is.null(Header)){
          warning(paste0('Length of Header ',m,' unequal length of columns+1 (for keycolumn) ',length(ColumnNames)))     
          ColumnNames= as.character(DataDefined)
          ColumnNames[DataDefined==9]='Key'
          ColumnNames[DataDefined==3]='Cls'
          Ind12 = sort(na.last=T,c(which(DataDefined==1),which(DataDefined==2)))
          Header1 = paste('C',1:length(Ind12),sep='')
          ColumnNames[Ind12] = Header1
      }#end if length(ColumnNames)==m-1)
       if(ncol(Data)>length(ColumnNames)){
            for(i in (length(ColumnNames)+1):ncol(Data)){
            ColumnNames[i]=paste0('Col',i+1)
          } 
       }
      indnames=which(DataDefined==6)
      if(nokeyname){
          if(keynotindata)
            laufinteger=1
          else
            laufinteger=0
      }
      else{       
        laufinteger=0
      }
      #if(missing(ForNamesHeader))
        ColumnNames[(indnames-laufinteger)]='Names' #minus keyspalte
        #print(indnames)
      #else
        #ColumnNames[indnames-nokeyname]=ForNamesHeader #minus keyspalte
      rm(indnames)
    }    # end if length(ColumnNames)==0)  

#     if( KeyName %in% ColumnNames){ 
#       stop('KeyName is in ColumnNames')
#     }

if(keynotindata){
    #Key hinzufuegen zu ColumnNames 
    Data = data.frame(Key,Data)
    #ColumnNames=union(KeyName,ColumnNames)
    columns=columns+1 #Wenn key Hinzugefuegt wurde, muss auch die Spalten Anzahl angepasst werden
}
    #ColumnNames zu Data
    colnames(Data)=ColumnNames
################################################################################################################
#TS:    
    #letzter Schritt ist die Klassen mit class() anzupassen:
    #Problem war: das entweder nur string oder nur Zahl gefordert war
    #R aber keine explizite Typen definition vorraus setzt. 
    #und eine Fehler auftrat wenn die Pruefziffern berechnet wurden
################################################################################################################
    for(i in 1: columns){
      if(class(Data[,i])=='integer'){
        Data[,i]= as.numeric(Data[,i])
      }
      if(class(Data[,i])=='factor'){
        Data[,i]=as.character(Data[,i])
      }
      #else class(Data[,i])=='character' oder 'numeric' => bleibt
    }
################################################################################################################
## Sollten die Zahlen zu lang sein, werden Rundungsfehler beim Auschreiben behandelt
################################################################################################################
    print_warning = FALSE; #Variable fuer Warnmeldung
    spaltennummer=c()
    
    round_row <- function( x ){
      if(nchar(as.character( x ))>13){
        print_warning <<- TRUE #Variable fuer Warnmeldung
        if( !(i %in% spaltennummer) ) spaltennummer <<- c(spaltennummer,i) # push columns where data is round           
      }
      
      #x = as.numeric(formatC( round( x, digits = 13 ), digits = 13) ) MT:Fehler, da so x<a*10^-14 zu Null gerundet werden
     # if(!is.character( x )){#MT:Korrektur, damit Strings nicht gekuerzt werden
        x = suppressWarnings(as.numeric(formatC(x, digits = 13) ))
     #T: Warning message: #NAs introduced by coercion, falls Nas angegeben werden
     # aber keine weitere Auswirkung, deswegen suppressWarnings
      #}
      return (x)
    }

# Fuer letzte for schleife, hat baer nicht geklappt
#    round_column<- function(x){#MT: wird diese Funktion ueberhaupt nch verwendet?
#      if( class(x) =='numeric' ){ 
#        x = as.numeric(mapply(round_row,x))
#      } else {
#       # x = as.character(x)
#      }
#      return( x )
#    }

    #Daten runden
    for( i in 1:columns ){
       if( class(Data[,i]) =='numeric' ){
          Data[,i] = as.data.frame( mapply(round_row, Data[,i]))
       }
      }
#Anwendung von round_column hier, klappt leider nicht, deswegen obere for schleife
    #Data=as.data.frame( mapply( round_column, Data, SIMPLIFY=FALSE) )
    #colnames( Data ) = ColumnNames
  
    #Meldung wenn die Daten angepasst wurden
    if( print_warning ){
      warning_str = 'Numbers in Data exceed the maximum number of digits. The Data will be round bevor writing in column(s): '
      warning_str = paste( warning_str, paste(spaltennummer, collapse=", ") )
      warning( warning_str )
      print('If you dont want to round your data to 13 digits, you have to cast these columns from numeric to character')
    }
###---------------Daten anpassen und Kontrolle Fertig gestellt-----------------

################################################################################################################
###---------------------Beginn mit schreiben in Datei--------------------------
################################################################################################################
    #Anzahl Spalten und Zeilen schreiben
    Size=c( paste0('#\t',rows), paste0('#\t',columns) ) 

    if(missing(Comment)){
       write.table(Size, file=FileName,quote=FALSE, sep='\t', row.names=FALSE, col.names=FALSE, na='NaN')

       }else{
        write.table(paste0('% ',Comment), file=FileName, quote=FALSE, sep='\t',row.names=FALSE, col.names=FALSE, na='NaN')
        write.table(Size, file=FileName,append=TRUE,quote=FALSE, sep='\t', row.names=FALSE, col.names=FALSE, na='NaN')
    }
    #write.table(Size, file=FileName,quote=FALSE, sep='\t', row.names=FALSE, col.names=FALSE, na='NaN')
     
    #Checksummen schreiben
    checksums=Checksum(Data)
    
    cat('#5#', file=FileName, append=TRUE)
    cat(checksums,'\n',file=FileName, append=TRUE, sep='\t')
    
    #Datadefined schreiben
    cat('#', file=FileName, append=TRUE)
    cat(DataDefined,'\n',file=FileName, append=TRUE, sep='\t')
  
    #ColumnNames schreiben =Header
    cat('#', file=FileName, append=TRUE)
    indtmp=which(ColumnNames=='Key')
    
 if(nokeyname)
   if(keynotindata)
     if(length(which(ColumnNames=='Key'))==0)
      ColumnNames=c('Key',ColumnNames)

   if(length(ColumnNames)!=ncol(Data))
     stop(paste0('Length of ColumnNames ',length(ColumnNames),' does not equal length of Data and Key and or Names',(ncol(Data)),'. Somethin went wrong, please read other warnings!'))

    cat(ColumnNames,'\n',file=FileName, append=TRUE, sep='\t')
    
    #Daten schreiben
    write.table(Data, file=FileName, append=TRUE, quote=FALSE, sep='\t', row.names=FALSE, col.names=FALSE, na='NaN')

##------------Daten schreiben abgeschlossen-----------  

    #zurueck wechseln ins alte Verzeichnis
    setwd(currentWD)
#  }
}
