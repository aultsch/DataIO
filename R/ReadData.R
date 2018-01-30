  ReadData <- function(FileName,InDirectory=getwd()){
  # V=ReadData(FileName)
  # V=ReadData(FileName,InDirectory)
  # Data=V$Data
  # Header=V$Header
  # Key=V$Key
  # DataDefined=V$DataDefined
  # Names=V$Names
  # Version= 0.9.1
  # The *.data file format enables the user to import data to Microsoft Excel,  
  # Mathworks Matlab and allows to inspect and edit data within every text editor. 
  # *.data file format is a tab separated csv file with specific header. 
  # To prevent failures each column is saved with a CRC-32 checksum. 
  # Every *.data file contains a DataDefined line which describes the data in each column. 
  # The key column is a unique column of integer values. One value codes exactly one data 
  # record, where this one line of data equals one observation.
  # Numbers are stored under \code{Data} inside the file Strings, 
  # using \code{DataDefined} =  9,0,1. Strings are saved as Names using 
  # \code{DataDefined} = 6,7. \code{DataDefined} 2,3 and 8 are   used for internal purposes. 
  #
  # INPUT:          
  # FileName: 				string, name of the *.data file
  #
  # OPTIONAL:     
  # DataDirectoy: 				string of the directory containing the *.data file
  #												(default ==  	getwd() )              
  #
  # OUTPUT list V with:
  # V$Data[1:n,1:d]       a \code{[n,k]} matrix containing the data
  # V$Header[1:d+1,1:s]   string vector with the names for the key and data columns,
  #              					by default 'Key' for \code{DataDefined==9}, 'Cls' for 
  #												\code{DataDefined==3}, 'C1', 'C2', 'C3', 'C4',  etc. for 
  #												\code{DataDefined==1 or ==2}. Note: must not contain whitespace
  # V$Key[1:d]            a numeric vector, unique number for each line, by default
  #												\code{1:n}, \code{n} being the number of data points 
  # V$DataDefined[1:d]    the "Defined" line of *.data
  #                                 Values for DataDefined: 9 -> Key ( only one column allowed !)
  #                                                         0 -> Columns that are not supposed to be considered.
  #                                                         1 -> numeric -> Columns  containing numeric data 
  #                                                         2 -> numeric -> Columns containing numeric data with some special condition such as trainig vs testing
  #                                                         3 -> numeric -> Columns that contain a pre-classification of the data
  #                                                         4 -> for BestMatchingUnits=Points on rectangular quad grid, 
  #                                                         5 -> for Points on hexagonal grid or other projections
  #                                                         6 -> character -> Colunms containing names 
  #                                                         8	-> character ->	Columns containing character data with some special condition such as trainig vs testing
  #
  #
  # V$Names[1:n,:]             string matrix of short identifiers for each line to be 
  #														 put as last column. If not available value is NULL
  #
  # AUTHOR: Tim Schneider 06/2014 <schnei5t@students.uni-marburg.de>
  # 1.Editor: MT 10/2014 Der BA entnommen und von *.lrna in *.data umdefiniert und an AG 
	#												Ansprueche angepasst
  #           MT 11/2014 In Format analog von LRN/Names gebracht, Kommentare verbessert
	#						MT 01/2015 Interface neu definiert
	#						MT 02/2015 diverse bigfixes
	#						MT 04/2015 Dokumentation angepasst und verbessert
  #           MT 06/2015 Neues Interface eingefuert
	#           MT 11/2016 FurtherTexts entfernt
# MT: Falsche Dateien solle nicht eingelesen werden, u.U. aufruf von ReadLRN und ReadNames implentieren
checkFilename(FileName,Directory=InDirectory,Extension='data',ReadOrWrite=TRUE,NameOfFunctionCalled='ReadData()')

#  requireRpackage('digest')
  if(grepl('.lrn',FileName)){
    stop('Please use ReadLRN()')
  }
  
  if(grepl('.names',FileName)){
    extensioncheck=FALSE
    stop('Please use ReadNAMES()')
  }
################################################################################################################
## Inputflow Kontrolle
################################################################################################################
  FileName  = addext(FileName,'data')

  suppressWarnings((InDirectory = normalizePath(InDirectory))) 
  
  if (!(file.exists(InDirectory)) ) {# directory exisiert nicht
    stop( paste('in ReadData:', InDirectory,'existiert nicht!'))
  }
  currentWD=getwd() #Aktuelles verzeichnis merken
  setwd(InDirectory) #Ins Verzeichnis wo sich Datei befindet wechseln

################################################################################################################
  #Beginn Daten einzulesen
  #Header einlesen mit HEADERSIZE
################################################################################################################
  HEADERSIZE = 5
  
  Header = read.table(FileName,
                 sep = '\t',
                 comment.char = "%", 
                 header=FALSE,  
                 fill=TRUE, 
                 stringsAsFactors = FALSE, 
                 na.strings=c('NaN','NA'),
                 nrows=HEADERSIZE)

  #Anzahl der Spalten und Zeilen
  if (Header[1,1]=='#') rows=as.numeric(Header[1,2]) else rows=as.numeric(substring(Header[1,1],2)) # rows: number of data points (rows)
  if (Header[2,1]=='#') columns=as.numeric(Header[2,2]) else columns=as.numeric(substring(Header[2,1],2)) # columns: number of columns
  
  #Info enthält Spalten namen und Datadefined 
  info = Header[4:5,]     # info: sind #9 Zeile und die Spaltennamen
  
################################################################################################################
## Datadefined auslesen
################################################################################################################
  #loescht die Prozentzeichen aus DataDefined
  #abgefangen werden damit #[[:digit:]] & [[:digit:]]#
  info[1,]=lapply(info[1,],function(x)gsub("#","",x))
  #ende info 
  
  DataDefined= info[1,]

  DataDefined= as.numeric(DataDefined)  #Besteht jetz aus z.B. 9, 1, 1, 6 ausgbae aber dann ohne 9. Da 9 keyspalte
  #Allerdings kornnen noch zB in der *.data 1 # oder # 1 einen NA erzeugen
  DataDefined=DataDefined[!is.na(DataDefined)]   
  
  #zuwenige oder zuviele Elemente in DataDefined 
  if( length(DataDefined)!=columns ){
    stop("Error in ReadData: Wrong number of elements in DataDefined");
  }
  #ende DataDefined
################################################################################################################
## Checksummen auslesen
################################################################################################################
  #Checksummen aus header (#5 zeile)
  #checksums=Header[3,] #+1 weil '#5' nicht benötigt wird
  sums=Header[3,]
  sums[1]=gsub('#5#', '', x=sums[1]) 
  #checksums=gsub('#5#', '', x=Header[3,]) 
 
  if( length( sums )<columns ){
     stop("Error in ReadData: Wrong number of elements in checksums");
  }
   checksums=sums[1:columns]
  #checksums= checksums[-length(checksums)]
  checksums=as.character(checksums) #Liste aus den Checksums machen
  #checksums=checksums[-(which(checksums=='#5'))]
  #print(checksums)
  
#   #zuwenige oder zuviele Checksums
#   if( length( checksums )!=columns ){
#     stop("Error in ReadData: Wrong number of elements in checksums");
#   }
  #ende Checksums
################################################################################################################
## MT: u.U. gehoeren diese Zeilen gedanklich woanders hin, machen aber strukturell keinen unterschied
  #vector für die ColumnClasses (Datentypen der Spalten) zusammen stellen
  #colClasses erhoehen die lesegweschwindigkeit
  colClasses=rep('0',columns)
  
  for(i in 1:length(DataDefined)){
    if(DataDefined[i]>5 && DataDefined[i]<9){
        colClasses[i]='character'      
    }
    else{
        colClasses[i]='numeric'
    }
  }
## /
################################################################################################################
  #hiermit wird das # am Anfang der Columnnames gelöscht 
  #man kann nicht alle #-Zeichen löschen da sie auch zum Spalten namen gehören können 
  if(info[2,1] == '#'){
    ColumnNames=as.character(info[2,2:(columns+1)])
  }else{
    info[2,1]=substring(info[2,1],2)
    ColumnNames=as.character(info[2,1:(columns)])
  }
################################################################################################################
## Auslesen des Datensatzes
################################################################################################################
  #gibt fehler meldung aus wenn columns kleiner als 5 Datensätze sind.
  #Grund ist eine C-Funktion die die ersten 5 Zeilen einliest um die Daten zubestimmen
  #mehr infos: http://stackoverflow.com/questions/5990654/incomplete-final-line-warning-when-trying-to-read-a-csv-file-into-r
  Data = read.table(FileName,
                    sep='\t',
                    quote = "", #To disable quoting altogether
                    comment.char = "#",   #Kommentarzeichen
                    header=FALSE,         #Wenn die erste Zeile der Header ist. zB. bei csv
                    fill=TRUE, 
                    stringsAsFactors = FALSE, 
                    na.strings=c('NaN','NA'),
                    nrows=(rows),       #Anzahl der Zeilen zum lesen 
                    skip=HEADERSIZE,    #Die ersten HEADERSIZE zeilen werden nicht nochmal gelesen
                    colClasses=colClasses, #DatenTypen der einzlenen Spalten numeric oder character
                    col.names=ColumnNames) #gleich die Namen der Spalten setzen
                    #colnames sollten uniqe sein, sonst wird umbenannt: mit x.1 
  
  #Checksummen überprüfung
 checksumList=Checksum(Data)

   #identical?
  for(i in 1:columns){
    if(!(all.equal(checksumList[i],checksums[i])==TRUE)){
      warning(paste0('Incorrect Checksum for Column:',i))
    }
  }
################################################################################################################
## Outputflow Kontrolle
################################################################################################################
  #Data(Tabellendaten) eigenschaftenändern
  #Key ist die Spalte 9
  #which sucht diese und speichert dies um
  KeyColumn = which(DataDefined==9) #int mit der 9 spalte
  Key = Data[,KeyColumn]  #Umspeichern in Key 

  Data=Data[,-(KeyColumn)]  #Key als Spalte Loeschen
  Data=data.frame(Data) #Dataframe mit einer spalte == liste -> muss wieder data.Frame sein
  rownames(Data)=Key                    #Key als Rownames einfügen
  KeyName= ColumnNames[KeyColumn]

  ColumnNames=ColumnNames[which(DataDefined==1,arr.ind=T)] 

  #Den Key Name aus ColumnNames loeschen
   #ColumnNames=ColumnNames[which(DataDefined!=6)] #Names aus ColumnNames loeschen
  
  #9 Spalte umspeichern als erstes in Datadefined
  DataDefined=DataDefined[-KeyColumn]
  DataDefined=c(9,DataDefined)

  #zurück wechseln ins alte Verzeichnis
  setwd(currentWD)
  
  #Resultlist zusammenstellen und zurück geben
  #DataDefined= DataDefined[2:columns] #die (Key spalte) aus DataDefined löschen 
  
 #Names

  NamesColumn = which(DataDefined==6) #int mit der 6 spalte

 if(length(NamesColumn)>0){
  Names = Data[,NamesColumn-1]  #Umspeichern in Names, #-1 da Keyspalte gel?scht
 # print(Names)
  Data=Data[,-(NamesColumn-1)]  #Names als Spalte Loeschen
 }else{Names=NULL}


  
 indtmp=which(ColumnNames=='Key')
if(length(indtmp)==1)
  ColumnNames=ColumnNames[-indtmp]
 
 Data=as.matrix(Data)
 class(Data)='numeric'
 if(sum(!is.finite(Data))>0)
   warning('Numeric Data matrix is not finite!')
 
 Data[which(!is.finite(Data))]=NaN
   result=list(Data=Data,Header=ColumnNames,Key=Key,DataDefined=DataDefined,Names=Names)     

  return (result)
}
