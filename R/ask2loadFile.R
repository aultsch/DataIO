ask2loadFile <-function(extension, InDirectory="", multipleFileExtensions=F){
# V <-ask2loadFile(".lrn")
# FileName = V$FileName
# InDirectory = V$InDirectory
# Benutzerdialog zum Einstellen eines Directory und zum Vorbereiten des
# Lesens des entsprechenden DataIO Dateityps
#
# INPUT
# extension             Angabe welcher dateityp gelesen werden soll Beispiel:  extension ='lrn'
# InDirectory           Startordner in welchem gesucht werden soll
# multipleFileExtensions
#
#
# OUTPUT
# FileName          Dateiname der vom Benutzer ausgeaaehlt wurde
# InDirectory         Directory welches vom Benutzer bestimmt wurde
# author MT 12/2015
# Nota: Im Gegensatz zu Matlab wird kein DialogFilename benoetigt

  if(!is.null(InDirectory))
    InDirectory = fileparts(InDirectory)$pathstr
  else
    InDirectory = getwd()

# Wenn man mehrere extensions zur Auswahl haben moechte:
  if(!multipleFileExtensions){
    extension=gsub('\\.','', extension)
    extension=gsub('\\*','', extension)
    extension=paste0('*.',extension)
    extensionList = extension

    if(length(extension)>1) extension <- paste(extension, collapse = ';')


    res=NULL
   tryCatch({
          if(Sys.info()["sysname"]=="Windows"){
            filter=c(extension,extension)
            pathLRN <- utils::choose.files(default=paste0(InDirectory, "/_"), caption=paste("Choose",extension,"File"), multi=FALSE, filters=filter)
          }else{
            print(paste("Please choose a",extension,"File."))
            pathLRN <- file.choose()
          }

          if(length(pathLRN) == 0) return(NULL)
          if(!(paste0("*",fileparts(pathLRN)$ext) %in% extensionList)){
            warning(paste("You did not select a",extension,"File!"))
            return(NULL)
          }

          tempList <- fileparts(pathLRN)
          res=list(FileName=paste0(tempList$name,tempList$ext),InDirectory=tempList$pathstr)
          },error=function(ex){
            #warning(ex) #MT schmiert sonst mit shiny ab
  					print('Error while loading data.')
            res=NULL
          })

   return(res)
  } else{
    extension=gsub('\\*','', extension) # remove leading stars
    extension = sapply(extension, function(x) if(substring(x,1,1)==".") substring(x,2,nchar(x)) else x) # remove leading points
    extensionList = extension
    extension=paste0('*.',extension)

    if(length(extension)>1) extension <- paste(extension, collapse = ';')


    res=NULL
    tryCatch({
      if(Sys.info()["sysname"]=="Windows"){
        filter=c(extension,extension)
        pathLRN <- choose.files(default=paste0(InDirectory, "/_"), caption=paste("Choose",extension,"File"), multi=FALSE, filters=filter)
      }else{
        print(paste("Please choose a",extension,"File."))
        pathLRN <- file.choose()
      }

      if(length(pathLRN) == 0) return(NULL)


      filenameWithExtension = paste(c(fileparts(pathLRN)$name,fileparts(pathLRN)$ext), collapse="")

      matchingExtension = sapply(extensionList, function(e){
        len = nchar(filenameWithExtension)
        if(len < nchar(e)) return(FALSE)
        ext = substring(filenameWithExtension, len - nchar(e)+1, len)
        if(ext == e) return(TRUE)
        else return(FALSE)
      })

      if(!any(matchingExtension)){
        warning(paste("You did not select a",extension,"File!"))
        return(NULL)
      }


      tempList <- fileparts(pathLRN)
      res=list(FileName=paste0(tempList$name,tempList$ext),InDirectory=tempList$pathstr)
    },error=function(ex){
      #warning(ex) #MT schmiert sonst mit shiny ab
      print('Error while loading data.')
      res=NULL
    })

    return(res)
  }
}
