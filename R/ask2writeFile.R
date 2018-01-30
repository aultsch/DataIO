ask2writeFile <- function(extension, Suggestion="", InDirectory=getwd()){
# V <-ask2writeFile(".lrn")
# FileName = V$FileName
# InDirectory = V$InDirectory
#
# INPUT
# extension
# Suggestion    Vorschlag fuer einen Dateinamen der Standardmaessig eingestellt wird
# InDirectory
#
# OUTPUT
# FileName
# InDirectory

  setwd(InDirectory)
	requireNamespace('tcltk')
  # den optionalen punkt am anfang der Dateiendung abschneiden
  if(substr(extension,1,1)==".")
    extension = substr(extension, 2, nchar(extension))

  # sicherstellen das der pfad ohne backslash endet
  if(!is.null(InDirectory))
    InDirectory = fileparts(InDirectory)$pathstr

  tryCatch({
    # zusaetzliches toplevel fenster notwendig, da der filedialog selbst nicht in der taskbar angezeigt wird.
    # daher wird dieses fenster erstellt und dann als parent fuer den eigentlichen filedialog benutzt, und anschliessend
    # direkt wieder geloescht.
    tt<-tcltk::tktoplevel()
    filename <- tcltk::tclvalue(tcltk::tkgetSaveFile(initialfile=Suggestion,filetypes = paste0("{",extension," {.",extension,"}}"),
                                       defaultextension=paste0(".","extension"), parent=tt))
    tcltk::tkdestroy(tt)
    if(filename == "") return()
    V <- fileparts(filename)

    FileName = paste0(V$name, V$ext)
    InDirectory = V$pathstr
  },error=function(ex){
    print('Error while loading data.')
    res=NULL
    return(NULL)
  })

  return(list(FileName = FileName, InDirectory=InDirectory))

}
