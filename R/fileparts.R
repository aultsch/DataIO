fileparts=function(pathname) 
# FP = fileparts(pathname)
# Path      = FP$pathstr
# Name      = FP$name
# Extension = FP$ext
# 
# INPUT
# pathname      character string representing pathname to be parsed    
# 
# OUTPUT
# a list containing
# pathstr             character string representing directory path
# name                character string representing base of file name
# ext                 character string representing file extension
#author: MT 12/2015
# ANALOG to MATLABs function
{#
 
  file=basename(pathname)
  ext=NULL
  tryCatch({
    atomicVectorInd=regexpr("\\.([[:alnum:]]+)$", file)
    Start = atomicVectorInd
    End=Start+attributes(atomicVectorInd)$match.length-1
    ext=substr(file,Start,End)
    file2=gsub(ext,'', file)

  },error=function(e){
#    requireRpackage('tools')
    ext=file_ext(file)
    file2=gsub(ext,'', file)
  })
  return(list(pathstr=dirname(pathname),name=file2,ext=ext))
}