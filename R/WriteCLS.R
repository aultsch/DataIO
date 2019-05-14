WriteCLS <- function(FileName, Cls, OutDirectory=getwd(),Key ,Comment=0){
# WriteCLS(FileName, Cls, OutDirectory,Key,Comment)
# example: WriteCLS('example.cls', c(1,1,1,1,2,2,2,2,3,3,3,3), c(1,2,3,4,5,6,7,8,9,10,11,12), getwd())

# write classification to a *.cls file.

# INPUT
# FileName              filename of *.cls file
# Cls                      either 2 column matrix with keys and classes 
#  
# OPTIONAL                        or only keys (with 3rd parameter) or only classes
# OutDirectory              where to write, default = current dir
# Comment                  Array of char to be written in the first 
#														line as 		#Comment
# Key(1:n) or []            vector of row type: unique key for each line, 
#														by default: c1:n) 


# $Author: ALU 2008
# HeSa DokuCheck Feb14
# 1.Editor: MT Mai2014
# 2.Editor MT 2018


filename=addext(FileName,'cls')
CurrentDir = getwd()
setwd(OutDirectory)

if(missing(Key)) 
  Key=1:length(Cls)

newcls=Cls
if (is.data.frame(Cls)||is.matrix(Cls)) {
    n=nrow(Cls)
    if(ncol(Cls)==2){
        newcls=Cls[2]
        # take new index, if there is one, otherwise take first column as index
        if (length(Key)!=nrow(Cls)) Key=Cls[1]
        }
    # cls only one column
    else if (length(Key)!=nrow(Cls))  Key=1:nrow(Cls)
    }
if (is.vector(Cls) ) { n=length(Cls)

                       if( length(Key)!=length(Cls)) 
                         stop('Key has not the length of Cls')

                     }
Cls=newcls
if(is.character(Comment)){
  write.table(paste0('#\t',Comment), filename, quote=FALSE, row.names=FALSE, col.names=FALSE, na='NaN')
  write.table(paste0('%',"\t",n), filename, append=TRUE, quote=FALSE, row.names=FALSE, col.names=FALSE, na='NaN')
}else{
write.table(paste0('%',"\t",n), filename, quote=FALSE, row.names=FALSE, col.names=FALSE, na='NaN')
}
write.table(cbind(Key,Cls), file=filename, append=TRUE, quote=FALSE, sep='\t', row.names=FALSE, col.names=FALSE, na='NaN')

setwd(CurrentDir)

}

