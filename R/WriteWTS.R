WriteWTS = function(FileName,wts, OutDirectory=getwd(), Lines=NULL,Columns=NULL, IsToroid=0,Comment){
#
# Save ESOM weights to a *.wts file
# 
# INPUT
# FileName                          name of the target file
# wts[1:Lines*Columns,1:weights]    Information stored as a List of weights in a 2D matrix, Note: Make this a  3 dimensional array through ListAsEsomNeurons (ListAsWts)
#                                   Information represents a 2D grid with a weight  for every neuron of the length of the data set (number of variables)                        
# Lines                             Number, Defines Grid Size (y-axis) of the corresponding U-matrix 
#                                   NOTE: One of Lines starts at the top, y-axis starts at bottom
# Columns                           Number, Defines Grid Size (x-axis) of the corresponding U-matrix 
#
# IsToroid                  used topology, either 0=planar" or 1=toroid
# OPTIONAL
# OutDirectory              where to write, default = current dir
# Comment                   array of characters to be written in the first line of the file, it 
#  										      will be marked with '\#', more than one line, use '\n#'
#                           Not More than 3 seperate Lines of Comments allowed
#

# author: FL/MT 07/2015
  

# jump to the output directory
filename=addext(FileName,'wts')
CurrentDir = getwd()
setwd(OutDirectory)
# check if Lines and Columns set
if(is.null(Lines)){
  Lines=50
  warning('Lines not set, Assuming Lines = 50')
}
if(is.null(Columns)){
  Columns=80
  warning('Columns not set, Assuming Columns = 80')
}
if(is.null(IsToroid)){
  IsToroid=1
  warning('topology not set, Assuming toroid')
}

# check if Lines and Columns match, if given
height = nrow(wts)
if(height!=Lines*Columns){
  stop(paste0('Lines ',Lines,' or Columns ',Columns,' has a wrong value, because its not equal to Length of list(number of rows in R matrix) ',height))
}


# if( (length(Lines) != 0) & (length(Columns) != 0) & (Lines*Columns != height) ) stop("The Lines of the data doesn't match the Lines given.")


################# probably redundant, will be removed later (22.06.2015)
#if (length(Lines)!=m) { Lines=rep(0,m)
#                       for (i in 1:m) Lines[i] = paste('s',i,sep='')
#                     }
 
# statt Spaltennamen wird jeweils eine 1 geschrieben
# for(1 in 1:m) keys[i] = '1'
 

# load all values of weight vectors into a matrix so that write.table can be used 
#wts=matrix(0,width*height,m)
#for (i in 1:k)
#   for (j in 1:l)
#   wts[(i-1)*l+j,] = som[i,j,]
################# end of redundant part

# write the header
header1 <- paste0('%',Lines,"\t",Columns, "\t", IsToroid) # size of the matrix that was used
header2 <- paste0('%',"\t",ncol(wts)) # dimensions of bestmatches/projections and the used topology
header3 <- paste0('%',"\t", paste(rep('1',ncol(wts)), collapse="\t")) # Headers; every header is just a 1
header = c(header1,header2,header3)

    if(missing(Comment)){
        write.table(header, file=filename, quote=FALSE, row.names=FALSE, col.names=FALSE, na='NaN')
    }else{
       write.table(paste0('# ',Comment), file=filename, quote=FALSE, sep='\t',row.names=FALSE, col.names=FALSE, na='NaN')
       write.table(header, append=TRUE, file=filename, quote=FALSE, row.names=FALSE, col.names=FALSE, na='NaN')
    }


# write the weight vectors, row-wise with Columns seperated by tabs
write.table(wts, file=filename, append=TRUE, quote=FALSE, sep='\t', row.names=FALSE, col.names=FALSE, na='NaN')

setwd(CurrentDir)

}

