Checksum <- function(INData){
# Checksum(INData,parallel)
#
# Version= 0.1 
#
# DESCRIPTION:  Checksum calculates the checksums for *.lrna file see dbt\ZFileFormatDocuments\lrna.html
#
# INPUT:        
# INData[1:d,1:n]        matrix, containing the Data to be processed       
#
# OUTPUT:
# Checksums[1:n]         vector, one checksum for each column in INData 
#
#
# AUTHOR: Tim Schneider 06/2014 <schnei5t@students.uni-marburg.de>
# EDITOR: Sabine Püls, Doku und mit CRC32.dbt() erweitert 
  
  
  # diese Funktion berechnet die Checksummen und gibt eine Liste mit Checksummen zurück
  # Auch das Parallelisieren wird hier übernommen 
  # allerdings ist das noch nicht implementiert
  
  # library(digest) #berechnet Checksums
  
  # if(!require(digest)){
  #  install.packages("digest")
  #  library(digest)
  # }else{library(digest)}
    #import(digest);
    
    columns=dim(INData)[2]
    requireNamespace('digest')
    resultList=lapply(INData,function(x){digest::digest(x,algo='crc32')})
	#resultList=apply(X$Data,2,function(x){CRC32.dbt(x)})
    resultList=as.character(resultList)
    
    return(unlist(unname(resultList)))
}
