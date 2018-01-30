#' load a *.data file into a data.frame
#' 
#' Converts the outputted List from \code{ReadData} into a data.frame.
#' 
#' 
#' @param V list from \code{ReadData} call
#' @return \item{data.frame}{ data.frame with named columns and *.data file's
#' key for row names.  If names or description is available, it will be put as
#' a separate column to the data.frame. }
#' @author Tim Schneider
#' @seealso \code{addext}, \code{WriteData},\code{ReadData}
#' @keywords dataToDataframe,file
#' @examples
#' 
#' 	#needs a *.data file "test.data" in current directory (getwd())
#' 	#dataToDataFrame( ReadData("test.data") )
#' 
dataToDataframe <- function(V){
  # CALL: data <- dataToDataframe(V)
  #
  # VERSION: 0.1
  #
  # DESCRIPTION: Converts a *.data file form the dbt function ReadData() into a data.frame
  #
  #
  # INPUT:  
  # list       a list from ReadData 
  #
  # OUTPUT:    data.frame 
  # 
  # WARNING: Data which exceed more than 13 digits will be round bevor writing.
  #           
  # AUTHOR: Tim Schneider 02/2015 <schnei5t@students.uni-marburg.de>
  #
  V=as.list(V) #MT: oder anders loesen, ab function(list) fuert beim packen zu einem fehler
  if(length(V)!= 6) warning("Not enough elements in list"); #error if the list has not the correct length
  
  frame = data.frame( V$Data );
  row.names <- V$Key;

  if( length(V$Names) > 0) frame = data.frame(frame, Names=V$Names) #append the Names 
  if( length(V$Description) > 0) frame = data.frame(frame, Description = V$Description) #append the Description
  
  return(frame); 
  
}
