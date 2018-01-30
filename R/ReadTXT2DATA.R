ReadTXT2DATA <- function(TxtFilename, TxtDirectory = getwd(), NamesInd = -1, DataDefined = c(), SkipLines = 0){

# ReadTXT2DATA(TxtFilename,TxtDirectory)
#
# Version= 0.1 
#
# DESCRIPTION:  reads *.txt tables to data file format, see dbt\ZFileFormatDocuments\data.html
#               actual allowed format of *.txt file:
#               - table separated with tabs
#               - comment character is '#', defined by default read.table() function
#
# INPUT:          
# TxtFilename: name of .*data file. Including file extension '*.data'
# OPTIONAL:     
# TxtDirectoy: Directory where *.data file is  (default ==  getwd() )
# NamesInd      integer, column with names of cases, genes etc.
# SkipLines     integer, the number of lines of the data file to skip before beginning to read data
#
# OUTPUT list V with
# V$Data(1:n,1:d)          matrix, numeric data with n lines and d columns
# V$Header(1:d+1)          array, column names ( +1 for key column)
# V$Key(1:n)               array, (unique?) key for each line
# V$DataDefined(1:d)       array, the "Defined" line of *.data
#                          Values for DataDefined: 
#                             9 -> Key ( only one column allowed !)
#                             0 -> columns that are not supposed to be considered.
#                             1 -> numeric -> columns containing numeric data 
#                             2 -> numeric -> columns containing numeric data with some special condition such as trainig vs testing
#                             3 -> numeric -> columns that contain a pre-classification of the data 
#                             6 -> character -> columns containing names 
#                             7 -> character -> columns containing descriptions
#                             8 -> character -> columns containing character data
# V$Names(1:n)             array, names e.g. case, gen etc.
# V$Description(1:n)       array, descriptions for each name, case, gen etc.
# V$AllStringData(1:n,1:c) matrix, character data with n lines and c columns, including Names and Description
#
# AUTHOR: Sabine Püls 3/2015
# EDITOR:

# NOTA 3/2015 Leerstellen werden leer belassen, sollten lieber mit NA befuellt werden

# INPUT
old <- getwd()
setwd(TxtDirectory)
# first line are headers, separation via tab, unequal line length will be fulfilled, SkipLines would be skipped before reading table
TxtRaw <- read.table(TxtFilename, header = TRUE, sep = '\t',fill = TRUE, skip = SkipLines)
# fill empty elements with 'NA'
# TxtRaw[TxtRaw == ''] <- 'NA'
# HESA: warum geht das nicht???
# (NAs auch dort wo eigentlich Daten standen)

# PROCESSING
Header <- names(TxtRaw)

# Key will be first column with unique numeric data
x <- TxtRaw[,1]
i <- 0
while(!length(x)==length(unique(x))){
	i <- i+1
	x <- TxtRaw[,i]
	if(!is.numeric(x)){
		i <- i+1
		x <- TxtRaw[,i]
	} # end if(is.numeric(x))
} # end while(!length(x)=length(unique(x))

Key <- TxtRaw[,i]

# separating numeric from character data
Data <- c()
AllStringData <- c()
for(i in c(1:length(Header))){
	if(is.numeric(TxtRaw[,i])){
		Data <- cbind(Data, TxtRaw[,i])
	}else{
		if(is.character(as.vector(TxtRaw[,i]))){
			AllStringData <- cbind(AllStringData, TxtRaw[,i])
		}else{
			print(paste('ReadTXT2DATA: Column ',i,' is neither of type numeric nor character'))
		} # end if(is.character(TxtRaw[,i]))
	} # end if(is.numeric(TxtRaw[,i]))
} # end for(i in length(Header))

# if names column known, choose names
if(NamesInd != -1){
	Names <- TxtRaw[,NamesInd]
}else{
	Names <- c()
} #end if(NamesInd != -1)

Description <- c()

# if DataDefined not given, produce some default
if(length(DataDefined == 0)){
	DataDefined <- rep(1, dim(Data)[2]) # one 1 for every column in Data
} # end if(length(DataDefined == 0)

# OUTPUT
setwd(old)
V <- list(Data, Header, Key, DataDefined, Names, Description, AllStringData)
return(V)

} # end function
