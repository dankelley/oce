#' READ_ODF: Read in an ODF file.
#' 
#' Description:
#'   Read in an ODF file.
#' 
#' @param filename location and name of the ODF file to be processed
#'
#' @export
#' 
#' @details
#' ODSToolbox Version: 2.0
#'
#' Last Updated: July 6, 2015.
#'
#' Source:
#'   Ocean Data and Information Services,
#'   Bedford Institute of Oceanography, DFO, Canada.
#'   DataServicesDonnees@@dfo-mpo.gc.ca
#'
#' Notes:
#'   This program was totally re-designed and re-written for
#'   ODSToolbox Version 2.0. It is not based on Version 1.0.
#'
#'   While this new version of read_odf corrects many errors in
#'   Version 1.0, and includes some new functionalities such as
#'   checking if all mandatory header blocks and all mandatory
#'   fields are presented in the input ODF file etc., it is possible
#'   that this program may have some conflicts with other tools in
#'   ODSToolbox, please email yongcun.hu@@dfo-mpo.gc.ca if the
#'   user find any problems.
#'
#' See also \code{\link{write_odf}}.
#'
#' Copyright 2006, DFO, Bedford Institute of Oceanography, Canada.
#' All Rights Reserved.
#' 
#' @author Yongcun Hu, Patrick Upson
#' 
#' Modified by Gordana Lazin, June 8, 2015
#' Import data using read.table function (line 190), much faster 
#' Replace formats for SYTM_01: froim strings to ISO time format (asumes UTC), line 231
#'
read_odf <- function(filename) {

  require(data.table) #R package
  
	# IMPORT: following 6 lines define some strings according to ODF file
	#         definition (see the last sub-function in this file), if these
	#         strings are changed in that definition, they must be changed
	#         here accordingly.
	DATA_LINE = '-- DATA --'          # starting line of data section
	
	SYTM = 'SYTM'
	POLYNOMIAL_CAL_HEADER = 'POLYNOMIAL_CAL_HEADER'
	COMPASS_CAL_HEADER = 'COMPASS_CAL_HEADER'

	INTEGER = 'integer'
	NUMERIC = 'numeric'

	if( !file.exists(filename) ) {
		stop("File does not exist")
	}
	
	# read the input ODF File
	F <- readFile(filename)

	# check if input ODF file is an empty file
	if(length(filename) <= 0) {
		stop("File contains no data")
	}
	
	# check if input ODF file has one and only one such line '-- DATA --'
	dataLineArray <- grepl(DATA_LINE, F);
	data_lines_index <- which(dataLineArray==TRUE)
#	print(data_lines_index)
	if(length(data_lines_index) > 0) {
		#if one or more data lines is found use the last index as the data to be read
		data_line_index <- data_lines_index[length(data_lines_index)]
	} else {
		#if no data line is found then use one line pas the end of the number of lines in the header
		data_line_index <- length(dataLineArray) + 1
	}

	if(length(data_lines_index) <= 0 ) {
		stop(
			' \n', 
			paste(' -- The input ODF file "', filename, '" does NOT have a\n'),
			'    separated beginning line for data section such as:\n',
			paste('        ', DATA_LINE, '\n'),
			'    By definition, every ODF file must have one such line.\n',
			' \n'
		)
	}  else if(length(data_lines_index) > 1) {
		stop(
			' \n',
			paste(' -- The input ODF file "', filename, '" has more than one\n'),
			'    separated beginning lines for data section such as:\n',
			paste('        ', DATA_LINE, '\n'),
			'    By definition, only one such line is allowed to separate\n',
			'    header blocks and data section.\n',
			' \n'
		)
		
	}

	# get the ODF header information
	odf_header <- define_ODF_header()
	
	curParmeterName = NULL
	curParameter  = NULL
	headObject = NULL
	
	#Create the ODF structure to be returned
	S <- list()
	
	#Scan through the header lines, we know where the index line is because 
	#it was found in the above section.
	for( idxLine in 1:(data_lines_index+1) ) {
		#Remove the last character of each line in the header (commas except  
	  #for the last line).
		line <- gsub(",$", "", F[idxLine])
		
		#Remove leading and trailing whitespace
		line <- gsub("^\\s+|\\s+$", "", line)
		
		#Test to see if the current line is a header object; if it exists in 
		#the list of ODF_Header names then create a list for the variables to 
		#follow and add them to the structrue to be returned.
		headIndex <- grep(line, names(odf_header), fixed=TRUE)
		if( length(headIndex) > 0 || idxLine >= data_lines_index) {
			headObject <- odf_header[headIndex]
			if( !is.null(curParmeterName) ) {
				#set the accumulated field names
				#names(curParameter) <- curParameterAttr
				S <- addToPram(S, curParmeterName, curParameter)
			}
			#Clear the curList and set the new parameter header value.
			curParmeterName <- line
			curParameter <- NULL
		} else {
			
			val <- extract_val(line)

			#Find the parameter from the header definition
			pramIndex <- grep(paste("^",val[1],"$",sep=""), headObject[[curParmeterName]][,1])
			headPram <- headObject[[curParmeterName]][pramIndex,]
			
#			print(paste("pram:",val[1]))
#			print(headPram)
		
			if( is.null(curParameter)) {
				curParameter <- list()
			}

			#Create or add values to the parameter currently being handled.
			#Index 4 in the parameter array is the parameter type.
			if(!is.null(curParameter$TYPE) && curParameter$TYPE == SYTM) {
				convertedVal <- tryCatch({
						as.integer(val[2])
					}, warning = function(war) {
						val[2]
					})
				curParameter <- addToPram(curParameter, val[1], convertedVal)
			} else if( headPram[2] == INTEGER ) {
				curParameter <- addToPram(curParameter, val[1], as.integer(val[2]))
#				print(paste(val[1], typeof(curParameter[[val[1]]])))
			} else if( headPram[2] == NUMERIC ) {
				if(curParmeterName == POLYNOMIAL_CAL_HEADER ||
					curParmeterName == COMPASS_CAL_HEADER ) {
					tmpVals <- gsub("\\s+|\\t+", ",", val[2])
					tmpVals <- strsplit(tmpVals, ",")[[1]]
					for( i in 1:length(tmpVals) ) {
						curParameter <- addToPram(curParameter, val[1], as.numeric(tmpVals[i]))
					}
				} else {
					curParameter <- addToPram(curParameter, val[1], as.numeric(val[2]))
				}
#				print(typeof(curParameter[[val[1]]]))
			} else {
				curParameter <- addToPram(curParameter, val[1], val[2])
			}
		}
	}

	
  # Read the data but skip the header
  S$DATA = read.table( filename, skip=data_lines_index, as.is=T, stringsAsFactors=F)
	
	# retrieve the parameter names and cast the columns into their proper types
	# this all assumes a perfect case that the PARAMETER_HEADER has been set correctly
	# and all values in the columns are of the proper type
	pram_names = NULL
	for( i in 1:length(S$PARAMETER_HEADER) ){
		code = length(S$PARAMETER_HEADER[[i]][['CODE']]) > 0
		wmo_code = length(S$PARAMETER_HEADER[[i]][['WMO_CODE']]) > 0
		name = length(S$PARAMETER_HEADER[[i]][['NAME']]) > 0
		
		if( code ) {
			pram_names <- c(pram_names, S$PARAMETER_HEADER[[i]][['CODE']])
		} else if(wmo_code) {
			pram_names <- c(pram_names, S$PARAMETER_HEADER[[i]][['WMO_CODE']])
		} else if( name ) {
			warning("\n",
					"The file contains no 'CODE' fields in the parameter list.\n",
					"\n")
			pram_names <- c(pram_names, S$PARAMETER_HEADER[[i]][['NAME']])
			
		}
#		if(S$PARAMETER_HEADER[[i]]$TYPE == 'DOUB' || S$PARAMETER_HEADER[[i]]$TYPE == 'SING') {
#			print("Double column")
#			#S$DATA[,i] <- as.numeric(S$DATA[,i])
#		} else if(S$PARAMETER_HEADER[[i]]$TYPE == 'INTE') {
#			print("Integer column")
#			S$DATA[,i] <- as.integer(S$DATA[,i])
#		}
	}
	
	#set the names for the columns in the matrix
	colnames(S$DATA) <- pram_names

	#add the input file to the structure for user convenience 
	S$INPUT_FILE = filename

  #Replace formats for SYTM_01: from strings to ISO time format (asumes UTC)
if(length(grep("SYTM_01",pram_names)) > 0) {
  S$DATA$SYTM_01=as.POSIXct(S$DATA$SYTM_01,format="%d-%b-%Y %H:%M:%S",tz='UTC')
}

	#return the resulting data structure with the header parameters and the data object
	S
}

#'
#' addToPram
#' 
#' Description:
#' 	Used to create and add to a list, parameters using the same name are added
#' to a list of other parameters using the same name. The list is then returned.
#' 
#' @param pram - Null or the existing list of parameters
#' @param name - the name of the sub-parameter list to add the value to
#' @param val - the value to add the the parameter ist.
#'
addToPram <- function(pram, name, val) {
	pramIdx <- grep(name, names(pram), fixed=TRUE)
	if( length(pramIdx) <= 0 ) {
		#if the structrue for the current field doesn't already exists
		#then create it and add the current list to it
		pram[[name]] <- val
	} else {
		#if the parameter exists already, but it's a list of parameters
		#rather than a list of lists of parameters. In other words if it has
		#top level names instead of just being a list then move the
		#top level parameters down one level in the list and start adding
		#lists of parameters to the parameter
		#
		#so this is what it looks like with top level parameters
		# $PARAMETER_HEADER$TYPE
		# $PARAMETER_HEADER$NAME
		# $PARAMETER_HEADER$UNITS
		# $PARAMETER_HEADER$CODE
		# ...
		#
		# this is what it should look like
		# $PARAMETER_HEADER[[1]]
		# $PARAMETER_HEADER[[1]]$TYPE
		# $PARAMETER_HEADER[[1]]$NAME
		# $PARAMETER_HEADER[[1]]$UNIT
		# $PARAMETER_HEADER[[1]]$CODE
		# ...
		if( length(names(pram[[name]])) > 0 ) { # typeof(pram[[name]]) != 'list'){
			#if the structure does already exist, but exists as a single parameter
			#then we have to make room for additional parameters under it
			tmp <- pram[[name]]
			tmpNames <- names(pram[[name]])
			names(tmp) <- tmpNames
			
			pram[[name]] <- list()
			pram[[name]][[1]] <- tmp
			pram[[name]][[2]] <- val
		} else {
			#print(paste(name, "you are here"))
			#if the structure for the current field already exists and is already
			#capable of holding additional parameters then just tack the new one
			#on to the end of the parameter list.
			pram[[name]][[length(pram[[name]])+1]] <- val
		}
	}
	
	pram
}
#'parameterReshape
#' 
#' Description:
#' 	Used to convert lists of lists into matrices inorder to present
#' 	the data in a more compact easier to read, and access, way.
#' 
#' @param paramList The parameter header to be reshaped
#'
parameterReshape <- function(paramList) {
	nameArray = NULL
	param = NULL
	
	#if the parameter list has more than one element, but has no names
	#then it's a list of lists
	if( length(names(paramList) ) <= 0 ) {
		for( i in 1:length(paramList) ){
			nameArray <- c(nameArray, names(paramList[[i]]))
		}
		nameArray <- unique(nameArray)
		
		param = data.frame(matrix(1, nrow=length(paramList), ncol=length(nameArray)))
		
		for( i in 1:length(paramList) ) {
			for( j in 1:length(nameArray)) {
				param[i,j] <- paramList[[i]][[nameArray[j]]]
			}
		}
	} else {
		nameArray = names(paramList)
		param = data.frame(matrix(1, nrow=1, ncol=length(nameArray)))
		for( j in 1:length(nameArray)) {
			param[j] <- paramList[[nameArray[j]]]
		}
	}
	
	names(param) <- nameArray
	
	param
}

#' readFile
#' 
#' Description:
#'   read the data from a file and return an array of lines
#' 
#' @param filename input ODF file name
#'
#' @details
#' Output : OUTPUT = array of file data
# ========================================================================
# Sub-function : readFile
#      Purpose : read the data from a file and return an array of lines
#        Input : filename -- input ODF file name
#       Output : OUTPUT = array of file data
# ------------------------------------------------------------------------
readFile <- function(filename) {
	conn=file(filename,open="r")
	line=readLines(conn)
	close(conn)
	
	line
}
#'
#' extract_val
#' 
#' Description:
#' 	Used to split a line into a key and value pair. Leading and trailing spaces
#' are removed and 00000D+00 strings are replaced with 00000E+00 strings.
#' 
#' @param line The line to be split up at an '=' symbol. The left side of the
#' 		symbol becomes the key, the right side becomes the value
#' 
# ========================================================================
# Sub-function : EXTRACT_VAL
#      Purpose : Extract field value from input string expression
#        Input : LINE -- input string expression such as Variable='Value'
#       Output : VAL -- extracted value of input string expression
#      Example : If LINE is CRUISE_NUMBER='NED2009002',
#              : then VAL is NED2009002
# ------------------------------------------------------------------------
extract_val <- function(line) {
	
	#create a character array used for substrings and indexing
	charArray <- strsplit(line, "")[[1]]
	equIndex <- grep("=", charArray)[1]
	
	#split the filed up into the field name and it's value
	val <- c(substr(line, 1, equIndex-1), substr(line, equIndex+1, length(charArray)))
	
	#remove leading and trailing whitespaces from both the field name and field value
	val <- gsub("^\\s+", "", val)
	
	#remove leading and trailing single quotes from both the field name and field value
	val <- gsub("^'|'$", "", val)

	#remove leading and trailing whitespaces from both the field name and field value
	val <- gsub("\\s+$", "", val)
	
	val <- convert_number(val)
	
	val
}

#'
#' convert_number
#' 
#' Description:
#' 	convert 00000D+00 strings to 00000E+00 strings.
#' 
#' @param sVal - a string or array of strings
#' 
convert_number <- function(sVal) {
	val <- sVal
	
	for( i in 1:length(val)) {
		if( grepl("([0-9]+D(\\+|-)[0-9][0-9])", val[i]) ) {
			#in older files numeric notation is sometimes 0.0000000D+00 for base 10 exponents
			#Replce the D wtih E so it can be processed by modern string to numeric functions
			val[i] <- gsub("D\\+", "E+", val[i])
			val[i] <- gsub("D-", "E-", val[i])
		}
	}
	val
}