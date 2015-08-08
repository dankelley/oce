#' Define the header section of an ODF file.
#'  
#' Defines and returns the header section of an ODF file.
#' Output:
#'   ODF_HEADER is a cell array containing the definition.
#' 
#'   This tool usually is called by other tools such as ODF read,
#'   write and edit etc. It is one of the core tools and users 
#'   should not make any changes to it.
#'
#'   A new block can be added easily to this definition to extend an
#'   ODF header structure. The new block should be considered as
#'   'optional', otherwise all existing ODF files will have to be
#'   modified as well.
#'
#'   Report any bugs to DataServicesDonnees@@dfo-mpo.gc.ca
#' 
#' ODSToolbox Version: 2.0
#'
#' Last Updated: July 15, 2015
#' 
#' @export 
#' @examples
#' ODF_header <- define_ODF_header()
#' 
#' @details 
#' Source:
#'   Ocean Data and Information Services,
#'   Bedford Institute of Oceanography, DFO, Canada.
#'   DataServicesDonnees@@dfo-mpo.gc.ca
#' 
#' @author Yongcun Hu, Patrick Upson
#'
#' 
define_ODF_header <- function() {
	tmp = array("", c(2,4))
	tmp[1,] <- c('ODF_HEADER', 'char', 'mandatory', 'single')
	tmp[2,] <- c('FILE_SPECIFICATION', 'char', 'mandatory', 'single')
	ODF <- list(tmp)
	
	tmp = array("", c(10,4))
	tmp[1,] <- c('CRUISE_HEADER', 'char', 'mandatory', 'single')
	tmp[2,] <- c('COUNTRY_INSTITUTE_CODE', 'integer', 'mandatory', 'single')
	tmp[3,] <- c('CRUISE_NUMBER', 'char', 'mandatory', 'single')
	tmp[4,] <- c('ORGANIZATION', 'char', 'mandatory', 'single')
	tmp[5,] <- c('CHIEF_SCIENTIST', 'char', 'mandatory', 'single')
	tmp[6,] <- c('START_DATE', 'char', 'mandatory', 'single')
	tmp[7,] <- c('END_DATE', 'char', 'mandatory', 'single')
	tmp[8,] <- c('PLATFORM', 'char', 'mandatory', 'single')
	tmp[9,] <- c('CRUISE_NAME', 'char', 'mandatory', 'single')
	tmp[10,] <- c('CRUISE_DESCRIPTION', 'char', 'mandatory', 'single')
	ODF <- c(ODF, list(tmp))
	
	tmp = array("", c(19,4))
	tmp[1,] <- c('EVENT_HEADER', 'char', 'mandatory', 'single')
	tmp[2,] <- c('DATA_TYPE', 'char', 'mandatory', 'single')
	tmp[3,] <- c('EVENT_NUMBER', 'char', 'mandatory', 'single')
	tmp[4,] <- c('EVENT_QUALIFIER1', 'char', 'mandatory', 'single')
	tmp[5,] <- c('EVENT_QUALIFIER2', 'char', 'mandatory', 'single')
	tmp[6,] <- c('CREATION_DATE', 'char', 'mandatory', 'single')
	tmp[7,] <- c('ORIG_CREATION_DATE', 'char', 'mandatory', 'single')
	tmp[8,] <- c('START_DATE_TIME', 'char', 'mandatory', 'single')
	tmp[9,] <- c('END_DATE_TIME', 'char', 'mandatory', 'single')
	tmp[10,] <- c('INITIAL_LATITUDE', 'numeric', 'mandatory', 'single')
	tmp[11,] <- c('INITIAL_LONGITUDE', 'numeric', 'mandatory', 'single')
	tmp[12,] <- c('END_LATITUDE', 'numeric', 'mandatory', 'single')
	tmp[13,] <- c('END_LONGITUDE', 'numeric', 'mandatory', 'single')
	tmp[14,] <- c('MIN_DEPTH', 'numeric', 'mandatory', 'single')
	tmp[15,] <- c('MAX_DEPTH', 'numeric', 'mandatory', 'single')
	tmp[16,] <- c('SAMPLING_INTERVAL', 'numeric', 'mandatory', 'single')
	tmp[17,] <- c('SOUNDING', 'numeric', 'mandatory', 'single')
	tmp[18,] <- c('DEPTH_OFF_BOTTOM', 'numeric', 'mandatory', 'single')
	tmp[19,] <- c('EVENT_COMMENTS', 'char', 'mandatory', 'multiple')
	ODF <- c(ODF, list(tmp))

	tmp = array("", c(6,4))
	tmp[1,] <- c('POLYNOMIAL_CAL_HEADER', 'char', 'optional', 'multiple')
	tmp[2,] <- c('PARAMETER_NAME', 'char', 'mandatory', 'single')
	tmp[3,] <- c('CALIBRATION_DATE', 'char', 'mandatory', 'single')
	tmp[4,] <- c('APPLICATION_DATE', 'char', 'mandatory', 'single')
	tmp[5,] <- c('NUMBER_COEFFICIENTS', 'integer', 'mandatory', 'single')
	tmp[6,] <- c('COEFFICIENTS', 'numeric', 'mandatory', 'multiple')
	ODF <- c(ODF, list(tmp))
	
	tmp = array("", c(5,4))
	tmp[1,] <- c('INSTRUMENT_HEADER', 'char', 'mandatory', 'single')
	tmp[2,] <- c('INST_TYPE', 'char', 'mandatory', 'single')
	tmp[3,] <- c('MODEL', 'char', 'mandatory', 'single')
	tmp[4,] <- c('SERIAL_NUMBER', 'char', 'mandatory', 'single')
	tmp[5,] <- c('DESCRIPTION', 'char', 'mandatory', 'single')
	ODF <- c(ODF, list(tmp))
	
	tmp = array("", c(6,4))
	tmp[1,] <- c('COMPASS_CAL_HEADER', 'char', 'optional', 'multiple')
	tmp[2,] <- c('PARAMETER_NAME', 'char', 'mandatory', 'single')
	tmp[3,] <- c('CALIBRATION_DATE', 'char', 'mandatory', 'single')
	tmp[4,] <- c('APPLICATION_DATE', 'char', 'mandatory', 'single')
	tmp[5,] <- c('DIRECTIONS', 'numeric', 'mandatory', 'multiple')
	tmp[6,] <- c('CORRECTIONS', 'numeric', 'mandatory', 'multiple')
	ODF <- c(ODF, list(tmp))
	
	tmp = array("", c(3,4))
	tmp[1,] <- c('HISTORY_HEADER', 'char', 'optional', 'multiple')
	tmp[2,] <- c('CREATION_DATE', 'char', 'mandatory', 'single')
	tmp[3,] <- c('PROCESS', 'char', 'mandatory', 'multiple')
	ODF <- c(ODF, list(tmp))
	
	tmp = array("", c(16,4))
	tmp[1,] <- c('PARAMETER_HEADER', 'char', 'mandatory', 'multiple')
	tmp[2,] <- c('TYPE', 'char', 'mandatory', 'single')
	tmp[3,] <- c('NAME', 'char', 'optional', 'single')
	tmp[4,] <- c('UNITS', 'char', 'optional', 'single')
	tmp[5,] <- c('CODE', 'char', 'mandatory', 'single')
	tmp[6,] <- c('WMO_CODE', 'char', 'optional', 'single')
	tmp[7,] <- c('NULL_VALUE', 'char', 'optional', 'single')
	tmp[8,] <- c('PRINT_FIELD_WIDTH', 'integer', 'optional', 'single')
	tmp[9,] <- c('PRINT_DECIMAL_PLACES', 'integer', 'optional', 'single')
	tmp[10,] <- c('ANGLE_OF_SECTION', 'numeric', 'mandatory', 'single')
	tmp[11,] <- c('MAGNETIC_VARIATION', 'numeric', 'mandatory', 'single')
	tmp[12,] <- c('DEPTH', 'numeric', 'mandatory', 'single')
	tmp[13,] <- c('MINIMUM_VALUE', 'numeric', 'optional', 'single')
	tmp[14,] <- c('MAXIMUM_VALUE', 'numeric', 'optional', 'single')
	tmp[15,] <- c('NUMBER_VALID', 'integer', 'optional', 'single')
	tmp[16,] <- c('NUMBER_NULL', 'integer', 'optional', 'single')
	ODF <- c(ODF, list(tmp))
	
	tmp = array("", c(6,4))
	tmp[1,] <- c('RECORD_HEADER', 'char', 'mandatory', 'single')
	tmp[2,] <- c('NUM_CALIBRATION', 'integer', 'optional', 'single')
	tmp[3,] <- c('NUM_SWING', 'integer', 'optional', 'single')
	tmp[4,] <- c('NUM_HISTORY', 'integer', 'optional', 'single')
	tmp[5,] <- c('NUM_CYCLE', 'integer', 'optional', 'single')
	tmp[6,] <- c('NUM_PARAM', 'integer', 'optional', 'single')
	ODF <- c(ODF, list(tmp))
	
	names(ODF) <- c('ODF_HEADER','CRUISE_HEADER','EVENT_HEADER',
			'POLYNOMIAL_CAL_HEADER','INSTRUMENT_HEADER','COMPASS_CAL_HEADER',
			'HISTORY_HEADER','PARAMETER_HEADER','RECORD_HEADER')
#	if(!is.null(ODF)) {
#		save('definition_ODF_header.RData');
#	}
	ODF
}
# end of define_ODF_header.R
# ========================================================================