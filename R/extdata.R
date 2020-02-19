#' Sample ctd dataset in odf format
#'
#' The location is approximately 30km southeast of Halifax Harbour,
#' at "Station 2" of the Halifax Line on the Scotian Shelf.
#'
#' @name CTD_BCD2014666_008_1_DN.ODF.gz
#'
#' @docType data
#'
#' @encoding UTF-8
#'
#' @examples
#' ctd <- read.ctd(system.file("extdata", "CTD_BCD2014666_008_1_DN.ODF.gz", package="oce"))
#' summary(ctd)
#' plot(ctd)
#'
#' @family raw datasets
#' @family things related to ctd data
#' @family things related to odf data
NULL


#' Sample RDI Teledyne adp dataset in format
#'
#' @name adp_rdi.000
#'
#' @docType data
#'
#' @encoding UTF-8
#'
#' @examples
#'\dontrun{
#' read.oce(system.file("extdata", "adp_rdi.000", package="oce"))
#'}
#'
#' @family raw datasets
#' @family things related to adp data
NULL


#' Sample ctd dataset in .cnv format
#'
#' @name ctd.cnv
#'
#' @docType data
#'
#' @encoding UTF-8
#'
#' @examples
#'\dontrun{
#' read.oce(system.file("extdata", "ctd.cnv", package="oce"))
#'}
#'
#' @family raw datasets
#' @family things related to ctd data
NULL


#' Sample ctd dataset in .ctd format
#'
#' @name d200321-001.ctd
#'
#' @docType data
#'
#' @encoding UTF-8
#'
#' @examples
#'\dontrun{
#' read.oce(system.file("extdata", "d200321-001.ctd", package="oce"))
#'}
#'
#' @family raw datasets
#' @family things related to ctd data
NULL


#' Sample ctd dataset in .cnv format
#'
#' @name d201211_0011.cnv
#'
#' @docType data
#'
#' @encoding UTF-8
#'
#' @examples
#'\dontrun{
#' read.oce(system.file("extdata", "d201211_0011.cnv", package="oce"))
#'}
#'
#' @family raw datasets
#' @family things related to ctd data
NULL



#' Sample met dataset (CSV format prior to October 2019)
#'
#' This is a subset of a file downloaded with [download.met()]
#' from the Environment Canada "Climate Data"
#' website \url{http://climate.weather.gc.ca/index_e.html}
#' sometime before October 13, 2019.
#'
#' @name test_met_csv1.csv
#'
#' @docType data
#'
#' @encoding UTF-8
#'
#' @examples
#'\dontrun{
#' read.met(system.file("extdata", "test_met_csv1.csv", package="oce"))
#'}
#'
#' @family raw datasets
#' @family things related to met data
NULL

#' Sample met dataset (CSV format as of October 2019)
#'
#' This is a subset of a file downloaded with [download.met()]
#' from the Environment Canada "Climate Data"
#' website \url{http://climate.weather.gc.ca/index_e.html}
#' on October 13, 2019.
#'
#' @name test_met_csv2.csv
#'
#' @docType data
#'
#' @encoding UTF-8
#'
#' @examples
#'\dontrun{
#' read.met(system.file("extdata", "test_met_csv2.csv", package="oce"))
#'}
#'
#' @family raw datasets
#' @family things related to met data
NULL

#' Sample met dataset (XML format as of October 2019)
#'
#' This is a subset of a file downloaded with [download.met()]
#' from the Environment Canada "Climate Data"
#' website \url{http://climate.weather.gc.ca/index_e.html}
#' on October 13, 2019.
#'
#' @name test_met_xml2.xml
#'
#' @docType data
#'
#' @encoding UTF-8
#'
#' @examples
#'\dontrun{
#' read.met(system.file("extdata", "test_met_xml2.xml", package="oce"))
#'}
#'
#' @family raw datasets
#' @family things related to met data
NULL

#' Sample xbt dataset
#'
#' @name xbt.edf
#'
#' @docType data
#'
#' @encoding UTF-8
#'
#' @examples
#'\dontrun{
#' xbt <- read.oce(system.file("extdata", "xbt.edf", package="oce"))
#'}
#'
#' @family raw datasets
#' @family things related to xbt data
NULL




