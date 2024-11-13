#' Sample ctd File in aml Format 1
#'
#' This file may be read with [read.ctd.aml()].  It is based
#' on a file donated by Ashley Stanek, which was shortened to
#' just 5 points for inclusion in oce, and which had some
#' identifying information (serial number, IP address, and WEP
#' code) redacted.
#'
#' @name ctd_aml_type1.csv.gz
#'
#' @docType data
#'
#' @encoding UTF-8
#'
#' @examples
#' ctd <- read.ctd.aml(system.file("extdata", "ctd_aml_type1.csv.gz", package = "oce"))
#'
#' @family raw datasets
#' @family things related to ctd data
NULL
#'
#' Sample ctd File in aml Format 3
#'
#' This file may be read with [read.ctd.aml()].  It is based
#' on a file created with Sailfish 1.4.8.0 software, as explained
#' in an oce issue at <https://github.com/dankelley/oce/issues/2247>.
#' Only the first 5 data points are provided here.
#'
#' @name ctd_aml_type3.csv.gz
#'
#' @docType data
#'
#' @encoding UTF-8
#'
#' @examples
#' ctd <- read.ctd.aml(system.file("extdata", "ctd_aml_type3.csv.gz", package = "oce"))
#'
#' @family raw datasets
#' @family things related to ctd data
NULL

#' Sample ctd File in .odf Format
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
#' ctd <- read.ctd(system.file("extdata", "CTD_BCD2014666_008_1_DN.ODF.gz", package = "oce"))
#' plot(ctd)
#'
#' @family raw datasets
#' @family things related to ctd data
#' @family things related to odf data
NULL


#' Sample adp File in RDI Format
#'
#' @name adp_rdi.000
#'
#' @docType data
#'
#' @examples
#' read.oce(system.file("extdata", "adp_rdi.000", package = "oce"))
#'
#' @family raw datasets
#' @family things related to adp data
NULL


#' Sample ctd File in .cnv Format
#'
#' @name ctd.cnv.gz
#'
#' @docType data
#'
#' @encoding UTF-8
#'
#' @examples
#' read.oce(system.file("extdata", "ctd.cnv.gz", package = "oce"))
#'
#' @family raw datasets
#' @family things related to ctd data
NULL


#' Sample ctd File in .ctd Format
#'
#' @name d200321-001.ctd.gz
#'
#' @docType data
#'
#' @encoding UTF-8
#'
#' @examples
#' read.oce(system.file("extdata", "d200321-001.ctd.gz", package = "oce"))
#'
#' @family raw datasets
#' @family things related to ctd data
NULL


#' Sample ctd File in .cnv Format
#'
#' @name d201211_0011.cnv.gz
#'
#' @docType data
#'
#' @encoding UTF-8
#'
#' @examples
#' read.oce(system.file("extdata", "d201211_0011.cnv.gz", package = "oce"))
#'
#' @family raw datasets
#' @family things related to ctd data
NULL

#' Sample xbt File in space-separated .edf Format
#'
#' @name xbt.edf
#'
#' @docType data
#'
#' @encoding UTF-8
#'
#' @examples
#' xbt <- read.xbt(system.file("extdata", "xbt.edf", package = "oce"))
#' summary(xbt)
#'
#' @family raw datasets
#' @family things related to xbt data
NULL

#' Sample xbt File in tab-separated .edf Format
#'
#' @name xbt2.edf
#'
#' @docType data
#'
#' @encoding UTF-8
#'
#' @examples
#' xbt2 <- read.xbt(system.file("extdata", "xbt2.edf", package = "oce"),
#'     type = "sippican2"
#' )
#' summary(xbt2)
#'
#' @family raw datasets
#' @family things related to xbt data
NULL
