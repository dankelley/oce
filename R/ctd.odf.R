## vim:textwidth=128:expandtab:shiftwidth=4:softtabstop=4

#' Read a CTD file in ODF format
#'
#' @template readCtdTemplate
#'
#' @param exclude either a character value holding a regular
#' expression that is used with [grep()] to remove lines from the
#' header before processing, or `NULL` (the default), meaning
#' not to exclude any such lines.  The purpose of this argument
#' is to solve problems with some files, which can have
#' thousands of lines that indicate details that are may be of
#' little value in processing.  For example, some files have thousands
#' of lines that would be excluded by using
#' `exclude="PROCESS='Nulled the .* value"` in the function call.
##'
#' @author Dan Kelley
#'
#' @details
#' `read.ctd.odf` reads files stored in Ocean Data Format, used in
#' some Canadian hydrographic databases.
#'
#' @references
#' The ODF format, used by the Canadian Department of Fisheries and Oceans, is
#' described to some extent in the documentation for [read.odf()].  It
#' is not clear that ODF format is handled correctly in `read.ctd.odf`, or
#' the more general function [read.odf()], because the format
#' varies between some sample files the author has encountered in his research.
#'
#' @family things related to ctd data
#' @family things related to odf data
#' @family functions that read ctd data
read.ctd.odf <- function(file, columns=NULL, station=NULL, missingValue, deploymentType="unknown",
                         monitor=FALSE, exclude=NULL, debug=getOption("oceDebug"), processingLog, ...)
{
    oceDebug(debug, "read.ctd.odf(\"", file, "\", ...) {\n", sep="", unindent=1, style="bold")
    if (!is.null(columns)) warning("'columns' is ignored by read.ctd.odf() at present")
    odf <- read.odf(file=file, columns=columns, exclude=exclude, debug=debug-1)
    res <- as.ctd(odf, debug=debug-1)
    ## replace any missingValue with NA
    if (!missing(missingValue) && !is.null(missingValue)) {
        for (item in names(res@data)) {
            res@data[[item]] <- ifelse(res@data[[item]]==missingValue, NA, res@data[[item]])
        }
    }
    if (!is.null(station))
        res@metadata$station <- station
    for (mname in names(odf@metadata))
        res@metadata[[mname]] <- odf@metadata[[mname]]
    res@metadata$pressureType <- "sea"
    res@metadata$deploymentType <- deploymentType
    oceDebug(debug, "} # read.ctd.odf()\n", unindent=1)
    res
}
