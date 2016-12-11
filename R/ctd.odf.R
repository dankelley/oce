## vim:textwidth=128:expandtab:shiftwidth=4:softtabstop=4

#' Read a CTD file in ODF format
#' @template readCtdTemplate
#'
#' @details
#' \code{read.ctd.odf} reads files stored in Ocean Data Format, used in
#' some Canadian hydrographic databases.
#'
#' @references
#' The ODF format, used by the Canadian Department of Fisheries and Oceans, is
#' described to some extent in the documentation for \code{\link{read.odf}}.  It
#' is not clear that ODF format is handled correctly in \code{read.ctd.odf}, or
#' the more general function \code{\link{read.odf}}, because the format 
#' varies between some sample files the author has encountered in his research.
read.ctd.odf <- function(file, columns=NULL, station=NULL, missingValue, monitor=FALSE,
                         debug=getOption("oceDebug"), processingLog, ...)
{
    oceDebug(debug, "read.ctd.odf(\"", file, "\", ...) {")
    if (!is.null(columns)) warning("'columns' is ignored by read.ctd.odf() at present")
    odf <- read.odf(file=file, columns=columns)
    res <- as.ctd(odf)
    ## replace any missingValue with NA
    if (!missing(missingValue) && !is.null(missingValue)) {
        for (item in names(res@data)) {
            res@data[[item]] <- ifelse(res@data[[item]]==missingValue, NA, res@data[[item]])
        }
    }
    if (!is.null(station))
        res@metadata$station <- station
    oceDebug(debug, "} # read.ctd.odf()")
    res
}
