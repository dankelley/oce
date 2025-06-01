#' Determine oce Variable Names from an NERC/BODC Names
#'
#' Translate names in the NERC/BODC vocabulary to oce names, primarily so that
#' [read.netcdf()] can produce more easily interpreted results. Please note
#' that `bodcNames2oceNames()` handles only a tiny subset of the huge number of
#' names in the NERC/BODC vocabulary (see Reference 1). To see the names that
#' the function handles currently, type `bodcNames2oceNames` in an R session.
#'
#' @param bodcNames character vector that specifies variable names
#' that use the NERC/BODC convention.
#'
#' @param unduplicate logical value indicating whether to rename
#' repeated entries, so that e.g. if `"temperature` occurs
#' twice, the second instance will be changed to `"temperature2"`.
#' Set this to FALSE if you plan are calling  `bodcNames2oceNames`
#' in a renaming function of your own, and call [unduplicateNames()]
#' at the end of your function; see Example 2.
#'
#' @return
#'
#' `bodcNames2oceNames` returns a vector of the same length as its
#' first argument, with translations to oce names, as appropriate.
#' Note that the usual oce convention for handling duplicates is
#' used, with the first name that maps to temperature being set
#' to `temperature`, the next to `temperature2`, etc.
#'
#' @examples
#'
#' # Example 1: typical usage
#' bodcNames2oceNames(c("PSALST01", "TEMPP901", "PRESPR01"))
#'
#' # Example 2: extend to add new variables
#' BODC2 <- function(originalNames) {
#'     rval <- bodcNames2oceNames(originalNames, unduplicate = FALSE)
#'     rval[rval == "bowler hat"] <- "hat"
#'     rval[rval == "top hat"] <- "hat"
#'     unduplicateNames(rval)
#' }
#' BODC2(c("PSALST01", "TEMPP901", "PRESPR01", "bowler hat", "top hat"))
#'
#' @references
#' 1. The NERC Environmental Data Server.
#' <http://vocab.nerc.ac.uk/collection/P01/current/>
#'
#' @family functions that convert variable names to the oce convention
#'
#' @author Dan Kelley
bodcNames2oceNames <- function(bodcNames, unduplicate = TRUE) {
    rval <- bodcNames
    for (i in seq_along(bodcNames)) {
        if (bodcNames[i] %in% c("PRESPR01")) {
            rval[i] <- "pressure"
        } else if (bodcNames[i] %in% c("PSALST01", "PSLTZZ01")) {
            rval[i] <- "salinity"
        } else if (bodcNames[i] %in% c(
            "TEMPP681", "TEMPS601", "TEMPR601",
            "TEMPPR01"
        )) {
            rval[i] <- "temperature" # "degree C", "IPTS-68"
        } else if (bodcNames[i] %in% c("TEMPP901", "TEMPS901", "TEMPS902", "TEMPR901")) {
            rval[i] <- "temperature" # "degree C", "ITS-90"
        } else if (bodcNames[i] %in% c("DOXYZZ01")) {
            rval[i] <- "oxygen"
        } else if (bodcNames[i] %in% c("OXYOCPVL01")) {
            rval[i] <- "oxygenVoltage"
        } else if (bodcNames[i] %in% c("CPHLPR0", "CPHLPR01")) {
            rval[i] <- "chlorophyll-a"
        } else if (bodcNames[i] %in% c("ScanNumber")) {
            rval[i] <- "scan"
        } else if (bodcNames[i] %in% c("CNDCST01")) {
            rval[i] <- "conductivity"
        } else if (bodcNames[i] %in% c("IRRDUV01")) {
            rval[i] <- "downwellingIrradiance"
        } else if (bodcNames[i] %in% c("measurement_time")) {
            rval[i] <- "measurementTime"
        } else if (bodcNames[i] %in% c("AHSFZZ01")) {
            rval[i] <- "heightAboveBottom"
        } else if (bodcNames[i] %in% c("POTMCV01")) {
            rval[i] <- "theta" # unesco
        } else if (bodcNames[i] %in% c("SIGTEQ01")) {
            rval[i] <- "sigmaTheta" # unesco
        }
    }
    if (unduplicate) unduplicateNames(rval) else rval
}
