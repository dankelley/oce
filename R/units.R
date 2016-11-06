#' Convert a String to a Unit
#'
#' @details
#' This function is not presently used by any \code{oce} functions, and is
#' provided as a convenience function for users.
#'
#' @param u A character string indicating a variable name. The following
#' names are recognized: \code{"DBAR"},
#' \code{"IPTS-68"}, \code{"ITS-90"}, \code{"PSS-78"}, and \code{"UMOL/KG"}.
#' All other names yield a return value equal to the value of the
#' \code{default} argument.
#' @param default A default to be used for the return value, if \code{u}
#' is not a recognized string.
#' @return A list with elements \code{unit}, an \code{\link{expression}},
#' and \code{scale}, a string.
as.unit <- function(u, default=list(unit=expression(), scale=""))
{
    if (!is.character(u))
        return(default)
    if (length(grep("DBAR", u, ignore.case=TRUE, useBytes=TRUE))) {
        res <- list(unit=expression(dbar), scale="")
    } else if (length(grep("IPTS-68", u, ignore.case=TRUE, useBytes=TRUE))) {
        res <- list(unit=expression(degree*C), scale="IPTS-68")
    } else if (length(grep("ITS-90", u, ignore.case=TRUE, useBytes=TRUE))) {
        res <- list(unit=expression(degree*C), scale="ITS-90")
    } else if (length(grep("PSS-78", u, ignore.case=TRUE, useBytes=TRUE))) {
        res <- list(unit=expression(), scale="PSS-78")
    } else if (length(grep("UMOL/KG", u, ignore.case=TRUE, useBytes=TRUE))) {
        res <- list(unit=expression(mu*mol/kg), scale="")
    } else {
        res <- default
    }
    res
}
