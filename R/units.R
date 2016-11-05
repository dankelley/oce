#' Convert a String to a Unit
#'
#' @details
#' String matching is used to try to guess a unit from a character string.
#' @param u A character string.
#' @param default A default, if the string is not recognized.
#' @return A list with elements \code{unit}, an \code{\link{expression}}, and \code{scale}, a string.
#' @examples
#' as.unit("DBAR")
#' as.unit("IPTS-68")
#' as.unit("ITS-90")
#' as.unit("PSS-78")
#' as.unit("UMOL/KG")
as.unit <- function(u, default=list(unit=expression(), scale=""))
{
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
