# vim:textwidth=80:expandtab:shiftwidth=4:softtabstop=4

#' Convert a String to a Unit
#'
#' This converts strings to unit objects.  Only a few strings are recognized,
#' because most oce functions have specialized unit vocabularies and so
#' have little need of this function.
#'
#' @param u A character string indicating a unit. Case is ignored, so that e.g.
#' `"dbar"` and `"DBAR"` yield equal results.  The following are recognized:
#' c(`"m-1"`, `"dbar"`, `"decibar"`, `"degree"`, `"degree_Celcius"`,
#' `"degree_north"`, `"degree_east"`, `"ipts-68"`, 
#' `"its-90"`, `"m/s^1"`, `"m/s^2"`, `"pss-78"`, 
#' `"umol/kg"`, `"micromole/kg"`)
#'
#' @param default A default to be used for the return value, if `u`
#' is not a recognized string.
#'
#' @return A list with elements `unit`, an [expression()],
#' and `scale`, a string.
#'
#' @examples
#' as.unit("DBAR")
#' as.unit("IPTS-68")
#' as.unit("ITS-90")
#' as.unit("PSS-78")
#' as.unit("UMOL/KG")
as.unit <- function(u, default=list(unit=expression(), scale=""))
{
    if (missing(u) || !is.character(u))
        return(default)
    u <- tolower(u)
    if (length(grep("m-1", u))) {
        res <- list(unit=expression(1/m), scale="")
    } else if (length(grep("dbar", u))) {
        res <- list(unit=expression(dbar), scale="")
    } else if (length(grep("decibar", u))) {
        res <- list(unit=expression(dbar), scale="")
    } else if (length(grep("^degree$", u))) {
        res <- list(unit=expression(degree), scale="")
    } else if (length(grep("degree_celcius", u))) {
        res <- list(unit=expression(degree*C), scale="")
    } else if (length(grep("degree_north", u))) {
        res <- list(unit=expression(degree*N), scale="")
    } else if (length(grep("degree_east", u))) {
        res <- list(unit=expression(degree*E), scale="")
    } else if (length(grep("ipts-68", u))) {
        res <- list(unit=expression(degree*C), scale="IPTS-68")
    } else if (length(grep("its-90", u))) {
        res <- list(unit=expression(degree*C), scale="ITS-90")
    } else if (length(grep("m/s^1", u))) {
        res <- list(unit=expression(m/s^1), scale="")
    } else if (length(grep("m/s^2", u))) {
        res <- list(unit=expression(m/s^2), scale="")
    } else if (length(grep("pss-78", u))) {
        res <- list(unit=expression(), scale="PSS-78")
    } else if (length(grep("umol/kg", u))) {
        res <- list(unit=expression(mu*mol/kg), scale="")
    } else if (length(grep("micromole/kg", u))) {
        res <- list(unit=expression(mu*mol/kg), scale="")
    } else {
        res <- default
    }
    res
}
