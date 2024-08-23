# vim:textwidth=80:expandtab:shiftwidth=4:softtabstop=4

#' Convert a String to a Unit
#'
#' This converts strings to unit objects.  Only a few strings are recognized,
#' because many oce functions have specialized unit vocabularies and so
#' have little need of `as.unit()`.
#'
#' @param u A character string indicating a unit. Case is ignored, so that e.g.
#' `"dbar"` and `"DBAR"` yield equal results.  The following are recognized:
#' c(`"m-1"`, `"dbar"`, `"decibar"`, `"degree"`, `"degree_Celsius"`,
#' `"degree celsius"`,
#' `"degree_north"`, `"degree_east"`, `"ipts-68"`,
#' `"its-90"`,
#' `"kg/m^3"`,
#' `"m/s^1"`, `"m/s^2"`, `"ml/L"`, `"pss-78"`,
#  `"uEinsteins/s/m^2"`,
#' `"umol/kg"`, `"micromole/kg"`, `"S/m"`, `"volts"`)
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
#'
#' @author Dan Kelley
as.unit <- function(u, default = list(unit = expression(), scale = "")) {
    if (missing(u) || !is.character(u)) {
        return(default)
    }
    uorig <- u # so e.g. S/m can be distinguished from 1/speed
    u <- tolower(u)
    if (grepl("^1$", u)) {
        list(unit = expression(), scale = "")
    } else if (grepl("m-1", u)) {
        list(unit = expression(1 / m), scale = "")
    } else if (grepl("dbar", u)) {
        list(unit = expression(dbar), scale = "")
    } else if (grepl("decibar", u)) {
        list(unit = expression(dbar), scale = "")
    } else if (grepl("^degree$", u)) {
        list(unit = expression(degree), scale = "")
    } else if (grepl("degree[s]{0,1}.celsius", u)) {
        list(unit = expression(degree * C), scale = "")
    } else if (grepl("degrees{0,1}.east", u)) {
        list(unit = expression(degree * E), scale = "")
    } else if (grepl("degrees{0,1}.north", u)) {
        list(unit = expression(degree * N), scale = "")
    } else if (grepl("degrees{0,1}.south", u)) {
        list(unit = expression(degree * S), scale = "")
    } else if (grepl("degrees{0,1}.west", u)) {
        list(unit = expression(degree * W), scale = "")
    } else if (grepl("ipts-68", u)) {
        list(unit = expression(degree * C), scale = "IPTS-68")
    } else if (grepl("its-90", u)) {
        list(unit = expression(degree * C), scale = "ITS-90")
    } else if (grepl("kg/m\\^3", u)) {
        list(unit = expression(kg/m^3), scale = "")
    } else if (grepl("^m$", u)) {
        list(unit = expression(m), scale = "")
    } else if (grepl("m/s^1", u)) {
        list(unit = expression(m / s^1), scale = "")
    } else if (grepl("m/s^2", u)) {
        list(unit = expression(m / s^2), scale = "")
    } else if (grepl("ml/l", u)) {
        list(unit = expression(ml / l), scale = "")
    } else if (grepl("mg/m\\^3", u)) {
        list(unit = expression(mg/m^3), scale = "")
    } else if (grepl("pss-78", u)) {
        list(unit = expression(), scale = "PSS-78")
    } else if (grepl("umol/kg", u)) {
        list(unit = expression(mu * mol / kg), scale = "")
    } else if (grepl("uEinsteins/s/m\\^2", uorig)) {
    #} else if (grepl("uUinsteins/s/m^2", uorig)) {
        list(unit = expression(mu * Einstein/s/kg), scale = "")
    } else if (grepl("micromole/kg", u)) {
        list(unit = expression(mu * mol / kg), scale = "")
    } else if (grepl("S/m", uorig)) {
        list(unit = expression(S/m), scale = "")
    } else if (grepl("seconds since", uorig)) {
        list(unit = expression(s), scale = "")
    } else if (grepl("volts", u)) {
        list(unit = expression(V), scale = "")
    } else {
        default
    }
}
