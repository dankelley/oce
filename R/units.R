# vim:textwidth=80:expandtab:shiftwidth=4:softtabstop=4

# NOTE TO DEVELOPERS: if you add a new conversion here, add a test
# to ../tests/testthat/test_units.R and run the tests in the file
# to be sure your work is OK.  Please follow the pattern you see
# here, with the use of regular expressions.  Failure to do so
# can lead to odd errors for some users -- see e.g.
# https://github.com/dankelley/oce/issues/2240, an odd error
# that led to rewriting some things you see here.

#' Convert a String to a Unit
#'
#' This converts strings to unit objects.  It is designed mainly for
#' use within various functions in the package, not for the end user.
#' Therefore, the documentation does not give a full listing; for that,
#' developers should examine the `tests/test_that/test_units.R` file.
#' Developers who wish to add new entries are asked to follow
#' the conventions in this file, with regard to regular expressions,
#' spaces between tokens, etc., and also to add tests for whatever
#' they add.
#'
#' @param u a character string indicating a unit. Case is ignored, so that e.g.
#' `"dbar"` and `"DBAR"` yield equal results.  Many common notations are
#' recognized, e.g. `kg/m^3` and `kg m-3` for density, etc.
#'
#' @param default a default to be used for the return value, if `u`
#' is not a recognized string.  Setting this to NULL is a good way to
#' discover whether a given value of `u` is recognized as a unit
#' by this function, as opposed to something (like a conductivity
#' ratio) that simply has no unit.
#'
#' @return if `as.unit` recognizes `u` as unit, then it returns a list
#' with elements `unit`, which is an [expression()], and `scale`,
#' which is a character value.  That is also the case if it does
#' not recognize `u`, and if `default` is not specified.  However,
#' if `u` is not recognized, and if `default` is provided by the user,
#' then `as.unit` returns the provided value of `default`.
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
    if (missing(u)) {
        return(default)
    }
    if (!is.character(u)) {
        stop("u, if provided, must be a character value")
    }
    uorig <- trimws(u) # keep this to e.g. distinguish S/m from s/m.
    u <- tolower(uorig)
    if (grepl("^1$", u)) {
        list(unit = expression(), scale = "")
    } else if (grepl("^1\\s*/\\s*m$", u) || grepl("^m-1$", u)) {
        list(unit = expression(1 / m), scale = "")
    } else if (grepl("^db$", u)) {
        list(unit = expression(dbar), scale = "")
    } else if (grepl("^dbar[s]{0,1}$", u)) {
        list(unit = expression(dbar), scale = "")
    } else if (grepl("^decibar[s]{0,1}$", u)) {
        list(unit = expression(dbar), scale = "")
    } else if (grepl("^degree[s]{0,1}$", u)) {
        list(unit = expression(degree), scale = "")
    } else if (grepl("^degree[s]{0,1}.celsius$", u)) {
        list(unit = expression(degree * C), scale = "")
    } else if (grepl("^degree[s]{0,1}.c$", u)) {
        list(unit = expression(degree * C), scale = "")
    } else if (grepl("^degrees{0,1}.east$", u)) {
        list(unit = expression(degree * E), scale = "")
    } else if (grepl("^degrees{0,1}.north$", u)) {
        list(unit = expression(degree * N), scale = "")
    } else if (grepl("^degrees{0,1}.south$", u)) {
        list(unit = expression(degree * S), scale = "")
    } else if (grepl("^degrees{0,1}.west$", u)) {
        list(unit = expression(degree * W), scale = "")
    } else if (grepl("^ipts-68$", u) || grepl("^ipts\\s*68$", u)) {
        list(unit = expression(degree * C), scale = "IPTS-68")
    } else if (grepl("^its-90$", u) || grepl("^its\\s*90$", u)) {
        list(unit = expression(degree * C), scale = "ITS-90")
    } else if (grepl("^kg\\s*/\\s*m\\^3$", u) || grepl("^kg\\s+m-3", u)) {
        list(unit = expression(kg / m^3), scale = "")
    } else if (grepl("^m$", u)) {
        list(unit = expression(m), scale = "")
    } else if (grepl("^m\\s*/\\s*s$", u) || grepl("^m\\s+s-1$", u)) {
        list(unit = expression(m / s), scale = "")
    } else if (grepl("^m\\s*/\\s*s\\^2$", u) || grepl("^m\\s+s-2$", u)) {
        list(unit = expression(m / s^2), scale = "")
    } else if (grepl("^ml\\s*/\\s*l$", u) || grepl("^ml\\s+l-1$", u)) {
        list(unit = expression(ml / l), scale = "")
    } else if (grepl("^ml\\s*/\\s*L$", u) || grepl("^ml\\s+L-1$", u)) {
        list(unit = expression(ml / l), scale = "")
    } else if (grepl("^mg\\s*/\\s*m\\^3$", u) || grepl("^mg\\s+m-3$", u)) {
        list(unit = expression(mg / m^3), scale = "")
    } else if (grepl("^pss-78$", u) || grepl("^pss\\s*78$", u)) {
        list(unit = expression(), scale = "PSS-78")
    } else if (grepl("^umol\\s*/\\s*kg$", u) || grepl("^umol\\s+kg-1$", u)) {
        list(unit = expression(mu * mol / kg), scale = "")
    } else if (grepl("^micromole\\s*/\\s*kg$", u) || grepl("^micromole\\s+kg-1$", u)) {
        list(unit = expression(mu * mol / kg), scale = "")
    } else if (grepl("^umol\\s*/\\s*l$", u) || grepl("^umol\\s+l-1$", u)) {
        list(unit = expression(mu * mol / l), scale = "")
    } else if (grepl("^micromole\\s*/\\s*l$", u) || grepl("^micromole\\s+l-1$", u)) {
        list(unit = expression(mu * mol / l), scale = "")
    } else if (grepl("^uEinsteins\\s*/\\s*s\\s*/\\s*m\\^2$", uorig) || grepl("^uEinsteins\\s+s-1\\s+m-2$", uorig)) {
        list(unit = expression(mu * Einstein / s / m^2), scale = "")
        # reciprocal velocity
    } else if (grepl("^s\\s*/\\s*m$", uorig) || grepl("^s\\s+m-1$", uorig)) {
        list(unit = expression(s / m), scale = "")
        # conductivity
    } else if (grepl("^S\\s*/\\s*m$", uorig) || grepl("^S\\s+m-1$", uorig)) {
        list(unit = expression(S / m), scale = "")
    } else if (grepl("^mS\\s*/\\s*cm$", uorig) || grepl("^mS\\s+cm-1$", uorig)) {
        list(unit = expression(mS / cm), scale = "")
    } else if (grepl("^uS\\s*/\\s*cm$", uorig) || grepl("^uS\\s+cm-1$", uorig)) {
        list(unit = expression(mu * S / cm), scale = "")
    } else if (grepl("^seconds\\s+since$", uorig)) {
        list(unit = expression(s), scale = "")
    } else if (grepl("^ug\\s*/\\s*l$", u)) {
        list(unit = expression(mu * g / l), scale = "")
    } else if (grepl("^ug\\s*/\\s*kg$", u)) {
        list(unit = expression(mu * g / kg), scale = "")
    } else if (grepl("^volt[s]{0,1}$", u)) {
        list(unit = expression(V), scale = "")
    } else {
        if (identical(default, "copy")) {
            list(unit = as.expression(bquote(.(uorig))), scale = "")
        } else {
            default
        }
    }
}
