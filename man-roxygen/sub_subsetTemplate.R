#' @description
#' The \code{[[<-} method works for all [oce-class] objects.
#' The purpose, as with the related extraction method, \code{[[},
#' is to insulate users from the internal details of [oce-class]
#' objects, by looking for items within the various storage
#' slots of the object. Items not actually stored can also be
#' replaced, including units and data-quality
#' flags.
#'
#' @details
#' As with \code{[[} method, the procedure works in steps.
#'
#' First, the `metadata` slot of `x` is checked to
#' see whether it contains something named with `i`.
#' If so, then the named item is replaced with `value`.
#'
#' Otherwise, if the string value of `i` ends in `Unit`, then the
#' characters preceding that are taken as the name of a variable, and
#' the `metadata` slot of `x` is updated to store that unit, e.g.
#'```
#' x[["temperatureUnits"]] <- list(unit=expression(degree*F),scale="")
#'```
#'
#' Similarly, if `i` ends in `Flag`, then quality-control
#' flags are set up as defined by `result`, e.g.
#' ```
#' o[["temperatureFlags"]] <- c(2,4,2,2)
#' ```
#' Otherwise, [pmatch()] is used for a partial-string match with
#' the names of the items that are in the `data` slot of `x`.
#' The first item found (if any) is then updated to hold the value `result`.
#'
#' If none of these conditions is met, a warning is issued.
#'
#' @param i character value naming the item to replace.
#'
#' @param j optional additional information on the `i` item.
#'
#' @param ... optional additional information (ignored).
#'
#' @param value The value to be placed into `x`, somewhere.
#'
#' @family functions that replace parts of oce objects

