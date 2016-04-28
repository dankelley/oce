#' @description
#' The \code{[[<-} method works for all \code{oce} objects, i.e.
#' objects inheriting from \code{\link{oce-class}}.  The purpose,
#' as with the related extraction method, \code{\link{[[}},
#' is to insulate users from the internal details of \code{oce}
#' objects, by looking for items within the various storage
#' slots of the object. Items not actually stored can also be 
#' replaced, including units and data-quality
#' flags.
#'
#' @details
#' As with \code{\link{[[}} method, the procedure
#' works in steps.
#'
#' First, the \code{metadata} slot of \code{x} is checked to
#' see whether it contains something named with \code{i}.
#' If so, then the named item is replaced with \code{value}.
#'
#' Otherwise, if the string value of \code{i} ends in \code{Unit}, then the
#' characters preceding that are taken as the name of a variable, and
#' the \code{metadata} slot of \code{x} is updated to store that unit, e.g.
#' \preformatted{x[["temperatureUnits"]] <- list(unit=expression(degree*F),scale="")}
#'
#' Similarly, if \code{i} ends in \code{Flag}, then quality-control
#' flags are set up as defined by \code{result}, e.g.
#' \preformatted{x[["temperatureFlags"]] <- c(2,4,2,2)}
#'
#' Otherwise, a partial string match is sought among the names of items
#' in the \code{data} slot of \code{x}. (This is done with \code{\link{pmatch}}.)
#' The first item found (if any) is then updated to hold the value \code{result}.
#'
#' If none of these conditions is met, a warning is issued.
#'
#' @param i The item to replace.
#' @param j Optional additional information on the \code{i} item.
#' @param ... Optional additional information (ignored).
#' @param value The value to be placed into \code{x}, somewhere.
#'
#' @family functions that replace parts of \code{oce} objects

