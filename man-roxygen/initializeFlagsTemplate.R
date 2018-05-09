#' @title Create storage for a flag, and initialize values, for a <%=class%> object
#'
#' @description
#' This function creates an item for a named variable within
#' the \code{flags} entry in the object's \code{metadata} slot.
#' The purpose is both to document a flag scheme
#' and to make it so that \code{\link{initializeFlags}}
#' and \code{\link{setFlags}} can specify flags by
#' name, in addition to number. A generic function, it is
#' specialized for some classes via interpretation of the
#' \code{scheme} argument (see \dQuote{Details}, for those
#' object classes that have such specializations).
#'
#' @details
#' If \code{object} already contains a \code{flags} entry with
#' the indicated name, then it is returned unaltered, and a warning
#' is issued.
#'
#' @section Caution:
#' This function was added in early May, 2018, and is likely to undergo
#' changes until the mid-summer of that year.  Use with caution.
#'
#' @param object An oce object.
#'
#' @param name Character value indicating the name of a variable within the
#' \code{data} slot of \code{object}.
#'
#' @param value Numerical or character value to be stored in the newly-created
#' entry within \code{flags}. (A character value will only work if
#' \code{\link{initializeFlags}} has been used first on \code{object}.)
#'
#' @param debug Integer set to 0 for quiet action or to 1 for some debugging.
#'
#' @return An object with the \code{flags} item within the
#' \code{metadata} slot set up as indicated.
#'
#' @family functions relating to data-quality flags

