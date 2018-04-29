#' @description Set data-quality flags within an oce object.
#'
#' This function changes specified entries in the object's data-quality
#' flags, leaving other entries unaltered. (If the object does not yet
#' have a flag set up to correspond with data of the indicated
#' \code{name}, then such a flag is set up, with values set to \code{default}.
#'
#' The choice of flag values is up to the user, but it may make sense to
#' stick to oceanographic conventions; see the \code{handleFlags} function
#' for the object class.
#'
#' @section Development note:
#' This function was added in late April, 2018, and is likely to undergo
#' changes until the end of June of that year.  Use with caution.
#'
#' @param object An oce object.
#' @param name Character string indicating the name of the variable to be flagged. If
#' this variable is not contained in the object's \code{data} slot, an error is reported.
#' @param value The value to be inserted in the flag.
#' @param default The default (good) value of the flag. This is used only if
#' the object does not yet have yet any entries for the \code{name} flag. In that case,
#' storage is set up for the flag, it is filled with the \code{default} value, and after
#' that the
#' @param i Integer index (required).
#' @param j Optional second integer index.
#' @param debug Integer set to 0 for quiet action or to 1 for some debugging.
#'
#' @return An object with flags set as indicated.
#'
#' @family functions relating to data-quality flags

