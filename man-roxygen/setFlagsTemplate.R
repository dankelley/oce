#' @title Set data-quality flags within a <%=class%> object
#'
#' @description
#' This function changes specified entries in the data-quality
#' flags of a <%=class%> object, leaving other entries unaltered. (If the object
#' does not yet have an entry in its \code{metadata} for a \code{name} flag,
#' then one is set up and filled with \code{default}.)
#'
#' @details
#' <%=note%>
#'
#' @section Development note:
#' This function was added in late April, 2018, and is likely to undergo
#' changes until the end of June of that year.  Use with caution.
#'
#' @param object An oce object.
#'
#' @param name Character string indicating the name of the variable to be flagged. If
#' this variable is not contained in the object's \code{data} slot, an error is reported.
#'
#' @param value The value to be inserted in the flag.
#'
#' @param default The default (good) value of the flag. This is used only if
#' the object does not yet have yet have a entry for \code{name} flags. In that case,
#' storage is set up for the flag and it is filled with the \code{default} value, after
#' which \code{setFlags} returns to the task of setting flag values at indicated locations.
#'
#' @param i Integer index (required). If the item for which a flag is to be created
#' is stored as a vector, then \code{i} must be a vector. If it is a matrix or
#' array, then \code{i} must be a matrix with as many columns as there are dimensions
#' of the item.
#'
#' @param debug Integer set to 0 for quiet action or to 1 for some debugging.
#'
#' @return An object with flags set as indicated.
#'
#' @family functions relating to data-quality flags

