#' @title Set data-quality flags within a <%=class%> object
#'
#' @description
#' This function changes specified entries in the data-quality
#' flags of a <%=class%> object, which are stored within
#' a list named \code{flags} that resides in the \code{metadata}
#' slot. If the object already has a flag set up for \code{name},
#' then only the specified entries are altered. If not, the flag
#' entry is first created and its entries set to \code{default},
#' after which the entries specified by \code{i}
#' are changed to \code{value}.
#'
#' The specification is made with \code{i}, the form of which
#' is determined by the data item in question. Generally,
#' the rules are as follows:
#'\enumerate{
#' \item If the data item is a vector, then \code{i} must be (a)
#' an integer vector specifying indices to be set to \code{value},
#' (b) a logical vector of length matching the data item, with
#' \code{TRUE} meaning to set the flag to \code{value}, or (c)
#' a function that takes an \code{oce} object as its single
#' argument, and returns a vector in either of the forms
#' just described.
#' \item If the data item is an array, then \code{i} must be
#' (a) a data frame of integers whose rows specify spots to change
#' (where the number of columns matches the number of dimensions
#' of the data item), (b) a logical array that has dimension equal to
#' that of the data item, or (c) a function that takes an \code{oce}
#' object as its single input and returns such a data frame or array.
#'}
#' See \dQuote{Details} for the particular case of
#' \code{\link{<%=class%>-class}} objects.
#'
#' @details
#' <%=note%>
#'
#' @section Caution:
#' This function was added in late April, 2018, and is likely to undergo
#' changes until the mid-summer of that year.  Use with caution.
#'
#' @param object An oce object.
#'
#' @param name Character string indicating the name of the variable to be flagged. If
#' this variable is not contained in the object's \code{data} slot, an error is reported.
#'
#' @param value The value to be inserted in the flag.
#'
#' @param i Indication of where to insert the flags; see \dQuote{Description} for
#' general rules and \dQuote{Details} for rules for \code{\link{<%=class%>-class}}
#' objects.
#'
#' @param default The default (good) value of the flag. This is used only if
#' the object does not yet have yet have a entry for \code{name} flags. In that case,
#' storage is set up for the flag and it is filled with the \code{default} value, after
#' which \code{setFlags} returns to the task of setting flag values at indicated locations.
#'
#' @param debug Integer set to 0 for quiet action or to 1 for some debugging.
#'
#' @return An object with flags set as indicated.
#'
#' @family functions relating to data-quality flags

