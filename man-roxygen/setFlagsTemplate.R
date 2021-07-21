#' @title Set data-quality flags within a <%=class%> object
#'
#' @description
#' This function changes specified entries in the data-quality
#' flags of a <%=class%> object, which are stored within
#' a list named `flags` that resides in the `metadata`
#' slot. If the object already has a flag set up for `name`,
#' then only the specified entries are altered. If not, the flag
#' entry is first created and its entries set to `default`,
#' after which the entries specified by `i`
#' are changed to `value`.
#'
#' The specification is made with `i`, the form of which
#' is determined by the data item in question. Generally,
#' the rules are as follows:
#'
#' 1. If the data item is a vector, then `i` must be (a)
#' an integer vector specifying indices to be set to `value`,
#' (b) a logical vector of length matching the data item, with
#' `TRUE` meaning to set the flag to `value`, or (c)
#' a function that takes an `oce` object as its single
#' argument, and returns a vector in either of the forms
#' just described.
#'
#' 2. If the data item is an array, then `i` must be
#' (a) a data frame of integers whose rows specify spots to change
#' (where the number of columns matches the number of dimensions
#' of the data item), (b) a logical array that has dimension equal to
#' that of the data item, or (c) a function that takes an `oce`
#' object as its single input and returns such a data frame or array.
#'
#' See \dQuote{Details} for the particular case of [<%=class%>-class] objects.
#'
#' @details
#' <%=note%>
#'
#' @param object An oce object.
#'
#' @param name Character string indicating the name of the variable to be flagged. If
#' this variable is not contained in the object's `data` slot, an error is reported.
#'
#' @param i Indication of where to insert the flags; see \dQuote{Description} for
#' general rules and \dQuote{Details} for rules for [<%=class%>-class]
#' objects.
#'
#' @param value The value to be inserted in the flag.
#'
#' @param debug Integer set to 0 for quiet action or to 1 for some debugging.
#'
#' @return An object with flags set as indicated.
#'
#' @family functions relating to data-quality flags

