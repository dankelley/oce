#' @title Create storage for a flag, and initialize values, for a <%=class%> object
#'
#' @description
#' This function creates an item for a named variable within
#' the `flags` entry in the object's `metadata` slot.
#' The purpose is both to document a flag scheme
#' and to make it so that [initializeFlags()]
#' and [setFlags()] can specify flags by
#' name, in addition to number. A generic function, it is
#' specialized for some classes via interpretation of the
#' `scheme` argument (see \dQuote{Details}, for those
#' object classes that have such specializations).
#'
#' @details
#' If `object` already contains a `flags` entry with
#' the indicated name, then it is returned unaltered, and a warning
#' is issued.
#'
#' @param object An [oce-class] object.
#'
#' @param name Character value indicating the name of a variable within the
#' `data` slot of `object`.
#'
#' @param value Numerical or character value to be stored in the newly-created
#' entry within `flags`. (A character value will only work if
#' [initializeFlags()] has been used first on `object`.)
#'
#' @param debug Integer set to 0 for quiet action or to 1 for some debugging.
#'
#' @return An object with the `flags` item within the
#' `metadata` slot set up as indicated.
#'
#' @family functions relating to data-quality flags

