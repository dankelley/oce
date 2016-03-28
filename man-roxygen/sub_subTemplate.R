#' @description
#' The \code{[[} notation works for all \code{oce} objects. It looks up
#' information in a two-step process. First, a class-specific function 
#' is used to try to access the requested information (see
#' \dQuote{Details of the specialized method}). Then, if no match is found, a
#' general function is used (see \sQuote{Details of the general
#' method}). If neither function is successful in locating
#' the desired item, then \code{NULL} is returned.
#'
#' @section Details of the general method:
#'
#' If the specialized method produces no matches, the following generalized
#' method is applied. As with the specialized method, the procedure hinges
#' first on the value of \code{i}.
#'
#' First, a check is made as to whether \code{i} names one of the standard
#' \code{oce} slots, and returns the slot contents if so. Thus, 
#' \code{x[["metadata"]]} will retrieve the \code{metadata} slot, 
#' while \code{x[["data"]]} and \code{x[["processingLog"]]} return
#' those slots.
#'
#' Then, a search is made of the items in the \code{metadata} slot. A direct
#' match to the item named in \code{i} will retrieve the corresponding item.
#' For example, a common use for \code{\link{ctd-class}} data is
#' \code{x[["station"]]}, which looks up the station ID.
#'
#' Finally, a partial-match search is done of the \code{data} slot, using
#' \code{\link{pmatch}}.
#'
#' If none of these three conditions holds, \code{NULL} is returned.
#'
#' @param i Character string indicating the name of item to extract.
#' @param j Optional additional information on the \code{i} item.
#' @param ... Optional additional information (ignored).
#'
#' @family functions that access data within oce objects
#' @author Dan Kelley

