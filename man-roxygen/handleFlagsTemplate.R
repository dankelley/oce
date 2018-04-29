#' @description Data-quality flags are stored in the \code{metadata}
#' slot of \code{\link{oce-class}} objects in a
#' \code{\link{list}} named \code{flags}.
#' The present function (a generic that has specialized versions
#' for various data classes) provides a way to
#' manipulate the core data based on
#' the data-quality flags. For example, a common operation is to replace suspicious
#' or erroneous data with \code{NA}.
#'
#' If \code{metadata$flags} in the object supplied as the first argument
#' is empty, then that object is returned, unaltered.
#' Otherwise, \code{handleFlags} analyses the data-quality flags within
#' the object, in relation to the \code{flags} argument, and interprets
#' the \code{action} argument to select an action to be applied to matched
#' data.
#'
#' Reasonable defaults are used if \code{flags} and \code{actions}
#' are not supplied (see \sQuote{Details}),
#' but different schemes are used in different
#' data archives, so it is risky to rely on these defaults.
#' It is usually necessary to tailor \code{flags} and \code{actions}
#' to the data and the analysis goals.
#'
#' @param flags An optional \code{\link{list}} containing (a)
#' items with names of entries in the \code{data} slot of \code{object},
#' or (b) a single unnamed item. In the first case, the attention is
#' focussed on the named items, while in the second case the
#' all the data in the \code{object}'s \code{data} slot are examined.
#' Each element in the list must be set to an integer or vector of integers,
#' specifying conditions to be met before actions are to be taken.
#' See \dQuote{Details} for the default that is used if \code{flags} is not supplied.
#'
#' @param actions An optional \code{\link{list}} that contains items with
#' names that match those in the \code{flags} argument.  If \code{actions}
#' is not supplied, the default will be to set all values identified by
#' \code{flags} to \code{NA}; this can also be specified by
#' specifying \code{actions=list("NA")}. It is also possible to specify
#' functions that calculate replacement values. These are provided
#' with \code{object} as the single argument, and must return a
#' replacement for the data item in question.
#' See \dQuote{Details} for the default that is used if \code{actions} is not supplied.
#'
#' @param debug An optional integer specifying the degree of debugging, with
#' value 0 meaning to skip debugging and 1 or higher meaning to print some
#' information about the arguments and the data. It is usually a good idea to set
#' this to 1 for initial work with a dataset, to see which flags are being
#' handled for each data item. If not supplied, this defaults to the value of
#' \code{\link{getOption}("oceDebug")}.
#'
## @section Implementation status: \code{handleFlags} is a new function as of March 2016,
## and it will probably continue to evolve through the rest of 2016.
## Users are asked to be patient, and to provide help by
## looking at the documentation and telling the developers
## whether the planned functioning seems reasonable.
#'
#' @family functions relating to data-quality flags

