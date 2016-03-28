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
#' the \code{action} argument to select an action to be applied to mached
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
#' or (b) a single item named \code{ALL} that indicates that the operations are to take
#' place all the entries in \code{object}'s \code{data} slot.
#' Each element in the list must be set to an integer or vector of integers,
#' specifying conditions to be met before actions are to be taken.
#'
#' @param actions An optional \code{\link{list}} that contains items with
#' names that match those in the \code{flags} argument.  If \code{actions}
#' is not supplied, the default will be to set all values identified by
#' \code{flags} to \code{NA}; this can also be specified by
#' specifying \code{actions=list(ALL="NA")}. It is also possible to specify 
#' functions that calculate replacement values. These are provided
#' with \code{object} as the single argument, and must return a
#' replacement for the data item in question.
#'
#' @section Implementation status: \code{handleFlags} is a new function as of March 2016,
#' and it will probably change through the Spring of 2016.
#' Almost nothing works yet, and users should not be doing
#' any more than looking at the documentation and telling the developers
#' whether the planned functioning seems reasonable.
#' The hope is to get trial code working for \code{ctd}
#' and \code{argo} data types by the end of April. After that, there
#' will likely  be a month or more of testing with real-world
#' work, with possible changes to the user interface. Then other
#' types will be added, as needed.
#'
#' @family functions that handle data-quality flags

