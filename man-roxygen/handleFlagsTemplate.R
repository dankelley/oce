#' @description \code{NEWhandleFlags} is a generic function that has specialized variants
#' that are used for different types of \code{oce} object.  Each is designed to help users
#' deal with data flags, by performing specified actions on data elements that are
#' flagged in particular ways.  A common case is to set flagged data to the R missing-value
#' code, \code{NA}.  The specialized forms of this function are set up to handle
#' flag codes that are typical for the target data type.
#' Specifying the \code{flags} and \code{actions} arguments permits finer-grained
#' control, as well as the handling of cases employing unusual conventions for the
#' values of flags.
#' @param flags An optional \code{\link{list}} that contains items with names matching the names of entries in the object's \code{data} slot. See \dQuote{Examples}.
#' @param actions An optional \code{\link{list}} that contains items with names that match those in the \code{flags} argument.  If this is not supplied, the action is to set flagged values to \code{NA}. See \dQuote{Examples}.
#' @section Caution: \code{NEWhandleFlags} is a new function as of March 2016, and it is very likely to change
#' through the Spring of 2016. Almost nothing works yet, and users should not be doing
#' any more than looking at the documentation and telling the developers whether the planned 
#' functioning seems reasonable.
#' The hope is to get trial code working for \code{ctd}
#' and \code{argo} data types by the end of April. After that, there will likely
#' be a month or more of testing with real-world
#' work, with possible changes to the user interface. After that, other types will be added,
#' needed.

