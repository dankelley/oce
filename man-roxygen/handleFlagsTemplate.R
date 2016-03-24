#' @description \code{NEWhandleFlags} is a generic function that has specialized variants
#' that are used for different types of \code{oce} object.  Each is designed to help users
#' deal with data flags, by performing specified actions on data elements that are
#' flagged in particular ways.  A common case is to set flagged data to the R missing-value
#' code, \code{NA}.  The specialized forms of this function are set up to handle
#' flag codes that are typical for the target data type.
#' Specifying the \code{flags} and \code{actions} arguments permits finer-grained
#' control, as well as the handling of cases employing unusual conventions for the
#' values of flags.
#' @param flags An optional \code{\link{list}} containing items with names of entries in the \code{data} slot of \code{object}. Flags in \code{oce} are always non-negative integers, and giving a negative value indicates an exception. For example, the World Hydrographic Program uses flag 3 for 'probably bad' and flag 4 for 'appears erroneous', so excluding temperatures flagged with those code values could be done with \code{flags=list(temperature=c(3,4))}. A disadvantage of this approach is that it requires listing all the codes that may need attention. Usually, the better approach is to use negative values to indicate the good data, e.g. WHP uses code 1 for 'correct' data, so \code{flags=list(temperature=-1)} could be used to isolate temperatures flagged with any other value. See \dQuote{Examples}.
#' @param actions An optional \code{\link{list}} that contains items with names that match those in the \code{flags} argument.  If \code{actions} is not supplied, the default will be to set all values specified by the \code{flags} argument to the R missing value, \code{NA}. This is also accomplished by e.g. \code{actions=list(temperature="NA")}. It is also possible to specify a function that will calculate a replacement value. This function will be provided with everything in the \code{data} slot of \code{object}, and must return a vector or matrix that will be used as a replacement for the original value. For example, a statistical model of a TS relationship might be used to replace bad salinities. See \dQuote{Examples}.
#' @section Caution: \code{NEWhandleFlags} is a new function as of March 2016, and it is very likely to change
#' through the Spring of 2016. Almost nothing works yet, and users should not be doing
#' any more than looking at the documentation and telling the developers whether the planned 
#' functioning seems reasonable.
#' The hope is to get trial code working for \code{ctd}
#' and \code{argo} data types by the end of April. After that, there will likely
#' be a month or more of testing with real-world
#' work, with possible changes to the user interface. Then other types will be added,
#' as needed.

