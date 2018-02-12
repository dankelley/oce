#' @section Retrieving slot contents (general):
#'
#' The full contents of the \code{data} and \code{metadata} slots of a \code{oce}
#' object named \code{obj} may be retrieved in the standard R way, with
#' \code{slot(obj, "data")} and \code{slot(obj, "metadata")}, or with
#' \code{obj[["data"]]} and \code{obj[["metadata"]]}, where the latter uses an
#' overloading of the \code{[[} operator for \code{oce} objects.
#'
#' The \code{[[} mechanism can also be used to look for named items within
#' the slots. It first checks the \code{metadata} slot, returning the value
#' if it is there, and then tries in the \code{data} slot if not. If this
#' order of precedence is unsuitable, \code{\link{oceGetData}} and
#' \code{\link{oceGetMetadata}} can be used.
#'
#' In addition to looking up stored quantities, the \code{[[} operator
#' has other useful capabilities. If an object holds data-quality flags, then
#' \code{obj[["flags"]]} will retrieve them, as a list with names matching
#' the variable names.  Similarly, \code{obj[["units"]]} return a list of units.
#' Individual items may be found in the usual R way within these lists, but there
#' is a simpler way: use e.g. \code{obj[["temperatureFlag"]]} or
#' \code{obj[["temperatureUnit"]]}. In such cases, \code{[[} is returning
#' information that is stored somewhere in the object, but it can also return
#' wholly \strong{derived} quantities, depending on the object type. For
#' example, \code{obj[["N2"]]} will return the square of the buoyancy
#' frequency, if \code{obj} is an object from which that can be calculated
#' (such as a \code{ctd} object). Information about accessing such derived
#' quantities may be found in the \dQuote{Special access methods}
#' section of the documentation for those classes that support it.
#'
#' @section Modifying slot contents (general):
#'
#' Although there is a \code{[[<-} notation for modifying the contents
#' of \code{oce} objects, it is wiser to use \code{\link{oceSetData}}
#' \code{\link{oceSetMetadata}}, respectively, to be sure where the change will be
#' made, and to record the change in the \code{processingLog}.
