#' @slot data In this and all \code{oce} objects, the \code{data} slot is a
#' \code{\link{list}} containing the main data for the object.
#'
#' @slot metadata In this and all \code{oce} objects, the \code{metadata} slot is
#' a \code{\link{list}} containing information about the \code{data} or about the
#' object itself.  An example of the former might be the location at which a
#' measurement was made, and of the latter might be the name of the experiment in
#' which the data were collected.
#'
#' @slot processingLog In this and all \code{oce} objects, the \code{processingLog}
#' slot is a \code{\link{list}} with entries describing the creation and evolution
#' of the object. This slot is updated by various \code{oce} functions.  The
#' contents are listed in object summaries and with
#' \code{\link{processingLogShow}}.
#'
#'
#'
#' @section Retrieving slot contents:
#'
#' The contents of the \code{data} and \code{metadata} slots of a \code{oce}
#' object named \code{obj} may be retrieved with the standard R system, using
#' \code{slot(obj, "data")} and \code{slot(obj, "metadata")}.  In addition, the
#' \code{[[} operator is overloaded for \code{oce} objects, as a convenience.
#'
#' If \code{"obj"} is an \code{oce} object, then \code{obj[["data"]]} retrieves
#' the \code{data} slot, and \code{obj[["metadata"]]} retrieves the \code{metadata}
#' slot.  Each of these is a list, so the items within them may be retrieved in
#' the usual way, e.g. either \code{ctd[["data"]]$salinity} or
#' \code{ctd[["data"]]["salinity"]} may be used to recover the salinities in a
#' \code{ctd} object. Similar actions apply to the \code{metadata} slot.
#'
#' Individual items within \code{oce} objects can be retrieved in two ways.  The
#' first is to use \code{\link{oceGetData}} and \code{\link{oceGetMetadata}},
#' respectively.  The second is to use the \code{[[} operator.
#'
#' Items within the slots may be retrieved by name alone, according to a somewhat
#' involved scheme. If \code{obj} is an \code{oce} object, then e.g.
#' \code{obj[["x"]]} will retrieve \code{x} from the \code{metadata} slot if it
#' exists there, or from the \code{data} slot, if it exists there. This order of
#' precedence seldom causes problems, because it is rare to have an
#' identically-named item in these two slots.
#'
#'
#' @section Modifying slot contents:
#'
#' Although there is a \code{[[<-} notation for modifying the contents
#' of \code{oce} objects, it is wiser to use \code{\link{oceSetData}}
#' \code{\link{oceSetMetadata}}, respectively.

