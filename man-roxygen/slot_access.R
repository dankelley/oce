#' @section Retrieving slot contents:
#'
#' The full contents of the \code{data} and \code{metadata} slots of a \code{oce}
#' object named \code{obj} may be retrieved in the standard R way, with
#' \code{slot(obj, "data")} and \code{slot(obj, "metadata")}.
#'
#' \strong{FIXME: check whether the above permits changes on sub-items, and
#' document if so, but state why it's a bad idea}
#'
#' Individual items within \code{oce} objects can be retrieved in two ways.  The
#' first is to use \code{\link{oceGetData}} and \code{\link{oceGetMetadata}},
#' respectively.  The second is to use the \code{[[} operator, which
#' works as follows.
#'
#' If \code{"obj"} is an \code{oce} object, then \code{obj[["data"]]} retrieves
#' the \code{data} slot, and \code{obj[["metadata"]]} retrieves the \code{metadata}
#' slot.  Each of these is a list, so the items within them may be retrieved in
#' the usual way, e.g. either \code{ctd[["data"]]$salinity} or
#' \code{ctd[["data"]]["salinity"]} may be used to recover the salinities in a
#' \code{ctd} object. Similar actions apply to the \code{metadata} slot.
#'
#' \strong{FIXME: do I really want to talk about the above paragraph? I've never done it,
#' and I don't want people doing it. Experts will already know the ins
#' and outs, anyway. And I don't want to damage the data-hiding system.}
#'
#' In some circumstances, the items within the slots may be retrieved by
#' name alone, according to a somewhat
#' involved scheme. If \code{obj} is an \code{oce} object, then e.g.
#' \code{obj[["x"]]} will retrieve \code{x} from the \code{metadata} slot if it
#' exists there, or from the \code{data} slot, if it exists there. This order of
#' precedence seldom causes problems, because it is rare to have an
#' identically-named item in these two slots.
#'
#' \strong{FIXME: talk about flags and units here}
#'
#' For some object types, the \code{[[} mechanism also provides a way to
#' obtain some derived quantities that are not actually stored in the data.
#' For example, the buoyancy frequency may be available for hydrographic
#' objects.  Any object that has this capability will have a section
#' called \dQuote{Retrieving derived quantities} in its documentation, which
#' should be consulted for details.
#'
#' @section Modifying slot contents:
#'
#' Although there is a \code{[[<-} notation for modifying the contents
#' of \code{oce} objects, it is wiser to use \code{\link{oceSetData}}
#' \code{\link{oceSetMetadata}}, respectively.

