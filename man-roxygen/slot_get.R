#' @section Retrieving slot contents:
#'
#' The full contents of the `data` and `metadata` slots of a [<%=class%>-class]
#' object may be retrieved in the standard R way using [slot()]. For
#' example `slot(o,"data")` returns the `data` slot of an object named `o`,
#' and similarly `slot(o,"metadata")` returns
#' the `metadata` slot.
#'
#' The slots may also be obtained with the \code{\link{[[,<%=class%>-method}}
#' operator, as e.g. `o[["data"]]` and `o[["metadata"]]`, respectively.
#'
#' The \code{\link{[[,<%=class%>-method}} operator can also
#' be used to retrieve items from within the `data` and `metadata` slots.
#' For example, `o[["temperature"]]` can be used to retrieve temperature
#' from an object containing that quantity. The rule is that a named
#' quantity is sought first within the object's `metadata` slot,
#' with the `data` slot being checked only if `metadata` does not
#' contain the item. This \code{[[} method can also be used to get
#' certain derived quantities, if the object contains sufficient
#' information to calculate them. For example, an object that holds
#' (practical) salinity, temperature and pressure, along with
#' longitude and latitude, has sufficient information to compute
#' Absolute Salinity, and so `o[["SA"]]` will yield the
#' calculated Absolute Salinity.
#'
#' It is also possible to find items more directly, using [oceGetData()] and
#' [oceGetMetadata()], but neither of these functions can
#' retrieve derived items.

