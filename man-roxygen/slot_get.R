#' @section Retrieving slot contents:
#'
#' The full contents of the \code{data} and \code{metadata} slots of a \code{<%=class%>}
#' object named \code{<%=class%>} may be retrieved in the standard R way, with
#' \code{slot(<%=class%>, "data")} and \code{slot(<%=class%>, "metadata")}, or with
#' \code{<%=class%>[["data"]]} and \code{<%=class%>[["metadata"]]}, where the latter uses an
#' overloading of the \code{[[} operator for \code{<%=class%>} objects.
#'
#' The \code{[[} operator can also be used to look for named items within
#' the slots (see \code{\link{[[,<%=class%>-method}}).
#' It first checks the \code{metadata} slot, returning the value
#' if it is there, and then tries in the \code{data} slot if not. If this
#' order of precedence is unsuitable, \code{\link{oceGetData}} and
#' \code{\link{oceGetMetadata}} can be used.

