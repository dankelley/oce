#' @section Retrieving slot contents:
#'
#' The full contents of the \code{data} and \code{metadata} slots of a \code{<%=class%>}
#' object named \code{<%=class%>} may be retrieved in the standard R way. For example,
#' \code{slot(<%=class%>, "data")} and \code{slot(<%=class%>, "metadata")} return
#' the \code{data} and \code{metadata} slots, respectively. The
#' \code{\link{[[,<%=class%>-method}} operator can also be used to access slots,
#' with \code{<%=class%>[["data"]]} and \code{<%=class%>[["metadata"]]}, respectively.
#' Furthermore, \code{\link{[[,<%=class%>-method}} can be used to retrieve
#' named items (and potentially some derived items) within the
#' \code{metadata} and \code{data} slots, the former taking precedence
#' over the latter in the lookup. It is also possible
#' to find items more directly, using \code{\link{oceGetData}} and
#' \code{\link{oceGetMetadata}}, but this cannot retrieve derived items.
