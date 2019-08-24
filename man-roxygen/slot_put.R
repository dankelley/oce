#' @section Modifying slot contents:
#'
#' Although the \code{[[<-} operator may permit modification of the contents
#' of [<%= class%>-class] objects (see \code{\link{[[<-,<%=class%>-method}}),
#' it is better to use [oceSetData()] and [oceSetMetadata()],
#' because those functions save an entry in the `processingLog`
#' that describes the change.

