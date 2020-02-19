#' @title Create colors suitable for <%=colorItem%> fields
#'
#' @aliases oce.colors<%=colorItemUC%> oceColors<%=colorItemUC%>
#'
#' @description
#' Create a set of colors for displaying <%=colorItem%> values,
#' based on the scheme devised by Kristen M. Thyng
#' in her `cmcolor` Python package
#' (available at https://github.com/kthyng/cmocean). The
#' color specifications were downloaded for use here
#' on 2015-09-29. To avoid changes in `oce` scripts,
#' more recent changes to `cmcolor` have not been
#' tracked; [oceColorsClosure()]
#' has an example of how to incorporate such changes.
#'
#' @return A vector of color specificications.
#'
#' @param n number of colors to create.
#'
#' @author Krysten M. Thyng (Python version), Dan Kelley (R transliteration)
#'
#' @examples
#' library(oce)
#' imagep(volcano, col=oceColors<%=colorItemUC%>(128),
#'        zlab="oceColors<%=colorItemUC%>")
#'
#' @family things related to colors

