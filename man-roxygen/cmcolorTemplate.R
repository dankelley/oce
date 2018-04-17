#' @title Create colours suitable for <%=colorItem%> fields
#' @aliases oce.colors<%=colorItemUC%> oceColors<%=colorItemUC%>
#' @description
#' Create a set of colours for displaying <%=colorItem%> values,
#' based on the scheme devised by Kristen M. Thyng
#' in her \code{cmcolor} Python package, which is available at
#' \url{https://github.com/kthyng/cmocean}.
#'
#' @return A vector of colour specificications.
#'
#' @param n number of colours to create.
#'
#' @author Krysten M. Thyng (Python version), Dan Kelley (R transliteration)
#'
#' @examples
#' library(oce)
#' imagep(volcano, col=oceColors<%=colorItemUC%>(128),
#'        zlab="volcano dataset, autoscaled with oceColors<%=colorItemUC%>(128)")
#'
#' @family things related to colors

