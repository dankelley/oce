#' @title Create colors suitable for <%=colorItem%> fields
#'
#' @aliases oce.colors<%=colorItemUC%> oceColors<%=colorItemUC%>
#'
#' @description
#' Create a set of colors for displaying <%=colorItem%> values,
#' based on the scheme devised by Thyng et al. (2016) and
#' presented in a python package by Thyng (2019).
#' The color specifications were transliterated from
#' python to R on 2015-09-29, but have not been adjusted
#' since, even though the python source has changed.  This
#' is to prevent breaking old `oce` code.  To get the
#' latest versions of these colours or other colours,
#' use the \CRANpkg{cmocean} R package
#' (Thyng, Richards, and Krylov, 2019) directly, as is
#' illustrated (with the "matter" scheme) in Example 2.
#' Note that the \CRANpkg{cmocean} core functions provide a way
#' to select between various versions of the colour schemes.
#' It is also worth considering the palettes provided by the
#' \CRANpkg{viridis} package, as illustrated (with the
#' "inferno" scheme) in Example 3.
#'
#' @return A vector of color specifications.
#'
#' @param n number of colors to create.
#'
#' @author Krysten M. Thyng (Python version), Dan Kelley (R transliteration)
#'
#' @examples
#' library(oce)
#'
#' # Example 1
#' imagep(volcano, col=oceColors<%=colorItemUC%>(128),
#'        zlab="oceColors<%=colorItemUC%>")
#'\dontrun{
#' # Example 2 (requires the cmocean package)
#' imagep(volcano, col=cmocean::cmocean("matter"),
#'        zlab='cmocean::cmocean("matter")')}
#'
#'\dontrun{
#' # Example 3 (requires the viridis package)
#' imagep(volcano, col=viridis::inferno,
#'        zlab='viridis::inferno')}
#'
#' @family things related to colors
#'
#' @references
#'
#' * Thyng, Kristen, Chad Greene, Robert Hetland, Heather Zimmerle, and Steven DiMarco.
#'   \dQuote{True Colors of Oceanography: Guidelines for Effective and Accurate Colormap Selection.}
#'   Oceanography 29, no. 3 (September 1, 2016): 9–13.
#'   \url{https://doi.org/10.5670/oceanog.2016.66}.
#'
#' * Thyng, Kristen. Kthyng/Cmocean. Python, 2019.
#'   \url{https://github.com/kthyng/cmocean}.
#'
#' * Thyng, Kristen, Clark Richards, and Ivan Krylov.
#'   Cmocean: Beautiful Colour Maps for Oceanography (version 0.2), 2019.
#'   \url{https://CRAN.R-project.org/package=cmocean}.

