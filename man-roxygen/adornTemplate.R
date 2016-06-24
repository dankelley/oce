#' @param adorn (\strong{Defunct})
#' An \code{\link{expression}} or vector of expressions that contain
#' R code that is to be executed immediately after each panel of the plot.
#' If the number of expressions matches the number of panels, then the 
#' expressions are used for the corresponding panels; otherwise, the
#' expression list is extended to match the number of panels (i.e.
#' to obtain \code{length(which)} elements). Note that \code{adorn}
#' is a dangerous argument, because if the expressions contained
#' therein set up local storage, there is a chance of entirely
#' disrupting the plotting. \strong{For this reason, \code{adorn} was
#' marked as defunct in June 2016, and will be removed entirely 
#' after the July CRAN release.} Users with existing code that uses
#' \code{adorn} should simply plot the panels individually, and
#' use conventional R functions, e.g. \code{\link{lines}} etc.,
#' after each panel, to achieve the desired effect.  (See
#' \code{\link{oce-defunct}} for notes on other deprecated or
#' defunct \code{oce} features.)
