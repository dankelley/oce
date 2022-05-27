#' @description
#'
#' Today's instruments typically record in the ITS-90 scale, but some old
#' datasets will be in the IPTS-68 scale. `T90fromT68()` converts from the
#' IPTS-68 to the ITS-90 scale, using Saunders' (1990) formula, while
#' `T68fromT90()` does the reverse. The difference between IPTS-68 and
#' ITS-90 values is typically a few millidegrees (see \sQuote{Examples}), which
#' is seldom visible on a typical temperature profile, but may be of interest
#' in some precise work.  Mostly for historical interest, `T90fromT48()`
#' is provided to convert from the ITS-48 system to ITS-90.
#'
#' @references
#' P. M. Saunders, 1990. The international temperature scale of
#' 1990, ITS-90.  WOCE Newsletter, volume 10, September 1990, page 10.
#' http://www.nodc.noaa.gov/woce/wdiu/wocedocs/newsltr/news10/contents.htm
#'
#' @examples
#' library(oce)
#' T68 <- seq(3, 20, 1)
#' T90 <- T90fromT68(T68)
#' sqrt(mean((T68-T90)^2))
#'
#' @author Dan Kelley
#'
#' @family functions that calculate seawater properties

