#' @description This function concatenates <%=class%> objects.
#"
#' @param object An object of \code{\link{<%=class%>-class}}, or a list containing such
#' objects (in which case the remaining arguments are ignored).
#"
#' @param ... Optional additional objects of \code{\link{<%=class%>-class}}.
#'
#'
#' @examples
#' ## 1. Split, then recombine, a ctd object.
#' data(ctd)
#' a <- subset(ctd, scan <= median(ctd[["scan"]]))
#' b <- subset(ctd, scan > median(ctd[["scan"]]))
#' ab <- concatenate(a,b)
#'
#' ## 2. Split, then recombine an adp object.
#' data(adp)
#' midtime <- median(adp[["time"]])
#' a <- subset(adp, time <= midtime)
#' b <- subset(adp, time > midtime)
#' ab <- concatenate(a, b)
#'
#'\dontrun{
#' ## 3. Download and combine EnvCan monthly met datasets.
#' d8 <- read.met(download.met(id=6358, year=2003, month=8, destdir="."))
#' d9 <- read.met(download.met(id=6358, year=2003, month=9, destdir="."))
#' dd <- concatenate(d8, d9)
#'}
#'
#' @author Dan Kelley
#'
#' @family functions that concatenate \code{oce} objects.
