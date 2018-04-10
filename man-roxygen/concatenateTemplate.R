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
#' ctd1 <- subset(ctd, scan <= median(ctd[["scan"]]))
#' ctd2 <- subset(ctd, scan > median(ctd[["scan"]]))
#' CTD <- concatenate(ctd1, ctd2)
#'
#' ## 2. Split, then recombine, an adp object.
#' data(adp)
#' midtime <- median(adp[["time"]])
#' adp1 <- subset(adp, time <= midtime)
#' adp2 <- subset(adp, time > midtime)
#' ADP <- concatenate(adp1, adp2)
#'
#'\dontrun{
#' ## 3. Download two met files and combine them.
#' met1 <- read.met(download.met(id=6358, year=2003, month=8))
#' met2 <- read.met(download.met(id=6358, year=2003, month=9))
#' MET <- concatenate(met1, met2)
#'}
#'
#' @author Dan Kelley
#'
#' @family functions that concatenate \code{oce} objects.
