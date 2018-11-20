#' @description This function concatenates <%=class%> objects. It is intended
#' for objects holding data sampled through time, and it works by pasting
#' together data linearly if they are vectors, by row if they are matrices,
#' and by second index if they are arrays. It has been tested for the following
#' classes:
#' \code{\link{adp-class}},
#' \code{\link{adv-class}},
#' \code{\link{ctd-class}},
#' and
#' \code{\link{met-class}}. It may do useful things for other classes, and
#' so users are encouraged to try, and to report problems to the developers.
#' It is unlikely that the function will do anything even remotely useful
#' for image and topographic data, to name just two cases that do not fit
#' the sampled-over-time category.
#'
#' @param object An object of \code{\link{<%=class%>-class}}, or a list containing such
#' objects (in which case the remaining arguments are ignored).
#'
#' @param ... Optional additional objects of \code{\link{<%=class%>-class}}.
#'
#' @return An object of \code{\link{<%=class%>-class}}.
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
