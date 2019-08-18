## vim:textwidth=128:expandtab:shiftwidth=4:softtabstop=4

#' Class to Store Windrose Data
#'
#' Windrose objects store statistical information about winds, mainly for
#' plotting as "wind rose" plots (see [plot,windrose-method()].
#' There is no reading method, because there is no standard way to store
#' wind data in files; instead, [as.windrose()] is provided
#' to construct `windrose` objects.  Data elements may be retrieved
#' with [[[,windrose-method()] or replaced with
#' [[[<-,windrose-method()]. Data summaries are provided with
#' [summary,windrose-method()].
#'
#' @templateVar class windrose
#'
#' @templateVar dataExample {}
#'
#' @templateVar metadataExample {}
#'
#' @template slot_summary
#'
#' @template slot_put
#'
#' @template slot_get
#'
#' @family classes provided by oce
#' @family things related to windrose data
#'
#' @md
setClass("windrose", contains="oce")

setMethod(f="initialize",
          signature="windrose",
          definition=function(.Object) {
              .Object@processingLog$time <- presentTime()
              .Object@processingLog$value <- "create 'windrose' object"
              return(.Object)
          })

#' Summarize a `windrose` object
#'
#' Summarizes some of the data in a `windrose` object.
#'
#' @param object A [windrose-class] object.
#'
#' @param ... Further arguments passed to or from other methods.
#'
#' @author Dan Kelley
#'
#' @family things related to windrose data
#'
#' @md
setMethod(f="summary",
          signature="windrose",
          definition=function(object, ...) {
              cat("Windrose data\n-------------\n\n")
              n <- length(object@data$theta)
              dtheta <- abs(diff(object@data$theta[1:2]))
              cat("* Have n=", n, "angles, separated by dtheta=", dtheta, "\n\n")
              invisible(callNextMethod()) # summary
          })


#' @title Extract Something From a Windrose Object
#'
#' @param x A [windrose-class] object.
#'
#' @section Details of the specialized windrose method:
#' There are no special features for [windrose-class] data;
#' the general method is used directly.
#' @template sub_subTemplate
#' @family things related to windrose data
#'
#' @md
setMethod(f="[[",
          signature(x="windrose", i="ANY", j="ANY"),
          definition=function(x, i, j, ...) {
              callNextMethod()         # [[
          })

#' @title Replace Parts of a Windrose Object
#'
#' @param x A [windrose-class] object.
#'
#' @template sub_subsetTemplate
#'
#' @family things related to windrose data
#'
#' @md
setMethod(f="[[<-",
          signature(x="windrose", i="ANY", j="ANY"),
          definition=function(x, i, j, ..., value) {
              callNextMethod(x=x, i=i, j=j, ...=..., value=value) # [[<-
          })


#' Create a Windrose Object
#'
#' Create a wind-rose object, typically for plotting with
#' [plot,windrose-method()].
#'
#' @param x The x component of wind speed (or stress) *or* an object of class
#' `met` (see [met-class]), in which case the `u` and
#' `v` components of that object are used for the components of wind speed,
#' and `y` here is ignored.
#'
#' @param y The y component of wind speed (or stress).
#'
#' @param dtheta The angle increment (in degrees) within which to classify the data.
#'
#' @param debug A flag that turns on debugging.  Set to 1 to get a moderate amount
#' of debugging information, or to 2 to get more.
#'
#' @return A [windrose-class] object, with `data` slot containing
#'
#' \describe{
#'     \item{`n`}{the number of x values}
#'     \item{`x.mean`}{the mean of the x values}
#'     \item{`y.mean`}{the mean of the y values}
#'     \item{`theta`}{the central angle (in degrees) for the class}
#'     \item{`count`}{the number of observations in this class}
#'     \item{`mean`}{the mean of the observations in this class}
#'     \item{`fivenum`}{the [fivenum()] vector for
#'         observations in this class (the min, the lower hinge, the
#'         median, the upper hinge, and the max)}
#' }
#'
#' @examples
#' library(oce)
#' xcomp <- rnorm(360) + 1
#' ycomp <- rnorm(360)
#' wr <- as.windrose(xcomp, ycomp)
#' summary(wr)
#' plot(wr)
#'
#' @author Dan Kelley, with considerable help from Alex Deckmyn.
#'
#' @family things related to windrose data
#'
#' @md
as.windrose <- function(x, y, dtheta = 15, debug=getOption("oceDebug"))
{
    oceDebug(debug, "as.windrose(x, y, dtheta=", dtheta, ", debug=", debug, ") {\n", sep="", unindent=1)
    if (inherits(x, "met")) {
        tmp <- x
        x <- tmp[["u"]]
        y <- tmp[["v"]]
    }
    ok <- !is.na(x) & !is.na(y)
    x <- x[ok]
    y <- y[ok]
    xlen <- length(x)
    pi <- atan2(1, 1) * 4
    dt <- dtheta * pi / 180
    dt2 <- dt / 2
    R <- sqrt(x^2 + y^2)
    angle <- atan2(y, x)
    ## L <- max(R, na.rm=TRUE)
    nt <- round(2 * pi / dt)
    count <- mean <- vector("numeric", nt)
    fives <- matrix(0, nt, 5)
    theta <- seq(-pi+dt2, pi-dt2, length.out=nt)
    ## The bin-detection code was faulty until 2012-02-07.  This
    ## was pointed out by Alex Deckmyn, who also suggested the
    ## present solution.  His issue reports, available on
    ## github.com/dankelley/oce/issues, are a model of
    ## patience and insight.
    ai <- 1 + floor( (angle+pi)/dt )
    ai <- (ai-1)%%nt + 1 # clean up problems (thanks, adeckmyn at github!!)
    if (min(ai) < 1)
        stop("problem setting up bins (ai<1)")
    if (max(ai) > nt)
        stop("problem setting up bins (ai>xlen)")
    for (i in 1:nt) {
        inside <- ai==i
        oceDebug(debug, sum(inside), "counts for angle category", i,
                 "(", round(180 / pi * (theta[i]-dt2), 4), "to", round(180 / pi * (theta[i]+dt2), 4), "deg)\n")
        count[i] <- sum(inside)
        mean[i] <- mean(R[inside], na.rm=TRUE)
        fives[i, ] <- fivenum(R[inside])
    }
    if (sum(count) != xlen)
        stop("miscount in angles")
    res <- new('windrose')
    res@data <- list(n=length(x), x.mean=mean(x, na.rm=TRUE), y.mean=mean(y, na.rm=TRUE), theta=theta*180/pi,
                     count=count, mean=mean, fives=fives)
    res@metadata$dtheta <- dtheta
    res@processingLog <- processingLogAppend(res@processingLog, paste(deparse(match.call()), sep="", collapse=""))
    oceDebug(debug, "} # as.windrose()\n", sep="", unindent=1)
    res
}


#' @title Plot Windrose data
#'
#' @description
#' Plot a [windrose-class] object.
#'
#' @param x A [windrose-class] object.
#'
#' @param type The thing to be plotted, either the number of counts in the angle
#' interval, the mean of the values in the interval, the median of the values, or
#' a [fivenum()] representation of the values.
#'
#' @param convention String indicating whether to use meteorological convention or
#' oceanographic convention for the arrows that emanate from the centre of the
#' rose.  In meteorological convection, an arrow emanates towards the right on
#' the diagram if the wind is from the east; in oceanographic convention, such an
#' arrow indicates flow *to* the east.
#'
#' @param mgp Three-element numerical vector to use for [`par`]`(mgp)`, and also
#' for [`par`]`(mar)`, computed from this.  The default is tighter than the R
#' default, in order to use more space for the data and less for the axes.
#'
#' @param mar Four-element numerical vector to be used with [`par`]`("mar")`.
#'
#' @param col Optional list of colors to use.  If not set, the colors will be
#' `c("red", "pink", "blue", "lightgray")`.  For the first three types of
#' plot, the first color in this list is used to fill in the rose, the third is
#' used for the petals of the rose, and the fourth is used for grid lines. For the
#' `"fivenum"` type, the first color is used for the inter-quartile range,
#' the second is used outside this range, the third is used for the median, and
#' the fourth is, again, used for the grid lines.
#'
#' @param ... Optional arguments passed to plotting functions.
#'
#' @examples
#' library(oce)
#' opar <- par(no.readonly = TRUE)
#' xcomp <- rnorm(360) + 1
#' ycomp <- rnorm(360)
#' wr <- as.windrose(xcomp, ycomp)
#' par(mfrow=c(1, 2))
#' plot(wr)
#' plot(wr, "fivenum")
#' par(opar)
#'
#' @author Dan Kelley
#'
#' @family functions that plot oce data
#' @family things related to windrose data
#' @aliases plot.windrose
#'
#' @md
setMethod(f="plot",
          signature=signature("windrose"),
          definition=function(x,
                              type=c("count", "mean", "median", "fivenum"),
                              convention=c("meteorological", "oceanographic"),
                              mgp=getOption("oceMgp"),
                              mar=c(mgp[1], mgp[1], 1+mgp[1], mgp[1]),
                              col,
                              ...)
          {
              if (!inherits(x, "windrose"))
                  stop("method is only for objects of class '", "windrose", "'")
              type <- match.arg(type)
              convention <- match.arg(convention)
              nt <- length(x@data$theta)
              pi <- 4 * atan2(1, 1)
              if (convention == "meteorological")
                  t <- x@data$theta * pi / 180   # in radians
              else
                  t <- pi + x@data$theta * pi / 180  # in radians
              dt <- t[2] - t[1]
              dt2 <- dt / 2
                                        # Plot setup
              par(mgp=mgp, mar=mar)
              plot.new()
              pin <- par("pin")
              xlim <- ylim <- c(-1, 1)
              if (pin[1] > pin[2])
                  xlim <- (pin[1]/pin[2]) * xlim
              else ylim <- (pin[2]/pin[1]) * ylim
              plot.window(xlim, ylim, "", asp = 1)
              if (missing(col))
                  col <- c("red", "pink", "blue", "darkgray")
              else {
                  if (length(col) != 4)
                      stop("'col' should be a list of 4 colors")
              }
                                        # Draw circle and radii
              tt <- seq(0, 2*pi, length.out=100)
              px <- cos(tt)
              py <- sin(tt)
              lines(px, py, col=col[4])
              for (i in 1:nt) {
                  lines(c(0, cos(t[i] - dt2)), c(0, sin(t[i] - dt2)), lwd=0.5, col=col[4])
              }
              text( 0, -1, "S", pos=1)
              text(-1,  0, "W", pos=2)
              text( 0,  1, "N", pos=3)
              text( 1,  0, "E", pos=4)
              ## Draw rose in a given type
              if (type == "count") {
                  max <- max(x@data$count, na.rm=TRUE)
                  for (i in 1:nt) {
                      r <- x@data$count[i] / max
                      ##cat("t=", t[i], " r=", r, "\n")
                      xlist <- c(0, r * cos(t[i] - dt2), r * cos(t[i] + dt2), 0)
                      ylist <- c(0, r * sin(t[i] - dt2), r * sin(t[i] + dt2), 0)
                      polygon(xlist, ylist, col=col[1], border=col[3])
                  }
                  title(paste("Counts (max ", max, ")", sep=""))
              } else if (type == "mean") {
                  max <- max(x@data$mean, na.rm=TRUE)
                  for (i in 1:nt) {
                      r <- x@data$mean[i] / max
                      ##cat("t=", t[i], " r=", r, "\n")
                      xlist <- c(0, r * cos(t[i] - dt2), r * cos(t[i] + dt2), 0)
                      ylist <- c(0, r * sin(t[i] - dt2), r * sin(t[i] + dt2), 0)
                      polygon(xlist, ylist, col=col[1], border=col[3])
                  }
                  title(paste("Means (max ", sprintf(max, fmt="%.3g"), ")", sep=""))
              } else if (type == "median") {
                  max <- max(x@data$fives[, 5], na.rm=TRUE)
                  for (i in 1:nt) {
                      r <- x@data$fives[i, 3] / max
                      ##cat("t=", t[i], " r=", r, "\n")
                      xlist <- c(0, r * cos(t[i] - dt2), r * cos(t[i] + dt2), 0)
                      ylist <- c(0, r * sin(t[i] - dt2), r * sin(t[i] + dt2), 0)
                      polygon(xlist, ylist, col=col[1], border=col[3])
                  }
                  title(paste("Medians (max ", sprintf(max, fmt="%.3g"), ")", sep=""))
              } else if (type == "fivenum") {
                  max <- max(x@data$fives[, 3], na.rm=TRUE)
                  for (i in 1:nt) {
                      for (j in 2:3) {
                          tm <- t[i] - dt2
                          tp <- t[i] + dt2
                          r0 <- x@data$fives[i, j-1] / max
                          r  <- x@data$fives[i, j  ] / max
                          xlist <- c(r0 * cos(tm), r * cos(tm), r * cos(tp), r0 * cos(tp))
                          ylist <- c(r0 * sin(tm), r * sin(tm), r * sin(tp), r0 * sin(tp))
                          thiscol <- col[c(2, 1, 1, 2)][j-1]
                          polygon(xlist, ylist, col=thiscol, border=col[4])
                      }
                      r <- x@data$fivenum[i, 3] / max
                      lines(c(r * cos(tm), r * cos(tp)), c(r * sin(tm), r * sin(tp)), col="blue", lwd=2)
                  }
                  title(paste("Fiveum (max ", sprintf(max, fmt="%.3g"), ")", sep=""))
              }
              invisible()
          })
