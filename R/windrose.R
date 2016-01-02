setMethod(f="initialize",
          signature="windrose",
          definition=function(.Object) {
              .Object@processingLog$time <- as.POSIXct(Sys.time())
              .Object@processingLog$value <- "create 'windrose' object"
              return(.Object)
          })


setMethod(f="summary",
          signature="windrose",
          definition=function(object, ...) {
              cat("Windrose data\n-------------\n\n")
              n <- length(object@data$theta)
              dtheta <- abs(diff(object@data$theta[1:2]))
              cat("* Have n=", n, "angles, separated by dtheta=", dtheta,"\n\n")
              callNextMethod()
          })


setMethod(f="[[",
          signature(x="windrose", i="ANY", j="ANY"),
          definition=function(x, i, j, ...) {
              callNextMethod()
          })


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
    ai <- 1 + floor((angle+pi)/dt)
    ai <- (ai-1)%%nt + 1 # clean up problems (thanks, adeckmyn at github!!)
    if (min(ai) < 1)
        stop("problem setting up bins (ai<1)")
    if (max(ai) > nt)
        stop("problem setting up bins (ai>xlen)")
    for (i in 1:nt) {
        inside <- ai==i
        oceDebug(debug, sum(inside), "counts for angle category", i,
                 "(", round(180/pi*(theta[i]-dt2), 4), "to", round(180/pi*(theta[i]+dt2), 4), "deg)\n")
        count[i] <- sum(inside)
        mean[i] <- mean(R[inside], na.rm=TRUE)
        fives[i,] <- fivenum(R[inside])
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

setMethod(f="plot",
          signature=signature("windrose"),
          definition=function(x,
                              type=c("count","mean", "median", "fivenum"),
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
                      stop("'col' should be a list of 4 colours")
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
                      polygon(xlist, ylist,col=col[1], border=col[3])
                  }
                  title(paste("Counts (max ", max, ")", sep=""))
              } else if (type == "mean") {
                  max <- max(x@data$mean, na.rm=TRUE)
                  for (i in 1:nt) {
                      r <- x@data$mean[i] / max
                      ##cat("t=", t[i], " r=", r, "\n")
                      xlist <- c(0, r * cos(t[i] - dt2), r * cos(t[i] + dt2), 0)
                      ylist <- c(0, r * sin(t[i] - dt2), r * sin(t[i] + dt2), 0)
                      polygon(xlist, ylist,col=col[1], border=col[3])
                  }
                  title(paste("Means (max ", sprintf(max, fmt="%.3g"), ")", sep=""))
              } else if (type == "median") {
                  max <- max(x@data$fives[,5], na.rm=TRUE)
                  for (i in 1:nt) {
                      r <- x@data$fives[i,3] / max
                      ##cat("t=", t[i], " r=", r, "\n")
                      xlist <- c(0, r * cos(t[i] - dt2), r * cos(t[i] + dt2), 0)
                      ylist <- c(0, r * sin(t[i] - dt2), r * sin(t[i] + dt2), 0)
                      polygon(xlist, ylist,col=col[1], border=col[3])
                  }
                  title(paste("Medians (max ", sprintf(max,fmt="%.3g"), ")", sep=""))
              } else if (type == "fivenum") {
                  max <- max(x@data$fives[,3], na.rm=TRUE)
                  for (i in 1:nt) {
                      for (j in 2:3) {
                          tm <- t[i] - dt2
                          tp <- t[i] + dt2
                          r0 <- x@data$fives[i, j-1] / max
                          r  <- x@data$fives[i, j  ] / max
                          xlist <- c(r0 * cos(tm), r * cos(tm), r * cos(tp), r0 * cos(tp))
                          ylist <- c(r0 * sin(tm), r * sin(tm), r * sin(tp), r0 * sin(tp))
                          thiscol <- col[c(2,1,1,2)][j-1]
                          polygon(xlist, ylist, col=thiscol, border=col[4])
                      }
                      r <- x@data$fivenum[i, 3] / max
                      lines(c(r * cos(tm), r * cos(tp)), c(r * sin(tm), r * sin(tp)), col="blue", lwd=2)
                  }
                  title(paste("Fiveum (max ", sprintf(max,fmt="%.3g"), ")", sep=""))
              }
              invisible()
          })


