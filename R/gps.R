## vim:textwidth=128:expandtab:shiftwidth=4:softtabstop=4

setMethod(f="initialize",
          signature="gps",
          definition=function(.Object, longitude, latitude, filename="") {
              if (!missing(longitude)) .Object@data$longitude <- as.numeric(longitude)
              if (!missing(latitude)) .Object@data$latitude <- as.numeric(latitude)
              .Object@metadata$filename <- filename
              .Object@processingLog$time <- as.POSIXct(Sys.time())
              .Object@processingLog$value <- "create 'gps' object"
              return(.Object)
          })


setMethod(f="summary",
          signature="gps",
          definition=function(object, ...) {
              cat("GPS Summary\n-----------------\n\n")
              callNextMethod()
          })


setMethod(f="[[",
          signature(x="gps", i="ANY", j="ANY"),
          definition=function(x, i, j, drop) {
              ## I use 'as' because I could not figure out callNextMethod() etc
              #as(x, "oce")[[i, j, drop]]
              as(x, "oce")[[i]]
          })

setMethod(f="plot",
          signature=signature("gps"),
          definition=function (x,
                               xlab="", ylab="",
                               asp,
                               clongitude, clatitude, span,
                               projection, parameters=NULL, orientation=NULL,
                               ## center, span,
                               expand=1,
                               mgp=getOption("oceMgp"),
                               mar=c(mgp[1]+1,mgp[1]+1,1,1),
                               bg,
                               axes=TRUE, cex.axis=par('cex.axis'),
                               add=FALSE, inset=FALSE,
                               geographical=0,
                               debug=getOption("oceDebug"),
                               ...)
          {
              oceDebug(debug, "plot.gps(...",
                       ", clongitude=", if(missing(clongitude)) "(missing)" else clongitude,
                       ", clatitude=", if(missing(clatitude)) "(missing)" else clatitude, 
                       ", span=", if(missing(span)) "(missing)" else span,
                       ", geographical=", geographical,
                       ", cex.axis=", cex.axis, 
                       ", inset=", inset, 
                       ", ...) {\n", sep="", unindent=1)
              if (!missing(projection)) {
                  if (missing(span))
                      span <- 1000
                  if (missing(clongitude))
                      longitudelim <- c(-180, 180)
                  else
                      longitudelim <- clongitude + c(-1, 1) * span / 111
                  if (missing(clatitude))
                      latitudelim <- c(-90, 90)
                  else
                      latitudelim <- clatitude + c(-1, 1) * span / 111
                  return(mapPlot(x[['longitude']], x[['latitude']], longitudelim, latitudelim,
                                 mgp=mgp, mar=mar,
                                 bg="white", type='l', axes=TRUE,
                                 projection=projection, parameters=parameters, orientation=orientation,
                                 debug=debug, ...))
              }
              geographical <- round(geographical)
              if (geographical < 0 || geographical > 2)
                  stop("argument geographical must be 0, 1, or 2")
              if (is.list(x) && "latitude" %in% names(x)) {
                  if (!("longitude" %in% names(x)))
                      stop("list must contain item named 'longitude'")
                  x <- as.gps(longitude=x$longitude, latitude=x$latitude)
              } else {
                  if (!inherits(x, "gps"))
                      stop("method is only for gps objects, or lists that contain 'latitude' and 'longitude'")
              }
              longitude <- x[["longitude"]]
              latitude <- x[["latitude"]]
              dots <- list(...)
              dotsNames <- names(dots)
              ##gave.center <- !missing(clongitude) && !missing(clatitude)
              if ("center" %in% dotsNames)
                  stop("use 'clongitude' and 'clatitude' instead of 'center'")
              if ("xlim" %in% dotsNames) stop("cannot supply 'xlim'; use 'clongitude' and 'span' instead")
              if ("ylim" %in% dotsNames) stop("cannot supply 'ylim'; use 'clatitude' and 'span' instead")
              if (!inset)
                  par(mar=mar)
              par(mgp=mgp)
              if (add) {
                  lines(longitude, latitude, ...)
              } else {
                  ##gaveSpan <- !missing(span)
                  if (!missing(clatitude) && !missing(clongitude)) {
                      if (!missing(asp))
                          warning("argument 'asp' being ignored, because argument 'clatitude' and 'clongitude' were given")
                      asp <- 1 / cos(clatitude * atan2(1, 1) / 45) #  ignore any provided asp, because lat from center over-rides it
                      xr <- clongitude + span * c(-1/2, 1/2) / 111.11 / asp
                      yr <- clatitude + span * c(-1/2, 1/2) / 111.11
                      xr0 <- xr
                      yr0 <- yr
                      oceDebug(debug, "xr=", xr," yr=", yr, " asp=", asp, "\n")
                  } else {
                      xr0 <- range(longitude, na.rm=TRUE)
                      yr0 <- range(latitude, na.rm=TRUE)
                      oceDebug(debug, "xr0=", xr0, " yr0=", yr0, "\n")
                      if (missing(asp)) {
                          if ("ylim" %in% dotsNames)
                              asp <- 1 / cos(mean(range(dots$ylim, na.rm=TRUE)) * atan2(1, 1) / 45) # dy/dx
                          else
                              asp <- 1 / cos(mean(yr0) * atan2(1, 1) / 45) # dy/dx
                      }
                      ## Expand
                      if (missing(span)) {
                          if (expand >= 0 && max(abs(xr0)) < 100 && max(abs(yr0) < 70)) { # don't expand if full map
                              xr <- mean(xr0) + expand * diff(xr0) * c(-1/2, 1/2)
                              yr <- mean(yr0) + expand * diff(yr0) * c(-1/2, 1/2)
                          } else {
                              xr <- xr0
                              yr <- yr0
                          }
                      } else {
                          xr <- mean(xr0) + span * c(-1/2, 1/2) / 111.11 / asp
                          yr <- mean(yr0)+ span * c(-1/2, 1/2) / 111.11
                      }
                      oceDebug(debug, "xr=", xr, " yr=", yr, "\n")
                  }
                  ## Trim lat or lon, to avoid empty margin space
                  asp.page <- par("fin")[2] / par("fin")[1] # dy / dx
                  oceDebug(debug, "par('pin')=", par('pin'), "\n")
                  oceDebug(debug, "par('fin')=", par('fin'), "\n")
                  oceDebug(debug, "asp=", asp, "\n")
                  oceDebug(debug, "asp.page=", asp.page, "\n")
                  if (!is.finite(asp))
                      asp <- 1 / cos(clatitude * atan2(1, 1) / 45)
                  if (asp < asp.page) {
                      oceDebug(debug, "type 1 (will narrow x range)\n")
                      d <- asp.page / asp * diff(xr)
                      oceDebug(debug, "  xr original:", xr, "\n")
                      xr <- mean(xr) + d * c(-1/2, 1/2)
                      oceDebug(debug, "  xr narrowed:", xr, "\n")
                  } else {
                      oceDebug(debug, "type 2 (will narrow y range)\n")
                      d <- asp.page / asp * diff(yr)
                      oceDebug(debug, "  yr original:", yr, "\n")
                      yr <- mean(yr) + d * c(-1/2, 1/2)
                      oceDebug(debug, "  yr narrowed:", yr, "\n")
                  }
                  ## Avoid looking beyond the poles, or the dateline
                  if (xr[1] < (-180))
                      xr[1] <- (-180)
                  if (xr[2] >  180)
                      xr[2] <- 180
                  if (yr[1] <  (-90))
                      yr[1] <- (-90)
                  if (yr[2] >  90)
                      yr[2] <- 90
                  oceDebug(debug, "after range trimming, xr=", xr, " yr=", yr, "\n")
                  ## Draw underlay, if desired
                  plot(xr, yr, asp=asp, xlab=xlab, ylab=ylab, type="n", xaxs="i", yaxs="i", axes=FALSE, ...)
                  if (!missing(bg)) {
                      plot.window(xr, yr, asp=asp, xlab=xlab, ylab=ylab, xaxs="i", yaxs="i", log="", ...)
                      usr <- par("usr")
                      oceDebug(debug, "drawing background; usr=", par('usr'), "bg=", bg, "\n")
                      rect(usr[1], usr[3], usr[2], usr[4], col=bg)
                      par(new=TRUE)
                  }
                  ## Ranges
                  ##plot(xr, yr, asp=asp, xlab=xlab, ylab=ylab, type="n", xaxs="i", yaxs="i", axes=FALSE, ...)
                  usrTrimmed <- par('usr')
                  ## Construct axes "manually" because axis() does not know the physical range
                  if (axes) {
                      prettyLat <- function(yr, ...)
                      {
                          res <- pretty(yr, ...)
                          if (diff(yr) > 100)
                              res <- seq(-90, 90, 45)
                          res
                      }
                      prettyLon <- function(xr, ...)
                      {
                          res <- pretty(xr, ...)
                          if (diff(xr) > 100)
                              res <- seq(-180, 180, 45)
                          res
                      }
                      oceDebug(debug, "xr:", xr, ", yr:", yr, ", xr0:", xr0, ", yr0:", yr0, "\n")
                      ##xr.pretty <- prettyLon(xr, n=if (geographical)3 else 5, high.u.bias=20)
                      xr.pretty <- prettyLon(par('usr')[1:2], n=if (geographical)3 else 5, high.u.bias=20)
                      ##yr.pretty <- prettyLat(yr, n=if (geographical)3 else 5, high.u.bias=20)
                      yr.pretty <- prettyLat(par('usr')[3:4], n=if (geographical)3 else 5, high.u.bias=20)
                      oceDebug(debug, "xr.pretty=", xr.pretty, "\n")
                      oceDebug(debug, "yr.pretty=", yr.pretty, "\n")
                      oceDebug(debug, "usrTrimmed", usrTrimmed, "(original)\n")
                      usrTrimmed[1] <- max(-180, usrTrimmed[1])
                      usrTrimmed[2] <- min( 180, usrTrimmed[2])
                      usrTrimmed[3] <- max( -90, usrTrimmed[3])
                      usrTrimmed[4] <- min(  90, usrTrimmed[4])
                      oceDebug(debug, "usrTrimmed", usrTrimmed, "\n")
                      oceDebug(debug, "par('usr')", par('usr'), "\n")
                      xlabels <- format(xr.pretty)
                      ylabels <- format(yr.pretty)
                      if (geographical >= 1) {
                          xlabels <- sub("-", "", xlabels)
                          ylabels <- sub("-", "", ylabels)
                      }
                      if (geographical == 2) {
                          xr.pretty <- prettyPosition(xr.pretty, debug=debug-1)
                          yr.pretty <- prettyPosition(yr.pretty, debug=debug-1)
                          xlabels <- formatPosition(xr.pretty, type='expression')
                          ylabels <- formatPosition(yr.pretty, type='expression')
                      }
                      axis(1, at=xr.pretty, labels=xlabels, pos=usrTrimmed[3], cex.axis=cex.axis)
                      oceDebug(debug, "putting bottom x axis at", usrTrimmed[3], "with labels:", xlabels, "\n")
                      axis(2, at=yr.pretty, labels=ylabels, pos=usrTrimmed[1], cex.axis=cex.axis, cex=cex.axis)
                      oceDebug(debug, "putting left y axis at", usrTrimmed[1], "\n")
                      axis(3, at=xr.pretty, labels=rep("", length.out=length(xr.pretty)), pos=usrTrimmed[4], cex.axis=cex.axis)
                      ##axis(3, at=xr.pretty, pos=usrTrimmed[4], labels=FALSE)
                      ##oceDebug(debug, "putting top x axis at", usrTrimmed[4], "\n")
                      axis(4, at=yr.pretty, pos=usrTrimmed[2], labels=FALSE, cex.axis=cex.axis)
                      oceDebug(debug, "putting right y axis at", usrTrimmed[2], "\n")
                  }
                  yaxp <- par("yaxp")
                  oceDebug(debug, "par('yaxp')", par("yaxp"), "\n")
                  oceDebug(debug, "par('pin')", par("pin"), "\n")
                  if (yaxp[1] < -90 | yaxp[2] > 90) {
                      oceDebug(debug, "trimming latitude; pin=", par("pin"), "FIXME: not working\n")
                      oceDebug(debug, "trimming latitdue; yaxp=", yaxp, "FIXME: not working\n")
                      ##yscale <- 180 / (yaxp[2] - yaxp[1])
                      ## FIXME: should allow type as an arg
                      points(x[["longitude"]], x[["latitude"]], ...)
                  } else {
                      points(longitude, latitude, ...)
                      if (axes)
                          rect(usrTrimmed[1], usrTrimmed[3], usrTrimmed[2], usrTrimmed[4])
                  }
              }
              ##box()
              oceDebug(debug, "par('usr')=", par('usr'), "\n")
              oceDebug(debug, "} # plot.gps()\n", unindent=1)
              invisible()
          })


as.gps <- function(longitude, latitude, filename="")
{
    names <- names(longitude)
    if ("longitude" %in% names && "latitude" %in% names) {
        latitude <- longitude[["latitude"]]
        longitude <- longitude[["longitude"]]
    }
    new('gps', longitude=longitude, latitude=latitude, filename=filename)
}


read.gps <- function(file, type=NULL, debug=getOption("oceDebug"), processingLog)
{
    oceDebug(debug, "read.gps(...) {\n", sep="", unindent=1)
    filename <- NULL
    if (is.character(file)) {
        filename <- fullFilename(file)
        file <- file(file, "r")
        on.exit(close(file))
    }
    if (!inherits(file, "connection"))
        stop("argument `file' must be a character string or connection")
    if (!isOpen(file)) {
        open(file, "r")
        on.exit(close(file))
    }
    if (is.null(type)) {
        tokens <- scan(file, "character", n=5)
        found <- grep("gpx", tokens)
        if (length(found) > 0) {
            type <- "gpx"
        } else {
            warning("cannot determine file type; assuming 'gpx'")
        }
    }
    type <- match.arg(type, c("gpx"))
    oceDebug(debug, "file type:", type, "\n")
    lines <- readLines(file)
    look <- grep("lat=", lines)
    latlon <- lines[look]
    latlonCleaned <- gsub("[a-zA-Z<>=\"/]*", "", latlon)
    latlon <- read.table(text=latlonCleaned)
    res <- new("gps", longitude=latlon[,2], latitude=latlon[,1], file=filename)
    oceDebug(debug, "} # read.gps()\n", sep="", unindent=1)
    res
}

