## vim: tw=120 shiftwidth=4 softtabstop=4 expandtab:
## cm.R current-meter support (interocean S4)

setMethod(f="initialize",
          signature="cm",
          definition=function(.Object) {
              .Object@processingLog$time <- as.POSIXct(Sys.time())
              .Object@processingLog$value <- "create 'cm' object"
              return(.Object)
          })

setMethod(f="summary",
          signature="cm",
          definition=function(object, ...) {
              dataNames <- names(object@data)
              threes <- matrix(nrow=(-1+length(dataNames)), ncol=3)
              ii <- 1
              for (i in 1:length(dataNames)) {
                  if (names(object@data)[i] != "time") {
                      threes[ii,] <- threenum(object@data[[dataNames[i]]])
                      ii <- ii + 1
                  }
              }
              rownames(threes) <- dataNames[dataNames != "time"] ## FIXME: should ignore 'sample' too, if it's there
              colnames(threes) <- c("Min.", "Mean", "Max.")
              vDim <- dim(object@data$v)

              cat("cm Summary\n----------\n\n", ...)
              cat(paste("* Instrument:         ", object@metadata$instrumentType, ", serial number ``", paste(object@metadata$serialNumber, collapse=""), "``\n", sep=""), ...)
              cat(paste("* Source filename:    ``", object@metadata$filename, "``\n", sep=""), ...)
              if ("latitude" %in% names(object@metadata)) {
                  cat(paste("* Location:           ", if (is.na(object@metadata$latitude)) "unknown latitude" else sprintf("%.5f N", object@metadata$latitude), ", ",
                            if (is.na(object@metadata$longitude)) "unknown longitude" else sprintf("%.5f E", object@metadata$longitude), "\n"))
              }
              cat(sprintf("* Measurements:       %s %s to %s %s sampled at %.4g Hz\n",
                          format(object@metadata$measurementStart), attr(object@metadata$measurementStart, "tzone"),
                          format(object@metadata$measurementEnd), attr(object@metadata$measurementEnd, "tzone"),
                          1 / object@metadata$measurementDeltat), ...)
              cat(sprintf("* Subsample:          %s %s to %s %s sampled at %.4g Hz\n",
                          format(object@metadata$subsampleStart), attr(object@metadata$subsampleStart, "tzone"),
                          format(object@metadata$subsampleEnd),  attr(object@metadata$subsampleEnd, "tzone"),
                          1 / object@metadata$subsampleDeltat), ...)
              cat(sprintf("* Cells:              %d, centered at %.3f m to %.3f m, spaced by %.3f m\n",
                          object@metadata$numberOfCells, object@metadata$distance[1],  object@metadata$distance[length(object@metadata$distance)], diff(object@metadata$distance[1:2])),  ...)
              print(threes)
              processingLogShow(object)
          })

read.cm <- function(file, from=1, to, by=1, tz=getOption("oceTz"),
                    type=c("s4"),
                    longitude=NA, latitude=NA,
                    debug=getOption("oceDebug"), monitor=FALSE, processingLog, ...)
{
    if (debug > 2)
        debug <- 2
    if (debug < 0)
        debug  <- 0
    oceDebug(debug, "read.cm(file=\"",file,
              "\", from=", format(from),
              ", to=", if (missing(to)) "(missing)" else format(to), ", by=", by, "type=", type, ", ...) {\n", sep="", unindent=1)
    type <- match.arg(type)
    if (type == "s4")
        read.cm.s4(file=file, from=from, to=to, by=by, tz=tz,
                   longitude=longitude, latitude=latitude,
                   debug=debug-1, monitor=monitor, processingLog=processingLog, ...)
    else
        stop("unknown type of current meter")
}

read.cm.s4 <- function(file, from=1, to, by=1, tz=getOption("oceTz"),
                       longitude=NA, latitude=NA,
                       debug=getOption("oceDebug"), monitor=FALSE, processingLog, ...)
{
    if (debug > 1)
        debug <- 1
    oceDebug(debug, "read.cm.s4(file=\"",file,
              "\", from=", format(from),
              ", to=", if (missing(to)) "(missing)" else format(to), ", by=", by, ", ...) {\n", sep="", unindent=1)
    if (is.character(file)) {
        filename <- fullFilename(file)
        file <- file(file, "r")
        on.exit(close(file))
    }
    if (!inherits(file, "connection"))
        stop("argument `file' must be a character string or connection")
    if (!isOpen(file)) {
        filename <- "(connection)"
        open(file, "rb")
        on.exit(close(file))
    }
    metadata <- list(filename=filename)
    ## Examine the first line of the file, to get serial number, etc.
    items <- scan(file, "character", nlines=1, sep="\t", quiet=TRUE) # slow, but just one line
    oceDebug(debug, "line 1 contains: ", paste(items, collapse=" "), "\n")
    metadata$manufacturer <- "interocean"
    metadata$instrumentType <- "s4"
    for (i in 1:(-1 + length(items))) {
        if (length(grep("Serial", items[i])))
            metadata$serialNumber <- items[i+1]
        else if (length(grep("Version", items[i])))
            metadata$version <- items[i+1]
        else if (length(grep("Type", items[i])))
            metadata$type <- items[i+1]
    }
    ## Skip through the rest of the header, and start paying attention when
    ## row number is 1, 2, and then 3.  These first rows give us the time
    ## sequence.
    foundNames <- FALSE
    for (skip in 2:20) {
        items <- scan(file, "character",nlines=1,sep="\t", quiet=TRUE) # slow, but just 20 lines, max
        oceDebug(debug, "line", skip, "contains: ", paste(items, collapse=" "), "\n")
        if (items[1] == "Sample #") {
            names <- sub('[ ]+$', '', sub('^[ ]+','', items))
            names <- ifelse(0 == nchar(names), paste("column", 1:length(names), sep=""), names)
            foundNames <- TRUE
        } else if (items[1] == "1") {
            start.day <- items[2]
        } else if (items[1] == "2") {
            start.hms <- items[3]
        } else if (items[1] == "3") {
            t0 <- strptime(paste(start.day, start.hms), "%m/%d/%Y %H:%M:%s", tz=tz)
            t1 <- strptime(paste(start.day, items[3]), "%m/%d/%Y %H:%M:%s", tz=tz)
            deltat <- as.numeric(t1) - as.numeric(t0)
            break
        }
    }
    ## NOTE: we *skip* the first 3 lines of data.  This is mainly because those lines
    ## had a different number of TAB-separated elements than the rows below, which
    ## thwarted the use of read.table() to read the data.  Besides, those first
    ## 3 lines are likely to just be in-air measurements, anyway ... there is likely
    ## to be little harm in skipping quite a lot more than just 3 points!
    metadata$measurementStart <- t0 + (2 + skip) * deltat
    metadata$measurementDeltat <- deltat
    d <- read.table(file, skip=skip, sep='\t', stringsAsFactors=FALSE)
    col.north <- 5
    col.east <- 6
    col.conductivity <- 13
    col.temperature <- 13
    col.depth <- 14
    col.heading <- 17
    col.salinity <- 19
    if (foundNames) {
        names <- names[1:dim(d)[2]]
        col.east <- which(names == "Veast")
        if (length(col.east) > 0)
            col.east <- col.east[1]
        col.north <- which(names == "Vnorth")
        if (length(col.north) > 0)
            col.north <- col.north[1]
        col.heading <- which(names == "Hdg")
        if (length(col.heading) > 0)
            col.heading <- col.heading[1]
        col.conductivity <- which(names == "Cond")
        if (length(col.conductivity) > 0)
            col.conductivity <- col.conductivity[1]
        col.salinity <- which(names == "Sal")
        if (length(col.salinity) > 0)
            col.salinity <- col.salinity[1]
        col.temperature <- which(names == "T-Temp")
        if (length(col.temperature) > 0)
            col.temperature <- col.temperature[1]
        col.depth <- which(names == "Depth")
        if (length(col.depth) > 0)
            col.depth <- col.depth[1]
    }
    u <- d[, col.east] / 100
    v <- d[, col.north] / 100
    heading <- d[, col.heading]
    ## the 42.91754 value is electrical conductivity at SP=35, t=15, p=0
    conductivity <- d[, col.conductivity] / 100 / 42.91754
    temperature <- d[, col.temperature]
    depth <- d[, col.depth]
    calculate.salinity.from.conductivity <- TRUE # FIXME: why is "Sal" so wrong in the sample file?
    if (calculate.salinity.from.conductivity)
        salinity <- swSCTp(conductivity, temperature, depth) # FIXME: should really be pressure
    else
        salinity <- d[, col.salinity]
    sample <- as.numeric(d[, 1])
    n <- length(u)
    time <- metadata$measurementStart + seq(0,n-1)*metadata$measurementDeltat
    if (inherits(from, "POSIXt")) {
        if (!inherits(to, "POSIXt"))
            stop("if 'from' is POSIXt, then 'to' must be, also")
        if (!is.numeric(by) || by != 1)
            stop("sorry, 'by' must equal 1, in this (early) version of read.cm.s4()")
        from.to.POSIX <- TRUE
        from.index <- which(time >= from)[1]
        if (is.na(from.index))
            from.index <- 1
        to.index <- which(to <= time)[1]
        if (is.na(to.index))
            to.index <- n
        oceDebug(debug, "Time-based trimming: from=", format(from), "to=", format(to), "yield from.index=", from.index, "and to.index=", to.index, "\n")
        keep <- seq(from.index, to.index)
    } else {
        if (!is.numeric(from))
            stop("'from' must be either POSIXt or numeric")
        if (missing(to))
            to <- n
        if (!is.numeric(to))
            stop("'to' must be either POSIXt or numeric")
        keep <- seq(from, to)
    }
    keep <- keep[1 <= keep]
    keep <- keep[keep <= n]
    data <- list(sample=sample[keep], time=time[keep], u=u[keep], v=v[keep], heading=heading[keep], salinity=salinity[keep], temperature=temperature[keep], depth=depth[keep])
    metadata$measurementEnd <- time[length(time)]
    rval <- new('cm')
    rval@metadata <- metadata
    rval@data <- data
    if (missing(processingLog)) processingLog <- paste(deparse(match.call()), sep="", collapse="")
    rval@processingLog <- processingLogAppend(rval@processingLog, processingLog)
    oceDebug(debug, "} # read.cm()\n", unindent=1)
    rval
}



setMethod(f="plot",
          signature=signature("cm"),
          definition=function(x,
                              which=c(1, 2, 7, 9),
                              type="l",
                              adorn=NULL,
                              drawTimeRange=getOption("oceDrawTimeRange"),
                              drawZeroLine=FALSE,
                              mgp=getOption("oceMgp"),
                              mar=c(mgp[1]+1.5,mgp[1]+1.5,1.5,1.5),
                              small=2000,
                              main="",
                              tformat,
                              debug=getOption("oceDebug"),
                              ...)
          {
              oceDebug(debug, "plot.cm() {\n", unindent=1)
              oceDebug(debug, "  par(mar)=", paste(par('mar'), collapse=" "), "\n")
              oceDebug(debug, "  par(mai)=", paste(par('mai'), collapse=" "), "\n")
              if (!inherits(x, "cm"))
                  stop("method is only for objects of class '", "cm", "'")
              if (!(is.null(x@metadata$have.actual.data) || x@metadata$have.actual.data)) {
                  warning("there are no profiles in this dataset")
                  return
              }
              opar <- par(no.readonly = TRUE)
              lw <- length(which)
              oceDebug(debug, "length(which) =", lw, "\n")
              if (lw > 1)
                  on.exit(par(opar))
              par(mgp=mgp, mar=mar)
              dots <- list(...)
              gave.ylim <- "ylim" %in% names(dots)
              ylim.given <- if (gave.ylim) dots[["ylim"]] else NULL

              oceDebug(debug, "later on in plot.adp:\n")
              oceDebug(debug, "  par(mar)=", paste(par('mar'), collapse=" "), "\n")
              oceDebug(debug, "  par(mai)=", paste(par('mai'), collapse=" "), "\n")
              oceDebug(debug, "which:", which, "\n")
              which <- oce.pmatch(which,
                                  list(u=1, v=2, "progressive vector"=3,
                                       "uv"=4, "uv+ellipse"=5, "uv+ellipse+arrow"=6,
                                       depth=7, salinity=8, temperature=9, heading=10, TS=11))
              oceDebug(debug, "which:", which, "\n")
              adorn.length <- length(adorn)
              if (adorn.length == 1) {
                  adorn <- rep(adorn, lw)
                  adorn.length <- lw
              }

              tt <- x@data$time
              class(tt) <- "POSIXct"              # otherwise image() gives warnings
              if (lw > 1) {
                  par(mfrow=c(lw, 1))
                  oceDebug(debug, "calling par(mfrow=c(", lw, ", 1)\n")
              }
              len <- length(x@data$u)
              for (w in 1:lw) {
                  oceDebug(debug, "which[", w, "]=", which[w], "; drawTimeRange=", drawTimeRange, "\n")
                  if (which[w] == 1) {
                      oce.plot.ts(x@data$time, x@data$u,
                                  type=type,
                                  xlab="", ylab=resizableLabel("u"),
                                  main=main, mgp=mgp, mar=c(mgp[1], mgp[1]+1.5, 1.5, 1.5),
                                  tformat=tformat)
                  } else if (which[w] == 2) {
                      oce.plot.ts(x@data$time, x@data$v,
                                  type=type,
                                  xlab="", ylab=resizableLabel("v"),
                                  main=main, mgp=mgp, mar=c(mgp[1], mgp[1]+1.5, 1.5, 1.5),
                                  tformat=tformat)
                  } else if (which[w] == 3) {     # or "progressive vector"
                      oceDebug(debug, "progressive vector plot\n")
                      dt <- as.numeric(difftime(x@data$time[2], x@data$time[1],units="sec")) # FIXME: assuming equal dt
                      m.per.km <- 1000
                      u <- x@data$u
                      v <- x@data$v
                      u[is.na(u)] <- 0        # zero out missing
                      v[is.na(v)] <- 0
                      x.dist <- cumsum(u) * dt / m.per.km
                      y.dist <- cumsum(v) * dt / m.per.km
                      plot(x.dist, y.dist,
                           xlab=resizableLabel("km"), ylab=resizableLabel("km"),
                           type='l', asp=1, ...)
                  } else if (which[w] %in% 4:6) {     # "uv" (if 4), "uv+ellipse" (if 5), or "uv+ellipse+arrow" (if 6)
                      oceDebug(debug, "\"uv\", \"uv+ellipse\", or \"uv+ellipse+arrow\" plot\n")
                      if (len <= small)
                          plot(x@data$u, x@data$v, type=type,
                               xlab=resizableLabel("u"), ylab=resizableLabel("v"),
                               asp=1, ...)
                      else
                          smoothScatter(x@data$u, x@data$v,
                                        xlab=resizableLabel("u"), ylab=resizableLabel("v"),
                                        asp=1, ...)
                      if (which[w] >= 5) {
                          oceDebug(debug, "\"uv+ellipse\", or \"uv+ellipse+arrow\" plot\n")
                          ok <- !is.na(x@data$u) & !is.na(x@data$v)
                          e <- eigen(cov(data.frame(u=x@data$u[ok], v=x@data$v[ok])))
                          major <- sqrt(e$values[1])
                          minor <- sqrt(e$values[2])
                          theta <- seq(0, 2*pi, length.out=360/5)
                          xx <- major * cos(theta)
                          yy <- minor * sin(theta)
                          theta0 <- atan2(e$vectors[2,1], e$vectors[1,1])
                          rotate <- matrix(c(cos(theta0), -sin(theta0), sin(theta0), cos(theta0)), nrow=2, byrow=TRUE)
                          xxyy <- rotate %*% rbind(xx, yy)
                          col <- if ("col" %in% names(dots)) col else "darkblue"
                          lines(xxyy[1,], xxyy[2,], lwd=5, col="yellow")
                          lines(xxyy[1,], xxyy[2,], lwd=2, col=col)
                          if (which[w] >= 6) {
                              oceDebug(debug, "\"uv+ellipse+arrow\" plot\n")
                              umean <- mean(x@data$u, na.rm=TRUE)
                              vmean <- mean(x@data$v, na.rm=TRUE)
                              arrows(0, 0, umean, vmean, lwd=5, length=1/10, col="yellow")
                              arrows(0, 0, umean, vmean, lwd=2, length=1/10, col=col)
                          }
                      }
                  } else if (which[w] == 7) {
                      oce.plot.ts(x@data$time, x@data$depth,
                                  type=type,
                                  xlab="", ylab=resizableLabel("depth"),
                                  main=main, mgp=mgp, mar=c(mgp[1], mgp[1]+1.5, 1.5, 1.5),
                                  tformat=tformat)
                  } else if (which[w] == 8) {
                      oce.plot.ts(x@data$time, x@data$salinity,
                                  type=type,
                                  xlab="", ylab=resizableLabel("S", "y"),
                                  main=main, mgp=mgp, mar=c(mgp[1], mgp[1]+1.5, 1.5, 1.5),
                                  tformat=tformat)
                  } else if (which[w] == 9) {
                      oce.plot.ts(x@data$time, x@data$temperature,
                                  type=type,
                                  xlab="", ylab=resizableLabel("T", "y"),
                                  main=main, mgp=mgp, mar=c(mgp[1], mgp[1]+1.5, 1.5, 1.5),
                                  tformat=tformat)
                  } else if (which[w] == 10) {
                      oce.plot.ts(x@data$time, x@data$heading,
                                  type=type,
                                  xlab="", ylab=resizableLabel("heading"),
                                  main=main, mgp=mgp, mar=c(mgp[1], mgp[1]+1.5, 1.5, 1.5),
                                  tformat=tformat)
                  } else if (which[w] == 11) {
                      plotTS(as.ctd(x@data$salinity, x@data$temperature, x@data$depth), main=main, ...) 
                  } else {
                      stop("unknown value of which (", which[w], ")")
                  }
                  if (w <= adorn.length) {
                      t <- try(eval(adorn[w]), silent=TRUE)
                      if (class(t) == "try-error")
                          warning("cannot evaluate adorn[", w, "]\n")
                  }
              }
              oceDebug(debug, "} # plot.cm()\n", unindent=1)
              invisible()
          })

