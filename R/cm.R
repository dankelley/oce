## vim: tw=120 shiftwidth=4 softtabstop=4 expandtab:
## cm.R current-meter support (interocean S4)

setMethod(f="initialize",
          signature="cm",
          definition=function(.Object, filename="(unknown)", sample, time,
                              u, v, heading,
                              conductivity, conductivityUnit="mS/cm",
                              salinity, temperature, pressure) {
              .Object@metadata$filename <- filename
              .Object@metadata$units$temperatureUnit <- "ITS-90" # guess on the unit
              .Object@metadata$units$conductivityUnit <- conductivityUnit
              .Object@data$sample <- if (missing(sample)) NULL else sample
              .Object@data$time <- if (missing(time)) NULL else time
              .Object@data$u <- if (missing(u)) NULL else u
              .Object@data$v <- if (missing(v)) NULL else v
              .Object@data$heading <- if (missing(heading)) NULL else heading
              .Object@data$conductivity <- if (missing(conductivity)) NULL else conductivity
              .Object@data$salinity <- if (missing(salinity)) NULL else salinity
              .Object@data$temperature <- if (missing(temperature)) NULL else temperature
              .Object@data$pressure <- if (missing(pressure)) NULL else pressure
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

              cat("cm summary\n----------\n\n", ...)
              showMetadataItem(object, "filename",      "File source:        ")
              showMetadataItem(object, "type",          "Instrument type:    ")
              showMetadataItem(object, "serialNumber",  "Serial Number:      ")
              showMetadataItem(object, "version",       "Version:            ")
              t <- object@data$time
              cat(sprintf("* Measurements:       %s %s to %s %s sampled at %.4g Hz\n",
                          format(t[1]), attr(t, "tzone"),
                          format(tail(t, 1)), attr(t, "tzone"),
                          1 / (as.numeric(t[2])-as.numeric(t[1]))), ...)
              print(round(threes, 3))
              processingLogShow(object)
          })

read.cm <- function(file, from=1, to, by=1, tz=getOption("oceTz"),
                    type=c("s4"),
                    longitude=NA, latitude=NA,
                    debug=getOption("oceDebug"), monitor=FALSE, processingLog, ...)
{
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
    serialNumber <- "unknown"
    version <- "unknown"
    type <- "unknown"
    for (i in 1:(-1 + length(items))) {
        if (length(grep("Serial", items[i])))
            serialNumber <- items[i+1]
        else if (length(grep("Version", items[i])))
            version <- items[i+1]
        else if (length(grep("Type", items[i])))
            type <- items[i+1]
    }
    ## Skip through the rest of the header, and start paying attention when
    ## row number is 1, 2, and then 3.  These first rows give us the time
    ## sequence.
    foundNames <- FALSE
    headerStart <- 0
    lines <- readLines(file, n=20)
    for (i in 2:20) {
        items <- strsplit(lines[i], "\t")[[1]]
        oceDebug(debug, "line", i, "contains: ", paste(items, collapse=" "), "\n")
        if (items[1] == "Sample #") {
            names <- sub('[ ]+$', '', sub('^[ ]+','', items))
            names <- ifelse(0 == nchar(names), paste("column", 1:length(names), sep=""), names)
            foundNames <- TRUE
            headerStart <- i
        } else if (items[1] == "1") {
            start.day <- items[2]
        } else if (items[1] == "2") {
            start.hms <- items[3]
        } else if (items[1] == "3") {
            t0 <- strptime(paste(start.day, start.hms), format="%m/%d/%Y %H:%M:%S", tz=tz)
            t1 <- strptime(paste(start.day, items[3]), format="%m/%d/%Y %H:%M:%S", tz=tz)
            deltat <- as.numeric(t1) - as.numeric(t0)
            break
        }
    }
    pushBack(lines, file)
    ## Now try to guess the meanings of column names. This really is guesswork, since I have no documentation that
    ## explains these things. See the help page for this function for some more thoughts on the problem.
    d <- read.table(file, skip=headerStart+1, sep='\t', stringsAsFactors=FALSE, fill=TRUE)
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
    trimLines <- grep("[ a-zA-Z]+", d[,1])
    oceDebug(debug, "Trimming the following lines, which seem not to be data lines: ",
             paste(trimLines, collapse=" "), "\n")
    d <- d[-trimLines,]
    u <- d[, col.east] / 100
    v <- d[, col.north] / 100
    heading <- d[, col.heading]
    ## the 42.91754 value is electrical conductivity at SP=35, t=15, p=0
    conductivity <- d[, col.conductivity]
    temperature <- d[, col.temperature]
    depth <- d[, col.depth]
    pressure <- swPressure(depth, eos="gsw") # gsw is faster than unesco with essentially same results
    salinity <- d[, col.salinity]

    ## The sample file has lines at the end that contain statistical summaries. Recognize these as non-numeric samples.
    sample <- as.numeric(d[, 1])
    n <- length(u)
    time <- seq(t0, by=deltat, length.out=n)
    if (inherits(from, "POSIXt")) {
        if (!inherits(to, "POSIXt"))
            stop("if 'from' is POSIXt, then 'to' must be, also")
        if (!is.numeric(by) || by != 1)
            stop("sorry, 'by' must equal 1, in this version of read.cm.s4()")
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
    rval <- new('cm', sample=as.numeric(sample[keep]), time=time[keep],
                u=u[keep], v=v[keep], heading=heading[keep],
                conductivity=conductivity[keep],
                salinity=salinity[keep], temperature=temperature[keep], pressure=pressure[keep])
    rval@metadata$filename <- filename
    rval@metadata$serialNumber <- serialNumber
    rval@metadata$version <- version
    rval@metadata$type <- type
    rval@metadata$longitude <- longitude
    rval@metadata$latitude <- latitude
    if (missing(processingLog)) processingLog <- paste(deparse(match.call()), sep="", collapse="")
    rval@processingLog <- processingLogAppend(rval@processingLog, processingLog)
    oceDebug(debug, "} # read.cm()\n", unindent=1)
    rval
}

setMethod(f="plot",
          signature=signature("cm"),
          definition=function(x,
                              which=c(1:2, 7:9),
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
                                       pressure=7, salinity=8, temperature=9, TS=10, conductivity=11,
                                       heading=20))
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
                           xlab=resizableLabel("distance km"), ylab=resizableLabel("distance km"),
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
                      ## an older version had depth stored
                      p <- if ("pressure" %in% names(x@data)) x@data$pressure else
                          swPressure(x@data$depth, eos="gsw")
                      oce.plot.ts(x@data$time, p,
                                  type=type,
                                  xlab="", ylab=resizableLabel("p", "y"),
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
                      plotTS(as.ctd(x@data$salinity, x@data$temperature, x@data$pressure), main=main, ...) 
                  } else if (which[w] == 11) {
                      cu <- x[["conductivityUnit"]]
                      oce.plot.ts(x@data$time, x@data$conductivity,
                                  type=type,
                                  xlab="",
                                  ylab=if (cu == "ratio") resizableLabel("C", "y") else
                                      if (cu == "mS/cm") resizableLabel("conductivity mS/cm", "y") else
                                          if (cu == "S/m") resizableLabel("conductivity S/m", "y") else
                                              "conductivity (unknown unit",
                                              main=main, mgp=mgp, mar=c(mgp[1], mgp[1]+1.5, 1.5, 1.5),
                                      tformat=tformat)
                  } else if (which[w] == 20) {
                      oce.plot.ts(x@data$time, x@data$heading,
                                  type=type,
                                  xlab="", ylab=resizableLabel("heading"),
                                  main=main, mgp=mgp, mar=c(mgp[1], mgp[1]+1.5, 1.5, 1.5),
                                  tformat=tformat)
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

