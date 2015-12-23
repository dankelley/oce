setMethod(f="initialize",
          signature="adv",
          definition=function(.Object,time,v,a,q,filename) {
              if (!missing(time)) .Object@data$time <- time
              if (!missing(v)) .Object@data$v <- v
              if (!missing(a)) .Object@data$a <- a 
              if (!missing(q)) .Object@data$q <- q
              .Object@metadata$filename <- if (missing(filename)) "" else filename
              .Object@processingLog$time <- as.POSIXct(Sys.time())
              .Object@processingLog$value <- "create 'adv' object"
              return(.Object)
          })

setMethod(f="summary",
          signature="adv",
          definition=function(object, ...) {
              cat("ADV Summary\n-----------\n\n", ...)
              cat(paste("* Instrument:             ", object@metadata$instrumentType,
                        ", serial number ``", object@metadata$serialNumber, "``\n",sep=""))
              cat(paste("* Source filename:        ``", object@metadata$filename, "``\n", sep=""))
              if ("latitude" %in% names(object@metadata)) {
                  cat(paste("* Location:              ",
                            if (is.na(object@metadata$latitude)) "unknown latitude" else sprintf("%.5f N", object@metadata$latitude), ", ",
                            if (is.na(object@metadata$longitude)) "unknown longitude" else sprintf("%.5f E", object@metadata$longitude), "\n"))
              }
              cat(sprintf("* Measurements:           %s %s to %s %s sampled at %.4g Hz (on average)\n",
                          format(object@metadata$measurementStart), attr(object@metadata$measurementStart, "tzone"),
                          format(object@metadata$measurementEnd), attr(object@metadata$measurementEnd, "tzone"),
                          1 / object@metadata$measurementDeltat), ...)
              cat(sprintf("* Subsample:              %s %s to %s %s sampled at %.4g Hz (on average)\n",
                          format(object@metadata$subsampleStart), attr(object@metadata$subsampleStart, "tzone"),
                          format(object@metadata$subsampleEnd),  attr(object@metadata$subsampleEnd, "tzone"),
                          1 / object@metadata$subsampleDeltat), ...)
              if ("samplingMode" %in% names(object@metadata)) {
                  if ("burst" == object@metadata$samplingMode) {
                      cat("* Burst sampling by       ", paste(object@metadata$samplesPerBurst, sep=","), "(all, or first 4)\n")
                  } else {
                      cat("* Sampling in continuous mode\n")
                  }
              }
              cat("* Number of samples:     ", object@metadata$numberOfSamples, "\n")
              cat("* Coordinate system:     ", object@metadata$originalCoordinate, "[originally],", object@metadata$oceCoordinate, "[presently]\n")
              cat("* Orientation:           ", object@metadata$orientation, "\n")
              cat("* Frequency:             ", object@metadata$frequency, "kHz\n")
              dataNames <- names(object@data)
              nrow <- length(dataNames) - length(grep("^time", dataNames))
              threes <- matrix(nrow=nrow, ncol=3)
              ii <- 1
              for (name in dataNames) {
                  if (0 == length(grep("^time", name))) {
                      if (0 == length(object@data[[name]])) {
                          threes[ii,] <- c(NA, NA, NA)
                      } else {
                          threes[ii,] <- threenum(as.numeric(object@data[[name]]))
                      }
                      ii <- ii + 1
                  }
              }
              rownames(threes) <- paste("    ", dataNames[-grep("^time", dataNames)])
              colnames(threes) <- c("Min.", "Mean", "Max.")
              print(threes)
              processingLogShow(object)
          })

setMethod(f="[[",
          signature(x="adv", i="ANY", j="ANY"),
          definition=function(x, i, j, drop) {
              if (i == "filename") {
                  return(x@metadata$filename)
              } else if (i == "time") {
                  return(x@data$time)
              } else if (i == "timeSlow") {
                  return(x@data$timeSlow)
              } else if (i == "v") {
                  return(x@data$v)
              } else if (i == "u1") {
                  return(x@data$v[,1])
              } else if (i == "u2") {
                  return(x@data$v[,2])
              } else if (i == "u3") {
                  return(x@data$v[,3])
              #} else if (i == "heading") {
              #    if ("heading" %in% names(x@data)) return(x@data$heading)
              #    else if ("headingSlow" %in% names(x@data)) return(x@data$headingSlow)
              #    else return(NULL)
              #} else if (i == "headingSlow") {
              #    return(x@data$headingSlow)
              #} else if (i == "pitch") {
              #     if ("pitch" %in% names(x@data)) return(x@data$pitch)
              #    else if ("pitchSlow" %in% names(x@data)) return(x@data$pitchSlow)
              #    else return(NULL)
              #} else if (i == "pitchSlow") {
              #    return(x@data$pitchSlow)
              #} else if (i == "roll") {
              #    if ("roll" %in% names(x@data)) return(x@data$roll)
              #    else if ("rollSlow" %in% names(x@data)) return(x@data$rollSlow)
              #    else return(NULL)
              #} else if (i == "rollSlow") {
              #    return(x@data$rollSlow)
              #} else if (i == "temperature") {
              #    return(x@data$temperature)
              } else if (i == "a") {
                  if (!missing(j) && j == "numeric") {
                      res <- x@data$a
                      dim <- dim(res)
                      res <- as.numeric(res)
                      dim(res) <- dim
                  } else {
                      res <- x@data$a
                  }
                  return(res)
              } else if (i == "q") {
                  if (!missing(j) && j == "numeric") {
                      res <- x@data$q
                      dim <- dim(res)
                      res <- as.numeric(res)
                      dim(res) <- dim
                  } else {
                      res <- x@data$q
                  }
                  return(res)
              } else {
                  res <- as(x, "oce")[[i]]
                  ## if (missing(j) || j != "nowarn")
                  ##     warning("adv[[\"", i, "\"]]: there is no item of that name\n", call.=FALSE)
                  res
              }
          })

setMethod(f="[[<-",
          signature="adv",
          definition=function(x, i, j, value) { # FIXME: use j for e.g. times
              if (i %in% names(x@metadata)) {
                  x@metadata[[i]] <- value
              } else if (i %in% names(x@data)) {
                 x@data[[i]] <- value
              } else if (i == "heading") {
                  x@data$headingSlow <- value
              } else if (i == "pitch" || i == "pitchSlow") {
                  x@data$pitchSlow <- value
              } else if (i == "pitch" || i == "pitchSlow") {
                  x@data$rollSlow <- value
              } else {
                  stop("there is no item named \"", i, "\" in this ", class(x), " object")
              }
              ## Not checking validity because user may want to shorten items one by one, and check validity later.
              ## validObject(x)
              invisible(x)
          })


setMethod(f="subset",
          signature="adv",
          definition=function(x, subset, ...) {
              subsetString <- paste(deparse(substitute(subset)), collapse=" ")
              res <- x
              dots <- list(...)
              debug <- if (length(dots) && ("debug" %in% names(dots))) dots$debug else getOption("oceDebug")
              if (missing(subset))
                  stop("must give 'subset'")

              if (missing(subset))
                  stop("must specify a 'subset'")
              if (length(grep("time", subsetString))) {
                  oceDebug(debug, "subsetting an adv object by time\n")
                  keep <- eval(substitute(subset), x@data, parent.frame(2)) # used for $ts and $ma, but $tsSlow gets another
                  sum.keep <- sum(keep)
                  if (sum.keep < 2)
                      stop("must keep at least 2 profiles")
                  oceDebug(debug, "keeping", sum.keep, "of the", length(keep), "time slots\n")
                  oceDebug(debug, vectorShow(keep, "keeping bins:"))
                  res <- x
                  names <- names(x@data)
                  haveSlow <- "timeSlow" %in% names
                  keep <- eval(substitute(subset), x@data, parent.frame(2)) # used for $ts and $ma, but $tsSlow gets another
                  if (haveSlow) {
                      subsetStringSlow <- gsub("time", "timeSlow", subsetString)
                      keepSlow <-eval(parse(text=subsetStringSlow), x@data, parent.frame(2))
                  }
                  if ("timeBurst" %in% names) {
                      subsetStringBurst <- gsub("time", "timeBurst", subsetString)
                      keepBurst <-eval(parse(text=subsetStringBurst), x@data, parent.frame(2))
                  }
                  for (name in names(x@data)) {
                      if ("distance" == name)
                          next
                      if (length(grep("Burst$", name))) {
                          res@data[[name]] = x@data[[name]][keepBurst]
                      } else if (length(grep("^time", name)) || is.vector(res@data[[name]])) {
                          if (1 == length(agrep("Slow$", name))) {
                              oceDebug(debug, "subsetting data$", name, " (using an interpolated subset)\n", sep="")
                              res@data[[name]] <- x@data[[name]][keepSlow]
                          } else {
                              oceDebug(debug, "subsetting data$", name, "\n", sep="")
                              res@data[[name]] <- x@data[[name]][keep]
                          }
                      } else if (is.matrix(res@data[[name]])) {
                          oceDebug(debug, "subsetting data$", name, ", which is a matrix\n", sep="")
                          res@data[[name]] <- x@data[[name]][keep,]
                      } else if (is.array(res@data[[name]])) {
                          oceDebug(debug, "subsetting data$", name, ", which is an array\n", sep="")
                          res@data[[name]] <- x@data[[name]][keep,,]
                      }
                  }
              } else {
                  stop("only 'time' is permitted for subsetting")
              }
              res@metadata$numberOfSamples <- dim(res@data$v)[1]
              res@metadata$numberOfCells <- dim(res@data$v)[2]
              res@processingLog <- processingLogAppend(res@processingLog, paste("subset(x, subset=", subsetString, ")", sep=""))
              res
          })



read.adv <- function(file, from=1, to, by=1, tz=getOption("oceTz"),
                     type=c("nortek", "sontek", "sontek.adr", "sontek.text"),
                     header=TRUE,
                     longitude=NA, latitude=NA,
                     start, deltat,
                     debug=getOption("oceDebug"), monitor=FALSE, processingLog)
{
    type <- match.arg(type)
    ## FIXME: all these read.adv variants should have the same argument list
    if (type == "nortek")
        read.adv.nortek(file=file, from=from, to=to, by=by, tz=tz,
                        header=header,
                        longitude=longitude, latitude=latitude,
                        debug=debug, monitor=monitor, processingLog=processingLog)
    else if (type == "sontek") # guess
        read.adv.sontek.serial(file=file, from=from, to=to, by=by, tz=tz,
                               longitude=longitude, latitude=latitude,
                               start=start, deltat=deltat,
                               debug=debug, monitor=monitor, processingLog=processingLog)
    else if (type == "sontek.adr")
        read.adv.sontek.adr(file=file, from=from, to=to, by=by, tz=tz,
                            longitude=longitude, latitude=latitude,
                            debug=debug, processingLog=processingLog)
    else if (type == "sontek.text")
        read.adv.sontek.text(basefile=file, from=from, to=to, by=by, tz=tz,
                             longitude=longitude, latitude=latitude,
                             debug=debug, processingLog=processingLog)
    else
        stop("read.adv() cannot understand type = \"", type, "\"")
}

setMethod(f="plot",
          signature=signature("adv"),
          definition=function(x, which=c(1:3,14,15),
                              col,
                              titles,
                              type="l",
                              lwd=par('lwd'),
                              adorn=NULL,
                              drawTimeRange=getOption("oceDrawTimeRange"),
                              drawZeroLine=FALSE,
                              useSmoothScatter,
                              mgp=getOption("oceMgp"),
                              mar=c(mgp[1]+1.5,mgp[1]+1.5,1.5,1.5),
                              tformat,
                              marginsAsImage=FALSE,
                              cex=par("cex"), cex.axis=par("cex.axis"), cex.main=par("cex.main"),
                              xlim, ylim,
                              brushCorrelation, colBrush="red",
                              main="",
                              debug=getOption("oceDebug"),
                              ...)
          {
              debug <- min(4, max(0, round(debug)))
              oceDebug(debug, "plot.adv(x, which=c(", paste(which,collapse=","),"), type=\"", type, "\", ...) {\n", sep="", unindent=1)
              have.brushCorrelation <- !missing(brushCorrelation)
              oceDebug(debug, "brushCorrelation", if (have.brushCorrelation) brushCorrelation else "not given", "\n")
              oceDebug(debug, "cex=",cex," cex.axis=", cex.axis, " cex.main=", cex.main, "\n")
              oceDebug(debug, "mar=c(",paste(mar, collapse=","), ")\n")
              if (!inherits(x, "adv"))
                  stop("method is only for objects of class '", "adv", "'")
              opar <- par(no.readonly = TRUE)
              dots <- names(list(...))
              ##if (!all(which %in% c(1:3,5:7,9:11,14:21,23)))
              ##   stop("\"which\" must be in the range c(1:3,5:7,9:11,14:21,23) but it is ", which)
              nw <- length(which)
              if (nw == 1 && is.character(which)) {
                  pm <- pmatch(which, c("velocity","amplitude","quality","hydrography", "angles"))
                  if (!is.na(pm)) {
                      nbeams <- 3
                      if (pm == 1)
                          which <- 0 + seq(1, nbeams)
                      else if (pm == 2)
                          which <- 4 + seq(1, nbeams)
                      else if (pm == 3)
                          which <- 8 + seq(1, nbeams)
                      else if (pm == 4)
                          which <- 14:15
                      else if (pm == 5)
                          which <- 16:18
                      nw <- length(which)
                  }
              }
              col.per.point <- FALSE
              if (missing(col)) {
                  col <- rep("black", length.out=nw)
              } else {
                  col.per.point <- length(col) == length(x@data$time) # FIXME slow timescale here?
                  if (!col.per.point)
                      col <- rep(col, length.out=nw)
              }
              if (!missing(titles) && length(titles) != nw)
                  stop("length of 'titles' must equal length of 'which'")
              if (nw > 1)
                  on.exit(par(opar))
              par(mgp=mgp, mar=mar)
              dots <- list(...)

              ## user may specify a matrix for xlim and ylim
              gave.ylim <- !missing(ylim)
              if (gave.ylim) {
                  if (is.matrix(ylim)) {
                      if (dim(ylim)[2] != nw) {
                          ylim2 <- matrix(ylim, ncol=2, nrow=nw) # FIXME: is this what I want?
                      }
                  } else {
                      ylim2 <- matrix(ylim, ncol=2, nrow=nw) # FIXME: is this what I want?
                  }
                  class(ylim2) <- class(ylim)
                  ylim <- ylim2
              }
              gave.xlim <- !missing(xlim)
              if (gave.xlim) {
                  if (is.matrix(xlim)) {
                      if (dim(xlim)[2] != nw) {
                          xlim2 <- matrix(xlim, ncol=2, nrow=nw) # FIXME: is this what I want?
                      }
                  } else {
                      if (length(xlim) != 2)
                          stop("xlim must be a vector of length 2, or a 2-column matrix")
                      xlim2 <- matrix(xlim[1:2], ncol=2, nrow=nw, byrow=TRUE)
                  }
                  xlim <- xlim2
              }
              adorn.length <- length(adorn)
              if (adorn.length == 1) {
                  adorn <- rep(adorn, nw)
                  adorn.length <- nw
              }
              oceDebug(debug, "before layout, cex=", par('cex'), "\n")
              if (nw > 1) {
                  if (marginsAsImage) {
                      w <- 1.5
                      lay <- layout(matrix(1:(2*nw), nrow=nw, byrow=TRUE), widths=rep(c(1, lcm(w)), nw))
                  } else {
                      lay <- layout(cbind(1:nw))
                  }
              }
              ## Translate word-style (FIXME: ugly coding)
              oceDebug(debug, "before nickname-substitution, which=c(", paste(which, collapse=","), ")\n")
              which2 <- vector("numeric", nw)
              if (nw == 1 && is.character(which)) {
                  wtmp <- char.expand(which,
                                      c("velocity", "amplitude", "backscatter", "hydrography", "angles"), nomatch=NULL)
                  if (!is.na(wtmp)) {
                      if (     wtmp == "velocity"   ) which <- 1:3
                      else if (wtmp == "amplitude"  ) which <- 5:7
                      else if (wtmp == "backscatter") which <- 9:11
                      else if (wtmp == "hydrography") which <- 14:15
                      else if (wtmp == "angles"     ) which <- 16:18
                      nw <- length(which)
                  }
              }
              for (w in 1:nw) {
                  ww <- which[w]
                  if (is.numeric(ww)) {
                      which2[w] <- ww
                  } else {
                      if (     ww == "u1") which2[w] <- 1
                      else if (ww == "u2") which2[w] <- 2
                      else if (ww == "u3") which2[w] <- 3
                      ## 4 not allowed since ADV is 3-beam
                      else if (ww == "a1") which2[w] <- 5
                      else if (ww == "a2") which2[w] <- 6
                      else if (ww == "a3") which2[w] <- 7
                      ## 4 not allowed since ADV is 3-beam
                      else if (ww == "q1") which2[w] <- 9
                      else if (ww == "q2") which2[w] <- 10
                      else if (ww == "q3") which2[w] <- 11
                      ## 4 not allowed since ADV is 3-beam
                      else if (ww == "salinity") which2[w] <- 13
                      else if (ww == "temperature") which2[w] <- 14
                      else if (ww == "pressure") which2[w] <- 15
                      else if (ww == "heading") which2[w] <- 16
                      else if (ww == "pitch") which2[w] <- 17
                      else if (ww == "roll") which2[w] <- 18
                      ## 19 beam-1 correlation-amplitude diagnostic plot
                      ## 20 beam-2 correlation-amplitude diagnostic plot
                      ## 21 beam-3 correlation-amplitude diagnostic plot
                      ## 22 not allowed, since ADVs have only 3 beams
                      else if (ww == "progressive vector") which2[w] <- 23
                      else if (ww == "uv") which2[w] <- 28
                      else if (ww == "uv+ellipse") which2[w] <- 29
                      else if (ww == "uv+ellipse+arrow") which2[w] <- 30
                      else if (ww == "analog1") which2[w] <- 50
                      else if (ww == "analog2") which2[w] <- 51
                      else if (ww == "voltage") which2[w] <- 100
                      else stop("unknown 'which':", ww)
                  }
              }
              which <- which2
              oceDebug(debug, "after nickname-substitution, which=c(", paste(which, collapse=","), ")\n")
              oceDebug(debug, "after layout, cex=", par('cex'), "\n")
              ## FIXME below here, was using tsSlow
              tlim <- range(x@data$time, na.rm=TRUE)
              for (w in 1:nw) {
                  if (w > 1)
                      main <- ""
                  oceDebug(debug, "plotting which[", w, "]=", which[w], "\n")
                  par(mgp=mgp, mar=mar)
                  if (which[w] %in% 1:3) {        # u1, u2, u3
                      y <- as.numeric(x@data$v[,which[w]])
                      if (have.brushCorrelation && type == "p") {
                          good <- as.numeric(x@data$q[,which[w]]) >= brushCorrelation
                          oce.plot.ts(x@data$time[good], y[good], ylab=beamName(x, which[w]),
                                      drawTimeRange=drawTimeRange,
                                      adorn=adorn[w],
                                      xlim=if (gave.xlim) xlim[w,] else tlim,
                                      ylim=if (gave.ylim) ylim[w,] else range(y, na.rm=TRUE),
                                      type=type,
                                      cex=cex, cex.axis=cex.axis, cex.main=cex.main,
                                      mgp=mgp,
                                      mar=c(mgp[1], mgp[1]+1.5, 1.5, 1.5),
                                      lwd=lwd[w], col=col[w],
                                      main=main,
                                      tformat=tformat,
                                      debug=debug-1,
                                      ...)
                          points(x@data$time[!good], x@data$v[!good,which[w]], col=colBrush, ...)
                      } else {
                          oce.plot.ts(x@data$time, y, ylab=beamName(x, which[w]),
                                      drawTimeRange=drawTimeRange,
                                      adorn=adorn[w],
                                      xlim=if (gave.xlim) xlim[w,] else tlim,
                                      ylim=if (gave.ylim) ylim[w,] else range(y, na.rm=TRUE),
                                      type=type,
                                      cex=cex, cex.axis=cex.axis, cex.main=cex.main,
                                      mgp=mgp,
                                      mar=c(mgp[1], mgp[1]+1.5, 1.5, 1.5),
                                      lwd=lwd[w], col=col[w],
                                      main=main,
                                      tformat=tformat,
                                      debug=debug-1)
                      }
                      if (drawZeroLine)
                          abline(h=0)
                      rm(y)                       # space may be tight
                  } else if (which[w] %in% 5:7) { # a1, a2, a3
                      ## FIXME/DRY: alter a1,a2,a3 if alter q1,q2,q3, since both almost the same
                      oceDebug(debug, "plotting a1, a2, or a3 since which[w] == ", which[w], "\n")
                      y <- as.numeric(x@data$a[,which[w]-4])
                      oceDebug(debug, "range(y):", paste(range(y, na.rm=TRUE), sep="-"), "\n")
                      if (have.brushCorrelation && type == "p") {
                          good <- as.numeric(x@data$q[,which[w]-4]) >= brushCorrelation
                          oce.plot.ts(x@data$time[good], y[good],
                                      ylab=c(expression(a[1]),expression(a[2]),expression(a[3]),expression(a[4]))[which[w]-4],
                                      drawTimeRange=drawTimeRange,
                                      adorn=adorn[w],
                                      xlim=if (gave.xlim) xlim[w,] else tlim,
                                      ylim=if (gave.ylim) ylim[w,] else range(y, na.rm=TRUE),
                                      type=type,
                                      cex=cex, cex.axis=cex.axis, cex.main=cex.main,
                                      mgp=mgp, mar=c(mgp[1], mgp[1]+1.5, 1.5, 1.5),
                                      lwd=lwd[w], col=col[w],
                                      main=main,
                                      tformat=tformat,
                                      debug=debug-1)
                          points(x@data$time[!good], y[!good], col=colBrush)
                      } else {
                          oce.plot.ts(x@data$time, y,
                                      ylab=c(expression(a[1]),expression(a[2]),expression(a[3]),expression(a[4]))[which[w]-4],
                                      drawTimeRange=drawTimeRange,
                                      adorn=adorn[w],
                                      xlim=if (gave.xlim) xlim[w,] else tlim,
                                      ylim=if (gave.ylim) ylim[w,] else range(y, na.rm=TRUE),
                                      type=type,
                                      cex=cex, cex.axis=cex.axis, cex.main=cex.main,
                                      mgp=mgp, mar=c(mgp[1], mgp[1]+1.5, 1.5, 1.5),
                                      lwd=lwd[w], col=col[w],
                                      main=main,
                                      tformat=tformat,
                                      debug=debug-1)
                      }
                      rm(y)                       # space may be tight
                  } else if (which[w] %in% 9:11) { # q1, q2, q3 (named c1, c2, and c3 in the object)
                      y <- as.numeric(x@data$q[,which[w]-8])
                      if (have.brushCorrelation && type == "p") {
                          good <- as.numeric(x@data$q[,which[w]-8]) >= brushCorrelation
                          oce.plot.ts(x@data$time[good], y[good],
                                      ylab=c(expression(q[1]),expression(q[2]),expression(q[3]),expression(q[4]))[which[w]-8],
                                      drawTimeRange=drawTimeRange,
                                      adorn=adorn[w],
                                      xlim=if (gave.xlim) xlim[w,] else tlim,
                                      ylim=if (gave.ylim) ylim[w,] else range(y, na.rm=TRUE),
                                      type=type,
                                      cex=cex, cex.axis=cex.axis, cex.main=cex.main,
                                      mgp=mgp, mar=c(mgp[1], mgp[1]+1.5, 1.5, 1.5),
                                      lwd=lwd[w], col=col[w],
                                      main=main,
                                      tformat=tformat,
                                      debug=debug-1)
                          points(x@data$time[!good], y[!good], col=colBrush)
                      } else {
                          oce.plot.ts(x@data$time, y,
                                      ylab=c(expression(q[1]),expression(q[2]),expression(q[3]),expression(q[4]))[which[w]-8],
                                      drawTimeRange=drawTimeRange,
                                      adorn=adorn[w],
                                      xlim=if (gave.xlim) xlim[w,] else tlim,
                                      ylim=if (gave.ylim) ylim[w,] else range(y, na.rm=TRUE),
                                      type=type,
                                      cex=cex, cex.axis=cex.axis, cex.main=cex.main,
                                      mgp=mgp, mar=c(mgp[1], mgp[1]+1.5, 1.5, 1.5),
                                      lwd=lwd[w], col=col[w],
                                      tformat=tformat,
                                      debug=debug-1)
                      }
                      rm(y)                       # space may be tight
                  } else if (which[w] == 13 || which[w] == "salinity") {
                      if ("salinity" %in% names(x@metadata)) {
                          if ("timeSlow" %in% names(x@data)) {
                              salinity <- rep(x@metadata$salinity, length(x@data$temperatureSlow))
                              oce.plot.ts(x@data$timeSlow, salinity, ylab=resizableLabel("S", "y"),
                                          drawTimeRange=drawTimeRange,
                                          adorn=adorn[w],
                                          xlim=if (gave.xlim) xlim[w,] else tlim,
                                          ylim=if (gave.ylim) ylim[w,] else x@metadata$salinity+c(0.5, -0.5),
                                          type=type,
                                          cex=cex, cex.axis=cex.axis, cex.main=cex.main,
                                          mgp=mgp, mar=c(mgp[1], mgp[1]+1.5, 1.5, 1.5),
                                          lwd=lwd[w], col=if(col.per.point) col else col[w],
                                          main=main,
                                          tformat=tformat,
                                          debug=debug-1)
                          } else {
                              salinity <- rep(x@metadata$salinity, length(x@data$temperature))
                              oce.plot.ts(x@data$time, salinity, ylab=resizableLabel("S", "y"),
                                          drawTimeRange=drawTimeRange,
                                          adorn=adorn[w],
                                          xlim=if (gave.xlim) xlim[w,] else tlim,
                                          ylim=if (gave.ylim) ylim[w,] else x@metadata$salinity+c(0.5, -0.5),
                                          type=type,
                                          cex=cex, cex.axis=cex.axis, cex.main=cex.main,
                                          mgp=mgp, mar=c(mgp[1], mgp[1]+1.5, 1.5, 1.5),
                                          lwd=lwd[w], col=if(col.per.point) col else col[w],
                                          main=main,
                                          tformat=tformat,
                                          debug=debug-1)
                          }
                      } else {
                          warning("no salinity in this ADV object")
                      }
                  } else if (which[w] == 14 || which[w] == "temperature") {
                      if ("timeSlow" %in% names(x@data) && "temperatureSlow" %in% names(x@data)) {
                          oce.plot.ts(x@data$timeSlow, x@data$temperatureSlow, ylab=resizableLabel("T", "y"),
                                      drawTimeRange=drawTimeRange,
                                      adorn=adorn[w],
                                      xlim=if (gave.xlim) xlim[w,] else tlim,
                                      ylim=if (gave.ylim) ylim[w,] else range(x@data$temperature, na.rm=TRUE),
                                      type=type,
                                      cex=cex, cex.axis=cex.axis, cex.main=cex.main,
                                      mgp=mgp, mar=c(mgp[1], mgp[1]+1.5, 1.5, 1.5),
                                      lwd=lwd[w], col=if(col.per.point) col else col[w],
                                      main=main,
                                      tformat=tformat,
                                      debug=debug-1)
                      } else {
                          oce.plot.ts(x@data$time, x@data$temperature, ylab=resizableLabel("T", "y"),
                                      drawTimeRange=drawTimeRange,
                                      adorn=adorn[w],
                                      xlim=if (gave.xlim) xlim[w,] else tlim,
                                      ylim=if (gave.ylim) ylim[w,] else range(x@data$temperature, na.rm=TRUE),
                                      type=type,
                                      cex=cex, cex.axis=cex.axis, cex.main=cex.main,
                                      mgp=mgp, mar=c(mgp[1], mgp[1]+1.5, 1.5, 1.5),
                                      lwd=lwd[w], col=if(col.per.point) col else col[w],
                                      main=main,
                                      tformat=tformat,
                                      debug=debug-1)
                      }
                  } else if (which[w] == 15 || which[w] == "pressure") {
                      oce.plot.ts(x@data$time, x@data$pressure, ylab=resizableLabel("p", "y"),
                                  drawTimeRange=drawTimeRange,
                                  adorn=adorn[w],
                                  xlim=if (gave.xlim) xlim[w,] else tlim,
                                  ylim=if (gave.ylim) ylim[w,] else range(x@data$pressure, na.rm=TRUE),
                                  type=type,
                                  cex=cex, cex.axis=cex.axis, cex.main=cex.main,
                                  mgp=mgp, mar=c(mgp[1], mgp[1]+1.5, 1.5, 1.5),
                                  lwd=lwd[w], col=if(col.per.point) col else col[w],
                                  main=main,
                                  tformat=tformat,
                                  debug=debug-1)
                  } else if (which[w] == 16 || which[w] == "heading") {
                      if ("timeSlow" %in% names(x@data) && "headingSlow" %in% names(x@data)) {
                          oce.plot.ts(x@data$timeSlow, x@data$headingSlow, ylab="heading",
                                      drawTimeRange=drawTimeRange,
                                      adorn=adorn[w],
                                      xlim=if (gave.xlim) xlim[w,] else tlim,
                                      ylim=if (gave.ylim) ylim[w,] else range(x@data$heading, na.rm=TRUE),
                                      type=type,
                                      cex=cex, cex.axis=cex.axis, cex.main=cex.main,
                                      mgp=mgp, mar=c(mgp[1], mgp[1]+1.5, 1.5, 1.5),
                                      lwd=lwd[w], col=if(col.per.point) col else col[w],
                                      main=main,
                                      tformat=tformat,
                                      debug=debug-1)
                      } else {
                          oce.plot.ts(x@data$time, x@data$heading, ylab="heading",
                                      drawTimeRange=drawTimeRange,
                                      adorn=adorn[w],
                                      xlim=if (gave.xlim) xlim[w,] else tlim,
                                      ylim=if (gave.ylim) ylim[w,] else range(x@data$heading, na.rm=TRUE),
                                      type=type,
                                      cex=cex, cex.axis=cex.axis, cex.main=cex.main,
                                      mgp=mgp, mar=c(mgp[1], mgp[1]+1.5, 1.5, 1.5),
                                      lwd=lwd[w], col=if(col.per.point) col else col[w],
                                      main=main,
                                      tformat=tformat,
                                      debug=debug-1)
                      }
                  } else if (which[w] == 17 || which[w] == "pitch") {    # pitch
                      if ("timeSlow" %in% names(x@data) && "pitchSlow" %in% names(x@data)) {
                          oce.plot.ts(x@data$timeSlow, x@data$pitchSlow, ylab="pitch",
                                      drawTimeRange=drawTimeRange,
                                      adorn=adorn[w],
                                      xlim=if (gave.xlim) xlim[w,] else tlim,
                                      ylim=if (gave.ylim) ylim[w,] else range(x@data$pitch, na.rm=TRUE),
                                      type=type,
                                      cex=cex, cex.axis=cex.axis, cex.main=cex.main,
                                      mgp=mgp, mar=c(mgp[1], mgp[1]+1.5, 1.5, 1.5),
                                      lwd=lwd[w], col=if(col.per.point) col else col[w],
                                      main=main,
                                      tformat=tformat,
                                      debug=debug-1)
                      } else {
                          oce.plot.ts(x@data$time, x@data$pitch, ylab="pitch",
                                      drawTimeRange=drawTimeRange,
                                      adorn=adorn[w],
                                      xlim=if (gave.xlim) xlim[w,] else tlim,
                                      ylim=if (gave.ylim) ylim[w,] else range(x@data$pitch, na.rm=TRUE),
                                      type=type,
                                      cex=cex, cex.axis=cex.axis, cex.main=cex.main,
                                      mgp=mgp, mar=c(mgp[1], mgp[1]+1.5, 1.5, 1.5),
                                      lwd=lwd[w], col=if(col.per.point) col else col[w],
                                      main=main,
                                      tformat=tformat,
                                      debug=debug-1)
                      }
                  } else if (which[w] == 18 || which[w] == "roll") {
                      if ("timeSlow" %in% names(x@data) && "rollSlow" %in% names(x@data)) {
                          oce.plot.ts(x@data$timeSlow, x@data$rollSlow, ylab="roll",
                                      drawTimeRange=drawTimeRange,
                                      adorn=adorn[w],
                                      xlim=if (gave.xlim) xlim[w,] else tlim,
                                      ylim=if (gave.ylim) ylim[w,] else range(x@data$roll, na.rm=TRUE),
                                      type=type,
                                      cex=cex, cex.axis=cex.axis, cex.main=cex.main,
                                      mgp=mgp, mar=c(mgp[1], mgp[1]+1.5, 1.5, 1.5),
                                      lwd=lwd[w], col=col[w],
                                      main=main,
                                      tformat=tformat,
                                      debug=debug-1)
                      } else {
                          oce.plot.ts(x@data$time, x@data$roll, ylab="roll",
                                      drawTimeRange=drawTimeRange,
                                      adorn=adorn[w],
                                      xlim=if (gave.xlim) xlim[w,] else tlim,
                                      ylim=if (gave.ylim) ylim[w,] else range(x@data$roll, na.rm=TRUE),
                                      type=type,
                                      cex=cex, cex.axis=cex.axis, cex.main=cex.main,
                                      mgp=mgp, mar=c(mgp[1], mgp[1]+1.5, 1.5, 1.5),
                                      lwd=lwd[w], col=col[w],
                                      main=main,
                                      tformat=tformat,
                                      debug=debug-1)
                      }
                      ## FIXME: should plot.adv() be passing mar, cex, etc to smoothScatter?
                  } else if (which[w] == 19) {    # beam 1 correlation-amplitude diagnostic plot
                      a <- as.numeric(x@data$a[,1])
                      q <- as.numeric(x@data$q[,1])
                      n <- length(a)
                      if (n < 2000 || (!missing(useSmoothScatter) && !useSmoothScatter)) {
                          plot(a, c,
                               xlab=gettext("Amplitude", domain="R-oce"),
                               ylab=gettext("Correlation", domain="R-oce"),
                               xlim=if (gave.xlim) xlim[w,] else range(a),
                               ylim=if (gave.ylim) ylim[w,] else range(c),
                               main=main,
                               debug=debug-1)
                      } else {
                          smoothScatter(a, c, nbin=64,
                                        xlab=gettext("Amplitude", domain="R-oce"),
                                        ylab=gettext("Correlation", domain="R-oce"),
                                        xlim=if (gave.xlim) xlim[w,] else range(a),
                                        ylim=if (gave.ylim) ylim[w,] else range(c),
                                        main=main,
                                        debug=debug-1)
                      }
                      mtext("beam 1")
                  } else if (which[w] == 20) {    # beam 2 correlation-amplitude diagnostic plot
                      a <- as.numeric(x@data$a[,2])
                      q <- as.numeric(x@data$q[,2])
                      n <- length(a)
                      if (n < 2000 || (!missing(useSmoothScatter) && !useSmoothScatter)) {
                          plot(a, c,
                               xlab=gettext("Amplitude", domain="R-oce"),
                               ylab=gettext("Correlation", domain="R-oce"),
                               xlim=if (gave.xlim) xlim[w,] else range(a),
                               ylim=if (gave.ylim) ylim[w,] else range(c),
                               main=main,
                               debug=debug-1)
                      } else {
                          smoothScatter(a, c, nbin=64,
                                        xlab=gettext("Amplitude", domain="R-oce"),
                                        ylab=gettext("Correlation", domain="R-oce"),
                                        xlim=if (gave.xlim) xlim[w,] else range(a),
                                        ylim=if (gave.ylim) ylim[w,] else range(c),
                                        main=main,
                                        debug=debug-1)
                      }
                      mtext("beam 2")
                  } else if (which[w] == 21) {    # beam 3 correlation-amplitude diagnostic plot
                      a <- as.numeric(x@data$a[,3])
                      q <- as.numeric(x@data$q[,3])
                      n <- length(a)
                      if (n < 2000 || (!missing(useSmoothScatter) && !useSmoothScatter)) {
                          plot(a, c,
                               xlab=gettext("Amplitude", domain="R-oce"),
                               ylab=gettext("Correlation", domain="R-oce"),
                               xlim=if (gave.xlim) xlim[w,] else range(a),
                               ylim=if (gave.ylim) ylim[w,] else range(c),
                               main=main)
                      } else {
                          smoothScatter(a, c, nbin=64,
                                        xlab=gettext("Amplitude", domain="R-oce"),
                                        ylab=gettext("Correlation", domain="R-oce"),
                                        xlim=if (gave.xlim) xlim[w,] else range(a),
                                        ylim=if (gave.ylim) ylim[w,] else range(c),
                                        main=main)
                      }
                      mtext("beam 3")
                  } else if (which[w] == 23 || which[w] == "progressive vector") {    # progressive vector
                      par(mar=c(mgp[1]+1,mgp[1]+1,1,1))
                      m.per.km <- 1000
                      u <- x@data$v[,1]
                      v <- x@data$v[,2]
                      u[is.na(u)] <- 0        # zero out missing
                      v[is.na(v)] <- 0
                      xDist <- integrateTrapezoid(x@data$time, u, 'cA') / m.per.km
                      yDist<- integrateTrapezoid(x@data$time, v, 'cA') / m.per.km
                      plot(xDist, yDist, xlab="km", ylab="km", type=type,
                           cex=cex, cex.axis=cex.axis, cex.main=cex.main,
                           asp=1, lwd=lwd[w], col=col[w], ...)
                      if (main[w] != "")
                          mtext(main[w], adj=1)
                  } else if (which[w] %in% 28:31) {
                      oceDebug(debug, "doing horizontal-velocity diagram\n")
                      par(mar=c(mgp[1]+1,mgp[1]+1,1,1))
                      n <- length(x@data$time)
                      if (n < 2000 || (!missing(useSmoothScatter) && !useSmoothScatter)) {
                          plot(x@data$v[,1], x@data$v[,2],
                               xlab=resizableLabel("u"),
                               ylab=resizableLabel("v"),
                               type=type,
                               cex=cex, cex.axis=cex.axis, cex.main=cex.main, asp=1,
                               xlim=if(gave.xlim)xlim, ylim=if(gave.ylim) ylim,
                               lwd=lwd[w], col=col[w], main=main, ...)
                      } else {
                          smoothScatter(x@data$v[,1], x@data$v[,2],
                                        xlab=resizableLabel("u"),
                                        ylab=resizableLabel("v"),
                                        cex=cex, cex.axis=cex.axis, cex.main=cex.main,
                                        asp=1, xlim=xlim, ylim=ylim, ...)
                      }
                      if (which[w] >= 29) {
                          ok <- !is.na(x@data$v[,1]) & !is.na(x@data$v[,2])
                          e <- eigen(cov(data.frame(u=x@data$v[ok,1], v=x@data$v[ok,2])))
                          major <- sqrt(e$values[1])
                          minor <- sqrt(e$values[2])
                          theta <- seq(0, 2*pi, length.out=360/5)
                          xx <- major * cos(theta)
                          yy <- minor * sin(theta)
                          theta0 <- atan2(e$vectors[2,1], e$vectors[1,1])
                          rotate <- matrix(c(cos(theta0), -sin(theta0), sin(theta0), cos(theta0)), nrow=2, byrow=TRUE)
                          xxyy <- rotate %*% rbind(xx, yy)
                          lines(xxyy[1,], xxyy[2,], lwd=5, col="yellow")
                          lines(xxyy[1,], xxyy[2,], lwd=2, col="darkblue")
                          if (which[w] >= 30) {
                              umean <- mean(x@data$v[,1], na.rm=TRUE)
                              vmean <- mean(x@data$v[,2], na.rm=TRUE)
                              arrows(0, 0, umean, vmean, lwd=5, length=1/10, col="yellow")
                              arrows(0, 0, umean, vmean, lwd=2, length=1/10, col=col)
                          }
                          if (main[w] != "")
                              mtext(main[w], adj=1)
                      }
                  } else if (which[w] == 50 || which[w] == "analog1") {
                      if ("analog1" %in% names(x@data)) {
                          oce.plot.ts(x@data$time, x@data$analog1, ylab="Analog 1",
                                      tformat=tformat,
                                      debug=debug-1)
                      } else {
                          warning("there is no analog1 signal in this ADV object")
                      }
                  } else if (which[w] == 51 || which[w] == "analog2") {
                      if ("analog2" %in% names(x@data)) {
                          oce.plot.ts(x@data$time, x@data$analog2, ylab="Analog 2",
                                      tformat=tformat,
                                      debug=debug-1)
                      } else {
                          warning("there is no analog2 signal in this ADV object")
                      }
                  } else if (which[w] == 100 || which[w] == "voltage") {
                      if ("voltageSlow" %in% names(x@data))
                          oce.plot.ts(x@data$timeSlow, x@data$voltageSlow, ylab="Voltage",
                                      tformat=tformat,
                                      debug=debug-1)
                      else
                          warning("no voltage signal to plot")
                  } else {
                      stop("unknown value of \"which\":", which[w])
                  }
                  drawTimeRange <- FALSE
                  if (marginsAsImage)  {
                      ## blank plot, to get axis length same as for images
                      omar <- par("mar")
                      par(mar=c(mar[1], 1/4, mgp[2]+1/2, mgp[2]+1))
                      plot(1:2, 1:2, type='n', axes=FALSE, xlab="", ylab="")
                      par(mar=omar)
                  }
              }
              oceDebug(debug, "} # plot.adv()\n", unindent=1)
              invisible()
          })

toEnuAdv <- function(x, declination=0, debug=getOption("oceDebug"))
{
    oceDebug(debug, "adv.2enu() {\n", unindent=1)
    coord <- x@metadata$oceCoordinate
    if (coord == "beam") {
        x <- xyzToEnuAdv(beamToXyzAdv(x, debug=debug-1), declination=declination, debug=debug-1)
    } else if (coord == "xyz") {
        x <- xyzToEnuAdv(x, declination=declination, debug=debug-1)
    } else if (coord == "enu") {
        ;
    } else {
        warning("adv.2enu cannot convert from coordinate system ", coord, " to ENU, so returning argument as-is")
    }
    oceDebug(debug, "} # adv.2enu()\n", unindent=1)
    x
}

beamToXyzAdv <- function(x, debug=getOption("oceDebug"))
{
    oceDebug(debug, "beamToXyzAdv() {\n", unindent=1)
    if (!inherits(x, "adv"))
        stop("method is only for objects of class '", "adv", "'")
    if (x@metadata$oceCoordinate != "beam")
        stop("input must be in beam coordinates, but it is in ", x@metadata$oceCoordinate, " coordinates")
    if (is.null(x@metadata$transformationMatrix)) {
        cat("How to add a transformation matrix to a velocimeter record named 'x':
            x@metadata$transformationMatrix <- rbind(c(11100, -5771,  -5321),
                                                     c(  291,  9716, -10002),
                                                     c( 1409,  1409,   1409)) / 4096")
        stop("cannot convert coordinates because metadata$transformationMatrix is NULL (see above).")
    }
    tm <- x@metadata$transformationMatrix
    oceDebug(debug, "Transformation matrix:\n")
    oceDebug(debug, sprintf("%.10f %.10f %.10f\n", tm[1,1], tm[1,2], tm[1,3]))
    oceDebug(debug, sprintf("%.10f %.10f %.10f\n", tm[2,1], tm[2,2], tm[2,3]))
    oceDebug(debug, sprintf("%.10f %.10f %.10f\n", tm[3,1], tm[3,2], tm[3,3]))
    ## Not using the matrix method because it might consume more memory, and
    ## measures no faster xyz <- tm %*% rbind(x@data$v[,1], x@data$v[,2],
    ## x@data$v[,3])
    u <- tm[1,1] * x@data$v[,1] + tm[1,2] * x@data$v[,2] + tm[1,3] * x@data$v[,3]
    v <- tm[2,1] * x@data$v[,1] + tm[2,2] * x@data$v[,2] + tm[2,3] * x@data$v[,3]
    w <- tm[3,1] * x@data$v[,1] + tm[3,2] * x@data$v[,2] + tm[3,3] * x@data$v[,3]
    x@data$v[,1] <- u
    x@data$v[,2] <- v
    x@data$v[,3] <- w
    x@metadata$oceCoordinate <- "xyz"
    x@processingLog <- processingLogAppend(x@processingLog, paste(deparse(match.call()), sep="", collapse=""))
    oceDebug(debug, "} # beamToXyzAdv()\n", unindent=1)
    x
}


xyzToEnuAdv <- function(x, declination=0,
                        cabled=FALSE, horizontalCase, sensorOrientation,
                        debug=getOption("oceDebug"))
{
    oceDebug(debug, "xyzToEnuAdv(x, declination=", declination,
              ",cabled=",cabled,
              ",horizontalCase=",if (missing(horizontalCase)) "(not provided)" else horizontalCase,
              ",sensorOrientation=",if (missing(sensorOrientation)) "(not provided)" else sensorOrientation,
              ",debug) {\n", unindent=1)
    if (!inherits(x, "adv"))
        stop("method is only for objects of class '", "adv", "'")
    if (x@metadata$oceCoordinate != "xyz")
        stop("input must be in xyz coordinates, but it is in ", x@metadata$oceCoordinate, " coordinates")
    if ("ts" %in% names(x@data) || "ma" %in% names(x@data))
        stop("cannot handle ADV objects that were created with oce version < 0.3")
    haveSlow <- "timeSlow" %in% names(x@data)
    if (haveSlow) {
        oceDebug(debug, "interpolating slowly-varying heading, pitch, and roll to the quickly-varying velocity times\n")
        t0 <- as.numeric(x@data$timeSlow[1])    # arbitrary; done in case approx hates large x values
        tFast <- as.numeric(x@data$time) - t0
        tSlow <- as.numeric(x@data$timeSlow) - t0
        heading <- approx(tSlow, x@data$headingSlow, xout=tFast)$y
        pitch <- approx(tSlow, x@data$pitchSlow, xout=tFast)$y
        roll <- approx(tSlow, x@data$rollSlow, xout=tFast)$y
    } else {
        heading <- x@data$heading
        pitch <- x@data$pitch
        roll <- x@data$roll
    }
    haveSteadyAngles <- length(heading) == 1 && length(pitch) == 1 && length(roll) == 1
    oceDebug(debug, "haveSteadyAngles=",haveSteadyAngles,"\n")
    # FIXME: haveTsSlow necessary here
    oceDebug(debug, "adv data does not have data ts slow; time-series data are data\n")
    ## Adjust various things, so that the xyz-to-enu formulae (based on RDI) will work
    ##
    ## The various cases are defined by help(xyzToEnuAdv).
    if (missing(sensorOrientation))
        sensorOrientation  <- x@metadata$orientation
    if (1 == length(agrep("nortek", x@metadata$manufacturer))) {
        if (!cabled) {
            if (sensorOrientation == "upward") {
                oceDebug(debug, "Case 1: Nortek vector velocimeter with upward-pointing sensor attached directly to pressure case.\n")
                oceDebug(debug, "        Using heading=heading=90, pitch=roll, roll=-pitch, S=X, F=-Y, and M=-Z.\n")
                heading <- heading - 90
                tmp <- pitch
                pitch <- roll
                roll <- -tmp
                starboard <- x@data$v[,1]
                forward <- -x@data$v[,2]
                mast <- -x@data$v[,3]
            } else if (sensorOrientation == "downward") {
                oceDebug(debug, "Case 2: Nortek vector velocimeter with downward-pointing sensor attached directly to pressure case.\n")
                oceDebug(debug, "        Using heading=heading=90, pitch=roll, roll=-pitch, S=X, F=Y, and M=Z.\n")
                heading <- heading - 90
                tmp <- pitch
                pitch <- roll
                roll <- -tmp
                starboard <- x@data$v[,1]
                forward <- x@data$v[,2]
                mast <- x@data$v[,3]
            } else {
                stop("need sensor orientation to be 'upward' or 'downward', not '", sensorOrientation,"'")
            }
        } else {
            ## vector cabelled: cases 3 to 6
            if (missing(horizontalCase))
                stop("must give horizontalCase for cabled Nortek Vector (cases 3 to 6)")
            if (!is.logical(horizontalCase))
                stop("must give horizontalCase as TRUE or FALSE")
            if (horizontalCase) {
                if (sensorOrientation == "upward") {
                    oceDebug(debug, "Case 3: Nortek vector velocimeter with upward-pointing sensor, cabled to a horizontal pressure case.\n")
                    oceDebug(debug, "        Using heading=heading=90, pitch=roll, roll=-pitch, S=X, F=Y, and M=Z.\n")
                    heading <- heading - 90
                    tmp <- pitch
                    pitch <- roll
                    roll <- -tmp
                    starboard <- x@data$v[,1]
                    forward <- x@data$v[,2]
                    mast <- x@data$v[,3]
                } else if (sensorOrientation == "downward") {
                    oceDebug(debug, "Case 4: Nortek vector velocimeter with downward-pointing sensor, cabled to a horizontal pressure case.\n")
                    oceDebug(debug, "        Using heading=heading=90, pitch=roll, roll=pitch, S=X, F=-Y, and M=-Z.\n")
                    heading <- heading - 90
                    tmp <- pitch
                    pitch <- roll
                    roll <- tmp
                    starboard <- x@data$v[,1]
                    forward <- x@data$v[,2]
                    mast <- x@data$v[,3]
                } else {
                    stop("need sensor orientation to be 'upward' or 'downward', not '", sensorOrientation,"'")
                }
            } else {
                stop("cannot handle cases 5 and 6 (vector velocimeter cabled to a vertical case)")
            }
        }
    } else if (1 == length(agrep("sontek", x@metadata$manufacturer))) {
        if (cabled)
            stop("cannot handle the case of a cabled Sontek unit (does it even exist?)")
        if (sensorOrientation == "upward") {
            oceDebug(debug, "Case 7: Sontek ADV velocimeter with upward-pointing sensor.\n")
            oceDebug(debug, "        Using heading=heading=90, pitch=roll, roll=-pitch, S=X, F=-Y, and M=-Z.\n")
            heading <- heading - 90
            tmp <- pitch
            pitch <- roll
            roll <- -tmp
            starboard <- x@data$v[,1]
            forward <- -x@data$v[,2]
            mast <- -x@data$v[,3]
        } else if (sensorOrientation == "downward") {
            oceDebug(debug, "Case 8: Sontek ADV velocimeter with downward-pointing sensor.\n")
            oceDebug(debug, "        Using heading=heading=90, pitch=roll, roll=-pitch, S=X, F=Y, and M=Z.\n")
            heading <- heading - 90
            tmp <- pitch
            pitch <- roll
            roll <- -tmp
            starboard <- x@data$v[,1]
            forward <- x@data$v[,2]
            mast <- x@data$v[,3]
        } else {
            stop("need metadata$orientation='upward' or 'downward', not '",x@metadata$orientation,"'")
        }
    } else {
        stop("unknown type of instrument; x@metadata$manufacturer must contain either \"sontek\" or \"nortek\"")
    }
    np <- dim(x@data$v)[1]
    if (np != length(heading))
        stop("heading length (", length(heading), ") does not match number of velocity samples (", np, ")") 
    enu <- .C("sfm_enu",
              as.integer(length(heading)), # need not equal np
              as.double(heading + declination),
              as.double(pitch),
              as.double(roll),
              as.integer(np),
              as.double(starboard),
              as.double(forward),
              as.double(mast),
              east = double(np),
              north = double(np),
              up = double(np),
              NAOK=TRUE,
              PACKAGE="oce")
    x@data$v[,1] <- enu$east
    x@data$v[,2] <- enu$north
    x@data$v[,3] <- enu$up
    x@metadata$oceCoordinate <- "enu"
    x@processingLog <- processingLogAppend(x@processingLog,
                                           paste("xyzToEnu(x",
                                                 ", declination=", declination, 
                                                 ", horizontalCase=", if (missing(horizontalCase)) "(missing)" else horizontalCase,
                                                 ", sensorOrientiation=", if (missing(sensorOrientation)) "(missing)" else sensorOrientation,
                                                 ", debug=", debug, ")", sep=""))
    oceDebug(debug, "} # xyzToEnuAdv()\n", unindent=1)
    x
}

enuToOtherAdv <- function(x, heading=0, pitch=0, roll=0, debug=getOption("oceDebug"))
{
    if (!inherits(x, "adv"))
        stop("method is only for objects of class '", "adv", "'")
    if (x@metadata$oceCoordinate != "enu")
        stop("input must be in \"enu\" coordinates, but it is in ", x@metadata$oceCoordinate, " coordinates")
    oceDebug(debug, "enuToOtherAdv(x, heading=", heading, ", pitch=", 
             pitch, ", roll=", roll, ", debug=", debug, ")", unindent=1)
    np <- dim(x@data$v)[1]
    other <- .C("sfm_enu",
              as.integer(length(heading)), # need not equal np
              as.double(heading),
              as.double(pitch),
              as.double(roll),
              as.integer(np),
              as.double(x@data$v[,1]),
              as.double(x@data$v[,2]),
              as.double(x@data$v[,3]),
              v1new = double(np),
              v2new = double(np),
              v3new = double(np),
              NAOK=TRUE,
              PACKAGE="oce")
    x@data$v[,1] <- other$v1new
    x@data$v[,2] <- other$v2new
    x@data$v[,3] <- other$v3new
    x@metadata$oceCoordinate <- "other"
    x@processingLog <- processingLogAppend(x@processingLog, paste(deparse(match.call()), sep="", collapse=""))
    oceDebug(debug, "} # enuToOtherAdv()\n", unindent=1)
    x
}
