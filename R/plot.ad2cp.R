#- # A helpful function.  I seem to do this kind of thing quite a lot, and so I may
#- # document this function later, and add it to the NAMESPACE.
#- makeNumeric <- function(x)
#- {
#-     if (is.numeric(x))
#-         return(x)
#-     if (is.vector(x))
#-         return(as.numeric(x))
#-     if (is.array(x)) {
#-         dim <- dim(x)
#-         rval <- as.numeric(x)
#-         dim(rval) <- dim
#-         return(rval)
#-     }
#-     stop("'x' must be a vector or an array")
#- }
#- 
#- 
#- #' Plot an AD2CP Object
#- #'
#- #' Used by \code{\link{plot,adp-method}} or called directly, this function
#- #' plots some aspects of AD2CP data. It is still in development as of the
#- #' Spring of 2023.
#- #'
#- #' @param x an AD2CP object, as created with [read.adp.ad2cp()] or by
#- #' [read.oce()] on a file of the AD2CP type.
#- #'
#- #' @param which a character value indicating what to plot.  Use NULL to see a
#- #' listing of the possibilities for this particular object.
#- #_ Some plots are
#- #_ indicated with a notation that is distinct to this function, e.g.
#- #_ `which="accelerometer"` plots 3 panels for the x, y and z components,
#- #_ and `which="accelerometer/x"` plots just the x component. This is a *trial*
#- #_ notation that might be removed or altered in a future version.
#- #'
#- #' @param col indication of colour, passed to [imagep()] or to [oce.plot.ts()],
#- #' depending on whether the plot is an image or a time-series graph. This
#- #' defaults to [oceColorsVelocity] for velocity images, [oceColorsViridis]
#- #' for amplitude and quality images, and to black for time-series plots.
#- #'
#- #' @param type plot type, used only for time-series graphs.
#- #'
#- #' @param lwd line width, used only for time-series graphs.
#- #'
#- #' @param cex character expansion factor.
#- #'
#- #' @param pch character code.
#- #'
#- #' @param debug a flag that turns on debugging.  Set to 1 to get a moderate
#- #' amount of debugging information, or to 2 to get more.
#- #'
#- #' @param ... optional other arguments, passed to the lower-level plotting
#- #' commands.
#- #'
#- #' @examples
#- #' library(oce)
#- #' # This example will only work for the author, because it uses a
#- #' # private file.  The file contains 'burst' and 'average' data.
#- #' f <- "/Users/kelley/Dropbox/oce_secret_data/ad2cp/secret1_trimmed.ad2cp"
#- #' if (file.exists(f)) {
#- #'     library(oce)
#- #'     d <- read.oce(f, dataType="average")
#- #'     # Example 1: time-distance variation of "average" velocity (beams 1 through 4)
#- #'     plot(d, which="v", col=oceColorsVelocity)
#- #'     # Example 2: time variation of amplitude (all beams)
#- #'     plot(d, which="a")
#- #_     # Example 3: time variation of x amplitude
#- #_     plot(d, which="a/1")
#- #'     # Example 3: time variation of the three components measured by the accelerometer
#- #'     plot(d, which="accelerometer")
#- #_     # Example 5: time variation of the x component of acceleration
#- #_     plot(d, which="accelerometer/x")
#- #' }
#- #'
#- #' @family functions that plot oce data
#- #' @family things related to ad2cp data
#- #'
#- #' @author Dan Kelley
#- plotAD2CP <- function(x, which=NULL, cex, col, pch, lwd, type, debug=getOption("oceDebug"), ...)
#- {
#-     stop("HOW DID WE GET HERE?")
#-     if (!is.ad2cp(x))
#-         stop("'x' must be an AD2CP object that was created by read.adp.ad2cp()")
#-     oceDebug(debug, "plotAD2CP(x", argShow(which), ") {\n", sep="", style="bold", unindent=1)
#-     dataType <- x@metadata$dataType
#-     oceDebug(debug, vectorShow(dataType))
#-     if (dataType == 0x23) {            # echosounderRaw
#-         if (is.null(which)) {
#-             which <- "amplitude"
#-         }
#-         if (which[1] == "amplitude") {
#-             if (missing(col)) {
#-                 imagep(x[["time"]], x[["distance"]], log10(Mod(x[["samples"]])),
#-                     ylab=resizableLabel("distance"), ...)
#-             } else {
#-                 imagep(x[["time"]], x[["distance"]], log10(Mod(x[["samples"]])),
#-                     ylab=resizableLabel("distance"), col=col, ...)
#-             }
#-         } else if (which[1] == "phase") {
#-             if (missing(col)) {
#-                 imagep(x[["time"]], x[["distance"]], 180/pi*Arg(x[["samples"]]),
#-                     ylab=resizableLabel("distance"), ...)
#-             } else {
#-                 imagep(x[["time"]], x[["distance"]], 180/pi*Arg(x[["samples"]]),
#-                     ylab=resizableLabel("distance"), col=col, ...)
#-             }
#-         } else {
#-             stop("which must be 'amplitude' or 'phase'")
#-         }
#-         return(invisible(NULL))
#-     }
#-     names1 <- sort(names(x@data))
#-     if (is.null(which))
#-         stop("which must be supplied; try one of: \"", paste(names1, collapse="\", \""), "\"")
#-     if (!is.character(which[1]))
#-         stop("'which' must be a character value")
#-     if (length(which) != 1L)
#-         stop("'which' must be of length 1")
#-     if (which == "?") {
#-         message("try setting 'which' to one of: \"", paste(names1, collapse="\", \""), "\"")
#-         return(invisible(NULL))
#-     }
#-     w <- strsplit(which, "/")[[1]]
#-     nw <- length(w)
#-     #message(vectorShow(w))
#-     if (nw > 4L)
#-         stop("'which' must contain zero to four \"/\" characters, but it has ", nw)
#-     if (!w[1] %in% names1)
#-         stop("unknown which, \"", w[1], "\"; try one of: \"", paste(names1, collapse="\", \""), "\"")
#-     # Handle some top-level defaults
#-     #?if (identical(w, "echosounder") && "echosounder" %in% names1) {
#-     #?    with(x@data$echosounder,
#-     #?        {
#-     #?            imagep(time, distance, echosounder, xlab="", ylab=resizableLabel("distance"))
#-     #?        })
#-     #?    return(invisible(NULL))
#-     #?}
#-     #if (nw < 2)
#-     #    stop("insufficient detail in which; try e.g. which=\"", which, "/?\" to see possibilities")
#-     oceDebug(debug, vectorShow(w[1]))
#-     #? d <- x@data[[w[1]]]
#-     time <- x[["time"]]
#-     ntime <- length(time)
#-     distance <- x[["distance"]]
#-     ndistance <- length(distance)
#-     # Find relevant subitem names, which are for things that can be shown in a
#-     # time-distance image, or in a variable-time linegraph.
#-     names2 <- sort(names(x@data))
#-     names2keep <- sapply(names2,
#-         function(n)
#-         {
#-             I <- x@data[[n]]
#-             if (n == "configuration" || n == "datasetDescription") FALSE
#-             else if (is.list(I)) TRUE
#-             else if (is.vector(I) && length(I) == ntime) TRUE
#-             else if (is.array(I) && dim(I)[1] == ntime && dim(I)[2] == ndistance) TRUE
#-             else FALSE
#-         })
#-     names2 <- names2[names2keep]
#-     oceDebug(debug, vectorShow(names2, n=100))
#-     #if (nw > 1L && w[2] == "?") {
#-     #    message("try setting 'which' to one of: \"", paste(paste0(w[1],"/",names2), collapse="\", \""), "\"")
#-     #    return(invisible(NULL))
#-     #}
#-     #if (nw > 2L && w[3] == "?") {
#-     #    if (w[2] %in% c("accelerometer", "magnetometer")) {
#-     #        message("try setting 'which' to one of: \"", paste0(paste0(w[1],"/",w[2]), "/", c("x","y","z"), collapse="\", \""), "\"")
#-     #    } else if (w[2] == "AHRS") {
#-     #        message("try setting 'which' to one of: \"", paste0(paste0(w[1],"/",w[2]), "/", c("quaternions","gyro"), collapse="\", \""), "\"")
#-     #    } else {
#-     #        message("sorry, no hints are available for which=\"", w[1], "/", w[2], "/?")
#-     #    }
#-     #    return(invisible(NULL))
#-     #}
#-     #if (nw > 3L && w[4] == "?" && w[2] == "AHRS") {
#-     #    if (w[3] == "quaternions") {
#-     #        message("try setting 'which' to one of: \"", paste0(paste0(w[1],"/",w[2]), "/", w[3], "/", c("x","y","z"), collapse="\", \""), "\"")
#-     #    } else if (w[3] == "gyro") {
#-     #        message("try setting 'which' to one of: \"", paste0(paste0(w[1],"/",w[2]), "/", w[3], "/", c("x","y","z"), collapse="\", \""), "\"")
#-     #    } else {
#-     #        stop("the 3th element of 'which' must be \"quaternions\" or \"gyro\", not \"", w[3], "\"")
#-     #    }
#-     #    return(invisible(NULL))
#-     #}
#-     #message("next is names(d):");print(names(d),file=stderr())
#-     opar <- par(no.readonly=TRUE)      # retain so we can reset afterwards, as CRAN requires
#-     # Ensure that user is asking for a plottable item.
#-     if (!w[1] %in% names2)
#-         stop("item \"", w[1], "\" is not available; try one of \"", paste(names2, collapse="\", \""), "\"")
#-     # Handle by case
#-     oceDebug(debug, vectorShow(w))
#-     if (w[1] %in% c("a", "q", "v")) {
#-         #.message(vectorShow(w,showNewline=FALSE))
#-         D <- makeNumeric(x[[w[1]]])
#-         #.message(vectorShow(D,showNewline=FALSE))
#-         nbeam <- dim(D)[3]
#-         #.message(vectorShow(nbeam,showNewline=FALSE))
#-         beams <- if (nw == 1) seq_len(nbeam) else as.integer(w[2])
#-         #.message(vectorShow(beams,showNewline=FALSE))
#-         par(mfrow=c(length(beams), 1))
#-         for (ibeam in beams) {
#-             #.message(vectorShow(ibeam,showNewline=FALSE))
#-             if (0 < ibeam && ibeam <= nbeam) {
#-                 #.message("plotting ibeam=", ibeam)
#-                 zlim <- if (w[1] == "v") {
#-                     c(-1, 1)*max(abs(D[, , ibeam]), na.rm=TRUE)
#-                 } else {
#-                     range(D[, , ibeam])
#-                 }
#-                 #message(vectorShow(zlim,showNewline=FALSE))
#-                 imagep(time, distance, D[, , ibeam], zlim=zlim, ylab="Distance [m]", col=col, ...)
#-                 mtext(paste(w[1], "beam", ibeam), side=3, adj=1, cex=par("cex"))
#-             }
#-         }
#-         if (length(beams) > 1L)
#-             par(opar)
#-         #message(vectorShow(D))
#-         #message(vectorShow(nbeam))
#-         #dots <- list(...)
#-         #dotsNames <- names(dots)
#-         #cat(str(dots))
#-         #beams <- if (nw < 2L) seq_len(nbeam) else w[2]
#-         #if (length(beams) > 1L)
#-         #    par(mfrow=c(length(beams), 1))
#-         #if (missing(col))
#-         #    col <- if (w[1] == "v") oceColorsVelocity else oceColorsViridis
#-         #for (ibeam in as.integer(beams)) {
#-         #    #message(vectorShow(ibeam))
#-         #    if (w[1] == "v" && !"zlim" %in% dotsNames) {
#-         #        zlim <- c(-1,1)*max(abs(D[,,ibeam]), na.rm=TRUE)
#-         #        imagep(time, distance, D[,,ibeam], zlim=zlim, ylab="Distance [m]", col=col, ...)
#-         #    } else {
#-         #        imagep(time, distance, D[,,ibeam], ylab="Distance [m]", col=col, ...)
#-         #    }
#-         #    mtext(paste0(w[1], "[,,",ibeam,"]"), side=3, line=0, adj=1)
#-         #}
#-         #if (length(beams) > 1L)
#-         #    par(opar)
#-     } else if (w[1] == "echosounder") {
#-         D <- x@data[[w[1]]]
#-         if (missing(col))
#-             imagep(D$time, D$distance, D$echosounder, ylab=resizableLabel("distance"), ...)
#-         else
#-             imagep(D$time, D$distance, D$echosounder, col=col, ylab=resizableLabel("distance"), ...)
#-     } else if (w[1] == "echosounderRaw") {
#-         D <- x@data[[w[1]]]
#-         if (missing(col))
#-             imagep(x[["time"]], x[["distance"]], Mod(x[["samples"]]), ylab=resizableLabel("distance"), ...)
#-         else
#-             imagep(x[["time"]], x[["distance"]], Mod(x[["samples"]]), ylab=resizableLabel("distance"), col=col, ...)
#-     } else if (w[1] == "altimeter") {
#-         message("FIXME: untested plot of ", w[1], "/", w[2], ": please report error")
#-         D <- x@data[[w[1]]]
#-         if (nw < 2)
#-             stop("")
#-         if (w[2] %in% c("distance", "quality", "status")) {
#-             oce.plot.ts(D$time, D[w[3]], ylab=w[3])
#-         } else {
#-             stop("insufficient detail in 'which'; try e.g. which=\"", w[1], "/", w[2], "/?\" to see possibilities")
#-         }
#-         message("FIXME: ... was the plot okay?")
#-     } else if (w[1] == "altimeterRaw") {
#-         message("FIXME: plot of ", w[1], "/", w[2], " is not tested, and may produce errors")
#-         D <- x@data[[w[1]]][[w[2]]]
#-         imagep(D$time, D$distance, D$samples, ylab=resizableLabel("distance"))
#-         message("FIXME: ... was the plot okay?")
#-     } else if (w[1] == "AHRS") {
#-         print(w)
#-         if (nw == 2) {
#-             message("'which' needs more detail; use which=\"", w[1], "/", w[2], "/?\" for hints")
#-             return(invisible(NULL))
#-         } else if (nw == 3) {
#-             if (missing(col))
#-                 col <- 1
#-             if (w[3] == "gyro") {
#-                 par(mfrow=c(3, 1))
#-                 oce.plot.ts(time, x@data[[w[1]]][[w[2]]][[w[3]]][["x"]], ylab=paste(w[3], "x"),
#-                     cex=if (missing(cex)) 1 else cex, col=if (missing(col)) 1 else col,
#-                     pch=if (missing(pch)) 1 else pch, lwd=if (missing(lwd)) 1 else lwd,
#-                     type=if (missing(type)) "l" else type)
#-                 oce.plot.ts(time, x@data[[w[1]]][[w[2]]][[w[3]]][["y"]], ylab=paste(w[3], "y"),
#-                     cex=if (missing(cex)) 1 else cex, col=if (missing(col)) 1 else col,
#-                     pch=if (missing(pch)) 1 else pch, lwd=if (missing(lwd)) 1 else lwd,
#-                     type=if (missing(type)) "l" else type)
#-                 oce.plot.ts(time, x@data[[w[1]]][[w[2]]][[w[3]]][["z"]], ylab=paste(w[3], "z"),
#-                     cex=if (missing(cex)) 1 else cex, col=if (missing(col)) 1 else col,
#-                     pch=if (missing(pch)) 1 else pch, lwd=if (missing(lwd)) 1 else lwd,
#-                     type=if (missing(type)) "l" else type)
#-                 par(opar)
#-             } else if (w[3] == "quaternions") {
#-                 par(mfrow=c(4, 1))
#-                 oce.plot.ts(time, x@data[[w[1]]][[w[2]]][[w[3]]][["x"]], ylab=paste(w[3], "x"),
#-                     cex=if (missing(cex)) 1 else cex, col=if (missing(col)) 1 else col,
#-                     pch=if (missing(pch)) 1 else pch, lwd=if (missing(lwd)) 1 else lwd,
#-                     type=if (missing(type)) "l" else type)
#-                 oce.plot.ts(time, x@data[[w[1]]][[w[2]]][[w[3]]][["y"]], ylab=paste(w[3], "y"),
#-                     cex=if (missing(cex)) 1 else cex, col=if (missing(col)) 1 else col,
#-                     pch=if (missing(pch)) 1 else pch, lwd=if (missing(lwd)) 1 else lwd,
#-                     type=if (missing(type)) "l" else type)
#-                 oce.plot.ts(time, x@data[[w[1]]][[w[2]]][[w[3]]][["z"]], ylab=paste(w[3], "z"),
#-                     cex=if (missing(cex)) 1 else cex, col=if (missing(col)) 1 else col,
#-                     pch=if (missing(pch)) 1 else pch, lwd=if (missing(lwd)) 1 else lwd,
#-                     type=if (missing(type)) "l" else type)
#-                 oce.plot.ts(time, x@data[[w[1]]][[w[2]]][[w[3]]][["w"]], ylab=paste(w[3], "w"),
#-                     cex=if (missing(cex)) 1 else cex, col=if (missing(col)) 1 else col,
#-                     pch=if (missing(pch)) 1 else pch, lwd=if (missing(lwd)) 1 else lwd,
#-                     type=if (missing(type)) "l" else type)
#-                 par(opar)
#-             } else {
#-                 stop("third 'which' entry must be \"gyro\" or \"quaternions\", not \"", w[3], "\"")
#-             }
#-         } else if (nw == 4) {
#-             if (w[3] == "gyro") {
#-                 if (w[4] %in% c("x", "y", "z")) {
#-                     oce.plot.ts(time, x@data[[w[1]]][[w[2]]][[w[3]]][[w[4]]], ylab=paste(w[3], "x"),
#-                         cex=if (missing(cex)) 1 else cex, col=if (missing(col)) 1 else col,
#-                         pch=if (missing(pch)) 1 else pch, lwd=if (missing(lwd)) 1 else lwd,
#-                         type=if (missing(type)) "l" else type)
#-                 } else {
#-                     stop("fourth word in which must be \"x\", \"y\" or \"z\", not \"", w[4], "\"")
#-                 }
#-             } else if (w[3] == "quaternions") {
#-                 if (w[4] %in% c("x", "y", "z", "w")) {
#-                     oce.plot.ts(time, x@data[[w[1]]][[w[2]]][[w[3]]][[w[4]]], ylab=paste(w[3], "x"),
#-                         cex=if (missing(cex)) 1 else cex, col=if (missing(col)) 1 else col,
#-                         pch=if (missing(pch)) 1 else pch, lwd=if (missing(lwd)) 1 else lwd,
#-                         type=if (missing(type)) "l" else type)
#-                 } else {
#-                     stop("fourth word in which must be \"x\", \"y\", \"z\" or \"w\", not \"", w[4], "\"")
#-                 }
#-             } else {
#-                 stop("third 'which' entry must be \"gyro\" or \"quaternions\", not \"", w[3], "\"")
#-             }
#-             return(invisible(NULL))
#-         }
#-         return(invisible(NULL))
#-     } else if (w[1] %in% c("accelerometer", "magnetometer")) {
#-         D <- x[[w[1]]]                 # has components $x, $y and $z
#-         if (nw == 1) { # plot 3 panels
#-             oceDebug(debug, "multi-panel with ", vectorShow(w))
#-             par(mfrow=c(3, 1))
#-             oce.plot.ts(time, D[["x"]], ylab=paste(w[1], "x"),
#-                 cex=if (missing(cex)) 1 else cex, col=if (missing(col)) 1 else col,
#-                 pch=if (missing(pch)) 1 else pch, lwd=if (missing(lwd)) 1 else lwd,
#-                 type=if (missing(type)) "l" else type)
#-             oce.plot.ts(time, D[["y"]], ylab=paste(w[1], "y"),
#-                 cex=if (missing(cex)) 1 else cex, col=if (missing(col)) 1 else col,
#-                 pch=if (missing(pch)) 1 else pch, lwd=if (missing(lwd)) 1 else lwd,
#-                 type=if (missing(type)) "l" else type)
#-             oce.plot.ts(time, D[["y"]], ylab=paste(w[1], "z"),
#-                 cex=if (missing(cex)) 1 else cex, col=if (missing(col)) 1 else col,
#-                 pch=if (missing(pch)) 1 else pch, lwd=if (missing(lwd)) 1 else lwd,
#-                 type=if (missing(type)) "l" else type)
#-             par(opar)
#-         } else {
#-             oceDebug(debug, "single-panel with ", vectorShow(w))
#-             if (!w[2] %in% names(D))
#-                 stop(w[1], "$", w[1], " does not contain \"", w[2], "\"; try one of \"", paste(sort(names(D)), collapse="\" \""), "\"")
#-             oce.plot.ts(time, D[[w[2]]], ylab=paste(w[1], w[2]),
#-                 cex=if (missing(cex)) 1 else cex, col=if (missing(col)) 1 else col,
#-                 pch=if (missing(pch)) 1 else pch, lwd=if (missing(lwd)) 1 else lwd,
#-                 type=if (missing(type)) "l" else type)
#-         }
#-     } else if (length(x[[w[1]]]) == ntime) {
#-         # time-series graph of some vector element
#-         oce.plot.ts(time, x[[w[1]]], ylab=paste(w[1]),
#-             cex=if (missing(cex)) 1 else cex, col=if (missing(col)) 1 else col,
#-             pch=if (missing(pch)) 1 else pch, lwd=if (missing(lwd)) 1 else lwd,
#-             type=if (missing(type)) "l" else type)
#-     } else {
#-         stop("although \"", w[1], "\" is present in the data, it is not handled yet by this function")
#-     }
#-     oceDebug(debug, "} # plotAD2CP()\n", sep="", style="bold", unindent=1)
#- }
