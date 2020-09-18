## vim:textwidth=128:expandtab:shiftwidth=4:softtabstop=4

#' Class to Store Tidal Models
#'
#' This class stores tidal-constituent models.
#'
#' @templateVar class tidem
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
#' @author Dan Kelley
#' @family functions that plot oce data
#' @family things related to tides
setClass("tidem", contains="oce")

setMethod(f="initialize",
          signature="tidem",
          definition=function(.Object, ...) {
              .Object <- callNextMethod(.Object, ...)
              .Object@metadata$version <- ""
              .Object@processingLog$time <- presentTime()
              .Object@processingLog$value <- "create 'tidem' object"
              return(.Object)
          })


#' Tidal Constituent Information
#'
#' The `tidedata` dataset contains Tide-constituent information that is
#' use by [tidem()] to fit tidal models.  `tidedata` is a list
#' containing
#' \describe{
#' \item{`const`}{
#' a list containing vectors
#' `name` (a string with constituent name),
#' `freq` (the frequency, in cycles per hour),
#' `kmpr` (a string naming the comparison constituent, blank if there is none),
#' `ikmpr` (index of comparison constituent, or `0` if there is none),
#' `df` (frequency difference between constituent and its
#' comparison, used in the Rayleigh criterion),
#' `d1` through `d6` (the first through sixth Doodson numbers),
#' `semi`,
#' `nsat` (number of satellite constituents),
#' `ishallow`,
#' `nshallow`,
#' `doodsonamp`,
#' and
#' `doodsonspecies`.
#'}
#' \item{`sat`}{
#' a list containing vectors
#' `deldood`,
#' `phcorr`,
#' `amprat`,
#' `ilatfac`,
#' and
#' `iconst`.
#'}
#' \item{`shallow`}{
#' a list containing vectors
#' `iconst`,
#' `coef`,
#' and
#' `iname`.
#'}
#'}
#' Apart from the use of `d1` through `d6`, the naming and content
#' follows `T_TIDE` (see Pawlowicz et al. 2002), which in turn builds upon
#' the analysis of Foreman (1978).
#'
#' @name tidedata
#'
#' @docType data
#'
#' @author Dan Kelley
#'
#' @references
#'
#' Foreman, M. G. G., 1978. Manual for Tidal Currents Analysis and Prediction.
#' Pacific Marine Science Report. British Columbia, Canada: Institute of Ocean
#' Sciences, Patricia Bay.
#'
#' Pawlowicz, Rich, Bob Beardsley, and Steve Lentz, 2002.  Classical tidal
#' harmonic analysis including error estimates in MATLAB using `T_TIDE`.
#' Computers and Geosciences, 28, 929-937.
#'
#' @source The data come from the `tide3.dat` file of the `T_TIDE`
#' package (Pawlowicz et al., 2002), and derive from Appendices provided by
#' Foreman (1978).  The data are scanned using \file{tests/tide.R} in this
#' package, which also performs some tests using `T_TIDE` values as a
#' reference.
#'
#' @family things related to tides
NULL

#' Tidal Current Dataset
#'
#' The `tidalCurrent` dataset contains tidal velocities reported in
#' Foreman's (1978) report (reference 1) on his Fortran code for the analysis of
#' tidal currents and provided in an associated webpage (reference 2).
#' Here, `tidalCurrent` is data frame containing
#' * `time` a POSIXct time.
#' * `u` the eastward component of velocity in m/s.
#' * `v` the northward component of velocity in m/s.
#'
#' @name tidalCurrent
#'
#' @docType data
#'
#' @examples
#' library(oce)
#' data(tidalCurrent)
#' par(mfrow=c(2, 1))
#' oce.plot.ts(tidalCurrent$time, tidalCurrent$u, ylab="u [m/s]")
#' abline(h=0, col=2)
#' oce.plot.ts(tidalCurrent$time, tidalCurrent$v, ylab="v [m/s]")
#' abline(h=0, col=2)
#'
#' @author Dan Kelley (reformatting data provided by Michael Foreman)
#'
#' @references
#'
#' 1. Foreman, M. G. G. “Manual for Tidal Currents Analysis and Prediction.”
#'    Pacific Marine Science Report.
#'    British Columbia, Canada: Institute of Ocean Sciences, Patricia Bay, 1978.
#' 2. \code{https://www.dfo-mpo.gc.ca/science/documents/data-donnees/tidal-marees/tidpack.zip}
#'
#' @source The data come from the `tide8.dat` and `tide9.dat` files provided
#' at reference 2.
#'
#' @family things related to tides
NULL


#' Summarize a Tidem Object
#'
#' By default, all fitted constituents are plotted, but it is quite useful to
#' set e.g. p=0.05 To see just those constituents that are significant at the 5
#' percent level.
#' Note that the p values are estimated as the average of the p values for the
#' sine and cosine components at a given frequency.
#'
#' @param object an object of class [tidem], as created by
#' [as.tidem()] or [tidem()].
#'
#' @param p optional value of the maximum p value for the display of an
#' individual coefficient.  If not given, all coefficients are shown.
#'
#' @param constituent optional character vector holding the names
#' of constituents on which to focus.
#' @template tideconst
#'
#' @param \dots further arguments passed to or from other methods.
#'
#' @return `NULL`
#'
#' @author Dan Kelley
#'
#' @examples
#'\dontrun{
#' library(oce)
#' data(sealevel)
#' tide <- tidem(sealevel)
#' summary(tide)
#'}
#'
#' @family things related to tides
setMethod(f="summary",
          signature="tidem",
          definition=function(object, p, constituent, ...) {
              debug <- if ("debug" %in% names(list(...))) list(...)$debug else 0
              version <- object@metadata$version
              if (missing(p))
                  p <- 1
              ok <- object@data$p <= p | version == 3
              haveP <- any(!is.na(object@data$p))
              if (missing(constituent)) {
                  fit <- data.frame(Const=object@data$const[ok],
                                    Name=object@data$name[ok],
                                    Freq=object@data$freq[ok],
                                    Amplitude=object@data$amplitude[ok],
                                    Phase=object@data$phase[ok],
                                    p=object@data$p[ok])
                  if (debug) {
                      cat("For missing(constituent) case, fit is:\n")
                      print(fit)
                  }
              } else {
                  i <- NULL
                  bad <- NULL
                  for (iconst in seq_along(constituent)) {
                      w <- which(object@data$name==constituent[iconst])
                      if (length(w) == 1) {
                          i <- c(i, w)
                      } else {
                          bad <- c(bad, iconst)
                      }
                  }
                  if (length(bad)) {
                      warning("the following constituents are not handled: '",
                              paste(constituent[bad], collapse="', '"), "'\n", sep="")
                  }
                  if (length(i) == 0)
                      stop("In summary,tidem-method() : no known constituents were provided", call.=FALSE)
                  i <- unique(i)
                  fit <- data.frame(Const=object@data$const[i],
                                    Name=object@data$name[i],
                                    Freq=object@data$freq[i],
                                    Amplitude=object@data$amplitude[i],
                                    Phase=object@data$phase[i],
                                    p=object@data$p[i])
                  if (debug) {
                      cat("For !missing(constituent) case, fit is:\n")
                      print(fit)
                  }
              }
              cat("tidem summary\n-------------\n")
              if (version != "3") {
                  cat("\nCall:\n")
                  cat(paste(deparse(object[["call"]]), sep="\n", collapse="\n"), "\n", sep="")
                  cat("RMS misfit to data: ", sqrt(var(object[["model"]]$residuals)), '\n')
                  cat("\nFitted Model:\n")
                  f <- fit[3:6]
                  if (debug > 0) {
                      cat("fit:\n");print(fit)
                      cat("f:\n");print(f)
                  }
                  rownames(f) <- as.character(fit[, 2])
                  if (haveP) {
                      printCoefmat(f, digits=3,
                                   signif.stars=getOption("show.signif.stars"),
                                   signif.legend=TRUE,
                                   P.values=TRUE, has.Pvalue=TRUE, ...)
                  } else {
                      printCoefmat(f[, -4], digits=3)
                  }
              } else {
                  cat("\nSupplied Model:\n")
                  f <- fit[3:5]
                  rownames(f) <- as.character(fit[, 2])
                  printCoefmat(f, digits=3)
              }
              processingLogShow(object)
              invisible(NULL)
          })

#' Extract Something From a Tidem Object
#'
#' @param x a [tidem-class] object.
#'
#' @section Details of the specialized `tidem` method:
#'
#' A vector of the frequencies of fitted constituents is recovered
#' with e.g. `x[["frequency"]]`. Similarly, amplitude is
#' recovered with e.g. `x[["amplitude"]]` and phase with
#' e.g. `x[["phase"]]`. If any other string is specified, then
#' the underlying accessor \code{\link{[[,oce-method}}) is used.
#'
#' @template sub_subTemplate
#'
#' @family things related to tides
setMethod(f="[[",
          signature(x="tidem", i="ANY", j="ANY"),
          definition=function(x, i, j, ...) {
              if (i == "frequency") {
                  x@data$freq
              } else if (i == "amplitude") {
                  x@data$amplitude
              } else if (i == "phase") {
                  x@data$phase
              } else {
                  callNextMethod()         # [[
              }
          })

#' Replace Parts of a Tidem Object
#'
#' @param x a [tidem-class] object.
#'
#' @template sub_subsetTemplate
#'
#' @family things related to tides
setMethod(f="[[<-",
          signature(x="tidem", i="ANY", j="ANY"),
          definition=function(x, i, j, ..., value) {
              callNextMethod(x=x, i=i, j=j, ...=..., value=value) # [[<-
          })



#' Plot a tidem Object
#'
#' Plot a summary diagram for a tidal fit.
#'
#' @param x a [tidem-class] object.
#'
#' @param which integer flag indicating plot type, 1 for stair-case spectral, 2
#' for spike spectral.
#'
#' @param constituents character vector holding the names of constituents that are
#' to be drawn and labelled. If `NULL`, then no constituents will be shown.
#' @template tideconst
#'
#' @param sides an integer vector of length equal to that of `constituents`,
#' designating the side on which the constituent labels are to be drawn. As in
#' all R graphics, the value `1` indicates the bottom of the plot, and
#' `3` indicates the top. If `sides=NULL`, the default, then all labels
#' are drawn at the top. Any value of `sides` that is not either 1 or 3
#' is converted to 3.
#'
#' @param col a character vector naming colors to be used for `constituents`.
#' Ignored if `sides=3`. Repeated to be of the same length as
#' `constituents`, otherwise.
#'
#' @param log if set to "`x`", the frequency axis will be logarithmic.
#'
#' @param mgp 3-element numerical vector to use for `par(mgp)`, and also
#' for `par(mar)`, computed from this.  The default is tighter than the R
#' default, in order to use more space for the data and less for the axes.
#'
#' @param mar value to be used with `[`par`]("mar")`.
#'
#' @param \dots optional arguments passed to plotting functions.
#'
#' @examples
#'\dontrun{
#' library(oce)
#' data(sealevel)
#' tide <- tidem(sealevel)
#' plot(tide)
#'}
#'
#' @section Historical note:
#' An argument named `labelIf` was removed in July 2016,
#' because it was discovered never to have worked as documented, and
#' because the more useful argument `constituents` had been added.
#'
#' @author Dan Kelley
#'
#' @family functions that plot oce data
#'
#' @aliases plot.tidem
#'
#' @family things related to tides
setMethod(f="plot",
          signature=signature("tidem"),
          definition=function(x,
                              which=1,
                              constituents=c("SA", "O1", "K1", "M2", "S2", "M4"),
                              sides=NULL,
                              col="blue",
                              log="",
                              mgp=getOption("oceMgp"),
                              mar=c(mgp[1]+1, mgp[1]+1, mgp[2]+0.25, mgp[2]+1),
                              ...)
          {
              data("tidedata", package="oce", envir=environment())
              tidedata <- get("tidedata")#, pos=globalenv())
              drawConstituent<-function(name="M2", side=3, col="blue", adj=NULL)
              {
                  w <- which(tidedata$const$name == name)
                  ## message("w:")
                  ## print(w)
                  if (!length(w)) {
                      warning("constituent '", name, "' is unknown")
                      return()
                  }
                  frequency <- tidedata$const$freq[w]
                  ## message("constituent '", name, "' has frequency ", frequency, " cph")
                  abline(v=frequency, col=col, lty="dotted")
                  if (par('usr')[1] < frequency && frequency <= par('usr')[2]) {
                      if (is.null(adj))
                          mtext(name, side=side, at=frequency, col=col, cex=0.8)
                      else
                          mtext(name, side=side, at=frequency, col=col, cex=0.8, adj=adj)
                  }
              }
              opar <- par(no.readonly = TRUE)
              lw <- length(which)
              if (lw > 1) on.exit(par(opar))
              par(mgp=mgp, mar=mar)
              frequency <- x@data$freq[-1] # trim z0
              amplitude <- x@data$amplitude[-1]
              name      <- x@data$name[-1]
              if (!is.null(constituents)) {
                  sides <- if (is.null(sides)) rep(3, length(constituents))
                  else rep(sides, length.out=length(constituents))
                  col <- rep(col, length.out=length(constituents))
              }
              sides[sides!=1&sides!=3] <- 3 # default to top
              for (w in 1:lw) {
                  ##message("w=", w, "; which[w]=", which[w])
                  if (which[w] == 2) {
                      plot(frequency, amplitude, col="white", xlab="Frequency [ cph ]", ylab="Amplitude [ m ]", log=log)
                      segments(frequency, 0, frequency, amplitude)
                      for (i in seq_along(constituents))
                          drawConstituent(constituents[i], side=sides[i], col=col[i])
                  } else if (which[w] == 1) {
                      plot(frequency, cumsum(amplitude), xlab="Frequency [ cph ]", ylab="Amplitude [ m ]", log=log, type='s')
                      for (i in seq_along(constituents))
                          drawConstituent(constituents[i], side=sides[i], col=col[i])
                  } else {
                      stop("unknown value of which ", which, "; should be 1 or 2")
                  }
              }
              ##mtext(x$call, side=4, adj=1, cex=2/3)
              if (!all(is.na(pmatch(names(list(...)), "main")))) title(...)
          })

#' Create tidem object from fitted harmonic data
#'
#' This function is intended to provide a bridge to
#' [predict.tidem()], enabling tidal predictions based
#' on published tables of harmonic fits.
#'
#' Note that only constituent names known to [tidem()] are handled.
#' The permitted names are those listed in Foreman (1978), and
#' tabulated with
#'\preformatted{
#' data(tidedata)
#' data.frame(name=tidedata$const$name, freq=tidedata$const$freq)
#'}
#' Warnings are issued for any constituent name that is not in this list; as
#' of the late summer of 2019, the only example seen in practice is
#' `M1`, which according to Wikipedia (2019) has frequency 0.0402557, which
#' is very close to that of `NO1`, i.e. 0.04026859, perhaps explaining
#' why Foreman (1978) did not handle this constituent. A warning is
#' issued if this or any other unhandled constituent is provided
#' in the `name` argument to `as.tidem()`.
#'
#' @param tRef a POSIXt value indicating the mean time of the
#' observations used to develop the harmonic model. This is rounded
#' to the nearest hour in [as.tidem()], to match [tidem()].
#'
#' @param latitude Numerical value indicating the latitude of the
#' observations that were used to create the harmonic model. This
#' is needed for nodal-correction procedures carried out
#' by [tidemVuf()].
#'
#' @param name character vector holding names of constituents.
#' @template tideconst
#'
#' @param amplitude Numeric vector of constituent amplitudes.
#'
#' @param phase Numeric vector of constituent Greenwich phases.
#' @template debugTemplate
#'
#' @return An object of [tidem-class], with only minimal
#' contents.
#'
#' @section Known issues:
#' There are two known differences between [tidem()] and the Matlab
#' `T_TIDE` package, as listed in references 3 and 4. Work on these
#' issues is planned for the summer of 2020.
#'
#' @examples
#' # Simulate a tide table with output from tidem().
#' data(sealevelTuktoyaktuk)
#' # 'm0' is model fitted by tidem()
#' m0 <- tidem(sealevelTuktoyaktuk)
#' p0 <- predict(m0, sealevelTuktoyaktuk[["time"]])
#' m1 <- as.tidem(mean(sealevelTuktoyaktuk[["time"]]), sealevelTuktoyaktuk[["latitude"]],
#'                m0[["name"]], m0[["amplitude"]], m0[["phase"]])
#' # Test agreement with tidem() result, by comparing predicted sealevels.
#' p1 <- predict(m1, sealevelTuktoyaktuk[["time"]])
#' expect_lt(max(abs(p1 - p0), na.rm=TRUE), 1e-10)
#' # Simplified harmonic model, using large constituents
#' # > m0[["name"]][which(m[["amplitude"]]>0.05)]
#' # [1] "Z0"  "MM"  "MSF" "O1"  "K1"  "OO1" "N2"  "M2"  "S2"
#' h <- "
#' name  amplitude      phase
#'   Z0 1.98061875   0.000000
#'   MM 0.21213065 263.344739
#'  MSF 0.15605629 133.795004
#'   O1 0.07641438  74.233130
#'   K1 0.13473817  81.093134
#'  OO1 0.05309911 235.749693
#'   N2 0.08377108  44.521462
#'   M2 0.49041340  77.703594
#'   S2 0.22023705 137.475767"
#' coef <- read.table(text=h, header=TRUE)
#' m2 <- as.tidem(mean(sealevelTuktoyaktuk[["time"]]),
#'                sealevelTuktoyaktuk[["latitude"]],
#'                coef$name, coef$amplitude, coef$phase)
#' p2 <- predict(m2, sealevelTuktoyaktuk[["time"]])
#' expect_lt(max(abs(p2 - p0), na.rm=TRUE), 1)
#' par(mfrow=c(3, 1))
#' oce.plot.ts(sealevelTuktoyaktuk[["time"]], p0)
#' ylim <- par("usr")[3:4] # to match scales in other panels
#' oce.plot.ts(sealevelTuktoyaktuk[["time"]], p1, ylim=ylim)
#' oce.plot.ts(sealevelTuktoyaktuk[["time"]], p2, ylim=ylim)
#'
#' @references
#' 1. Foreman, M. G. G., 1978. Manual for Tidal Currents Analysis and Prediction.
#' Pacific Marine Science Report. British Columbia, Canada: Institute of Ocean
#' Sciences, Patricia Bay.
#'
#' 2. Wikipedia, "Theory of Tides." https://en.wikipedia.org/wiki/Theory_of_tides
#' Downloaded Aug 17, 2019.
#'
#' 3. Github issue 1653: tidem() and t_tide do not produce identical results
#' https://github.com/dankelley/oce/issues/1653
#'
#' 4. Github issue 1654: predict(tidem()) uses all constituents, unlike T_TIDE
#' https://github.com/dankelley/oce/issues/1654
#'
#' @family things related to tides
as.tidem <- function(tRef, latitude, name, amplitude, phase, debug=getOption("oceDebug"))
{
    oceDebug(debug, "as.tidem() {\n", sep="", unindent=1)
    if (missing(tRef))
        stop("tRef must be given")
    if (missing(latitude))
        stop("latitude must be given")
    if (missing(name))
        stop("name must be given")
    if (missing(amplitude))
        stop("amplitude must be given")
    if (missing(phase))
        stop("phase must be given")
    nname <- length(name)
    if (nname != length(amplitude))
        stop("lengths of name and amplitude must be equal but they are ", nname, " and ", length(amplitude))
    if (nname != length(phase))
        stop("lengths of name and phase must be equal but they are ", nname, " and ", length(phase))
    data("tidedata", package="oce", envir=environment())
    tidedata <- get("tidedata")
    tRef <- numberAsPOSIXct(3600 * round(as.numeric(tRef, tz="UTC") / 3600), tz="UTC")
    oceDebug(debug, "input head(name): ", paste(head(name), collapse=" "), "\n")
    oceDebug(debug, "input head(phase): ", paste(head(phase), collapse=" "), "\n")
    oceDebug(debug, "input head(amplitude): ", paste(head(amplitude), collapse=" "), "\n")

    freq <- rep(NA, nname)
    indices <- rep(NA, nname)
    ibad <- NULL
    for (i in seq_along(name)) {
        oceDebug(debug, "adjusting amplitude and phase for constituent '", name[i], "'\n", sep="")
        j <- which(tidedata$const$name==name[i])
        oceDebug(debug, "  inferred j=", j, " from constituent name\n", sep="")
        if (length(j)) {
            vuf <- tidemVuf(tRef, j=j, latitude=latitude)
            oceDebug(debug, "  inferred vuf=", deparse(vuf), "\n")
            indices[i] <- j
            amplitude[i] <- amplitude[i] * vuf$f
            phase[i] <- phase[i] - (vuf$v+vuf$u)*360
            freq[i] <- tidedata$const$freq[j]
        } else {
            ibad <- c(ibad, i)
        }
    }
    if (length(ibad)) {
        warning("the following constituents are not handled: '", paste(name[ibad], collapse="', '"), "'\n", sep="")
        indices <- indices[-ibad]
        name <- name[-ibad]
        amplitude <- amplitude[-ibad]
        phase <- phase[-ibad]
        freq <- freq[-ibad]
    }
    oceDebug(debug, "after vuf correction, head(name): ", paste(head(name), collapse=" "), "\n")
    oceDebug(debug, "after vuf correction, head(phase): ", paste(head(phase), collapse=" "), "\n")
    oceDebug(debug, "after vuf correction, head(amplitude): ", paste(head(amplitude), collapse=" "), "\n")
    oceDebug(debug, "} # as.tidem()\n", sep="", unindent=1)
    phase <- phase %% 360
    res <- new('tidem')
    res@data <- list(tRef=tRef,
                      const=indices,
                      name=name,
                      freq=freq,
                      amplitude=amplitude,
                      phase=phase,
                      p=rep(NA, length(name)))
    res@metadata$version <- 3
    res@processingLog <- processingLogAppend(res@processingLog, paste(deparse(match.call()), sep="", collapse=""))
    oceDebug(debug, "} # as.tidem()\n", sep="", unindent=1)
    res
}


#' Nodal Modulation Calculations for Tidem
#'
#' Carry out nodal modulation calculations for [tidem()]. This function is based directly
#' on `t_vuf` in the `T_TIDE` Matlab package (Pawlowicz et al., 2002),
#' which inherits from the Fortran code described by Foreman (1978).
#'
#' @param t a single time in [POSIXct()] format, with timezone `"UTC"`.
#'
#' @param j integer vector, giving indices of tidal constituents to use.
#'
#' @param latitude optional numerical value containing the latitude in degrees North.
#' If not provided, `u` in the return value will be a vector consisting of
#' repeated 0 value, and `f` will be a vector of repeated 1 value.
#'
#' @return A `list` containing
#' items named `v`, `u` and `f` as described in the `T_TIDE` documentation,
#' as well in Pawlowicz et al. (2002) and Foreman (1978).
#'
#' @author Dan Kelley translated this from the `t_vuf` function
#' of the `T_TIDE` Matlab package (see Pawlowicz et al. 2002).
#'
#' @examples
#' ## Look up values for the M2 constituent in Halifax Harbour, Canada.
#' data("tidedata")
#' j  <- which(tidedata$const$name=="M2")
#' tidemVuf(t=as.POSIXct("2008-01-22 18:50:24"), j=j, lat=44.63)
#'
#' @references
#' * Foreman, M. G. G., 1978. Manual for Tidal Currents Analysis and Prediction.
#' Pacific Marine Science Report. British Columbia, Canada: Institute of Ocean
#' Sciences, Patricia Bay.
#'
#' * Pawlowicz, Rich, Bob Beardsley, and Steve Lentz, 2002.  Classical tidal
#' harmonic analysis including error estimates in MATLAB using `T_TIDE`.
#' Computers and Geosciences, 28, 929-937.
#'
#' @family things related to tides
tidemVuf <- function(t, j, latitude=NULL)
{
    debug <- 0
    if (length(t) > 1)
        stop("t must be a single POSIXct item")
    data("tidedata", package="oce", envir=environment())
    tidedata <- get("tidedata")#, pos=globalenv())

    a <- tidemAstron(t)

    if (debug > 0) print(a)

    doodson <- cbind(tidedata$const$d1,
                     tidedata$const$d2,
                     tidedata$const$d3,
                     tidedata$const$d4,
                     tidedata$const$d5,
                     tidedata$const$d6)

    ##v=rem( const.doodson*astro+const.semi, 1);
    oceDebug(debug,
              "doodson[1,]=", doodson[1, ], "\n",
              "doodson[2,]=", doodson[2, ], "\n",
              "doodson[3,]=", doodson[3, ], "\n")
    v <- doodson %*% a$astro + tidedata$const$semi
    oceDebug(debug, "tidedata$const$semi[", j, "]=", tidedata$const$semi[j], "\n")
    v <- v - trunc(v)
    oceDebug(debug, "v[1:3]=", v[1:3], "\n")
    if (!is.null(latitude) && !is.na(latitude)) {
        if (abs(latitude) < 5) latitude <- sign(latitude) * 5
        slat <- sin(pi * latitude / 180)
        k <- which(tidedata$sat$ilatfac == 1)
        rr    <- tidedata$sat$amprat
        rr[k] <- rr[k] * 0.36309 * (1.0 - 5.0 * slat * slat) / slat
        k     <- which(tidedata$sat$ilatfac == 2)
        rr[k] <- rr[k] * 2.59808 * slat

        uu <- tidedata$sat$deldood %*% a$astro[4:6] + tidedata$sat$phcorr
        uu <- uu - trunc(uu)

        oceDebug(debug, "uu[1:3]=", uu[1:3], "\n")

        nsat <- length(tidedata$sat$iconst)
        ##nfreq <- length(tidedata$const$numsat)
        ## loop, rather than make a big matrix
        oceDebug(debug,
                  "tidedata$sat$iconst=", tidedata$sat$iconst, "\n",
                  "length(sat$iconst)=", length(tidedata$sat$iconst), "\n")
        fsum.vec <- vector("numeric", nsat)
        u.vec <- vector("numeric", nsat)
        for (isat in 1:nsat) {
            oceDebug(debug, "isat=", isat, "\n")
            use <- tidedata$sat$iconst == isat
            fsum.vec[isat] <- 1 + sum(rr[use] * exp(1i * 2 * pi * uu[use]))
            u.vec[isat] <- Arg(fsum.vec[isat]) / 2 / pi
            if (isat==8 && debug > 0) {
                cat("TEST at isat=8:\n")
                cat("fsum.vec[", isat, "]=", fsum.vec[isat], " (EXPECT  1.18531604917590 - 0.08028013402313i)\n")
                cat("u.vec[   ", isat, "]=", u.vec[isat], "       (EXPECT -0.01076294959868)\n")
            }
        }
        oceDebug(debug,
                  "uvec[", j, "]=", u.vec[j], "\n",
                  "fsum.vec[", j, "]=", fsum.vec[j], "\n")
        f <- abs(fsum.vec)
        u <- Arg(fsum.vec)/2/pi
        oceDebug(debug, "f=", f, "\n")
        oceDebug(debug, "u=", u, "\n")

        for (k in which(!is.na(tidedata$const$ishallow))) {
            ik <- tidedata$const$ishallow[k] + 0:(tidedata$const$nshallow[k] - 1)
            f[k] <- prod(f[tidedata$shallow$iname[ik]]^abs(tidedata$shallow$coef[ik]))
            u[k] <- sum(u[tidedata$shallow$iname[ik]]*tidedata$shallow$coef[ik])
            v[k] <- sum(v[tidedata$shallow$iname[ik]]*tidedata$shallow$coef[ik])
            if (debug>0 && k < 28)
                cat("k=", k, "f[k]=", f[k], " u[k]=", u[k], "v[k]=", v[k], "\n")
        }
        u <- u[j]
        v <- v[j]
        f <- f[j]
    } else {
        v <- v[j]
        u <- rep(0, length(j))
        f <- rep(1, length(j))
    }
    if (length(v) < length(u)) {
        warning("trimming u and f to get same length as v -- this is a bug")
        u <- u[seq_along(v)]
        f <- f[seq_along(v)]
    }
    list(v=v, u=u, f=f)
}


#' Astronomical Calculations for Tidem
#'
#' Do some astronomical calculations for [tidem()].  This function is based directly
#' on `t_astron` in the `T_TIDE` Matlab package (see Pawlowicz et al. 2002),
#' which inherits from the Fortran code described by Foreman (1978).
#'
#' @param t Either a time in `POSIXct` format (with `"UTC"` timezone,
#' or else odd behaviours may result),
#' or an integer. In the second case, it is converted to a time with
#' [numberAsPOSIXct()], using `tz="UTC"`.
#'
#' @return A `list` containing items named
#' `astro` and `ader` (see the `T_TIDE` documentation).
#'
#' @author Dan Kelley translated this from the `t_astron` function
#' of the `T_TIDE` Matlab package (see Pawlowicz et al. 2002).
#'
#' @examples
#' tidemAstron(as.POSIXct("2008-01-22 18:50:24"))
#'
#' @references
#' * Foreman, M. G. G., 1978. Manual for Tidal Currents Analysis and Prediction.
#' Pacific Marine Science Report. British Columbia, Canada: Institute of Ocean
#' Sciences, Patricia Bay.
#'
#' * Pawlowicz, Rich, Bob Beardsley, and Steve Lentz, 2002.  Classical tidal
#' harmonic analysis including error estimates in MATLAB using `T_TIDE`.
#' Computers and Geosciences, 28, 929-937.
#'
#' @family things related to tides
tidemAstron <- function(t)
{
    if (length(t) > 1)
        stop("t must be a single POSIXct item")
    debug <- FALSE
    if (is.numeric(t))
        t <- numberAsPOSIXct(t, tz="UTC")
    d <- as.numeric(difftime(t, ISOdatetime(1899, 12, 31, 12, 0, 0, tz="UTC"), units="days"))
    D <- d / 10000
    a <- matrix(c(1.0, d, D^2, D^3), ncol=1)

    oceDebug(debug, "d=", formatC(d, digits=10), "D=", D, "a=", a, "\n")

    scHcPcNpPp <-
        matrix(c(270.434164, 13.1763965268, -0.0000850,   0.000000039,
                 279.696678,  0.9856473354,  0.00002267,  0.000000000,
                 334.329556,  0.1114040803, -0.0007739,  -0.00000026,
                 -259.183275, 0.0529539222, -0.0001557,  -0.000000050,
                 281.220844,  0.0000470684,  0.0000339,   0.000000070),
               nrow=5, ncol=4, byrow=TRUE)
    astro <- ( (scHcPcNpPp %*% a) / 360 ) %% 1

    oceDebug(debug, "astro=", astro, "\n")

    rem <- as.numeric(difftime(t, trunc.POSIXt(t, units="days"), tz="UTC", units="days"))

    oceDebug(debug, "rem2=", rem, "\n")

    tau <- rem + astro[2, 1] - astro[1, 1]
    astro <- c(tau, astro)
    da <- matrix(c(0.0, 1.0, 2e-4*D, 3e-4*D^2), nrow=4, ncol=1)
    ader <- (scHcPcNpPp %*% da) / 360
    dtau <- 1 + ader[2, 1] - ader[1, 1]
    ader <- c(dtau, ader)
    list(astro=astro, ader=ader)
}

#' Change tidal constituent name from T-TIDE to Foreman convention
#'
#' This is used by [tidem()] to permit users to specify constituent names in either
#' the T-TIDE convention (see Pawlowicz et al. 2002) or Foreman convention
#' (see Foreman (1978). There are only two such instances:
#' `"MS"`, which gets translated to `"M8"`, and `"UPSI"`,
#' which gets translated to `"UPS1"`.
#'
#' @param names a vector of character values, holding constituent names
#'
#' @param debug an integer controlling warnings. If this is zero, then no warnings
#' are issued during processing; otherwise, as is the default, warnings are
#' issued for each conversion that is required.
#'
#' @return A vector of character values of tidal constituent names, in the Foreman naming convention.
#'
#' @references
#' Foreman, M. G. G., 1978. Manual for Tidal Currents Analysis and Prediction.
#' Pacific Marine Science Report. British Columbia, Canada: Institute of Ocean
#' Sciences, Patricia Bay.
#'
#' Pawlowicz, Rich, Bob Beardsley, and Steve Lentz, 2002.  Classical tidal
#' harmonic analysis including error estimates in MATLAB using `T_TIDE`.
#' Computers and Geosciences, 28, 929-937.
tidemConstituentNameFix <- function(names, debug=1)
{
    if ("MS" %in% names) {
        if (debug)
            warning("constituent name switched from T-TIDE 'MS' to Foreman (1978) 'M8'")
        names[names == "MS"] <- "M8"
    }
    if ("-MS" %in% names) {
        if (debug)
            warning("removed-constituent name switched from T-TIDE 'MS' to Foreman (1978) 'M8'")
        names[names == "-MS"] <- "-M8"
    }
    if ("UPSI" %in% names) {
        if (debug)
            warning("constituent name switched from T-TIDE 'UPSI' to Foreman (1978) 'UPS1'")
        names[names == "UPSI"] <- "UPS1"
    }
    if ("-UPSI" %in% names) {
        if (debug)
            warning("removed-constituent name switched from T-TIDE 'UPSI' to Foreman (1978) 'UPS1'")
        names[names == "-UPSI"] <- "-UPS1"
    }
    names
}


#' Fit a Tidal Model to a Timeseries
#'
#' The fit is done in terms of sine and cosine components at the indicated
#' tidal frequencies, with the amplitude and phase being calculated from the
#' resultant coefficients on the sine and cosine terms.
#'
#' The tidal constituents to be used in the analysis are specified as follows;
#' see \dQuote{Constituent Naming Convention}.
#'
#' 1. If `constituents` is not provided, then the constituent
#' list will be made up of the 69 constituents designated by Foreman as "standard".
#' These include astronomical frequencies and some shallow-water frequencies,
#' and are as follows: `c("Z0", "SA", "SSA", "MSM", "MM", "MSF", "MF",
#' "ALP1", "2Q1", "SIG1", "Q1", "RHO1", "O1", "TAU1", "BET1", "NO1", "CHI1",
#' "PI1", "P1", "S1", "K1", "PSI1", "PHI1", "THE1", "J1", "SO1", "OO1", "UPS1",
#' "OQ2", "EPS2", "2N2", "MU2", "N2", "NU2", "GAM2", "H1", "M2", "H2", "MKS2",
#' "LDA2", "L2", "T2", "S2", "R2", "K2", "MSN2", "ETA2", "MO3", "M3", "SO3",
#' "MK3", "SK3", "MN4", "M4", "SN4", "MS4", "MK4", "S4", "SK4", "2MK5", "2SK5",
#' "2MN6", "M6", "2MS6", "2MK6", "2SM6", "MSK6", "3MK7", "M8")`.
#'
#' 2. If the first item in `constituents` is the string
#' `"standard"`, then a provisional list is set up as in Case 1, and then
#' the (optional) rest of the elements of `constituents` are examined, in
#' order.  Each of these constituents is based on the name of a tidal
#' constituent in the Foreman (1978) notation.  (To get the list, execute
#' `data(tidedata)` and then execute `cat(tideData$name)`.)  Each
#' named constituent is added to the existing list, if it is not already there.
#' But, if the constituent is preceded by a minus sign, then it is removed
#' from the list (if it is already there).  Thus, for example,
#' `constituents=c("standard", "-M2", "ST32")` would remove the M2
#' constituent and add the ST32 constituent.
#'
#' 3. If the first item is not `"standard"`, then the list of
#' constituents is processed as in Case 2, but without starting with the
#' standard list. As an example, `constituents=c("K1", "M2")` would fit
#' for just the K1 and M2 components. (It would be strange to use a minus sign
#' to remove items from the list, but the function allows that.)
#'
#' In each of the above cases, the list is reordered in frequency prior to the
#' analysis, so that the results of [summary,tidem-method()] will be in a
#' familiar form.
#'
#' Once the constituent list is determined, `tidem` prunes the elements of
#' the list by using the Rayleigh criterion, according to which two
#' constituents of frequencies \eqn{f_1}{f1} and \eqn{f_2}{f2} cannot be
#' resolved unless the time series spans a time interval of at least
#' \eqn{rc/(f_1-f_2)}{rc/(f1-f2)}.
#'
#' Finally, `tidem` looks in the remaining constituent list to check
#' that the application of the Rayleigh criterion has not removed any of the
#' constituents specified directly in the `constituents` argument. If
#' any are found to have been removed, then they are added back. This last
#' step was added on 2017-12-27, to make `tidem` behave the same
#' way as the Foreman (1978) code, as illustrated in his
#' Appendices 7.2 and 7.3. (As an aside, his Appendix 7.3 has some errors,
#' e.g. the frequency for the 2SK5 constituent is listed there (p58) as
#' 0.20844743, but it is listed as 0.2084474129 in his Appendix 7.1 (p41).
#' For this reason, the frequency comparison is relaxed to a `tol`
#' value of `1e-7` in a portion of the oce test suite
#' (see `tests/testthat/test_tidem.R` in the source).
#'
#' A specific example may be of help in understanding the removal of unresolvable
#' constituents. For example, the `data(sealevel)` dataset is of length
#' 6718 hours, and this is too short to resolve the full list of constituents,
#' with the conventional (and, really, necessary) limit of `rc=1`.
#' From Table 1 of Foreman (1978), this timeseries is too short to resolve the
#' `SA` constituent, so that `SA` will not be in the resultant.
#' Similarly, Table 2 of Foreman (1978) dictates the removal of
#' `PI1`, `S1` and `PSI1` from the list. And, finally,
#' Table 3 of Foreman (1978) dictates the removal of
#' `H1`, `H2`, `T2` and `R2`, and since that document
#' suggests that `GAM2` be subsumed into `H1`,
#' then if `H1` is already being deleted, then
#' `GAM2` will also be deleted.
#'
#' A summary of constituents may be found with:
#' \preformatted{
#' data(tidedata)
#' print(tidedata$const)
#' }
#'
#' @param t A `sealevel` object created with
#' [read.sealevel()] or [as.sealevel()], or a vector of
#' times. In the former case, time is part of the object, so `t` may not
#' be given here.  In the latter case, `tidem` needs a way to determine
#' time, so `t` must be given.
#'
#' @param x an optional numerical vector holding something that varies with
#' time.  This is ignored if `t` is a [sealevel-class] object,
#' in which case it is inferred as `t[["elevation"]]`.
#'
#' @param constituents an optional character vector holding the names
#' of tidal constituents to which the fit is done (see \dQuote{Details}
#' and \dQuote{Constituent Naming Convention}.)
#' @template tideconst
#'
#' @param infer a list of constituents to be inferred from
#' fitted constituents according to the method outlined
#' in Section 2.3.4 of Foreman (1978).
#' If `infer` is `NULL`, the default, then
#' no such inferences are made. Otherwise, some constituents
#' are computed based on other constituents, instead of being
#' determined by regression at the proper frequency.
#' If provided, `infer` must be a list containing
#' four elements:
#' `name`, a vector of strings naming the constituents to be
#' inferred; `from`, a vector of strings naming the fitted
#' constituents used as the sources for those inferences (these
#' source constituents are added to the regression list, if they
#' are not already there);
#' `amp`, a numerical vector of factors to be applied to the
#' source amplitudes; and `phase`, a numerical vector of angles,
#' in degrees, to be subtracted from the source phases. For example,
#' Following Foreman (1998), if any of the `name` items
#' have already been computed, then the suggested inference is ignored,
#' and the already-computed values are used.
#'\preformatted{
#' infer=list(name=c("P1","K2"),
#'            from=c("K1", "S2"),
#'            amp=c(0.33093, 0.27215),
#'            phase=c(-7.07, -22.4)
#'}
#' means that the amplitude of `P1` will be set as 0.33093 times the calculated amplitude
#' of `K1`, and that the `P1` phase will be set to the `K1` phase,
#' minus an offset of `-7.07` degrees.
#' (This example is used in the Foreman (1978) discussion of a
#' Fortran analysis code and also in Pawlowicz et al. (2002) discussion
#' of the T_TIDE Matlab code.
#' Rounded to the 0.1mm resolution of values reported in Foreman (1978)
#' and Pawlowicz et al. (2002),
#' the `tidem` results have root-mean-square amplitude difference
#' to Foreman's (1978) Appendix 7.3 of 0.06mm; by comparison,
#' the results in Table 1 of Pawlowicz et al. (2002) agree with Foreman's
#' results to RMS difference 0.04mm.)
#'
#' @param latitude if provided, the latitude of the observations.  If not
#' provided, `tidem` will try to infer this from `sl`.
#'
#' @param rc the value of the coefficient in the Rayleigh criterion.
#'
#' @param regress function to be used for regression, by default
#' [lm()], but could be for example `rlm` from the
#' `MASS` package.
#'
#' @template debugTemplate
#'
#' @return An object of [tidem-class], consisting of
#' \item{const}{constituent number, e.g. 1 for `Z0`, 1 for `SA`,
#' etc.} \item{model}{the regression model} \item{name}{a vector of constituent
#' names, in non-subscript format, e.g. "`M2`".} \item{frequency}{a vector
#' of constituent frequencies, in inverse hours.} \item{amplitude}{a vector of
#' fitted constituent amplitudes, in metres.} \item{phase}{a vector of fitted
#' constituent phase.  NOTE: The definition of phase is likely to change as
#' this function evolves.  For now, it is phase with respect to the first data
#' sample.} \item{p}{a vector containing a sort of p value for each
#' constituent.  This is calculated as the average of the p values for the
#' sine() and cosine() portions used in fitting; whether it makes any sense is
#' an open question.}
#'
#' @section Bugs:
#'
#' \enumerate{
#' \item This function is not fully developed yet, and both the
#' form of the call and the results of the calculation may change.
#'
#' \item The reported `p` value may make no sense at all, and it might be
#' removed in a future version of this function. Perhaps a significance level
#' should be presented, as in the software developed by both Foreman and
#' Pawlowicz.
#'
#' }
#'
#' @section Constituent Naming Convention:
#'
#' `tidem` uses constituent names that follow the convention
#' set by Foreman (1978). This convention is slightly different
#' from that used in the T-TIDE package of Pawlowicz et al.
#' (2002), with Foreman's `UPS1` and `M8` becoming
#' `UPSI` and `MS` in T-TIDE. To permit the use of either notation,
#' [tidem()] uses [tidemConstituentNameFix()] to
#' convert from T-TIDE names to the
#' Foreman names, issuing warnings when doing so.
#'
#' @section Agreement with `T_TIDE` results:
#'
#' The `tidem` amplitude and phase results, obtained with
#'\preformatted{
#'tidem(sealevelTuktoyaktuk, constituents=c("standard", "M10"),
#'      infer=list(name=c("P1", "K2"),
#'                 from=c("K1", "S2"),
#'                 amp=c(0.33093, 0.27215),
#'                 phase=c(-7.07, -22.40))),
#'}
#' are identical the `T_TIDE` values listed in
#' Table 1 of Pawlowicz et al. (2002),
#' after rounding amplitude and phase to 4 and 2 digits past
#' the decimal place, to match the format of the table.
#'
#' @author Dan Kelley
#'
#' @references
#'
#' Foreman, M. G. G., 1978. Manual for Tidal Currents Analysis and Prediction.
#' Pacific Marine Science Report. British Columbia, Canada: Institute of Ocean
#' Sciences, Patricia Bay.
#'
#' Foreman, M. G. G., Neufeld, E. T., 1991.  Harmonic tidal analyses of long
#' time series.  International Hydrographic Review, 68 (1), 95-108.
#'
#' Leffler, K. E. and D. A. Jay, 2009.  Enhancing tidal harmonic analysis:
#' Robust (hybrid) solutions.  Continental Shelf Research, 29(1):78-88.
#'
#' Pawlowicz, Rich, Bob Beardsley, and Steve Lentz, 2002.  Classical tidal
#' harmonic analysis including error estimates in MATLAB using `T_TIDE`.
#' Computers and Geosciences, 28, 929-937.
#'
#' @examples
#' library(oce)
#' # The demonstration time series from Foreman (1978),
#' # also used in T_TIDE (Pawlowicz, 2002).
#' data(sealevelTuktoyaktuk)
#' tide <- tidem(sealevelTuktoyaktuk)
#' summary(tide)
#'
#' # AIC analysis
#' extractAIC(tide[["model"]])
#'
#' # Fake data at M2
#' t <- seq(0, 10*86400, 3600)
#' eta <- sin(0.080511401 * t * 2 * pi / 3600)
#' sl <- as.sealevel(eta)
#' m <- tidem(sl)
#' summary(m)
#'
#' @family things related to tides
tidem <- function(t, x, constituents, infer=NULL,
                  latitude=NULL, rc=1, regress=lm,
                  debug=getOption("oceDebug"))
{
    oceDebug(debug, "tidem(t, x,\n", sep="", unindent=1)
    oceDebug(debug, "      constituents=", if (missing(constituents)) "(missing)" else paste("c('", paste(constituents, collapse="', '"), "')", sep=""), ",\n", sep="", unindent=1)
    oceDebug(debug, "      latitude=", if (is.null(latitude)) "NULL" else latitude, ",\n", sep="", unindent=1)
    oceDebug(debug, "      rc=", rc, ",\n", sep="", unindent=1)
    oceDebug(debug, "      debug=", debug, ") {\n", sep="", unindent=1)
    cl <- match.call()
    if (missing(t))
        stop("must supply 't', either a vector of times or a sealevel object")
    if (inherits(t, "sealevel")) {
        sl <- t
        t <- sl[["time"]]
        x <- sl[["elevation"]]
        if (is.null(latitude))
            latitude <- sl[["latitude"]]
    } else {
        if (missing(x))
            stop("must supply 'x', since the first argument is not a sealevel object")
        if (inherits(x, "POSIXt")) {
            warning("tidem() switching first 2 args to permit old-style usage")
            tmp <- x
            x <- t
            t <- tmp
        }
        if (length(x) != length(t))
            stop("lengths of 'x' and 't' must match, but they are ", length(x), " and ", length(t), " respectively")
        if (inherits(t, "POSIXt")) {
            t <- as.POSIXct(t)
        } else {
            stop("t must be a vector of POSIXt times")
        }
        sl <- as.sealevel(x, t)
    }

    ## Check infer extensively, to prevent weird errors for e.g. an improperly-named
    ## constituent.
    data("tidedata", package="oce", envir=environment())
    tidedata <- get("tidedata")#, pos=globalenv())
    tc <- tidedata$const
    ntc <- length(tc$name)
    ## 'infer' sanity-check
    if (!is.null(infer)) {
        if (!is.list(infer))
            stop("infer must be a list")
        if (length(infer) != 4)
            stop("infer must hold 4 elements")
        if (!all.equal(sort(names(infer)), c("amp", "from", "name", "phase")))
            stop("infer must contain 'name', 'from', 'amp', and 'phase', and nothing else")
        if (!is.character(infer$name))
            stop("infer$name must be a vector of character strings")
        infer$name <- tidemConstituentNameFix(infer$name)
        if (!is.character(infer$from))
            stop("infer$from must be a vector of character strings")
        infer$from <- tidemConstituentNameFix(infer$from)
        if (length(infer$name) != length(infer$from))
            stop("lengths of infer$name and infer$from must be equal")
        if (length(infer$name) != length(infer$amp))
            stop("lengths of infer$name and infer$amp must be equal")
        if (length(infer$name) != length(infer$phase))
            stop("lengths of infer$name and infer$phase must be equal")
        for (n in infer$name) {
            if (!(n %in% tc$name))
                stop("infer$name value '", n, "' is not a known tidal constituent; see const$name in ?tidedata")
        }
        for (n in infer$from) {
            if (!(n %in% tc$name))
                stop("infer$from value '", n, "' is not a known tidal constituent; see const$name in ?tidedata")
        }
    }                                  # 'infer' sanity-check

    ## The arguments seem to be OK, so start the actual analysis now.
    startTime <- t[1]
    endTime <- tail(t, 1)
    years <- as.numeric(difftime(endTime, startTime, units="secs")) / 86400 / 365.25
    if (years > 18.6)
        warning("Time series spans 18.6 years, but tidem() is ignoring this important fact")

    standard <- tc$ikmpr > 0
    addedConstituents <- NULL
    if (missing(constituents)) {
        ## Default 'name', 'freq', 'kmpr' and 'indices'; drop Z0 because we infer it directly.
        ##> message("head(name)=", paste(head(name), collapse=" "), " BEFORE")
        name <- tc$name[standard]
        ##> message("head(name)=", paste(head(name), collapse=" "), " AFTER")
        freq <- tc$freq[standard]
        kmpr <- tc$kmpr[standard]
        indices <- seq(1:ntc)[standard] # NB. Z0 need not be dropped; we work with indices later anyway
        oceDebug(debug, "starting with ", length(name), " default constituents: ", paste(name, collapse=" "), sep="", "\n")
    } else {
        ## Build up 'name'; later, infer 'indices' and thereby 'freq' and 'kmpr'.
        name <- NULL
        #nconst <- length(constituents)
        #oceDebug(debug, "tc$name[standard]=", paste(tc$name[standard], collapse=" "), "\n", sep="")
        for (i in seq_along(constituents)) {
            ## if (debug > 0) cat("[", constituents[i], "]\n", sep="")
            if (constituents[i] == "standard") {
                ## must be first!
                if (i != 1)
                    stop("\"standard\" must occur first in constituents list")
                ##OLD name <- tc$name[standard][-1] # the -1 is to remove Z0
                name <- tc$name[standard]
            } else {
                ##oceDebug(debug > 1, "before renaming: ", paste(constituents), "\n")
                constituents <- tidemConstituentNameFix(constituents)
                ##oceDebug(debug > 1, "after renaming: ", paste(constituents), "\n")
                if (substr(constituents[i], 1, 1) == "-") {
                    ## Case 1: removal. Require a valid name, and warn if not in the list already.
                    nameRemove <- substr(constituents[i], 2, nchar(constituents[i]))
                    if (1 != sum(tc$name == nameRemove))
                        stop("'", nameRemove, "' is not a known tidal constituent; try one of: ",
                             paste(tc$name, collapse=" "))
                    remove <- which(name == nameRemove)
                    oceDebug(debug > 1, "removed '", nameRemove, "'\n", sep="")
                    if (0 == length(remove))
                        warning("'", nameRemove, "' is not in the list of constituents currently under study", sep="")
                    else
                        name <- name[-remove]
                } else {
                    ## Case 2: addition. Require a valid name, and ignore repeat requests.
                    add <- which(tc$name == constituents[i])
                    if (1 != length(add))
                        stop("'", constituents[i], "' is not a known tidal constituent (line 1093)")
                    if (!(constituents[i] %in% name)) {
                        name <- c(name, tc$name[add])
                        addedConstituents <- c(addedConstituents, add)
                    }
                }
                ##oceDebug(debug, "using names= ", paste(name, collapse=" "), "\n")
            }
        }
    }
    oceDebug(debug, "will fit for ", length(name), " constituents: ", paste(name, collapse=" "), "\n", sep="")
    ## We let users add "Z0" as a constituent, but we remove it now since the
    ## regression will have an intercept and that becomes Z0.
    fitForZ0 <- "Z0" %in% name
    oceDebug(debug, "fitForZ0=", fitForZ0, "\n")
    ## All of the names should be valid from the above, but to protect against code changes,
    ## we check to be sure.
    if (any(!(name %in% tc$name))) {
        bad <- NULL
        for (n in name)
            if (!(n %in% tc$name))
                bad <- c(bad, n)
        stop("unknown constituent names: ", paste(bad, collapse=" "), " (please report this error to developer)")
    }
    ## Infer indices from the names, sort them as in the tidal-constituent
    ## table, tc, and then look up freq and kmpr from that table.
    indices <- sort(unlist(lapply(name,function(name) which(tc$name==name))))
    name <- tc$name[indices]
    freq <- tc$freq[indices]
    kmpr <- tc$kmpr[indices]
    ## if (debug > 2) {
    ##     cat("next is at line 875 (initial setup, before 'infer' handled)\n")
    ##     print(data.frame(name=name, indices=indices, freq=freq, kmpr=kmpr))
    ## }

    nc <- length(name)

    ## Check Rayleigh criterion
    interval <- diff(range(as.numeric(sl@data$time), na.rm=TRUE)) / 3600 # in hours
    oceDebug(debug, "interval=", interval, " hours\n")
    dropTerm <- NULL
    for (i in 1:nc) {
        cc <- which(tc$name == kmpr[i])
        if (length(cc)) {
            cannotFit <- (interval * abs(freq[i]-tc$freq[cc])) < rc
            oceDebug(debug, "i=", i, ", name=", name[i], ", kmpr[", i, "]=", kmpr[i],", cannotFit=", cannotFit,"\n", sep="")
            if (cannotFit) {
                dropTerm <- c(dropTerm, i)
            }
        }
    }
    oceDebug(debug, "before trimming constituents for Rayleigh condition, name[1:", length(name), "]=", paste(name, collapse=" "), sep="", "\n")
    if (length(dropTerm) > 0) {
        cat("Note: the tidal record is too short to fit for constituents: ", paste(name[dropTerm], collapse=" "), "\n")
        indices <- indices[-dropTerm]
        name <- name[-dropTerm]
        freq <- freq[-dropTerm]
        kmpr <- kmpr[-dropTerm]
    }
    oceDebug(debug, "after trimming constituents for Rayleight condition, name[1:", length(name), "]=", paste(name, collapse=" "), sep="", "\n")
    ## Ensure that any added constituents are in the list, i.e. prevent
    ## the Rayleigh criterion from trimming them. (Before work on
    ## issue 1350, they would simply be dropped if they failed the Rayleigh
    ## criterion. Although that was a sensible choice, it was decided
    ## on 2017-12-27, whilst working on issue 1350, to make tidem() do the
    ## the same thing as the Foreman 1978 code as exemplified in his appendices
    ## 7.2 and 7.3.)
    if (length(addedConstituents)) {
        oceDebug(debug, "addedConstituents=", paste(addedConstituents, collapse=" "), "\n")
        for (a in addedConstituents) {
            ## Avoid adding constituents that we already have
            if (!(tc$name[a] %in% name)) {
                message("ADDING:")
                message("  tc$name[a=", a, "]='", tc$name[a], "'", sep="")
                message("  tc$freq[a=", a, "]='", tc$freq[a], "'", sep="")
                message("  tc$kmpr[a=", a, "]='", tc$kmpr[a], "'", sep="")
                indices <- c(indices, which(tc$name==name[a]))
                name <- c(name, tc$name[a])
                freq <- c(freq, tc$freq[a])
                kmpr <- c(kmpr, tc$kmpr[a])
            }
        }
    }
    oceDebug(debug, "after adding new constituents, name[1:", length(name), "]=", paste(name, collapse=" "), sep="", "\n")
    ## Ensure that we fit for any infer$from constituents, *regardless* of whether
    ## those consitituents are permitted by the Rayleigh criterion.
    if (!is.null(infer)) {
        for (n in c(infer$from)) {
            if (!(n %in% name)) {
                a <- which(tc$name == n)
                indices <- c(indices, a)
                name <- c(name, tc$name[a])
                freq <- c(freq, tc$freq[a])
                kmpr <- c(kmpr, tc$kmpr[a])
                message("fitting for infer$from=", n, ", even though the Rayleigh Criterion would exclude it")
            }
        }
    }

    ## sort constituents by index (which, among other things, ensures that Z0 is at the start, if it exists)
    oindices <- order(indices)
    indices <- indices[oindices]
    name <- name[oindices]
    freq <- freq[oindices]
    kmpr <- kmpr[oindices]
    nc <- length(name)
    oceDebug(debug, "name[1:", length(name), "]: ", paste(name, collapse=" "), "\n", sep="")
    rm(oindices) # clean up namespace
    if (0 == nc)
        stop("cannot fit for any constituents")
    elevation <- sl[["elevation"]]
    time <- sl[["time"]]
    nt <- length(elevation)
    x <- array(dim=c(nt, 2 * nc))
    oceDebug(debug, vectorShow(nc))
    oceDebug(debug, vectorShow(dim(x)))
    x[, 1] <- rep(1, nt)
    pi <- 4 * atan2(1, 1)
    rpd <- atan2(1, 1) / 45            # radians per degree
    ##tRef <- ISOdate(1899, 12, 31, 12, 0, 0, tz="UTC") # was this ever used?
    ##tRef <- centralTime # used previous to "dk" branch early 2018
    tRef <- numberAsPOSIXct(3600 * round(mean(as.numeric(time, tz="UTC")) / 3600), tz="UTC")
    ## message("  tRef=", format(tRef, "%Y-%m-%d %H:%M:%S"), " (in tidem)")
    hour2pi <- 2 * pi * (as.numeric(time) - as.numeric(tRef)) / 3600
    oceDebug(debug, "tRef=", tRef, ", nc=", nc, ", length(name)=", length(name), "\n")
    ##    cat(sprintf("hour[1] %.3f\n",hour[1]))
    ##    cat(sprintf("hour.offset[1] %.3f\n",hour.offset[1]))
    for (i in 1:nc) {
        oceDebug(debug, "setting ", i, "-th coefficient (name=", name[i], " freq=", freq[i], " cph)", "\n", sep="")
        ft <- freq[i] * hour2pi
        x[, 1 + 2*(i-1)] <- cos(ft)
        x[, 2 + 2*(i-1)] <- sin(ft)
    }
    name2 <- matrix(rbind(paste(name, "_C", sep=""), paste(name, "_S", sep="")), nrow=length(name), ncol=2)
    dim(name2) <- c(2 * length(name), 1)
    colnames(x) <- name2
    #model <- lm(elevation ~ x, na.action=na.exclude)
    oceDebug(debug, "about to do regression\n")
    if ("Z0_S" %in% colnames(x)) {
        x <- x[, -which("Z0_S" == colnames(x))]
        oceDebug(debug, "model has Z0, so trimming the sin(freq*time) column\n")
    }
    if (debug) {
        cat("x[,1]:\n");print(x[,1])
        cat("x[,2]:\n");print(x[,2])
    }
    model <- regress(elevation ~ x - 1, na.action=na.exclude)
    if (debug > 0) {
        cat("regression worked OK; the results are as follows:\n")
        print(summary(model))
    }
    coef  <- model$coefficients
    p.all <- if (4 == dim(summary(model)$coefficients)[2])
        summary(model)$coefficients[, 4]
    else
        rep(NA, length=1+nc)
    amplitude <- phase <- p <- vector("numeric", length=nc)
    oceDebug(debug, vectorShow(nc))
    oceDebug(debug, vectorShow(phase))
    ## FIXME: should do offset/trend removal explicitly
    ## amplitude[1] <- coef[1]
    ## phase[1] <- 0
    ## p[1] <- p.all[1]
    oceDebug(debug, vectorShow(name))
    ic <- 1
    for (i in seq_len(nc)) {
        if (name[i] == "Z0") { # Z0 has Z0_C but no Z0_S (since zero is a degenerate regression variable)
            if (i != 1)
                stop("Z0 should be at the start of the regression coefficients. Please report this to developer.")
            j <- which(tidedata$const$name==name[i])
            vuf <- tidemVuf(tRef, j=j, latitude=latitude)
            amplitude[i] <- coef[ic]
            phase[i] <- 0
            p[i] <- p.all[ic]
            oceDebug(debug, "processed coefs at i=", i, ", ic=", ic, ", name=", name[i], ", f=", vuf$f, ", angle adj=", (vuf$u+vuf$v)*360, ", amplitude=", amplitude[i], ", phase=", phase[i], ", p=", p[i], "\n", sep="")
            ic <- ic + 1
        } else {
            C <- coef[ic]              # coefficient on cos(t)
            S <- coef[ic+1]            # coefficient on sin(t)
            amplitude[i] <- sqrt(S^2 + C^2)
            ## Calculate phase from the coefficients on sin() and cos().  Generally,
            ##    cos(t - phase) == cos(phase)*cos(t) + sin(phase)*sin(t)
            ## By the definition of the regression model, we have
            ##    cos(t - phase) == c * cos(t) + s * sin(t)
            ## and thus phase is defined by
            ##    tan(phase) == s/c
            phase[i] <- atan2(S, C)
            ## Adjust amplitude phase, as in ~/src/foreman/tide12_r2.f:405
            j <- which(tidedata$const$name==name[i])
            vuf <- tidemVuf(tRef, j=j, latitude=latitude)
            amplitude[i] <- amplitude[i] / vuf$f
            p[i] <- 0.5 * (p.all[ic+1] + p.all[ic])
            oceDebug(debug, "processed coefs at i=", i, ", ic=", ic, ", name=", name[i], ", S=", S, ", C=", C, ", f=", vuf$f, ", angle adj=", (vuf$u+vuf$v)*360, ", amplitude=", amplitude[i], ", phase=", phase[i], ", p=", p[i], "\n", sep="")
            ic <- ic + 2
        }
    }
    oceDebug(debug, vectorShow(phase))
    phase <- phase * 180 / pi
    phase <- ifelse(phase < -360, 720 + phase, phase)
    phase <- ifelse(phase < 0, 360 + phase, phase)

    ## FIXME: if 'inference calculation' is to be done, it should match
    ##     ~/src/t_tide_v1.3beta/t_tide.m:468
    ##     ~/src/foreman/tide12_r2.f:422

    ##FTESTING if (FALSE) { # FIXME
    ##FTESTING ## The regression gives us an intercept, which we call Z0
    ##FTESTING indices <- c(1, indices) # the index for Z0 is 1
    ##FTESTING name <- c("Z0", name)
    ##FTESTING freq <- c(0, freq)
    ##FTESTING }

    ## Do Greenwich phase corerrection, if `infer` is TRUE
    C <- unlist(lapply(name, function(n) which(n == tidedata$const$name)))
    vuf <- tidemVuf(tRef, j=C, latitude=latitude)
    oceDebug(debug, vectorShow(freq))
    oceDebug(debug, vectorShow(phase))
    oceDebug(debug, vectorShow(vuf$u))
    oceDebug(debug, vectorShow(vuf$v))
    phase <- phase + (vuf$v+vuf$u)*360
    phase <- ifelse(phase < 0, phase+360, phase)
    phase <- ifelse(phase > 360, phase-360, phase)

    ## Handle (optional) inferred constituents. We know that
    ## this list is well-formed because of extensive tests near
    ## the start of this function.
    if (!is.null(infer)) {
        if (debug > 0) {
            cat("BEFORE inference:\n")
            print(data.frame(name=name, freq=round(freq,6), amplitude=round(amplitude,4)))
        }
        for (n in seq_along(infer$name)) {
            oceDebug(debug, "n=", n, "; handling inferred constituent ", infer$name[n], "\n")
            iname <- which(tc$name == infer$name[n])[1]
            oceDebug(debug, "infer$name[", n, "]='", infer$name[n], "' yields iname=", iname, "\n", sep="")
            oceDebug(debug, "iname=", iname, "\n")
            ## NOTE: we know that infer$from has been fitted for, because we forced it to be,
            ## irrespective of the Rayleight Criterion. Still, we test here, in case the code
            ## gets changed later.
            if (infer$from[n] %in% name) {
                ifrom <- which(name == infer$from[n])[1]
                if (infer$name[n] %in% name) {
                    ##message("name already in list")
                    iname <- which(name == infer$name[n])[1]
                    ## Update, skipping 'indices', 'name' and 'freq', since they are already OK.
                    amplitude[iname] <- infer$amp[n] * amplitude[ifrom]
                    phase[iname] <- phase[ifrom] - infer$phase[n]
                    p[iname] <- p[ifrom]
                    oceDebug(debug, "replace existing ", name[iname], " based on ", name[ifrom], " (", freq[ifrom], " cph)\n", sep="")
                    warning("inferring '", infer$name[n], "' which is already included in the regression. Foreman says to skip it; unsure on what T_TIDE does\n")
                } else {
                    ## We append new values at the end, knowing that they will get
                    ## shifted back to their proper positions when we reorder the
                    ## whole solution, after handling these inferences.
                    ##
                    ## The first step is to adjust the amp and phase of infer$from; this
                    ## is done based on formulae in Foreman (1978) sec 2.3.4. It looks
                    ## as though t_tide.m on and after about line 472 is doing a
                    ## similar thing, although the numbers do not agree exactly,
                    ## as shown in issue 1351, code 1351c.R.
                    ##TTIDE ## below is some broken code. It was not working,
                    ##TTIDE ## so I gave up and just coded in Foreman's equations
                    ##TTIDE ## directly.
                    ##TTIDE ## snarg=nobsu*pi*(fi(ii)   -fu(jref(ii)) )*dt;
                    ##TTIDE ##message("T_TIDE")
                    ##TTIDE tRangeHours <- diff(range(as.numeric(t))) / 3600
                    ##TTIDE freqName <- tc$freq[which(infer$name[n] == tc$name)]
                    ##TTIDE freqFrom <- tc$freq[which(infer$from[n] == tc$name)]
                    ##TTIDE snarg <- pi * tRangeHours * (freqName - freqFrom)
                    ##TTIDE ## t_tide.m:473 scarg=sin(snarg)./snarg;
                    ##TTIDE scarg <- sin(snarg) / snarg
                    ##TTIDE ##message("  snarg=", snarg, ",  scarg=", scarg)
                    ##TTIDE ## t_tide.m 310
                    ##TTIDE ## mu=length(fu); % # base frequencies
                    ##TTIDE ## t_tide.m 311
                    ##TTIDE ## mi=length(fi); % # inferred
                    ##TTIDE ## t_tide.m:449
                    ##TTIDE ## [v,u,f]=t_vuf(ltype,centraltime,[ju;jinf],lat);
                    ##TTIDE ## t_tide.m:451
                    ##TTIDE ## vu=(v+u)*360; % total phase correction (degrees)
                    ##TTIDE ## t_tide.m:476
                    ##TTIDE ## pearg=2*pi*(vu(mu+ii)-vu(jref(ii))+inf.ph(ii))/360;
                    ##TTIDE if (debug > 1) {
                    ##TTIDE     cat("vuf for 'name' follows (arg j=", which(tc$name==infer$name[n])[1], ")\n")
                    ##TTIDE     print(tidemVuf(tRef, which(tc$name==infer$name[n])[1], latitude=latitude))
                    ##TTIDE     cat("vuf for 'from' follows (arg j=", which(tc$name==infer$from[n])[1], ")\n")
                    ##TTIDE     print(tidemVuf(tRef, which(tc$name==infer$name[n])[1], latitude=latitude))
                    ##TTIDE }
                    ##TTIDE vufName <- tidemVuf(tRef, which(tc$name==infer$name[n])[1], latitude=latitude)
                    ##TTIDE vuName <- (vufName$v + vufName$u) * 360
                    ##TTIDE vufFrom <- tidemVuf(tRef, which(tc$name==infer$from[n])[1], latitude=latitude)
                    ##TTIDE vuFrom <- (vufFrom$v + vufFrom$u) * 360
                    ##TTIDE pearg <- 2 * pi * (vuName - vuFrom + infer$phase[n]) / 360
                    ##TTIDE ## t_tide.m:477
                    ##TTIDE ## pcfac=inf.amprat(ii).*f(mu+ii)./f(jref(ii)).*exp(i*pearg);
                    ##TTIDE ## Relates loosely to Foreman (1978 sec2.3.4 p28) "S"
                    ##TTIDE pcfac <- infer$amp[n] * vufName$f / vufFrom$f * cos(pearg)
                    ##TTIDE ## t_tide.m:478
                    ##TTIDE ## pcorr=1+pcfac.*scarg;
                    ##TTIDE pcorr <- 1 + pcfac * scarg
                    ##TTIDE ##message("  pearg=", pearg, ", pcfac=", pcfac)
                    ##TTIDE ##message("  pcorr=", pcorr, " (should divide infer amp by this)")
                    ##TTIDE ##message("  new amp for (", infer$from[n], ") might be=", pcorr*amplitude[ifrom])
                    ##
                    ## Foreman (1978) sec 2.3.4.
                    ## Notation: suffices "1" and "2" refer to "from" and "name" here.
                    i1 <- which(tc$name==infer$from[n])[1]
                    i2 <- which(tc$name==infer$name[n])[1]
                    oceDebug(1+debug, "tRef=", format(tRef, "%Y-%m-%d %H:%M:%S"),
                             ", i1=", i1, ", i2=", i2, ", lat=", latitude, "\n")
                    vuf12 <- tidemVuf(tRef, c(i1, i2), latitude=latitude)
                    #vuf2 <- tidemVuf(tRef, i2, latitude=latitude)
                    f1 <- vuf12$f[1]
                    f2 <- vuf12$f[2]
                    oceDebug(1+debug, "f1=", f1, ", f2=", f2, "\n")
                    ## FIXME: what is unit of u and v? t_tide.m:482 suggests it is degrees
                    ## Foreman's tide12_r2.f:399 suggests U and V are in cycles,
                    ## and this is consistent with Pawlowicz's t_tide.m:451
                    ## We convert vu1 and vu2 to be in degrees, as t_tide.m does
                    vu1 <- (vuf12$v[1] + vuf12$u[1]) * 360
                    vu2 <- (vuf12$v[2] + vuf12$u[2]) * 360
                    oceDebug(debug, "vu1=", vu1, ", vu2=", vu2, "\n")
                    sigma1 <- tc$freq[i1]
                    sigma2 <- tc$freq[i2]
                    oceDebug(debug, "sigma1=", sigma1, ", sigma2=", sigma2, "\n")
                    ## tmp is pi*N*(sigma2-sigma1) in Foreman
                    tmp <- pi * interval * (sigma2 - sigma1)
                    r12 <- infer$amp[n]
                    ## FIXME: sign for Foreman?
                    zeta <- infer$phase[n]
                    S <- r12 * (f2/f1) * sin(tmp) * sin(rpd*(vu2-vu1+zeta)) / tmp
                    C <- 1 + r12 * (f2/f1) * sin(tmp) * cos(rpd*(vu2-vu1+zeta)) / tmp
                    oceDebug(debug, "tmp=", tmp, ", S=", S, ", C=", C, ", sqrt(S^2+C^2)=", sqrt(S^2+C^2), "\n")
                    oceDebug(1+debug, infer$from[n], "amplitude, old=", amplitude[ifrom], ", new=", amplitude[ifrom]/sqrt(S^2+C^2), "\n")
                    amplitude[ifrom] <- amplitude[ifrom] / sqrt(S^2+C^2)
                    oceDebug(1+debug, infer$from[n], "phase, old=", phase[ifrom], ", new=", phase[ifrom]+atan2(S, C) / rpd, "\n")
                    phase[ifrom] <- phase[ifrom] + atan2(S, C) / rpd
                    ## End of Foreman 1978 inference calculation. Now we can define 'name' i.t.o. 'from'
                    iname <- which(tc$name == infer$name[n])[1]
                    oceDebug(1+debug, "Below is inference for ", infer$name[n], " (index=", iname, ")\n")
                    indices <- c(indices, iname)
                    name <- c(name, infer$name[n])
                    freq <- c(freq, tc$freq[iname])
                    amplitudeInferred <- infer$amp[n] * amplitude[ifrom]
                    phaseInferred <- phase[ifrom] - infer$phase[n]
                    oceDebug(1+debug, "  ", infer$name[n], "inferred amplitude=", amplitudeInferred, "\n")
                    oceDebug(1+debug, "  ", infer$name[n], "inferred phase=", phaseInferred, "\n")
                    amplitude <- c(amplitude, amplitudeInferred)
                    phase <- c(phase, phaseInferred)
                    p <- c(p, p[ifrom])
                    oceDebug(1+debug, "  create ", infer$name[n], " (index=", iname, ", ", tc$freq[iname], " cph) based on ", name[ifrom], " (index ", ifrom, ", ", freq[ifrom], " cph)\n", sep="")
                }
            } else {
                stop("Internal error (please report): cannot infer ", infer$name[n], " from ", infer$from[n], " because the latter was not computed")
            }
        }
        ## reorder by original position in tc
        o <- order(indices)
        indices <- indices[o]
        stopifnot(length(o)==length(name))
        stopifnot(length(o)==length(freq))
        stopifnot(length(o)==length(amplitude))
        stopifnot(length(o)==length(phase))
        stopifnot(length(o)==length(p))
        name <- name[o]
        freq <- freq[o]
        amplitude <- amplitude[o]
        phase <- phase[o]
        p <- p[o]
        rm(o)
        if (debug > 0) {
            cat("AFTER inference\n")
            print(data.frame(name=name, freq=round(freq,5), amplitude=round(amplitude,4)))
        }
    }
    phase <- phase %% 360
    res <- new('tidem')
    res@data <- list(model=model,
                      call=cl,
                      tRef=tRef,
                      const=indices,
                      name=name,
                      freq=freq,
                      amplitude=amplitude,
                      phase=phase,
                      p=p)
    res@metadata$rc <- rc
    res@metadata$version <- 2
    res@processingLog <- processingLogAppend(res@processingLog, paste(deparse(match.call()), sep="", collapse=""))
    oceDebug(debug, "} # tidem()\n", sep="", unindent=1)
    res
}


#' Predict a Tidal Signal
#'
#' This creates a time-series of predicted tides, based on a
#' tidal model object that was created by [as.tidem()] or [tidem()].
#'
#' @param object a [tidem-class] object.
#'
#' @param newdata vector of POSIXt times at which to make the
#' prediction.  For models created with [tidem()],
#' the `newdata` argument is optional, and if it is not provided, then
#' the predictions are at the observation times given to
#' [tidem()]. However, `newdata` is required  if [as.tidem()]
#' had been used to create `object`.
#'
#' @param \dots optional arguments passed on to children.
#'
#' @return A vector of predictions.
#'
#' @examples
#'
#'\dontrun{
#' library(oce)
#' # 1. tidal anomaly
#' data(sealevelTuktoyaktuk)
#' time <- sealevelTuktoyaktuk[["time"]]
#' elevation <- sealevelTuktoyaktuk[["elevation"]]
#' oce.plot.ts(time, elevation, type='l', ylab="Height [m]", ylim=c(-2, 6))
#' tide <- tidem(sealevelTuktoyaktuk)
#' lines(time, elevation - predict(tide), col="red")
#' abline(h=0, col="red")
#'
#' # 2. prediction at specified times
#' data(sealevel)
#' m <- tidem(sealevel)
#' ## Check fit over 2 days (interpolating to finer timescale)
#' look <- 1:48
#' time <- sealevel[["time"]]
#' elevation <- sealevel[["elevation"]]
#' oce.plot.ts(time[look], elevation[look])
#' # 360s = 10 minute timescale
#' t <- seq(from=time[1], to=time[max(look)], by=360)
#' lines(t, predict(m, newdata=t), col='red')
#' legend("topright", col=c("black","red"),
#' legend=c("data","model"),lwd=1)
#'}
#'
#' @author Dan Kelley
#'
#' @family things related to tides
predict.tidem <- function(object, newdata, ...)
{
    dots <- list(...)
    debug <- if ("debug" %in% names(dots)) dots$debug else 0
    oceDebug(debug, "predict.tidem() {\n", sep="", unindent=1)
    if (!missing(newdata) && !inherits(newdata, "POSIXt"))
        stop("newdata must be of class POSIXt")
    version <- object@metadata$version
    if (!is.null(version) && version == 3) {
        oceDebug(debug, "object@metadata$version is 3, so assuming the object was created by as.tidem()\n")
        if (missing(newdata))
            stop("must supply newdata because object was created with as.tidem()")
        hour2pi <- 2 * pi * (as.numeric(newdata) - as.numeric(object[["tRef"]])) / 3600
        oceDebug(debug, vectorShow(hour2pi))
        ## message("head(hour2pi): ", paste(head(hour2pi), collapse=" "))
        nc <- length(object@data$name)
        res <- rep(0, length(hour2pi))
        for (i in seq_len(nc)) {
            oceDebug(debug, "accounting for constituent[", i, "] = ", object@data$name[i], "\n", sep="")
            omega.t <- object@data$freq[i] * hour2pi
            a <- object@data$amplitude[i] * sin(2 * pi * object@data$phase[i] / 360)
            b <- object@data$amplitude[i] * cos(2 * pi * object@data$phase[i] / 360)
            res <- res + a*sin(omega.t) + b*cos(omega.t)
        }
    } else if (!is.null(version) && version == 2) {
        oceDebug(debug, "object@metadata$version is 2, so assuming the object was created by tidem()\n")
        if (!missing(newdata) && !is.null(newdata)) {
            oceDebug(debug, "newdata was provided\n")
            freq <- object@data$freq
            name <- object@data$name
            nc <- length(freq)
            tt <- as.numeric(as.POSIXct(newdata, tz="UTC"))
            nt <- length(tt)
            x <- array(dim=c(nt, 2 * nc))
            x[, 1] <- rep(1, nt)
            hour2pi <- 2 * pi * (as.numeric(tt) - as.numeric(object[["tRef"]])) / 3600
            for (i in 1:nc) {
                omega.t <- freq[i] * hour2pi
                x[, 2*i-1] <- cos(omega.t)
                x[, 2*i  ] <- sin(omega.t)
            }
            colnames(x) <- matrix(rbind(paste(name, "_C", sep=""), paste(name, "_S", sep="")), nrow=length(name), ncol=2)
            if ("Z0_S" %in% colnames(x)) {
                x <- x[, -which("Z0_S" == colnames(x))]
                oceDebug(debug, "model has Z0, so trimming the sin(freq*time) column\n")
            }
            res <- as.numeric(predict(object@data$model, newdata=list(x=x), ...))
        } else {
            oceDebug(debug, "newdata was not provided\n")
            res <- as.numeric(predict(object@data$model, ...))
        }
    } else {
        if (!("version" %in% names(object@metadata)))
            warning("prediction is being made based on an old object; it may be wrong\n")
        res <- as.numeric(predict(object@data$model, ...))
    }
    oceDebug(debug, "} # predict.tidem()\n", sep="", unindent=1)
    res
}



#' Get a Tidal Prediction from a WebTide Database
#'
#' Get a tidal prediction from a WebTide database. This only
#' works if the standalone WebTide application is installed,
#' and if it is installed in a standard location. The details
#' of installation are not within the oce purview.
#'
#' There are two methods of using this function.
#' *Case 1:* `action="map"`. In this case, if
#' `plot` is `FALSE`, a list is returned, containing
#' all the `node`s in the selected database, along with all
#' the `latitude`s and `longitude`s. This value is
#' also returned (silently) if `plot` is true, but in that case,
#' a plot is drawn to indicate the node locations. If `latitude` and
#' `longitude` are given, then the node nearest that spot is indicated on
#' the map; otherwise, if `node` is given, then the location of that
#' node is indicated. There is also a special case: if `node` is negative
#' and `interactive()` is `TRUE`,
#' then [locator()] is called, and the node nearest the spot
#' where the user clicks the mouse is indicated in the plot and in the
#' return value.
#'
#' *Case 2:* `action="predict"`. If `plot` is `FALSE`,
#' then a list is returned, indicating `time`, predicted
#' `elevation`, velocity components `u` and `v`,
#' `node` number, the name of the `basedir`, and
#' the `region`. If `plot` is `TRUE`, this list is returned
#' silently, and time-series plots are drawn for elevation, u, and v.
#'
#' Naturally, `webtide` will not work unless WebTide has been installed on
#' the computer.
#'
#' @param action An indication of the action, either `action="map"` to
#' draw a map or `action="predict"` to get a prediction; see
#' \sQuote{Details}.
#'
#' @param longitude,latitude optional location at which prediction is required (ignored if
#' `node` is given).
#'
#' @param node optional integer relating to a node in the database. If `node`
#' is given, then neither `latitude` nor `longitude` may be given.
#' If `node` is positive, then specifies indicates the node. If it is negative,
#' [locator()] is called so that the user can click (once) on the map, after
#' which the node is displayed on the map.
#'
#' @param time a vector of times (in the UTC timezone)
#' at which prediction is to be made.
#' If not supplied, this will be the week starting at the present time,
#' computed with [presentTime()], with a 15 minute increment.
#'
#' @param basedir directory containing the `WebTide` application.
#'
#' @param region database region, given as a directory name in the WebTide
#' directory.  For example, `h3o` is for Halifax Harbour, `nwatl` is
#' for the northwest Atlantic, and `sshelf` is for the Scotian Shelf and
#' Gulf of Maine.
#'
#' @param plot boolean indicating whether to plot.
#'
#' @param tformat optional argument passed to [oce.plot.ts()], for
#' plot types that call that function.  (See [strptime()] for the
#' format used.)
#'
#' @template debugTemplate
#'
#' @param \dots optional arguments passed to plotting functions. A common
#' example is to set `xlim` and `ylim`, to focus a map region.
#'
#' @return The value depends on `action`:
#'
#' * If `action="map"` the return value is a
#' list containing the index of the nearest node, along with the
#' `latitude` and `longitude` of that node.  If
#' `plot` is `FALSE`, this value is returned invisibly.
#'
#' * If `action="predict"`, the return value is a list containing a vector
#' of times (`time`), as well as vectors of the predicted `elevation`
#' in metres and the predicted horizontal components of velocity, `u` and
#' `v`, along with the `node` number, and the `basedir` and
#' `region` as supplied to this function. If `plot` is `FALSE`,
#' this value is returned invisibly.
#'
#' @source The WebTide software may be downloaded for free at the
#' Department of Fisheries and Oceans (Canada) website at
#' `http://www.bio.gc.ca/science/research-recherche/ocean/webtide/index-en.php`
#' (checked February 2016 and May 2017).
#'
#' @section Caution:
#' WebTide is not an open-source application, so the present function was
#' designed based on little more than guesses about the WebTide file structure.
#' Users should be on the lookout for odd results.
#'
#' @examples
#'\dontrun{
#' ## needs WebTide at the system level
#' library(oce)
#' ## 1. prediction at Halifax NS
#' longitude <- -63.57
#' latitude <- 44.65
#' prediction <- webtide("predict", longitude=longitude, latitude=latitude)
#' mtext(sprintf("prediction at %fN %fE", latitude, longitude), line=0.75, side=3)
#' ## 2. map
#' webtide(lon=-63.57,lat=44.65,xlim=c(-64,-63),ylim=c(43.0,46))
#'}
#'
#' @author Dan Kelley
#'
#' @family things related to tides
webtide <- function(action=c("map", "predict"),
                    longitude, latitude, node, time,
                    basedir=getOption("webtide"),
                    region="nwatl",
                    plot=TRUE, tformat, debug=getOption("oceDebug"), ...)
{
    debug <- max(0, min(floor(debug), 2))
    oceDebug(debug, "webtide(action=\"", action, "\", ...)\n",
             sep="", unindent=1, style="bold")
    rpd <- atan2(1, 1) / 45  # radians per degree
    action <- match.arg(action)
    nodeGiven <- !missing(node)
    longitudeGiven <- !missing(longitude)
    latitudeGiven <- !missing(latitude)
    path <- paste(basedir, "/data/", region, sep="")

    ## 2016-02-03: it seems that there are several possibilities for this filename.
    suffices <- c(".nod", "ll.nod", "_ll.nod")
    nodFiles <- paste(region, suffices, sep="")
    triangles <- NULL
    for (nodFile in nodFiles) {
        if (1 == length(list.files(path=path, pattern=nodFile))) {
            triangles <- read.table(paste(path, nodFile, sep="/"), col.names=c("triangle", "longitude", "latitude"))
            oceDebug(debug, "found webtide information in '", nodFile, "'\n", sep="")
            break
        } else {
            oceDebug(debug, "looked for webtide information in '", nodFile, "' but this file does not exist\n", sep="")
        }
    }
    if (is.null(triangles))
        stop("cannot find WebTide data file; rerun with debug=1 to see the searched list")
    if (action == "map") {
        if (plot) {
            asp <- 1 / cos(rpd*mean(range(triangles$latitude, na.rm=TRUE)))
            par(mfrow=c(1, 1), mar=c(3, 3, 2, 1), mgp=c(2, 0.7, 0))
            plot(triangles$longitude, triangles$latitude, pch=2, cex=1/4, lwd=1/8,
                 asp=asp, xlab="", ylab="", ...)
            ## Try for a coastline of well-suite resolution, if we have ocedata installed.
            usr <- par("usr")
            bestcoastline <- coastlineBest(lonRange=usr[1:2], latRange=usr[3:4], debug=debug-1)
            oceDebug(debug, "coastlineBest() suggests using", bestcoastline, "as the coastline\n")
            if (bestcoastline == "coastlineWorld") {
                data(list=bestcoastline, package="oce", envir=environment())
                coastlineWorld <- get("coastlineWorld")
            } else {
                if (requireNamespace("ocedata", quietly=TRUE)) {
                    data(list=bestcoastline, package="ocedata", envir=environment())
                    oceDebug(debug, "Using", bestcoastline, "from the ocedata package.\n")
                    coastlineWorld <- get(bestcoastline)
                } else {
                    data(list="coastlineWorld", package="oce", envir=environment())
                    oceDebug(debug, "The ocedata package is not available, so using", bestcoastline, "from oce\n")
                    coastlineWorld <- get("coastlineWorld")
                }
            }
            polygon(coastlineWorld[['longitude']], coastlineWorld[['latitude']], col="tan")
            ## use lon and lat, if node not given
            if (!nodeGiven && longitudeGiven && latitudeGiven) {
                closest <- which.min(geodDist(triangles$longitude, triangles$latitude, longitude, latitude))
                node <- triangles$triangle[closest]
            }
            if (nodeGiven && node < 0 && interactive()) {
                point <- locator(1)
                node <- which.min(geodDist(triangles$longitude, triangles$latitude, point$x, point$y))
            }
            if (missing(node)) {
                node <- triangles$number
                longitude <- triangles$longitude
                latitude <- triangles$latitude
            } else {
                if (is.finite(node)) {
                    node <- triangles$triangle[node]
                    longitude <- triangles$longitude[node]
                    latitude <- triangles$latitude[node]
                    points(longitude, latitude, pch=20, cex=2, col='blue')
                    legend("topleft", pch=20, pt.cex=2, cex=3/4, col='blue', bg='white',
                           legend=sprintf("node %.0f %.3fN %.3fE", node, latitude, longitude))
                }
            }
            oceDebug(debug, "} # webtide()\n", sep="", unindent=1, style="bold")
            return(invisible(list(node=node, latitude=latitude, longitude=longitude)))
        } else  {
            node <- triangles$triangle
            longitude <- triangles$longitude
            latitude <- triangles$latitude
            oceDebug(debug, "} # webtide()\n", sep="", unindent=1, style="bold")
            return(list(node=node, latitude=latitude, longitude=longitude))
        }
    } else if (action == "predict") {
        if (missing(time))
            time <- seq.POSIXt(from=presentTime(), by="15 min", length.out=7*4*24) # Q: what about timezone?
        if (missing(node)) {
            if (missing(longitude) || missing(latitude))
                stop("'longitude' and 'latitude' must be given unless 'node' is given")
            node <- which.min(geodDist(triangles$longitude, triangles$latitude, longitude, latitude))
        } else {
            latitude <- triangles$latitude[node]
            longitude <- triangles$longitude[node]
        }
        oceDebug(debug, latitude, "N ", longitude, "E -- use node ", node,
                 " at ", triangles$latitude[node], "N ", triangles$longitude[node], "E\n", sep="")
        constituentse <- dir(path=path, pattern="*.s2c")
        constituentsuv <- dir(path=path, pattern="*.v2c")
        nconstituents <- length(constituentse)
        period <- ampe <- phasee <- ampu <- phaseu <- ampv <- phasev <- vector("numeric", length(nconstituents))
        data("tidedata", package="oce", envir=environment())
        tidedata  <- get("tidedata")#,   pos=globalenv())
        for (i in 1:nconstituents) {
            twoLetter <- substr(constituentse[i], 1, 2)
            C <- which(twoLetter == tidedata$const$name)
            period[i] <- 1 / tidedata$const$freq[C]
            ## Elevation file contains one entry per node, starting with e.g.:
            ##tri
            ## period 23.934470 (hours) first harmonic
            ##260.000000 (days)
            ##1 0.191244 223.820954
            ##2 0.188446 223.141200
            coneFile <- paste(path, constituentse[i], sep="/")
            cone <- read.table(coneFile, skip=3)[node, ]
            ampe[i] <- cone[[2]]
            phasee[i] <- cone[[3]]
            conuvFile <- paste(path, constituentsuv[i], sep="/")
            conuv <- read.table(conuvFile, skip=3)[node, ]
            ampu[i] <- conuv[[2]]
            phaseu[i] <- conuv[[3]]
            ampv[i] <- conuv[[4]]
            phasev[i] <- conuv[[5]]
            oceDebug(debug, coneFile, sprintf("%s ", twoLetter),
                     sprintf("%4.2fh", period[i]),
                     sprintf(" (node %d) ", node),
                     sprintf("%4.4fm ", ampe[i]), sprintf("%3.3fdeg", phasee[i]), "\n", sep="")
        }
        elevation <- u <- v <- rep(0, length(time))
        ## NOTE: tref is the *central time* for tidem()
        tRef <- ISOdate(1899, 12, 31, 12, 0, 0, tz="UTC")
        h <- (as.numeric(time) - as.numeric(tRef)) / 3600
        tRef <- 3600 * round(mean(as.numeric(time)) / 3600)

        for (i in 1:nconstituents) {
            twoLetter <- substr(constituentse[i], 1, 2)
            C <- which(twoLetter == tidedata$const$name)
            vuf <- tidemVuf(tRef, j=C, latitude=latitude)
            phaseOffset <- (vuf$u + vuf$v) * 360
            ## NOTE: phase is *subtracted* here, but *added* in tidem()
            elevation <- elevation + ampe[i] * cos( (360 * h / period[i] - phasee[i] + phaseOffset) * rpd)
            ##> lines(time, elevation, col=i,lwd=3) ## Debug
            u <- u + ampu[i] * cos( (360 * h / period[i] - phaseu[i] + phaseOffset) * rpd)
            v <- v + ampv[i] * cos( (360 * h / period[i] - phasev[i] + phaseOffset) * rpd)
            oceDebug(debug, sprintf("%s ", twoLetter),
                     sprintf("%4.2fh ", period[i]),
                     sprintf("%4.4fm ", ampe[i]), sprintf("%3.3fdeg", phasee[i]), "\n", sep="")
        }
        if (plot) {
            par(mfrow=c(3, 1))
            oce.plot.ts(time, elevation, type='l', xlab="", ylab=resizableLabel("elevation"),
                        main=sprintf("node %.0f %.3fN %.3fE", node, latitude, longitude),
                        tformat=tformat)
            abline(h=0, lty='dotted', col='gray')
            oce.plot.ts(time, u, type='l', xlab="", ylab=resizableLabel("u"),
                        drawTimeRange=FALSE, tformat=tformat)
            abline(h=0, lty='dotted', col='gray')
            oce.plot.ts(time, v, type='l', xlab="", ylab=resizableLabel("v"),
                        drawTimeRange=FALSE, tformat=tformat)
            abline(h=0, lty='dotted', col='gray')
            oceDebug(debug, "} # webtide()\n", sep="", unindent=1, style="bold")
            return(invisible(list(time=time, elevation=elevation, u=u, v=v,
                                  node=node, basedir=basedir, region=region)))
        } else {
            oceDebug(debug, "} # webtide()\n", sep="", unindent=1, style="bold")
            return(list(time=time, elevation=elevation, u=u, v=v,
                        node=node, basedir=basedir, region=region))
        }
    }
}
