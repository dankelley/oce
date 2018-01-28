## vim:textwidth=128:expandtab:shiftwidth=4:softtabstop=4

##. TESTinfer1 <- !TRUE

#' @title Class to Store Tidal Models
#'
#' @description
#' Class to store tidal-constituent models.
#'
#' @author Dan Kelley
#' @family functions that plot \code{oce} data
#' @family things related to \code{tidem} data
setClass("tidem", contains="oce")

setMethod(f="initialize",
          signature="tidem",
          definition=function(.Object) {
              .Object@processingLog$time <- as.POSIXct(Sys.time())
              .Object@processingLog$value <- "create 'tidem' object"
              return(.Object)
          })


#' @title Tidal Constituent Information
#'
#' @description
#' The \code{tidedata} dataset contains Tide-constituent information that is
#' use by \code{\link{tidem}} to fit tidal models.  \code{tidedata} is a list
#' containing
#' \describe{
#' \item{\code{const}}{
#' a list containing vectors
#' \code{name} (a string with constituent name),
#' \code{freq} (the frequency, in cycles per hour),
#' \code{kmpr} (a string naming the comparison constituent, blank if there is none),
#' \code{ikmpr} (index of comparison constituent, or \code{0} if there is none),
#' \code{df} (frequency difference between constituent and its
#' comparison, used in the Rayleigh criterion),
#' \code{d1} through \code{d6} (the first through sixth Doodson numbers),
#' \code{semi},
#' \code{nsat} (number of satellite constituents),
#' \code{ishallow},
#' \code{nshallow},
#' \code{doodsonamp},
#' and
#' \code{doodsonspecies}.
#'}
#' \item{\code{sat}}{
#' a list containing vectors
#' \code{deldood},
#' \code{phcorr},
#' \code{amprat},
#' \code{ilatfac},
#' and
#' \code{iconst}.
#'}
#' \item{\code{shallow}}{
#' a list containing vectors
#' \code{iconst},
#' \code{coef},
#' and
#' \code{iname}.
#'}
#'}
#' Apart from the use of \code{d1} through \code{d6}, the naming and content
#' follows \code{T_TIDE} (see Pawlowicz et al. 2002), which in turn builds upon
#' the analysis of Foreman (1977).
#'
#' @name tidedata
#' @docType data
#' @author Dan Kelley
#' @references
#' Foreman, M. G. G., 1977.  Manual for tidal heights analysis and
#' prediction.  Pacific Marine Science Report 77-10, Institute of Ocean
#' Sciences, Patricia Bay, Sidney, BC, 58pp.
#'
#' Pawlowicz, Rich, Bob Beardsley, and Steve Lentz, 2002.  Classical tidal
#' harmonic analysis including error estimates in MATLAB using \code{T_TIDE}.
#' Computers and Geosciences, 28, 929-937.
#' @source The data come from the \code{tide3.dat} file of the \code{T_TIDE}
#' package (Pawlowicz et al., 2002), and derive from Appendices provided by
#' Foreman (1977).  The data are scanned using \file{tests/tide.R} in this
#' package, which also performs some tests using \code{T_TIDE} values as a
#' reference.
#' @family things related to \code{tidem} data
NULL


#' @title Summarize a Tidem Object
#'
#' @description
#' By default, all fitted constituents are plotted, but it is quite useful to
#' set e.g. p=0.05 To see just those constituents that are significant at the 5
#' percent level.
#' Note that the p values are estimated as the average of the p values for the
#' sine and cosine components at a given frequency.
#'
#' @param object an object of class \code{"tidem"}, usually, a result of a call
#' to \code{tidem}.
#' @param p optional value of the maximum p value for the display of an
#' individual coefficient.  If not given, all coefficients are shown.
#' @param constituent optional name of constituent on which to focus.
#' @param \dots further arguments passed to or from other methods.
#' @return \code{NULL}
#' @author Dan Kelley
#' @examples
#' \dontrun{
#' library(oce)
#' data(sealevel)
#' tide <- tidem(sealevel)
#' summary(tide)
#' }
#'
#' @family things related to \code{tidem} data
setMethod(f="summary",
          signature="tidem",
          definition=function(object, p, constituent, ...) {
              if (missing(p))
                  p <- 1
              ok <- object@data$p <= p
              haveP <- any(!is.na(object@data$p))
              if (missing(constituent)) {
                  fit <- data.frame(Const=object@data$const[ok],
                                    Name=object@data$name[ok],
                                    Freq=object@data$freq[ok],
                                    Amplitude=object@data$amplitude[ok],
                                    Phase=object@data$phase[ok],
                                    p=object@data$p[ok])
              } else {
                  i <- NULL
                  for (const in constituent)
                      i <- c(i, which(object@data$name==const))
                  i <- unique(i)
                  if (length(i) == 0)
                      stop("there is no such constituent '", constituent, "'")
                  fit <- data.frame(Const=object@data$const[i],
                                    Name=object@data$name[i],
                                    Freq=object@data$freq[i],
                                    Amplitude=object@data$amplitude[i],
                                    Phase=object@data$phase[i],
                                    p=object@data$p[i])
              }
              cat("tidem summary\n-------------\n")
              cat("\nCall:\n")
              cat(paste(deparse(object[["call"]]), sep="\n", collapse="\n"), "\n", sep="")
              cat("RMS misfit to data: ", sqrt(var(object[["model"]]$residuals)), '\n')
              cat("\nFitted model:\n")
              f <- fit[3:6]
              rownames(f) <- as.character(fit[, 2])
              digits <- 3
              if (haveP) {
                  printCoefmat(f, digits=digits,
                               signif.stars=getOption("show.signif.stars"),
                               signif.legend=TRUE,
                               P.values=TRUE, has.Pvalue=TRUE, ...)
              } else {
                  printCoefmat(f[, -4], digits=digits)
              }
              processingLogShow(object)
              invisible()
          })

#' @title Extract Something From a Tidem Object
#' @param x A tidem object, i.e. one inheriting from \code{\link{tidem-class}}.
#' @section Details of the specialized \code{tidem} method:
#'
#' A vector of the frequencies of fitted constituents is recovered
#' with e.g. \code{x[["frequency"]]}. Similarly, amplitude is
#' recovered with e.g. \code{x[["amplitude"]]} and phase with
#' e.g. \code{x[["phase"]]}. If any other string is specified, then
#' the underlying accessor \code{\link{[[,oce-method}}) is used.
#'
#' @template sub_subTemplate
#' @family things related to \code{tidem} data
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

#' @title Replace Parts of a Tidem Object
#' @param x An \code{tidem} object, i.e. inheriting from \code{\link{tidem-class}}
#' @template sub_subsetTemplate
#' @family things related to \code{tidem} data
setMethod(f="[[<-",
          signature(x="tidem", i="ANY", j="ANY"),
          definition=function(x, i, j, ..., value) {
              callNextMethod(x=x, i=i, j=j, ...=..., value=value) # [[<-
          })



#' @title Plot a Tidem Prediction
#'
#' @description
#' Plot a summary diagram for a tidal fit.
#'
#' @param x A \code{tidem} object, i.e. one inheriting from
#' \code{\link{tidem-class}}.
#' @param which integer flag indicating plot type, 1 for stair-case spectral, 2
#' for spike spectral.
#'
#' @param constituents a character vector of constituents that are
#' to be drawn and label. If \code{NULL}, then no constituents will be shown.
#' Consult the built-in dataset \code{\link{tidedata}} for the permissible
#' constituent names and their frequencies.
#'
#' @param sides an integer vector of length equal to that of \code{constituents},
#' designating the side on which the constituent labels are to be drawn. As in
#' all R graphics, the value \code{1} indicates the bottom of the plot, and
#' \code{3} indicates the top. If \code{sides=NULL}, the default, then all labels
#' are drawn at the top. Any value of \code{sides} that is not either 1 or 3
#' is converted to 3.
#'
#' @param col a character vector naming colours to be used for \code{constituents}.
#' Ignored if \code{sides=3}. Repeated to be of the same length as
#' \code{constituents}, otherwise.
#'
#' @param log if set to "\code{x}", the frequency axis will be logarithmic.
#' @param mgp 3-element numerical vector to use for \code{par(mgp)}, and also
#' for \code{par(mar)}, computed from this.  The default is tighter than the R
#' default, in order to use more space for the data and less for the axes.
#' @param mar value to be used with \code{\link{par}("mar")}.
#' @param \dots optional arguments passed to plotting functions.
#' @examples
#' \dontrun{
#' library(oce)
#' data(sealevel)
#' tide <- tidem(sealevel)
#' plot(tide)
#' }
#'
#' @section Historical note:
#' An argument named \code{labelIf} was removed in July 2016,
#' because it was discovered never to have worked as documented, and
#' because the more useful argument \code{constituents} had been added.
#'
#' @author Dan Kelley
#'
#' @family functions that plot \code{oce} data
#' @family things related to \code{tidem} data
#' @aliases plot.tidem
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


#' @title Nodal Modulation Calculations for Tidem
#'
#' @description
#' Do nodal modulation calculations for \code{\link{tidem}}. This function is based directly
#' on \code{t_vuf} in the \code{T_TIDE} Matlab package [1].
#'
#' @param t The time in \code{POSIXct} format.  (It is \strong{very} important to
#' use \code{tz="GMT"} in constructing \code{t}.)
#' @param j Indices of tidal constituents to use.
#' @param latitude Optional numerical value continaing the latitude in degrees North.
#' @return A \code{\link[base]{list}} containing
#' items named \code{v}, \code{u} and \code{f} (see the \code{T_TIDE}
#' documentation).
#' @author Dan Kelley translated this from \code{t_astron} from the \code{T_TIDE}
#' package.
#' @examples
#' tidemVuf(as.POSIXct("2008-01-22 18:50:24"), 43, 45.0)
#' @family things related to \code{tidem} data
#' @references
#' 1. Pawlowicz, Rich, Bob Beardsley, and Steve Lentz, 2002.  Classical tidal
#' harmonic analysis including error estimates in MATLAB using \code{T_TIDE}.
#' Computers and Geosciences, 28, 929-937.
tidemVuf <- function(t, j, latitude=NULL)
{
    debug <- 0
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
        oceDebug(debug, "f=", f, "\n") # FIXME
        oceDebug(debug, "u=", u, "\n") # FIXME

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
    }
    else {
        v <- v[j]
        u <- rep(0, length(j))
        f <- rep(1, length(j))
    }
    if (length(v) < length(u)) {
        warning("trimming u and f to get same length as v -- this is a bug")
        u <- u[1:length(v)]
        f <- f[1:length(v)]
    }
    list(v=v, u=u, f=f)
}


#' @title Astronomical Calculations for Tidem
#'
#' @description
#' Do some astronomical calculations for \code{\link{tidem}}.  This function is based directly
#' on \code{t_astron} in the \code{T_TIDE} Matlab package [1].
#'
#' @param t Either a time in \code{POSIXct} format (with \code{"UTC"} timezoen),
#' or an integer. In the second case, it is converted to a time with
#' \code{\link{numberAsPOSIXct}(t,tz="UTC")}.
#' If \code{t} (It is \strong{very} important to use \code{tz="GMT"} in constructing \code{t}.)
#' @return A \code{\link[base]{list}} containing items named
#' \code{astro} and \code{ader} (see \code{T_TIDE} documentation).
#' @author Dan Kelley translated this from \code{t_astron} in the \code{T_TIDE}
#' package.
#' @examples
#' tidemAstron(as.POSIXct("2008-01-22 18:50:24"))
#' @family things related to \code{tidem} data
#' @references
#' 1. Pawlowicz, Rich, Bob Beardsley, and Steve Lentz, 2002. Classical tidal
#' harmonic analysis including error estimates in MATLAB using \code{T_TIDE}.
#' Computers and Geosciences, 28, 929-937.
tidemAstron <- function(t)
{
    debug <- FALSE
    if (is.numeric(t))
        t <- numberAsPOSIXct(t, tz="UTC")
    d <- as.numeric(difftime(t, ISOdatetime(1899, 12, 31, 12, 0, 0, tz="UTC"), units="days"))
    D <- d / 10000
    a <- matrix(c(1, d, D^2, D^3), 4, 1)

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
    da <- matrix(c(0, 1, 2e-4*D, 3e-4*D^2), 4, 1)
    ader <- (scHcPcNpPp %*% da) / 360
    dtau <- 1 + ader[2, 1] - ader[1, 1]
    ader <- c(dtau, ader)
    list(astro=astro, ader=ader)
}


#' @title Fit a Tidem (Tidal Model) to a Timeseries
#'
#' @description
#' The fit is done in terms of sine and cosine components at the indicated
#' tidal frequencies, with the amplitude and phase being calculated from the
#' resultant coefficients on the sine and cosine terms.
#'
#' @details
#' The tidal constituents to be used in the analysis are specified as follows;
#' see \dQuote{Constituent Naming Convention}.
#'
#' \itemize{
#'
#' \item \strong{Case 1}. If \code{constituents} is not provided, then the constituent
#' list will be made up of the 69 constituents designated by Foreman as "standard".
#' These include astronomical frequencies and some shallow-water frequencies,
#' and are as follows: \code{c("Z0", "SA", "SSA", "MSM", "MM", "MSF", "MF",
#' "ALP1", "2Q1", "SIG1", "Q1", "RHO1", "O1", "TAU1", "BET1", "NO1", "CHI1",
#' "PI1", "P1", "S1", "K1", "PSI1", "PHI1", "THE1", "J1", "SO1", "OO1", "UPS1",
#' "OQ2", "EPS2", "2N2", "MU2", "N2", "NU2", "GAM2", "H1", "M2", "H2", "MKS2",
#' "LDA2", "L2", "T2", "S2", "R2", "K2", "MSN2", "ETA2", "MO3", "M3", "SO3",
#' "MK3", "SK3", "MN4", "M4", "SN4", "MS4", "MK4", "S4", "SK4", "2MK5", "2SK5",
#' "2MN6", "M6", "2MS6", "2MK6", "2SM6", "MSK6", "3MK7", "M8")}.
#'
#' \item \strong{Case 2}. If the first item in \code{constituents} is the string
#' \code{"standard"}, then a provisional list is set up as in Case 1, and then
#' the (optional) rest of the elements of \code{constituents} are examined, in
#' order.  Each of these constituents is based on the name of a tidal
#' constituent in the Foreman (1977) notation.  (To get the list, execute
#' \code{data(tidedata)} and then execute \code{cat(tideData$name)}.)  Each
#' named constituent is added to the existing list, if it is not already there.
#' But, if the constituent is preceeded by a minus sign, then it is removed
#' from the list (if it is already there).  Thus, for example,
#' \code{constituents=c("standard", "-M2", "ST32")} would remove the M2
#' constituent and add the ST32 constituent.
#'
#' \item \strong{Case 3}. If the first item is not \code{"standard"}, then the list of
#' constituents is processed as in Case 2, but without starting with the
#' standard list. As an example, \code{constituents=c("K1", "M2")} would fit
#' for just the K1 and M2 components. (It would be strange to use a minus sign
#' to remove items from the list, but the function allows that.)
#' }
#'
#' In each of the above cases, the list is reordered in frequency prior to the
#' analysis, so that the results of \code{\link{summary,tidem-method}} will be in a
#' familiar form.
#'
#' Once the constituent list is determined, \code{tidem} prunes the elements of
#' the list by using the Rayleigh criterion, according to which two
#' constituents of frequencies \eqn{f_1}{f1} and \eqn{f_2}{f2} cannot be
#' resolved unless the time series spans a time interval of at least
#' \eqn{rc/(f_1-f_2)}{rc/(f1-f2)}.
#'
#' Finally, \code{tidem} looks in the remaining constituent list to check
#' that the application of the Rayleigh criterion has not removed any of the
#' constituents specified directly in the \code{constituents} argument. If
#' any are found to have been removed, then they are added back. This last
#' step was added on 2017-12-27, to make \code{tidem} behave the same
#' way as the Foreman (1977) code [1], as illustrated in his
#' Appendices 7.2 and 7.3. (As an aside, his Appendix 7.3 has some errors,
#' e.g. the frequency for the 2SK5 constituent is listed there (p58) as 
#' 0.20844743, but it is listed as 0.2084474129 in his Appendix 7.1 (p41).
#' For this reason, the frequency comparison is relaxed to a \code{tol}
#' value of \code{1e-7} in a portion of the oce test suite
#' (see \code{tests/testthat/test_tidem.R} in the source).
#'
#' A specific example may be of help in understanding the removal of unresolvable
#' constitutents. For example, the \code{data(sealevel)} dataset is of length
#' 6718 hours, and this is too short to resolve the full list of constituents,
#' with the conventional (and, really, necessary) limit of \code{rc=1}.
#' From Table 1 of [1], this timeseries is too short to resolve the
#' \code{SA} constituent, so that \code{SA} will not be in the resultant.
#' Similarly, Table 2 of [1] dictates the removal of
#' \code{PI1}, \code{S1} and \code{PSI1} from the list. And, finally,
#' Table 3 of [1] dictates the removal of
#' \code{H1}, \code{H2}, \code{T2} and \code{R2}.  Also, since Table 3
#' of [1] indiates that \code{GAM2} gets subsumed into \code{H1},
#' and if \code{H1} is already being deleted in this test case, then
#' \code{GAM2} will also be deleted.
#'
#' A list of constituent names is created by the following:
#' \preformatted{
#' data(tidedata)
#' print(tidedata$const$name)
#' }
#'
#' \strong{The text should include discussion of the (not yet performed) nodal
#' correction treatment.}
#'
#' @param t Either a \code{sealevel} object (e.g. produced by
#' \code{\link{read.sealevel}} or \code{\link{as.sealevel}}) or a vector of
#' times. In the former case, time is part of the object, so \code{t} may not
#' be given here.  In the latter case, \code{tidem} needs a way to determine
#' time, so \code{t} must be given.
#' @param x an optional numerical vector holding something that varies with
#' time.  This is ignored if \code{t} is a \code{\link{sealevel-class}} object,
#' in which case it is inferred as \code{t[["elevation"]]}.
#' @param constituents an optional vector of strings that name
#' tidal constituents to which the fit is done (see \dQuote{Details}
#' and \dQuote{Constituent Naming Convention}.)
#'
#' @param infer a list of constituents to be inferred from
#' fitted constituents according to the method outlined
#' in Section 2.3.4 of Foreman (1977) [1].
#' If \code{infer} is \code{NULL}, the default, then
#' no such inferences are made. Otherwise, some constituents
#' are computed based on other constituents, instead of being
#' determined by regression at the proper frequency.
#' If provided, \code{infer} must be a list containing
#' four elements:
#' \code{name}, a vector of strings naming the constituents to be
#' inferred; \code{from}, a vector of strings naming the fitted
#' constituents used as the sources for those inferences (these
#' source constituents are added to the regression list, if they
#' are not already there);
#' \code{amp}, a numerical vector of factors to be applied to the
#' source amplitudes; and \code{phase}, a numerical vector of angles,
#' in degrees, to be subtracted from the source phases. For example,
#' Following Foreman (1997) [1], if any of the \code{name} items
#' have already been computed, then the suggested inference is ignored,
#' and the already-computed values are used.
#'\preformatted{
#' infer=list(name=c("P1","K2"),
#'            from=c("K1", "S2"),
#'            amp=c(0.33093, 0.27215),
#'            phase=c(-7.07, -22.4)
#'}
#' means that the amplitude of \code{P1} will be set as 0.33093 times the calculated amplitude
#' of \code{K1}, and that the \code{P1} phase will be set to the \code{K1} phase,
#' minus an offset of \code{-7.07} degrees.
#' (This example is used in the Foreman (1977) [1] discussion of a
#' Fortran analysis code and also in Pawlowicz et al. (2002) [4] discussion
#' of the T_TIDE Matlab code.
#' Rounded to the 0.1mm resolution of values reported in [1] and [2],
#' the \code{tidem} results have root-mean-square amplitude difference
#' to Foreman's Appendix 7.3 of 0.06mm; by comparision,
#' the results in Table 1 of Pawlowicz et al. (2002) agree with Foreman's
#' results to RMS difference 0.04mm.)
#'
#' @param latitude if provided, the latitude of the observations.  If not
#' provided, \code{tidem} will try to infer this from \code{sl}.
#' @param rc the value of the coefficient in the Rayleigh criterion.
#' @param regress function to be used for regression, by default
#' \code{\link{lm}}, but could be for example \code{rlm} from the
#' \code{MASS} package.
#' @template debugTemplate
#' @return An object of \code{\link{tidem-class}}, consisting of
#' \item{const}{constituent number, e.g. 1 for \code{Z0}, 1 for \code{SA},
#' etc.} \item{model}{the regression model} \item{name}{a vector of constituent
#' names, in non-subscript format, e.g. "\code{M2}".} \item{frequency}{a vector
#' of constituent frequencies, in inverse hours.} \item{amplitude}{a vector of
#' fitted constituent amplitudes, in metres.} \item{phase}{a vector of fitted
#' constituent phase.  NOTE: The definition of phase is likely to change as
#' this function evolves.  For now, it is phase with respect to the first data
#' sample.} \item{p}{a vector containing a sort of p value for each
#' constituent.  This is calculated as the average of the p values for the
#' sine() and cosine() portions used in fitting; whether it makes any sense is
#' an open question.}
#' @section Bugs:
#'
#' \enumerate{
#' \item This function is not fully developed yet, and both the
#' form of the call and the results of the calculation may change.
#'
#' \item Nodal correction is not done.
#'
#' \item The reported \code{p} value may make no sense at all, and it might be
#' removed in a future version of this function. Perhaps a significance level
#' should be presented, as in the software developed by both Foreman and
#' Pawlowicz.
#'
#' }
#'
#' @section Constituent Naming Convention:
#'
#' \code{tidem} uses constituent names that follow the convention
#' set by Foreman (1977) [1]. This convention is slightly different
#' from that used in the T-TIDE package of Pawlowicz et al.
#' (2002) [4], with Foreman's \code{UPS1} and \code{M8} becoming
#' \code{UPSI} and \code{MS} in T-TIDE. As a convenience,
#' \code{tidem} converts from these T-TIDE names to the
#' Foreman names, issuing warnings when doing so.
#'
#' @section Agreement with \code{T_TIDE} results:
#'
#' The \code{tidem} amplitude and phase results, obtained with
#'\preformatted{
#'tidem(sealevelTuktoyaktuk, constituents=c("standard", "M10"),
#'      infer=list(name=c("P1", "K2"),
#'                 from=c("K1", "S2"),
#'                 amp=c(0.33093, 0.27215),
#'                 phase=c(-7.07, -22.40))),
#'}
#' are identical the \code{T_TIDE} values listed in
#' Table 1 of Pawlowicz et al. (2002),
#' after rounding amplitude and phase to 4 and 2 digits past
#' the decimal place, to match the format of the table.
#'
#' @author Dan Kelley
#' @references
#' 1. Foreman, M. G. G., 1977.  Manual for tidal heights analysis and
#' prediction.  Pacific Marine Science Report 77-10, Institute of Ocean
#' Sciences, Patricia Bay, Sidney, BC, 58pp.
#'
#' 2. Foreman, M. G. G., Neufeld, E. T., 1991.  Harmonic tidal analyses of long
#' time series.  International Hydrographic Review, 68 (1), 95-108.
#'
#' 3. Leffler, K. E. and D. A. Jay, 2009.  Enhancing tidal harmonic analysis:
#' Robust (hybrid) solutions.  Continental Shelf Research, 29(1):78-88.
#'
#' 4. Pawlowicz, Rich, Bob Beardsley, and Steve Lentz, 2002.  Classical tidal
#' harmonic analysis including error estimates in MATLAB using \code{T_TIDE}.
#' Computers and Geosciences, 28, 929-937.
#'
#' @examples
#' library(oce)
#' # The demonstration time series from Foreman (1977),
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
#' @family things related to \code{tidem} data
tidem <- function(t, x, constituents, infer=NULL,
                  latitude=NULL, rc=1, regress=lm,
                  debug=getOption("oceDebug"))
{
    constituentNameFix <- function(names) # from T-TIDE to Foreman name
    {
        if ("MS" %in% names) {
            warning("constituent name switched from T-TIDE 'MS' to Foreman (1977) 'M8'")
            names[names == "MS"] <- "M8"
        }
        if ("-MS" %in% names) {
            warning("removed-constituent name switched from T-TIDE 'MS' to Foreman (1977) 'M8'")
            names[names == "-MS"] <- "-M8"
        }
        if ("UPS1" %in% names) {
            warning("constituent name switched from T-TIDE 'UPSI' to Foreman (1977) 'UPS1'")
            names[names == "UPS1"] <- "UPSI"
        }
        if ("-UPS1" %in% names) {
            warning("removed-constituent name switched from T-TIDE 'UPSI' to Foreman (1977) 'UPS1'")
            names[names == "-UPS1"] <- "-UPSI"
        }
        names
    }
    oceDebug(debug, "tidem(t, x, constituents",
             ", latitude=", if (is.null(latitude)) "NULL" else latitude,
             ", rc=", rc,
             ", debug=", debug, ") {\n", sep="", unindent=1)
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
    ## constitutent.
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
        infer$name <- constituentNameFix(infer$name)
        if (!is.character(infer$from))
            stop("infer$from must be a vector of character strings")
        infer$from <- constituentNameFix(infer$from)
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
        name <- tc$name[standard][-1]
        ##> message("head(name)=", paste(head(name), collapse=" "), " AFTER")
        freq <- tc$freq[standard][-1]
        kmpr <- tc$kmpr[standard][-1]
        indices <- seq(1:ntc)[standard] # NB. Z0 need not be dropped; we work with indices later anyway
        oceDebug(debug, "starting with default constituents: ", paste(name, collapse=" "), "\n")
    } else {
        ## Build up 'name'; later, infer 'indices' and thereby 'freq' and 'kmpr'.
        name <- NULL
        nconst <- length(constituents)
        for (i in 1:nconst) {
            ## if (debug > 0) cat("[", constituents[i], "]\n", sep="")
            if (constituents[i] == "standard") {
                ## must be first!
                if (i != 1)
                    stop("\"standard\" must occur first in constituents list")
                name <- tc$name[standard][-1]
            } else {
                constituents <- constituentNameFix(constituents)
                if (substr(constituents[i], 1, 1) == "-") {
                    ## Case 1: removal. Require a valid name, and warn if not in the list already.
                    nameRemove <- substr(constituents[i], 2, nchar(constituents[i]))
                    if (1 != sum(tc$name == nameRemove))
                        stop("'", nameRemove, "' is not a known tidal constituent; try one of: ",
                             paste(tc$name, collapse=" "))
                    remove <- which(name == nameRemove)
                    if (0 == length(remove))
                        warning(nameRemove, "is not in the list of constituents currently under study")
                    else
                        name <- name[-remove]
                } else {
                    ## Case 2: addition. Require a valid name, and ignore repeat requests.
                    add <- which(tc$name == constituents[i])
                    if (1 != length(add))
                        stop("'", constituents[i], "' is not a known tidal constituent; try one of: ",
                                paste(tc$name, collapse=" "), "\n")
                    if (!(constituents[i] %in% name)) {
                        name <- c(name, tc$name[add])
                        addedConstituents <- c(addedConstituents, add)
                    }
                }
                oceDebug(debug, "using names= ", paste(name, collapse=" "), "\n")
            }
        }
    }
    ## We let users add "Z0" as a constituent, but we remove it now since the
    ## regression will have an intercept and that becomes Z0.
    if ("Z0" %in% name)
        name <- name[name != "Z0"]
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
    if (debug > 2) {
        cat("next is at line 875 (initial setup, before 'infer' handled)\n")
        print(data.frame(name=name, indices=indices, freq=freq, kmpr=kmpr))
    }

    nc <- length(name)

    ## Check Rayleigh criterion
    interval <- (as.numeric(tail(sl@data$time, 1)) - as.numeric(sl@data$time[1])) / 3600
    dropTerm <- NULL
    for (i in 1:nc) {
        cc <- which(tc$name == kmpr[i])
        if (length(cc)) {
            cannotFit <- (interval * abs(freq[i]-tc$freq[cc])) < rc
            ##cat("compare name=", name[i], "with", kmpr[i],":", cannotFit,"\n")
            if (cannotFit) {
                dropTerm <- c(dropTerm, i)
            }
        }
    }
    if (length(dropTerm) > 0) {
        cat("Note: the tidal record is too short to fit for constituents: ", paste(name[dropTerm], collapse=" "), "\n")
        indices <- indices[-dropTerm]
        name <- name[-dropTerm]
        freq <- freq[-dropTerm]
        kmpr <- kmpr[-dropTerm]
    }
    ## Ensure that any added constitutents are in the list, i.e. prevent
    ## the Rayleigh criterion from trimming them. (Before work on
    ## issue 1350, they would simply be dropped if they failed the Rayleigh
    ## criterion. Although that was a sensible choice, it was decided
    ## on 2017-12-27, whilst working on issue 1350, to make tidem() do the
    ## the same thing as the Foreman 1977 code as exemplified in his appendices
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
    ##. if (TESTinfer1) {
    ##.     ## Ensure that we fit for any infer$name constituents, *regardless* of whether
    ##.     ## those consitituents are permitted by the Rayleigh criterion.
    ##.     if (!is.null(infer)) {
    ##.         for (n in c(infer$name)) {
    ##.             if (!(n %in% name)) {
    ##.                 a <- which(tc$name == n)
    ##.                 indices <- c(indices, a)
    ##.                 name <- c(name, tc$name[a])
    ##.                 freq <- c(freq, tc$freq[a])
    ##.                 kmpr <- c(kmpr, tc$kmpr[a])
    ##.                 message("fitting for infer$name=", n, ", even though the Rayleigh Criterion would exclude it")
    ##.             }
    ##.         }
    ##.     }
    ##. }

    ## sort constituents by index
    oindices <- order(indices)
    indices <- indices[oindices]
    name <- name[oindices]
    freq <- freq[oindices]
    kmpr <- kmpr[oindices]
    rm(oindices) # clean up namespace

    nc <- length(name)
    elevation <- sl[["elevation"]]
    time <- sl[["time"]]
    nt <- length(elevation)
    x <- array(dim=c(nt, 2 * nc))
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
        oceDebug(debug, "setting coefficients for ", name[i], " (", freq[i], " cph)", "\n", sep="")
        ft <- freq[i] * hour2pi
        x[, 2*i-1] <- sin(ft)
        x[, 2*i  ] <- cos(ft)
    }
    name2 <- matrix(rbind(paste(name, "_S", sep=""), paste(name, "_C", sep="")), nrow=length(name), ncol=2)
    dim(name2) <- c(2 * length(name), 1)
    colnames(x) <- name2
    #model <- lm(elevation ~ x, na.action=na.exclude)
    oceDebug(debug, "about to do regression\n")
    model <- regress(elevation ~ x, na.action=na.exclude)
    if (debug > 0) {
        oceDebug(debug, "regression worked OK; the results are as follows:\n")
        print(summary(model))
    }
    coef  <- model$coefficients
    p.all <- if (4 == dim(summary(model)$coefficients)[2])
        summary(model)$coefficients[, 4]
    else
        rep(NA, length=1+nc)
    amplitude <- phase <- p <- vector("numeric", length=1+nc)
    ## FIXME: should do offset/trend removal explicitly
    amplitude[1] <- coef[1]
    phase[1] <- 0
    p[1] <- p.all[1]
    for (i in seq.int(2, nc+1)) {
        is <- 2 * (i - 1)
        ic <- 2 * (i - 1) + 1
        s <- coef[is]                   # coefficient on sin(t)
        c <- coef[ic]                   # coefficient on cos(t)
        if (debug > 0)
            cat(name[i-1], "gives s=", s, "and c=", c, "\n")
        amplitude[i] <- sqrt(s^2 + c^2)
        ## Calculate phase from the coefficients on sin() and cos().  Generally,
        ##    cos(t - phase) == cos(phase)*cos(t) + sin(phase)*sin(t)
        ## By the definition of the regression model, we have
        ##    cos(t - phase) == c * cos(t) + s * sin(t)
        ## and thus phase is defined by
        ##    tan(phase) == s/c
        phase[i] <- atan2(s, c)
        ## Adjust amplitude phase, as in ~/src/foreman/tide12_r2.f:405
        j <- which(tidedata$const$name==name[i-1])
        vuf <- tidemVuf(tRef, j=j, latitude=latitude)
        amplitude[i] <- amplitude[i] / vuf$f
        p[i] <- 0.5 * (p.all[is] + p.all[ic])
        if (debug > 0)
            cat(name[i-1], "F=", vuf$f, "angle adj=", (vuf$u+vuf$v)*360, "; amp=", amplitude[i], " phase=", phase[i], "\n")
    }
    phase <- phase * 180 / pi
    phase <- ifelse(phase < -360, 720 + phase, phase)
    phase <- ifelse(phase < 0, 360 + phase, phase)

    ## FIXME: if 'inference calculation' is to be done, it should match
    ##     ~/src/t_tide_v1.3beta/t_tide.m:468
    ##     ~/src/foreman/tide12_r2.f:422

    ## The regression gives us an intercept, which we call Z0
    indices <- c(1, indices) # the index for Z0 is 1
    name <- c("Z0", name)
    freq <- c(0, freq)

    if (debug > 0) {
        message("BEFORE inference:")
        print(data.frame(name=name, freq=round(freq,6), amplitude=round(amplitude,4)))
    }

    ## Do Greenwich phase corerrection, if `infer` is TRUE
    C <- unlist(lapply(name, function(n) which(n == tidedata$const$name)))
    vuf <- tidemVuf(tRef, j=C, latitude=latitude)
    phase <- phase + (vuf$v+vuf$u)*360
    phase <- ifelse(phase < 0, phase+360, phase)
    phase <- ifelse(phase > 360, phase-360, phase)

    ## Handle (optional) inferred constituents. We know that
    ## this list is well-formed because of extensive tests near
    ## the start of this function.
    if (!is.null(infer)) {
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
                    message("name already in list")
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
                    ## is done based on formulae in Foreman (1977) sec 2.3.4. It looks
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
                    ##TTIDE ## Relates loosely to Foreman (1977 sec2.3.4 p28) "S"
                    ##TTIDE pcfac <- infer$amp[n] * vufName$f / vufFrom$f * cos(pearg)
                    ##TTIDE ## t_tide.m:478
                    ##TTIDE ## pcorr=1+pcfac.*scarg;
                    ##TTIDE pcorr <- 1 + pcfac * scarg
                    ##TTIDE ##message("  pearg=", pearg, ", pcfac=", pcfac)
                    ##TTIDE ##message("  pcorr=", pcorr, " (should divide infer amp by this)")
                    ##TTIDE ##message("  new amp for (", infer$from[n], ") might be=", pcorr*amplitude[ifrom])
                    ##
                    ## Foreman (1977) [1] sec 2.3.4.
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
                    ## End of Foreman 1977 inference calculation. Now we can define 'name' i.t.o. 'from'
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
        if (debug > 0) {
            cat("AFTER inference:\n")
            print(data.frame(name=name, freq=round(freq,6), amplitude=round(amplitude,4)))
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
            cat("AFTER reordering\n")
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
    res@metadata$version <- "2"
    res@processingLog <- processingLogAppend(res@processingLog, paste(deparse(match.call()), sep="", collapse=""))
    res
}


#' @title Predict a Time Series from a Tidem Tidal Model
#'
#' @description
#' Predict a time series from a tidal model.
#' This is a wrapper around the predict method for \code{object$model}.
#'
#' @param object A \code{tidem} object, i.e. one inheriting from
#' \code{\link{tidem-class}}.
#' @param newdata optional vector of POSIXt times at which to make the
#' prediction.  If not present, \code{predict.tidem} uses the times that were
#' provided in the original call to \code{\link{tidem}}.
#' @param \dots optional arguments passed on to children.
#' @return A vector of predictions.
#' @examples
#'
#' \dontrun{
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
#' lines(t, predict(m,newdata=t), col='red')
#' legend("topright", col=c("black","red"),
#' legend=c("data","model"),lwd=1)
#' }
#'
#' @author Dan Kelley
#' @family things related to \code{tidem} data
predict.tidem <- function(object, newdata, ...)
{
    if (!missing(newdata) && !is.null(newdata)) {
        ##newdata.class <- class(newdata)
        if (inherits(newdata, "POSIXt")) {
            freq <- object@data$freq[-1]     # drop first (intercept)
            name <- object@data$name[-1]     # drop "z0" (intercept)
            nc <- length(freq)
            tt <- as.numeric(as.POSIXct(newdata, tz="UTC"))
            nt <- length(tt)
            x <- array(dim=c(nt, 2 * nc))
            x[, 1] <- rep(1, nt)
            hour2pi <- 2 * pi * (as.numeric(tt) - as.numeric(object[["tRef"]])) / 3600
            for (i in 1:nc) {
                omega.t <- freq[i] * hour2pi
                x[, 2*i-1] <- sin(omega.t)
                x[, 2*i  ] <- cos(omega.t)
            }
            name2 <- matrix(rbind(paste(name, "_S", sep=""), paste(name, "_C", sep="")), nrow=length(name), ncol=2)
            dim(name2) <- c(2 * length(name), 1)
            colnames(x) <- name2
            res <- predict(object@data$model, newdata=list(x=x), ...)
        } else {
            stop("newdata must be of class POSIXt")
        }
    } else {
        if (!("version" %in% names(object@metadata)))
            warning("prediction is being made based on an old object; it may be wrong\n")
        res <- predict(object@data$model, ...)
    }
    as.numeric(res)
}



#' @title Get a Tidal Prediction from a WebTide Database
#'
#' @description
#' Get a tidal prediction from a WebTide database. There are two distinct cases.
#'
#' \emph{Case 1:} \code{action="map"}. In this case, if
#' \code{plot} is \code{FALSE}, a list is returned, containing
#' all the \code{node}s in the selected database, along with all
#' the \code{latitude}s and \code{longitude}s. This value is
#' also returned (silently) if \code{plot} is true, but in that case,
#' a plot is drawn to indicate the node locations. If \code{latitude} and
#' \code{longitude} are given, then the node nearest that spot is indicated on
#' the map; otherwise, if \code{node} is given, then the location of that
#' node is indicated. There is also a special case: if \code{node} is negative
#' and \code{interactive()} is \code{TRUE},
#' then \code{\link{locator}} is called, and the node nearest the spot
#' where the user clicks the mouse is indicated in the plot and in the
#' return value.
#'
#' \code{Case 2:} \code{action="predict"}. If \code{plot} is \code{FALSE},
#' then a list is returned, indicating \code{time}, predicted
#' \code{elevation}, velocity components \code{u} and \code{v},
#' \code{node} number, the name of the \code{basedir}, and
#' the \code{region}. If \code{plot} is \code{TRUE}, this list is returned
#' silently, and time-series plots are drawn for elevation, u, and v.
#'
#' Naturally, \code{webtide} will not work unless WebTide has been installed on
#' the computer.
#'
#' @param action An indication of the action, either \code{action="map"} to
#' draw a map or \code{action="predict"} to get a prediction; see
#' \sQuote{Details}.
#' @param longitude,latitude optional location at which prediction is required (ignored if
#' \code{node} is given).
#' @param node optional integer relating to a node in the database. If \code{node}
#' is given, then neither \code{latitude} nor \code{longitude} may be given.
#' If \code{node} is positive, then specifies indicates the node. If it is negative,
#' \code{\link{locator}} is called so that the user can click (once) on the map, after
#' which the node is displayed on the map.
#' @param time a vector of times at which prediction is to be made.
#' If not supplied, this will be the week starting at the present time,
#' incrementing by 15 minutes.
#' @param basedir directory containing the \code{WebTide} application.
#' @param region database region, given as a directory name in the WebTide
#' directory.  For example, \code{h3o} is for Halifax Harbour, \code{nwatl} is
#' for the northwest Atlantic, and \code{sshelf} is for the Scotian Shelf and
#' Gulf of Maine.
#' @param plot boolean indicating whether to plot.
#' @param tformat optional argument passed to \code{\link{oce.plot.ts}}, for
#' plot types that call that function.  (See \code{\link{strptime}} for the
#' format used.)
#' @template debugTemplate
#' @param \dots optional arguments passed to plotting functions. A common
#' example is to set \code{xlim} and \code{ylim}, to focus a map region.
#' @return If \code{action="map"} the return value is a
#' list containing the index of the nearest node, along with the
#' \code{latitude} and \code{longitude} of that node.  If
#' \code{plot} is \code{FALSE}, this value is returned invisibly.
#'
#' If \code{action="predict"}, the return value is a list containing a vector
#' of times (\code{time}), as well as vectors of the predicted \code{elevation}
#' in metres and the predicted horizontal components of velocity, \code{u} and
#' \code{v}, along with the \code{node} number, and the \code{basedir} and
#' \code{region} as supplied to this function.
#' If \code{plot} is \code{FALSE}, this value is returned invisibly.
#'
#' @source The WebTide software may be downloaded for free at the
#' Department of Fisheries and Oceans (Canada) website at
#' \code{http://www.bio.gc.ca/science/research-recherche/ocean/webtide/index-en.php}
#' (checked February 2016 and May 2017).
#'
#' @return a list containing \code{node}, \code{longitude} and \code{latitude}. If
#' no node was provided to \code{webtide} then this list will cover the whole
#' set of nodes in the WebTide model; otherwise, it will be just the node that
#' was requested.
#'
#' @section Caution:
#' WebTide is not an open-source application, so the present function was
#' designed based on little more than guesses about the WebTide file structure.
#' Users should be on the lookout for odd results.
#' @examples
#' \dontrun{
#' ## needs WebTide at the system level
#' library(oce)
#' ## 1. prediction at Halifax NS
#' longitude <- -63.57
#' latitude <- 44.65
#' prediction <- webtide("predict", longitude=longitude, latitude=latitude)
#' mtext(sprintf("prediction at %fN %fE", latitude, longitude), line=0.75, side=3)
#' ## 2. map
#' webtide(lon=-63.57,lat=44.65,xlim=c(-64,-63),ylim=c(43.0,46))
#' }
#' @author Dan Kelley
webtide <- function(action=c("map", "predict"),
                    longitude, latitude, node, time,
                    basedir=getOption("webtide"),
                    region="nwatl",
                    plot=TRUE, tformat, debug=getOption("oceDebug"), ...)
{
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
            ##usr <- par('usr')
            ##best <- coastlineBest(lonRange=usr[1:2], latRange=usr[3:4])
            data("coastlineWorld", package="oce", envir=environment())
            coastlineWorld <- get("coastlineWorld")
            ##data(best, envir=environment(), debug=debug-1)
            ##coastline <- get(best)
            lines(coastlineWorld[['longitude']], coastlineWorld[['latitude']])
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
            return(invisible(list(node=node, latitude=latitude, longitude=longitude)))
        } else  {
            node <- triangles$triangle
            longitude <- triangles$longitude
            latitude <- triangles$latitude
            return(list(node=node, latitude=latitude, longitude=longitude))
        }
    } else if (action == "predict") {
        if (missing(time))
            time <- seq.POSIXt(from=Sys.time(), by="15 min", length.out=7*4*24)
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
            return(invisible(list(time=time, elevation=elevation, u=u, v=v,
                                  node=node, basedir=basedir, region=region)))
        } else {
            return(list(time=time, elevation=elevation, u=u, v=v,
                        node=node, basedir=basedir, region=region))
        }
    }
}
