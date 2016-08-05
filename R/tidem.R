## vim:textwidth=128:expandtab:shiftwidth=4:softtabstop=4

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
#' containing \describe{ \item{const}{a list containing vectors \describe{
#' \item{list("name")}{a string with constituent name} \item{list("freq")}{the
#' frequency, in cycles per hour} \item{list("kmpr")}{a string naming the
#' comparison constituent, blank if there is none} \item{list("ikmpr")}{index
#' of comparison constituent, or \code{0} if there is none}
#' \item{list("df")}{frequency difference betwee constituent and its
#' comparison, used in the Rayleigh criterion} \item{list("d1")}{first Doodson
#' number} \item{list("d2")}{second Doodson number} \item{list("d3")}{third
#' Doodson number} \item{list("d4")}{fourth Doodson number}
#' \item{list("d5")}{fifth Doodson number} \item{list("d6")}{sixth Doodson
#' number} \item{list("semi")}{(fill in some info later)}
#' \item{list("nsat")}{number of satellite constituents}
#' \item{list("ishallow")}{(fill in some info later)}
#' \item{list("nshallow")}{(fill in some info later)}
#' \item{list("doodsonamp")}{(fill in some info later)}
#' \item{list("doodsonspecies")}{(fill in some info later)} } }
#' \item{list("sat")}{a list containing vectors \describe{
#' \item{list("deldood")}{(fill in some info later)}
#' \item{list("phcorr")}{(fill in some info later)} \item{list("amprat")}{(fill
#' in some info later)} \item{list("ilatfac")}{(fill in some info later)}
#' \item{list("iconst")}{(fill in some info later)} } }
#' \item{list("shallow")}{a list containing vectors \describe{
#' \item{list("iconst")}{(fill in some info later)} \item{list("coef")}{(fill
#' in some info later)} \item{list("iname")}{(fill in some info later)} } } }
#' Apart from the use of \code{d1} through \code{d6}, the naming and content
#' follows \code{T_TIDE}.  All of this is based on Foreman (1977), to which the
#' reader is referred for details.
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
              n <- length(object[["p"]])
              ok <- if (!missing(p)) object@data$p <= p else seq(1, n)
              haveP <- any(!is.na(object@data$p))
              if (missing(constituent)) {
                  fit <- data.frame(Const=object@data$const[ok],
                                    Name=object@data$name[ok],
                                    Freq=object@data$freq[ok],
                                    Amplitude=object@data$amplitude[ok],
                                    Phase=object@data$phase[ok],
                                    p=object@data$p[ok])
              } else {
                  i <- which(object@data$name==constituent)
                  if (length(i) == 0)
                      stop("there is no such constituent '", constituent, "'")
                  fit <- data.frame(Const=object@data$const[i],
                                    Name=object@data$name[i],
                                    Freq=object@data$freq[i],
                                    Amplitude=object@data$amplitude[i],
                                    Phase=object@data$phase[i],
                                    p=p)
              }
              cat("tidem summary\n-------------\n")
              cat("\nCall:\n")
              cat(paste(deparse(object[["call"]]), sep="\n", collapse="\n"), "\n", sep="")
              cat("RMS misfit to data: ", sqrt(var(object[["model"]]$residuals)), '\n')
              cat("\nFitted model:\n")
              f <- fit[3:6]
              rownames(f) <- as.character(fit[,2])
              digits <- 3
              if (haveP) {
                  printCoefmat(f, digits=digits,
                               signif.stars=getOption("show.signif.stars"),
                               signif.legend=TRUE,
                               P.values=TRUE, has.Pvalue=TRUE, ...)
              } else {
                  printCoefmat(f[,-4], digits=digits)
              }
              processingLogShow(object)
              invisible(NULL)
          })

#' @title Extract Something From a Tidem Object
#' @param x A tidem object, i.e. one inheriting from \code{\link{tidem-class}}.
#' @template sub_subTemplate
#' @family things related to \code{tidem} data
setMethod(f="[[",
          signature(x="tidem", i="ANY", j="ANY"),
          definition=function(x, i, j, ...) {
              callNextMethod()
          })

#' @title Replace Parts of a Tidem Object
#' @param x An \code{tidem} object, i.e. inheriting from \code{\link{tidem-class}}
#' @template sub_subsetTemplate
#' @family things related to \code{tidem} data
setMethod(f="[[<-",
          signature(x="tidem", i="ANY", j="ANY"),
          definition=function(x, i, j, value) {
              callNextMethod(x=x, i=i, j=j, value=value)
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
setMethod(f="plot",
          signature=signature("tidem"),
          definition=function(x,
                              which=1,
                              constituents=c("SA", "O1", "K1", "M2", "S2", "M4"),
                              sides=NULL,
                              col="blue",
                              log="",
                              mgp=getOption("oceMgp"),
                              mar=c(mgp[1]+1,mgp[1]+1,mgp[2]+0.25,mgp[2]+1),
                              ...)
          {
              data("tidedata", package="oce", envir=environment())
              tidedata <- get("tidedata")#, pos=globalenv())
              drawConstituent<-function(name="M2",side=3,col="blue",adj=NULL)
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
              "doodson[1,]=",doodson[1,],"\n",
              "doodson[2,]=",doodson[2,],"\n",
              "doodson[3,]=",doodson[3,],"\n")
    v <- doodson %*% a$astro + tidedata$const$semi
    oceDebug(debug, "tidedata$const$semi[",j,"]=",tidedata$const$semi[j],"\n")
    v <- v - trunc(v)
    oceDebug(debug, "v[1:3]=",v[1:3],"\n")
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

        oceDebug(debug, "uu[1:3]=",uu[1:3], "\n")

        nsat <- length(tidedata$sat$iconst)
        ##nfreq <- length(tidedata$const$numsat)
        ## loop, rather than make a big matrix
        oceDebug(debug,
                  "tidedata$sat$iconst=", tidedata$sat$iconst, "\n",
                  "length(sat$iconst)=", length(tidedata$sat$iconst),"\n")
        fsum.vec <- vector("numeric", nsat)
        u.vec <- vector("numeric", nsat)
        for (isat in 1:nsat) {
            oceDebug(debug, "isat=",isat,"\n")
            use <- tidedata$sat$iconst == isat
            fsum.vec[isat] <- 1 + sum(rr[use] * exp(1i * 2 * pi * uu[use]))
            u.vec[isat] <- Arg(fsum.vec[isat]) / 2 / pi
            if (isat==8 && debug > 0) {
                cat("TEST at isat=8:\n")
                cat("fsum.vec[",isat,"]=",fsum.vec[isat]," (EXPECT  1.18531604917590 - 0.08028013402313i)\n")
                cat("u.vec[   ",isat,"]=",u.vec[isat],"       (EXPECT -0.01076294959868)\n")
            }
        }
        oceDebug(debug,
                  "uvec[",j,"]=", u.vec[j], "\n",
                  "fsum.vec[",j,"]=", fsum.vec[j],"\n")
        f <- abs(fsum.vec)
        u <- Arg(fsum.vec)/2/pi
        oceDebug(debug, "f=",f,"\n") # FIXME
        oceDebug(debug, "u=",u,"\n") # FIXME

        for (k in which(!is.na(tidedata$const$ishallow))) {
            ik <- tidedata$const$ishallow[k] + 0:(tidedata$const$nshallow[k] - 1)
            f[k] <- prod(f[tidedata$shallow$iname[ik]]^abs(tidedata$shallow$coef[ik]))
            u[k] <- sum(u[tidedata$shallow$iname[ik]]*tidedata$shallow$coef[ik])
            v[k] <- sum(v[tidedata$shallow$iname[ik]]*tidedata$shallow$coef[ik])
            if (debug>0 && k < 28) cat("k=",k,"f[k]=",f[k]," u[k]=",u[k],"v[k]=",v[k],"\n")
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
#' @param t The time in \code{POSIXct} format.  (It is \strong{very} important to
#' use \code{tz="GMT"} in constructing \code{t}.)
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
                                        # Code mimics t_astron in t_tide
    debug <- FALSE
    d <- as.numeric(difftime(t, ISOdatetime(1899,12,31,12,0,0,tz="UTC"), units="days"))
    D <- d / 10000
    a <- matrix(c(1, d, D^2, D^3), 4, 1)

    oceDebug(debug, "d=",formatC(d,digits=10),"D=",D,"a=", a, "\n")

    sc.hc.pc.np.pp <-
        matrix(c(270.434164, 13.1763965268,-0.0000850, 0.000000039,
                 279.696678,  0.9856473354, 0.00002267,0.000000000,
                 334.329556,  0.1114040803,-0.0007739,-0.00000026 ,
                 -259.183275, 0.0529539222,-0.0001557,-0.000000050,
                 281.220844,  0.0000470684, 0.0000339, 0.000000070),
               nrow=5, ncol=4, byrow=TRUE)
    astro <- ((sc.hc.pc.np.pp %*% a) / 360) %% 1

    oceDebug(debug, "astro=",astro,"\n")

    rem <- as.numeric(difftime(t, trunc.POSIXt(t,units="days"), tz="UTC", units="days"))

    oceDebug(debug, "rem2=",rem,"\n")

    tau <- rem + astro[2,1] - astro[1,1]
    astro <- c(tau, astro)
    da <- matrix(c(0, 1, 2e-4*D, 3e-4*D^2), 4, 1)
    ader <- (sc.hc.pc.np.pp %*% da) / 360
    dtau <- 1 + ader[2,1] - ader[1,1]
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
#' The tidal constituents to be used in the analysis are specified as follows.
#'
#' \enumerate{
#'
#' \item Case 1. If \code{constituents} is not provided, then the constituent
#' list will be made up of the 69 constituents regarded by Foreman as standard.
#' These include astronomical frequencies and some shallow-water frequencies,
#' and are as follows: \code{c("Z0", "SA", "SSA", "MSM", "MM", "MSF", "MF",
#' "ALP1", "2Q1", "SIG1", "Q1", "RHO1", "O1", "TAU1", "BET1", "NO1", "CHI1",
#' "PI1", "P1", "S1", "K1", "PSI1", "PHI1", "THE1", "J1", "SO1", "OO1", "UPS1",
#' "OQ2", "EPS2", "2N2", "MU2", "N2", "NU2", "GAM2", "H1", "M2", "H2", "MKS2",
#' "LDA2", "L2", "T2", "S2", "R2", "K2", "MSN2", "ETA2", "MO3", "M3", "SO3",
#' "MK3", "SK3", "MN4", "M4", "SN4", "MS4", "MK4", "S4", "SK4", "2MK5", "2SK5",
#' "2MN6", "M6", "2MS6", "2MK6", "2SM6", "MSK6", "3MK7", "M8")}.
#'
#' \item Case 2. If the first item in \code{constituents} is the string
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
#' \item Case 3. If the first item is not \code{"standard"}, then the list of
#' constituents is processed as in Case 2, but without starting with the
#' standard list. As an example, \code{constituents=c("K1", "M2")} would fit
#' for just the K1 and M2 components. (It would be strange to use a minus sign
#' to remove items from the list, but the function allows that.)
#'
#' In each of the above cases, the list is reordered in frequency prior to the
#' analysis, so that the results of \code{\link{summary,tidem-method}} will be in a
#' familiar form.
#'
#' Once the constituent list is determined, \code{tidem} prunes the elements of
#' the list by using the Rayleigh criterion, according to which two
#' constituents of frequencies \eqn{f_1}{f1} and \eqn{f_2}{f2} cannot be
#' resolved unless the time series spans a time interval of at least
#' \eqn{rc/(f_1-f_2)}{rc/(f1-f2)}. The value \code{rc=1} yields nominal
#' resolution.
#'
#' A list of constituent names is created by the following: \preformatted{
#' data(tidedata) print(tidedata$const$name) }
#'
#' \strong{The text should include discussion of the (not yet performed) nodal
#' correction treatement.}
#' }
#'
#' @param t Either a \code{sealevel} object (e.g. produced by
#' \code{\link{read.sealevel}} or \code{\link{as.sealevel}}) or a vector of
#' times. In the former case, time is part of the object, so \code{t} may not
#' be given here.  In the latter case, \code{tidem} needs a way to determine
#' time, so \code{t} must be given.
#' @param x an optional numerical vector holding something that varies with
#' time.  This is ignored if \code{t} is a \code{\link{sealevel-class}} object,
#' in which case it is inferred as \code{t[["elevation"]]}.
#' @param constituents an optional list of tidal constituents to which the fit
#' is done (see \dQuote{Details}.)
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
#' \item 1.This function is not fully developed yet, and both the
#' form of the call and the results of the calculation may change.
#'
#' \item 2.Nodal correction is not done.
#'
#' \item 3.The reported \code{p} value may make no sense at all, and it might be
#' removed in a future version of this function. Perhaps a significance level
#' should be presented, as in the software developed by both Foreman and
#' Pawlowicz.
#'
#' }
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
tidem <- function(t, x, constituents, latitude=NULL, rc=1, regress=lm,
                  debug=getOption("oceDebug"))
{
    oceDebug(debug, "tidem(t, x, constituents,",
             "latitude=", if(is.null(latitude)) "NULL" else latitude, ", rc, debug) {\n", sep="", unindent=1)
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
            warning("tidem() switching first 2 args to permit old-style usage\n")
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

    cl <- match.call()
    startTime <- t[1]
    endTime <- tail(t, 1)
    centralTime <- numberAsPOSIXct((as.numeric(startTime)+as.numeric(endTime))/2, tz=attr(startTime, "tzone"))
    years <- as.numeric(difftime(endTime,startTime, units="secs")) / 86400 / 365.25
    if (years > 18.6)
        warning("Time series spans 18.6 years, but tidem() is ignoring this important fact")

    data("tidedata", package="oce", envir=environment())
    tidedata <- get("tidedata")#, pos=globalenv())
    tc <- tidedata$const
    ntc <- length(tc$name)

    if (debug > 0)
        print(tc)

    name <- freq <- kmpr <- NULL
    indices <- NULL
    standard <- tc$ikmpr > 0
    if (missing(constituents)) {
        name <- tc$name[standard][-1]
        freq <- tc$freq[standard][-1]
        kmpr <- tc$kmpr[standard][-1]
        indices <- c(indices, seq(1:ntc)[standard]) # FIXME: why is Z0 not chopped, as for last 3 lines?
        if (debug > 0)
            print(name)
    } else {
        nconst <- length(constituents)
        for (i in 1:nconst) {
            if (debug > 0)
                cat("[", constituents[i], "]\n",sep="")
            if (constituents[i] == "standard") { # must be first!
                if (i != 1)
                    stop("\"standard\" must occur first in constituents list")
                name <- tc$name[standard][-1]
                freq <- tc$freq[standard][-1]
                kmpr <- tc$kmpr[standard][-1]
                indices <- c(indices, seq(1:ntc)[tc$standard])
            } else {
                if (substr(constituents[i], 1, 1) == "-") {
                    cc <- substr(constituents[i], 2, nchar(constituents[i]))
                    delete <- which(tc$name == cc)
                    if (length(delete) == 1)
                        indices <- indices[indices != delete]
                    else
                        stop("cannot delete constituent '", cc, "' from the list because it is not there")
                } else {
                    add <- which(tc$name == constituents[i])
                    if (length(add) == 1) {
                        if (0 == sum(indices == add)) {
                            indices <- c(indices, add) # avoid duplicates
                        } else {
                            stop("cannot add constituent '", constituents[i], "' because it is not known; see ?tideconst")
                        }
                    }
                }
            }
            if (debug > 0)
                cat("<<", tc$name[indices], ">>\n")
        }
    }
    ## FIXME: what's going on here?  we already have name, etc.  What is tc2 for??
    indices <- indices[order(indices)]
    tc2 <- list(name=tc$name[indices], freq=tc$freq[indices], kmpr=tc$kmpr[indices])

    iZ0 <- which(tc2$name == "Z0")      # Remove Z0
    name <- tc2$name
    if (debug > 0)
        print(name)
    if (length(iZ0)) name <- name[-iZ0]
    nc <- length(name)
    index <- vector("numeric", nc)
    freq <- vector("numeric", nc)
    kmpr <- vector("numeric", nc)

    for (i in 1:nc) {                   # Build up based on constituent names
        ic <- which(tc$name == name[i])
        if (!length(ic))
            stop("there is no tidal constituent named \"", name[i], "\"")
        index[i] <- ic
        freq[i] <- tc$freq[ic]
        kmpr[i] <- tc$kmpr[ic]
    }
    nc <- length(freq)
    ## Check Rayleigh criterion
    interval <- as.numeric(difftime(max(sl@data$time,na.rm=TRUE), min(sl@data$time,na.rm=TRUE), units="hours"))
    drop.term <- NULL
    for (i in 1:nc) {
        cc <- which(tc2$name == kmpr[i])
        if (length(cc)) {
            cannot.fit <- (interval * abs(freq[i]-tc2$freq[cc])) < rc
            ##cat("compare name=", name[i], "with", kmpr[i],":", cannot.fit,"\n")
            if (cannot.fit)
                drop.term <- c(drop.term, i)
        }
    }
    if (length(drop.term) > 0) {
        if (debug > 0)
            cat("Record is too short to fit for constituents:", name[drop.term],"\n")
        index <- index[-drop.term]
        name <- name[-drop.term]
        freq <- freq[-drop.term]
        kmpr <- kmpr[-drop.term]
    }
    nc <- length(freq)
    elevation <- sl[["elevation"]]
    time <- sl[["time"]]
    nt <- length(elevation)
    x <- array(dim=c(nt, 2 * nc))
    x[,1] <- rep(1, nt)
    pi <- 4 * atan2(1, 1)
    ## tRef <- ISOdate(1899, 12, 31, 12, 0, 0, tz="UTC")
    tRef <- centralTime
    hour2pi <- 2 * pi * (as.numeric(time, tz="UTC") - as.numeric(tRef)) / 3600
    oceDebug(debug, "tRef=", tRef, "\n")
    oceDebug(debug, "nc=", nc, "\n")
    ##    cat(sprintf("hour[1] %.3f\n",hour[1]))
    ##    cat(sprintf("hour.offset[1] %.3f\n",hour.offset[1]))
    for (i in 1:nc) {
        oceDebug(debug, "setting coefficients for", name[i], "at", freq[i], "cph", "\n")
        ft <- freq[i] * hour2pi
        x[,2*i-1] <- sin(ft)
        x[,2*i  ] <- cos(ft)
    }
    name2 <- matrix(rbind(paste(name,"_S",sep=""), paste(name,"_C",sep="")), nrow=(length(name)), ncol=2)
    dim(name2) <- c(2 * length(name), 1)
    colnames(x) <- name2
    #model <- lm(elevation ~ x, na.action=na.exclude)
    model <- regress(elevation ~ x, na.action=na.exclude)
    if (debug > 0)
        print(summary(model))
    coef  <- model$coefficients
    if (4 == dim(summary(model)$coefficients)[2])
        p.all <- summary(model)$coefficients[,4]
    else
        p.all <- rep(NA, length=1+nc)
    amplitude <- phase <- p <- vector("numeric", length=1+nc)
    ## FIXME: should do offset/trend removal explicitly
    amplitude[1] <- coef[1]
    phase[1] <- 0
    p[1] <- p.all[1]
    for (i in seq.int(2,nc+1)) {
        is <- 2 * (i - 1)
        ic <- 2 * (i - 1) + 1
        s <- coef[is]                   # coefficient on sin(t)
        c <- coef[ic]                   # coefficient on cos(t)
        if (debug > 0)
            cat(name[i-1], "gives s=",s,"and c=",c,"\n")
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
        phaseOffset <- (vuf$u + vuf$v) * 360 * pi / 180 # the 360 is because tidemVuf returns in cycles
        phase[i] <- phase[i] + phaseOffset 
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

    res <- new('tidem')
    res@data <- list(model=model,
                      call=cl,
                      tRef=tRef,
                      const=c(1,   index),
                      name=c("Z0", name),
                      freq=c(0,    freq),
                      amplitude=amplitude,
                      phase=phase,
                      p=p)
    res@metadata$rc <- rc
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
#' oce.plot.ts(time, elevation, type='l', ylab="Height [m]", ylim=c(-2,6))
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
            x[,1] <- rep(1, nt)
            hour2pi <- 2 * pi * (as.numeric(tt) - as.numeric(object[["tRef"]])) / 3600
            for (i in 1:nc) {
                omega.t <- freq[i] * hour2pi
                x[,2*i-1] <- sin(omega.t)
                x[,2*i  ] <- cos(omega.t)
            }
            name2 <- matrix(rbind(paste(name,"_S",sep=""), paste(name,"_C",sep="")), nrow=(length(name)), ncol=2)
            dim(name2) <- c(2 * length(name), 1)
            colnames(x) <- name2
            res <- predict(object@data$model, newdata=list(x=x), ...)
        } else {
            stop("newdata must be of class POSIXt")
        }
    } else {
        res <- predict(object@data$model, ...)
    }
    as.numeric(res)
}



#' @title Get a Tidal Prediction from a WebTide Database
#'
#' @description
#' Get a tidal prediction from a WebTide database.
#'
#' If \code{action="map"} then a map is drawn, with a dot for the lower-left
#' corner of each triangle used in the finite-element tidal simulation upon
#' which WebTide predictions are based.  If \code{node} is missing, then
#' \code{\link{locator}} is called, so that the user can indicate a spot of
#' interest on the map, and this point is indicated on the map (and in the
#' return value).  If \code{node} is provided, however, the point is indicated
#' but \code{\link{locator}} is not called.  (This second style is of use in
#' documenting interactive work after the fact.)
#'
#' If \code{action="predict"} then either a node number or the longitude and
#' latitude must be specified.  If \code{plot=TRUE} (the default) then a plot
#' is drawn, but no plot is produced otherwise.  In either case, the (silent)
#' return value is a list as described in the next section.  The times used for
#' prediction are specified with the \code{time} argument, and if this is not
#' specified then a week following the present time is used.
#'
#' Naturally, \code{webtide} will not work unless WebTide has been installed on
#' the computer.
#'
#' @param action An indication of the action, either \code{action="map"} to
#' draw a map or \code{action="predict"} to get a prediction; see
#' \sQuote{Details}.
#' @param longitude longitude at which prediction is required (ignored if
#' \code{node} is given).
#' @param latitude latitude at which prediction is required (ignored if
#' \code{node} is given).
#' @param node node to look up; only needed if \code{longitude} and
#' \code{latitude} are not given.
#' @param time times at which prediction is to be made.  If not supplied, this
#' will be the week starting at the present time, incrementing by 15 minutes.
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
#' @return If \code{action="map"} and \code{plot=TRUE}, the return value is a
#' list containing the index of the nearest node, along with the
#' \code{latitude} and \code{longitude} of that node.  If \code{action="map"}
#' and \code{plot=FALSE}, the return value is a list of all nodes, longitude,
#' and latitudes.
#'
#' If \code{action="predict"}, the return value is a list containing a vector
#' of times (\code{time}), as well as vectors of the predicted \code{elevation}
#' in metres and the predicted horizontal components of velocity, \code{u} and
#' \code{v}, along with the \code{node} number, and the \code{basedir} and
#' \code{region} as supplied to this function.
#'
#' @source The WebTide software may be downloaded for free at the
#' Department of Fisheries and Oceans (Canada) website, which in February 2016
#' was
#' \code{http://www.bio.gc.ca/science/research-recherche/ocean/webtide/index-en.php},
#' although this site seems not to be particularly static.
#'
#' @section Caution:
#' WebTide is not an open-source application, so the present function was
#' designed based on little more than guesses about the WebTide file structure.
#' Users should be on the lookout for odd results.
#' @examples
#' \dontrun{
#' library(oce)
#' prediction <- webtide("predict", longitude=-69.61, latitude=48.14)
#' }
#' @author Dan Kelley
webtide <- function(action=c("map", "predict"),
                    longitude, latitude, node, time,
                    basedir=getOption("webtide"),
                    region="nwatl",
                    plot=TRUE, tformat, debug=getOption("oceDebug"), ...)
{
    action <- match.arg(action)
    subdir <- paste(basedir, "/data/", region, sep="")

    ## 2016-02-03: it seems that there are several possibilities for this filename.
    triangles <- NULL
    warn <- options("warn")$warn
    options(warn=-1)
    nodFile <- paste(subdir, "/", region, ".nod", sep="")
    t <- try({ triangles <- read.table(nodFile,
        col.names=c("triangle","longitude","latitude")) }, silent=TRUE)
    if (inherits(t, "try-error")) {
        nodFile <- paste(subdir, "/", region, "ll.nod", sep="")
        t <- try({ triangles <- read.table(nodFile,
            col.names=c("triangle","longitude","latitude")) }, silent=TRUE)
        if (inherits(t, "try-error")) {
            nodFile <- paste(subdir, "/", region, "_ll.nod", sep="")
            t <- try({ triangles <- read.table(nodFile,
                col.names=c("triangle","longitude","latitude")) }, silent=TRUE)
            if (inherits(t, "try-error")) {
                stop("cannot find WebTide nod file; last trial name was ", nodFile)
            } else {
                oceDebug(debug, "Found node information in ", nodFile, "\n")
            }
        } else {
            oceDebug(debug, "Found node information in ", nodFile, "\n")
        }
    } else {
        oceDebug(debug, "Found node information in ", nodFile, "\n")
    }
    if (is.null(triangles))
        stop("Could not find the '.nod' file")
    options(warn=warn)
    rm(warn)

    if (action == "map") {
        if (plot) {
            asp <- 1 / cos(pi/180*mean(range(triangles$latitude, na.rm=TRUE)))
            par(mfrow=c(1,1), mar=c(3,3,2,1), mgp=c(2,0.7,0))
            plot(triangles$longitude, triangles$latitude, pch=2, cex=1/4, lwd=1/8,
                 asp=asp, xlab="", ylab="", ...)
            ##usr <- par('usr')
            ##best <- coastlineBest(lonRange=usr[1:2], latRange=usr[3:4])
            warning("tidem: using default coastline for testing")
            data("coastlineWorld", package="oce", envir=environment())
            coastlineWorld <- get("coastlineWorld")
            ##data(best, envir=environment(), debug=debug-1)
            ##coastline <- get(best)
            lines(coastlineWorld[['longitude']], coastlineWorld[['latitude']])
            if (missing(node)) {
                point <- locator(1)
                node <- which.min(geodDist(triangles$longitude, triangles$latitude, point$x, point$y))
            }
            longitude <- triangles$longitude[node]
            latitude <- triangles$latitude[node]
            points(longitude, latitude, pch=20, cex=2, col='blue')
            legend("topleft", pch=20, pt.cex=2, cex=3/4, col='blue', bg='white',
                   legend=sprintf("node %.0f %.3fN %.3fE", node, latitude, longitude))
        } else  {
            node <- seq_along(triangles$longitude)
            longitude <- triangles$longitude
            latitude <- triangles$latitude
        }
        return(list(node=node, latitude=latitude, longitude=longitude))
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
        constituentse <- dir(path=subdir, pattern="*.s2c")
        constituentsuv <- dir(path=subdir, pattern="*.v2c")
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
            coneFile <- paste(subdir, constituentse[i], sep="/")
            cone <- read.table(coneFile, skip=3)[node,]
            ampe[i] <- cone[[2]]
            phasee[i] <- cone[[3]]
            conuvFile <- paste(subdir,constituentsuv[i],sep="/")
            conuv <- read.table(conuvFile, skip=3)[node,]
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
        for (i in 1:nconstituents) {
            twoLetter <- substr(constituentse[i], 1, 2)
            C <- which(twoLetter == tidedata$const$name)
            vuf <- tidemVuf(tRef, j=C, latitude=latitude)
            phaseOffset <- (vuf$u + vuf$v) * 360
            ## NOTE: phase is *subtracted* here, but *added* in tidem()
            elevation <- elevation + ampe[i] * cos((360 * h / period[i] - phasee[i] + phaseOffset) * pi / 180)
            ##> lines(time, elevation, col=i,lwd=3) ## Debug
            u <- u + ampu[i] * cos((360 * h / period[i] - phaseu[i] + phaseOffset) * pi / 180)
            v <- v + ampv[i] * cos((360 * h / period[i] - phasev[i] + phaseOffset) * pi / 180)
            oceDebug(debug, sprintf("%s ", twoLetter),  
                     sprintf("%4.2fh ", period[i]),
                     sprintf("%4.4fm ", ampe[i]), sprintf("%3.3fdeg", phasee[i]), "\n", sep="")
 
        }
        if (plot) {
            par(mfrow=c(3,1))
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
        }
    }
    invisible(list(time=time, elevation=elevation, u=u, v=v,
                   node=node, basedir=basedir, region=region))
}
