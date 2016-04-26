#' @title Class to Store Sealevel Data
#' 
#' Class to store sealevel data, e.g. from a tide gauge, with standard slots
#' \code{metadata}, \code{data} and \code{processingLog}.
#' 
#' @section Methods: Data may be accessed as e.g.  \code{sealevel[["time"]]},
#' where the string could also be e.g.  \code{"elevation"} for the
#' corresponding sea-level elevation, or e.g.  \code{"longitude"} or
#' \code{"latitude"} for scalars.  Items in \code{metadata} must be specifield
#' by full name, but those in \code{data} may be abbreviated, so long as the
#' abbreviation is unique.
#' 
#' Everything that may be accessed may also be assigned, e.g.
#' \code{sealevel[["elevation"]] <- value}.
#' 
#' The \code{show} method displays information about the object, while
#' \code{\link{summary,sealevel-method}} provides a statistical summary.
#' @author Dan Kelley
#' @family classes provided by \code{oce}
#' @family things related to \code{sealevel} data
setClass("sealevel", contains="oce")


#' @title Sealevel data for Halifax Harbour
#' 
#' This sample sea-level dataset is the 2003 record from Halifax Harbour in
#' Nova Scotia, Canada.  For reasons that are not mentioned on the data archive
#' website, the record ends on the 8th of October.
#' 
#' @name sealevel
#' @docType data
#' @author Dan Kelley
#' @source The data were created as \preformatted{ sealevel <-
#' read.oce("490-01-JAN-2003_slev.csv") sealevel <- oce.edit(sealevel,
#' "longitude", -sealevel[["longitude"]], reason="Fix longitude hemisphere") }
#' where the csv file was downloaded from [1]. Note the correction of longitude
#' sign, which is required because the data file has no indication that this is
#' the western hemisphere.
#'
#' @family datasets provided with \code{oce}
#' @family things related to \code{sealevel} data
NULL

#' Sea-level data set acquired in 1975 at Tuktoyaktuk
#' 
#' This sea-level dataset is provided with in Appendix 7.2 of Foreman (1977)
#' and also with the \code{T_TIDE} package (Pawlowicz et al., 2002). It results
#' from measurements made in 1975 at Tuktoyaktuk, Northwest Territories,
#' Canada.
#' 
#' The data set contains 1584 points, some of which have NA for sea-level
#' height.
#' 
#' Although Foreman's Appendix 7.2 states that times are in Mountain standard
#' time, the timezone is set to \code{UTC} in the present case, so that the
#' results will be similar to those he provides in his Appendix 7.3.
#' 
#' @name sealevelTuktoyaktuk
#' @docType data

#' @references Foreman, M. G. G., 1977.  Manual for tidal heights analysis and
#' prediction.  Pacific Marine Science Report 77-10, Institute of Ocean
#' Sciences, Patricia Bay, Sidney, BC, 58pp.
#' 
#' Pawlowicz, Rich, Bob Beardsley, and Steve Lentz, 2002.  Classical tidal
#' harmonic analysis including error estimates in MATLAB using \code{T_TIDE}.
#' Computers and Geosciences, 28, 929-937.
#' @source The data were based on the \code{T_TIDE} dataset, which in turn
#' seems to be based on Appendix 7.2 of Foreman (1977).  Minor editing was on
#' file format, and then the \code{sealevelTuktoyaktuk} object was created
#' using \code{\link{as.sealevel}}.
#' @examples
#' \dontrun{
#' library(oce)
#' data(sealevelTuktoyaktuk)
#' time <- sealevelTuktoyaktuk[["time"]]
#' elevation <- sealevelTuktoyaktuk[["elevation"]]
#' oce.plot.ts(time, elevation, type='l', ylab="Height [m]", ylim=c(-2,6))
#' legend("topleft", legend=c("Tuktoyaktuk (1975)","Detided"),
#'        col=c("black","red"),lwd=1)
#' tide <- tidem(sealevelTuktoyaktuk)
#' detided <- elevation - predict(tide)
#' lines(time, detided, col="red")
#' }
#' 
#' @family datasets provided with \code{oce}
#' @family things related to \code{sealevel} data
NULL

setMethod(f="initialize",
          signature="sealevel",
          definition=function(.Object, elevation, time) {
              if (!missing(elevation))
                  .Object@data$elevation <- elevation
              if (!missing(time))
                  .Object@data$time <- time 
              .Object@processingLog$time <- as.POSIXct(Sys.time())
              .Object@processingLog$value <- "create 'sealevel' object"
              return(.Object)
          })



#' @title Summarize a Sealevel Object
#' 
#' Summarizes some of the data in a sealevel object.
#' 
#' Pertinent summary information is presented.
#' 
#' @param object A \code{sealevel} object, i.e. one inheriting from \code{\link{sealevel-class}}.
#' @param \dots further arguments passed to or from other methods.
#' @return A matrix containing statistics of the elements of the \code{data}
#' slot.
#' @author Dan Kelley
#' @examples
#' library(oce)
#' data(sealevel)
#' summary(sealevel)
#' 
#' @family things related to \code{sealevel} data
setMethod(f="summary",
          signature="sealevel",
          definition=function(object, ...) {
              cat("Sealevel Summary\n----------------\n\n")
              showMetadataItem(object, "stationNumber",  "number:              ")
              showMetadataItem(object, "version", "version:             ")
              showMetadataItem(object, "stationName",    "name:                ")
              showMetadataItem(object, "region",  "region:              ")
              showMetadataItem(object, "deltat",  "sampling delta-t:    ")
              cat("* Location:           ",       latlonFormat(object@metadata$latitude,
                                                               object@metadata$longitude,
                                                               digits=5), "\n")
              showMetadataItem(object, "year",    "year:                ")
              ndata <- length(object@data$elevation)
              cat("* number of observations:  ", ndata, "\n")
              cat("*    \"      non-missing:   ", sum(!is.na(object@data$elevation)), "\n")
              callNextMethod()
          })



#' @title Subset a Sealevel Object
#' 
#' Subset a sealevel object
#' 
#' This function is somewhat analogous to \code{\link{subset.data.frame}}, but
#' subsetting is only permitted by time.
#' 
#' @param x A \code{sealevel} object, i.e. one inheriting from \code{\link{sealevel-class}}.
#' @param subset a condition to be applied to the \code{data} portion of
#' \code{x}.
#' @param \dots ignored.
#' @return A new \code{sealevel} object.
#' @author Dan Kelley
#' @examples
#' library(oce)
#' data(sealevel)
#' plot(sealevel)
#' plot(subset(sealevel, time < mean(range(sealevel[['time']]))))
#' 
#' @family things related to \code{sealevel} data
setMethod(f="subset",
          signature="sealevel",
          definition=function(x, subset, ...) {
              res <- new("sealevel")
              res@metadata <- x@metadata
              res@processingLog <- x@processingLog
              for (i in seq_along(x@data)) {
                  r <- eval(substitute(subset), x@data, parent.frame(2))
                  r <- r & !is.na(r)
                  res@data[[i]] <- x@data[[i]][r]
              }
              names(res@data) <- names(x@data)
              subsetString <- paste(deparse(substitute(subset)), collapse=" ")
              res@processingLog <- processingLogAppend(res@processingLog, paste("subset.sealevel(x, subset=", subsetString, ")", sep=""))
              res
          })
 


#' @title Extract Something From a Sealevel Object
#' @param x A sealevel object, i.e. one inheriting from \code{\link{sealevel-class}}.
#' @template sub_subTemplate
#' @family things related to \code{sealevel} data
setMethod(f="[[",
          signature(x="sealevel", i="ANY", j="ANY"),
          definition=function(x, i, j, ...) {
              callNextMethod()
          })

#' @title Replace Parts of a Sealevel Object
#' @param x An \code{sealevel} object, i.e. inheriting from \code{\link{sealevel-class}}
#' @template sub_subsetTemplate
#' @family things related to \code{sealevel} data
setMethod(f="[[<-",
          signature(x="sealevel", i="ANY", j="ANY"),
          definition=function(x, i, j, value) {
              callNextMethod(x=x, i=i, j=j, value=value)
          })

setValidity("sealevel",
            function(object) {
                ndata <- length(object@data)
                lengths <- vector("numeric", ndata)
                for (i in 1:ndata)
                    lengths[i] <- length(object@data[[i]])
                if (var(lengths) != 0) {
                    cat("lengths of data elements are unequal\n")
                    return(FALSE)
                } else
                    return(TRUE)
            })



#' @title Coerce Data Into a Sealevel Object
#' 
#' Coerces a dataset (minimally, a sequence of times and heights) into a
#' sealevel dataset.
#' 
#' The arguments are based on the standard data format, as were described in a
#' file formerly available at [1].
#' 
#' @param elevation a list of sea-level heights in metres, in an hourly
#' sequence.
#' @param time optional list of times, in POSIXct format.  If missing, the list
#' will be constructed assuming hourly samples, starting at 0000-01-01
#' 00:00:00.
#' @param header a character string as read from first line of a standard data
#' file.
#' @param stationNumber three-character string giving station number.
#' @param stationVersion single character for version of station.
#' @param stationName the name of station (at most 18 characters).
#' @param region the name of the region or country of station (at most 19
#' characters).
#' @param year the year of observation.
#' @param longitude the longitude in decimal degrees, positive east of
#' Greenwich.
#' @param latitude the latitude in decimal degrees, positive north of the
#' equator.
#' @param GMTOffset offset from GMT, in hours.
#' @param decimationMethod a coded value, with 1 meaning filtered, 2 meaning a
#' simple average of all samples, 3 meaning spot readings, and 4 meaning some
#' other method.
#' @param referenceOffset ?
#' @param referenceCode ?
#' @param deltat optional interval between samples, in hours (as for the
#' \code{\link{ts}} timeseries function). If this is not provided, and \code{t}
#' can be understood as a time, then the difference between the first two times
#' is used.  If this is not provided, and \code{t} cannot be understood as a
#' time, then 1 hour is assumed.
#' @return An object of \code{\link[base]{class}} \code{"sealevel"} (for
#' details, see \code{\link{read.sealevel}}).
#' @author Dan Kelley
#' @seealso The documentation for \code{\link{sealevel-class}} explains the
#' structure of sealevel objects, and also outlines the other functions dealing
#' with them.
#' @references \code{http://ilikai.soest.hawaii.edu/rqds/hourly.fmt} (this link
#' worked for years but failed at least temporarily on December 4, 2016).
#' @examples
#' library(oce)
#' 
#' # Construct a year of M2 tide, starting at the default time
#' # 0000-01-01T00:00:00.
#' h <- seq(0, 24*365)
#' elevation <- 2.0 * sin(2*pi*h/12.4172)
#' sl <- as.sealevel(elevation)
#' summary(sl)
#' 
#' # As above, but start at the Y2K time.
#' time <- as.POSIXct("2000-01-01") + h * 3600
#' sl <- as.sealevel(elevation, time)
#' summary(sl)
#' @family things related to \code{sealevel} data
as.sealevel <- function(elevation,
                        time,
                        header=NULL,
                        stationNumber=NA,
                        stationVersion=NA,
                        stationName=NULL,
                        region=NULL,
                        year=NA,
                        longitude=NA, latitude=NA,
                        GMTOffset=NA,
                        decimationMethod=NA,
                        referenceOffset=NA,
                        referenceCode=NA,
                        deltat)
{
    if (missing(elevation))
        stop("must supply sealevel height, elevation, in metres")
    res <- new('sealevel')
    n <- length(elevation)
    if (missing(time)) {              # construct hourly from time "zero"
        start <- as.POSIXct("0000-01-01 00:00:00", tz="UTC")
        time <- as.POSIXct(start + seq(0, n - 1, 1) * 3600, tz="UTC")
        if (is.na(GMTOffset))
            GMTOffset <- 0 # FIXME: do I want to do this?
    } else {
        time <- as.POSIXct(time, tz="UTC")
    }
    if (missing(deltat))
        deltat <- as.numeric(difftime(time[2], time[1], units="hours"))
    if (is.na(deltat) | deltat <= 0)
        deltat <- 1
    res@metadata$filename <- ""
    res@metadata$header <- header
    res@metadata$year <- year
    res@metadata$stationNumber <- stationNumber
    res@metadata$stationVersion <- stationVersion
    res@metadata$stationName <- stationName
    res@metadata$region <- region
    res@metadata$latitude <- latitude
    res@metadata$longitude <- longitude
    res@metadata$GMTOffset <- GMTOffset
    res@metadata$decimationMethod <- decimationMethod
    res@metadata$referenceOffset <- referenceOffset
    res@metadata$referenceCode <- referenceCode
    res@metadata$units <- list(elevation=list(unit=expression(m), scale=""))
    res@metadata$n <- length(t)
    res@metadata$deltat <- deltat
    res@data$elevation <- elevation
    res@data$time <- time
    res@processingLog <- processingLogAppend(res@processingLog, paste(deparse(match.call()),sep="",collapse=""))
    res
}


#' @title Plot Sealevel Data
#' 
#' Plot a summary diagram for sealevel data.
#' 
#' Creates a plot for a sea-level dataset, in one of two varieties.  Depending
#' on the length of \code{which}, either a single-panel or multi-panel plot is
#' drawn.  If there is just one panel, then the value of \code{par} used in
#' \code{plot,sealevel-method} is retained upon exit, making it convenient to add to
#' the plot.  For multi-panel plots, \code{par} is returned to the value it had
#' before the call, and so \code{adorn} must be used to add to individual
#' panels.
#' 
#' @param x an object of class \code{"sealevel"}, e.g. as read by
#' \code{\link{read.sealevel}}.
#' @param which a numerical or string vector indicating desired plot types,
#' with possibilities 1 or \code{"all"} for a time-series of all the data, 2 or
#' \code{"month"} for a time-series of just the first month, 3 or
#' \code{"spectrum"} for a power spectrum (truncated to frequencies below 0.1
#' cycles per hour, or 4 or \code{"cumulativespectrum"} for a cumulative
#' integral of the power spectrum.
#' @param adorn vector of expressions to be executed for the panels in turn,
#' e.g. to adorn the plots.  If the number matches the number of panels, then
#' the strings are applied to the appropriate panels, as they are drawn from
#' top-left to bottom-right.  If only a single expression is provided, it is
#' used for all panels. (See \dQuote{Examples}.)
#' @param drawTimeRange boolean that applies to panels with time as the
#' horizontal axis, indicating whether to draw the time range in the top-left
#' margin of the plot.
#' @param mgp 3-element numerical vector to use for \code{par(mgp)}, and also
#' for \code{par(mar)}, computed from this.  The default is tighter than the R
#' default, in order to use more space for the data and less for the axes.
#' @param mar value to be used with \code{\link{par}("mar")}.
#' @param marginsAsImage boolean, \code{TRUE} to put a wide margin to the right
#' of time-series plots, matching the space used up by a palette in an
#' \code{\link{imagep}} plot.
#' @param debug a flag that turns on debugging, if it exceeds 0.
#' @param \dots optional arguments passed to plotting functions.
#' @return None.
#' @author Dan Kelley
#' @seealso The documentation for \code{\link{sealevel-class}} explains the
#' structure of sealevel objects, and also outlines the other functions dealing
#' with them.
#' @references The example refers to Hurricane Juan, which caused a great deal
#' of damage to Halifax in 2003.  Since this was in the era of the digital
#' photo, a casual web search will uncover some spectacular images of damage,
#' from both wind and storm surge. A map of the path of Hurricane Juan across
#' Nova Scotia is at
#' \url{http://ec.gc.ca/ouragans-hurricanes/default.asp?lang=En&n=222F51F7-1}
#' (link checked April 15, 2015). Landfall, very near the site of this sealevel
#' gauge, was between 00:10 and 00:20 Halifax local time on Monday, Sept 29,
#' 2003.
#' @examples
#' library(oce)
#' data(sealevel)
#' ## local Halifax time is UTC + 4h; see [2] on timing
#' juan <- as.POSIXct("2003-09-29 00:15:00", tz="UTC")+4*3600
#' plot(sealevel, which=1, xlim=juan+86400*c(-7,7))
#' abline(v=juan, col='red')
#' 
#' @family functions that plot \code{oce} data
#' @family things related to \code{sealevel} data
setMethod(f="plot",
          signature=signature("sealevel"),
          definition=function(x, which=1:3,
                              adorn=NULL,
                              drawTimeRange=getOption("oceDrawTimeRange"),
                              mgp=getOption("oceMgp"),
                              mar=c(mgp[1]+0.5, mgp[1]+1.5, mgp[2]+1, mgp[2]+3/4),
                              marginsAsImage=FALSE,
                              debug=getOption("oceDebug"),
                              ...)
          {
              oceDebug(debug, "plot.sealevel(..., mar=c(", paste(mar, collapse=", "), "), ...) {\n",sep="", unindent=1)
              dots <- list(...)
              titlePlot<-function(x)
              {
                  title <- ""
                  if (!is.null(x@metadata$stationNumber) || !is.null(x@metadata$stationName) || !is.null(x@metadata$region))
                      title <- paste(title, gettext("Station ", domain="R-oce"),
                                     if (!is.na(x@metadata$stationNumber)) x@metadata$stationNumber else "",
                                     " ",
                                     if (!is.null(x@metadata$stationName)) x@metadata$stationName else "",
                                     " ",
                                     if (!is.null(x@metadata$region)) x@metadata$region else "",
                                     sep="")
                  if (!is.na(x@metadata$latitude) && !is.na(x@metadata$longitude))
                      title <- paste(title, latlonFormat(x@metadata$latitude, x@metadata$longitude), sep="")
                  if (nchar(title) > 0)
                      mtext(side=3, title, adj=1, cex=2/3)
              }
              drawConstituent<-function(frequency=0.0805114007,label="M2",col="darkred",side=1)
              {
                  abline(v=frequency, col=col)
                  mtext(label, side=side, at=frequency, col=col, cex=3/4*par("cex"))
              }
              drawConstituents<-function()
              {
                  drawConstituent(0.0387306544, "O1", side=1)
                  ##draw.constituent(0.0416666721, "S1", side=3)
                  drawConstituent(0.0417807462, "K1", side=3)
                  drawConstituent(0.0789992488, "N2", side=1)
                  drawConstituent(0.0805114007, "M2", side=3)
                  drawConstituent(0.0833333333, "S2", side=1)
              }

              if (!inherits(x, "sealevel"))
                  stop("method is only for objects of class '", "sealevel", "'")
              opar <- par(no.readonly = TRUE)
              par(mgp=mgp, mar=mar)
              lw <- length(which)
              if (marginsAsImage) {
                  scale <- 0.7
                  w <- (1.5 + par("mgp")[2]) * par("csi") * scale * 2.54 + 0.5
                  if (lw > 1)
                      lay <- layout(matrix(1:(2*lw), nrow=lw, byrow=TRUE), widths=rep(c(1, lcm(w)), lw))
              } else {
                  if (lw > 1)
                      lay <- layout(cbind(1:lw))
              }
              if (lw > 1) on.exit(par(opar))

              ## tidal constituents (in cpd):
              ## http://www.soest.hawaii.edu/oceanography/dluther/HOME/Tables/Kaw.htm
              adornLength <- length(adorn)
              if (adornLength == 1) {
                  adorn <- rep(adorn, 4)
                  adornLength <- 4
              }
              num.NA <- sum(is.na(x@data$elevation))

              par(mgp=mgp)
              ##par(mar=c(mgp[1],mgp[1]+2.5,mgp[2]+0.5,mgp[2]+1))
              par(mar=mar)
              MSL <- mean(x@data$elevation, na.rm=TRUE)
              if ("xlim" %in% names(dots)) {
                  xtmp <- subset(x@data$elevation, dots$xlim[1] <= x@data$time & x@data$time <= dots$xlim[2])
                  tmp <- max(abs(range(xtmp-MSL,na.rm=TRUE)))
              } else {
                  tmp <- max(abs(range(x@data$elevation-MSL,na.rm=TRUE)))
              }
              ylim <- c(-tmp,tmp)
              oceDebug(debug, "ylim=", ylim, "\n")
              n <- length(x@data$elevation) # do not trust value in metadata

              oceDebug(debug, "which:", which, "\n")
              which2 <- oce.pmatch(which, list(all=1, month=2, spectrum=3, cumulativespectrum=4))
              oceDebug(debug, "which2:", which2, "\n")

              for (w in 1:length(which2)) {
                  oceDebug(debug, "plotting for code which2[", w, "] = ", which2[w], "\n", sep="")
                  if (which2[w] == 1) {
                      plot(x@data$time, x@data$elevation-MSL,
                           xlab="",
                           ylab=resizableLabel("elevation"),
                           type='l', ylim=ylim, xaxs="i",
                           lwd=0.5, axes=FALSE, ...)
                      tics <- oce.axis.POSIXct(1, x@data$time, drawTimeRange=drawTimeRange, cex.axis=1, debug=debug-1)
                      box()
                      titlePlot(x)
                      yax <- axis(2)
                      abline(h=yax, col="darkgray", lty="dotted")
                      abline(v=tics, col="darkgray", lty="dotted")
                      abline(h=0,col="darkgreen")
                      mtext(side=4,text=sprintf("%.2f m",MSL),col="darkgreen", cex=2/3)
                  } else if (which2[w] == 2) {     # sample month 
                      from <- trunc(x@data$time[1], "day")
                      to <- from + 28 * 86400 # 28 days
                      look <- from <= x@data$time & x@data$time <= to
                      xx <- x
                      for(i in seq_along(x@data)) {
                          xx@data[[i]] <- x@data[[i]][look]
                      }
                      atWeek <- seq(from=from, to=to, by="week")
                      atDay  <- seq(from=from, to=to, by="day")
                      tmp <- (pretty(max(xx@data$elevation-MSL,na.rm=TRUE) -
                                     min(xx@data$elevation-MSL,na.rm=TRUE))/2)[2]
                      ylim <- c(-tmp,tmp)
                      plot(xx@data$time, xx@data$elevation - MSL,
                           xlab="",
                           ylab=resizableLabel("elevation"),
                           type='l',ylim=ylim, xaxs="i",
                           axes=FALSE)
                      oce.axis.POSIXct(1, xx@data$time, drawTimeRange=drawTimeRange, cex.axis=1, debug=debug-1)
                      yax <- axis(2)
                      abline(h=yax, col="lightgray", lty="dotted")
                      box()
                      abline(v=atWeek, col="darkgray", lty="dotted")
                      abline(v=atDay, col="lightgray", lty="dotted")
                      abline(h=0,col="darkgreen")
                      mtext(side=4,text=sprintf("%.2f m",MSL),col="darkgreen", cex=2/3)
                  } else if (which2[w] == 3) {
                      if (num.NA == 0) {
                          Elevation <- ts(x@data$elevation, start=1, deltat=x@metadata$deltat)
                          ##s <- spectrum(Elevation-mean(Elevation),spans=c(5,3),plot=FALSE,log="y",demean=TRUE,detrend=TRUE)
                          s <- spectrum(Elevation-mean(Elevation),plot=FALSE,log="y",demean=TRUE,detrend=TRUE)
                          par(mar=c(mgp[1]+1.25,mgp[1]+1.5,mgp[2]+0.25,mgp[2]+3/4))
                          xlim <- c(0, 0.1) # FIXME: should be able to set this
                          ylim <- range(subset(s$spec, xlim[1] <= s$freq & s$freq <= xlim[2]))
                          plot(s$freq,s$spec,xlim=xlim, ylim=ylim,
                               xlab=resizableLabel("frequency cph"),
                               ylab=resizableLabel("spectral density m2/cph"),
                               #[m^2/cph]",
                               type='l',log="y")
                          grid()
                          drawConstituents()
                      } else {
                          warning("cannot draw sealevel spectum, because the series contains missing values")
                      }
                  } else if (which2[w] == 4) {
                      if (num.NA == 0) {
                          n <- length(x@data$elevation)
                          Elevation <- ts(x@data$elevation, start=1, deltat=x@metadata$deltat)
                          s <- spectrum(Elevation-mean(Elevation),plot=FALSE,log="y",demean=TRUE,detrend=TRUE)
                          nCumSpec <- length(s$spec)
                          cumSpec <- sqrt(cumsum(s$spec) / nCumSpec)
                          ##e <- x@data$elevation - mean(x@data$elevation)
                          par(mar=c(mgp[1]+1.25,mgp[1]+2.5,mgp[2]+0.25,mgp[2]+0.25))
                          plot(s$freq, cumSpec,
                               xlab=resizableLabel("frequency cph"),
                               ylab=expression(paste(integral(Gamma,0,f)," df [m]")),
                               type='l',xlim=c(0,0.1))
                          if (adornLength > 3) {
                              t <- try(eval(adorn[4]), silent=TRUE)
                              if (class(t) == "try-error") warning("cannot evaluate adorn[", 4, "]\n")
                          }
                          grid()
                          drawConstituents()
                      } else {
                          warning("cannot draw sealevel spectum, because the series contains missing values")
                      }
                  } else {
                      stop("unrecognized value of which: ", which[w])
                  }
                  if (marginsAsImage)  {
                      ## blank plot, to get axis length same as for images
                      omar <- par("mar")
                      par(mar=c(mar[1], 1/4, mgp[2]+1/2, mgp[2]+1))
                      plot(1:2, 1:2, type='n', axes=FALSE, xlab="", ylab="")
                      par(mar=omar)
                  }
                  if (adornLength > 1) {
                      t <- try(eval(adorn[w]), silent=TRUE)
                      if (class(t) == "try-error")
                          warning("cannot evaluate adorn[", w, "]\n")
                  }
              }
              oceDebug(debug, "} # plot.sealevel()\n", unindent=1)
              invisible()
          })



#' @title Read a Sealevel File
#' 
#' Read a data file holding sea level data.  BUG: the time vector assumes GMT,
#' regardless of the GMT.offset value.
#' 
#' This function starts by scanning the first line of the file, from which it
#' determines whether the file is in one of two known formats: type 1, the
#' format used at the Hawaii archive centre, and type 2, the
#' comma-separated-value format used by the Marine Environmental Data Service.
#' (The file type is inferred by checking for the existence of the string
#' \code{Station_Name} on the first line of the file, indicating type 2.) If
#' the file is in neither of these formats, the user might wish to scan it
#' directly, and then to use \code{\link{as.sealevel}} to create a
#' \code{sealevel} object.
#' 
#' @param file a connection or a character string giving the name of the file
#' to load.  See Details for the types of files that are recognized.
#' @param tz time zone.  The default value, \code{oceTz}, is set to \code{UTC}
#' at setup.  (If a time zone is present in the file header, this will
#' supercede the value given here.)
#' @param processingLog if provided, the action item to be stored in the log.
#' (Typically only provided for internal calls; the default that it provides is
#' better for normal calls by a user.)
#' @template debugTemplate
#' @return An object of \code{\link{sealevel-class}}.
#' @author Dan Kelley
#' @references The Hawaii archive site at
#' \code{http://ilikai.soest.hawaii.edu/uhslc/datai.html} provides a graphical
#' interface for downloading sealevel data in Type 1, with format as described
#' at \code{http://ilikai.soest.hawaii.edu/rqds/hourly.fmt} (this link worked
#' for years but failed at least temporarily on December 4, 2016).  The MEDS
#' repository (\url{http://www.isdm-gdsi.gc.ca/isdm-gdsi/index-eng.html})
#' provides Type 2 data.
#' @examples
#' \dontrun{
#' library(oce)
#' # this yields the sealevel dataset
#' sl <- read.oce("h275a96.dat")
#' summary(sl)
#' plot(sl)
#' m <- tidem(sl)
#' plot(m)
#' }
#' 
#' @family things related to \code{sealevel} data
read.sealevel <- function(file, tz=getOption("oceTz"), processingLog, debug=getOption("oceDebug"))
{
    if (!is.character(file))
        stop("'file' must be a character string")
    fileOrig <- file
    filename <- fullFilename(file)
    if (is.character(file)) {
        file <- file(file, "r")
        on.exit(close(file))
    }
    if (!inherits(file, "connection")) {
        stop("argument `file' must be a character string or connection")
    }
    if (!isOpen(file)) {
        open(file, "r")
        on.exit(close(file))
    }
    firstLine <- scan(file, what='char', sep="\n", n=1, quiet=TRUE)
    header <- firstLine
    pushBack(firstLine, file)
    stationNumber <- NA
    stationVersion <- NA
    stationName <- NULL
    region <- NULL
    year <- NA
    latitude <- NA
    longitude <- NA
    GMTOffset <- NA
    decimationMethod <- NA
    referenceOffset <- NA
    referenceCode <- NA
    res <- new('sealevel')
    if (substr(firstLine, 1, 12) == "Station_Name") { # type 2
        oceDebug(debug, "File is of format 1 (e.g. as in MEDS archives)\n")
        ## Station_Name,HALIFAX
        ## Station_Number,490
        ## Latitude_Decimal_Degrees,44.666667
        ## Longitude_Decimal_Degrees,63.583333
        ## Datum,CD
        ## Time_Zone,AST
        ## SLEV=Observed Water Level
        ## Obs_date,SLEV
        ## 01/01/2001 12:00 AM,1.82,
        headerLength <- 8
        header <- readLines(file, n = headerLength)
        if (debug > 0) {
            print(header)
        }
        stationName   <- strsplit(header[1], ",")[[1]][2]
        stationNumber <- as.numeric(strsplit(header[2], ",")[[1]][2])
        latitude      <- as.numeric(strsplit(header[3], ",")[[1]][2])
        longitude     <- as.numeric(strsplit(header[4], ",")[[1]][2])
        tz            <- strsplit(header[6], ",")[[1]][2] # needed for get GMT offset
        GMTOffset     <- GMTOffsetFromTz(tz)
        x <- read.csv(file, header=FALSE, stringsAsFactors=FALSE, skip=headerLength)
        if (length(grep("[0-9]{4}/", x$V1[1])) > 0) {
            oceDebug(debug, "Date format is year/month/day hour:min with hour in range 1:24\n")
            time <- strptime(as.character(x$V1), "%Y/%m/%d %H:%M", "UTC") + 3600 * GMTOffset
        } else {
            oceDebug(debug, "Date format is day/month/year hour:min AMPM with hour in range 1:12 and AMPM indicating whether day or night\n")
            time <- strptime(as.character(x$V1), "%d/%m/%Y %I:%M %p", "UTC") + 3600 * GMTOffset
        }
        elevation <- as.numeric(x$V2)
        oceDebug(debug, "tz=", tz, "so GMTOffset=", GMTOffset,"\n",
                  "first pass has time string:", as.character(x$V1)[1], "\n",
                  "first pass has time start:", format(time[1]), " ", attr(time[1], "tzone"), "\n")
        year <- as.POSIXlt(time[1])$year + 1900
    } else { # type 1
        if(debug) cat("File is of type 2 (e.g. as in the Hawaii archives)\n")
        d <- readLines(file)
        n <- length(d)
        header <- d[1]
        stationNumber    <- substr(header,  1,  3)
        stationVersion   <- substr(header,  4,  4)
        stationName      <- substr(header,  6, 23)
        stationName      <- sub("[ ]*$","",stationName)
        region           <- substr(header, 25, 43)
        region           <- sub("[ ]*$","",region)
        year             <- substr(header, 45, 48)
        latitudeStr      <- substr(header, 50, 55) #degrees,minutes,tenths,hemisphere
        latitude <- as.numeric(substr(latitudeStr,   1, 2)) + (as.numeric(substr(latitudeStr,  3, 5)))/600
        if (tolower(substr(latitudeStr,  6, 6)) == "s") latitude <- -latitude
        longitudeStr     <- substr(header, 57, 63) #degrees,minutes,tenths,hemisphere
        longitude <- as.numeric(substr(longitudeStr, 1, 3)) + (as.numeric(substr(longitudeStr, 4, 6)))/600
        if (tolower(substr(longitudeStr, 7, 7)) == "w") longitude <- -longitude
        GMTOffset        <- substr(header, 65, 68) #hours,tenths (East is +ve)
        oceDebug(debug, "GMTOffset=", GMTOffset, "\n")
        decimationMethod <- substr(header, 70, 70) #1=filtered 2=average 3=spot readings 4=other
        referenceOffset  <- substr(header, 72, 76) # add to values
        referenceCode    <- substr(header, 77, 77) # add to values
        units            <- substr(header, 79, 80)
        oceDebug(debug, "units=", units, "\n")
        if (tolower(units) != "mm")
            stop("require units to be 'mm' or 'MM', not '", units, "'")
        elevation <- array(NA_real_, 12*(n-1))
        ## first.twelve.hours  <- 3600 * (0:11)
        ## second.twelve.hours <- 3600 * (12:23)
        twelve <- seq(1, 12, 1)
        last.day.portion <- -1 # ignored; prevents undefined warning in code analysis
        for (i in 2:n) {
            sp <- strsplit(d[i],"[ ]+")[[1]]
            target.index <- 12 * (i-2) + twelve
            elevation[target.index] <- as.numeric(sp[4:15])
            day.portion <- as.numeric(substr(sp[3], 9, 9))
            if (i == 2) {
                start.day <- as.POSIXct(strptime(paste(substr(sp[3],1,8),"00:00:00"), "%Y%m%d"), tz=tz)
            } else {
                if (day.portion == 1) {
                    if (i > 2 && last.day.portion != 2)
                        stop("non-alternating day portions on data line ", i)
                } else if (day.portion == 2) {
                    if (i > 2 && last.day.portion != 1)
                        stop("non-alternating day portions on data line ", i)
                } else {
                    stop("day portion is ", day.portion, " but must be 1 or 2, on data line", i)
                }
            }
            last.day.portion <- day.portion
        }
        time <- as.POSIXct(start.day + 3600 * (seq(0, 12*(n-1)-1)), tz=tz)
        elevation[elevation==9999] <- NA
        if (tolower(units) == "mm") {
            elevation <- elevation / 1000
        } else {
            stop("require units to be MM")
        }
    }
    num.missing <- sum(is.na(elevation))
    if (num.missing > 0) warning("there are ", num.missing, " missing points in this timeseries, at indices ", paste(which(is.na(elevation)), ""))
    res@metadata$filename <- filename
    res@metadata$header <- header
    res@metadata$year <- year
    res@metadata$stationNumber <- stationNumber
    res@metadata$stationVersion <- stationVersion
    res@metadata$stationName <- stationName
    res@metadata$region <- region
    res@metadata$latitude <- latitude
    res@metadata$longitude <- longitude
    res@metadata$GMTOffset <- GMTOffset
    res@metadata$decimationMethod <- decimationMethod
    res@metadata$referenceOffset <- referenceOffset
    res@metadata$referenceCode <- referenceCode
    res@metadata$units <- list(elevation=list(unit=expression(m), scale=""))
    res@metadata$n <- length(time)
    res@metadata$deltat <- as.numeric(difftime(time[2], time[1], units <- "hours"))
    if (missing(processingLog))
        processingLog <- paste('read.sealevel(file="', file, '", tz="', tz, sep="", collapse="")
    res@data$elevation <- elevation
    res@data$time <- time
    res@processingLog <- processingLogAppend(res@processingLog,
                                              paste('read.sealevel(file="', fileOrig, '", tz="', tz, '")', sep="", collapse=""))
    res
}

