## vim:textwidth=128:expandtab:shiftwidth=4:softtabstop=4

#' Class to store CTD (or general hydrographic) Data
#'
#' Class to store hydrographic data such as measured with a CTD (conductivity,
#' temperature, depth) instrument.
#'
#' Temperature is stored in the ITS-90 scale within the object, but the IPTS-68
#' value can be accessed with e.g.  \code{ctd[["temperature68"]]}, and this must
#' be done in using various seawater functions, e.g. the density function
#' \code{\link{swRho}}, if the UNESCO formulation is being requested.
#' (See \code{\link{[[,ctd-method}} and \code{\link{[[<-,ctd-method}} for
#' more on accessing and altering information within \code{ctd-class} objects.)
#'
#' The TEOS-10 notation for these quantities also works, with \code{ctd[["SP"]]},
#' \code{ctd[["t"]]} and \code{ctd[["p"]]} returning identical values to those
#' returned for the longer names.
#'
#' After the names listed above have been checked, the remaining names in the
#' \code{data} slot are checked using \code{\link{pmatch}}, so that e.g.
#' \code{ctd[["sal"]]} will recover practical \code{salinity}, \code{ctd[["sc"]]}
#' will recover \code{scan} (if it exists), etc.
#'
#' @section Accessing data:
#' Data may be extracted with \code{\link{[[,ctd-method}} and inserted
#' with \code{\link{[[<-,ctd-method}}. Type \code{?"[[,ctd-method"}
#' or \code{?"[[<-,ctd-method"} to learn more.
#'
#' Depth is accessed with e.g. \code{ctd[["depth"]]}, while its negative, the
#' vertical coordinate, is accessed with e.g.  \code{ctd[["z"]]}; note that these
#' are calculated using \code{\link{swDepth}} and \code{\link{swZ}}, and that any
#' values that may have been read in a data file are ignored.
#'
#' Potential temperature defined according to UNESCO-1980 is calculated with
#' \code{ctd[["theta"]]} or \code{ctd[["potential temperature"]]}.  Salinity is
#' retrieved with \code{ctd[["S"]]} or \code{ctd[["salinity"]]}.
#'
#' Conservative Temperature defined according to TEOS-2010 is calculated with
#' \code{ctd[["CT"]]} or \code{ctd[["conservative temperature"]]}.  Absolute
#' salinity is calculated with \code{ctd[["SA"]]} or \code{ctd[["absolute
#' salinity"]]}. Note that the salinity calculation requires a latitude and
#' longitude, and if the \code{ctd} object lacks those data, the values 300E and
#' 30N will be used as a default.
#'
#' The square of buoyancy frequency is retrieved with \code{ctd[["N2"]]} or
#' \code{\link{swN2}}, density ratio with \code{ctd[["Rrho"]]} and spiciness with
#' \code{ctd[["spice"]]}.
#'
#' @section Extracting values:
#' Items stored in the object may be altered with e.g.  \code{ctd[["salinity"]]
#'   <- rep(35,10)}.  For obvious reasons, this does not work with derived
#' quantities such as conservative temperature, etc.
#'
#' @section Reading/creating data:
#' A file containing CTD profile data may be read with
#' \code{\link{read.ctd}}, and a CTD object can also be created with
#' \code{\link{as.ctd}}.  See \code{\link{read.ctd}} for references on data
#' formats used in CTD files. Data can also be assembled into
#' \code{ctd} objects with \code{\link{as.ctd}}.
#'
#'
#' Statistical summaries are provided by \code{\link{summary,ctd-method}}, while
#' \code{\link{show}} displays an overview.
#'
#' CTD objects may be plotted with \code{\link{plot,ctd-method}}, which does much of its
#' work by calling \code{\link{plotProfile}} or \code{\link{plotTS}}, both of
#' which can also be called by the user, to get fine control over the plots.
#'
#' A CTD profile can be isolated from a larger record with \code{\link{ctdTrim}},
#' a task made easier when \code{\link{plotScan}} is used to examine the results.
#' Towyow data can be split up into sets of profiles (ascending or descending)
#' with \code{\link{ctdFindProfiles}}.  CTD data may be smoothed and/or cast onto
#' specified pressure levels with \code{\link{ctdDecimate}}.
#'
#' Low-level manipulation may be done with functions such as
#' \code{\link{ctdAddColumn}} and \code{\link{ctdUpdateHeader}}.  Additionally,
#' many of the contents of CTD objects may be altered with the \code{\link{[[,ctd-method}} scheme
#' discussed above, and skilled users may also manipulate the contents directly.
#'
#' @author Dan Kelley
#' 
#' @family things related to \code{ctd} data
#' @family classes provided by \code{oce}
setClass("ctd", contains="oce")


#' A CTD profile in Halifax Harbour.
#' 
#' This is a CTD profile measured in Halifax Harbour in 2003, based
#' on \code{\link{ctdRaw}}, but trimmed to just the downcast with
#' \code{\link{ctdTrim}}, using indices inferred by inspection of the
#' results from \code{\link{plotScan}}.
#'
#' This station was sampled by students enrolled in the Dan Kelley's
#' Physical Oceanography class at Dalhousie University.
#' The data were acquired near the centre of the Bedford Basin of the
#' Halifax Harbour, during an October 2003 field trip of Dalhousie University's
#' Oceanography 4120/5120 class. The original \code{.cnv} data file had
#' temperature in the IPTS-68 scale, but this was converted to the more modern
#' scale using \code{\link{T90fromT68}}.
#'
#' @name ctd
#' @docType data
#'
#' @usage data(ctd)
#' 
#' @examples
#' \dontrun{
#' library(oce)
#' data(ctd)
#' plot(ctd)
#' }
#' 
#' @seealso The full profile (not trimmed to the downcast) is available as
#' \link{ctdRaw}.
#' 
#' @family datasets provided with \code{oce}
#' @family things related to \code{ctd} data
NULL

#' Seawater CTD profile, without trimming of extraneous data.
#' 
#' 
#' This is sample CTD profile provided for testing.  It includes not just the
#' (useful) portion of the dataset during which the instrument was being lowered,
#' but also data from the upcast and from time spent near the surface.  Spikes are
#' also clearly evident in the pressure record.  With such real-world wrinkles,
#' this dataset provides a good example of data that need trimming with
#' \code{\link{ctdTrim}}.
#' 
#' This station was sampled by students enrolled in the Dan Kelley's
#' Physical Oceanography class at Dalhousie University.
#' The data were acquired near the centre of the Bedford Basin of the
#' Halifax Harbour, during an October 2003 field trip of Dalhousie University's
#' Oceanography 4120/5120 class. The original \code{.cnv} data file had
#' temperature in the IPTS-68 scale, but this was converted to the more modern
#' scale using \code{\link{T90fromT68}}.
#'
#' @name ctdRaw
#' @docType data
#'
#' @usage data(ctdRaw)
# 
#' @seealso A similar dataset (trimmed to the downcast) is available as
#' \code{\link{ctd}}.
#' 
#' @family things related to \code{ctd} data
#' @family datasets provided with \code{oce}
NULL


## DEVELOPERS: please pattern functions and documentation on this, for uniformity.
## DEVELOPERS: You will need to change the docs, and the 3 spots in the code
## DEVELOPERS: marked '# DEVELOPER 1:', etc.
#' @title Handle flags in \code{ctd} objects
#' @details
#' If \code{flags} and \code{actions} are not provided, the
#' default is to use WHP (World Hydrographic Program) flags [1], in which the
#' value 2 indicates good data, and other values indicate either unchecked,
#' suspicious, or bad data. Any data not flagged as good are set
#' to \code{NA} in the returned value. Since WHP flag codes run
#' from 1 to 9, this default is equivalent to
#' setting \code{flags=list(all=c(1, 3:9))} along with
#' \code{action=list("NA")}.
#' @param object An object of \code{\link{ctd-class}}.
#' @template handleFlagsTemplate
#' @references
#' 1. \url{https://www.nodc.noaa.gov/woce/woce_v3/wocedata_1/whp/exchange/exchange_format_desc.htm}
#' @examples
#' library(oce)
#' data(section)
#' stn <- section[["station", 100]]
#' # 1. Default: anything not flagged as 2 is set to NA, to focus
#' # solely on 'good', in the World Hydrographic Program scheme.
#' STN <- handleFlags(stn)
#' data.frame(old=stn[['salinity']], flag=stn[['salinityFlag']], new=STN[['salinity']])
#'
#' # 2. A less restrictive case: include also 'questionable' data,
#' # and only apply this action to salinity.
#' STN <- handleFlags(stn, flags=list(salinity=c(1, 4:9)))
#'
#' # 3. Use smoothed TS relationship to nudge questionable data.
#' # This is perhaps a silly idea, but at least it illustrates
#' # how to write a nontrivial function for an action.
#' f<-function(x) {
#'   S <- x[["salinity"]]
#'   T <- x[["temperature"]]
#'   df <- 0.5 * length(S) # smooths a bit
#'   sp <- smooth.spline(T, S, df=df)
#'   0.5 * (S + predict(sp, T)$y)
#' }
#' par(mfrow=c(1,2))
#' STN <- handleFlags(stn, flags=list(salinity=c(1,3:9)), action=list(salinity=f))
#' plotProfile(stn, "salinity", mar=c(3, 3, 3, 1))
#' p <- stn[['pressure']]
#' par(mar=c(3, 3, 3, 1))
#' plot(STN[['salinity']] - stn[['salinity']], p, ylim=rev(range(p)))
#'
#' @family things related to \code{ctd} data
setMethod("handleFlags",
          c(object="ctd", flags="ANY", actions="ANY"),
          function(object, flags=list(), actions=list()) {
              ## DEVELOPER 1: alter the next comment to explain your setup
              ## Default to the World Hydrographic Program system, with
              ## flags from 1 to 9, with flag=2 for acceptable data.
              if (missing(flags))
                  flags <- list(c(1, 3:9)) # DEVELOPER 2: alter this line to suit a newdata class
              if (missing(actions)) {
                  actions <- list("NA") # DEVELOPER 3: alter this line to suit a new data class
                  names(actions) <- names(flags)
              }
              if (any(names(actions)!=names(flags))) {
                  stop("names of flags and actions must match")
              }
              handleFlagsInternal(object, flags, actions)
          })

## To save storage, this new() function has arguments only for quantities that are present in almost all cases. For example, not
## all data files will have oxygen, so that's not present here. Similarly, not all files have data-quality columns, so they are
## not present either. Columnar data should be added after the object is created, using ctdAddColumn(), which updates metadata
## as needed. As for adding metadata, do that directly. Examples of these things are seen throughout this file.  Note that
## normal users should employ read.ctd() or as.ctd() to create ctd objects ... this function is intended for internal use, and
## may be changed at any moment.


setMethod(f="initialize",
          signature="ctd",
          definition=function(.Object, pressure, salinity, temperature, conductivity,
                              units,
                              pressureType, deploymentType) {
              ## Assign to some columns so they exist if needed later (even if they are NULL)
              .Object@data$pressure <- if (missing(pressure)) NULL else pressure
              .Object@data$temperature <- if (missing(temperature)) NULL else temperature
              .Object@data$salinity <- if (missing(salinity)) NULL else salinity
              .Object@data$conductivity <- if (missing(conductivity)) NULL else conductivity
              names <- names(.Object@data)
              .Object@metadata$names <- names
              .Object@metadata$labels <- titleCase(names) # paste(toupper(substring(names,1,1)), substring(names,2),sep="")
              ##.Object@metadata$filename <- filename
              if (missing(units)) {
                  .Object@metadata$units <- list(temperature=list(unit=expression(degree*C), scale="ITS-90"),
                                                 salinity=list(unit=expression(), scale="PSS-78"),
                                                 conductivity=list(unit=expression(ratio), scale=""),
                                                 pressure=list(unit=expression(dbar), scale=""),
                                                 depth=list(unit=expression(m), scale=""))
              } else {
                  .Object@metadata$units <- units # FIXME: but what if spelled wrong etc
              }
              .Object@metadata$pressureType <- if (!missing(pressureType)) pressureType else "sea" # guess on the unit
              .Object@metadata$deploymentType <- if (!missing(deploymentType)) deploymentType else "unknown" # "profile" "mooring" "towyo" "thermosalinograph"
              .Object@metadata$waterDepth <- NA
              #.Object@metadata$latitude <- NA
              #.Object@metadata$longitude <- NA
              #.Object@metadata$waterDepth <- NA
              .Object@processingLog$time <- as.POSIXct(Sys.time())
              .Object@processingLog$value <- "create 'ctd' object"
              return(.Object)
          })


#' Summarize a \code{ctd} Object
#' 
#' Summarizes some of the data in a \code{ctd} object, presenting such information
#' as the station name, sampling location, data ranges, etc.
#'
#' @param object An object of class \code{"ctd"}, usually, a result of a call to
#' \code{\link{read.ctd}}, \code{\link{read.oce}}, or \code{\link{as.ctd}}.
#' 
#' @param ... Further arguments passed to or from other methods.
#' 
#' @examples
#' library(oce)
#' data(ctd)
#' summary(ctd)
#' 
#' @author Dan Kelley
#' 
#' @family things related to \code{ctd} data
setMethod(f="summary",
          signature="ctd",
          definition=function(object, ...) {
              ##mnames <- names(object@metadata)
              cat("CTD Summary\n-----------\n\n")
              showMetadataItem(object, "type",                      "Instrument:          ")
              showMetadataItem(object, "model",                     "Instrument model:    ")
              showMetadataItem(object, "serialNumber",              "Instr. serial no.:   ")
              showMetadataItem(object, "serialNumberTemperature",   "Temp. serial no.:    ")
              showMetadataItem(object, "serialNumberConductivity",  "Cond. serial no.:    ")
              showMetadataItem(object, "filename",                  "File:                ")
              showMetadataItem(object, "hexfilename",               "Original file:       ")
              showMetadataItem(object, "institute",                 "Institute:           ")
              showMetadataItem(object, "scientist",                 "Chief scientist:     ")
              showMetadataItem(object, "date",                      "Date:                ", isdate=TRUE)
              showMetadataItem(object, "startTime",                 "Start time:          ", isdate=TRUE)
              showMetadataItem(object, "systemUploadTime",          "System upload time:  ", isdate=TRUE)
              showMetadataItem(object, "cruise",                    "Cruise:              ")
              showMetadataItem(object, "ship",                      "Vessel:              ")
              showMetadataItem(object, "station",                   "Station:             ")
              showMetadataItem(object, "deploymentType",            "Deployment type:     ")
              if ("longitude" %in% names(object@data)) {
                  cat("* Mean location:      ",       latlonFormat(mean(object@data$latitude, na.rm=TRUE),
                                                                   mean(object@data$longitude, na.rm=TRUE),
                                                                   digits=5), "\n")
              } else if ("longitude" %in% names(object@metadata)) {
                  cat("* Location:           ",       latlonFormat(object@metadata$latitude,
                                                                   object@metadata$longitude,
                                                                   digits=5), "\n")
              } else {
                  cat("* Mean location:      unknown\n")
              }
              showMetadataItem(object, "waterDepth", "Water depth:         ")
              showMetadataItem(object, "levels", "Number of levels: ")
              callNextMethod()
          })

#' @title Extract Parts of a \code{ctd} Object
#' @param x A \code{ctd} object, i.e. one inheriting from \code{\link{ctd-class}}.
#' @template sub_subTemplate
#'
#' @examples
#' data(ctd)
#' head(ctd[["temperature"]])
#'
#' @section Details of the specialized \code{ctd} method:
#'
#' Some uses of \code{[[,ctd-method} involve direct retrieval of
#' items within the \code{data} slot of the \code{ctd} object, 
#' while other uses involve calculations based on items in that
#' \code{data} slot. For an example, all \code{ctd} objects
#' should hold an item named \code{temperature} in the \code{data}
#' slot, so for example \code{x[["temperature"]]} will retrieve that
#' item. By contrast, \code{x[["sigmaTheta"]]} is taken to be a
#' request to compute \eqn{\sigma_\theta}{sigma[theta]}, and so
#' it yields a call to \code{\link{swTheta}(x)} \emph{even if}
#' the \code{data} slot of \code{x} might happen to contain an item
#' named \code{theta}. This can be confusing at first, but it tends
#' to lead to fewer surprises in everyday work, for otherwise the
#' user would be forced to check the contents of any \code{ctd}
#' object under analysis, to determine whether that item will be looked
#' up or computed. Nothing is lost in this scheme, since the data
#' within the object are always accessible with a direct call, e.g.
#' \code{x@data$sigmaTheta}, or a with \code{\link{oceGetData}}.
#'
#' This preference for computed over stored quantities is accomplished
#' by first checking for computed quantities, and then falling
#' back to the general \code{\link{[[}} method if no match is found.
#' The computed quantities are as follows.
#'
#' \itemize{
#'
#' \item{\code{CT} or \code{Conservative Temperature}: Conservative Temperature,
#' computed with \code{\link[gsw]{gsw_CT_from_t}} in the \code{gsw} package.}
#'
#' \item{\code{depth} Depth in metres below the surface, computed with \code{\link{swDepth}(x)}.}
#'
#' \item{\code{N2} Square of Brunt-Vaisala frequency, computed  with \code{\link{swN2}(x)}.}
#'
#' \item{\code{potential temperature}, potential temperature in the UNESCO formulation,
#' computed with \code{\link{swTheta}(x)}. This is a synonym for \code{theta}.}
#'
#' \item{\code{Rrho} density ratio, computed with \code{\link{swRrho}(x)}.}
#'
#' \item{\code{SA} or \code{Absolute Salinity}: Absolute Salinity,
#' computed with \code{\link[gsw]{gsw_SA_from_SP}} in the \code{gsw} package.}
#'
#' \item{\code{sigmaTheta} a form of potential density anomaly, computed with
#' \code{\link{swSigmaTheta}(x)}.}
#'
#' \item{\code{sigma0} equal to \code{sigmaTheta}, i.e. potential density anomaly
#' referenced to a pressure of 0dbar, computed with \code{\link{swSigma0}(x)}.}
#'
#' \item{\code{sigma1} potential density anomaly
#' referenced to a pressure of 1000dbar, computed with \code{\link{swSigma1}(x)}.}
#'
#' \item{\code{sigma2} potential density anomaly
#' referenced to a pressure of 2000dbar, computed with \code{\link{swSigma2}(x)}.}
#'
#' \item{\code{sigma3} potential density anomaly
#' referenced to a pressure of 3000dbar, computed with \code{\link{swSigma3}(x)}.}
#'
#' \item{\code{sigma4} potential density anomaly
#' referenced to a pressure of 4000dbar, computed with \code{\link{swSigma4}(x)}.}
#'                                        
#' \item{\code{SP} salinity on the Practical Salinity Scale, which is \code{x@data$salinity}.}
#'
#' \item{\code{spice} a variable that is in some sense orthogonal to density, calculated
#' with \code{\link{swSpice}(x)}.}
#'
#' \item{\code{SR} Reference Salinity computed with \code{\link[gsw]{gsw_SR_from_SP}} in
#' the \code{gsw} package.}
#'
#' \item{\code{Sstar} Preformed Salinity computed with \code{\link[gsw]{gsw_SR_from_SP}} in
#' the \code{gsw} package.}
#'
#' \item{\code{temperature68}, temperature on the IPTS-1968 scale, computed
#' with \code{\link{T68fromT90}(x)}.}
#'
#' \item{\code{theta}, potential temperature in the UNESCO formulation,
#' computed with \code{\link{swTheta}(x)}. This is a synonym for \code{potential temperature}.}
#'
#' \item{\code{z} Vertical coordinate in metres above the surface, computed with
#' \code{\link{swZ}(x)}.}
#'
#' }
#'
#' @family things related to \code{ctd} data
setMethod(f="[[",
          signature(x="ctd", i="ANY", j="ANY"),
          ##definition=function(x, i, j=NULL, drop=NULL) {
          definition=function(x, i, j, ...) {
              ##dataNames <- names(x@data)
              if (i == "salinity" || i == "SP") {
                  x@data$salinity
              } else if (i == "SR") {
                  gsw::gsw_SR_from_SP(SP=x@data$salinity)
              } else if (i == "Sstar") {
                  SA <- gsw::gsw_SA_from_SP(SP=x@data$salinity, p=x@data$pressure,
                                            longitude=x@metadata$longitude,
                                            latitude=x@metadata$latitude)
                  gsw::gsw_Sstar_from_SA(SA=SA, p=x@data$pressure,
                                         longitude=x@metadata$longitude,
                                         latitude=x@metadata$latitude)
              } else if (i == "temperature") {
                  x@data$temperature
              } else if (i == "temperature68") {
                  T68fromT90(x@data$temperature)
              } else if (i == "pressure") {
                  x@data$pressure
              } else if (i == "longitude") {
                  if ("longitude" %in% names(x@data)) x@data$longitude else x@metadata$longitude
              } else if (i == "latitude") {
                  if ("latitude" %in% names(x@data)) x@data$latitude else x@metadata$latitude
              } else if (i == "N2") {
                  swN2(x)
              } else if (i == "sigmaTheta") {
                  swSigmaTheta(x)
              } else if (i == "sigma0") {
                  swSigma0(x)
              } else if (i == "sigma1") {
                  swSigma1(x)
              } else if (i == "sigma2") {
                  swSigma2(x)
              } else if (i == "sigma3") {
                  swSigma3(x)
              } else if (i == "sigma4") {
                  swSigma4(x)
              } else if (i %in% c("theta", "potential temperature")) {
                  swTheta(x)
              } else if (i == "Rrho") {
                  swRrho(x)
              } else if (i == "spice") {
                  swSpice(x)
              } else if (i %in% c("absolute salinity", "SA")) {
                  SP <- x@data$salinity
                  t <- x@data$temperature
                  p <- x@data$pressure
                  n <- length(SP)
                  lon <- x@metadata$longitude
                  if (n != length(lon))
                      lon <- rep(x@metadata$longitude, length.out=n)
                  lon <- ifelse(lon < 0, lon + 360, lon)
                  haveLatLon <- TRUE
                  if (!any(is.finite(lon))) {
                      lon <- rep(300, n)
                      haveLatLon <- FALSE
                  }
                  lat <- x@metadata$latitude
                  if (n != length(lat))
                      lat <- rep(x@metadata$latitude, length.out=n)
                  if (!any(is.finite(lat))) {
                      lat <- rep(30, n)
                      haveLatLon <- FALSE
                  }
                  SP[is.nan(SP)] <- NA
                  p[is.nan(p)] <- NA
                  lat[is.nan(lat)] <- NA
                  lon[is.nan(lon)] <- NA
                  gsw::gsw_SA_from_SP(SP, p, lon, lat)
              } else if (i %in% c("conservative temperature", "CT")) {
                  SP <- x@data$salinity
                  t <- x@data$temperature
                  p <- x@data$pressure
                  gsw::gsw_CT_from_t(SP, t, p)
              } else if (i == "z") {
                  ## FIXME-gsw: permit gsw version here
                  swZ(x)
              } else if (i == "depth") {
                  ## FIXME-gsw: permit gsw version here
                  swDepth(x)
              } else if (i == "N2") {
                  swN2(x)
              } else {
                  callNextMethod()
              }
          })

#' @title Replace Parts of a \code{ctd} Object
#' @param x A \code{ctd} object, i.e. inheriting from \code{\link{ctd-class}}
#' @template sub_subsetTemplate
#'
#' @examples
#' data(ctd)
#' summary(ctd)
#' # Move the CTD profile a nautical mile north.
#' ctd[["latitude"]] <- 1/60 + ctd[["latitude"]] # acts in metadata
#' # Increase the salinity by 0.01.
#' ctd[["salinity"]] <- 0.01 + ctd[["salinity"]] # acts in data
#' summary(ctd)
#'
#' @family things related to \code{ctd} data
setMethod(f="[[<-",
          signature(x="ctd", i="ANY", j="ANY"),
          definition=function(x, i, j, ..., value) { # FIXME: use j for e.g. times
              callNextMethod(x=x, i=i, j=j, value=value)
          })


#' Coerce data into ctd dataset.
#' 
#' Assemble data into a \code{\link{ctd-class}} dataset.
#' 
#' If the first argument is an \code{\link{rsk-class}} object, the pressure it
#' contains may need to be adjusted, because \code{rsk} objects may contain either
#' absolute pressure or sea pressure. This adjustment is handled automatically by
#' \code{as.ctd}, by examination of the metadata item named \code{pressureType}
#' (described in the documentation for \code{\link{read.rsk}}).  Once the sea
#' pressure is determined, adjustments may be made with the
#' \code{pressureAtmospheric} argument, although in that case it is better
#' considered a pressure adjustment than the atmospheric pressure.
#' 
#' \code{\link{rsk-class}} objects may store sea pressure or absolute pressure (the
#' sum of sea pressure and atmospheric pressure), depending on how the object was
#' created with \code{\link{as.rsk}} or \code{\link{read.rsk}}.  However,
#' \code{\link{ctd-class}} objects store sea pressure, which is needed for
#' plotting, calculating density, etc. This poses no difficulities, however,
#' because \code{as.ctd} automatically converts absolute pressure to sea pressure,
#' if the metadata in the \code{\link{rsk-class}} object indicates that this is
#' appropriate. Further alteration of the pressure can be accomplished with the
#' \code{pressureAtmospheric} argument, as noted above.
# 
#' @param salinity There are two choices for \code{salinity}. First, it can be a
#' vector indicating the practical salinity through the water column. In that case,
#' \code{as.ctd} employs the other arguments listed below. The second choice is
#' that \code{salinity} is something from which practical salinity, temperature,
#' etc., can be inferred. In that case, the relevant information is extracted and
#' the other arguments to \code{as.ctd} are ignored, except for
#' \code{pressureAtmospheric}; see \dQuote{Details}.
#' 
#' @param temperature \emph{in-situ} temperature [\eqn{^\circ deg}C], defined on
#' the ITS-90 scale; see \dQuote{Temperature units} in the documentation for
#' \code{\link{swRho}}.
#' 
#' @param pressure Vector of pressure values, one for each \code{salinity} and
#' \code{temperature} pair, or just a single pressure, which is repeated to match
#' the length of \code{salinity}.
#' 
#' @param conductivity electrical conductivity ratio through the water column
#' (optional). To convert from raw conductivity in milliSeimens per centimeter
#' divide by 42.914 to get conductivity ratio (see Culkin and Smith, 1980).
#' 
#' @param SA absolute salinity (as in TEOS-10).  If given, the supplied absolute
#' salinity is converted internally to UNESCO-defined practical salinity.
#' 
#' @param CT conservative temperature (as in TEOS-10).  If given, the supplied
#' conservative temperature is converted internally to UNESCO-defined in-situ
#' temperature.
#' 
#' @param oxygen optional oxygen concentration
#' 
#' @param nitrate optional nitrate concentration
#' 
#' @param nitrite optional nitrite concentration
#' 
#' @param phosphate optional phosphate concentration
#' 
#' @param silicate optional silicate concentration
#' 
#' @param scan optional scan number.  If not provided, this will be set to
#' \code{1:length(salinity)}.
#' 
#' @param time optional vector of times of observation
#' 
#' @param other optional list of other data columns that are not in the standard
#' list
#' 
#' @param units an optional list containing units.  If not supplied, a default of
#' \code{list(temperature=list(unit=expression(degree*C), scale="ITS-90"),
#'   salinity=list(unit=expression(ratio), scale="",
#'   pressure=list(unit=expression(dbar), scale="")} is used. This is quite
#' typical of archived datasets, but for some instrumental files it will make
#' sense to use \code{salinity=list(unit=expression(uS/cm), scale="")} or 
#' \code{salinity=list(unit=expression(S/m), scale="")}.
#' 
#' @param flags if supplied, this is a \code{\link{list}} containing data-quality
#' flags. The elements of this list must have names that match the data
#' provided to the object.
#' 
#' @param pressureType a character string indicating the type of pressure; may be
#' \code{"absolute"}, for total pressure, i.e. the sum of atmospheric pressure
#' and sea pressure, or \code{"sea"}.
#' 
#' @param missingValue optional missing value, indicating data that should be
#' taken as \code{NA}.
#' 
#' @param quality optional quality flag, e.g. from the salinity quality flag in WOCE data.
#' (In WOCE, \code{quality=2} indicates good data, \code{quality=3} means
#' questionable data, and \code{quality=4} means bad data. (NOTE: this is deprecated
#' as of March 2016 and will be removed soon.)
#' 
#' @param filename optional source filename to be stored in the object
#' 
#' @param type optional type of CTD, e.g. "SBE"
#' 
#' @param model optional model of instrument
#' 
#' @param serialNumber optional serial number of instrument
#' 
#' @param ship optional string containing the ship from which the observations were made.
#' 
#' @param scientist optional string containing the chief scientist on the cruise.
#' 
#' @param institute optional string containing the institute behind the work.
#' 
#' @param address optional string containing the address of the institute.
#' 
#' @param cruise optional string containing a cruise identifier.
#' 
#' @param station optional string containing a station identifier.
#' 
#' @param date optional string indicating
#' the date at which the profile was started. This is copied verbatim into
#' the result's \code{metadata} slot, and is not used in any processing. Since
#' it serves no purpose, this argument is deprecated as of April 2016,
#' and will be marked 'defunct' in an upcoming CRAN release;
#' see \link{oce-deprecated}.
#' 
#' @param startTime optional indication of the start time for the profile, 
#' which is used in some several plotting functions.  This is best given as a 
#' \code{\link{POSIXt}} time, but it may also be a character string
#' that can be converted to a time with \code{\link{as.POSIXct}},
#' using \code{UTC} as the timezone.
#' 
#' @param recovery optional indication of the recovery time, in the format
#' described for \code{startTime}.  This is not presently used by \code{oce},
#' and is stored in the result's \code{metadata} slot just in case the user
#' requires it.
#' 
#' @param longitude optional numerical value containing longitude in decimal
#' degrees, positive in the eastern hemisphere. If this is a single number,
#' then it is stored in the \code{metadata} slot of the returned value; if it
#' is a vector of numbers, they are stored in \code{data} and a mean value is
#' stored in \code{metadata}.
#' 
#' @param latitude optional numerical value containing the latitude in decimal
#' degrees, positive in the northern hemisphere. See the note on length, for
#' the \code{longitude} argument.
#' 
#' @param deploymentType character string indicating the type of deployment. Use
#' \code{"unknown"} if this is not known, \code{"profile"} for a profile (in
#' which the data were acquired during a downcast, while the device was lowered
#' into the water column, perhaps also including an upcast; \code{"moored"} if
#' the device is installed on a fixed mooring, \code{"thermosalinograph"} (or
#' \code{"tsg"}) if the device is mounted on a moving vessel, to record
#' near-surface properties, or \code{"towyo"} if the device is repeatedly
#' lowered and raised.
#' 
#' @param pressureAtmospheric if \code{NA} (the default), then pressure is copied
#' from the \code{pressure} argument or from the contents of the first argument
#' (as described above for \code{salinity}). Otherwise, if
#' \code{pressureAtmospheric} is a numerical value (a constant or a vector),
#' then it is subtracted from pressure before storing it in the return value.
#' See \dQuote{Details} for special considerations if \code{salinity}
#' is a \code{\link{rsk-class}} object.
#' 
#' @param waterDepth optional numerical value indicating the water depth in
#' metres. This is different from the maximum recorded pressure, although
#' the latter is used by some oce functions as a guess on water depth, the
#' most important example being \code{\link{plot,section-method}}.
#' 
#' @param sampleInterval optional numerical value indicating the time between
#' samples in the profile.
#' 
#' @param src optional string indicating data source.
#' 
#' @template debugTemplate
#' 
#' @return An object of \code{\link{ctd-class}}.
#' 
#' @examples
#' library(oce)
#' pressure <- 1:50
#' temperature <- 10 - tanh((pressure - 20) / 5) + 0.02*rnorm(50)
#' salinity <- 34 + 0.5*tanh((pressure - 20) / 5) + 0.01*rnorm(50)
#' ctd <- as.ctd(salinity, temperature, pressure)
#' summary(ctd)
#' plot(ctd)
#' 
#' @references Culkin, F., and Norman D. Smith, 1980. Determination of the
#' concentration of potassium chloride solution having the same electrical
#' conductivity, at 15 C and infinite frequency, as standard seawater of salinity
#' 35.0000 ppt (Chlorinity 19.37394 ppt). \emph{IEEE Journal of Oceanic
#' Engineering}, \bold{5}, pp 22-23.
#' 
#' @author Dan Kelley
#'
#' @family things related to \code{ctd} data
as.ctd <- function(salinity, temperature=NULL, pressure=NULL, conductivity=NULL,
                   SA=NULL, CT=NULL, oxygen=NULL, nitrate=NULL, nitrite=NULL, phosphate=NULL, silicate=NULL,
                   scan=NULL, time=NULL, other=NULL,
                   units=NULL, flags=NULL,
                   pressureType="sea",
                   missingValue=NA, quality=NULL,
                   filename="", type="", model="", serialNumber="",
                   ship="", scientist="", institute="", address="", cruise="", station="",
                   date=NULL, startTime=NULL, recovery=NULL,
                   longitude=NA, latitude=NA,
                   deploymentType="unknown",
                   pressureAtmospheric=0, waterDepth=NA,
                   sampleInterval=NA,
                   src="",
                   debug=getOption("oceDebug"))
{
    oceDebug(debug, "as.ctd(...) {\n", sep="", unindent=1)
    res <- new('ctd')
    unitsGiven <- !is.null(units)
    if (!is.null(startTime) && is.character(startTime))
        startTime <- as.POSIXct(startTime, tz="UTC")
    if (!is.null(recovery) && is.character(recovery))
        recovery <- as.POSIXct(recovery, tz="UTC")
    if (missing(salinity)) {
        stop("must provide salinity")
        ##if (inherits(salinity, "ctd"))
        ##    return(salinity) # a convenience that lets us coerce without altering
        ## 1. coerce an oce object (with special tweaks for rsk)
    } else if (inherits(salinity, "oce")) {
        if (inherits(salinity, "ctd"))
            return(salinity)
        oceDebug(debug, "'salinity' is an oce object, so ignoring other arguments\n")
        o <- salinity
        d <- o@data
        m <- o@metadata
        dnames <- names(d)
        mnames <- names(m)
        ship <- m$ship
        cruise <- m$cruise
        station <- m$station
        scientist <- m$station
        if (is.character(m$startTime))
            startTime <- as.POSIXct(m$startTime, tz="UTC")
        if (is.na(latitude) && "latitude" %in% names(m))
            latitude <- m$latitude
        if (is.na(longitude) && "longitude" %in% names(m))
            longitude <- m$longitude
        if (missing(date) && "date" %in% names(m)) {
            date <- m$date
        }
        filename <- if ("filename" %in% mnames) m$filename else ""
        model <- m$model
        serialNumber <- m$serialNumber
        sampleInterval <- m$sampleInterval
        if (!is.null(m$waterDepth))
            waterDepth <- m$waterDepth
        ## Copy some WOCE things into oce-convention names (originals retained)
        if ("PSAL" %in% dnames && !("salinity" %in% dnames)) d$salinity <- d$PSAL
        if ("TEMP" %in% dnames && !("temperature" %in% dnames)) d$temperature <- d$TEMP
        if ("PRES" %in% dnames && !("pressure" %in% dnames)) d$pressure <- d$PRES
        temperature <- d$temperature
        pressure <- d$pressure
        ## "rsk" stores total pressure, not sea pressure as "ctd" stores.
        if (inherits(o, "rsk")) {
            oceDebug(debug, "first argument is an rsk object\n")
            pressureAtmosphericStandard <- 10.1325
            ##pressureMin <- min(pressure, na.rm=TRUE)
            ## FIXME: could examine min(pressure) to see if it's between 9 and 11.
            if (is.null(o@metadata$pressureType)) {
                oceDebug(debug, "metadata$pressureType is NULL\n")
                warning("rsk object lacks metadata$pressureType; assuming absolute and subtracting standard atm pressure to get sea pressure")
                pressure <- pressure - pressureAtmosphericStandard
            } else {
                ## subtract atm pressure, if it has not already been subtracted
                oceDebug(debug, "metadata$pressureType is not NULL\n")
                if ("sea" != substr(o@metadata$pressureType, 1, 3)) {
                    oceDebug(debug, "must convert from absolute pressure to sea pressure\n")
                    if (!("pressureAtmospheric" %in% mnames)) {
                        oceDebug(debug, "pressure is 'absolute'; subtracting std atm 10.1325 dbar\n")
                        pressure <- pressure - 10.1325
                    } else {
                        pressure <- pressure - m$pressureAtmospheric
                        oceDebug(debug, "pressure is 'absolute'; subtracting metadata 10.1325dbar\n")
                    }
                } else {
                    oceDebug(debug, "this rsk object contains sea pressure, so no need to remove atmospheric pressure\n")
                }
            }
        }
        if (!missing(pressureAtmospheric)) {
            len <- length(pressureAtmospheric)
            if (1 != len && len != length(pressure))
                stop("length(pressureAtmospheric) must be 1 or length(pressure)")
            pressure <- pressure - pressureAtmospheric
        }
        ## "rsk" stores conductivity (in mS/cm, not as ratio), and does not store salinity
        if ("COND" %in% names(d))
            conductivity <- d$COND
        else
            conductivity <- d$conductivity
        if (inherits(o, "rsk")) {
            if (is.null(conductivity))
                stop("as.ctd() cannot coerce an rsk object that lacks conductivity")
            salinity <- swSCTp(conductivity=conductivity/42.914, temperature=temperature, pressure=pressure)
            if (is.null(units)) # this lets the user over-ride
                units <- list(temperature=list(unit=expression(degree*C), scale="ITS-90"),
                              salinity=list(unit=expression(), scale="PSS-78"),
                              conductivity=list(unit=expression(mS/cm), scale=""),
                              pressure=list(unit=expression(dbar), scale=""))
        } else {
            salinity <- d$salinity # FIXME: ok for objects (e.g. rsk) that lack salinity?
        }
        if (inherits(o, "ctd") && missing(units)) {
            if (missing(units)) # this lets the user over-ride
                units <- o@metadata$units
        }
        res@metadata$units <- units
        if (!is.null(flags))
            res@metadata$flags <- flags
        res@metadata$pressureType <- pressureType
        res@metadata$startTime <- startTime
        res@data$pressure <- pressure
        res@data$salinity <- salinity
        res@data$temperature <- temperature
        res@data$conductivity <- conductivity
        ## res <- ctdAddColumn(res, swSigmaTheta(salinity, temperature, pressure),
        ##                    name="sigmaTheta", label="Sigma Theta", unit=list(unit=expression(kg/m^3), scale=""))
        ## copy relevant metadata
        if ("date" %in% mnames) res@metadata$date <- o@metadata$date
        if ("deploymentType" %in% mnames) res@metadata$deploymentType <- o@metadata$deploymentType
        if ("filename" %in% mnames) res@metadata$filename <- o@metadata$filename
        if ("serialNumber" %in% mnames) res@metadata$serialNumber <- o@metadata$serialNumber
        if ("ship" %in% mnames) res@metadata$ship <- o@metadata$ship
        if ("cruise" %in% mnames) res@metadata$cruise <- o@metadata$cruise
        if ("station" %in% mnames) res@metadata$station <- o@metadata$station
        if ("scientist" %in% mnames) res@metadata$scientist <- o@metadata$scientist
        if ("units" %in% mnames) {
            ## the usual case
            res@metadata$units$conductivity <- o@metadata$units$conductivity
            res@metadata$units$temperature <- o@metadata$units$temperature
        } else {
            ## permit a case that existed for a few months in 2015
            if ("conductivityUnit" %in% mnames)
                res@metadata$units$conductivity <- o@metadata$conductivityUnit
            if ("temperatureUnit" %in% mnames)
                res@metadata$units$temperature <- o@metadata$temperatureUnit
        }
        if ("pressureType" %in% mnames) res@metadata$pressureType <- pressureType
        if ("scan" %in% dnames) res@data$scan <- d$scan
        if ("time" %in% dnames) res@data$time <- d$time
        if ("quality" %in% dnames) res@data$quality <- d$quality
        if ("oxygen" %in% dnames) res@data$oxygen <- d$oxygen
        if ("nitrate" %in% dnames) res@data$nitrate <- d$nitrate
        if ("nitrite" %in% dnames) res@data$nitrite <- d$nitrite
        if ("phosphate" %in% dnames) res@data$phosphate <- d$phosphate
        if ("silicate" %in% dnames) res@data$silicate <- d$silicate
        ## FIXME: need to add all columns from @data in the rsk object
        nrow <- length(res@data$temperature)
        for (field in names(d)) {
            if (!(field %in% c('pressure', 'salinity', 'temperature', 'conductivity'))) {
                if (nrow == length(d[[field]]))
                    res <- ctdAddColumn(res, d[[field]], field)
            }
        }
        ## FIXME: next in dnames or mnames??
        if ("longitude" %in% dnames && "latitude" %in% dnames) {
            longitude <- d$longitude
            latitude <- d$latitude
            if (length(longitude) != length(latitude))
                stop("lengths of longitude and latitude must match")
            if (length(longitude) == length(temperature)) {
                res@data$longitude <- longitude
                res@data$latitude <- latitude
            }
        } else if ("longitude" %in% mnames && "latitude" %in% mnames) {
            res@metadata$longitude <- m$longitude
            res@metadata$latitude <- m$latitude
        }
        res@metadata$deploymentType <- deploymentType
        res@processingLog <- processingLogAppend(res@processingLog, paste(deparse(match.call()), sep="", collapse=""))
        oceDebug(debug, "} # as.ctd()\n", sep="", unindent=1)
    } else if (is.list(salinity) || is.data.frame(salinity)) {
        oceDebug(debug, "salinity is a list or data frame\n")
        ## 2. coerce a data-frame or list
        x <- salinity
        names <- names(x)
        ## Permit oce-style names or WOCE-style names for the three key variables (FIXME: handle more)
        if (3 == sum(c("salinity", "temperature", "pressure") %in% names)) {
            res@data$pressure <- x$pressure
            res@data$salinity <- x$salinity
            res@data$temperature <- x$temperature
            res@metadata$units <- units
            res@metadata$pressureType <- pressureType
        } else if (3 == sum(c("PSAL", "TEMP", "PRES") %in% names)) {
            res@data$pressure <- x$PRES
            res@data$salinity <- x$PSAL
            res@data$temperature <- x$TEMP
            res@metadatdata$units <- units
            res@metadatdata$pressureType <- pressureType
        } else {
            stop("the first argument must contain salinity, temperature, and pressure")
        }
        if ("longitude" %in% names)
            res@metadata$longitude <- if (1 == length(longitude)) longitude else x$longitude
        if ("latitude" %in% names)
            res@metadata$latitude <- if (1 == length(latitude)) latitude else x$latitude
        if ("conductivity" %in% names) res@data$conductivity <- x$conductivity
        if ("COND" %in% names) res@data$conductivity <- x$COND # FIXME accept other WOCE names
        if ("quality" %in% names)res@data$quality <- x$quality
        if ("oxygen" %in% names)res@data$oxygen <- x$oxygen
        if ("nitrate" %in% names)res@data$nitrate <- x$nitrate
        if ("nitrite" %in% names)res@data$nitrite <- x$nitrite
        if ("phosphate" %in% names)res@data$phosphate <- x$phosphate
        if ("silicate" %in% names)res@data$silicate <- x$silicate
        if ("time" %in% names)res@data$time <- x$time
        oceDebug(debug, "} # as.ctd()\n", sep="", unindent=1)
    } else {
        oceDebug(debug, "salinity, temperature, pressure (etc) supplied\n")
        ## 3. explicit mode
        if (missing(temperature) && missing(CT)) stop("must give temperature or CT")
        if (missing(pressure)) stop("must give pressure")
        if (!missing(units))
            res@metadata$units <- units
        res@metadata$pressureType <- pressureType
        salinity <- as.vector(salinity)
        temperature <- as.vector(temperature)
        pressure <- as.vector(pressure)
        if (!missing(pressureAtmospheric))
            pressure <- pressure - pressureAtmospheric
        haveSA <- !missing(SA)
        haveCT <- !missing(CT)
        if (haveSA != haveCT)
            stop("SA and CT must both be supplied, if either is")
        if (!missing(SA)) {
            n <- length(SA)
            if (length(CT) != n)
                stop("lengths of SA and CT must match")
            if (missing(longitude)) {
                longitude <- rep(300, n)
                latitude <- rep(0, n)
                warning("longitude and latitude set to default values, since none given")
            }
            salinity <- gsw::gsw_SP_from_SA(SA, pressure, longitude, latitude)
            temperature <- gsw::gsw_t_from_CT(SA, CT, pressure)
        }
        ##depths <- max(length(salinity), length(temperature), length(pressure))
        ## 2015-01-24: now insist that lengths make sense; only pressure can be mismatched
        salinity <- as.vector(salinity)
        temperature <- as.vector(temperature)
        pressure <- as.vector(pressure)
        nS <- length(salinity)
        nT <- length(temperature)
        np <- length(pressure)
        if (nS != nT)
            stop("lengths of salinity and temperature must match, but they are ", nS, " and ", nT)
        if (np == 1)
            pressure <- rep(pressure, nS)
        np <- length(pressure)
        if (nS != np)
            stop("lengths of salinity and pressure must match, but they are ", nS, " and ", np)
        if (missing(scan))
            scan <- seq_along(salinity)
        data <- list(scan=scan,
                     salinity=salinity,
                     temperature=temperature,
                     pressure=pressure,
                     sigmaTheta=swSigmaTheta(salinity, temperature, pressure)) # FIXME: what about gsw?
        res@metadata$units$sigmaTheta <- list(unit=expression(kg/m^3), scale="")
        if (!missing(conductivity)) data$conductivity <- as.vector(conductivity)
        if (!missing(quality)) data$quality <- quality
        if (!missing(oxygen)) data$oxygen <- oxygen
        if (!missing(nitrate)) data$nitrate <- nitrate
        if (!missing(nitrite)) data$nitrite <- nitrite
        if (!missing(phosphate)) data$phosphate <- phosphate
        if (!missing(silicate)) data$silicate <- silicate
        if (!missing(time)) data$time <- time
        if (!missing(other)) {
            names <- names(other)
            for (i in seq_along(names)) {
                if (names[i] != "") {
                    data[[names[i]]] <- other[[names[i]]]
                } else {
                    warning("'other' item number ", i, " has no name")
                }
            }
        }
        if (!missing(missingValue)) {
            data[data==missingValue] <- NA
        }
        ##20150712 if (is.na(waterDepth)) {
        ##20150712     waterDepth <- max(abs(data$pressure), na.rm=TRUE)
        ##20150712     res@processingLog <- processingLogAppend(res@processingLog,
        ##20150712                                              "inferred water depth from maximum pressure")
        ##20150712 }
        names <- names(data)
        labels <- titleCase(names) # paste(toupper(substring(names,1,1)),substring(names,2),sep="")
        if (length(longitude) != length(latitude))
            stop("lengths of longitude and latitude must match")
        if (1 < length(longitude) && length(longitude) != length(salinity))
            stop("lengths of salinity and longitude must match")
        ## FIXME: should sampleInterval be a default?
        res@metadata$names <- names
        res@metadata$labels <- labels
        res@metadata$filename <- filename
        res@metadata$ship <- ship
        res@metadata$scientist <- scientist
        res@metadata$institute <- institute
        res@metadata$address <- address
        res@metadata$cruise <- cruise
        res@metadata$station <- station
        res@metadata$date <- date
        res@metadata$startTime <- startTime
        res@metadata$recovery <- recovery
        res@metadata$type <- type
        res@metadata$model <- model
        res@metadata$serialNumber <- serialNumber
        res@metadata$src <- src
        ## If lon and lat are vectors, place in data, with averages in metadata.
        if (length(latitude) == 1) {
            res@metadata$longitude <- longitude[1]
            res@metadata$latitude <- latitude
        } else {
            if (length(latitude) != length(temperature))
                stop("lengths of latitude and temperature must match")
            data$longitude <- longitude
            data$latitude <- latitude
        }
        res@data <- data
    }
    if (!unitsGiven) { # guess on units
        names <- names(res@data)
        if ("salinity" %in% names) res@metadata$units$salinity <- list(unit=expression(), scale="PSS-78")
    }
    ## the 'units' argument takes precedence over guesses
    dataNames <- names(res@data)
    unitsNames <- names(units)
    if (!is.null(flags))
        res@metadata$flags <- flags

    if ("temperature" %in% dataNames && !("temperature" %in% unitsNames))
        res@metadata$units$temperature <- list(unit=expression(degree*C), scale="ITS-90")
    if ("salinity" %in% dataNames && !("salinity" %in% unitsNames))
        res@metadata$units$salinity <- list(unit=expression(), scale="PSS-78")
    if ("pressure" %in% dataNames && !("pressure" %in% unitsNames))
        res@metadata$units$pressure <- list(unit=expression(dbar), scale="")
    if (is.na(res@metadata$waterDepth) && !is.na(waterDepth))
        res@metadata$waterDepth <- waterDepth
    oceDebug(debug, "} # as.ctd()\n", sep="", unindent=1)
    res
}


#' Add a column to the data slot of a ctd object
#'
#' Add a column to the \code{data} slot of an object of
#' \code{\link{ctd-class}}, also updating the \code{metadata}
#' slot as appropriate.
#'
#' @param x A \code{ctd} object, e.g. as read by \code{\link{read.ctd}}.
#' @param column A column of data to be inserted, in the form of a
#'     numeric vector, whose length matches that of columns in the
#'     objecct.
#' @param name Character string indicating the name this column is to
#'     have in the \code{data} slot of \code{x}.
#' @param label Optional character string or expression indicating the
#'     name of the column, as it will appear in plot labels.  (If not
#'     given, \code{name} will be used.)
#' @param unit Optional indication of the unit, in the form of a list containing
#'     items \code{unit}, which is an expression, and \code{scale}, which is a
#'     character string. For example, modern measurements of temperature have
#'     unit \code{list(name=expression(degree*C), scale="ITS-90")}.
#' @template debugTemplate
#' @return A ctd object.
#' @seealso The documentation for \code{\link{ctd-class}} explains the structure
#'    of CTD objects, and also outlines the other functions dealing with them.
#'
#' @examples
#' library(oce)
#' data(ctd)
#' F <- 32 + (9/5)*ctd[["temperature"]]
#' ctdNew <- ctdAddColumn(ctd, F, "temperatureF",
#'     unit=list(unit=expression(degree*F), scale="ITS-90"))
#'
#' @author Dan Kelley
#'
#' @family things related to \code{ctd} data
ctdAddColumn <- function (x, column, name, label, unit=NULL, debug = getOption("oceDebug"))
{
    ## FIXME: not using the units
    oceDebug(debug, "ctdAddColumn(x, column, name=\"", name, "\", label=\"", label, "\", debug) {\n", sep="", unindent=1)
    if (missing(column))
        stop("must supply column data")
    if (length(column) != length(x@data[[1]]))
        stop("column has ", length(column), " data but it must have ", length(x@data[[1]]), " data to match existing object")
    if (missing(name))
        stop("must supply \"name\"")
    if (missing(label))
        label <- name
    replace <- name %in% names(x@data)
    res <- x
    ##r <- range(column)
    res@data[[name]] <- column
    if (!replace) {
        res@metadata$names <- c(res@metadata$names, name)
        res@metadata$labels <- c(res@metadata$labels, label)
    }
    if (!is.null(unit)) {
        if (0 == length(unit)) {
            ##> message("NULL unit; name:", name)
            ##> message("unit:")
            ##> print(unit)
            res@metadata$units[[name]] <- list(unit=expression(), scale="")
        } else if (1 == length(unit)) {
            res@metadata$units[[name]] <- if (is.expression(unit)) list(unit=unit, scale="") else
                list(unit=as.expression(unit), scale="")
        } else if (2 == length(unit)) {
            if (is.list(unit)) {
                res@metadata$units[[name]] <- unit
            } else {
                stop("unit should be a list containing two items")
            }
        } else {
            warning("ignoring unit since it not of length 1 or 2")
        }
    }
    res@processingLog <- processingLogAppend(res@processingLog, paste("ctdAddColumn(..., name=\"", name, "\", ...)", sep=""))
    oceDebug(debug, "} # ctdAddColumn()\n", sep="", unindent=1)
    res
}


#' Decimate a CTD profile.
#' 
#' Interpolate a CTD profile to specified pressure values.
#' 
#' The \code{"approx"} method is best for bottle data, in which the usual task is
#' to interpolate from a coarse sampling grid to a finer one. For CTD data, the
#' \code{"boxcar"} method is the more common choice, because the task is normally
#' to sub-sample, and some degree of smoothing is usually desired.  (The
#' \code{"lm"} method is quite slow, and the results are similar to those of the
#' boxcar method.)
#' 
#' Note that a sort of numerical cabeling effect can result from this procedure,
#' but it can be avoided as follows
#' 
#' \preformatted{
#' xd <- ctdDecimate(x)
#' xd[["sigmaTheta"]] <- swSigmaTheta(xd[["salinity"]],xd[["temperature"]],xd[["pressure"]])
#' }
#'
#' @template flagDeletionTemplate
#' 
#' @param x a \code{ctd} object, e.g. as read by \code{\link{read.ctd}}.
#' 
#' @param p pressure increment, or vector of pressures.  In the first case,
#' pressures from 0dbar to the rounded maximum pressure are used, incrementing by
#' \code{p} dbars.  If a vector of pressures is given, interpolation is done to
#' these pressures.
#'   
#' @param method the method to be used for calculating decimated values.  This may
#' be a function or a string naming a built-in method.  The built-in methods are
#' \code{"boxcar"} (based on a local average), \code{"approx"} (based on linear
#' interpolation between neighboring points), \code{"lm"} (based on local
#' regression, with \code{e} setting the size of the local region), \code{"rr"}
#' (for the Reineger and Ross method, carried out with \code{\link{oce.approx}})
#' and \code{"unesco"} (for the UNESCO method, carried out with.
#' \code{\link{oce.approx}}.  If \code{method} is a function, then it must take
#' three arguments, the first being pressure, the second being an arbitrary
#' variable in another column of the data, and the third being a vector of target
#' pressures at which the calculation is carried out, and the return value must be
#' a vector.  See \dQuote{Examples}.
#' 
#' @param e is an expansion coefficient used to calculate the local neighbourhoods
#' for the \code{"boxcar"} and \code{"lm"} methods.  If \code{e=1}, then the
#' neighbourhood for the i-th pressure extends from the (\code{i-1})-th pressure to
#' the (\code{i+1})-th pressure.  At the endpoints it is assumed that the outside
#' bin is of the same pressure range as the first inside bin.  For other values of
#' \code{e}, the neighbourhood is expanded linearly in each direction.  If the
#' \code{"lm"} method produces warnings about "prediction from a rank-deficient
#' fit", a larger value of \code{"e"} should be used.
#' 
#' @template debugTemplate
#'
#' @return An object of \code{\link{ctd-class}}, with pressures that are as set by
#' the \code{"p"} parameter and all other properties modified appropriately.
#' 
#' @seealso The documentation for \code{\link{ctd-class}} explains the structure of
#' CTD objects, and also outlines the other functions dealing with them.
#' 
#' @examples
#' library(oce)
#' data(ctd)
#' plotProfile(ctd, "salinity", ylim=c(10, 0))
#' p <- seq(0, 45, 1)
#' ctd2 <- ctdDecimate(ctd, p=p)
#' lines(ctd2[["salinity"]], ctd2[["pressure"]], col="blue")
#' p <- seq(0, 45, 1)
#' ctd3 <- ctdDecimate(ctd, p=p, method=function(x,y,xout)
#'                     predict(smooth.spline(x, y, df=30), p)$y)
#' lines(ctd3[["salinity"]], ctd3[["pressure"]], col="red")
#' 
#' 
#' @references
#' R.F. Reiniger and C.K. Ross, 1968.  A method of interpolation with
#' application to oceanographic data.  \emph{Deep Sea Research}, \bold{15},
#' 185-193.
#' 
#' @author Dan Kelley
#'
#' @family things related to \code{ctd} data
ctdDecimate <- function(x, p=1, method="boxcar", e=1.5, debug=getOption("oceDebug"))
{
    methodFunction <- is.function(method)
    if (!methodFunction) {
        methods <- c("boxcar", "approx", "lm", "rr", "unesco")
        imethod <- pmatch(method, methods, nomatch=0)
        if (imethod > 0) method <- methods[imethod] else
            stop('unknown method "', method, '"')
    }
    oceDebug(debug, "ctdDecimate(x, p, method=\"",
             if (methodFunction) "(a function)" else method,
             "\", ...) {\n", sep="", unindent=1)
    ## if (!inherits(x, "ctd"))
    ##     stop("method is only for objects of class '", "ctd", "'")
    res <- x
    res[["flags"]] <- NULL
    warning("Data flags are omitted from the decimated ctd object. Use handleFlags() first to remove bad data.")
    n <- length(x@data$pressure)
    if (n < 2) {
        warning("too few data to ctdDecimate()")
        return(res)
    }
    ## Figure out pressure targets, pt
    if (length(p) == 1) {
        pt <- seq(0, p * floor(max(x@data$pressure, na.rm=TRUE) / p), p)
    } else {
        pt <- p
    }
    npt <- length(pt)
    dataNames <- names(x@data)         # Step through each variable.
    dataNew <- vector("list", length(dataNames)) # as.data.frame(array(NA, dim=c(npt, length(dataNames))))
    names(dataNew) <- dataNames
    oceDebug(debug, "methodFunction=", methodFunction, "\n")
    if (methodFunction) {
        ##message("function must have take three args: x, y and xout; x will be pressure.")
        pressure <- x[["pressure"]]
        tooDeep <- pt > max(pressure, na.rm=TRUE)
        for (datumName in names(x@data)) {
            if (!length(x[[datumName]])) {
                dataNew[[datumName]] <- NULL
            } else {
                if ("pressure" == datumName)
                    next
                oceDebug(debug, 'about to apply method() to "', datumName, '"\n', sep='')
                if (all(is.na(x@data[[datumName]]))) {
                    dataNew[[datumName]] <- rep(NA, npt)
                } else {
                    dataNew[[datumName]] <- method(pressure, x[[datumName]], pt)
                }
            }
        }
        dataNew[["pressure"]] <- pt
    } else {
        if (method == "approx") {
            numGoodPressures <- sum(!is.na(x@data$pressure))
            if (numGoodPressures > 0)
                tooDeep <- pt > max(x@data[["pressure"]], na.rm=TRUE)
            for (datumName in dataNames) {
                oceDebug(debug, "decimating \"", datumName, "\"\n", sep="")
                if (numGoodPressures < 2 || !length(x[[datumName]])) {
                    dataNew[[datumName]] <- rep(NA, npt)
                } else {
                    if (datumName != "pressure") {
                        good <- sum(!is.na(x@data[[datumName]]))
                        if (good > 2) {
                            dataNew[[datumName]] <- approx(x@data[["pressure"]], x@data[[datumName]], pt, rule=2)$y
                            dataNew[[datumName]][tooDeep] <- NA
                        } else {
                            dataNew[[datumName]] <- rep(NA, npt)
                        }
                    }
                }
            }
        } else if ("rr" == method || "unesco" == method) {
            oceDebug(debug, "Reiniger-Ross method\n")
            xvar <- x@data[["pressure"]]
            for (datumName in dataNames) {
                oceDebug(debug, "decimating \"", datumName, "\"\n", sep="")
                if (!length(x[[datumName]])) {
                    dataNew[[datumName]] <- NULL
                } else {
                    if (datumName != "pressure") {
                        yvar <- x@data[[datumName]]
                        if (all(is.na(yvar))) {
                            dataNew[[datumName]] <- rep(NA, npt)
                        } else {
                            pred <- oce.approx(xvar, yvar, pt, method=method)
                            dataNew[[datumName]] <- pred
                        }
                    }
                }
            }
        } else if ("boxcar" == method) {
            dp <- diff(pt[1:2])
            pbreaks <- -dp / 2 + c(pt, tail(pt, 1) + dp)
            p <- x@data[["pressure"]]
            for (datumName in dataNames) {
                oceDebug(debug, "decimating", datumName)
                if (!length(x[[datumName]])) {
                    dataNew[[datumName]] <- NULL
                } else {
                    if (datumName != "pressure" && datumName != "scan" && datumName != "flag") {
                        if (all(is.na(x@data[[datumName]]))) {
                            dataNew[[datumName]] <- rep(NA, pt)
                        } else {
                            dataNew[[datumName]] <- binMean1D(p, x@data[[datumName]], xbreaks=pbreaks)$result
                        }
                    }
                }
            }
        } else {
            for (i in 1:npt) {
                if (i==1) {
                    focus <- (x@data$pressure >= (pt[i] - e*(pt[i+1] - pt[ i ]))) & (x@data$pressure <= (pt[i] + e*(pt[i+1] - pt[ i ])))
                } else if (i == npt) {
                    focus <- (x@data$pressure >= (pt[i] - e*(pt[ i ] - pt[i-1]))) & (x@data$pressure <= (pt[i] + e*(pt[ i ] - pt[i-1])))
                } else {
                    focus <- (x@data$pressure >= (pt[i] - e*(pt[ i ] - pt[i-1]))) & (x@data$pressure <= (pt[i] + e*(pt[i+1] - pt[ i ])))
                }
                if (sum(focus, na.rm=TRUE) > 0) {
                    if ("boxcar" == method) {
                        for (datumName in dataNames) {
                            if (!length(x[[datumName]])) {
                                dataNew[[datumName]] <- NULL
                            } else {
                                if (datumName != "pressure") {
                                    dataNew[[datumName]][i] <- mean(x@data[[datumName]][focus],na.rm=TRUE)
                                }
                            }
                        }
                    } else if ("lm" == method) { # FIXME: this is far too slow
                        xvar <- x@data[["pressure"]][focus]
                        for (datumName in dataNames) {
                            if (!length(x[[datumName]])) {
                                dataNew[[datumName]] <- NULL
                            } else {
                                if (datumName != "pressure") {
                                    yvar <- x@data[[datumName]][focus]
                                    t <- try(m <- lm(yvar ~ xvar), silent=TRUE)
                                    if (class(t) != "try-error")
                                        dataNew[[datumName]][i] <- predict(m, newdata=list(xvar=pt[i]))
                                    else
                                        dataNew[[datumName]][i] <- NA
                                }
                            }
                        }
                    } else {
                        stop("impossible to get here -- developer error")
                    }
                } else {                    # No data in the focus region
                    for (datumName in dataNames) {
                        if (!length(x[[datumName]])) {
                            dataNew[[datumName]] <- NULL
                        } else {
                            if (datumName != "pressure") {
                                dataNew[[datumName]][i] <- NA
                            }
                        }
                    }
                }
            }
        }
    }
    dataNew[["pressure"]] <- pt
    ## convert any NaN to NA
    for (i in 1:length(dataNew)) {
        dataNew[[i]][is.nan(dataNew[[i]])] <- NA
    }
    ##message("ctd.R:733 dataNew[['pressure']]: ", paste(dataNew[['pressure']], collapse=" "))
    res@data <- dataNew
    res@processingLog <- processingLogAppend(res@processingLog, paste(deparse(match.call()), sep="", collapse=""))
    oceDebug(debug, "} # ctdDecimate()\n", unindent=1)
    res
}

#' Find profiles within a towyow CTD record
#' 
#' Examine the pressure record looking for extended periods of either ascent or descent, and return
#' either indices to these events or a vector of CTD records containing the events.
#' 
#' The method works by examining the pressure record.  First, this is smoothed using
#' \code{\link{smooth.spline}}, which is provided with any extra arguments as supplied to the present
#' function, e.g. \code{ctdFindProfiles(..., df=10)} uses a spline with 10 degrees of freedom.  The
#' spline is then first differenced with \code{\link{diff}}.  Median values of the positive and
#' negative first-difference values are then multiplied by \code{cutoff}.  This establishes criteria
#' for any given point to be in an ascending profile, a descending profile, or a non-profile.
#' Contiguous regions are then found, and those that have fewer than \code{minLength} points are
#' discarded.  Then, those that have pressure ranges less than \code{minHeight} are discarded.
#' 
#' It is often necessary to pass the resultant profiles through \code{\link{ctdTrim}}, to remove
#' artifacts such as an equilibration phase, etc.
#' 
#' @param x A \code{ctd} object, as read by \code{\link{read.ctd}} or created by \code{\link{as.ctd}}.
#' 
#' @param cutoff criterion on pressure difference; see \dQuote{Details}.
#' 
#' @param minLength lower limit on number of points in candidate profiles.
#' 
#' @param minHeight lower limit on height of candidate profiles.
#' 
#' @param direction String indicating the travel direction to be selected.
#' 
#' @param arr.ind Should array indices be returned, or a vector of ctd objects?
#' 
#' @template debugTemplate
#' 
#' @param ... Optional extra arguments that are passed to \code{\link{smooth.spline}}.
#' 
#' @return If \code{arr.ind=TRUE}, a data frame with columns \code{start} and \code{end}, the indices
#' of the downcasts.  Otherwise, a vector of \code{ctd} objects.
#' 
#' @seealso The documentation for \code{\link{ctd-class}} explains the structure
#' of CTD objects, and also outlines the other functions dealing with them.
#' 
#' @examples
#' \dontrun{
#' library(oce)
#' d <- read.csv("towyow.csv", header=TRUE)
#' towyow <- as.ctd(d$salinity, d$temperature, d$pressure)
#' 
#' casts <- ctdFindProfiles(towyow)
#' par(mfrow=c(length(casts), 3))
#' for (cast in casts) {
#'   plotProfile(cast, "salinity")
#'   plotProfile(cast, "temperature")
#'   plotTS(cast, type='o')
#' }
#' }
#' 
#' @author Dan Kelley
#' 
#' @family things related to \code{ctd} data
ctdFindProfiles <- function(x, cutoff=0.5, minLength=10, minHeight=0.1*diff(range(x[["pressure"]])),
                            direction=c("descending", "ascending"),
                            arr.ind=FALSE,
                            debug=getOption("oceDebug"), ...)
{
    oceDebug(debug, "ctdFindProfiles(x, cutoff=", cutoff,
             ", minLength=", minLength,
             ", minHeight=", minHeight,
             ", direction=\"", direction, "\"",
             ", arr.ind=", arr.ind, ", debug=", debug, ") {\n", sep="", unindent=1)
    if (!inherits(x, "ctd"))
        stop("method is only for objects of class '", "ctd", "'")
    direction <- match.arg(direction)
    pressure <- x[["pressure"]]
    dp <- diff(pressure)
    dp <- c(dp[1], dp)
    if (direction == "descending") {
        ps <- smooth.spline(pressure, ...)
        dp <- diff(ps$y)
        dp <- c(dp[1], dp)
        look <- dp > cutoff * median(dp[dp>0])
        start <- which(diff(look) == 1)
        if (0 == length(start))
            start <- 1
        end <- which(diff(look) == -1)
        if (0 == length(end))
            end <- length(pressure)
        if (start[1] > end[1])
            start <- start[-1]
    } else if (direction == "ascending") {
        ps <- smooth.spline(pressure, ...)
        dp <- diff(ps$y)
        dp <- c(dp[1], dp)
        look <- dp < cutoff * median(dp[dp<0])
        start <- which(diff(look) == 1)
        if (0 == length(start))
            start <- 1
        if (0 == length(end))
            end <- length(pressure)
        end <- which(diff(look) == -1)
        if (0 == length(end))
            end <- length(pressure)
        if (start[1] > end[1])
            start <- start[-1]
    } else {
        stop("direction must be either \"ascending\" or \"descending\"") # cannot reach here
    }
    oceDebug(debug, "start:", start, "(before trimming)\n")
    oceDebug(debug, "end:", end, "(before trimming)\n")
    start <- subset(start, start<max(end))
    end <- subset(end, end>min(start))
    oceDebug(debug, "start:", start, "(after trimming)\n")
    oceDebug(debug, "end:", end, "(after trimming)\n")
    if (length(end) > length(start))
        end <- end[1:length(start)]
    keep <- abs(end - start) >= minLength
    oceDebug(debug, "start:", start[keep], "(using minLength)\n")
    oceDebug(debug, "end:", end[keep], "(using minLength)\n")
    keep <- keep & (abs(ps$y[end] - ps$y[start]) >= minHeight)
    oceDebug(debug, "heights:", ps$y[end]-ps$y[start], "; compare with minHeight=", minHeight, "\n")
    oceDebug(debug, "start:", start[keep], "(using minHeight)\n")
    oceDebug(debug, "end:", end[keep], "(using minHeight)\n")
    indices <- data.frame(start=start[keep], end=end[keep])
    if (debug) print(indices)
    if (is.logical(arr.ind) && arr.ind) {
        oceDebug(debug, "} # ctdFindProfiles()\n", sep="", unindent=1)
        return(indices)
    } else {
        ncasts <- length(indices$start)
        casts <- vector("list", ncasts)
        for (i in 1:ncasts) {
            oceDebug(debug, "profile #", i, "of", ncasts, "\n")
            cast <- ctdTrim(x, "index", parameters=c(indices$start[i], indices$end[i]))
            cast@processingLog <- processingLogAppend(cast@processingLog,
                                                      paste(paste(deparse(match.call()), sep="", collapse=""),
                                                            " # profile ", i, " of ", ncasts))
            casts[[i]] <- cast
        }
        oceDebug(debug, "} # ctdFindProfiles()\n", sep="", unindent=1)
        return(casts)
    }
}

#' Read an ODF -type CTD file, i.e. a file with name ending in \code{.odf}.
#' @template readCtdTemplate
#'
#' @details
#' \code{read.ctd.odf} reads files stored in Ocean Data Format, used in
#' some Canadian hydrographic databases.
#'
#' @references
#' The ODF format, used by the Canadian Department of Fisheries and Oceans, is
#' described to some extent in the documentation for \code{\link{read.odf}}.  It
#' is not clear that ODF format is handled correctly in \code{read.ctd.odf}, or
#' the more general function \code{\link{read.odf}}, because the format seems to
#' be somewhat variable and so the R code was written in an ad-hoc way, to handle
#' a few files being used in the author's research.
read.ctd.odf <- function(file, columns=NULL, station=NULL, missing.value=-999, monitor=FALSE,
                         debug=getOption("oceDebug"), processingLog, ...)
{
    oceDebug(debug, "read.ctd.odf() {")
    if (!is.null(columns)) warning("'columns' is ignored by read.ctd.odf() at present")
    odf <- read.odf(file)
    res <- as.ctd(odf)
    if (!is.null(station))
        res@metadata$station <- station
    res@metadata$units <- list(temperature=list(unit=expression(degree*C), scale="ITS-90"),
                               conductivity=list(unit=expression(ratio), scale="")) # FIXME just a guess for ODV
    oceDebug(debug, "} # read.ctd.odf()")
    res
}


#' Trim start/end portions of a CTD cast
#' 
#' Often in CTD profiling, the goal is to isolate only the downcast, discarding measurements made in
#' the air, in an equilibration phase in which the device is held below the water surface, and then the
#' upcast phase that follows the downcast.  This is handled reasonably well by \code{ctdTrim} with
#' \code{method="downcast"}, although it is almost always best to use \code{\link{plotScan}} to
#' investigate the data, and then use the \code{method="index"} or \code{method="scan"} method based on
#' visual inspection of the data.
#'
#' \code{ctdTrim} begins by examining the pressure differences between subsequent samples. If
#' these are all of the same value, then the input \code{ctd} object is returned, unaltered.
#' This handles the case of pressure-binned data. However, if the pressure difference
#' varies, a variety of approaches are taken to trimming the dataset.
#' 
#' \itemize{
#'   \item{If \code{method[1]} is \code{"downcast"} then an attempt is made to only data for
#'   which the CTD is descending.  This is done in stages, with variants based on \code{method[1]}, if
#'   supplied.  \emph{Step 1.} The pressure data are despiked with a smooth() filter with method "3R".
#'   This removes wild spikes that arise from poor instrument connections, etc.  \emph{Step 2.} If no
#'   \code{parameters} are given, then any data with negative pressures are deleted.  If there is a
#'   parameter named \code{pmin}, then that pressure (in decibars) is used instead as the lower limit.
#'   This is a commonly-used setup, e.g.  \code{ctdTrim(ctd, parameters=list(pmin=1))} removes the top
#'   decibar (roughly 1m) from the data.  Specifying \code{pmin} is a simple way to remove near-surface
#'   data, such as a shallow equilibration phase, and if specified will cause \code{ctdTrim} to skip
#'   step 4 below.  \emph{Step 3.} The maximum pressure is determined, and data acquired subsequent to
#'   that point are deleted.  This removes the upcast and any subsequent data.  \emph{Step 4.} If the
#'   \code{pmin} parameter is not specified, an attempt is made to remove an initial equilibrium phase
#'   by a regression of pressure on scan number.  There are three variants to this, depending on the
#'   value of the second \code{method} element. If it is \code{"A"} (or not given), the procedure is to
#'   call \code{\link{nls}} to fit a piecewise linear model of pressure as a function of scan,
#'   in which pressure is 
#'   constant for scan less than a critical value, and then linearly varying for with scan. This is
#'   meant to handle the common situation in which the CTD is held at roughly constant depth (typically 
#'   a metre or so) to equilibrate, before it is lowered through the water column.
#'   Case \code{"B"} is the same,
#'   except that the pressure in the surface region is taken to be zero (this does not make
#'   much sense, but it might help in some cases). Note that, prior to early 2016, method \code{"B"} was
#'   called method \code{"C"}; the old \code{"B"} method was judged useless and was removed.}
#'   
#'   \item{If \code{method="index"} or \code{"scan"}, then each column of data is subsetted according to the
#'   value of \code{parameters}. If the latter is a logical vector of length matching data column
#'   length, then it is used directly for subsetting. If \code{parameters} is a numerical vector with
#'   two elements, then the index or scan values that lie between \code{parameters[1]}
#'   and \code{parameters[2]} (inclusive) are used for subsetting.  The
#'   two-element method is probably the most useful, with the values being determined by visual
#'   inspection of the results of \code{\link{plotScan}}. While this may take a minute or two, the
#'   analyst should bear in mind that a deep-water CTD profile might take 6 hours, corresponding to
#'   ship-time costs exceeding a week of salary.}
#'   
#'   \item{If \code{method="range"} then data are selected based on the value of the column named
#'   \code{parameters$item}.  This may be by range or by critical value.  By range: select values
#'   between \code{parameters$from} (the lower limit) and \code{parameters$to} (the upper limit) By
#'   critical value: select if the named column exceeds the value.  For example, \code{ctd2 <-
#'     ctdTrim(ctd, "range", parameters=list(item="scan", from=5))} starts at scan number 5 and
#'   continues to the end, while
#'   \code{ctdTrim(ctd,"range",parameters=list(item="scan",from=5,to=100))} also starts at scan 5,
#'   but extends only to scan 100.}
#' 
#'   \item{If \code{method} is a function, then it must return a vector of \code{\link{logical}}
#'   values, computed based on two arguments: \code{data} (a
#'   \code{\link{list}}), and \code{parameters} as supplied to \code{ctdTrim}.  Both
#'   \code{inferWaterDepth} and \code{removeInversions} are ignored in the function case. See
#'   \dQuote{Examples}.}
#' }
#'
#' @param x A \code{ctd} object, e.g. as read by \code{\link{read.ctd}}.
#' 
#' @param method A string (or a vector of two strings) specifying the trimming method, or a function to
#' be used to determine data indices to keep.  If \code{method} is not provided, \code{"downcast"} is
#' assumed. See \dQuote{Details}.
#' 
#' @param removeDepthInversions Logical value indicating whether to remove any levels at which depth is
#' less than, or equal to, a depth above.  (This is needed if the object is to be assembled into a
#' section, unless \code{\link{ctdDecimate}} will be used, which will remove the inversions.
#' 
#' @param parameters A list whose elements depend on the method; see \dQuote{Details}.
#' 
#' @template debugTemplate
#' 
#' @return An object of \code{\link{ctd-class}}, with data having been trimmed in some way.
#' 
#' @examples
#' \dontrun{
#' library(oce)
#' data(ctdRaw) 
#' plot(ctdRaw) # barely recognizable, due to pre- and post-cast junk
#' plot(ctdTrim(ctdRaw)) # looks like a real profile ...
#' plot(ctdDecimate(ctdTrim(ctdRaw),method="boxcar")) # ... smoothed
#' # Demonstrate use of a function. The scan limits were chosen
#' # by using locator(2) on a graph made by plotScan(ctdRaw).
#' trimByIndex<-function(data, parameters) {
#'   parameters[1] < data$scan & data$scan < parameters[2]
#' }
#' trimmed <- ctdTrim(ctdRaw, trimByIndex, parameters=c(130, 380))
#' plot(trimmed)
#' }
#' 
#' @references
#' The Seabird CTD instrument is described at
#' \url{http://www.seabird.com/products/spec_sheets/19plusdata.htm}.
#' 
#' @author Dan Kelley
#'
#' @family things related to \code{ctd} data
ctdTrim <- function(x, method, removeDepthInversions=FALSE, parameters=NULL,
                   debug=getOption("oceDebug"))
{
    oceDebug(debug, "ctdTrim() {\n", unindent=1)
    methodIsFunction <- !missing(method) && is.function(method)
    if (!inherits(x, "ctd"))
        stop("method is only for objects of class '", "ctd", "'")
    pressure <- x[["pressure"]]
    if (1 == length(unique(diff(pressure)))) {
        oceDebug(debug, "diff(p) is constant, so return input unaltered\n", unindent=1)
        oceDebug(debug, "} # ctdTrim()\n", unindent=1)
        return(x)
    }
    if (!("scan" %in% names(x@data))) {
        x@data$scan <- seq_along(pressure)
    }
    res <- x
    if (!methodIsFunction) {
        n <- length(pressure)
        if (n < 2) {
            warning("too few data to ctdTrim()")
            return(res)
        }
        if (missing(method)) {
            method <- "downcast"
            submethod <- "A"
        } else {
            if (length(method) == 1) {
                submethod <- method[1]
                submethod <- "A"
            } else if (length(method) == 2) {
                submethod <- method[2]
                method <- method[1]
            } else {
                stop("if provided, 'method' must be of length 1 or 2")
            }
        }
        method <- match.arg(method, c("downcast", "index", "scan", "range"))
        oceDebug(debug, paste("ctdTrim() using method \"", method, "\"\n", sep=""))
        keep <- rep(TRUE, n)
        if (method == "index") {
            if (is.logical(parameters)) {
                if (length(parameters) != n)
                    stop("for method=\"index\", need length(parameters) to match number of pressure values")
                keep <- parameters
            } else {
                oceDebug(debug, paste("trimming from index \"", parameters[1], " to ", parameters[2], "\"\n", sep=""))
                if (length(parameters) == 2) {
                    parameters[1] <- max(1, as.integer(parameters[1]))
                    parameters[2] <- min(n, as.integer(parameters[2]))
                    keep <- rep(FALSE, n)
                    keep[seq.int(parameters[1], parameters[2])] <- TRUE
                } else {
                    stop("length of parameters must be 2, or must match the ctd column length")
                }
            }
        } else if (method == "scan") {
            if (!"scan" %in% names(x@data)) stop("no \"scan\" in this ctd dataset")
            scan <- x[["scan"]]
            if (is.logical(parameters)) {
                if (length(parameters) != n)
                    stop("for method=\"scan\", need length(parameters) to match number of pressure values")
                keep <- parameters
            } else {
                if (length(parameters) == 2) {
                    keep <- parameters[1] <= scan & scan <= parameters[2]
                } else {
                    stop("length of parameters must be 2, or must match the ctd column length")
                }
            }
        } else if (method == "downcast") {
            ## 1. despike to remove (rare) instrumental problems
            ##pSmooth <- smooth(x@data$pressure, kind="3R")
            ## 2014-01-08: remove the following block that reverses a profile.  This
            ## was happening for some 24-Hz data (see also below), and it seems unlikely
            ## this block of code will ever be useful, anyway.
            ## 2015-04-04 ascending <- FALSE
            ## 2015-04-04 if (FALSE) {
            ## 2015-04-04     ascending <- 0 > mean(diff(pSmooth[1:min(3, 0.2*n)]))
            ## 2015-04-04     oceDebug(debug, "ascending=", ascending, "\n")
            ## 2015-04-04     if (ascending) {
            ## 2015-04-04         for (name in names(x@data)) {
            ## 2015-04-04             x@data[[name]] <- rev(x@data[[name]])
            ## 2015-04-04         }
            ## 2015-04-04     }
            ## 2015-04-04 }
            pmin <- -5
            pminGiven <- FALSE
            if (!missing(parameters)) {
                if ("pmin" %in% names(parameters)) {
                    pmin <- parameters$pmin
                    pminGiven <- TRUE
                } else {
                    stop("parameter not understood for this method")
                }
            }
            oceDebug(debug, 'pmin=', pmin, '\n')
            keep <- (pressure > pmin) # 2. in water (or below start depth)
            ## 2014-01-08 delta.p <- diff(x@data$pressure)  # descending
            ## 2014-01-08 delta.p <- c(delta.p[1], delta.p) # to get right length
            ## 2014-01-08 ## previous to this time, we had
            ## 2014-01-08          keep <- keep & (delta.p > 0)
            ## 2014-01-08 ## here.  However, this failed for some data with 24 Hz sampling, because in
            ## 2014-01-08 ## that case, what was clearly a descent phase had sign flips in delta.p;
            ## 2014-01-08 ## for this reason, the line of code was dropped today.

            ## 3. trim the upcast and anything thereafter (ignore beginning and end)
            ##2015-04-04 # This was misbehaving on RBR data, and I'd prefer to get the simpler
            ##2015-04-04 # method working, anyway, so I'm removing the fancy bits.
            ##2015-04-04 trim.top <- as.integer(0.1*n)
            ##2015-04-04 trim.bottom <- as.integer(0.9*n)
            ##2015-04-04 max.spot <- which.max(smooth(x@data$pressure[trim.top:trim.bottom],kind="3R"))
            ##2015-04-04 max.location <- trim.top + max.spot
            ##2015-04-04 keep[max.location:n] <- FALSE
            max.location <- which.max(smooth(pressure, kind="3R"))
            keep[max.location:n] <- FALSE
            oceDebug(debug, "removed data at indices from ", max.location,
                     " (where pressure is ", pressure[max.location], ") to the end of the data\n", sep="")
            ## 2011-02-04 if (FALSE) {
            ## 2011-02-04     ## deleted method: slowly-falling data
            ## 2011-02-04     delta.p.sorted <- sort(delta.p)
            ## 2011-02-04     if (!is.null(parameters)) {
            ## 2011-02-04         dp.cutoff <- t.test(delta.p[keep], conf.level=0.5)$conf.int[1]
            ## 2011-02-04         print(t.test(delta.p[keep], conf.level=0.05))#$conf.int[1]
            ## 2011-02-04     } else {
            ## 2011-02-04         dp.cutoff <- delta.p.sorted[0.1*n]
            ## 2011-02-04     }
            ## 2011-02-04     keep[delta.p < dp.cutoff] <- FALSE
            ## 2011-02-04 }
            ## 4. remove equilibration phase
            ## 2011-02-04 if (FALSE) {                # old method, prior to Feb 2008
            ## 2011-02-04     pp <- x@data$pressure[keep]
            ## 2011-02-04     ss <- x@data$scan[keep]
            ## 2011-02-04     equilibration <- (predict(m <- lm(pp ~ ss), newdata=list(ss=x@data$scan)) < 0)
            ## 2011-02-04     keep[equilibration] <- FALSE
            ## 2011-02-04 }
            if (!pminGiven) {                 # new method, after Feb 2008
                submethodChoices <- c("A", "B")
                sm <- pmatch(submethod, submethodChoices)
                if (is.na(submethod))
                    stop("unknown submethod '", submethod, "'")
                submethod <- submethodChoices[sm]
                ## bilinearAold<-function(param) { # param=c(s0,p0,dpds); this uses ss and pp
                ##     s0 <-  param[1]
                ##     p0 <- abs(param[2])
                ##     dpds <- param[3]
                ##     ifelse(ss < s0, p0, p0 + dpds * (ss - s0))
                ##     model <- ifelse(ss < s0, p0, p0 + dpds * (ss - s0))
                ##     diff <- pp - model
                ##     misfit <- sqrt(mean(diff^2))
                ##     oceDebug(debug-1, "bilinearA s0=", s0, "p0=", p0, "dpds=", dpds, "; misfit=", misfit, "\n")
                ##     misfit
                ## }
                bilinearA<-function(s, s0, p0, dpds) { # same model as B but results treated differently
                    oceDebug(debug-1, "bilinearA s0=", s0, "p0=", p0, "dpds=", dpds, "\n")
                    ifelse(s < s0, p0, p0+dpds*(s-s0))
                }
                bilinearB<-function(s, s0, dpds) {
                    oceDebug(debug-1, "bilinearB s0=", s0, "dpds=", dpds, "\n")
                    ifelse(s < s0, 0, dpds*(s-s0))
                }
                pp <- pressure[keep]
                pp <- despike(pp) # some, e.g. data(ctdRaw), have crazy points in air
                ss <- x@data$scan[keep]
                ##look <- smooth(pp) < 20 # smooth because in-air can sometimes be crazy high
                end <- which(smooth(pp) > 20)[1]
                if (!is.na(end)) {
                    pp <- pp[1:end]
                    ss <- ss[1:end]
                }
                p0 <- 0
                s0 <- ss[0.25*length(ss)]
                p0 <- pp[1]
                ##p1 <- max(pp) #pp[0.9*length(pp)]
                if (length(ss) > 2)
                    dpds0 <-  diff(range(pp, na.rm=TRUE)) / diff(range(ss, na.rm=TRUE))
                else
                    dpds0 <- 0
                ## Handle submethods.
                ## Note in December 2015: the old method B seemed useless. Even method
                ## C seems a bit useless to DK, actually, and he may remove that too.
                if (submethod == "A") {
                    oceDebug(debug, "method[2]=\"A\"\n")
                    t <- try(m <- nls(pp ~ bilinearA(ss, s0, p0, dpds),
                                      start=list(s0=s0, p0=0, dpds=dpds0)), silent=TRUE)
                    if (class(t) == "try-error") stop("trimming failed to converge with submethod A")
                    C <- coef(m)
                    scanStart <- max(1, floor(0.5 + C["s0"]))
                    ##> oceDebug(debug, "method[2]=\"A\", so using single-segment model\n")
                    ##> sGuess <- mean(ss, na.rm=TRUE)
                    ##> pGuess <- 0
                    ##> dpdsGuess <- mean(diff(pp)/diff(ss), na.rm=TRUE)
                    ##> t <- try(o <- optim(c(sGuess, pGuess, dpdsGuess), bilinearA), silent=!TRUE)
                    ##> if (class(t) == "try-error") stop("trimming failed to converge with submethod A")
                    ##> scanStart <- o$par[1]
                ##} else if (submethod == "B") {
                ##    oceDebug(debug, "method[2]=\"B\" so using two-segment model with constant near-surface pressure\n")
                ##    t <- try(m <- nls(pp ~ bilinearB(ss, s0, p0, dpds),
                ##                      start=list(s0=s0, p0=0, dpds=dpds0)), silent=TRUE)
                ##    if (class(t) == "try-error") stop("trimming failed to converge with submethod B")
                ##    C <- coef(m)
                ##    scanStart <- max(1, floor(0.5 + C["s0"] - C["p0"] / C["dpds"]))
                } else if (submethod == "B") {
                    oceDebug(debug, "method[3]=\"B\" so using two-segment model with zero near-surface pressure\n")
                    t <- try(m <- nls(pp ~ bilinearB(ss, s0, dpds),
                                      start=list(s0=s0, dpds=dpds0)), silent=TRUE)
                    if (class(t) == "try-error") stop("trimming failed to converge with submethod B")
                    C <- coef(m)
                    scanStart <- max(1, floor(0.5 + C["s0"]))
                } else {
                    stop("unknown submethod '", submethod, "'")
                }
                oceDebug(debug-1, "scanStart:", scanStart, "\n")
                keep <- keep & (x@data$scan > scanStart)
            }
            ## 2014-01-08: remove the following block that reverses a profile.
            ## 2015-04-04 if (ascending) {
            ## 2015-04-04     for (name in names(x@data)) {
            ## 2015-04-04         x@data[[name]] <- rev(x@data[[name]])
            ## 2015-04-04     }
            ## 2015-04-04 }
        } else if (method == "range") {
            if (!("item" %in% names(parameters)))
                stop("'parameters' must be a list containing 'item'")
            oceDebug(debug, "method='range'; parameters are as follows:\n")
            if (debug>0)
                print(parameters)
            item <- parameters$item
            if (!(item %in% names(x@data)))
                stop("x@data has no item named '", item, "'")
            keep <- rep(TRUE, n)
            if ("from" %in% names(parameters))
                keep <- keep & (x@data[[item]] >= parameters$from)
            if ("to" %in% names(parameters))
                keep <- keep & (x@data[[item]] <= parameters$to)
        } else {
            stop("'method' not recognized; must be 'index', 'downcast', 'scan', or 'range'")
        }
    } else {
        keep <- method(data=x@data, parameters=parameters)
    }
    if (is.data.frame(res@data)) {
        res@data <- res@data[keep,]
    } else {
        for (i in seq_along(res@data)) {
            res@data[[i]] <- res@data[[i]][keep]
        }
    }
    ## waterDepthWarning <- FALSE
    ## if (inferWaterDepth) {
    ##     res@metadata$waterDepth <- max(res@data$pressure, na.rm=TRUE)
    ##     waterDepthWarning <- TRUE
    ## }
    if (removeDepthInversions) {
        badDepths <- c(FALSE, diff(pressure) <= 0)
        nbad <- sum(badDepths)
        if (nbad > 0) {
            for (col in seq_along(x@data))
                res@data[[col]] <- res@data[[col]][!badDepths]
            msg <- sprintf("removed %d levels that had depth inversions", nbad)
            warning(msg)
            msg <- sprintf("Note: ctdTrim() removed %d levels that had depth inversions",
                           nbad)
            warning("should add note about trimming depth inversions to processingLog")
        }
    }
    res@processingLog <- processingLogAppend(res@processingLog, paste(deparse(match.call()), sep="", collapse=""))
    ## if (waterDepthWarning)
    ##     res@processingLog <- processingLogAppend(res@processingLog, "inferred water depth from maximum pressure")
    oceDebug(debug, "} # ctdTrim()\n", unindent=1)
    res
}


#' Update a CTD header
#' 
#' Update the header of a \code{ctd} object, e.g. adjusting \code{nvalues} and the
#' \code{span} of each column. This is done automatically by \code{ctdTrim}, for
#' example.
#' 
#' @param x A \code{ctd} object, e.g. as read by \code{\link{read.ctd}}.
#' 
#' @template debugTemplate
#' 
#' @return A new \code{\link{ctd-class}} object.
#' 
#' @examples
#' library(oce)
#' data(ctd) 
#' ctd[["pressure"]] <- ctd[["pressure"]] + 3
#' ctdNew <- ctdUpdateHeader(ctd)
#' 
#' @references
#' The Seabird CTD instrument is described at
#'   \url{http://www.seabird.com/products/spec_sheets/19plusdata.htm}.
#' 
#' @author Dan Kelley
#'
#' @family things related to \code{ctd} data
ctdUpdateHeader <- function (x, debug = FALSE)
{
    if (length(x@metadata$header) < 1)
        stop("there is no header in this CTD object")
    if (length(x@data) < 1)
        stop("there are no data in this CTD object")
    replaceHeaderElement<-function(h, match, new)
    {
        for (i in 1:length(h)) {
            if (length(grep(match, h[i], perl=TRUE, useBytes=TRUE))) {
                h[i] <- new;
                break;
            }
        }
        return(h)
    }
    ## adjust nvalues
    ## ... fill in ...
    ## adjust column ranges
    ##nquan <- length(x@data)
    res <- x
    h <- x@metadata$header
    for (i in seq_along(x@data)) {
        r <- range(x@data[[i]])
        prefix <- sprintf("^#[\t ]*span[\t ]*%d[\t ]*=", i)
        span <- sprintf("# span %d = %g, %g", i, r[1], r[2])
        h <- replaceHeaderElement(h, prefix, span)
    }
    res@metadata$header <- h
    res
}


#' Write a CTD data object as a .csv file
#' 
#' Writes a comma-separated file containing the data frame stored in
#' \code{object@data}.  The file is suitable for reading with a spreadsheet, or
#' with \code{\link{read.csv}}.  Note that the output file will retain none of the
#' meta-data stored in \code{object}.
#' 
#' @param object A \code{ctd} object, e.g. as read by \code{\link{read.ctd}}.
#' 
#' @param file Either a character string (the file name) or a connection.  This is
#' a mandatory argument.
#' 
#' @seealso The documentation for \code{\link{ctd-class}} explains the structure
#' of CTD objects, and also outlines the other functions dealing with them.
#' 
#' @examples
#' \dontrun{
#' library(oce)
#' data(ctd)
#' write.ctd(ctd, "ctd.csv")
#' d <- read.csv("ctd.csv")
#' plot(as.ctd(d$salinity, d$temperature, d$pressure))
#' } 
#' 
#' @author Dan Kelley
#'
#' @family things related to \code{ctd} data
write.ctd <- function(object, file=stop("'file' must be specified"))
{
    if (!inherits(object, "ctd"))
        stop("method is only for objects of class '", "ctd", "'")
    if (is.character(file)) {
        if (file == "")
            stop("'file' must be a non-empty string")
        con <- file(file, "w")
    } else if (inherits(file, "connection")) {
        con <- file
    }
    write.table(object@data, col.names=TRUE, row.names=FALSE, sep=",", file=con)
    close(con)
}


#' Plot seawater CTD data
#' 
#' Plot CTD data, by default in a four-panel display showing (a) profiles of
#' salinity and temperature, (b) profiles of density and the square of buoyancy
#' frequency, (c) a TS diagram and (d) a coastline diagram indicating the station
#' location.
#' 
#' Creates a multi-panel summary plot of data measured in a CTD cast. The panels
#' are controlled by the \code{which} argument.  Normally, 4 panels are specified
#' with the \code{which}, but it can also be useful to specify less than 4 panels,
#' and then to draw other panels after this call.
#' 
#' If only 2 panels are requested, they will be drawn side by side.
#' 
#' If more than one panel is drawn, then on exit from \code{plot,ctd-method}, the value
#' of \code{par} will be reset to the value it had before the function call.
#' However, if only one panel is drawn, the adjustments to \code{par} made within
#' \code{plot,ctd-method} are left in place, so that further additions may be made to the
#' plot.
#' 
#' @param x A \code{ctd} object, e.g. as read by \code{\link{read.ctd}}, or a
#' list containing items named \code{salinity} and \code{temperature}.
#' 
#' @param which List of desired plot types, as given below. If \code{which} is not
#' supplied, a default will be used. This default will be \code{c(1,2,3,5)} if the
#' CTD is in profiling mode (i.e.  if \code{x@metadata$deploymentType=="profile"}
#' or if that item is non extant). If the deployment type is \code{"moored"} or
#' \code{"thermosalinograph"}, the default will be \code{c(30, 3, 31, 5)}.  If it
#' is \code{"towyo"}, \code{c(30, 31, 32, 3)} will be used. Details are as follows.
#' 
#' \itemize{
#'     \item \code{which=1} or \code{which="salinity+temperature"} gives
#'     a combined profile of temperature and salinity
#'     \item \code{which=2} or \code{which="density+N2"} gives a combined
#'     profile of \eqn{\sigma_\theta}{sigma-theta} and \eqn{N^2}{N^2}
#'     \item \code{which=3} or \code{which="TS"} gives a TS plot
#'     \item \code{which=4} or \code{which="text"} gives a textual
#'     summary of some aspects of the data
#'     \item \code{which=5} or \code{which="map"} gives a map plotted
#'     with \code{\link{plot,coastline-method}}, with a dot for
#'     the station location.  Notes near the top boundary of the map give the
#'     station number, the sampling date, and the name of the chief scientist,
#'     if these are known. Note that the longitude will be converted to a value
#'     between -180 and 180 before plotting.  (See also notes
#'     about \code{span}.)
#'     \item \code{which=5.1} as for \code{which=5}, except that the file name
#'     is drawn above the map
#'     \item \code{which=6} or \code{which="density+dpdt"} gives a
#'     profile of density and \eqn{dP/dt}{dP/dt}, which is useful for
#'     evaluating whether the instrument is dropping properly through the
#'     water column
#'     \item \code{which=7} or \code{which="density+time"} gives a
#'     profile of density and time
#'     \item \code{which=8} or \code{which="index"} gives a profile of
#'     index number (especially useful for \code{\link{ctdTrim}})
#'     \item \code{which=9} or \code{which="salinity"} gives a salinity profile
#'     \item \code{which=10} or \code{which="temperature"} gives a temperature profile
#'     \item \code{which=11} or \code{which="density"} gives a density profile
#'     \item \code{which=12} or \code{which="N2"} gives an \eqn{N^2}{N^2} profile
#'     \item \code{which=13} or \code{which="spice"} gives a spiciness profile
#'     \item \code{which=14} or \code{which="tritium"} gives a tritium profile
#'     \item \code{which=15} or \code{which="Rrho"} gives an Rrho profile
#'     \item \code{which=16} or \code{which="RrhoSF"} gives an RrhoSF profile
#'     \item \code{which=17} or \code{which="conductivity"} gives a conductivity profile
#' }
#' 
#' @param col Colour of lines or symbols.
#' 
#' @param fill A legacy parameter that will be permitted only temporarily; see
#' \dQuote{History}.
#' 
#' @param borderCoastline Colour of coastlines and international borders, passed
#' to \code{\link{plot,coastline-method}} if a map is included in \code{which}.
#' 
#' @param colCoastline Fill colour of coastlines and international borders, passed
#' to \code{\link{plot,coastline-method}} if a map is included in \code{which}. Set to
#' \code{NULL} to avoid filling.
#' 
#' @param eos String indicating the equation of state to be used, either
#' \code{"unesco"} or \code{"gsw"}.
#' 
#' @param ref.lat Latitude of reference point for distance calculation.
#' 
#' @param ref.lon Longitude of reference point for distance calculation.
#' 
#' @param grid Set \code{TRUE} to get a grid on all plots.
#' 
#' @param coastline A specification of the coastline to be used for
#' \code{which="map"}.  This may be a coastline object, whether built-in or
#' supplied by the user, or a character string.  If the later, it may be the
#' name of a built-in coastline (\code{"coastlineWorld"},
#' \code{"coastlineWorldFine"}, or
#' \code{"coastlineWorldCoarse"}), or \code{"best"}, to choose 
#' a suitable coastline for the locale, or \code{"none"} to prevent
#' the drawing of a coastline.  There is a speed penalty for providing
#' \code{coastline} as a character string, because it forces
#' \code{\link{plot,coastline-method}} to load it on every call.  So, if
#' \code{\link{plot,coastline-method}} is to be called several times for a given
#' coastline, it makes sense to load it in before the first call, and to
#' supply the object as an argument, as opposed to the name of the object.
#' 
#' @param Slim Optional limits of salinity axes.
#' 
#' @param Clim Optional limits of conductivity axes.
#' 
#' @param Tlim Optional limits of temperature axes.
#' 
#' @param plim Optional limits of pressure axes.
#' 
#' @param densitylim Optional limits of density axis.
#' 
#' @param N2lim Optional limits of \eqn{N^2}{N^2} axis.
#' 
#' @param Rrholim Optional limits of \eqn{R_rho}{R_rho} axis.
#' 
#' @param dpdtlim Optional limits of dP/dt axis.
#' 
#' @param timelim Optional limits of delta-time axis.
#' 
#' @param lonlim Optional limits of longitude axis of map (ignored if no map
#' plotted) DEPRECATED 2014-01-07. 
#' 
#' @param latlim Optional limits of latitude axis of map (ignored if no map
#' plotted) DEPRECATED 2014-01-07. 
#' 
#' @param clongitude Center longitude.
#' 
#' @param clatitude Center latitude.
#' 
#' @param span Optional span of map, in km.  If not given, this will be determined
#' as a small multiple of the distance to the nearest point of land, in an
#' attempt to show some coastline in the plot.
#' 
#' @param showHemi Logical indicating whether to show hemisphere in axis tick
#' labels.
#' 
#' @param lonlabel,latlabel,sides Optional vectors of longitude and latitude to
#' label on the indicated sides of plot, passed to \code{\link{plot,coastline-method}}.
#' Using these arguments permits reasonably simple customization.  If they are are
#' not provided, reasonable defaults will be used.
#' 
#' @param projection Projection for map, if desired.  If this is \code{NULL}, no
#' projection will be used; the map will simply show longitude and latitude in a
#' cartesian frame, scaled to retain shapes at the centre.  If this is the string
#' \code{"automatic"}, then either a Mercator or Stereographic projection will be
#' used, depending on whether the CTD station is within 70 degrees of the equator
#' or at higher latitudes.  Finally, if this is a string in the format used by
#' \code{\link{mapPlot}}, then it is is passed to that function.
#' 
#' @param parameters Parameters for map, as for \code{projection}.
#' 
#' @param orientation Orientation for map, as for \code{projection}.
#' 
#' @param latlon.pch Symbol code for sample location (ignored if no map plotted).
#' 
#' @param latlon.cex Symbol expansion factor for sample location (ignored if no
#' map plotted).
#' 
#' @param latlon.col Colour of symbol for sample location (ignored if no map
#' plotted).
#' 
#' @param cex Size to be used for plot symbols (see \code{\link{par}}).
#' 
#' @param cex.axis Size factor for axis labels (see \code{\link{par}}).
#' 
#' @param pch Code for plotting symbol (see \code{\link{par}}).
#' 
#' @param useSmoothScatter Boolean, set to \code{TRUE} to use
#' \code{\link{smoothScatter}} instead of \code{\link{plot}} to draw the plot.
#' 
#' @param df Optional argument that is ignored except for plotting buoyancy
#' frequency; in that case, it is passed to \code{\link{swN2}} as the argument
#' named \code{df}.
#' 
#' @param keepNA Flag indicating whether to keep \code{NA} values in linegraphs,
#' which will yield breaks in the lines.
#' 
#' @param type The type of plot to draw, using the same scheme as
#' \code{\link{plot}}.
#' 
#' @param adorn List of expressions to be executed for the panels in turn, e.g. to
#' adorn the plots.  If the number matches the number of panels, then the strings
#' are applied to the appropriate panels, as they are drawn from top-left to
#' bottom-right.   If only a single expression is provided, it is used for all
#' panels.  (See \dQuote{Examples}.)
#' 
#' @param mgp 3-element numerical vector to use for \code{par(mgp)}, and also for
#' \code{par(mar)}, computed from this.  The default is tighter than the R
#' default, in order to use more space for the data and less for the axes.
#' 
#' @param mar Value to be used with \code{\link{par}("mar")}.
#' 
#' @param inset Set to \code{TRUE} for use within \code{\link{plotInset}}.  The
#' effect is to prevent the present function from adjusting margins, which is
#' necessary because margin adjustment is the basis for the method used by
#' \code{\link{plotInset}}.
#' 
#' @param add Logical, indication of whether to add to an existing plot.  This
#' only works if \code{length(which)=1}, and it will yield odd results if the
#' value of \code{which} does not match that in the previous plots.
#' 
#' @template debugTemplate
#' 
#' @param ... Optional arguments passed to plotting functions. A common example is
#' to set \code{df}, for use in \link{swN2} calculations.
#' 
#' 
#' @seealso
#' The documentation for \code{\link{ctd-class}} explains the structure of CTD
#' objects, and also outlines the other functions dealing with them.
#' 
#' @section History:
#' Until February, 2016, \code{plot,ctd-method} relied on a now-defunct argument
#' \code{fill} to control colours; \code{colCoastline} is to be used now, instead.
#' Also, now it is possible to set the edge of coasts and international
#' boundaries, with \code{borderCoastline}.
#' 
#' @examples
#' library(oce)
#' data(ctd) 
#' plot(ctd)
#' 
#' @author Dan Kelley
#' 
#' @family functions that plot \code{oce} data
#' @family things related to \code{ctd} data
setMethod(f="plot",
          signature=signature("ctd"),
          definition=function(x, which,
                              col=par("fg"),
                              fill, # to catch old method
                              borderCoastline=NA, colCoastline="lightgray",
                              eos=getOption("oceEOS", default='gsw'),
                              ref.lat=NaN, ref.lon=NaN,
                              grid=TRUE, coastline="best",
                              Slim, Clim, Tlim, plim, densitylim, N2lim, Rrholim,
                              dpdtlim, timelim,
                              lonlim, latlim, # FIXME: maybe should be deprecated 2014-01-07
                              clongitude, clatitude, span, showHemi=TRUE,
                              lonlabel=NULL, latlabel=NULL, sides=NULL,
                              projection=NULL, parameters=NULL, orientation=NULL,
                              latlon.pch=20, latlon.cex=1.5, latlon.col="red",
                              cex=1, cex.axis=par('cex.axis'),
                              pch=1,
                              useSmoothScatter=FALSE,
                              df,
                              keepNA=FALSE,
                              type='l',
                              adorn=NULL,
                              mgp=getOption("oceMgp"),
                              mar=c(mgp[1]+1.5,mgp[1]+1.5,mgp[1]+1.5,mgp[1]+1),
                              inset=FALSE,
                              add=FALSE,
                              debug=getOption("oceDebug"),
                              ...)
          {
              eos <- match.arg(eos, c("unesco", "gsw"))
              if (!missing(fill)) {
                  ## permit call as documented before 2016-02-03
                  ## Note: the code permitted fill=TRUE but this was never documented
                  if (is.character(fill)) {
                      colCoastline <- fill
                  } else {
                      if (is.logical(fill) && !fill) {
                          colCoastline <- NULL
                      }
                  }
                  warning("In plot,ctd-method() : 'fill' being accepted for backwards compatibility; please use 'colCoastline' instead", call.=FALSE)
              }
              if (missing(which)) {
                  oceDebug(debug, "plot,ctd-method(..., eos=\"", eos, "\", inset=", inset, ", ...) {\n", sep="", unindent=1)
                  dt <- x@metadata$deploymentType
                  if (is.null(dt)) {
                      which <- c(1, 2, 3, 5)
                  } else {
                      types <- c("profile", "moored", "thermosalinograph", "tsg", "towyo")
                      itype <- pmatch(dt, types, nomatch=0)
                      if (itype == 0) {
                          ## warning("unknown deploymentType \"", dt, "\"; using \"profile\" instead")
                          dt <- "profile"
                      } else {
                          dt <- types[itype]
                      }
                      if ("profile" == dt) {
                          which <- c(1, 2, 3, 5)
                      } else if ("moored" == dt) {
                          which <- c(30, 3, 31, 5)
                      } else if ("thermosalinograph" == dt) {
                          which <- c(30, 3, 31, 5)
                      } else if ("tsg" == dt) { # @richardsc -- do you think we still need this?
                          which <- c(30, 3, 31, 5)
                      } else if ("towyo" == dt) {
                          which <- c(30, 3, 33, 5)
                      } else {
                          which <- c(1, 2, 3, 5)
                      }
                  }
               } else {
                  oceDebug(debug, "plot,ctd-method(..., which=c(", paste(which, collapse=",", sep=""),
                           "), eos=\"", eos, "\", inset=", inset, ", ...) {\n", sep="", unindent=1)
              }
              lw <- length(which)
              dots <- list(...)
              dotsNames <- names(dots)
              ## FIXME: In the below, we could be more clever for single-panel plots
              ## but it may be better to get users out of the habit of supplying xlim
              ## etc (which will yield errors in plot.lm(), for example).
              if ("xlim" %in% dotsNames)
                  stop("in plot,ctd-method() : 'xlim' argument not allowed; use Slim for a salinity profile, Tlim for a temperature profile, etc", call.=FALSE)
              if ("ylim" %in% dotsNames)
                  stop("in plot,ctd-method() : 'ylim' argument not allowed; use plim for a profile, Tlim for a TS plot, etc", call.=FALSE)
              opar <- par(no.readonly = TRUE)
              if (add && lw > 1) {
                  warning("ignoring add=TRUE because length(which) > 1")
                  add <- FALSE
              }
              if (lw > 1) on.exit(par(opar))
              if (length(type) < lw)
                  type <- rep(type, lw) # FIXME: recycle more sensibly
              if (length(pch) < lw)
                  pch <- rep(pch, lw) # FIXME: recycle more sensibly
              if (length(cex) < lw)
                  cex <- rep(cex, lw) # FIXME: recycle more sensibly
              ##dec_deg<-function(x, code = "lat")
              ##{
              ##    if (code == "lat") {
              ##        if (x < 0) {
              ##            x <- -x
              ##            sprintf("%.0f %.2fS", floor(x), 60 * (x - floor(x)))
              ##        } else {
              ##            sprintf("%.0f %.2fN", floor(x), 60 * (x - floor(x)))
              ##        }
              ##    } else {
              ##        if (x < 0) {
              ##            x <- -x
              ##            sprintf("% %.2fW", floor(x), 60 * (x - floor(x)))
              ##        } else {
              ##            sprintf("% %.2fE", floor(x), 60 * (x - floor(x)))
              ##        }
              ##    }
              ##}
              adorn.length <- length(adorn)
              if (adorn.length == 1) {
                  adorn <- rep(adorn, lw)
                  adorn.length <- lw
              }
              if (!inset)
                  par(mar=mar)
              par(mgp=mgp)

              if (lw > 1) {
                  ##oldpar <- par(no.readonly = TRUE)
                  if (lw > 2) layout(matrix(1:4, nrow=2, byrow=TRUE)) else
                      layout(matrix(1:2, nrow=2, byrow=TRUE))
                  ##layout.show(lay)
                  ##stop()
              }
              ## Ignore any bottom region consisting of NA for temperature and salinity, e.g.
              ## as created by as.section() or read.section().
              if (0 == length(x@data$salinity)) {
                  warning("no data to plot in this object")
                  return(invisible())
              }
              last.good <- which(rev(is.na(x@data$salinity))==FALSE)[1]
              if (!is.na(last.good) && length(last.good) > 0) {
                  last.good <- length(x@data$temperature) - last.good + 1
                  for (nc in seq_along(x@data)) {
                      if (!is.null(x@data[[nc]])) {
                          x@data[[nc]] <- x@data[[nc]][1:last.good]
                      }
                  }
              }
              if (!missing(latlim))
                  warning("the latlim argument is deprecated; should instead specify clongitude, clatitude, and span")
              if (!missing(lonlim))
                  warning("the lonlim argument is deprecated; should instead specify clongitude, clatitude, and span")

              whichOrig <- which
              which <- oce.pmatch(which,
                                  list("salinity+temperature"=1,
                                       "density+N2"=2,
                                       TS=3,
                                       text=4,
                                       map=5,
                                       "density+dpdt"=6,
                                       "density+time"=7,
                                       index=8,
                                       salinity=9,
                                       temperature=10,
                                       density=11,
                                       N2=12,
                                       spice=13,
                                       tritium=14,
                                       Rrho=15,
                                       RrhoSF=16,
                                       "conductivity"=17,
                                       "Sts"=30,
                                       "Tts"=31,
                                       "pts"=32,
                                       "rhots"=33))

              for (w in 1:length(which)) {
                  if (is.na(which[w])) {
                      if (whichOrig[w] %in% names(x@data)) {
                          plotProfile(x, xtype=x[[whichOrig[w]]], xlab=whichOrig[w],
                                      Slim=Slim, Tlim=Tlim, plim=plim,
                                      eos=eos,
                                      useSmoothScatter=useSmoothScatter,
                                      grid=grid, col.grid="lightgray", lty.grid="dotted",
                                      cex=cex[w], pch=pch[w], type=type[w], keepNA=keepNA, inset=inset, add=add,
                                      debug=debug-1,
                                      ...)
                      } else {
                          warning("plot,ctd-method(): unknown plot type \"", whichOrig[w], "\" requested\n", call.=FALSE)
                      }
                      next
                  }
                  if (which[w] == 1) {
                      plotProfile(x, xtype="salinity+temperature", Slim=Slim, Tlim=Tlim, plim=plim,
                                  eos=eos,
                                  useSmoothScatter=useSmoothScatter,
                                  grid=grid, col.grid="lightgray", lty.grid="dotted",
                                  cex=cex[w], pch=pch[w], type=type[w], keepNA=keepNA, inset=inset, add=add,
                                  debug=debug-1,
                                  ...)
                  } else if (which[w] == 2) {
                      plotProfile(x, xtype="density+N2",
                                  plim=plim,
                                  eos=eos,
                                  useSmoothScatter=useSmoothScatter,
                                  df=df,
                                  grid=grid, col.grid="lightgray", lty.grid="dotted",
                                  cex=cex[w], pch=pch[w], type=type[w], keepNA=keepNA, inset=inset, add=add,
                                  debug=debug-1,
                                  ...)
                  } else if (which[w] == 6) {
                      plotProfile(x, xtype="density+dpdt",
                                  plim=plim, densitylim=densitylim, dpdtlim=dpdtlim,
                                  col=col,
                                  eos=eos,
                                  useSmoothScatter=useSmoothScatter,
                                  grid=grid, col.grid="lightgray", lty.grid="dotted",
                                  cex=cex[w], pch=pch[w], type=type[w], keepNA=keepNA, inset=inset, add=add,
                                  debug=debug-1,
                                  ...)
                  } else if (which[w] == 7) {
                      plotProfile(x, xtype="density+time",
                                  plim=plim, densitylim=densitylim, timelim=timelim,
                                  useSmoothScatter=useSmoothScatter,
                                  grid=grid, col.grid="lightgray", lty.grid="dotted",
                                  cex=cex[w], pch=pch[w], type=type[w], keepNA=keepNA, inset=inset, add=add,
                                  debug=debug-1,
                                  ...)
                  } else if (which[w] == 8) {
                      plotProfile(x, xtype="index",
                                  plim=plim,
                                  col=col,
                                  eos=eos,
                                  useSmoothScatter=useSmoothScatter,
                                  grid=grid, col.grid="lightgray", lty.grid="dotted",
                                  cex=cex[w], pch=pch[w], type=type[w], keepNA=keepNA, inset=inset, add=add,
                                  debug=debug-1,
                                  ...)
                  } else if (which[w] == 9) {
                      plotProfile(x, xtype="salinity",
                                  plim=plim,
                                  Slim=Slim,
                                  col=col,
                                  eos=eos,
                                  useSmoothScatter=useSmoothScatter,
                                  grid=grid, col.grid="lightgray", lty.grid="dotted",
                                  cex=cex[w], pch=pch[w], type=type[w], keepNA=keepNA, inset=inset, add=add,
                                  debug=debug-1,
                                  ...)
                  } else if (which[w] == 10) {
                      plotProfile(x, xtype="temperature",
                                  plim=plim,
                                  Tlim=Tlim,
                                  col=col,
                                  eos=eos,
                                  useSmoothScatter=useSmoothScatter,
                                  grid=grid, col.grid="lightgray", lty.grid="dotted",
                                  cex=cex[w], pch=pch[w], type=type[w], keepNA=keepNA, inset=inset, add=add,
                                  debug=debug-1,
                                  ...)
                   } else if (which[w] == 11) {
                      plotProfile(x, xtype="density",
                                  plim=plim,
                                  densitylim=densitylim,
                                  grid=grid,
                                  col=col,
                                  eos=eos,
                                  useSmoothScatter=useSmoothScatter,
                                  col.grid="lightgray", lty.grid="dotted",
                                  cex=cex[w], pch=pch[w], type=type[w], keepNA=keepNA, inset=inset, add=add,
                                  debug=debug-1,
                                  ...)
                  } else if (which[w] == 12) {
                      plotProfile(x, xtype="N2",
                                  plim=plim,
                                  N2lim=N2lim,
                                  grid=grid,
                                  col=col,
                                  eos=eos,
                                  df=df,
                                  useSmoothScatter=useSmoothScatter,
                                  col.grid="lightgray", lty.grid="dotted",
                                  cex=cex[w], pch=pch[w], type=type[w], keepNA=keepNA, inset=inset, add=add,
                                  debug=debug-1,
                                  ...)
                  } else if (which[w] == 13) {
                      plotProfile(x, xtype="spice",
                                  plim=plim,
                                  col=col,
                                  eos=eos,
                                  useSmoothScatter=useSmoothScatter,
                                  grid=grid, col.grid="lightgray", lty.grid="dotted",
                                  cex=cex[w], pch=pch[w], type=type[w], keepNA=keepNA, inset=inset, add=add,
                                  debug=debug-1,
                                  ...)
                  } else if (which[w] == 14) {
                      plotProfile(x, xtype="tritium",
                                  plim=plim,
                                  col=col,
                                  eos=eos,
                                  useSmoothScatter=useSmoothScatter,
                                  grid=grid, col.grid="lightgray", lty.grid="dotted",
                                  cex=cex[w], pch=pch[w], type=type[w], keepNA=keepNA, inset=inset, add=add,
                                  debug=debug-1,
                                  ...)
                  } else if (which[w] == 15) {
                      plotProfile(x, xtype="Rrho",
                                  plim=plim,
                                  col=col,
                                  eos=eos,
                                  useSmoothScatter=useSmoothScatter,
                                  grid=grid, col.grid="lightgray", lty.grid="dotted",
                                  cex=cex[w], pch=pch[w], type=type[w], keepNA=keepNA, inset=inset, add=add,
                                  debug=debug-1,
                                  ...)
                  } else if (which[w] == 16) {
                      plotProfile(x, xtype="RrhoSF",
                                  plim=plim,
                                  col=col,
                                  eos=eos,
                                  useSmoothScatter=useSmoothScatter,
                                  grid=grid, col.grid="lightgray", lty.grid="dotted",
                                  cex=cex[w], pch=pch[w], type=type[w], keepNA=keepNA, inset=inset, add=add,
                                  debug=debug-1,
                                  ...)
                  } else if (which[w] == 17) {
                      plotProfile(x, xtype="conductivity", Clim=Clim, plim=plim,
                                  col=col,
                                  eos=eos,
                                  useSmoothScatter=useSmoothScatter,
                                  grid=grid, col.grid="lightgray", lty.grid="dotted",
                                  cex=cex[w], pch=pch[w], type=type[w], keepNA=keepNA, inset=inset, add=add,
                                  debug=debug-1,
                                  ...)
                  } else if (which[w] == 3) {
                      ##par(mar=c(3.5,3,2,2))
                      lwd.rho <- if ("lwd.rho" %in% names(dots)) dots$lwd.rho else par('lwd')
                      lty.rho <- if ("lty.rho" %in% names(dots)) dots$lty.rho else par('lty')
                      plotTS(x, Slim=Slim, Tlim=Tlim,
                             grid=grid, col.grid="lightgray", lty.grid="dotted",
                             eos=eos,
                             lwd.rho=lwd.rho, lty.rho=lty.rho,
                             useSmoothScatter=useSmoothScatter, pch=pch, cex=cex,
                             inset=inset,
                             add=add,
                             debug=debug-1, ...) # FIXME use inset here
                  } else if (which[w] == 4) {
                      textItem<-function(xloc, yloc, item, label, cex=0.8, d.yloc=0.8) {
                          if (!is.null(item) && !is.na(item))
                              text(xloc, yloc, paste(label, item), adj = c(0, 0), cex=cex)
                          yloc - d.yloc
                      }
                      par(mar=c(0,0,0,0))
                      plot.new()
                      plot.window(c(0,10), c(0,10))
                      xloc <- 0
                      yloc <- 8
                      cex <- 3/4
                      xm <- x@metadata
                      yloc <- textItem(xloc, yloc, xm$station,         " Station:  ", cex=cex)
                      if (!is.null(xm$filename) && nchar(xm$filename) > 0) {
                          yloc <- textItem(xloc, yloc, xm$filename,    " File:     ", cex=cex)
                      }
                      if (!is.null(xm$scientist))	{
                          yloc <- textItem(xloc, yloc, xm$scientist,   " Scientist:", cex=cex)
                      }
                      if (!is.null(xm$institute))	{
                          yloc <- textItem(xloc, yloc, xm$institute,   " Institute:", cex=cex)
                      }
                      if (!is.null(xm$date)) {
                          yloc <- textItem(xloc, yloc, xm$date,        " Date:     ", cex=cex)
                      }
                      if (!is.null(xm$ship)) {
                          yloc <- textItem(xloc, yloc, xm$ship,        " Ship:     ", cex=cex)
                      }
                      if (!is.null(xm$cruise)) {
                          yloc <- textItem(xloc, yloc, xm$cruise,      " Cruise:   ", cex=cex)
                      }
                      if (!is.null(xm$station)) {
                          yloc <- textItem(xloc, yloc, xm$station,     " Station:  ", cex=cex)
                      }
                      if (!is.null(xm$waterDepth)) {
                          yloc <- textItem(xloc, yloc, xm$waterDepth, " Depth:    ", cex=cex)
                      }
                      if (!is.na(xm$longitude) && !is.na(xm$latitude)) {
                          yloc <- textItem(xloc, yloc, latlonFormat(xm$latitude, xm$longitude),   " Location: ", cex=cex)
                      }
                      ## if (!is.na(ref.lat) && !is.na(ref.lon)) {
                      ##     ##dist <- geodDist(xm$longitude, xm$latitude, ref.lon, ref.lat)
                      ##     ##kms <- sprintf("%.2f km", dist/1000)
                      ##     ##rlat <- text(xloc, yloc, paste(" Distance to (", dec_deg(ref.lon),
                      ##     ##                               ",", dec_deg(ref.lat), ") = ", kms), adj = c(0, 0), cex=cex)
                      ##     yloc <- yloc - d.yloc
                      ## }
                  } else if (which[w] == 5) { # map
                      if (!is.null(x[["latitude"]]) &&
                          !is.null(x[["longitude"]]) &&
                          is.finite(x[["latitude"]][1]) &&
                          is.finite(x[["longitude"]][1])) {
                          oceDebug(debug, "plot(ctd, ...) { # of type MAP\n")
                          ## Calculate span, if not given
                          if (missing(span)) {
                              if (requireNamespace("ocedata", quietly=TRUE)) {
                                  data("coastlineWorldMedium", package="ocedata", envir=environment())
                                  mcoastline <- get("coastlineWorldMedium")
                                  d <- geodDist(mcoastline[['longitude']],
                                                mcoastline[['latitude']],
                                                mean(x[['longitude']], na.rm=TRUE),
                                                mean(x[['latitude']], na.rm=TRUE))
                                  rm(mcoastline)
                              } else {
                                  data("coastlineWorld", package="oce", envir=environment())
                                  mcoastline <- get("coastlineWorld")
                                  d <- geodDist(mcoastline[['longitude']],
                                                mcoastline[['latitude']],
                                                mean(x[['longitude']], na.rm=TRUE),
                                                mean(x[['latitude']], na.rm=TRUE))
                              }
                              ## Previously, used nearest 20 points, but that requires sorting a
                              ## possibly very long vector. Note the check on the result
                              ## of bound125(), which is a new function
                              nearest <- min(d, na.rm=TRUE)
                              span <- bound125(5 * nearest)
                              if (span < 5 * nearest)
                                  span <- 5 * nearest # safety check
                              oceDebug(debug, "span not given; nearest land ", round(nearest,0),
                                       "km, so set span=", round(span,0), "\n")
                          }
                          ## the "non-projection" case is terrible up north (FIXME: prob should not do this)
                          if (!missing(projection) && !is.na(pmatch(projection, "automatic"))) {
                              meanlon <- mean(x[["longitude"]], na.rm=TRUE)
                              meanlat <- mean(x[["latitude"]], na.rm=TRUE)
                              projection <- if (meanlat > 70)
                                  paste("+proj=stere +lon_0=", meanlon, sep="") else "+proj=merc"
                              oceDebug(debug, "using", projection, "projection (chosen automatically)\n")
                          } else {
                              oceDebug(debug, "using", projection, "projection (specified)\n")
                          }
                          ##message("projection:", projection)
                          oceDebug(debug, "projection=", if (is.null(projection)) "NULL" else projection, ", span=", span, "km\n")
                          if (is.character(coastline)) {
                              oceDebug(debug, "coastline is a string: \"", coastline, "\"\n", sep="")
                              if (requireNamespace("ocedata", quietly=TRUE)) {
                                  library(ocedata) # FIXME: is this needed?
                                  if (coastline == "best") {
                                      bestcoastline <- coastlineBest(span=span)
                                      oceDebug(debug, "'best' coastline is: \"", bestcoastline, '\"\n', sep="")
                                      if (bestcoastline == "coastlineWorld")
                                          data(list=bestcoastline, package="oce", envir=environment())
                                      else
                                          data(list=bestcoastline, package="ocedata", envir=environment())
                                      coastline <- get(bestcoastline)
                                  } else if (coastline == "coastlineWorld") {
                                      oceDebug(debug, "using 'coastlineWorld'\n")
                                      data("coastlineWorld", package="oce", envir=environment())
                                      coastline <- get("coastlineWorld")
                                  } else if (coastline == "coastlineWorldFine") {
                                      oceDebug(debug, "using 'coastlineWorldFine'\n")
                                      data("coastlineWorldFine", package="ocedata", envir=environment())
                                      coastline <- get("coastlineWorldFine")
                                  } else if (coastline == "coastlineWorldMedium") {
                                      oceDebug(debug, "using 'coastlineWorldMedium'\n")
                                      data("coastlineWorldMedium", package="ocedata", envir=environment())
                                      coastline <- get("coastlineWorldMedium")
                                  }  else {
                                      stop("there is no built-in coastline file of name \"", coastline, "\"")
                                  }
                              } else {
                                  warning("CTD plots will have better coastlines after doing install.packages(\"ocedata\")", call.=FALSE)
                                  data("coastlineWorld", package="oce", envir=environment())
                                  coastline <- get("coastlineWorld")
                              }
                          }
                          if (missing(lonlim)) {
                              mlon <- mean(x[["longitude"]], na.rm=TRUE)
                              lonlim.c <- mlon + c(-1, 1) * min(abs(range(coastline[["longitude"]], na.rm=TRUE) - mlon))
                              clon <- mean(lonlim.c)
                              if (missing(latlim)) {
                                  mlat <- mean(x[["latitude"]], na.rm=TRUE)
                                  oceDebug(debug, "CASE 1: both latlim and lonlim missing; using projection=",
                                           if (is.null(projection)) "NULL" else projection, "\n")
                                  latlim.c <- mlat + c(-1, 1) * min(abs(range(coastline[["latitude"]],na.rm=TRUE) - mlat))
                                  latlim.c <- ifelse(latlim.c > 90, 89.99, latlim.c)
                                  oceDebug(debug, "about to plot coastline\n")
                                  oceDebug(debug, "clatitude=", mean(latlim.c), "\n")
                                  oceDebug(debug, "clongitude=", clon, "\n")
                                  oceDebug(debug, "span=", span, "\n")
                                  oceDebug(debug, "projection=", projection, "\n")
                                  oceDebug(debug, "parameters=", parameters, "\n")
                                  oceDebug(debug, "orientation=", orientation, "\n")
                                  oceDebug(debug, "ok, about to call plot(coastline)\n")
                                  plot(coastline,
                                       clongitude=standardizeLongitude(clon), clatitude=mean(latlim.c), span=span,
                                       projection=projection, parameters=parameters, orientation=orientation,
                                       border=borderCoastline, col=colCoastline,
                                       mgp=mgp, mar=mar, inset=inset, cex.axis=cex.axis,
                                       lonlabel=lonlabel, latlabel=latlabel, sides=sides,
                                       debug=debug-1)
                                  oceDebug(debug, " ... did plot(coastline)\n")
                              } else {
                                  oceDebug(debug, "CASE 2: latlim given, lonlim missing\n")
                                  clat <- mean(latlim)
                                  plot(coastline,
                                       clongitude=standardizeLongitude(clon), clatitude=clat, span=span,
                                       projection=projection, parameters=parameters, orientation=orientation,
                                       border=borderCoastline, col=colCoastline,
                                       mgp=mgp, mar=mar, inset=inset, cex.axis=cex.axis,
                                       lonlabel=lonlabel, latlabel=latlabel, sides=sides,
                                       debug=debug-1)
                              }
                              if (is.numeric(which[w]) && round(which[w],1) == 5.1) # HIDDEN FEATURE
                                  mtext(gsub(".*/", "", x@metadata$filename), side=3, line=0.1, cex=0.7*cex)
                          } else {
                              oceDebug(debug, "lonlim was provided\n")
                              clon <- mean(lonlim)
                              if (missing(latlim)) {
                                  oceDebug(debug, "CASE 3: lonlim given, latlim missing\n")
                                  latlim.c <- mean(x@metadata$latitude, na.rm=TRUE) + c(-1, 1) * min(abs(range(coastline[["latitude"]],na.rm=TRUE) - x@metadata$latitude))
                                  clat <- mean(latlim.c)
                                  plot(coastline,
                                       clongitude=standardizeLongitude(clon), clatitude=clat, span=span,
                                       projection=projection, parameters=parameters, orientation=orientation,
                                       border=borderCoastline, col=colCoastline,
                                       mgp=mgp, mar=mar, inset=inset, cex.axis=cex.axis,
                                       lonlabel=lonlabel, latlabel=latlabel, sides=sides,
                                       debug=debug-1)
                              } else {
                                  oceDebug(debug, "CASE 4: both latlim and lonlim given\n")
                                  clat <- mean(latlim)
                                  plot(coastline,
                                       clongitude=standardizeLongitude(clon), clatitude=clat, span=span,
                                       border=borderCoastline, col=colCoastline,
                                       projection=projection, parameters=parameters, orientation=orientation,
                                       mgp=mgp, mar=mar, inset=inset, cex.axis=cex.axis,
                                       lonlabel=lonlabel, latlabel=latlabel, sides=sides,
                                       debug=debug-1)
                              }
                          }
                          if (is.null(projection)) {
                              points(standardizeLongitude(x[["longitude"]]), x[["latitude"]],
                                     cex=latlon.cex, col=latlon.col, pch=latlon.pch)
                          } else {
                              mapScalebar()
                              mapPoints(x[["longitude"]], x[["latitude"]],
                                     cex=latlon.cex, col=latlon.col, pch=latlon.pch)
                          }
                          if (!is.null(x@metadata$station) && !is.na(x@metadata$station))
                              mtext(x@metadata$station,
                                    side=3, adj=0, cex=0.8*par("cex"), line=1.125)
                          if (!is.null(x@metadata$startTime) && 4 < nchar(x@metadata$startTime))
                              mtext(format(x@metadata$startTime, "%Y-%m-%d %H:%S"),
                                    side=3, adj=1, cex=0.8*par("cex"), line=1.125)
                      }
                      oceDebug(debug, "} # plot(ctd, ...) of type \"map\"\n", unindent=1)
                  } else if (which[w] ==30) { # S timeseries
                      oce.plot.ts(x[["time"]], x[["salinity"]], ylab=resizableLabel("S", "y"))
                  } else if (which[w] ==31) { # T timeseries
                      oce.plot.ts(x[["time"]], x[["temperature"]], ylab=resizableLabel("T", "y"))
                  } else if (which[w] ==32) { # p timeseries
                      oce.plot.ts(x[["time"]], x[["pressure"]], ylab=resizableLabel("p", "y"))
                  } else if (which[w] ==33) { # sigmaTheta timeseries
                      oce.plot.ts(x[["time"]], x[["sigmaTheta"]], ylab=resizableLabel("sigmaTheta", "y"))
                  } else {
                      stop("unknown value of which, ", which[w])
                  }
                  if (w <= adorn.length && nchar(adorn[w]) > 0) {
                      t <- try(eval(adorn[w]), silent=TRUE)
                      if (class(t) == "try-error")
                          warning("cannot evaluate adorn[", w, "]\n")
                  }
              }
              oceDebug(debug, "} # plot,ctd-method()\n", unindent=1)
              invisible()
          })


#' Subset a CTD object
#'
#' This function is somewhat analogous to
#' \code{\link{subset.data.frame}}, but only one independent variable may be
#' used in \code{subset} in any call to the function, which means that
#' repeated calls will be necessary to subset based on more than one
#' independent variable (e.g. time and distance).
#'
#' @param x An object inheriting from \code{\link{ctd-class}}.
#' @param subset An expression indicating how to subset \code{x}.
#' @param ... Ignored.
#' @return A \code{ctd} object.
#' @examples
#' library(oce)
#' data(ctd)
#' plot(ctd)
#' plot(subset(ctd, pressure<10))
#'
#' @author Dan Kelley
#'
#' @family things related to \code{ctd} data
setMethod(f="subset",
          signature="ctd",
          definition=function(x, subset, ...) {
              res <- new("ctd") # start afresh in case x@data is a data.frame
              res@metadata <- x@metadata
              res@processingLog <- x@processingLog
              for (i in seq_along(x@data)) {
                  r <- eval(substitute(subset), x@data, parent.frame(2))
                  r <- r & !is.na(r)
                  res@data[[i]] <- x@data[[i]][r]
              }
              names(res@data) <- names(x@data)
              subsetString <- paste(deparse(substitute(subset)), collapse=" ")
              res@processingLog <- processingLogAppend(res@processingLog, paste("subset.ctd(x, subset=", subsetString, ")", sep=""))
              res
          })


#' Plot seawater data in a low-level fashion
#' 
#' Plot CTD data as time-series against scan number, to help with trimming
#' extraneous data from a CTD cast.
#' 
#' @param x A \code{ctd} object, i.e. inheriting from \code{\link{ctd-class}}.
#' 
#' @param which Numerical vector numerical codes specifying the panels to draw: 1
#' for pressure vs scan, 2 for \code{diff(pressure)} vs scan, 3 for temperature vs
#' scan, and 4 for salinity vs scan.
#' 
#' @param xtype Character string indicating variable for the x axis. May be
#' \code{"scan"} (the default) or \code{"time"}. In the former case, a
#' \code{scan} variable will be created using \code{\link{seq_along}},
#' if necessary. In the latter case, an error results if the \code{data}
#' slot of \code{x} lacks a variable called \code{time}.
#'
#' @param type Line type.
#' 
#' @param mgp Three-element numerical vector to use for \code{par(mgp)}, and also
#' for \code{par(mar)}, computed from this.  The default is tighter than the R
#' default, in order to use more space for the data and less for the axes.
#' 
#' @param mar Four-element vector be used with \code{\link{par}("mar")}.  If set
#' to \code{NULL}, then \code{par("mar")} is used.  A good choice for a TS diagram
#' with a palette to the right is \code{mar=par("mar")+c(0, 0, 0, 1))}.
#' 
#' @param ... Optional arguments passed to plotting functions.
#' 
#' @examples
#' library(oce)
#' data(ctdRaw) 
#' plotScan(ctdRaw)
#' abline(v=c(130, 350), col='red') # useful for ctdTrim()
#' 
#' @author Dan Kelley
#' @family functions that plot \code{oce} data
#' @family things related to \code{ctd} data
plotScan <- function(x, which=1, xtype="scan",
                     type='l', mgp=getOption("oceMgp"), mar=c(mgp[1]+1.5,mgp[1]+1.5,mgp[1],mgp[1]), ...)
{
    if (!inherits(x, "ctd"))
        stop("method is only for objects of class '", "ctd", "'")
    nw <- length(which)
    if (nw > 1)
        par(mfrow=c(nw,1))
    par(mar=mar)
    par(mgp=mgp)
    xtype <- match.arg(xtype, c("scan", "time"))
    for (w in which) {
        if (xtype == "scan") {
            xvar <- if (("scan" %in% names(x@data))) x[["scan"]] else seq_along(x@data$pressure)
            if (w == 1) {
                plot(xvar, x@data$pressure, xlab="Scan", ylab=resizableLabel("p", "y"),
                     yaxs='r', type=type, ...)
            } else if (w == 2) {
                plot(xvar[-1], diff(x@data$pressure), xlab="Scan", ylab="diff(pressure)",
                     yaxs='r', type=type, ...)
            } else if (w == 3) {
                plot(xvar, x[["temperature"]], xlab="Scan", ylab=resizableLabel("T", "y"),
                     yaxs='r', type=type, ...)
            } else if (w == 4) {
                plot(xvar, x[["salinity"]], xlab="Scan", ylab=resizableLabel("S", "y"),
                     yaxs='r', type=type, ...)
            } else {
                stop("unknown 'which'; must be in 1:4")
            }
        } else if (xtype == "time") {
            if (!("time" %in% names(x@data)))
                stop("there is no 'time' in this ctd object")
            if (w == 1) {
                oce.plot.ts(x@data$time, x@data$pressure, ylab=resizableLabel("p", "y"),
                            yaxs='r', type=type, ...)
            } else if (w == 2) {
                oce.plot.ts(x@data$time[-1], diff(x@data$pressure), ylab="diff(pressure)",
                            yaxs='r', type=type, ...)
            } else if (w == 3) {
                oce.plot.ts(x@data$time, x[["temperature"]], ylab=resizableLabel("T", "y"),
                            yaxs='r', type=type, ...)
            } else if (w == 4) {
                oce.plot.ts(x@data$time, x[["salinity"]], ylab=resizableLabel("S", "y"),
                            yaxs='r', type=type, ...)
            } else {
                stop("unknown 'which'; must be in 1:4")
            }
        }
    }
}

#' Read a general CTD file
#' @template readCtdTemplate
#' @param type If \code{NULL}, then the first line is studied, in order to
#' determine the file type.  If \code{type="SBE19"}, then a \emph{Seabird 19}, or
#' similar, CTD format is assumed. If \code{type="WOCE"} then a WOCE-exchange file
#' is assumed.  If \code{type="ITP"} then an ice-tethered profiler file is
#' assumed.  If \code{type="ODF"} an ODF file is assumed.  If \code{type="ODV"} an
#' ascii-ODV file is assumed.
#'
#' @details
#' \code{read.ctd()} is a base function that in turn calls specialized functions, e.g.
#' \code{\link{read.ctd.sbe}} for SBE data files.
#'
#' @seealso Other functions that read CTD data:
#' \code{\link{read.ctd.itp}} for ice-tethered-profiler format,
#' \code{\link{read.ctd.odf}} for ODF format,
#' \code{\link{read.ctd.odv}} for ODV format,
#' \code{\link{read.ctd.sbe}} for SBE format, and
#' \code{\link{read.ctd.woce}} for WOCE format.
read.ctd <- function(file, type=NULL, columns=NULL, station=NULL, missing.value=-999,
                     monitor=FALSE, debug=getOption("oceDebug"), processingLog, ...)
{
    ## Special case: ruskin files are handled by read.rsk()
    if (is.character(file) && length(grep(".rsk$",file))) {
        return(read.rsk(file=file, debug=debug))
    }

    if (missing(processingLog)) processingLog <- paste(deparse(match.call()), sep="", collapse="")
    ##ofile <- file
    filename <- NULL
    if (is.null(type)) {
        if (is.character(file)) {
            if (length(grep(".rsk$",file))) {
                return(read.rsk(file=file, debug=debug))
            }
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
        line <- scan(file, what='char', sep="\n", n=1, quiet=TRUE) # slow, but just one line
        pushBack(line, file)
        ## FIXME: detect ODV type in first or second line; see oce.magic().
        if ("CTD" == substr(line, 1, 3)) {
            type <- "WOCE"
        } else if ("* Sea-Bird" == substr(line, 1, 10)) {
            type <- "SBE19"
        } else {
            stop("Cannot discover type in line '", line, "'\n")
        }
    } else {
        if (!is.na(pmatch(type, "SBE19"))) {
            type <- "SBE19"
        } else if (!is.na(pmatch(type, "WOCE"))) {
            type <- "WOCE"
        } else {
            stop("type must be SBE19, WOCE, ODF, ODV, or ITP, not ", type)
        }
    }                                   # FIXME: should just use oce.magic() here
    res <- switch(type,
                   SBE19 = read.ctd.sbe(file, columns=columns, station=station,
                                        missing.value=missing.value, monitor=monitor,
                                        debug=debug, processingLog=processingLog, ...),
                   WOCE  = read.ctd.woce(file, columns=columns, station=station,
                                         missing.value=missing.value, monitor=monitor,
                                         debug=debug, processingLog=processingLog, ...),
                   ODF = read.ctd.odf(file, columns=columns, station=station,
                                      missing.value=missing.value, monitor=monitor,
                                      debug=debug, processingLog=processingLog, ...),
                   ODV = read.ctd.odv(file, columns=columns, station=station,
                                      missing.value=missing.value, monitor=monitor,
                                      debug=debug, processingLog=processingLog, ...),
                   ITP = read.ctd.itp(file, columns=columns, station=station,
                                      missing.value=missing.value, monitor=monitor,
                                      debug=debug, processingLog=processingLog, ...))
    ## water depth is sometimes zero, which is a hassle in section plots, so make a guess
    #if (!"waterDepth" %in% names(res@metadata)) # may be entirely missing
    #    res@metadata$waterDepth <- max(res@data$pressure, na.rm=TRUE)
    #if (res@metadata$waterDepth < 1)   # may be silly
    #    res@metadata$waterDepth <- max(res@data$pressure, na.rm=TRUE)
    res
}

#' Translate WOCE data names to \code{oce} data names
#'
#' Translate WOCE-style names to \code{oce} names, using \code{\link{gsub}}
#' to match patterns. For example, the pattern \code{"CTDOXY.*"} is taken
#' to mean \code{oxygen}.
#'
#' @param names vector of strings holding WOCE-style names.
#'
#' @return vector of strings holding \code{oce}-style names.
#' @author Dan Kelley
woceNames2oceNames <- function(names)
{
    ## FIXME: this almost certainly needs a lot more translations. The next comment lists some that
    ## FIXME: I've seen. But how are we to know, definitively? It would be great to find an official
    ## FIXME: list, partly because the present function should be documented, and that documentation
    ## FIXME: should list a source.
    ## SAMPNO,BTLNBR,BTLNBR_FLAG_W,DATE,TIME,LATITUDE,LONGITUDE,DEPTH,CTDPRS,CTDTMP,CTDSAL,CTDSAL_FLAG_W,SALNTY,SALNTY_FLAG_W,OXYGEN,OXYGEN_FLAG_W,SILCAT,SILCAT_FLAG_W,NITRIT,NITRIT_FLAG_W,NO2+NO3,NO2+NO3_FLAG_W,PHSPHT,PHSPHT_FLAG_W
    names <- gsub("_FLAG_W", "Flag", names)
    names <- gsub("CTDOXY", "oxygen", names)
    names <- gsub("CTDPRS", "pressure", names)
    names <- gsub("CTDSAL", "salinity", names)
    names <- gsub("CTDTMP", "temperature", names)
    names <- gsub("OXYGEN", "oxygen", names)
    names <- gsub("SALNTY", "salinityBottle", names)
    names <- gsub("SILCAT", "silicate", names)
    names <- gsub("NITRIT", "nitrite", names)
    names <- gsub("NO2+NO3", "nitrite+nitrate", names)
    names <- gsub("PHSPHT", "phosphate", names)
    names
}

#' Read a WOCE-type CTD file in which the first word is "CTD"
#' @template readCtdTemplate
#'
#' @details
#' \code{read.ctd.woce()} reads files stored in the exchange format used
#' by the World Ocean Circulation Experiment (WOCE), in which the first 4
#' characters are ``\code{CTD,}''. It also also in a rarer format with
#' the first 3 characters are \code{CTD}'' followed by a blank or the end
#' of the line.
#'
#' @references
#' The WOCE-exchange format is described at
#' \code{http://woce.nodc.noaa.gov/woce_v3/wocedata_1/whp/exchange/exchange_format_desc.htm},
#' and a sample file is at
#' \url{http://woce.nodc.noaa.gov/woce_v3/wocedata_1/whp/exchange/example_ct1.csv}
read.ctd.woce <- function(file, columns=NULL, station=NULL, missing.value=-999, monitor=FALSE,
                          debug=getOption("oceDebug"), processingLog, ...)
{
    if (length(grep("\\*", file))) {
        oceDebug(debug, "read.ctd.woce(file=\"", file, "\") { # will read a series of files\n", unindent=1)
        files <- list.files(pattern=file)
        nfiles <- length(files)
        if (monitor)
            pb <- txtProgressBar(1, nfiles, style=3)
        res <- vector("list", nfiles)
        for (i in 1:nfiles) {
            res[[i]] <- read.ctd.woce(files[i], debug=debug-1)
            if (monitor)
                setTxtProgressBar(pb, i)
        }
        oceDebug(debug, "} # read.ctd.woce() {\n")
        return(res)
    }
    ## FIXME: should have an argument that selects CTDSAL or SALNTY
    oceDebug(debug, "read.ctd.woce(file=\"", file, "\", ..., debug=", debug, ", ...) {\n", sep="", unindent=1)
    if (is.character(file)) {
        filename <- fullFilename(file)
        file <- file(file, "r")
        on.exit(close(file))
    } else {
        filename <- ""
    }
    if (!inherits(file, "connection"))
        stop("argument `file' must be a character string or connection")
    if (!isOpen(file)) {
        open(file, "r")
        on.exit(close(file))
    }
    res <- new("ctd", pressureType="sea")
    ## Header
    scientist <- ship <- institute <- address <- NULL
    filename.orig <- NULL
    sampleInterval <- NaN
    systemUploadTime <- NULL
    latitude <- longitude <- NaN
    startTime <- NULL
    waterDepth <- NA
    date <- recovery <- NULL
    header <- c()
    ##col.names.inferred <- NULL
    ##conductivity.standard <- 4.2914
    ## http://www.nodc.noaa.gov/woce_V2/disk02/exchange/exchange_format_desc.htm
    ## First line
    line <- scan(file, what='char', sep="\n", n=1, quiet=TRUE)
    oceDebug(debug, paste("examining header line '",line,"'\n", sep=""))
    header <- line
    waterDepthWarning <- FALSE

    ## Handle a format used in a 2003 survey of the Canada Basin
    if (substr(line, 1, 3) == "CTD" && substr(line, 4, 4) != ",")  {
        oceDebug(debug, "WOCE-like style used in a 2003 survey of the Arctic Canada Basin\n")
        ##CTD
        ##CRUISE NAME = LSSL 2003-21
        ##AREA = Arctic Ocean, Canada Basin
        ##SHIP = CCGS Louis S St.Laurent
        ##CASTNO = 1
        ##DATE = 11-Aug-2003
        ##LATITUDE (N)= 71.391
        ##LONGITUDE (W)= 134.001
        ##Pressure,Temperature,Salinity,Oxygen,Fluorescence,Transmission
        ##   DB   ,ITS-90 DEGC,   PSU  , ML/L ,     UG/L   ,      %
        ##         1,   -1.1999,   28.4279,      8.77,     0.026,    87.679
        lines <- readLines(file)
        oceDebug(debug, "file has", length(lines), "lines\n")
        headerEnd <- grep("[ ]*DB[ ]*,", lines)
        if (is.na(headerEnd))
            stop("cannot decode the header in this CTD file")
        header <- lines[1:headerEnd]
        oceDebug(debug, "headerEnd:", headerEnd, "\n")
        names <- c("pressure", "temperature", "salinity", "oxygen", "fluorescence", "transmission") # may get updated
        for (i in seq_along(header)) {
            if (length(grep("CRUISE", header[i], ignore.case=TRUE))) {
                cruise<- sub("CRUISE[ ]*NAME[ ]*=[ ]*", "", header[i], ignore.case=TRUE)
                cruise <- sub("[ ]*$", "", cruise)
            } else if (length(grep("SHIP", header[i], ignore.case=TRUE))) {
                ship <- header[i]
                ship <- sub("^[ ]*SHIP[ ]*=[ ]*", "", ship, ignore.case=TRUE)
                ship <- sub(" *$", "", ship)
            } else if (length(grep("CASTNO", header[i], ignore.case=TRUE))) {
                station <- sub("[ ]*$", "", sub("CASTNO[ ]*=[ ]*", "", header[i]))
            } else if (length(grep("^[ ]*Pressure,", header[i]))) {
                names <- strsplit(gsub(" *$", "", tolower(header[i])), ",")[[1]]
            } else if (length(grep("LATITUDE", header[i]))) {
                latitude <- as.numeric(sub("LATITUDE.*=[ ]*", "", header[i]))
                if (length(grep(".*S.*", header[i], ignore.case=TRUE)))
                    latitude <- -latitude
            } else if (length(grep("LONGITUDE", header[i]))) {
                longitude <- as.numeric(sub("LONGITUDE.*=[ ]*", "", header[i]))
                if (length(grep(".*W.*", header[i], ignore.case=TRUE)))
                    longitude <- -longitude
            } else if (length(grep("DATE", header[i]))) {
                date <- decodeTime(sub("[ ]*$", "", sub("[ ]*DATE[ ]*=[ ]*", "", header[i])), "%d-%b-%Y") # e.g. 01-Jul-2013 Canada Day
            }
        }
        dataLines <- lines[seq.int(headerEnd+1, length(lines)-1)]
        data <- as.list(read.table(textConnection(dataLines), header=FALSE, sep=",", col.names=names))
        res@metadata$header <- header
        res@metadata$filename <- filename # provided to this routine
        res@metadata$filename.orig <- filename.orig # from instrument
        res@metadata$systemUploadTime <- systemUploadTime
        res@metadata$units <- list(temperature=list(unit=expression(degree*C), scale="ITS-90"),
                                   salinity=list(unit=expression(), scale="PSS-78"),
                                   conductivity=list(unit=expression(ratio), scale=""))
        res@metadata$pressureType <- "sea"
        res@metadata$ship <- ship
        res@metadata$scientist <- scientist
        res@metadata$institute <- institute
        res@metadata$address <- address
        res@metadata$cruise <- NULL
        res@metadata$station <- station
        res@metadata$deploymentType <- "unknown"
        res@metadata$date <- date
        res@metadata$startTime <- startTime
        res@metadata$latitude <- latitude
        res@metadata$longitude <- longitude
        res@metadata$recovery <- recovery
        res@metadata$waterDepth <- max(abs(data$pressure), na.rm=TRUE) # not in header
        res@metadata$sampleInterval <- sampleInterval
        res@metadata$names <- names
        res@metadata$labels <- labels
        res@metadata$src <- filename
    } else {                           # CTD, 20000718WHPOSIOSCD
        tmp <- sub("(.*), ", "", line)
        date <- substr(tmp, 1, 8)
        ##cat("DATE '", date, "'\n", sep="")
        diw <- substr(tmp, 9, nchar(tmp)) # really, divisionINSTITUTEwho
        institute <- diw # BUG: really, it is division, institute, who, strung together
        ## Kludge: recognize some institutes
        if (0 < regexpr("SIO", diw))
            institute <- "SIO"
        gotHeader <- FALSE
        while (TRUE) {
            line <- scan(file, what='char', sep="\n", n=1, quiet=TRUE) # slow, for perhaps 20 lines of header
            oceDebug(debug, paste("examining header line '",line,"'\n"))
            if ((0 < (r<-regexpr("FILE_NAME", line)))) {
                ##  #CTDFILE_NAME:     KB51D003.WCT
                oceDebug(debug, "infer filename from:", line, "\n")
                filename.orig <- sub("^.*NAME:[ ]*", "", line)
                oceDebug(debug, " trim to '", filename.orig, "'\n", sep='')
                filename.orig <- sub("[ ]*$", "", filename.orig)
                oceDebug(debug, " trim to '", filename.orig, "'\n", sep='')
            }
            header <- c(header, line)
            ## SAMPLE:
            ##      EXPOCODE = 31WTTUNES_3
            ##      SECTION_ID = P16C
            ##      STNNBR = 221
            ##      CAST = 1
            ##      DATE = 19910901
            ##      TIME = 0817
            ##      LATITUDE = -17.5053
            ##      LONGITUDE = -150.4812
            ##      BOTTOM = 3600
            if (!(0 < (r<-regexpr("^[ ]*#", line)[1]))) { # first non-hash line
                ## NUMBER_HEADERS = 10
                nh <- as.numeric(sub("(.*)NUMBER_HEADERS = ", "", ignore.case=TRUE, line))
                if (is.finite(nh)) {
                    for (i in 2:nh) {
                        line <- scan(file, what='char', sep="\n", n=1, quiet=TRUE)
                        header <- c(header, line)
                        oceDebug(debug, line, "\n")
                        if ((0 < (r<-regexpr("LATITUDE",  line))))
                            latitude  <- as.numeric(sub("[a-zA-Z =]*","", line))
                        else if ((0 < (r<-regexpr("LONGITUDE", line))))
                            longitude <- as.numeric(sub("(.*) =","", line))
                        else if ((0 < (r<-regexpr("DATE", line))))
                            date <- decodeTime(sub(" *$", "", sub("[ ]*DATE[ ]*=[ ]*", "", line)), "%Y%m%d") # e.g. 20130701 Canada Day
                        else if ((0 < (r<-regexpr(pattern="DEPTH", text=line, ignore.case=TRUE))))
                            waterDepth <- as.numeric(sub("[a-zA-Z =:]*","", line))
                        else if ((0 < (r<-regexpr(pattern="Profondeur", text=line, ignore.case=TRUE))))
                            waterDepth <- as.numeric(sub("[a-zA-Z =]*","", line))
                        else if ((0 < (r<-regexpr(pattern="STNNBR", text=line, ignore.case=TRUE))))
                            station <- as.numeric(sub("[a-zA-Z =]*","", line))
                        else if ((0 < (r<-regexpr(pattern="Station", text=line, ignore.case=TRUE))))
                            station <- as.numeric(sub("[a-zA-Z =]*","", line))
                        else if ((0 < (r<-regexpr(pattern="Mission", text=line, ignore.case=TRUE))))
                            scientist <- sub("[ ]*$", "", sub(".*:", "", line))
                    }
                    break
                } else {
                    gotHeader <- TRUE
                    break
                }
            }
        }
        if (!gotHeader) {
            while (TRUE) {                    # catch any remaining "#" lines
                line <- scan(file, what='char', sep="\n", n=1, quiet=TRUE)
                if (!(0 < (r<-regexpr("^#", line))))
                    break
                header <- c(header, line)
            }
        }
        ## 2 more header lines, one giving quantities, the next units, e.g.
        ## EXPOCODE,SECT_ID,STNNBR,CASTNO,SAMPNO,BTLNBR,BTLNBR_FLAG_W,DATE,TIME,LATITUDE,LONGITUDE,DEPTH,CTDPRS,CTDTMP,CTDSAL,CTDSAL_FLAG_W,SALNTY,SALNTY_FLAG_W,OXYGEN,OXYGEN_FLAG_W,SILCAT,SILCAT_FLAG_W,NITRIT,NITRIT_FLAG_W,NO2+NO3,NO2+NO3_FLAG_W,PHSPHT,PHSPHT_FLAG_W
        ## ,,,,,,,,,,,,DBAR,IPTS-68,PSS-78,,PSS-78,,UMOL/KG,,UMOL/KG,,UMOL/KG,,UMOL/KG,,UMOL/KG,
        varNames <- strsplit(line, split=",")[[1]]
        oceDebug(debug, "varNames: ", paste(varNames, sep=" "), "\n")
        oceDebug(debug, "oce names: ", paste(woceNames2oceNames(varNames), sep=" "), "\n")

        varNames <- gsub("^ *", "", gsub(" *$", "", varNames)) # trim whitespace
        ## catch some typos that have occured in files processed by oce
        oceDebug(debug, paste("before trying to correct typos, varNames=c(\"", paste(varNames, collapse="\", \""), "\")\n", sep=""))
        varNames <- gsub("FLAW", "FLAG", varNames) # Meteor39/4 cruise in Lab Sea had CTDSAL_FLAW_W for all 248 stations
        oceDebug(debug, paste("after trying to correct typos, varNames=c(\"", paste(varNames, collapse="\", \""), "\")\n", sep=""))
        line <- scan(file, what='char', sep="\n", n=1, quiet=TRUE) # skip the units line
        varUnits <- strsplit(line, split=",")[[1]]
        pcol <- pmatch("CTDPRS", varNames)
        if (is.na(pcol)) {
            pcol <- pmatch("DB", varNames)
            if (is.na(pcol))
                stop("cannot find pressure column in list c(\"", paste(varNames, '","'), "\"); need 'DB' or 'CTDPRS'")
        }
        Scol <- pmatch("CTDSAL", varNames)
        if (is.na(Scol)) {
            Scol <- pmatch("SALNTY", varNames)
            if (is.na(Scol))
                stop("cannot find salinity column in list c(\"", paste(varNames, '","'), "\"); need 'CTDSAL' or 'SALNTY'")
        }
        ## FIXME: use these flags ... they are ignored at present.
        Sflagcol <- pmatch("CTDSAL_FLAG_W", varNames)
        if (is.na(Sflagcol)) {
            Sflagcol <- pmatch("SALNTY_FLAG_W", varNames)
            if (is.na(Sflagcol))
                stop("cannot find salinity-flag column in list c(\"", paste(varNames, '","'), "\"); need 'CTDSAL_FLAG_W' or 'SALNTY_FLAG_W'")
        }
        Tcol <- pmatch("CTDTMP", varNames)
        if (is.na(Tcol))
            stop("cannot find temperature column in list", paste(varNames,","))
        Ocol <- pmatch("CTDOXY", varNames)
        oceDebug(debug, "pcol=", pcol, "Scol=", Scol, "Tcol=", Tcol, "Ocol=", Ocol, "\n")
        line <- scan(file, what='char', sep="\n", n=1, quiet=TRUE)
        varUnits <- strsplit(line, split=",")[[1]]
        lines <- readLines(file)
        ## nlines <- length(lines)
        ## pressure <- vector("numeric", nlines)
        ## temperature <- vector("numeric", nlines)
        ## salinity <- vector("numeric", nlines)
        ## oxygen <- vector("numeric", nlines)
        ## b <- 0
        oceDebug(debug, "pcol:", pcol, ", Scol:", Scol, ", Tcol:", Tcol, ", Ocol:", Ocol, "\n")
        ##m <- matrix(NA, nrow=nlines, ncol=length(varNames))
        ending <- grep("END_DATA", lines)
        if (length(ending) == 1)
            lines <- lines[-ending]
        varNamesOce <- woceNames2oceNames(varNames)
        nonflags <- grep("Flag$",varNamesOce, invert=TRUE)
        flags <- grep("Flag$",varNamesOce)
        dataAndFlags <- read.csv(text=lines, header=FALSE, col.names=woceNames2oceNames(varNames))
        data <- as.list(dataAndFlags[, nonflags])
        flags <- as.list(dataAndFlags[, flags])
        names(flags) <- gsub("Flag", "", names(flags))
        names <- names(data)
        labels <- titleCase(names)
        if (is.na(waterDepth)) {
            waterDepth <- max(abs(data$pressure), na.rm=TRUE)
            waterDepthWarning <- TRUE
        }
        ## catch e.g. -999 sometimes used for water depth's missing value
        if (is.finite(waterDepth) && waterDepth <= 0)
            waterDepth <- NA
        res@metadata$header <- header
        res@metadata$filename <- filename # provided to this routine
        res@metadata$filename.orig <- filename.orig # from instrument
        res@metadata$units <- list(temperature=list(unit=expression(degree*C), scale="ITS-90"),
                                   conductivity=list(unit=expression(ratio), scale=""))
        res@metadata$flags <- flags
        res@metadata$pressureType <- "sea"
        res@metadata$systemUploadTime <- systemUploadTime
        res@metadata$ship <- ship
        res@metadata$scientist <- scientist
        res@metadata$institute <- institute
        res@metadata$address <- address
        res@metadata$cruise <- NULL
        res@metadata$station <- station
        res@metadata$deploymentType <- "unknown"
        res@metadata$date <- date
        res@metadata$startTime <- startTime
        res@metadata$latitude <- latitude
        res@metadata$longitude <- longitude
        res@metadata$recovery <- recovery
        res@metadata$waterDepth <- waterDepth
        res@metadata$sampleInterval <- sampleInterval
        res@metadata$names <- names
        res@metadata$labels <- labels
        res@metadata$src <- filename
    }
    res@data <- data
    if (missing(processingLog))
        processingLog <- paste(deparse(match.call()), sep="", collapse="")
    res@processingLog <- processingLogAppend(res@processingLog, processingLog)
    if (waterDepthWarning)
        res@processingLog <- processingLogAppend(res@processingLog, "inferred water depth from maximum pressure")
    oceDebug(debug, "} # read.ctd.woce()\n" , unindent=1) # FIXME: use S4 for ctd / woce
    res@metadata$units$pressure <- list(unit=expression(dbar), scale="")
    res
}

#' Read a WOCE-type CTD file with first word "EXPOCODE"
#' @template readCtdTemplate
#'
#' @details
#' \code{read.ctd.woce.other()} reads files stored in the exchange format used
#' by the World Ocean Circulation Experiment (WOCE), in which the first
#' word in the file is \code{EXPOCODE}.
read.ctd.woce.other <- function(file, columns=NULL, station=NULL, missing.value=-999, monitor=FALSE,
                                debug=getOption("oceDebug"), processingLog, ...)
{
    ##EXPOCODE 06MT18/1      WHP-ID A1E    DATE 090591
    ##STNNBR    558 CASTNO   1 NO.RECORDS=   83
    ##INSTRUMENT NO. NB3 SAMPLING RATE  31.25 HZ
    ##  CTDPRS  CTDTMP  CTDSAL  CTDOXY  NUMBER  QUALT1
    ##    DBAR  ITS-90  PSS-78 UMOL/KG    OBS.       *
    ## ******* ******* ******* *******               *
    ##     4.0  6.7068 34.7032   327.8      -9    2222
    ##     6.0  6.7059 34.7035   328.1      -9    2222
    ##     8.0  6.6928 34.7041   328.8      -9    2222
    examineHeaderLines <- 10
    header <- readLines(file, n=examineHeaderLines)
    station <- ""
    for (i in 1: examineHeaderLines) {
        if (1 == length(grep("STNNBR.*", header[i]))) {
            station <- gsub(" .*", "", gsub("STNNBR[ ]*", "", header[i]))
        } else if (1 == length(grep(".*DATE.*", header[i]))) {
            date <- gsub(" .*", "", gsub(".*DATE[ ]*", "", header[i]))
            month <- as.numeric(substr(date, 1, 2))
            day <- as.numeric(substr(date, 3, 4))
            year <- 1900 + as.numeric(substr(date, 5, 6))
            date <- ISOdatetime(year,month,day,0,0,0, tz="UTC")
        }
    }
    data <- read.table(file, skip=6, header=FALSE)
    pressure <- data$V1
    temperature <- data$V2
    salinity <- data$V3
    oxygen <- data$V4
    salinity[salinity == missing.value] <- NA
    temperature[temperature == missing.value] <- NA
    pressure[pressure == missing.value] <- NA
    oxygen[oxygen == missing.value] <- NA
    as.ctd(salinity, temperature, pressure, oxygen=oxygen, station=station, date=date)
}

parseLatLon <- function(line, debug=getOption("oceDebug"))
{
    ## The following formats are understood (for, e.g. latitude)
    ## * NMEA Latitude = 47 54.760 N
    ## ** Latitude:      47 53.27 N
    x <- line
    ##positive <- TRUE
    oceDebug(debug, "parseLatLon(\"", line, "\") {\n", sep="")
    oceDebug(debug, "  step 1. \"", x, "\" (as provided)\n", sep="")
    x <- sub("^[ =a-z*:]*", "", x, ignore.case=TRUE)
    oceDebug(debug, "  step 2. \"", x, "\" (now should have no header text or symbols)\n", sep="")
    sign <- 1
    if (length(grep("[sSwW]", line)))
        sign <- -1
    x <- sub("[ =a-z:*]*$", "", x, ignore.case=TRUE) # trim anything not a number
    oceDebug(debug, "  step 3. \"", x, "\" (now should have no trailing text or symbols)\n", sep="")
    ## if single number, it's decimal degrees; if two numbers, degrees and then decimal minutes
    xx <- strsplit(x, '[ \\t]+')[[1]]
    if (1 == length(xx)) {
        res <- as.numeric(xx)
        oceDebug(debug, "  step 4a. \"", res, "\" (inferred from single #, decimal degrees)\n", sep="")
    } else if (2 == length(xx)) {
        res <- as.numeric(xx[1]) + as.numeric(xx[2]) / 60
        oceDebug(debug, "  step 4b. \"", res, "\" (inferred from two #, degrees and decimal minutes)\n", sep="")
    } else {
        ## 2014-06-17 it's annoying to see this error msg
        ##warning("cannot decode latitude or longitude from \"", line, "\"")
        res <- NA
    }
    res <- res * sign
    oceDebug(debug, "} # parseLatLon()\n", unindent=1)
    res
}

time.formats <- c("%b %d %Y %H:%M:%s", "%Y%m%d")


##> #' Read an SBE-type CTD file
##> #' @template readCtdTemplate
##> #'
##> #' @details
##> #' \code{read.ctd.sbe()} reads files stored in Seabird \code{.cnv} format.
##> #'
##> #' @references
##> #' The Sea-Bird SBE 19plus profiler is described at
##> #' \url{http://www.seabird.com/products/spec_sheets/19plusdata.htm}.  Some more
##> #' information is given in the Sea-Bird data-processing manaual
##> #' \url{http://www.seabird.com/old-manuals/Software_Manuals/SBE_Data_Processing/SBEDataProcessing_7.20g.pdf}.
##> read.ctd.sbe <- function(file, columns=NULL, station=NULL, missing.value,
##>                              monitor=FALSE, debug=getOption("oceDebug"), processingLog, ...)
##> {
##>     if (!is.null(columns)) {
##>         columnsNames <- names(columns)
##>         if (!("temperature" %in% columnsNames)) stop("'columns' must contain 'temperature'")
##>         if (!("pressure" %in% columnsNames)) stop("'columns' must contain 'pressure'")
##>         if (!("salinity" %in% columnsNames)) stop("'columns' must contain 'salinity'")
##>         if (3 > length(columns)) stop("'columns' must contain three or more elements")
##>     }
##> 
##>     if (length(grep("\\*", file))) {
##>         oceDebug(debug, "read.ctd.sbe(file=\"", file, "\") { # will read a series of files\n", unindent=1)
##>         files <- list.files(pattern=file)
##>         nfiles <- length(files)
##>         if (monitor)
##>             pb <- txtProgressBar(1, nfiles, style=3)
##>         res <- vector("list", nfiles)
##>         for (i in 1:nfiles) {
##>             res[[i]] <- read.ctd.sbe(files[i], debug=debug-1)
##>             if (monitor)
##>                 setTxtProgressBar(pb, i)
##>         }
##>         oceDebug(debug, "} # read.ctd.sbe() {\n")
##>         return(res)
##>     }
##>     oceDebug(debug, "read.ctd.sbe(file=\"", file, "\") {\n", unindent=1)
##> 
##>     ## Read Seabird data file.  Note on headers: '*' is machine-generated,
##>     ## '**' is a user header, and '#' is a post-processing header.
##>     filename <- ""
##>     if (is.character(file)) {
##>         filename <- fullFilename(file)
##>         file <- file(file, "r")
##>         on.exit(close(file))
##>     }
##>     if (!inherits(file, "connection"))
##>         stop("argument `file' must be a character string or connection")
##>     if (!isOpen(file)) {
##>         open(file, "r")
##>         on.exit(close(file))
##>     }
##>     res <- new("ctd", pressureType="sea")
##>     ## Header
##>     scientist <- ship <- institute <- address <- cruise <- hexfilename <- ""
##>     sampleInterval <- NA
##>     systemUploadTime <- NULL
##>     latitude <- longitude <- NA
##>     startTime <- NULL
##>     waterDepth <- NA
##>     date <- recovery <- NA
##>     header <- c()
##>     col.names.inferred <- NULL
##>     found.scan <- FALSE
##>     found.time <- FALSE
##>     found.pressure <- FALSE
##>     found.depth <- FALSE
##>     ##conductivity.standard <- 4.2914
##>     found.header.latitude <- found.header.longitude <- FALSE
##>     serialNumber <- serialNumberConductivity <- serialNumberTemperature <- ""
##>     conductivityUnit = list(unit=expression(ratio), scale="") # guess; other types are "mS/cm" and "S/m"
##>     temperatureUnit = list(unit=expression(degree*C), scale="ITS-90") # guess; other option is IPTS-68
##>     pressureType = "sea"               # guess; other option is "absolute"
##> 
##>     lines <- readLines(file)
##>     for (iline in seq_along(lines)) {
##>         line <- lines[iline]
##>         #line <- scan(file, what='char', sep="\n", n=1, quiet=TRUE)
##>         oceDebug(debug, "examining header line '",line,"'\n", sep="")
##>         header <- c(header, line)
##>         ##if (length(grep("\*END\*", line))) #BUG# why is this regexp no good (new with R-2.1.0)
##>         aline <- iconv(line, from="UTF-8", to="ASCII", sub="?")
##>         if (length(grep("END", aline, perl=TRUE, useBytes=TRUE))) {
##>             ## Sometimes SBE files have a header line after the *END* line.
##>             iline <- iline + 1
##>             if (length(grep("[a-cf-zA-CF-Z]", lines[iline])))
##>                 iline <- iline + 1
##>             break
##>         }
##>         lline <- tolower(aline)
##>         ## BUG: discovery of column names is brittle to format changes
##>         if (0 < (r <- regexpr("# name ", lline))) {
##>             oceDebug(debug, "lline: '",lline,"'\n",sep="")
##>             tokens <- strsplit(line, split=" ", useBytes=TRUE)
##>             oceDebug(debug, "   successfully tokenized\n")
##>             name <- tokens[[1]][6]
##>             oceDebug(debug, "  name: '",name,"'\n",sep="")
##>             if (0 < regexpr("scan", lline)) {
##>                 name <- "scan"
##>                 found.scan <- TRUE
##>             }
##>             if (0 < regexpr("pressure", lline)) {
##>                 if (0 > regexpr("deg c", lline)) {
##>                     ## ignore "# name 5 = ptempC: Pressure Temperature [deg C]"
##>                     name <- "pressure"
##>                     found.pressure <- TRUE
##>                 }
##>             }
##>             if (0 < regexpr("time", lline)) {
##>                 name <- "time"
##>                 found.time <- TRUE
##>             }
##>             if (0 < regexpr("salinity", lline)) {
##>                 name <- "salinity"
##>                 found.salinity <- TRUE
##>             }
##>             if (0 < regexpr("temperature", lline)) {
##>                 ## ignore "# name 5 = ptempC: Pressure Temperature [deg C]"
##>                 if (0 > regexpr("pressure", lline) && 0 > regexpr("potential", lline)) {
##>                     name <- "temperature"
##>                     found.temperature <- TRUE
##>                     unit <- gsub(":.*","",gsub(".*=[ ]*","", line))
##>                     if (length(grep("68", unit)))
##>                         temperatureUnit <- list(unit=expression(degree*C), scale="IPTS-68")
##>                     else if (length(grep("90", unit)))
##>                         temperatureUnit <- list(unit=expression(degree*C), scale="ITS-90")
##>                     oceDebug(debug, "temperatureUnit: ", temperatureUnit$unit, "(inferred from '", unit, "'\n", sep="")
##>                 }
##>             }
##>             if (0 < regexpr("conductivity", lline)) {
##>                 if (0 < regexpr("ratio", lline)) {
##>                     found.conductivity.ratio <- TRUE;
##>                     name <- "conductivityratio"
##>                     conductivityUnit = list(unit=expression(ratio), scale="")
##>                 } else {
##>                     found.conductivity <- TRUE;
##>                     name <- "conductivity"
##>                     unit <- gsub(":.*","",gsub(".*=[ ]*","", line))
##>                     if (length(grep("S/m", unit)))
##>                         conductivityUnit <- list(unit="S/m", scale="")
##>                     else if (length(grep("mS/cm", unit)))
##>                         conductivityUnit <- list(unit="mS/cm", scale="")
##>                 }
##>             }
##>             if (0 < regexpr("depth", lline) || 0 < regexpr("depSM", lline)) {
##>                 name <- "depth"
##>                 found.depth <- TRUE
##>             }
##>             if (0 < regexpr("fluorometer", lline))
##>                 name <- "fluorometer"
##>             ## Used to have oxygen.temperature and oxygen.current here (why??)
##>             if (0 < regexpr("oxygen", lline))
##>                 name <- "oxygen"
##>             if (0 < regexpr("flag", lline))
##>                 name <- "flag"
##>             if (0 < regexpr("sigma-theta", lline)) {
##>                 name <- "sigmaTheta"
##>                 ##foundSigmaTheta <- TRUE
##>             } else {
##>                 if (0 < regexpr("sigma-t", lline)) {
##>                     name <- "sigmat"
##>                     ##foundSigmaT <- TRUE
##>                 }
##>             }
##>             col.names.inferred <- c(col.names.inferred, name)
##>         }
##>         if (0 < regexpr(".*seacat profiler.*", lline))
##>             serialNumber <- gsub("[ ].*$","",gsub(".*sn[ ]*","",lline))
##>         if (length(grep("^\\* temperature sn", lline)))
##>             serialNumberTemperature <- gsub("^.*=\\s", "", lline)
##>         if (length(grep("^\\* conductivity sn", lline)))
##>             serialNumberConductivity <- gsub("^.*=\\s", "", lline)
##>         if (0 < (r<-regexpr("date:", lline))) {
##>             d <- sub("(.*)date:([ ])*", "", lline)
##>             date <- decodeTime(d, "%Y%m%d") # e.g. 20130701 Canada Day
##>         }
##>         ##* NMEA UTC (Time) = Jul 28 2011  04:17:53
##>         ##* system upload time = jan 26 2010 13:02:57
##>         if (length(grep("^\\* .*time.*=.*$", lline))) {
##>             if (0 == length(grep("real-time sample interval", lline))) {
##>                 d <- sub(".*=", "", lline)
##>                 d <- sub("^ *", "", d)
##>                 d <- sub(" *$", "", d)
##>                 date <- decodeTime(d)
##>             }
##>         }
##>         if (0 < (r<-regexpr("filename", lline)))
##>             hexfilename <- sub("(.*)FileName =([ ])*", "", ignore.case=TRUE, lline)
##>         if (0 < (r<-regexpr("system upload time", lline))) {
##>             d <- sub("([^=]*)[ ]*=[ ]*", "", ignore.case=TRUE, lline)
##>             systemUploadTime <- decodeTime(d)
##>             oceDebug(debug, " systemUploadTime ", format(systemUploadTime), " inferred from substring '", d, "'\n", sep="")
##>         }
##>         ## Styles:
##>         ## * NMEA Latitude = 47 54.760 N
##>         ## ** Latitude:      47 53.27 N
##>         if (!found.header.latitude && (0 < (r<-regexpr("latitude*[0-8]*", lline, ignore.case=TRUE)))) {
##>             latitude <- parseLatLon(lline, debug=debug-1)
##>             found.header.latitude <- TRUE
##>         }
##>         if (!found.header.longitude && (0 < (r<-regexpr("longitude*[0-8]*", lline, ignore.case=TRUE)))) {
##>             longitude <- parseLatLon(lline, debug=debug-1)
##>             found.header.longitude <- TRUE
##>         }
##>         if (0 < (r<-regexpr("start_time =", lline))) {
##>             d <- sub("#[ ]*start_time[ ]*=[ ]*", "", lline)
##>             startTime <- decodeTime(d)
##>             oceDebug(debug, " startTime ", format(startTime), "' inferred from substring '", d, "'\n", sep="")
##>         }
##>         if (0 < (r<-regexpr("ship:", lline))) {
##>             ship <- sub("(.*)ship:([ \t])*", "", ignore.case=TRUE, line) # note: using full string
##>             ship <- sub("[ \t]*$", "", ship)
##>         }
##>         if (0 < (r<-regexpr("scientist:", lline)))
##>             scientist <- sub("(.*)scientist:([ ])*", "", ignore.case=TRUE, line) # full string
##>         if (0 < (r<-regexpr("chef", lline)))
##>             scientist <- sub("(.*):([ ])*", "", ignore.case=TRUE, line) # full string
##>         if (0 < (r<-regexpr("institute:", lline)))
##>             institute <- sub("(.*)institute:([ ])*", "", ignore.case=TRUE, line) # full string
##>         if (0 < (r<-regexpr("address:", lline)))
##>             address <- sub("(.*)address:([ ])*", "", ignore.case=TRUE, line) # full string
##>         if (0 < (r<-regexpr("cruise:", lline))) {
##>             cruise <- sub("(.*)cruise:([ ])*", "", ignore.case=TRUE, line) # full string
##>             cruise <- sub("[ ]*$", "", ignore.case=TRUE, cruise) # full string
##>         }
##>         if (is.null(station)) {
##>             if (0 < (r<-regexpr("station:", lline)))
##>                 station <- sub("[ ]*$", "", sub("(.*)station:([ ])*", "", ignore.case=TRUE, line)) # full string
##>         }
##>         if (0 < (r<-regexpr("recovery:", lline)))
##>             recovery <- sub("(.*)recovery:([ ])*", "", lline)
##>         if (0 < (r<-regexpr("depth", lline))) { # "** Depth (m): 3447 "
##>             look <- sub("[a-z:()]*", "", lline, ignore.case=TRUE)
##>             look <- gsub("^[*a-zA-Z\\(\\) :]*", "", lline, ignore.case=TRUE)
##>             look <- gsub("[ ]*", "", look, ignore.case=TRUE)
##>             oceDebug(debug, " trying to get water depth from '", lline, "', reduced to '", look, "'\n", sep="")
##>             if (!length(grep('[a-zA-Z]', look))) {
##>                 waterDepth<- as.numeric(look)
##>                 oceDebug(debug, "got waterDepth: ", waterDepth, "\n")
##>             }
##>         }
##>         if (0 < (r<-regexpr("water depth:", lline))
##>             || 0 < (r<-regexpr(pattern="profondeur", text=lline))) {
##>             ## Examples from files in use by author:
##>             ##** Profondeur: 76
##>             ##** Water Depth:   40 m
##>             look <- sub("[ ]*$", "", sub(".*:[ ]*", "", lline))
##>             linesplit <- strsplit(look," ")[[1]]
##>             nitems <- length(linesplit)
##>             if (nitems == 1) {
##>                 waterDepth <- as.numeric(linesplit[1])
##>             } else if (nitems == 2) {
##>                 unit <- linesplit[2]
##>                 if (unit == "m") {
##>                     waterDepth <- as.numeric(linesplit[1])
##>                 } else if (unit == "km") {
##>                     waterDepth <- 1000 * as.numeric(linesplit[1])
##>                 } else {
##>                     warning("ignoring unit on water depth '", look, "'")
##>                 }
##>             } else {
##>                 stop("cannot interpret water depth from '", lline, "'")
##>             }
##>         }
##>         if (0 < (r<-regexpr("^. sample rate =", lline))) {
##>             ## * sample rate = 1 scan every 5.0 seconds
##>             rtmp <- lline;
##>             rtmp <- sub("(.*) sample rate = ", "", rtmp)
##>             rtmp <- sub("scan every ", "", rtmp)
##>             rtmp <- strsplit(rtmp, " ")
##>             ##      if (length(rtmp[[1]]) != 3)
##>             ##        warning("cannot parse sample-rate string in `",line,"'")
##>             sampleInterval <- as.double(rtmp[[1]][2]) / as.double(rtmp[[1]][1])
##>             if (rtmp[[1]][3] == "seconds") {
##>                 ;
##>             } else {
##>                 if (rtmp[[1]][3] == "minutes") {
##>                     sampleInterval <- sampleInterval / 60;
##>                 } else {
##>                     if (rtmp[[1]][3] == "hours") {
##>                         sampleInterval <- sampleInterval / 3600;
##>                     } else {
##>                         warning("cannot understand `",rtmp[[1]][2],"' as a unit of time for sampleInterval")
##>                     }
##>                 }
##>             }
##>         }
##>     }
##>     oceDebug(debug, "Finished reading header\n")
##>     if (debug > 0) {
##>         if (is.nan(sampleInterval))
##>             warning("'* sample rate =' not found in header")
##>         if (is.nan(latitude))
##>             warning("'** Latitude:' not found in header")
##>         if (is.nan(longitude))
##>             warning("'** Longitude:' not found in header")
##>         if (is.null(date))
##>             warning("'** Date:' not found in header")
##>         if (is.null(recovery))
##>             warning("'** Recovery' not found in header")
##>     }
##>     ## Require p,S,T data at least
##>     if (!found.temperature)
##>         stop("cannot find 'temperature' in this file")
##>     if (!found.pressure && !found.depth)
##>         stop("no column named 'pressure', 'depth' or 'depSM'")
##> 
##>     res@metadata$header <- header
##>     res@metadata$type <- "SBE"
##>     res@metadata$hexfilename <- hexfilename # from instrument
##>     res@metadata$serialNumber <- serialNumber
##>     res@metadata$serialNumberConductivity <- serialNumberConductivity
##>     res@metadata$pressureType <- pressureType
##>     res@metadata$units <- list(conductivity=conductivityUnit, temperature=temperatureUnit)
##>     res@metadata$systemUploadTime <- systemUploadTime
##>     res@metadata$ship <- ship
##>     res@metadata$scientist <- scientist
##>     res@metadata$institute <- institute
##>     res@metadata$address <- address
##>     res@metadata$cruise <- cruise
##>     res@metadata$station <- station
##>     res@metadata$deploymentType <- "unknown"
##>     res@metadata$date <- date
##>     res@metadata$startTime <- startTime
##>     res@metadata$latitude <- latitude
##>     res@metadata$longitude <- longitude
##>     res@metadata$recovery <- recovery
##>     res@metadata$waterDepth <- waterDepth # if NA, will update later
##>     res@metadata$sampleInterval <- sampleInterval
##>     res@metadata$names <- col.names.inferred
##>     res@metadata$labels <- col.names.inferred
##>     res@metadata$filename <- filename
##>     ## Read the data as a table.
##>     ## FIXME: should we match to standardized names?
##>     ##col.names.forced <- c("scan","pressure","temperature","conductivity","descent","salinity","sigmaThetaUnused","depth","flag")
##> 
##>     ## Handle similar names by tacking numbers on the end, e.g. the first column that
##>     ## is automatically inferred to hold temperature is called "temperature", while the
##>     ## next one is called "temperature2", and a third would be called "temperature3".
##>     col.names.inferred <- tolower(col.names.inferred)
##>     for (uname in unique(col.names.inferred)) {
##>         w <- which(uname == col.names.inferred)
##>         lw <- length(w)
##>         ##message("uname:", uname, ", lw: ", lw)
##>         if (1 != lw) {
##>             col.names.inferred[w[-1]] <- paste(uname, seq.int(2, lw), sep="")
##>         }
##>     }
##>     pushBack(lines, file)
##>     if (is.null(columns)) {
##>         oceDebug(debug, "About to read these names:", col.names.inferred,"\n")
##>         data <- as.list(read.table(file, skip=iline-1, header=FALSE, col.names=col.names.inferred))
##>         ## data <- as.list(read.table(text=lines[seq.int(iline, length(lines))],
##>         ##                            header=FALSE, col.names=col.names.inferred))
##>         ndata <- length(data[[1]])
##>         if (0 < ndata) {
##>             haveData <- TRUE
##>             names <- names(data)
##>             ##labels <- names
##>             if (!found.scan) {
##>                 data[['scan']] <- 1:ndata
##>             }
##>         } else {
##>             haveData <- FALSE
##>             warning("no data in CTD file \"", filename, "\"\n")
##>             data <- list(scan=NULL, salinity=NULL, temperature=NULL, pressure=NULL)
##>         }
##>     } else {
##>         dataAll <- read.table(file, skip=iline-1, header=FALSE, col.names=col.names.inferred)
##>         ## dataAll <- read.table(text=lines[seq.int(iline, length(lines))],
##>         ##                       header=FALSE, col.names=col.names.inferred)
##>         if ("scan" %in% names(columns)) {
##>             data <- dataAll[, as.numeric(columns)]
##>             names(data) <- names(columns)
##>         } else {
##>             data <- cbind(seq.int(1, dim(dataAll)[1]), dataAll[, as.numeric(columns)])
##>             names(data) <- c("scan", names(columns))
##>         }
##>         data <- as.list(data)
##>         ndata <- length(data[[1]])
##>         haveData <- ndata > 0
##>     }
##>     if (missing(processingLog))
##>         processingLog <- paste(deparse(match.call()), sep="", collapse="")
##>     ##hitem <- processingLogItem(processingLog)
##>     res@data <- data
##>     ## Add standard things, if missing
##>     if (haveData) {
##>         if (!found.salinity) {
##>             if (found.conductivity.ratio) {
##>                 warning("cannot find 'salinity' in this file; calculating from T, conductivity ratio, and p")
##>                 C <- data$conductivityratio
##>                 cmax <- max(C, na.rm=TRUE)
##>                 if (cmax > 5) {
##>                     warning("max(conductivity) > 5, so dividing by 42.914 before computing S. However, the original data are left in the object.")
##>                     C <- C / 42.914
##>                 } else if (cmax > 1) {
##>                     warning("max(conductivity) between 1 and 5, so dividing by 4.2914 before computing S. However, the original data are left in the object.")
##>                     C <- C / 4.2914
##>                 }
##>                 S <- swSCTp(C, data$temperature, data$pressure)
##>             } else if (found.conductivity) {
##>                 warning("cannot find 'salinity' in this file; calculating from T, conductivity, and p")
##>                 C <- data$conductivity
##>                 cmax <- max(C, na.rm=TRUE)
##>                 if (cmax > 5) {
##>                     warning("max(conductivity) > 5, so dividing by 42.914 before computing S. However, the original data are left in the object.")
##>                     C <- C / 42.914
##>                 } else if (cmax > 1) {
##>                     warning("max(conductivity) between 1 and 5, so dividing by 4.2914 before computing S. However, the original data are left in the object.")
##>                     C <- C / 4.2914
##>                 }
##>                 S <- swSCTp(C, data$temperature, data$pressure)
##>             } else {
##>                 stop("cannot find salinity in this file, nor conductivity or conductivity ratio")
##>             }
##>             res <- ctdAddColumn(res, S, name="salinity", label="Salinity", unit=c("", "PSS-78"), debug=debug-1)
##>         }
##>         if (found.depth && !found.pressure) { # BUG: this is a poor, nonrobust approximation of pressure
##>             g <- if (found.header.latitude) gravity(latitude) else 9.8
##>             rho0 <- 1000 + swSigmaTheta(median(res@data$salinity), median(res@data$temperature), 0)
##>             res <- ctdAddColumn(res, res@data$depth * g * rho0 / 1e4, name="pressure", label="Pressure",
##>                                 unit=list(unit="dbar", scale=""), debug=debug-1)
##>             warning("created a pressure column from the depth column\n")
##>         }
##>         ## res <- ctdAddColumn(res, swSigmaTheta(res@data$salinity, res@data$temperature, res@data$pressure),
##>         ##                 name="sigmaTheta", label="Sigma Theta", unit=list(unit=expression(kg/m^3), scale=""),
##>         ##                 debug=debug-1)
##>     }
##>     ## waterDepthWarning <- FALSE
##>     ## if (is.na(res@metadata$waterDepth)) {
##>     ##     res@metadata$waterDepth <- max(abs(res@data$pressure), na.rm=TRUE)
##>     ##     waterDepthWarning <- TRUE
##>     ## }
##>     res@processingLog <- processingLogAppend(res@processingLog, paste(deparse(match.call()), sep="", collapse=""))
##>     ## update to temperature IPTS-90, if have an older version
##>     if (2 == length(res@metadata$unit$temperature) &&
##>         "IPTS-68" == as.character(res@metadata$units$temperature$scale)) {
##>         res@data$temperature68 <- res@data$temperature
##>         res@metadata$units$temperature68 <- list(unit=expression(degree*C), scale="IPTS-68")
##>         res@data$temperature <- T90fromT68(res@data$temperature68)
##>         res@metadata$units$temperature <- list(unit=expression(degree*C), scale="ITS-90")
##>         warning("converted temperature from IPTS-68 to ITS-90")
##>         res@processingLog <- processingLogAppend(res@processingLog, "converted temperature from IPTS-68 to ITS-90")
##>     }
##>     if (!("salinity" %in% names(res@metadata$units))) res@metadata$units$salinity <- list(unit=expression(), scale="PSS-78")
##>     if (!("pressure" %in% names(res@metadata$units))) res@metadata$units$pressure <- list(unit=expression(dbar), scale="")
##>     if (!("depth" %in% names(res@metadata$units))) res@metadata$units$depth <- list(unit=expression(m), scale="")
##>     oceDebug(debug, "} # read.ctd.sbe()\n")
##>     ## if (waterDepthWarning)
##>     ##     res@processingLog <- processingLogAppend(res@processingLog, "inferred water depth from maximum pressure")
##>     res
##> }

#' Read an ODV-type CTD file
#' @template readCtdTemplate
#'
#' @details
#' \code{read.ctd.odf()} reads files stored in ODV format, used by some European data providers.
#'
#' @references
#' The \code{ODV} format is described in a file stored on the website of the British
#' Oceanographic Data Center, \code{bodc.ac.uk}, in a directory named
#' \code{data/codes_and_formats/odv_format}. (The URL is not provided here
#' because it is unreliable, which causes problems with CRAN submission of the
#' oce package.)
read.ctd.odv <- function(file, columns=NULL, station=NULL, missing.value=-999, monitor=FALSE,
                         debug=getOption("oceDebug"), processingLog, ...)
{
    stop("FIXME: make read.ctd.odv() work")
}


plotTS <- function (x,
                    inSitu=FALSE,
                    type='p',
                    referencePressure=0,
                    nlevels=6, levels,
                    grid=TRUE,
                    col.grid="lightgray",
                    lty.grid="dotted",
                    rho1000=FALSE,
                    eos=getOption("oceEOS", default='gsw'),
                    cex=par("cex"), col = par("col"), pch=par("pch"),
                    bg, pt.bg="transparent",
                    col.rho="darkgray",
                    cex.rho=3/4*par("cex"),
                    rotate=TRUE,
                    useSmoothScatter=FALSE,
                    xlab, ylab,
                    Slim, Tlim,
                    mgp=getOption("oceMgp"),
                    mar=c(mgp[1]+1.5,mgp[1]+1.5,mgp[1],mgp[1]),
                    lwd=par('lwd'), lty=par('lty'),
                    lwd.rho=par("lwd"), lty.rho=par("lty"),
                    add=FALSE, inset=FALSE,
                    debug=getOption("oceDebug"),
                    ...)
{
    oceDebug(debug, "plotTS(..., lwd.rho=", lwd.rho, ", lty.rho=", lty.rho,
             "eos=\"", eos, "\", ",
             "mgp=c(", paste(mgp, collapse=","), "), ",
             "mar=c(", paste(mar, collapse=","), "), ",
             "...) {\n", sep="", unindent=1)
    eos <- match.arg(eos, c("unesco", "gsw"))
    xat <- NULL
    yat <- NULL
    if (!inherits(x, "ctd")) {
        if (inherits(x, "section")) {
            x <- as.ctd(x[["salinity"]], x[["temperature"]], x[["pressure"]])
        } else {
            names <- names(x)
            if ("temperature" %in% names && "salinity" %in% names) {
                x <- as.ctd(x$salinity, x$temperature, x$pressure)
            } else {
                names <- names(x@data)
                if ("temperature" %in% names && "salinity" %in% names) {
                    x <- as.ctd(x@data$salinity, x@data$temperature, x@data$pressure)
                } else {
                    stop("cannot find salinity and temperature in 'x'")
                }
            }
        }
    }
    if (eos == "gsw") {
        salinity <- x[["SA"]]
        y <- x[["CT"]]
    } else {
        y <- if (inSitu) x[["temperature"]] else swTheta(x, referencePressure=referencePressure, eos=eos)
        salinity <- x[["salinity"]]
    }
    if (!any(is.finite(salinity))) {
        warning("no valid salinity data")
        return(invisible(list(xat=NULL, yat=NULL)))
    }
    if (!any(is.finite(y))) {
        warning("no valid temperature data")
        return(invisible(list(xat=NULL, yat=NULL)))
    }
    if (missing(Slim)) Slim <- range(salinity, na.rm=TRUE)
    if (missing(Tlim)) Tlim <- range(y, na.rm=TRUE)
    if (!add) {
        ##omar <- par("mar")
        ##omgp <- par("mgp")
        ##opar <- par(no.readonly=TRUE)
        if (!inset) {
            ## on.exit(par(mar=omar, mgp=omgp))
            if (3 == length(mgp)) par(mgp=mgp)
            if (!is.null(mar)) {
                if (4 == length(mar)) par(mar=mar)
            }
        }
    }
    ##axis.name.loc <- mgp[1]
    if (missing(xlab)) {
        if (eos == "gsw")
            xlab <- resizableLabel("absolute salinity", "x")
        else
            xlab <- resizableLabel("S","x")
    }
    if (missing(ylab)) {
        if (eos == "gsw")
            ylab <- resizableLabel("conservative temperature", "y")
        else
            ylab <- if (inSitu) resizableLabel("T", "y") else resizableLabel("theta", "y")
    }
    if (useSmoothScatter) {
        smoothScatter(salinity, y,
                      xlab = xlab, ylab=ylab,
                      xaxs = if (min(salinity, na.rm=TRUE)==0) "i" else "r", # avoid plotting S<0
                                        #cex=cex, pch=pch, col=col, cex.axis=par("cex.axis"),
                      xlim=Slim, ylim=Tlim,
                      ...)
    } else {
        if (add) {
            if (type == 'p') {
                points(salinity, y, cex=cex, pch=pch, col=col, bg=pt.bg, lwd=lwd, lty=lty)
            } else if (type == 'l') {
                lines(salinity, y, col=col, lwd=lwd, lty=lty, ...)
            } else if (type == 'o') {
                points(salinity, y, cex=cex, pch=pch, col=col, bg=pt.bg, lwd=lwd, lty=lty)
                lines(salinity, y, col=col, lwd=lwd, lty=lty, ...)
            } else if (type != 'n') {
                points(salinity, y, cex=cex, pch=pch, col=col, bg=pt.bg, lwd=lwd, lty=lty)
            }
        } else {
            plot(Slim, Tlim,
                 xlab = xlab, ylab=ylab,
                 xaxs = if (min(salinity,na.rm=TRUE)==0) "i" else "r", # avoid plotting S<0
                 cex=cex, pch=pch, col=col, cex.axis=par("cex.axis"),
                 type="n",
                 ...)
            if (!missing(bg)) {
                usr <- par('usr')
                rect(usr[1], usr[3], usr[2], usr[4], col=bg)
            }
            if (type == 'p') {
                points(salinity, y, cex=cex, pch=pch, col=col, bg=pt.bg, lwd=lwd, lty=lty, ...)
            } else if (type == 'l') {
                lines(salinity, y, col=col, lwd=lwd, lty=lty, ...)
            } else if (type == 'o') {
                points(salinity, y, cex=cex, pch=pch, col=col, bg=pt.bg, lwd=lwd, lty=lty, ...)
                lines(salinity, y, col=col, lwd=lwd, lty=lty, ...)
            } else if (type != 'n') {
                points(salinity, y, cex=cex, pch=pch, col=col, bg=pt.bg, lwd=lwd, lty=lty, ...)
            }
        }
    }
    ## grid, isopycnals, then freezing-point line
    if (grid)
        grid(col=col.grid, lty=lty.grid)
    drawIsopycnals(nlevels=nlevels, levels=levels, rotate=rotate, rho1000=rho1000, digits=2,
                   eos=eos, cex=cex.rho, col=col.rho, lwd=lwd.rho, lty=lty.rho)
    usr <- par("usr")
    Sr <- c(max(0, usr[1]), usr[2])
    lines(Sr, swTFreeze(salinity=Sr, pressure=0)) # old: darkblue that looked black
    box()                              # redraw box (otherwise overdrawn with isopycnals)
    oceDebug(debug, "} # plotTS(...)\n", sep="", unindent=1)
    ## infer from par()
    xaxp <- par("xaxp")
    xat <- seq(xaxp[1], xaxp[2], length.out=1+xaxp[3])
    yaxp <- par("yaxp")
    yat <- seq(yaxp[1], yaxp[2], length.out=1+yaxp[3])
    invisible(list(xat=xat, yat=yat))
}

drawIsopycnals <- function(nlevels=6, levels, rotate=TRUE, rho1000=FALSE, digits=2,
                           eos=getOption("oceEOS", default='gsw'),
                           cex=0.75*par('cex'), col="darkgray", lwd=par("lwd"), lty=par("lty"))
{
    eos <- match.arg(eos, c("unesco", "gsw"))
    usr <- par("usr")
    SAxisMin <- max(0.1, usr[1])       # avoid NaN, which UNESCO density gives for freshwater
    SAxisMax <- usr[2]
    TAxisMin <- usr[3]
    TAxisMax <- usr[4]
    Scorners <- c(SAxisMin, SAxisMax, SAxisMin, SAxisMax)
    Tcorners <- c(TAxisMin, TAxisMin, TAxisMax, TAxisMax)
    if (eos == "gsw") {
        rhoCorners <- gsw::gsw_rho(Scorners, Tcorners, rep(0, 4)) - 1000
    } else {
        rhoCorners <- swSigma(c(SAxisMin, SAxisMax, SAxisMin, SAxisMax),
                              c(TAxisMin, TAxisMin, TAxisMax, TAxisMax),
                              rep(0, 4))
    }
    rhoMin <- min(rhoCorners, na.rm=TRUE)
    rhoMax <- max(rhoCorners, na.rm=TRUE)
    if (missing(levels)) {
        levels <- pretty(c(rhoMin, rhoMax), n=nlevels)
        ## Trim first and last values, since not in box
        levels <- levels[-1]
        levels <- levels[-length(levels)]
    }
    if (any(levels > 1000))
        levels <- levels - 1000
    Tn <- 200
    Tline <- seq(TAxisMin, TAxisMax, length.out=Tn)
    cex.par <- par("cex")               # need to scale text() differently than mtext()
    for (rho in levels) {
        rhoLabel <- if (rho1000) 1000+rho else rho
        rhoLabel <- round(rhoLabel, digits)
        ## FIXME-gsw: will this handle gsw?
        Sline <- swSTrho(Tline, rep(rho, Tn), rep(0, Tn), eos=eos)
        ok <- !is.na(Sline) # crazy T can give crazy S
        if (sum(ok) > 2) {
            Sok <- Sline[ok]
            Tok <- Tline[ok]
            lines(Sok, Tok, col = col, lwd=lwd, lty=lty)
            if (cex > 0) {
                if (Sok[length(Sok)] > SAxisMax) { # to right of box
                    i <- match(TRUE, Sok > SAxisMax)
                    if (rotate)
                        mtext(rhoLabel, side=4, at=Tline[i], line=0, cex=cex, col=col)
                    else
                        text(usr[2], Tline[i], rhoLabel, pos=4, cex=cex/cex.par, col=col, xpd=TRUE)
                } else { # above box ... if the line got there
                    if (max(Tok) > (TAxisMax - 0.05 * (TAxisMax - TAxisMin)))
                        mtext(rhoLabel, side=3, at=Sline[Tn], line=0.1, cex=cex, col=col)
                }
            }
        }
    }
}

plotProfile <- function (x,
                         xtype="salinity+temperature",
                         ytype=c("pressure", "z", "depth", "sigmaTheta"),
                         eos=getOption("oceEOS", default="gsw"),
                         lty=1,
                         xlab=NULL, ylab=NULL,
                         col='black',
                         col.salinity = "darkgreen",
                         col.temperature = "red",
                         col.rho = "blue",
                         col.N2 = "brown",
                         col.dpdt = "darkgreen",
                         col.time = "darkgreen",
                         pt.bg="transparent",
                         grid = TRUE,
                         col.grid = "lightgray",
                         lty.grid = "dotted",
                         Slim, Clim, Tlim, densitylim, N2lim, Rrholim, dpdtlim, timelim, plim, ylim,
                         lwd=par("lwd"),
                         xaxs="r",
                         yaxs="r",
                         cex=1, pch=1,
                         useSmoothScatter=FALSE,
                         df,
                         keepNA=FALSE,
                         type='l',
                         mgp=getOption("oceMgp"),
                         mar=c(1 + if (length(grep('\\+', xtype))) mgp[1] else 0, mgp[1]+1.5, mgp[1]+1.5, mgp[1]),
                         add=FALSE,
                         inset=FALSE,
                         debug=getOption("oceDebug"),
                         ...)
{
    oceDebug(debug, "plotProfile(x, xtype[1]=\"", xtype[1],
             "\", debug=", debug, ", ...) {\n", sep="", unindent=1)
    eos <- match.arg(eos, c("unesco", "gsw"))
    plotJustProfile <- function(x, y, col="black", type="l", lty=lty,
                                lwd=par("lwd"),
                                cex=1, pch=1, pt.bg="transparent",
                                df=df, keepNA=FALSE, debug=getOption("oceDebug"))
    {
        oceDebug(debug, "plotJustProfile(type=\"", if (is.vector(type)) "(a vector)" else type, "\", col[1:3]=c(\"", paste(col[1:3], collapse='","'), "\"), ...) {\n", sep="", unindent=1)
        if (!keepNA) {
            keep <- !is.na(x) & !is.na(y)
            x <- x[keep]
            y <- y[keep]
            if (length(x) < 1 || length(y) < 1) {
                warning("no good data to plot")
                return(invisible())
            }
        }
        if (type == 'l') {
            lines(x, y, col = col, lwd=lwd, lty=lty, ...)
        } else if (type == 's') {
            lines(x, y, col = col, lwd=lwd, lty=lty, type='s')
        } else if (type == 'p') {
            points(x, y, col = col, cex=cex, pch=pch, bg=pt.bg)
        } else if (type == 'o') {
            lines(x, y, col=col, lwd=lwd, lty=lty, ...)
            points(x, y, col=col, cex=cex, pch=pch, bg=pt.bg)
        } else if (type == 'b') {
            lines(x, y, col=col, lwd=lwd, lty=lty, ...)
            points(x, y, col=col, cex=cex, pch=pch, bg=pt.bg)
        } else if (type == 'n') {
            ; # skip it
        } else {
            lines(x, y, col = col, lwd=lwd, lty=lty)
        }
        oceDebug(debug, "} # plotJustProfile\n")
    }                                  # plotJustProfile
    #if (!inherits(x, "ctd"))
    #    stop("method is only for objects of class '", "ctd", "'")
    ylimGiven <- !missing(ylim)
    densitylimGiven <- !missing(densitylim)
    dots <- list(...)
    ytype <- match.arg(ytype)
    if (!is.null(ylab) && ylab == "") {
        yname <- ""
    } else {
        yname <- switch(ytype,
                        pressure=resizableLabel("p", "y"),
                        z=resizableLabel("z", "y"),
                        depth=resizableLabel("depth", "y"),
                        sigmaTheta=resizableLabel("sigmaTheta", "y"))
    }
    ## if plim given on a pressure plot, then it takes precedence over ylim
    if (ytype == "pressure")
        if (!missing(plim))
            ylim <- plim
    if (missing(ylim))
        ylim <- switch(ytype,
                       pressure=rev(range(x@data$pressure, na.rm=TRUE)),
                       z=range(swZ(x@data$pressure), na.rm=TRUE),
                       depth=rev(range(swDepth(x), na.rm=TRUE)),
                       sigmaTheta=rev(range(x[["sigmaTheta"]], na.rm=TRUE)))
    examineIndices <- switch(ytype,
                       pressure = (min(ylim) <= x@data$pressure & x@data$pressure <= max(ylim)),
                       z = (min(ylim) <= swZ(x@data$pressure) & swZ(x@data$pressure) <= max(ylim)),
                       depth = (min(ylim) <= swDepth(x@data$pressure) & swDepth(x@data$pressure) <= max(ylim)),
                       sigmaTheta  = (min(ylim) <= x[["sigmaTheta"]] & x[["sigmaTheta"]] <= max(ylim)))
    if (0 == sum(examineIndices) && ytype == 'z' && ylim[1] >= 0 && ylim[2] >= 0) {
        warning("nothing is being plotted, because z is always negative and ylim specified a positive interval\n")
        return(invisible())
    }
    x@data <- as.list(x@data)
    dataNames <- names(x@data)
    if (length(xtype) == length(x@data$pressure))
        xtype <- xtype[examineIndices]
    for (dataName in dataNames) {
        x@data[[dataName]] <- x@data[[dataName]][examineIndices]
    }
    axis.name.loc <- mgp[1]
    know.time.unit <- FALSE
    if ("time" %in% names(x@data)) {
        know.time.unit <- TRUE
        time <- x@data$time
    } else {
        time <- 0:(length(x@data$pressure) - 1)
        if (!is.null(x@metadata$sampleInterval) && !is.na(x@metadata$sampleInterval)) {
            know.time.unit <- TRUE
            time <- time * x@metadata$sampleInterval
        }
    }
    if (ytype == "pressure")
        y <- x@data$pressure
    else if (ytype == "z")
        y <- swZ(x@data$pressure)
    else if (ytype == "depth")
        y <- swDepth(x@data$pressure)
    else if (ytype == "sigmaTheta")
        y <- swSigmaTheta(x)

    if (!add)
        par(mar=mar, mgp=mgp)
    if (length(xtype) == length(y)) {
        if ('axes' %in% names(list(...))) {
            plot(xtype, y, xlab="", ylab=yname, type=type, xaxs=xaxs, yaxs=yaxs, ylim=ylim, col=col, lty=lty, cex=cex, pch=pch, ...)
            if (list(...)$axes) {
                axis(3)
                mtext(xlab, side = 3, line = axis.name.loc, cex=par("cex"))
                axis(2)
            }
            box()
        } else {
            plot(xtype, y, xlab="", ylab=yname, type=type, axes=FALSE, xaxs=xaxs, yaxs=yaxs, ylim=ylim, col=col, lty=lty, cex=cex, pch=pch, ...)
            axis(3)
            mtext(xlab, side = 3, line = axis.name.loc, cex=par("cex"))
            axis(2)
            box()
        }
    } else if (is.numeric(xtype)) {
        if (length(xtype) != length(y))
            stop("length(xtype) must match number of levels in the CTD object")
        if (add) {
            lines(xtype, y, type=type, lty=lty, ...)
        } else {
            plot(xtype, y, xlab="", ylab=yname, type=type, axes=FALSE, xaxs=xaxs, yaxs=yaxs, ylim=ylim, lty=lty, cex=cex, pch=pch, ...)
            axis(3)
            mtext(xlab, side = 3, line = axis.name.loc, cex=par("cex"))
            axis(2)
            box()
            if (grid) {
                at <- par("yaxp")
                abline(h=seq(at[1], at[2], length.out=at[3]+1), col=col.grid, lty=lty.grid)
                at <- par("xaxp")
                abline(v=seq(at[1], at[2], length.out=at[3]+1), col=col.grid, lty=lty.grid)
            }
        }
    } else if (xtype == "index") {
        index <- 1:length(x@data$pressure)
        plot(index, x@data$pressure, ylim=ylim, col=col, lty=lty, xlab = "index", ylab = yname, type=type, xaxs=xaxs, yaxs=yaxs, cex=cex, pch=pch)
        if (grid) {
            at <- par("yaxp")
            abline(h=seq(at[1], at[2], length.out=at[3]+1), col=col.grid, lty=lty.grid)
        }
    } else if (xtype == "density+time") {
        if (add)
            warning("argument 'add' is ignored for xtype=\"density+time\"")
        st <- if (eos == "unesco") swSigmaTheta(x@data$salinity, x@data$temperature, x@data$pressure) else
            swSigmaTheta(x@data$salinity, x@data$temperature, x@data$pressure,
                         longitude=x[["longitude"]], latitude=x[["latitude"]], eos=eos)
        if (missing(densitylim))
            densitylim <- range(x@data$sigmaTheta, na.rm=TRUE)
        look <- if (keepNA) 1:length(y) else !is.na(st) & !is.na(y)
        plot(st[look], y[look], xlim=densitylim, ylim=ylim, cex=cex, pch=pch,
             type = type, col = col.rho, lty=lty, xlab = "", ylab = yname, axes = FALSE, xaxs=xaxs, yaxs=yaxs, ...)
        ## lines(st[look], y[look])
        axis(3, col = col.rho, col.axis = col.rho, col.lab = col.rho)
        ## FIXME: do next with resizable label; also for the N2
        ##br <- if (getOption("oceUnitBracket") == '[') c("[", "]") else c("(", ")")
        if (getOption("oceUnitBracket") == '[') {
            label <- if (eos == "unesco") expression(paste(sigma[theta], " [ ", kg/m^3, " ]")) else
                expression(paste(sigma[1], " [ ", kg/m^3, " ]"))
        } else {
            label <- if (eos == "unesco") expression(paste(sigma[theta], " ( ", kg/m^3, " )")) else
                expression(paste(sigma[1], " ( ", kg/m^3, " )"))
        }
        mtext(label, side = 3, line = axis.name.loc, col = col.rho, cex=par("cex"))
        axis(2)
        box()
        par(new = TRUE)                ## FIXME: this probably won't work if add=TRUE
        if (missing(timelim))
            timelim <- range(time, na.rm=TRUE)
        plot(time, y, xlim=timelim, ylim=ylim, type=type, xlab="", ylab=yname, axes=FALSE, lwd=lwd, col=col.time, xaxs=xaxs, yaxs=yaxs, lty=lty, cex=cex, pch=pch)
        axis(1, col=col.time, col.axis=col.time, col.lab=col.time)
        ## lines(time, y, lwd=lwd, col=col.time)
        if (know.time.unit) {
            if (getOption("oceUnitBracket") == '[') {
                mtext(expression(paste(Delta*t, " [ s ]")), side = 1, line = axis.name.loc, cex=par("cex"), col=col.time)
            } else {
                mtext(expression(paste(Delta*t, " ( s )")), side = 1, line = axis.name.loc, cex=par("cex"), col=col.time)
            }
        } else {
            if (getOption("oceUnitBracket") == '[') {
                mtext(expression(paste(Delta*t, " [ unknown unit ]")), side = 1, line = axis.name.loc, cex=par("cex"), col=col.time)
            } else {
                mtext(expression(paste(Delta*t, " ( unknown unit )")), side = 1, line = axis.name.loc, cex=par("cex"), col=col.time)
            }
        }
        box()
        if (grid) {
            at <- par("yaxp")
            abline(h=seq(at[1], at[2], length.out=at[3]+1), col=col.grid, lty=lty.grid)
        }
    } else if (xtype == "density+dpdt") {
        if (add)
            warning("argument 'add' is ignored for xtype=\"density+dpdt\"")
        if (missing(densitylim))
            densitylim <- range(x@data$sigmaTheta, na.rm=TRUE)
        st <- swSigmaTheta(x@data$salinity, x@data$temperature, x@data$pressure)
        look <- if (keepNA) 1:length(y) else !is.na(st) & !is.na(y)
        plot(st[look], y[look],
             xlim=densitylim, ylim=ylim, col=col.rho, lty=lty, cex=cex, pch=pch,
             type = type, xlab = "", ylab = yname, axes = FALSE, xaxs=xaxs, yaxs=yaxs, ...)
        axis(3, col = col.rho, col.axis = col.rho, col.lab = col.rho)
        if (getOption("oceUnitBracket") == '[') {
            mtext(expression(paste(sigma[theta], " [ ", kg/m^3, " ]")), side = 3, line = axis.name.loc, col = col.rho, cex=par("cex"))
        } else {
            mtext(expression(paste(sigma[theta], " ( ", kg/m^3, " )")), side = 3, line = axis.name.loc, col = col.rho, cex=par("cex"))
        }
        axis(2)
        box()
        ## lines(st, y, col = col.rho, lwd=lwd)
        par(new = TRUE)
        dpdt <- diff(x@data$pressure) / diff(time)
        dpdt <- c(dpdt[1], dpdt)        # fake first point
        df <- min(max(x@data$pressure, na.rm=TRUE) / 5, length(x@data$pressure) / 10) # FIXME: adjust params
        dpdt.sm <- smooth.spline(x@data$pressure, dpdt, df=df)
        if (missing(dpdtlim))
            dpdtlim <- range(dpdt.sm$y)
        plot(dpdt.sm$y, dpdt.sm$x,
             xlim=dpdtlim, ylim=ylim, type=type, xlab="", ylab=yname, axes=FALSE, lwd=lwd, col=col.dpdt, cex=cex, pch=pch,
             xaxs=xaxs, yaxs=yaxs, lty=lty, ...)
        axis(1, col=col.dpdt, col.axis=col.dpdt, col.lab=col.dpdt)
        ## lines(dpdt.sm$y, dpdt.sm$x, lwd=lwd, col=col.dpdt)
        if (getOption("oceUnitBracket") == '[') {
            if (know.time.unit) {
                mtext(expression(dp/dt * " [ dbar / s ]"),
                      side=1, line=axis.name.loc, cex=par("cex"), col=col.dpdt)
            } else {
                mtext(expression(dp/dt * " [ dbar / (time unit) ]"),
                      side=1, line=axis.name.loc, cex=par("cex"), col=col.dpdt)
            }
        } else {
            if (know.time.unit) {
                mtext(expression(dp/dt * " ( dbar / s )"),
                      side=1, line=axis.name.loc, cex=par("cex"), col=col.dpdt)
            } else {
                mtext(expression(dp/dt * " ( dbar / (time unit) )"),
                      side=1, line=axis.name.loc, cex=par("cex"), col=col.dpdt)
            }
        }
        box()
        if (grid) {
            at <- par("yaxp")
            abline(h=seq(at[1], at[2], length.out=at[3]+1), col=col.grid, lty=lty.grid)
            at <- par("xaxp")
            abline(v=seq(at[1], at[2], length.out=at[3]+1), col=col.grid, lty=lty.grid)
        }
    } else if (xtype == "S" || xtype == "salinity") {
        salinity <- if (eos == "gsw") swAbsoluteSalinity(x) else x@data$salinity
        if (!any(is.finite(salinity))) {
            warning("no valid salinity data")
            return(invisible())
        }
        if (missing(Slim)) {
            if ("xlim" %in% names(dots)) Slim <- dots$xlim else Slim <- range(salinity, na.rm=TRUE)
        }
        if (useSmoothScatter) {
            smoothScatter(salinity, y, xlim=Slim, ylim=ylim, xlab="", ylab=yname, axes=FALSE, ...)
            axis(2)
            axis(3)
            box()
            if (is.null(xlab)) {
                if (eos == "gsw") {
                    mtext(resizableLabel("absolute salinity", "x"), side = 3, line = axis.name.loc, cex=par("cex"))
                } else {
                    mtext(resizableLabel("S", "x"), side = 3, line = axis.name.loc, cex=par("cex"))
                }
            } else {
                mtext(xlab, side=3, line=axis.name.loc, cex=par("cex"))
            }
        } else {
            look <- if (keepNA) 1:length(y) else !is.na(salinity) & !is.na(y)
            if (!add) {
                plot(salinity[look], y[look],
                     xlim=Slim, ylim=ylim, lty=lty, cex=cex, pch=pch,
                     type = "n", xlab = "", ylab = yname, axes = FALSE, xaxs=xaxs, yaxs=yaxs, ...)
                if (is.null(xlab)) {
                    if (eos == "gsw") {
                        mtext(resizableLabel("absolute salinity", "x"), side = 3, line = axis.name.loc, cex=par("cex"))
                    } else {
                        mtext(resizableLabel("S", "x"), side = 3, line = axis.name.loc, cex=par("cex"))
                    }
                } else {
                    mtext(xlab, side=3, line=axis.name.loc, cex=par("cex"))
                }
                axis(2)
                axis(3)
                box()
                if (grid) {
                    at <- par("yaxp")
                    abline(h=seq(at[1], at[2], length.out=at[3]+1), col=col.grid, lty=lty.grid)
                    at <- par("xaxp")
                    abline(v=seq(at[1], at[2], length.out=at[3]+1), col=col.grid, lty=lty.grid)
                }
            }
            ## 2014-02-07: use col here, since no second axis to worry about
            plotJustProfile(salinity, y, type=type, lwd=lwd, lty=lty,
                            cex=cex, pch=pch, col=col, pt.bg=pt.bg,
                            keepNA=keepNA, debug=debug-1)
        }
    } else if (xtype == "C" || xtype == "conductivity") {
        if ('conductivity' %in% names(x@data)) {
            conductivity <- x@data$conductivity
        } else {
            conductivity <- swCSTp(x[['salinity']], x[['temperature']], x[['pressure']], eos=eos)
        }
        if (!any(is.finite(conductivity))) {
            warning("no valid conductivity data")
            return(invisible())
        }
        if (missing(Clim)) {
            if ("xlim" %in% names(dots)) Clim <- dots$xlim else Clim <- range(conductivity, na.rm=TRUE)
        }
        if (useSmoothScatter) {
            smoothScatter(conductivity, y, xlim=Clim, ylim=ylim, xlab="", ylab=yname, axes=FALSE, ...)
            axis(2)
            axis(3)
            box()
            if (is.null(xlab)) {
                ## Look up conductivity unit (issue 731)
                unit <- x[["conductivityUnit"]]
                if (is.null(unit)) {
                    mtext(resizableLabel("C", "x"), side=3, line=axis.name.loc, cex=par("cex"))
                } else {
                    if (unit$unit == "ratio") {
                        mtext(resizableLabel("C", "x"), side=3, line=axis.name.loc, cex=par("cex"))
                    } else if (unit[[1]] == "mS/cm") {
                        mtext(resizableLabel("conductivity mS/cm", "x"), side=3, line=axis.name.loc, cex=par("cex"))
                    } else if (unit[[1]] == "S/m") {
                        mtext(resizableLabel("conductivity S/m", "x"), side=3, line=axis.name.loc, cex=par("cex"))
                    } else {
                        stop("unknown conductivity unit ", unit, "; should be 'ratio', 'mS/cm' or 'S/m'")
                    }
                }
            } else {
                mtext(xlab, side=3, line=axis.name.loc, cex=par("cex"))
            }
        } else {
            look <- if (keepNA) 1:length(y) else !is.na(conductivity) & !is.na(y)
            if (!add) {
                plot(conductivity[look], y[look],
                     xlim=Clim, ylim=ylim, lty=lty, cex=cex, pch=pch,
                     type = "n", xlab = "", ylab = yname, axes = FALSE, xaxs=xaxs, yaxs=yaxs, ...)
                if (is.null(xlab)) {
                    ## Look up conductivity unit (issue 731)
                    unit <- x[["conductivityUnit"]]
                    if (is.null(unit)) {
                        mtext(resizableLabel("C", "x"), side=3, line=axis.name.loc, cex=par("cex"))
                    } else {
                        if (as.character(unit[[1]]) == "ratio") {
                            mtext(resizableLabel("C", "x"), side=3, line=axis.name.loc, cex=par("cex"))
                        } else if (as.character(unit[[1]]) == "mS/cm") {
                            mtext(resizableLabel("conductivity mS/cm", "x"), side=3, line=axis.name.loc, cex=par("cex"))
                        } else if (as.character(unit[[1]]) == "S/m") {
                            mtext(resizableLabel("conductivity S/m", "x"), side=3, line=axis.name.loc, cex=par("cex"))
                        } else {
                            stop("unknown conductivity unit ", unit[[1]], "; should be 'ratio', 'mS/cm' or 'S/m'")
                        }
                    }
                } else {
                    mtext(xlab, side=3, line=axis.name.loc, cex=par("cex"))
                }
                axis(2)
                axis(3)
                box()
                if (grid) {
                    at <- par("yaxp")
                    abline(h=seq(at[1], at[2], length.out=at[3]+1), col=col.grid, lty=lty.grid)
                    at <- par("xaxp")
                    abline(v=seq(at[1], at[2], length.out=at[3]+1), col=col.grid, lty=lty.grid)
                }
            }
            ## 2014-02-07: use col here, since no second axis to worry about
            plotJustProfile(conductivity, y, type=type, lwd=lwd, lty=lty,
                            cex=cex, pch=pch, col=col, pt.bg=pt.bg,
                            keepNA=keepNA, debug=debug-1)
        }
    } else if (xtype %in% c("oxygen", "nitrate", "nitrite", "phosphate", "silicate", "tritium",
                            "u" ,"v")) {
        if (!(xtype %in% names(x@data)))
            stop("no ", xtype, " in this station")
        if (!any(!is.na(x@data[[xtype]])))
            stop("all ", xtype, " values in this station are NA")
        if (useSmoothScatter) {
            smoothScatter(x@data[[xtype]], y, ylim=ylim, xlab="", ylab=resizableLabel("pressure", "y"), axes=FALSE, ...)
            axis(2)
            axis(3)
            box()
            mtext(resizableLabel(xtype, "x"), side = 3, line = axis.name.loc, cex=par("cex"))
        } else {
            look <- if (keepNA) 1:length(y) else !is.na(x@data[[xtype]]) & !is.na(y)
            if (!add) {
                if (ylimGiven) {
                    plot(x@data[[xtype]][look], y[look],
                         ylim=ylim, lty=lty,
                         type = "n", xlab = "", ylab = yname, axes = FALSE, xaxs=xaxs, yaxs=yaxs, ...)
                } else {
                    plot(x@data[[xtype]][look], y[look],
                         ylim=rev(range(y[look])), lty=lty,
                         type = "n", xlab = "", ylab = yname, axes = FALSE, xaxs=xaxs, yaxs=yaxs, ...)
                }
                mtext(resizableLabel(xtype, "x"), side = 3, line = axis.name.loc, cex=par("cex"))
                axis(2)
                axis(3)
                box()
                if (grid) {
                    at <- par("yaxp")
                    abline(h=seq(at[1], at[2], length.out=at[3]+1), col=col.grid, lty=lty.grid)
                    at <- par("xaxp")
                    abline(v=seq(at[1], at[2], length.out=at[3]+1), col=col.grid, lty=lty.grid)
                }
            }
            ## 2014-02-07: use col here, since no second axis to worry about
            plotJustProfile(x@data[[xtype]][look], y[look], type=type, lwd=lwd, lty=lty,
                            cex=cex, col=col, pch=pch, pt.bg=pt.bg,
                            keepNA=keepNA, debug=debug-1)
        }
    } else if (xtype == "Rrho" || xtype == "RrhoSF") {
        Rrho <- swRrho(x, sense=if (xtype=="Rrho") "diffusive" else "finger")
        look <- if (keepNA) 1:length(y) else !is.na(Rrho) & !is.na(y)
        if (!add) {
            if (ylimGiven) {
                plot(Rrho, y[look], lty=lty,
                     xlim=if (!missing(Rrholim)) Rrholim, ylim=ylim, cex=cex, pch=pch,
                     type = "n", xlab = "", ylab = yname, axes = FALSE, xaxs=xaxs, yaxs=yaxs, ...)
            } else {
                plot(Rrho, y[look], lty=lty,
                     xlim=if (!missing(Rrholim)) Rrholim, ylim=rev(range(y[look])), cex=cex, pch=pch,
                     type = "n", xlab = "", ylab = yname, axes = FALSE, xaxs=xaxs, yaxs=yaxs, ...)
            }
            mtext(expression(R[rho]), side = 3, line = axis.name.loc, cex=par("cex"))
            axis(2)
            axis(3)
            box()
            if (grid) {
                at <- par("yaxp")
                abline(h=seq(at[1], at[2], length.out=at[3]+1), col=col.grid, lty=lty.grid)
                at <- par("xaxp")
                abline(v=seq(at[1], at[2], length.out=at[3]+1), col=col.grid, lty=lty.grid)
            }
        }
        ## 2014-02-07: use col here, since no second axis to worry about
        plotJustProfile(Rrho, y[look], type=type, lwd=lwd, lty=lty,
                        cex=cex, col=col, pch=pch, pt.bg=pt.bg,
                        keepNA=keepNA, debug=debug-1)
    } else if (xtype == "T" || xtype == "temperature") {
        temperature <- if (eos == "gsw") swConservativeTemperature(x) else x@data$temperature
        if (!any(is.finite(temperature))) {
            warning("no valid temperature data")
            return(invisible())
        }
        if (missing(Tlim)) {
            if ("xlim" %in% names(dots)) Tlim <- dots$xlim else Tlim <- range(temperature, na.rm=TRUE)
        }
        if (useSmoothScatter) {
            smoothScatter(temperature, y, xlim=Tlim, ylim=ylim, xlab="", ylab=yname, axes=FALSE, ...)
            axis(2)
            axis(3)
            box()
            if (eos == "gsw")
                mtext(resizableLabel("conservative temperature", "x"), side = 3, line = axis.name.loc, cex=par("cex"))
            else
                mtext(resizableLabel("T", "x"), side = 3, line = axis.name.loc, cex=par("cex"))
        } else {
            look <- if (keepNA) 1:length(y) else !is.na(x@data$temperature) & !is.na(y)
            if (!add) {
                plot(temperature[look], y[look], lty=lty,
                     xlim=Tlim, ylim=ylim, cex=cex, pch=pch,
                     type = "n", xlab = "", ylab = "", axes = FALSE, xaxs=xaxs, yaxs=yaxs, ...)
                if (eos == "gsw") {
                    mtext(resizableLabel("conservative temperature", "x"),
                          side = 3, line = axis.name.loc, cex=par("cex"))
                } else {
                    mtext(resizableLabel("T", "x"),
                          side = 3, line = axis.name.loc, cex=par("cex"))
                }
                mtext(yname, side = 2, line = axis.name.loc, cex=par("cex"))
                axis(2)
                axis(3)
                box()
                if (grid) {
                    at <- par("yaxp")
                    abline(h=seq(at[1], at[2], length.out=at[3]+1), col=col.grid, lty=lty.grid)
                    at <- par("xaxp")
                    abline(v=seq(at[1], at[2], length.out=at[3]+1), col=col.grid, lty=lty.grid)
                }
            }
            plotJustProfile(temperature, y, type=type, col=col, lwd=lwd, lty=lty,
                            cex=cex, pch=pch, pt.bg=pt.bg,
                            keepNA=keepNA, debug=debug-1)
        }
    } else if (xtype == "theta" || xtype == "potential temperature") {
        theta <- swTheta(x, eos=eos)
        if (missing(Tlim)) {
            if ("xlim" %in% names(dots)) Tlim <- dots$xlim else Tlim <- range(theta, na.rm=TRUE)
        }
        if (useSmoothScatter) {
            smoothScatter(theta, y, xlim=Tlim, ylim=ylim, xlab="", ylab=yname, axes=FALSE, ...)
            axis(2)
            axis(3)
            box()
            if (eos == "gsw")
                mtext(resizableLabel("conservative temperature", "x"), side = 3, line = axis.name.loc, cex=par("cex"))
            else
                mtext(resizableLabel(theta, "x"), side = 3, line = axis.name.loc, cex=par("cex"))
        } else {
            look <- if (keepNA) 1:length(y) else !is.na(theta) & !is.na(y)
            if (!add) {
                plot(theta[look], y[look], lty=lty,
                     xlim=Tlim, ylim=ylim, cex=cex, pch=pch,
                     type="n", xlab="", ylab="", axes=FALSE, xaxs=xaxs, yaxs=yaxs, ...)
                if (is.null(xlab)) {
                    if (eos == "gsw") {
                        mtext(resizableLabel("conservative temperature", "x"), side=3, line=axis.name.loc, cex=par("cex"))
                    } else {
                        mtext(resizableLabel("theta", "x"), side=3, line=axis.name.loc, cex=par("cex"))
                    }
                } else {
                    mtext(xlab, side=3, line=axis.name.loc, cex=par("cex"))
                }
                mtext(yname, side=2, line=axis.name.loc, cex=par("cex"))
                axis(2)
                axis(3)
                box()
                if (grid) {
                    at <- par("yaxp")
                    abline(h=seq(at[1], at[2], length.out=at[3]+1), col=col.grid, lty=lty.grid)
                    at <- par("xaxp")
                    abline(v=seq(at[1], at[2], length.out=at[3]+1), col=col.grid, lty=lty.grid)
                }
            }
            plotJustProfile(theta, y, type=type, lwd=lwd, cex=cex, lty=lty,
                            col=col, pch=pch, pt.bg=pt.bg,
                            keepNA=keepNA, debug=debug-1)
        }
    } else if (xtype == "sigmaTheta") {
        st <- swSigmaTheta(x@data$salinity, x@data$temperature, x@data$pressure) # FIXME: why not use existing column?
        look <- if (keepNA) 1:length(y) else !is.na(st) & !is.na(y)
        ## FIXME: if this works, extend to other x types
        look <- look & (min(ylim) <= y & y <= max(ylim))
        if (!add) {
            if (densitylimGiven) {
                plot(st[look], y[look], xlim=densitylim, ylim=ylim, type = "n", xlab = "", ylab = yname, axes = FALSE, xaxs=xaxs, yaxs=yaxs, lty=lty, cex=cex, pch=pch, ...)
            } else {
                plot(st[look], y[look], xlim=range(st[look], na.rm=TRUE), ylim=ylim, type = "n", xlab = "", ylab = yname, axes = FALSE, xaxs=xaxs, yaxs=yaxs, lty=lty, cex=cex, pch=pch, ...)
            }
            if (is.null(xlab)) {
                if (getOption("oceUnitBracket") == '[') {
                    mtext(expression(paste(sigma[theta], " [ ", kg/m^3, " ]")), side = 3, line = axis.name.loc, cex=par("cex"))
                } else {
                    mtext(expression(paste(sigma[theta], " ( ", kg/m^3, " )")), side = 3, line = axis.name.loc, cex=par("cex"))
                }
            } else {
                mtext(xlab, side=3, line=axis.name.loc, cex=par("cex"))
            }
            axis(2)
            axis(3)
            box()
            if (grid) {
                at <- par("yaxp")
                abline(h=seq(at[1], at[2], length.out=at[3]+1), col=col.grid, lty=lty.grid)
                at <- par("xaxp")
                abline(v=seq(at[1], at[2], length.out=at[3]+1), col=col.grid, lty=lty.grid)
            }
        }
        plotJustProfile(st, y, col = col, type=type, lwd=lwd, lty=lty,
                        cex=cex, pch=pch, pt.bg=pt.bg,
                        keepNA=keepNA, debug=debug-1)
    } else if (xtype == "density") {
        rho <- swRho(x@data$salinity, x@data$temperature, x@data$pressure) # NOTE: ignoring any existing column
        look <- if (keepNA) 1:length(y) else !is.na(rho) & !is.na(y)
        ## FIXME: if this works, extend to other x types
        look <- look & (min(ylim) <= y & y <= max(ylim))
        if (!add) {
            if (densitylimGiven) {
                plot(rho[look], y[look], xlim=densitylim, ylim=ylim, type = "n", xlab = "", ylab = yname, axes = FALSE, xaxs=xaxs, yaxs=yaxs, lty=lty, cex=cex, pch=pch, ...)
            } else {
                plot(rho[look], y[look], xlim=range(rho[look], na.rm=TRUE), ylim=ylim, type = "n", xlab = "", ylab = yname, axes = FALSE, xaxs=xaxs, yaxs=yaxs, lty=lty, cex=cex, pch=pch, ...)
            }
            if (is.null(xlab)) {
                if (getOption("oceUnitBracket") == '[') {
                    mtext(expression(paste(rho, " [ ", kg/m^3, " ]")), side = 3, line = axis.name.loc, cex=par("cex"))
                } else {
                    mtext(expression(paste(rho, " ( ", kg/m^3, " )")), side = 3, line = axis.name.loc, cex=par("cex"))
                }
            } else {
                mtext(xlab, side=3, line=axis.name.loc, cex=par("cex"))
            }
            axis(2)
            axis(3)
            box()
            if (grid) {
                at <- par("yaxp")
                abline(h=seq(at[1], at[2], length.out=at[3]+1), col=col.grid, lty=lty.grid)
                at <- par("xaxp")
                abline(v=seq(at[1], at[2], length.out=at[3]+1), col=col.grid, lty=lty.grid)
            }
        }
        plotJustProfile(rho, y, col = col, type=type, lwd=lwd, lty=lty,
                        cex=cex, pch=pch, pt.bg=pt.bg,
                        keepNA=keepNA, debug=debug-1)
    } else if (xtype == "density+N2") {
        if (add)
            warning("argument 'add' is ignored for xtype=\"density+dpdt\"")
        st <- swSigmaTheta(x, eos=eos)
        if (!any(is.finite(st))) {
            warning("no valid sigma-theta data")
            return(invisible())
        }
        look <- if (keepNA) 1:length(y) else !is.na(st) & !is.na(y)
        if (missing(densitylim))
            densitylim <- range(st, na.rm=TRUE)
        plot(st[look], y[look], lty=lty,
             xlim=densitylim, ylim=ylim, cex=cex, pch=pch,
             type = "n", xlab = "", ylab = yname, axes = FALSE, xaxs=xaxs, yaxs=yaxs, ...)
        axis(3, col = col.rho, col.axis = col.rho, col.lab = col.rho)
        tmpsep <- getOption("oceUnitSep")
        sep <- if (!is.null(tmpsep)) tmpsep else ""
        if (getOption("oceUnitBracket") == '[') {
            label <- if (eos == "unesco") bquote(sigma[theta]*" ["*.(sep)*kg/m^3*.(sep)*"]") else
                bquote(sigma[0]*" ["*.(sep)*kg/m^3*.(sep)*"]")
        } else {
            label <- if (eos == "unesco") bquote(sigma[theta]*" ("*.(sep)*kg/m^3*.(sep)*")") else
                bquote(sigma[0]*" ("*.(sep)*kg/m^3*.(sep)*")")
        }
        mtext(label, side=3, line=axis.name.loc, col=col.rho, cex=par("cex"))
        axis(2)
        box()
        if (type == 'l') {
            lines(st, y, col = col.rho, lwd=lwd, lty=lty)
        } else if (type == 'p') {
            points(st, y, col = col.rho, pch=pch, cex=cex)
        } else {
            points(st, y, col = col.rho, pch=pch, cex=cex)
            lines(st, y, col = col.rho, lwd=lwd, lty=lty)
        }
        par(new = TRUE)
        N2 <- swN2(x, df=df, eos=eos)
        N2[!is.finite(N2)] <- NA
        if (missing(N2lim))
            N2lim <- range(N2, na.rm=TRUE)
        look <- if (keepNA) 1:length(y) else !is.na(N2) & !is.na(y)
        if (0 == sum(look)) {
            warning("no valid N2 data")
            return(invisible())
        }
        plot(N2[look], y[look], lty=lty,
             xlim=N2lim, ylim=ylim, cex=cex, pch=pch,
             type = "n", xlab = "", ylab = "", axes = FALSE, lwd=lwd, xaxs=xaxs, yaxs=yaxs)
        axis(1, col = col.N2, col.axis = col.N2, col.lab = col.N2)

        if (type == 'l') {
            lines(N2, y, col = col.N2, lwd=lwd, lty=lty)
        } else if (type == 'p') {
            points(N2, y, col = col.N2, pch=pch, cex=cex)
        } else {
            points(N2, y, col = col.N2, pch=pch, cex=cex)
            lines(N2, y, col = col.N2, lwd=lwd, lty=lty)
        }
        if (getOption("oceUnitBracket") == '[') {
            mtext(expression(paste(N^2, " [ ", s^-2, " ]")), side = 1, line = axis.name.loc, col = col.N2, cex=par("cex"))
        } else {
            mtext(expression(paste(N^2, " ( ", s^-2, " )")), side = 1, line = axis.name.loc, col = col.N2, cex=par("cex"))
        }
        box()
        if (grid) {
            at <- par("yaxp")
            abline(h=seq(at[1], at[2], length.out=at[3]+1), col=col.grid, lty=lty.grid)
        }
    } else if (xtype == "N2") {
        N2 <- swN2(x, df=df, eos=eos)
        if (missing(N2lim))
            N2lim <- range(N2, na.rm=TRUE)
        look <- if (keepNA) 1:length(y) else !is.na(N2) & !is.na(y)
        if (!add) {
            plot(N2[look], y[look], lty=lty,
                 xlim=N2lim, ylim=ylim, cex=cex, pch=pch,
                 type = "n", xlab = "", ylab = yname, axes = FALSE)
            if (getOption("oceUnitBracket") == '[') {
                mtext(expression(paste(N^2, " [ ", s^-2, " ]")), side = 3, line = axis.name.loc, cex=par("cex"), xaxs=xaxs, yaxs=yaxs)
            } else {
                mtext(expression(paste(N^2, " ( ", s^-2, " )")), side = 3, line = axis.name.loc, cex=par("cex"), xaxs=xaxs, yaxs=yaxs)
            }
            axis(2)
            axis(3)
            box()
            if (grid) {
                at <- par("yaxp")
                abline(h=seq(at[1], at[2], length.out=at[3]+1), col=col.grid, lty=lty.grid)
                at <- par("xaxp")
                abline(v=seq(at[1], at[2], length.out=at[3]+1), col=col.grid, lty=lty.grid)
            }
        }
        ## 2014-02-07: use col (not col.rho) here, since no second axis to worry about
        plotJustProfile(x=N2, y=y, col=col, type=type, lwd=lwd, lty=lty,
                        cex=cex, pch=pch, pt.bg=pt.bg,
                        keepNA=keepNA, debug=debug-1)
    } else if (xtype == "spice") {
        spice <-swSpice(x)
        look <- if (keepNA) 1:length(y) else !is.na(spice) & !is.na(y)
        if (!add) {
            plot(spice[look], y[look], lty=lty,
                 ylim=ylim, cex=cex, pch=pch,
                 type = "n", xlab = "", ylab = yname, axes = FALSE)
            mtext(resizableLabel("spice", "x"), side = 3, line = axis.name.loc, cex=par("cex"), xaxs=xaxs, yaxs=yaxs)
            axis(2)
            axis(3)
            box()
            if (grid) {
                at <- par("yaxp")
                abline(h=seq(at[1], at[2], length.out=at[3]+1), col=col.grid, lty=lty.grid)
                at <- par("xaxp")
                abline(v=seq(at[1], at[2], length.out=at[3]+1), col=col.grid, lty=lty.grid)
            }
        }
        plotJustProfile(x=spice, y=y, type=type, lwd=lwd, lty=lty,
                        cex=cex, col=col, pch=pch, pt.bg=pt.bg,
                        keepNA=keepNA, debug=debug-1)
    } else if (xtype == "salinity+temperature") {
        if (add)
            warning("argument 'add' is ignored for xtype=\"salinity+temperature\"")
        salinity <- if (eos == "gsw") swAbsoluteSalinity(x) else x@data$salinity
        temperature <- if (eos == "gsw") swConservativeTemperature(x) else x@data$temperature
        if (!any(is.finite(salinity))) {
            warning("no valid salinity data")
            return(invisible())
        }
        if (!any(is.finite(temperature))) {
            warning("no valid temperature data")
            return(invisible())
        }
        if (missing(Slim)) Slim <- range(salinity, na.rm=TRUE)
        if (missing(Tlim)) Tlim <- range(temperature, na.rm=TRUE)
        look <- if (keepNA) 1:length(y) else !is.na(temperature) & !is.na(y)
        plot(temperature[look], y[look],
             xlim=Tlim, ylim=ylim, col = col.temperature, lty=lty, cex=cex, pch=pch,
             type = type, xlab = "", ylab = yname, axes = FALSE, xaxs=xaxs, yaxs=yaxs)
        axis(3, col = col.temperature, col.axis = col.temperature, col.lab = col.temperature)
        if (is.null(getOption('plotProfileNoXLab'))) {
            if (eos == "gsw")
                mtext(resizableLabel("conservative temperature", "x"), side = 3, line=axis.name.loc, col=col.temperature, cex=par("cex"))
            else
                mtext(resizableLabel("T", "x"), side=3, line=axis.name.loc, col=col.temperature, cex=par("cex"))
        }
        axis(2)
        box()
        ## lines(temperature, y, col = col.temperature, lwd=lwd)
        par(new = TRUE)
        look <- if (keepNA) 1:length(y) else !is.na(x@data$salinity) & !is.na(y)
        plot(salinity[look], y[look],
             xlim=Slim, ylim=ylim, col = col.salinity, lty=lty, cex=cex, pch=pch,
             type = type, xlab = "", ylab = "", axes = FALSE, xaxs=xaxs, yaxs=yaxs)
        axis(1, col = col.salinity, col.axis = col.salinity, col.lab = col.salinity)
        if (is.null(getOption('plotProfileNoXLab'))) {
            if (eos == "gsw")
                mtext(resizableLabel("absolute salinity", "x"), side=1, line=axis.name.loc, col=col.salinity, cex=par("cex"))
            else
                mtext(resizableLabel("S", "x"), side=1, line=axis.name.loc, col=col.salinity, cex=par("cex"))
        }
        box()
        if (grid) {
            at <- par("yaxp")
            abline(h=seq(at[1], at[2], length.out=at[3]+1), col=col.grid, lty=lty.grid)
        }
        ## lines(salinity, y, col = col.salinity, lwd=if (length(lwd)>1)lwd[2] else lwd[1])
    } else {
        w <- which(names(x@data) == xtype)
        if (length(w) < 1)
            stop("unknown xtype value (\"", xtype, "\")")
        look <- if (keepNA) 1:length(y) else !is.na(x@data[[xtype]]) & !is.na(y)
        if (!add) {
            plot(x@data[[xtype]][look], y[look],
                 ylim=ylim, lty=lty, cex=cex, pch=pch,
                 type = "n", xlab="", ylab="",axes = FALSE, xaxs=xaxs, yaxs=yaxs)
            axis(3)
            mtext(resizableLabel("p"), side = 2, line = axis.name.loc, cex=par("cex"))
            label <- if (w <= length(x@metadata$labels)) x@metadata$labels[w] else
                as.character(xtype)
            if (is.character(label) && label == "sigmaTheta")
                label <- resizableLabel("sigmaTheta", "x")
            mtext(label, side=3, line=axis.name.loc, cex=par("cex"))
            axis(2)
            box()
        }
        if (type == "l") {
            lines(x@data[[w]], y, lwd=lwd, col=col, lty=lty)
        } else if (type == "p") {
            points(x@data[[w]], y, lwd=lwd, pch=pch, col=col, lty=lty, cex=cex)
        } else if (type == "b" || type == "o") {
            lines(x@data[[w]], y, lwd=lwd, col=col)
            points(x@data[[w]], y, lwd=lwd, pch=pch, col=col, lty=lty, cex=cex)
        } else {
            points(x@data[[w]], y, lwd=lwd, pch=pch, col=col, lty=lty, cex=cex)
        }
        if (grid) {
            at <- par("xaxp")
            abline(v=seq(at[1], at[2], length.out=at[3]+1), col=col.grid, lty=lty.grid)
            at <- par("yaxp")
            abline(h=seq(at[1], at[2], length.out=at[3]+1), col=col.grid, lty=lty.grid)
        }
    }
    oceDebug(debug, "} # plotProfile()\n", unindent=1)
}

#' Read an ITP-type CTD file
#' @template readCtdTemplate
#'
#' @details
#' \code{read.ctd.itp()} reads files stored in ice-tethered profile format.
#'
#' @references
#' Information about ice-tethered profile data is provided at
#' \url{http://www.whoi.edu/page.do?pid=23096}, which also provides a link for
#' downloading data.  Note that the present version only handles data in
#' profiler-mode, not fixed-depth mode.
read.ctd.itp <- function(file, columns=NULL, station=NULL, missing.value=-999, monitor=FALSE,
                         debug=getOption("oceDebug"), processingLog, ...)
{
    oceDebug(debug, "read.ctd.itp() {\n", unindent=1)
    if (is.character(file)) {
        filename <- fullFilename(file)
        file <- file(file, "r")
        on.exit(close(file))
    } else {
        filename <- ""
    }
    if (!inherits(file, "connection"))
        stop("argument `file' must be a character string or connection")
    if (!isOpen(file)) {
        open(file, "r")
        on.exit(close(file))
    }
    lines <- readLines(file, encoding="UTF-8")
    nlines <- length(lines)
    if ("%endofdat" == substr(lines[nlines], 1, 9)) {
        lines <- lines[1:(nlines-1)]
        nlines <- nlines - 1
    }
    if (nlines < 2)
        stop("file is too short; must have more than 2 lines")
    isProfile <- '%' != substr(lines[2], 1, 1)
    ## see e.g. http://www.whoi.edu/page.do?pid=125516
    if (isProfile) {
        ## %ITP 59, profile 2: year day longitude(E+) latitude(N+) ndepths
        ## 2013  247.25002   156.2163  80.3189  371
        ## %year day pressure(dbar) temperature(C) salinity oxygen(umol/kg)
        ## 2013  247.25036   18   -1.6548   30.5816  366.5573
        ## 2013  247.25043   20   -1.6523   30.7274  365.4786
        ## 2013  247.25052   22   -1.6537   31.1021  362.6732
        station <- gsub(":.*", "", gsub(".*profile[ ]*", "", lines[1]))
        d <- scan(text=lines[2], quiet=TRUE)
        year <- d[1]
        yearday <- d[2]
        longitude <- d[3]
        if (longitude < 0)
            longitude <- 360 + longitude
        latitude <- d[4]
        d <- read.table(text=lines[4:nlines])
        items <- scan(text=lines[3], what="character", quiet=TRUE)
        pcol <- grep("pressure", items)[1]
        Scol <- grep("salinity", items)[1]
        Tcol <- grep("temperature", items)[1]
        Ocol <- grep("oxygen", items)[1]
        pressure <- d[, pcol]
        temperature <- d[, Tcol]
        salinity <- d[, Scol]
        oxygen <- d[, Ocol]
        res <- as.ctd(salinity, temperature, pressure, oxygen=oxygen,
                       longitude=longitude, latitude=latitude,
                       startTime=ISOdate(year, 1, 1) + yearday * 3600 * 24,
                       station=station)
    } else {
        stop("can only handle 'profile' data type, not (presumably) SAMI type")
    }
    oceDebug(debug, "} # read.ctd.itp()\n", unindent=1)
    res
}

