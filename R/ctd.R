## vim:textwidth=128:expandtab:shiftwidth=4:softtabstop=4

#' Class to Store CTD (or general hydrographic) Data
#'
#' This class stores hydrographic data such as measured with a CTD (conductivity,
#' temperature, depth) instrument, or with other systems that produce
#' similar data.  Data repositories may store conductivity, temperature
#' and depth, as in the instrument name, but it is also common to store
#' salinity, temperature and pressure instead (or in addition). For this
#' reason, `ctd` objects are required to hold `salinity`,
#' `temperature` and `pressure` in their `data` slot,
#' with other data being optional. Formulae are available for converting
#' between variants of these data triplets, e.g. [swSCTp()]
#' can calculate `salinity` given `conductivity`, `temperature`
#' and `pressure`, and these are used by the main functions that
#' create `ctd` objects. For example, if [read.ctd.sbe()]
#' is used to read a Seabird file that contains only conductivity, temperature
#' and pressure, then that function will automatically append a data
#' item to hold salinity.  Since [as.ctd()] does the same with
#' salinity, the result this is that all `ctd` objects hold `salinity`,
#' `temperature` and `pressure`, which are henceforth called
#' the three basic quantities.
#'
#' Different units and scales are permitted for the three basic quantities, and
#' most `oce` functions check those units and scales before
#' doing calculations (e.g. of seawater density), because those calculations
#' demand certain units and scales. The way this is handled is that the
#' accessor function \code{\link{[[,ctd-method}}] returns values in standardized
#' form. For example, a `ctd` object might hold temperature defined on the
#' IPTS-68 scale, but e.g. `ctd[["temperature"]]` returns a value on the ITS-90
#' scale. (The conversion is done with [T90fromT68()].)  Similarly,
#' pressure may be stored in either dbars or PSI, but e.g. `ctd[["pressure"]]`
#' returns a value in dbars, after dividing by 0.689476 if the value is
#' stored in PSI. Luckily, there is (as of early 2016) only one salinity scale in
#' common use in data files, namely PSS-78.
#'
#' @templateVar class ctd
#'
#' @templateVar dataExample The key items stored in this slot are: `salinity`, `temperature`, and `pressure`, although in many instances there are quite a few additional items.
#'
#' @templateVar metadataExample An example of the former might be the location at which a `ctd` measurement was made, stored in `longitude` and `latitude`, and of the latter might be `filename`, the name of the data source.
#'
#' @template slot_summary
#'
#' @template slot_put
#'
#' @template slot_get
#'
#' @section Reading/creating `ctd` objects:
#' A file containing CTD profile data may be read with
#' [read.ctd()], and a CTD object can also be created with
#' [as.ctd()].  See [read.ctd()] for references on data
#' formats used in CTD files. Data can also be assembled into
#' `ctd` objects with [as.ctd()].
#'
#' Statistical summaries are provided by [summary,ctd-method()], while
#' [show()] displays an overview.
#'
#' CTD objects may be plotted with [plot,ctd-method()], which does much of its
#' work by calling [plotProfile()] or [plotTS()], both of
#' which can also be called by the user, to get fine control over the plots.
#'
#' A CTD profile can be isolated from a larger record with [ctdTrim()],
#' a task made easier when [plotScan()] is used to examine the results.
#' Towyow data can be split up into sets of profiles (ascending or descending)
#' with [ctdFindProfiles()].  CTD data may be smoothed and/or cast onto
#' specified pressure levels with [ctdDecimate()].
#'
#' As with all oce objects, low-level manipulation may be done with
#' [oceSetData()] and [oceSetMetadata()]. Additionally,
#' many of the contents of CTD objects may be altered with the
#' \code{\link{[[<-,ctd-method}} scheme,
#' and sufficiently skilled users may even manipulate the contents directly.
#'
#' @section Data sources:
#'
#' Archived CTD (and other) data may be found on servers such as
#' 1. \url{https://cchdo.ucsd.edu/}
#'
#' @examples
#'
#' # 1. Create a ctd object with fake data.
#' a <- as.ctd(salinity=35+1:3/10, temperature=10-1:3/10, pressure=1:3)
#' summary(a)
#'
#' # 2. Fix a typo in a station latitude (fake! it's actually okay)
#' data(ctd)
#' ctd <- oceSetMetadata(ctd, "latitude", ctd[["latitude"]]-0.001,
#'                      "fix latitude typo in log book")
#'
#' @author Dan Kelley
#'
#' @family things related to ctd data
#' @family classes provided by oce
setClass("ctd", contains="oce")

setAs("list", "ctd", function(from) {
      as.ctd(from) #salinity=from$salinity, temperature=from$temperature, pressure=from$pressure)
})


#' A CTD profile in Halifax Harbour
#'
#' This is a CTD profile measured in Halifax Harbour in 2003, based
#' on [ctdRaw()], but trimmed to just the downcast with
#' [ctdTrim()], using indices inferred by inspection of the
#' results from [plotScan()].
#'
#' This station was sampled by students enrolled in the Dan Kelley's
#' Physical Oceanography class at Dalhousie University.
#' The data were acquired near the centre of the Bedford Basin of the
#' Halifax Harbour, during an October 2003 field trip of Dalhousie University's
#' Oceanography 4120/5120 class. The original `.cnv` data file had
#' temperature in the IPTS-68 scale, but this was converted to the more modern
#' scale using [T90fromT68()].
#'
#' @name ctd
#' @docType data
#'
#' @usage data(ctd)
#'
#' @examples
#'\dontrun{
#' library(oce)
#' data(ctd)
#' plot(ctd)
#'}
#'
#' @seealso The full profile (not trimmed to the downcast) is available as
#' `data(ctdRaw)`.
#'
#' @family datasets provided with oce
#' @family things related to ctd data
NULL

#' Seawater CTD Profile, Without Trimming of Extraneous Data
#'
#' This is sample CTD profile provided for testing.  It includes not just the
#' (useful) portion of the dataset during which the instrument was being lowered,
#' but also data from the upcast and from time spent near the surface.  Spikes are
#' also clearly evident in the pressure record.  With such real-world wrinkles,
#' this dataset provides a good example of data that need trimming with
#' [ctdTrim()].
#'
#' This station was sampled by students enrolled in the Dan Kelley's
#' Physical Oceanography class at Dalhousie University.
#' The data were acquired near the centre of the Bedford Basin of the
#' Halifax Harbour, during an October 2003 field trip of Dalhousie University's
#' Oceanography 4120/5120 class. The original `.cnv` data file had
#' temperature in the IPTS-68 scale, but this was converted to the more modern
#' scale using [T90fromT68()].
#'
#' @name ctdRaw
#' @docType data
#'
#' @usage data(ctdRaw)
#'
#' @seealso A similar dataset (trimmed to the downcast) is available as
#' `data(ctd)`.
#'
#' @family things related to ctd data
#' @family datasets provided with oce
NULL


## DEVELOPERS: please pattern functions and documentation on this, for uniformity.
## DEVELOPERS: You will need to change the docs, and the 3 spots in the code
## DEVELOPERS: marked '# DEVELOPER 1:', etc.
#' @title Handle Flags in ctd Objects
#'
#' @param object a [ctd-class] object.
#'
#' @template handleFlagsTemplate
#'
#' @references
#' The following link used to work, but failed as of December 2020.
#'
#' 1. `https://www.nodc.noaa.gov/woce/woce_v3/wocedata_1/whp/exchange/exchange_format_desc.htm`
#'
#' @examples
#' library(oce)
#' data(section)
#' stn <- section[["station", 100]]
#' # 1. Default: anything not flagged as 2 is set to NA, to focus
#' # solely on 'good', in the World Hydrographic Program scheme.
#' STN1 <- handleFlags(stn, flags=list(c(1, 3:9)))
#' data.frame(old=stn[["salinity"]], new=STN1[["salinity"]], salinityFlag=stn[["salinityFlag"]])
#'
#' # 2. Use bottle salinity, if it is good and ctd is bad
#' replace <- 2 == stn[["salinityBottleFlag"]] && 2 != stn[["salinityFlag"]]
#' S <- ifelse(replace, stn[["salinityBottle"]], stn[["salinity"]])
#' STN2 <- oceSetData(stn, "salinity", S)
#'
#' # 3. Use smoothed TS relationship to nudge questionable data.
#' f <- function(x) {
#'   S <- x[["salinity"]]
#'   T <- x[["temperature"]]
#'   df <- 0.5 * length(S) # smooths a bit
#'   sp <- smooth.spline(T, S, df=df)
#'   0.5 * (S + predict(sp, T)$y)
#' }
#' par(mfrow=c(1,2))
#' STN3 <- handleFlags(stn, flags=list(salinity=c(1,3:9)), action=list(salinity=f))
#' plotProfile(stn, "salinity", mar=c(3, 3, 3, 1))
#' p <- stn[["pressure"]]
#' par(mar=c(3, 3, 3, 1))
#' plot(STN3[["salinity"]] - stn[["salinity"]], p, ylim=rev(range(p)))
#'
#' # 4. Single-variable flags (vector specification)
#' data(section)
#' # Multiple-flag scheme: one per data item
#' A <- section[["station", 100]]
#' deep <- A[["pressure"]] > 1500
#' flag <- ifelse(deep, 7, 2)
#' for (flagName in names(A[["flags"]]))
#'     A[[paste(flagName, "Flag", sep="")]] <- flag
#' Af <- handleFlags(A)
#' expect_equal(is.na(Af[["salinity"]]), deep)
#'
#' # 5. Single-variable flags (list specification)
#' B <- section[["station", 100]]
#' B[["flags"]] <- list(flag)
#' Bf <- handleFlags(B)
#' expect_equal(is.na(Bf[["salinity"]]), deep)
#'
#' @family things related to ctd data
setMethod("handleFlags", signature=c(object="ctd", flags="ANY", actions="ANY", where="ANY", debug="ANY"),
          definition=function(object, flags=NULL, actions=NULL, where=NULL, debug=getOption("oceDebug")) {
              ## DEVELOPER 1: alter the next comment to explain your setup
              if (is.null(flags)) {
                  flags <- defaultFlags(object)
                  if (is.null(flags))
                      stop("must supply 'flags', or use initializeFlagScheme() on the ctd object first")
              }
              if (is.null(actions)) {
                  actions <- list("NA") # DEVELOPER 3: alter this line to suit a new data class
                  names(actions) <- names(flags)
              }
              if (any(names(actions)!=names(flags)))
                  stop("names of flags and actions must match")
              handleFlagsInternal(object=object, flags=flags, actions=actions, where=where, debug=debug)
          })

#' @templateVar class ctd
#'
#' @templateVar note Since all the entries in the `data` slot of ctd objects are vectors, `i` must be a vector (either logical as in Example 1 or integer as in Example 2), or a function taking a `ctd` object and returning such a vector (see \dQuote{Indexing rules}).
#'
#' @template setFlagsTemplate
#'
#' @examples
#' library(oce)
#' # Example 1: Range-check salinity
#' data(ctdRaw)
#' ## Salinity and temperature range checks
#' qc <- ctdRaw
#' # Initialize flags to 2, meaning good data in the default
#' # scheme for handleFlags(ctd).
#' qc <- initializeFlags(qc, "salinity", 2)
#' qc <- initializeFlags(qc, "temperature", 2)
#' # Flag bad salinities as 4
#' oddS <- with(qc[["data"]], salinity < 25 | 40 < salinity)
#' qc <- setFlags(qc, name="salinity", i=oddS, value=4)
#' # Flag bad temperatures as 4
#' oddT <- with(qc[["data"]], temperature < -2 | 40 < temperature)
#' qc <- setFlags(qc, name="temperature", i=oddT, value=4)
#' # Compare results in TS space
#' par(mfrow=c(2, 1))
#' plotTS(ctdRaw)
#' plotTS(handleFlags(qc, flags=c(1, 3:9)))
#'
#' # Example 2: Interactive flag assignment based on TS plot, using
#' # WHP scheme to define 'acceptable' and 'bad' codes
#'\dontrun{
#' options(eos="gsw")
#' data(ctd)
#' qc <- ctd
#' qc <- initializeFlagScheme(qc, "WHP CTD")
#' qc <- initializeFlags(qc, "salinity", 2)
#' Sspan <- diff(range(qc[["SA"]]))
#' Tspan <- diff(range(qc[["CT"]]))
#' n <- length(qc[["SA"]])
#' par(mfrow=c(1, 1))
#' plotTS(qc, type="o")
#' message("Click on bad points; quit by clicking to right of plot")
#' for (i in seq_len(n)) {
#'     xy <- locator(1)
#'     if (xy$x > par("usr")[2])
#'         break
#'     i <- which.min(abs(qc[["SA"]] - xy$x)/Sspan + abs(qc[["CT"]] - xy$y)/Tspan)
#'     qc <- setFlags(qc, "salinity", i=i, value=4)
#'     qc <- handleFlags(qc, flags=list(salinity=4))
#'     plotTS(qc, type="o")
#' }
#'}
#'
#' @family things related to ctd data
setMethod("setFlags",
          c(object="ctd", name="ANY", i="ANY", value="ANY", debug="ANY"),
          function(object, name=NULL, i=NULL, value=NULL, debug=getOption("oceDebug")) {
              oceDebug(debug, "setFlags,ctd-method name=", name, ", i, value=", value, "\n")
              if (is.null(i))
                 stop("must supply i")
              if (!is.vector(i) && !is.function(i))
                  stop("'i' must be a vector or a function returning a vector")
              res <- setFlagsInternal(object, name, i, value, debug-1)
              res
          })

#' @templateVar class ctd
#' @templateVar details {NA}
#' @template initializeFlagSchemeTemplate
setMethod("initializeFlagScheme",
          signature=c(object="ctd", name="ANY", mapping="ANY", default="ANY", update="ANY", debug="ANY"),
          definition=function(object, name=NULL, mapping=NULL, default=NULL, update=NULL, debug=0) {
              if (is.null(name))
                  stop("must supply 'name'")
              invisible(callNextMethod())
          })

#' Initialize storage for a ctd object
#'
#' This function creates [ctd-class] objects. It is mainly
#' used by `oce` functions such as [read.ctd()] and [as.ctd()],
#' and it is not intended for novice users, so it may change at any time, without
#' following the usual rules for transitioning to deprecated and defunct status
#' (see [oce-deprecated]).
#'
#' @details
#' To save storage, this function has arguments only for quantities that are often present in data
#' files all cases. For example, not
#' all data files will have oxygen, so that's not present here.
#' Extra data may be added after the object is created, using
#' [oceSetData()].
#' Similarly, [oceSetMetadata()] may be used to add metadata (station ID, etc),
#' while bearing in mind that other functions look for such information
#' in very particular places (e.g. the station ID is a string named `station`
#' within the `metadata` slot). See [ctd-class] for more information
#' on elements stored in `ctd` objects.
#'
#' @param .Object the string `"ctd"`
#' @param pressure optional numerical vector of pressures.
#' @param salinity optional numerical vector of salinities.
#' @param temperature optional numerical vector of temperatures.
#' @param conductivity optional numerical vector of conductivities.
#' @param units optional list indicating units for the quantities specified
#' in the previous arguments. If this
#' is not supplied, a default is set up, based on which of the
#' `pressure` to `conductivity` arguments were specified.
#' If all of those 4 arguments were specified, then `units` is set
#' up as if the call included the following:
#' \code{units=list(temperature=list(unit=expression(degree*C), scale="ITS-90"),
#'      salinity=list(unit=expression(), scale="PSS-78"),
#'      conductivity=list(unit=expression(), scale=""),
#'      pressure=list(unit=expression(dbar), scale=""),
#'      depth=list(unit=expression(m), scale=""))}. This list is trimmed
#' of any of the 4 items that were not specified in the previous
#' arguments. Note that if `units` is specified, then it is just
#' copied into the `metadata` slot of the returned object, so the user
#' must be careful to set up values that will make sense to other `oce`
#' functions.
#' @param pressureType optional character string indicating the type of pressure;
#' if not supplied, this defaults to `"sea"`, which indicates the excess of
#' pressure over the atmospheric value, in dbar.
#' @param deploymentType optional character string indicating the type of deployment, which may
#' be `"unknown"`, `"profile"`, `"towyo"`, or `"thermosalinograph"`.
#' If this is not set, the value defaults to `"unknown"`.
#' @param ... Ignored.
#'
#' @family things related to ctd data
#'
#' @examples
#'
#' ## 1. empty
#' new("ctd")
#'
#' ## 2. fake data with no location information, so can only
#' ##    plot with the UNESCO equation of state.
#' ##    NOTE: always name arguments, in case the default order gets changed
#' ctd <- new("ctd", salinity=35+1:3/10, temperature=10-1:3/10, pressure=1:3)
#' summary(ctd)
#' plot(ctd, eos="unesco")
#'
#' ## 3. as 2, but insert location and plot with GSW equation of state.
#' ctd <- oceSetMetadata(ctd, "latitude", 44)
#' ctd <- oceSetMetadata(ctd, "longitude", -63)
#' plot(ctd, eos="gsw")
#'
#' @aliases initialize,ctd-method
setMethod(f="initialize",
          signature="ctd",
          definition=function(.Object, pressure, salinity, temperature, conductivity, units, pressureType, deploymentType, ...) {
              .Object <- callNextMethod(.Object, ...)
              ## Assign to some columns so they exist if needed later (even if they are NULL)
              .Object@data$pressure <- if (missing(pressure)) NULL else pressure
              .Object@data$temperature <- if (missing(temperature)) NULL else temperature
              .Object@data$salinity <- if (missing(salinity)) NULL else salinity
              .Object@data$conductivity <- if (missing(conductivity)) NULL else conductivity
              names <- names(.Object@data)
              ##.Object@metadata$names <- names
              ##.Object@metadata$labels <- titleCase(names) # paste(toupper(substring(names,1,1)), substring(names,2),sep="")
              ##.Object@metadata$filename <- filename
              if (missing(units)) {
                  .Object@metadata$units <- list()
                  if (!missing(pressure))
                      .Object@metadata$units$pressure <- list(unit=expression(dbar), scale="")
                  if (!missing(salinity))
                      .Object@metadata$units$salinity <- list(unit=expression(), scale="PSS-78")
                  if (!missing(temperature))
                      .Object@metadata$units$temperature <- list(unit=expression(degree*C), scale="ITS-90")
                  if (!missing(conductivity))
                      .Object@metadata$units$conductivity <- list(unit=expression(), scale="")
              } else {
                  .Object@metadata$units <- units # CAUTION: we are being quite trusting here
              }
              .Object@metadata$pressureType <- if (!missing(pressureType)) pressureType else "sea" # guess on the unit
              .Object@metadata$deploymentType <- if (!missing(deploymentType)) deploymentType
                  else "unknown" # "profile" "mooring" "towyo" "thermosalinograph"
              .Object@metadata$waterDepth <- NA
              #.Object@metadata$latitude <- NA
              #.Object@metadata$longitude <- NA
              #.Object@metadata$waterDepth <- NA
              .Object@processingLog$time <- presentTime()
              .Object@processingLog$value <- "create 'ctd' object"
              return(.Object)
          })


#' Summarize a ctd Object
#'
#' Summarizes some of the data in a `ctd` object, presenting such information
#' as the station name, sampling location, data ranges, etc. If the object was read
#' from a `.cnv` file or a `.rsk` file, then the `OriginalName`
#' column for the data summary will contain the original names of data within
#' the source file.
#'
#' @param object a [ctd-class] object.
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
#' @family things related to ctd data
setMethod(f="summary",
          signature="ctd",
          definition=function(object, ...) {
              ##mnames <- names(object@metadata)
              cat("CTD Summary\n-----------\n\n")
              type <- object@metadata$type
              model <- object@metadata$model
              mnames <- names(object@metadata)
              if (!is.null(type) && nchar(type)) {
                  if (is.null(model)) {
                      cat("* Instrument:         ", type, "\n")
                  } else {
                      cat("* Instrument:         ", type, model, "\n")
                  }
              }
              ##showMetadataItem(object, "type",                      "Instrument:          ")
              ##showMetadataItem(object, "model",                     "Instrument model:    ")
              ##showMetadataItem(object, "serialNumber",              "Instr. serial no.:   ")
              showMetadataItem(object, "serialNumberTemperature",   "Temp. serial no.:    ")
              showMetadataItem(object, "serialNumberConductivity",  "Cond. serial no.:    ")
              showMetadataItem(object, "filename",                  "File:                ", quote=TRUE)
              showMetadataItem(object, "hexfilename",               "Original file:       ")
              showMetadataItem(object, "institute",                 "Institute:           ")
              showMetadataItem(object, "scientist",                 "Chief scientist:     ")
              ##showMetadataItem(object, "date",                      "Date:                ", isdate=TRUE)
              showMetadataItem(object, "startTime",                 "Start time:          ", isdate=TRUE)
              ##showMetadataItem(object, "systemUploadTime",          "System upload time:  ", isdate=TRUE)
              if ("sampleInterval" %in% mnames && "sampleIntervalUnits" %in% mnames)
                  cat("* Sample interval:     ",
                      object@metadata$sampleInterval, " ", object@metadata$sampleIntervalUnits, "\n", sep="")
              showMetadataItem(object, "cruise",                    "Cruise:              ")
              showMetadataItem(object, "ship",                      "Vessel:              ")
              showMetadataItem(object, "station",                   "Station:             ")
              deploymentType <- object@metadata$deploymentType
              if (!is.null(deploymentType) && deploymentType != "unknown")
                  showMetadataItem(object, "deploymentType",            "Deployment type:     ")
              if ("longitude" %in% names(object@data)) {
                  cat("* Mean location:      ",       latlonFormat(mean(object@data$latitude, na.rm=TRUE),
                                                                   mean(object@data$longitude, na.rm=TRUE),
                                                                   digits=5), "\n")
              } else if ("longitude" %in% names(object@metadata) && !is.na(object@metadata$longitude)) {
                  cat("* Location:            ",       latlonFormat(object@metadata$latitude,
                                                                    object@metadata$longitude,
                                                                    digits=5), "\n", sep="")
              }
              showMetadataItem(object, "waterDepth", "Water depth:         ")
              showMetadataItem(object, "levels", "Number of levels: ")
              names <- names(object@data)
              invisible(callNextMethod()) # summary
          })


#' @title Extract Something From a ctd Object
#'
#' @param x a [ctd-class] object.
#'
#' @examples
#' data(ctd)
#' head(ctd[["temperature"]])
#'
#' @template sub_subTemplate
#'
#' @section Details of the specialized `ctd` method:
#'
#' Some uses of \code{\link{[[,ctd-method}} involve direct retrieval of
#' items within the `data` slot of the `ctd` object,
#' while other uses involve calculations based on items in that
#' `data` slot. For an example, all `ctd` objects
#' should hold an item named `temperature` in the `data`
#' slot, so for example `x[["temperature"]]` will retrieve that
#' item. By contrast, `x[["sigmaTheta"]]` is taken to be a
#' request to compute \eqn{\sigma_\theta}{sigma[theta]}, and so
#' it yields a call to [swTheta]`(x)` even if
#' the `data` slot of `x` might happen to contain an item
#' named `theta`. This can be confusing at first, but it tends
#' to lead to fewer surprises in everyday work, for otherwise the
#' user would be forced to check the contents of any `ctd`
#' object under analysis, to determine whether that item will be looked
#' up or computed. Nothing is lost in this scheme, since the data
#' within the object are always accessible with [oceGetData()].
#'
#' It should be noted that the accessor is set up to retrieve quantities
#' in conventional units. For example, [read.ctd.sbe()] is
#' used on a `.cnv` file that stores pressure in psi, it will
#' be stored in the same unit within the `ctd` object, but
#' `x[["pressure"]]` will return a value that has been converted
#' to decibars.  (To get pressure in PSI, use `x[["pressurePSI"]]`.)
#' Similarly, temperature is
#' returned in the ITS-90 scale, with a conversion having been performed with
#' [T90fromT68()], if the object holds temperature in
#' IPTS-68.  Again, temperature on the IPTS-68
#' scale is returned with `x@@data$temperature`.
#'
#' This preference for computed over stored quantities is accomplished
#' by first checking for computed quantities, and then falling
#' back to the general `[[` method if no match is found.
#'
#' Some quantities are optionally computed. For example, some data files
#' (e.g. the one upon which the [section()] dataset is based)
#' store `nitrite` along with the sum of nitrite and nitrate, the
#' latter with name ``NO2+NO3``. In this case, e.g. `x[["nitrate"]]`
#' will detect the setup, and subtract nitrite from the sum to yield
#' nitrate.
#'
#' The list given below provides notes on some quantities that are,
#' or may be, computed.
#'
#' * `conductivity` without a second argument (e.g. `a[["conductivity"]]`)
#' returns the value stored in the object. However, if a second argument is given,
#' and it is string specifying a unit, then conversion is made to that unit. The
#' permitted units are: either `""` or `"ratio"` (for ratio),
#' `"uS/cm"`, `"mS/cm"` and `"S/m"`. The calculations are based on
#' the definition of conductivity ratio as the ratio between measured conductivity
#' and the standard value 4.2914 S/m.
#'
#' * `CT` or `Conservative Temperature`: Conservative Temperature,
#' computed with [gsw::gsw_CT_from_t()].
#'
#' * `density`: seawater density, computed with [swRho]`(x)`.
#' (Note that it may be better to call that function directly, to gain
#' control of the choice of equation of state, etc.)
#'
#' * `depth`: Depth in metres below the surface, computed
#' with [swDepth]`(x)`.
#'
#' * `N2`: Square of Brunt-Vaisala frequency, computed  with [swN2]`(x)`.
#'
#' * `potential temperature`: Potential temperature in the
#' UNESCO formulation, computed with [swTheta]`(x)`.
#' This is a synonym for `theta`.
#'
#' * `Rrho`: Density ratio, computed with [swRrho]`(x)`.
#'
#' * `SA` or `Absolute Salinity`: Absolute Salinity,
#' computed with [gsw::gsw_SA_from_SP()].
#' The calculation involves location as well as measured water properties.
#' If the object `x` does not containing information on the location,
#' then 30N and 60W is used for the calculation, and a warning is generated.
#'
#' * `sigmaTheta`: A form of potential density anomaly, computed with
#' [swSigmaTheta]`(x)`.
#'
#' * `sigma0` Equal to `sigmaTheta`, i.e. potential density anomaly
#' referenced to a pressure of 0dbar, computed with [swSigma0]`(x)`.
#'
#' * `sigma1`: Potential density anomaly
#' referenced to a pressure of 1000dbar, computed with [swSigma1]`(x)`.
#'
#' * `sigma2`: Potential density anomaly
#' referenced to a pressure of 2000dbar, computed with [swSigma2]`(x)`.
#'
#' * `sigma3`: Potential density anomaly
#' referenced to a pressure of 3000dbar, computed with [swSigma3]`(x)`.
#'
#' * `sigma4`: potential density anomaly
#' referenced to a pressure of 4000dbar, computed with [swSigma4]`(x)`.
#'
#' * `SP`: Salinity on the Practical Salinity Scale, which is
#' `salinity` in the `data` slot.
#'
#' * `spice` or `spiciness0`: a variable that is in some sense
#' orthogonal to density, calculated with [swSpice]`(x)`.
#' Note that this is defined differently for `eos="unesco"` and
#' `eos="gsw"`.
#'
#' * `SR`: Reference Salinity computed with [gsw::gsw_SR_from_SP()].
#'
#' * `Sstar`: Preformed Salinity computed with [gsw::gsw_SR_from_SP()].
#' See `SA` for a note on longitude and latitude.
#'
#' * `theta`: potential temperature in the UNESCO formulation,
#' computed with [swTheta]`(x)`. This is a synonym for
#' `potential temperature`.
#'
#' * `time`: returns either a vector of times, a single
#' time, or `NULL`. A vector is returned if `time`
#' is present in the `data` slot, or if a time can be
#' inferred from other entries in the `data` slot (some of which,
#' such as the common `timeS`, also employ
#' `startTime` within the `metadata` slot). A single
#' value is returned if the dataset only has information on the start
#' time (which is stored as `startTime` within the `metadata`
#' slot. If it is impossible to determine the sampling time, then
#' `NULL` is returned. These time variants occur, in the
#' present version of oce, only for data read by [read.ctd.sbe()],
#' the documentation of which explains how times are computed.
#'
#' * `z`: Vertical coordinate in metres above the surface, computed with
#' [swZ]`(x)`.
#'
#'
#' @author Dan Kelley
#'
#' @family things related to ctd data
setMethod(f="[[",
          signature(x="ctd", i="ANY", j="ANY"),
          definition=function(x, i, j, ...) {
              data <- x@data
              metadata <- x@metadata
              dataNames <- names(data)
              metadataNames <- names(metadata)
              ## message("i=\"", i, "\"")
              if (i == "conductivity") {
                  C <- data$conductivity
                  ##message("i=", i, ", j=", if (missing(j)) "(missing)" else j)
                  if (!is.null(C) && !missing(j)) {
                      if (!(j %in% c("", "ratio", "uS/cm", "mS/cm", "S/m")))
                          stop("unknown conductivity unit \"", j, "\"; must be \"\", \"ratio\", \"uS/cm\", \"mS/cm\" or \"S/m\"")
                      if (j == "")
                          j <- "ratio" # lets us use switch()
                      unit <- metadata$units$conductivity$unit
                      if (is.null(unit) || !length(unit)) {
                          ## FIXME: maybe should look at median value, to make a guess
                          ## warning("ctd object lack conductivity units; assuming \"ratio\"")
                          unit <- "ratio"
                      }
                      ##message("A")
                      unit <- as.character(unit)
                      ##message("next is unit:")
                      ##print(dput(unit))
                      C <- data$conductivity
                      ##message("B")
                      ## Rather than convert from 3 inputs to 3 outputs, express as ratio, then convert as desired
                      if (!unit %in% c("ratio", "uS/cm", "mS/cm", "S/m"))
                          stop("object has unknown conductivity unit \"", unit, "\"; must be \"ratio\", \"uS/cm\", \"mS/cm\" or \"S/m\"")
                      C <- C / switch(unit, "uS/cm"=42914, "mS/cm"=42.914, "S/m"=4.2914, "ratio"=1)
                      C <- C * switch(j, "uS/cm"=42914, "mS/cm"=42.914, "S/m"=4.2914, "ratio"=1)
                  }
                  C
              } else if (i == "salinity" || i == "SP") {
                  if ("salinity" %in% dataNames) {
                      S <- data$salinity
                  } else {
                      C <- data$conductivity
                      if (!is.null(C)) {
                          if (is.null(metadata$units$conductivity)) {
                              warning("conductivity has no unit, so guessing it is conductivity-ratio. Be cautious on calculated salinity.")
                          } else {
                              unit <- as.character(metadata$units$conductivity$unit)
                              if (0 == length(unit)) {
                                  S <- swSCTp(C, x[["temperature"]], x[["pressure"]])
                                  warning("constructed salinity from temperature, conductivity-ratio and pressure")
                              } else if (unit == "uS/cm") {
                                  S <- swSCTp(C/42914.0, x[["temperature"]], x[["pressure"]])
                                  warning("constructed salinity from temperature, conductivity and pressure")
                              } else if (unit == "mS/cm") {
                                  ## e.g. RSK
                                  S <- swSCTp(C/42.914, x[["temperature"]], x[["pressure"]])
                                  warning("constructed salinity from temperature, conductivity and pressure")
                              } else if (unit == "S/m") {
                                  S <- swSCTp(C/4.2914, x[["temperature"]], x[["pressure"]])
                                  warning("constructed salinity from temperature, conductivity and pressure")
                              } else {
                                  stop("unrecognized conductivity unit '", unit, "'; only uS/cm, mS/cm and S/m are handled")
                              }
                          }
                      } else {
                          stop("the object's data slot lacks 'salinity', and it cannot be calculated since 'conductivity' is also missing")
                      }
                  }
                  S
              } else if (i == "SR") {
                  gsw::gsw_SR_from_SP(SP=x[["salinity"]])
              } else if (i == "Sstar") {
                  if (!any(is.finite(x[["longitude"]])) || !any(is.finite(x[["latitude"]])))
                      stop("object lacks location information, so Sstar cannot be computed")
                  n <- length(data$salinity)
                  ## Lengthen lon and lat if necessary, by repeating.
                  lon <- metadata$longitude
                  if (n != length(lon))
                      lon <- rep(metadata$longitude, length.out=n)
                  lat <- metadata$latitude
                  if (n != length(lat))
                      lat <- rep(metadata$latitude, length.out=n)
                  lon <- ifelse(lon < 0, lon + 360, lon) # not required because gsw_saar() does this ... but UNDOCUMENTED
                  ## Do the calculation in two steps
                  SA <- gsw::gsw_SA_from_SP(SP=x[["salinity"]], p=x[["pressure"]], longitude=lon, latitude=lat)
                  gsw::gsw_Sstar_from_SA(SA=SA, p=x[["pressure"]], longitude=lon, latitude=lat)
              } else if (i == "temperature") {
                  scale <- metadata$units[["temperature"]]$scale
                  if (!is.null(scale) && "IPTS-48" == scale) {
                      T90fromT48(x@data$temperature)
                  } else if (!is.null(scale) && "IPTS-68" == scale) {
                      T90fromT68(x@data$temperature)
                  } else {
                      x@data$temperature
                  }
              } else if (i == "pressure") {
                  if ("pressure" %in% dataNames) {
                      pressure <- data$pressure
                      unit <- metadata$units[["pressure"]]$unit
                      ## NOTE: 2019-04-29: The next will always return pressure, from the
                      ## else part of the conditional. This is because oce
                      ## stores pressure as dbar, and copies any original PSI data
                      ## into data$pressurePSI.
                      if (!is.null(unit) && "psi" == as.character(unit))
                          pressure * 0.6894757 # 1 psi=6894.757 Pa
                      else pressure
                  } else {
                      if ("depth" %in% dataNames)
                          swPressure(data$depth)
                      else stop("object's data slot does not contain 'pressure' or 'depth'")
                  }
              ## } else if (i == "longitude") {
              ##     if ("longitude" %in% metadataNames) metadata$longitude else data$longitude
              ## } else if (i == "latitude") {
              ##     if ("latitude" %in% metadataNames) metadata$latitude else data$latitude
              } else if (i == "time") {
                  ## After checking for 'time' literally in the metadata
                  ## and data slots, we turn to the 10 time variants
                  ## listed in the SBE Seasoft data processing manual,
                  ## revision 7.26.8, Appendix VI.
                  if ("time" %in% dataNames) {
                      data$time
                  } else if ("timeS" %in% dataNames) {
                      if (!("startTime" %in% metadataNames)) {
                          warning("have timeS in data slot, but no startTime in metadata slot, so cannot compute [[\"time\"]]\n")
                          NULL
                      } else {
                          metadata$startTime + data$timeS
                      }
                  } else if ("timeM" %in% dataNames) {
                      if (!("startTime" %in% metadataNames)) {
                          warning("have timeM in data slot, but no startTime in metadata slot, so cannot compute [[\"time\"]]\n")
                          NULL
                      } else {
                          metadata$startTime + 60 * data$timeM
                      }
                  } else if ("timeH" %in% dataNames) {
                      if (!("startTime" %in% metadataNames)) {
                          warning("have timeH in data slot, but no startTime in metadata slot, so cannot compute [[\"time\"]]\n")
                          NULL
                      } else {
                          metadata$startTime + 3600 * data$timeH
                      }
                  } else if ("timeJ" %in% dataNames) {
                      if (!("startTime" %in% metadataNames)) {
                          warning("have timeJ in data slot, but no startTime in metadata slot, so cannot compute [[\"time\"]]\n")
                          NULL
                      } else {
                          t0 <- ISOdatetime(1900 + as.POSIXlt(metadata$startTime)$year, 1, 1, 0, 0, 0, tz="UTC")
                          timeJ <- data$timeJ
                          ## Handle wraparound (FIXME: this code is tricky and not well-tested, esp wrt leap year)
                          wraps <- sum(diff(timeJ) < 0)
                          n <- length(timeJ)
                          for (w in rev(which(diff(timeJ)<0))) {
                              timeJ[seq(w+1, n)] <- round(timeJ[w]) + timeJ[seq(w+1, n)]
                          }
                          t0 + 86400 * (timeJ - 1)
                      }
                  } else if ("timeN" %in% dataNames) {
                      as.POSIXct(data$timeN, origin="1970-01-01",tz="UTC")
                  } else if ("timeQ" %in% dataNames) {
                      as.POSIXct(data$timeQ, origin="2000-01-01",tz="UTC")
                  } else if ("timeK" %in% dataNames) {
                      as.POSIXct(data$timeK, origin="2000-01-01",tz="UTC")
                  } else if ("timeJV2" %in% dataNames) {
                      if (!("startTime" %in% metadataNames)) {
                          warning("have timeJV2 in data slot, but no startTime in metadata slot, so cannot compute [[\"time\"]]\n")
                          NULL
                      } else {
                          t0 <- ISOdatetime(1900 + as.POSIXlt(metadata$startTime)$year, 1, 1, 0, 0, 0, tz="UTC")
                          timeJV2 <- data$timeJV2
                          ## Handle wraparound (FIXME: this code is tricky and not well-tested, esp wrt leap year)
                          wraps <- sum(diff(timeJV2) < 0)
                          n <- length(timeJV2)
                          for (w in rev(which(diff(timeJV2)<0))) {
                              timeJV2[seq(w+1, n)] <- round(timeJV2[w]) + timeJV2[seq(w+1, n)]
                          }
                          t0 + 86400 * (timeJV2 - 1)
                      }
                  } else if ("timeSCP" %in% dataNames) {
                      if (!("startTime" %in% metadataNames)) {
                          warning("have timeSCP in data slot, but no startTime in metadata slot, so cannot compute [[\"time\"]]\n")
                          NULL
                      } else {
                          t0 <- ISOdatetime(1900 + as.POSIXlt(metadata$startTime)$year, 1, 1, 0, 0, 0, tz="UTC")
                          timeSCP <- data$timeSCP
                          ## Handle wraparound (FIXME: this code is tricky and not well-tested, esp wrt leap year)
                          wraps <- sum(diff(timeSCP) < 0)
                          n <- length(timeSCP)
                          for (w in rev(which(diff(timeSCP)<0))) {
                              timeSCP[seq(w+1, n)] <- round(timeSCP[w]) + timeSCP[seq(w+1, n)]
                          }
                          t0 + 86400 * (timeSCP - 1)
                      }
                  } else if ("timeY" %in% dataNames) {
                      as.POSIXct(data$timeY, origin="1970-01-01",tz="UTC")
                  } else if ("time" %in% metadataNames) {
                      metadata$time
                  } else if ("startTime" %in% metadataNames) {
                      metadata$startTime
                  } else {
                      NULL
                  }
                  ## end of time decoding (whew!)
              } else if (i == "N2") {
                  swN2(x)
              } else if (i == "density") {
                  swRho(x)
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
              } else if (i == "spice" || i == "spiciness") {
                  swSpice(x)
              } else if (i %in% c("absolute salinity", "SA")) {
                  if (!any(is.finite(x[["longitude"]])) || !any(is.finite(x[["latitude"]])))
                      stop("object lacks location information, so SA cannot be computed")
                  SP <- x[["salinity"]]
                  p <- x[["pressure"]]
                  n <- length(SP)
                  ## Lengthen lon and lat if necessary, by repeating.
                  lon <- x[["longitude"]]
                  if (n != length(lon))
                      lon <- rep(lon, length.out=n)
                  lat <- x[["latitude"]]
                  if (n != length(lat))
                      lat <- rep(lat, length.out=n)
                  lon <- ifelse(lon < 0, lon + 360, lon) # not required because gsw_saar() does this ... but UNDOCUMENTED
                  ##: Change e.g. NaN to NA ... FIXME: tests show that this is not required:
                  ##:     > a<-as.ctd(10:11, c(35, asin(3)), 1:2, lon=-60, lat=50)
                  ##:                        a[["SA"]]
                  ##:     [1] 10.0472934071578 11.0520223880037
                  ##:     > a<-as.ctd(10:11, c(35, NA), 1:2, lon=-60, lat=50)
                  ##:     > a[["SA"]]
                  ##:     [1] 10.0472934071578 11.0520223880037
                  ##: SP[!is.finite(SP)] <- NA
                  ##: p[!is.finite(p)] <- NA
                  ##: lon[!is.finite(lon)] <- NA
                  ##: lat[!is.finite(lat)] <- NA
                  gsw::gsw_SA_from_SP(SP, p, lon, lat)
              } else if (i %in% c("conservative temperature", "CT")) {
                  if (!any(is.finite(x[["longitude"]])) || !any(is.finite(x[["latitude"]])))
                      stop("object lacks location information, so CT cannot be computed")
                  gsw::gsw_CT_from_t(SA=x[["SA"]], t=x[["temperature"]], p=x[["pressure"]])
              } else if (i == "nitrate") {
                  if ("nitrate" %in% dataNames) {
                      data$nitrate
                  } else {
                      if ("nitrite" %in% dataNames && "NO2+NO3" %in% dataNames)
                          data[["NO2+NO3"]] - data$nitrite
                      else NULL
                  }
              } else if (i == "nitrite") {
                  if ("nitrite" %in% dataNames) {
                      data$nitrite
                  } else {
                      if ("nitrate" %in% dataNames && "NO2+NO3" %in% dataNames)
                          data[["NO2+NO3"]] - data$nitrate
                      else NULL
                  }
              } else if (i == "z") {
                  swZ(x) # FIXME-gsw: permit gsw version here
              } else if (i == "depth") {
                  if ("depth" %in% names(data)) data$depth else swDepth(x) # FIXME-gsw: permit gsw version here
              } else if (i == "N2") {
                  swN2(x)
              } else {
                  ## message("FIXME: [[,ctd-method calling next method")
                  callNextMethod()     # [[ defined in R/AllClass.R
              }
          })

#' @title Replace Parts of a ctd Object
#'
#' @param x a [ctd-class] object.
#'
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
#' @family things related to ctd data
setMethod(f="[[<-",
          signature(x="ctd", i="ANY", j="ANY"),
          definition=function(x, i, j, ..., value) {
              callNextMethod(x=x, i=i, j=j, ..., value=value) # [[<-
          })


#' Coerce data into CTD object
#'
#' Assemble data into a [ctd-class] object.
#'
#' @param salinity There are several distinct choices for `salinity`.
#'
#' * It can be a
#' vector indicating the practical salinity through the water column. In that case,
#' `as.ctd` employs the other arguments listed below.
#'
#' * It can be an [rsk-class] object (see \dQuote{Converting rsk objects} for details).
#'
#' * It can be something (a data frame, a list or an `oce` object)
#' from which practical salinity, temperature, pressure, and conductivity
#' can be inferred. In this case, the relevant information
#' is extracted  and the other arguments to `as.ctd` are ignored, except for
#' `pressureAtmospheric`. If the first argument has salinity, etc., in
#' matrix form (as can happen with some objects of [argo-class]),
#' then only the first column is used, and a warning to that effect is given,
#' unless the `profile` argument is specified and then that specific
#' profile is extracted.
#'
#' * It can be unspecified, in which case `conductivity` becomes a mandatory
#' argument, because it will be needed for computing actual salinity,
#' using [swSCTp()].
#'
#' @param temperature *in-situ* temperature in \eqn{^\circ}{deg}C
#' on the ITS-90 scale; see \dQuote{Temperature units} in the documentation for
#' [swRho()].
#'
#' @param pressure Vector of pressure values, one for each `salinity` and
#' `temperature` pair, or just a single pressure, which is repeated to match
#' the length of `salinity`.
#'
#' @param conductivity electrical conductivity ratio through the water column
#' (optional). To convert from raw conductivity in milliSeimens per centimeter
#' divide by 42.914 to get conductivity ratio (see Culkin and Smith, 1980).
#'
##1108 @param SA absolute salinity (as in TEOS-10).  If given, the supplied absolute
##1108 salinity is converted internally to UNESCO-defined practical salinity.
##1108
##1108 @param CT conservative temperature (as in TEOS-10).  If given, the supplied
##1108 conservative temperature is converted internally to UNESCO-defined in-situ
##1108 temperature.
##1108
##1108 @param oxygen optional oxygen concentration
##1108
##1108 @param nitrate optional nitrate concentration
##1108
##1108 @param nitrite optional nitrite concentration
##1108
##1108 @param phosphate optional phosphate concentration
##1108
##1108 @param silicate optional silicate concentration
##1108
#' @param scan optional scan number.  If not provided, this will be set to
#' [seq_along]`(salinity)`.
#'
#' @param time optional vector of times of observation
#'
#' @param other optional list of other data columns that are not in the standard
#' list. Note that this argument is **deprecated**, meaning that it
#' will be removed soon. Please use `as.ctd()` to construct a [ctd-class]
#' object, and then use [oceSetData()] later, to add other data,
#' as needed.
#'
#' @param units an optional list containing units.  If not supplied,
#' defaults are set for `pressure`, `temperature`, `salinity`,
#' and `conductivity`. Since these are simply guesses, users
#' are advised strongly to supply `units`. See \dQuote{Examples}.
#'
#' @param flags if supplied, this is a [list] containing data-quality
#' flags. The elements of this list must have names that match the data
#' provided to the object.
#'
#' @param missingValue optional missing value, indicating data that should be
#' taken as `NA`. Set to `NULL` to turn off this feature.
#'
#' @param type optional type of CTD, e.g. "SBE"
#'
#' @param serialNumber optional serial number of instrument
#'
#' @param ship optional string containing the ship from which the observations were made.
#'
##1108 @param scientist optional string containing the chief scientist on the cruise.
##1108
##1108 @param institute optional string containing the institute behind the work.
##1108
##1108 @param address optional string containing the address of the institute.
##1108
#' @param cruise optional string containing a cruise identifier.
#'
#' @param station optional string containing a station identifier.
#'
##1108 @param date optional string indicating
##1108 the date at which the profile was started. This is copied verbatim into
##1108 the result's `metadata` slot, and is not used in any processing. Since
##1108 it serves no purpose, this argument is deprecated as of April 2016,
##1108 and will be marked 'defunct' in an upcoming CRAN release;
##1108 see [oce-deprecated].
#'
#' @param startTime optional indication of the start time for the profile,
#' which is used in some several plotting functions.  This is best given as a
#' [POSIXt] time, but it may also be a character string
#' that can be converted to a time with [as.POSIXct()],
#' using `UTC` as the timezone.
#'
##1108 @param recovery optional indication of the recovery time, in the format
##1108 described for `startTime`.  This is not presently used by `oce`,
##1108 and is stored in the result's `metadata` slot just in case the user
##1108 requires it.
#'
#' @param longitude optional numerical value containing longitude in decimal
#' degrees, positive in the eastern hemisphere. If this is a single number,
#' then it is stored in the `metadata` slot of the returned value; if it
#' is a vector of numbers, then they are stored in the `data` slot.
#'
#' @param latitude optional numerical value containing the latitude in decimal
#' degrees, positive in the northern hemisphere. See the note on length, for
#' the `longitude` argument.
#'
#' @param deploymentType character string indicating the type of deployment. Use
#' `"unknown"` if this is not known, `"profile"` for a profile (in
#' which the data were acquired during a downcast, while the device was lowered
#' into the water column, perhaps also including an upcast; `"moored"` if
#' the device is installed on a fixed mooring, `"thermosalinograph"` (or
#' `"tsg"`) if the device is mounted on a moving vessel, to record
#' near-surface properties, or `"towyo"` if the device is repeatedly
#' lowered and raised.
#'
#' @param pressureAtmospheric A numerical value (a constant or a vector),
#' that is subtracted from pressure before storing it in the return value.
#' (This altered pressure is also used in calculating `salinity`, if
#' that is to be computed from `conductivity`, etc., using
#' [swSCTp()]; see `salinity` above.)
##
##1108 @param waterDepth optional numerical value indicating the water depth in
##1108 metres. This is different from the maximum recorded pressure, although
##1108 the latter is used by some oce functions as a guess on water depth, the
##1108 most important example being [plot,section-method()].
#'
#' @param sampleInterval optional numerical value indicating the time between
#' samples in the profile.
#'
#' @param profile optional positive integer specifying the number of the profile
#' to extract from an object that has data in matrices, such as for some
#' `argo` objects. Currently the `profile` argument is only utilized for
#' [argo-class] objects.
#'
##1108 @param src optional string indicating data source.
#'
#' @template debugTemplate
#'
#' @section Converting rsk objects:
#' If the `salinity` argument is an object of [rsk-class],
#' then `as.ctd` passes it,
#' `pressureAtmospheric`,
#' `longitude`,
#' `latitude`
#' `ship`,
#' `cruise`,
#' `station` and
#' `deploymentType`
#' to [rsk2ctd()], which builds the ctd object that is
#' returned by `as.ctd`. The other arguments to `as.ctd`
#' are ignored in this instance, because `rsk` objects already
#' contain their information. If required, any data or metadata
#' element can be added to the value returned by `as.ctd`
#' using [oceSetData()] or [oceSetMetadata()],
#' respectively.
#'
#' The returned [rsk-class] object contains pressure in a form that
#' may need to be adjusted, because `rsk` objects may contain
#' either absolute pressure or sea pressure. This adjustment is handled
#' automatically by `as.ctd`, by examination of the metadata item
#' named `pressureType` (described in the documentation for
#' [read.rsk()]).  Once the sea pressure is determined,
#' adjustments may be made with the `pressureAtmospheric` argument,
#' although in that case it is better considered a pressure adjustment
#' than the atmospheric pressure.
#'
#' [rsk-class] objects may store sea pressure or absolute pressure (the
#' sum of sea pressure and atmospheric pressure), depending on how the object was
#' created with [as.rsk()] or [read.rsk()].  However,
#' [ctd-class] objects store sea pressure, which is needed for
#' plotting, calculating density, etc. This poses no difficulties, however,
#' because `as.ctd` automatically converts absolute pressure to sea pressure,
#' if the metadata in the [rsk-class] object indicates that this is
#' appropriate. Further alteration of the pressure can be accomplished with the
#' `pressureAtmospheric` argument, as noted above.
#'
#' @return A [ctd-class] object.
#'
#' @examples
#' library(oce)
#' ## 1. fake data, with default units
#' pressure <- 1:50
#' temperature <- 10 - tanh((pressure - 20) / 5) + 0.02*rnorm(50)
#' salinity <- 34 + 0.5*tanh((pressure - 20) / 5) + 0.01*rnorm(50)
#' ctd <- as.ctd(salinity, temperature, pressure)
#' # Add a new column
#' fluo <- 5 * exp(-pressure / 20)
#' ctd <- oceSetData(ctd, name="fluorescence", value=fluo,
#'                   unit=list(unit=expression(mg/m^3), scale=""))
#' summary(ctd)
#'
#' ## 2. fake data, with supplied units (which are the defaults, actually)
#' ctd <- as.ctd(salinity, temperature, pressure,
#'     units=list(salinity=list(unit=expression(), scale="PSS-78"),
#'     temperature=list(unit=expression(degree*C), scale="ITS-90"),
#'     pressure=list(unit=expression(dbar), scale="")))
#'
#' @references Culkin, F., and Norman D. Smith, 1980. Determination of the
#' concentration of potassium chloride solution having the same electrical
#' conductivity, at 15 C and infinite frequency, as standard seawater of salinity
#' 35.0000 ppt (Chlorinity 19.37394 ppt). *IEEE Journal of Oceanic
#' Engineering*, volume **5**, pages 22-23.
#'
#' @author Dan Kelley
#'
#' @family things related to ctd data
as.ctd <- function(salinity, temperature=NULL, pressure=NULL, conductivity=NULL,
                   ##1108 SA=NULL, CT=NULL, oxygen=NULL, nitrate=NULL, nitrite=NULL, phosphate=NULL, silicate=NULL,
                   scan=NULL,
                   time=NULL, other=NULL,
                   units=NULL, flags=NULL,
                   ##1108 pressureType="sea",
                   missingValue=NULL,
                   ##1108 quality=NULL, filename="",
                   type="", serialNumber="",
                   ship=NULL, cruise=NULL, station=NULL,
                   ##1108 date=NULL,
                   startTime=NULL,
                   ##1108 recovery=NULL,
                   longitude=NULL, latitude=NULL,
                   deploymentType="unknown",
                   pressureAtmospheric=0,
                   ##1108 waterDepth=NA,
                   sampleInterval=NA,
                   profile=NULL,
                   ##1108 src="",
                   debug=getOption("oceDebug"))
{
    oceDebug(debug, "as.ctd(...) {\n", sep="", unindent=1, style="bold")
    if (!missing(salinity) && inherits(salinity, "rsk")) {
        oceDebug(debug, "first argument is an 'rsk' object\n")
        res <- rsk2ctd(salinity,
                       pressureAtmospheric=pressureAtmospheric,
                       longitude=longitude,
                       latitude=latitude,
                       ship=ship,
                       station=station,
                       cruise=cruise,
                       deploymentType=deploymentType,
                       debug=debug-1)
        oceDebug(debug, "} # as.ctd()\n", sep="", unindent=1, style="bold")
        return(res)
    }
    res <- new('ctd')
    waterDepth <- NA
    unitsGiven <- !is.null(units)
    salinityGiven <- !missing(salinity)
    if (!is.null(startTime) && is.character(startTime))
        startTime <- as.POSIXct(startTime, tz="UTC")
    ##1108 if (!is.null(recovery) && is.character(recovery))
    ##1108     recovery <- as.POSIXct(recovery, tz="UTC")

    if (salinityGiven) {
        firstArg <- salinity
    } else {
        if (!missing(conductivity) && !missing(temperature) && !missing(pressure)) {
            salinity <- swSCTp(conductivity=conductivity, temperature=temperature, pressure=pressure)
        } else {
            stop("if salinity is not provided, conductivity, temperature and pressure must all be provided")
        }
        firstArg <- NULL
    }
    filename <- ""
    ounits <- NULL # replace with metadata$units if first arg is an oce object
    if (inherits(salinity, "oce")) {
        if (inherits(salinity, "ctd")) {
            oceDebug(debug, "first argument is a ctd object already, so returning as-is\n")
            oceDebug(debug, "} # as.ctd()\n", sep="", unindent=1, style="bold")
            return(salinity)
        }
        oceDebug(debug, "first argument is an oce object, so ignoring other arguments\n")
        o <- salinity
        d <- o@data
        m <- o@metadata
        ounits <- o@metadata$units
        dnames <- names(d)
        mnames <- names(m)
        ship <- m$ship
        cruise <- m$cruise
        station <- m$station
        startTime <- if (is.character(m$startTime)) startTime <- as.POSIXct(m$startTime, tz="UTC") else m$startTime
        ## Look for lon and lat first in data slot, second in metadata slot
        if (is.null(longitude)) {
            if ("longitude" %in% dnames) {
                longitude <- d$longitude
            } else if ("longitude" %in% mnames) {
                longitude <- m$longitude
            }
        }
        if (is.null(latitude)) {
            if ("latitude" %in% dnames) {
                latitude <- d$latitude
            } else if ("latitude" %in% mnames) {
                latitude <- m$latitude
            }
        }
        ##1108 if (missing(date) && "date" %in% names(m)) {
        ##1108     date <- m$date
        ##1108 }
        filename <- if ("filename" %in% mnames) m$filename else ""
        ##1108 model <- m$model
        serialNumber <- m$serialNumber
        sampleInterval <- m$sampleInterval
        if (!is.null(m$waterDepth))
            waterDepth <- m$waterDepth
        ## Copy some WOCE things into oce-convention names (originals retained)
        if ("PSAL" %in% dnames && !("salinity" %in% dnames)) d$salinity <- d$PSAL
        if ("TEMP" %in% dnames && !("temperature" %in% dnames)) d$temperature <- d$TEMP
        if ("PRES" %in% dnames && !("pressure" %in% dnames)) d$pressure <- d$PRES
        if (!missing(pressureAtmospheric)) {
            len <- length(pressureAtmospheric)
            if (1 != len && len != length(pressure))
                stop("length(pressureAtmospheric) must be 1 or length(pressure)")
            d$pressure <- d$pressure - pressureAtmospheric
        }
        salinity <- d$salinity
        res@metadata$units <- o@metadata$units
        if (!is.null(flags))
            res@metadata$flags <- flags
        if (!is.null(o@metadata$flags))
            res@metadata$flags <- o@metadata$flags
        ##1108 res@metadata$pressureType <- pressureType
        ## copy relevant metadata.
        ##1108 if ("date" %in% mnames) res@metadata$date <- o@metadata$date

        ## if any changes here, update oce.R @ ODF_CTD_LINK {
        res@metadata$startTime <- startTime
        if ("eventNumber" %in% mnames) res@metadata$eventNumber <- o@metadata$eventNumber
        if ("eventQualifier" %in% mnames) res@metadata$eventQualifier <- o@metadata$eventQualifier
        ## } ODF_CTD_LINK

        if ("deploymentType" %in% mnames) res@metadata$deploymentType <- o@metadata$deploymentType
        if ("filename" %in% mnames) res@metadata$filename <- o@metadata$filename
        if ("serialNumber" %in% mnames) res@metadata$serialNumber <- o@metadata$serialNumber
        if ("ship" %in% mnames) res@metadata$ship <- o@metadata$ship
        if ("cruise" %in% mnames) res@metadata$cruise <- o@metadata$cruise
        if ("station" %in% mnames) res@metadata$station <- o@metadata$station
        if ("scientist" %in% mnames) res@metadata$scientist <- o@metadata$scientist
        if ("units" %in% mnames) {
            ## the usual case
            ## res@metadata$units$conductivity <- o@metadata$units$conductivity
            ## res@metadata$units$temperature <- o@metadata$units$temperature
            res@metadata$units <- o@metadata$units
        } else {
            ## permit a case that existed for a few months in 2015
            if ("conductivityUnit" %in% mnames)
                res@metadata$units$conductivity <- o@metadata$conductivityUnit
            if ("temperatureUnit" %in% mnames)
                res@metadata$units$temperature <- o@metadata$temperatureUnit
        }
        if ("pressureType" %in% mnames) res@metadata$pressureType <- o@metadata$pressureType
        ## if ("scan" %in% dnames) res@data$scan <- d$scan
        ## FIXME: time goes into metadata or data ... does that make sense?
        if ("time" %in% dnames) if (length(d$time) > 1) res@data$time <- d$time else res@metadata$time <- d$time
        ## if ("quality" %in% dnames) res@data$quality <- d$quality
        ## if ("oxygen" %in% dnames) res@data$oxygen <- d$oxygen
        ## if ("nitrate" %in% dnames) res@data$nitrate <- d$nitrate
        ## if ("nitrite" %in% dnames) res@data$nitrite <- d$nitrite
        ## if ("phosphate" %in% dnames) res@data$phosphate <- d$phosphate
        ## if ("silicate" %in% dnames) res@data$silicate <- d$silicate
        if (inherits(o, 'argo')) {
            if (is.null(profile)) {
                profile <- 1
                if (dim(o[["pressure"]])[2] != 1)
                    warning("using just column 1 of matrix data; use the 'profile' argument to select a specific profile or try as.section() to keep all columns")
            }
            if (!is.numeric(profile) || length(profile) != 1 || profile < 1) {
                stop("profile must be a positive integer")
            }
            for (field in names(d)) {
                dataInField <- d[[field]]
                ## in argo objects there are both matrix (temperature,
                ## salinity, etc) and vector (time, latitude, etc)
                ## data fields. For the former we want to extract the
                ## single column. For the latter we want to extract
                ## the single value associated with that column
                if (field == "time") { # apparently POSIXct class things aren't vectors
                    res@metadata$startTime <- d[[field]][profile]
                    res@data$time <- NULL
                } else if (is.vector(dataInField)) {
                    ncol <- length(d[[field]])
                    if (profile > ncol)
                        stop("profile cannot exceed ", ncol, " for a data matrix with ", ncol, " columns")
                    if (field %in% c('longitude', 'latitude')) {
                        res@metadata[[field]] <- d[[field]][profile]
                    } else {
                        res@data[[field]] <- d[[field]][profile]
                    }
                } else if (is.matrix(dataInField)) {
                    ncol <- ncol(d[[field]])
                    if (profile > ncol)
                        stop("profile cannot exceed ", ncol, " for a data matrix with ", ncol, " columns")
                    res@data[[field]] <- d[[field]][, profile]
                } else if (is.array(dataInField)) { ## argo can sometimes come out this (odd) way
                    warning("argo data '", field, "' converted from 1-D array to 1-col matrix")
                    if (1 == length(dim(d[[field]])))
                        d[[field]] <- as.vector(d[[field]])
                    res@data[[field]] <- d[[field]]
                } else {
                    warning("not storing '", field, "' because it is in an unknown format")
                }
            }
            ## argo
        } else {
            ## oce object, not argo
            for (field in names(d)) {
                if (field != "time") {
                    res@data[[field]] <- d[[field]]
                }
            }
            if ("longitude" %in% dnames && "latitude" %in% dnames) {
                longitude <- d$longitude
                latitude <- d$latitude
                if (length(longitude) != length(latitude))
                    stop("lengths of longitude and latitude must match")
                if (length(longitude) == length(temperature)) {
                    res@data$longitude <- longitude
                    res@data$latitude <- latitude
                } else {
                    res@metadata$longitude <- longitude[1]
                    res@metadata$latitude <- latitude[1]
                }
            } else if ("longitude" %in% mnames && "latitude" %in% mnames) {
                res@metadata$longitude <- m$longitude
                res@metadata$latitude <- m$latitude
            }
        }
        res@metadata$deploymentType <- deploymentType
        res@metadata$dataNamesOriginal <- m$dataNamesOriginal
        res@processingLog <- processingLogAppend(res@processingLog, paste(deparse(match.call()), sep="", collapse=""))
        ## move e.g. salinityFlag from data slot to metadata$flags
        dataNames <- names(res@data)
        flagNameIndices <- grep(".*Flag$", dataNames)
        if (length(flagNameIndices)) {
            for (iflag in flagNameIndices) {
                fname <- gsub("Flag$", "", dataNames[iflag])
                res@metadata$flags[[fname]] <- res@data[[dataNames[iflag]]]
                res@data[[dataNames[iflag]]] <- NULL
            }
        }
    } else if (is.list(salinity) || is.data.frame(salinity)) {
        if (length(salinity) == 0)
            stop("first argument cannot be a zero-length list or data frame")
        oceDebug(debug, "salinity is a list or data frame\n")
        ## 2. coerce a data-frame or list
        x <- salinity
        if (is.list(x) && inherits(x[[1]], "oce")) {
            oceDebug(debug, "list made up of oce objects (all expected to be in same form as first)\n")
            ## Copy data over
            dataNames <- names(x[[1]]@data)
            oceDebug(debug, 'copying data entries: "', paste(dataNames, collapse='", "'), '"\n', sep="")
            for (name in dataNames) {
                res@data[[name]] <- unlist(lapply(x, function(xx) xx[[name]]))
            }
            ## If longitude and latitude are not in 'data', the next will copy from metadata (if present there)
            if (!("longitude" %in% dataNames))
                res@data$longitude <- unlist(lapply(x, function(xx) rep(xx[["longitude"]], length.out=length(xx[["salinity"]]))))
            if (!("latitude" %in% dataNames))
                res@data$latitude <- unlist(lapply(x, function(xx) rep(xx[["latitude"]], length.out=length(xx[["salinity"]]))))
            ## Flags
            if ("flags" %in% names(x[[1]]@metadata)) {
                flagNames <- names(x[[1]]@metadata$flags)
                oceDebug(debug, 'copying flag entries: "', paste(flagNames, collapse='", "'), '"\n', sep="")
                for (name in flagNames) {
                    res@metadata$flags[[name]] <- unlist(lapply(x, function(xx) xx@metadata$flags[[name]]))
                }
                res@metadata$flagScheme <- x[[1]]@metadata$flagScheme
            }
            ## Units
            res@metadata$units <- x[[1]]@metadata$units
        } else {
            oceDebug(debug, "list or data frame not made up of oce objects (as tested by first entry)\n")
            names <- names(x)
            ## Permit oce-style names or WOCE-style names for the three key variables (FIXME: handle more)
            if (3 == sum(c("salinity", "temperature", "pressure") %in% names)) {
                res@data$pressure <- x$pressure
                res@data$salinity <- x$salinity
                res@data$temperature <- x$temperature
                res@metadata$units <- units
                ##1108 res@metadata$pressureType <- pressureType
                res@metadata$pressureType <- "sea"
            } else if (3 == sum(c("PSAL", "TEMP", "PRES") %in% names)) {
                res@data$pressure <- x$PRES
                res@data$salinity <- x$PSAL
                res@data$temperature <- x$TEMP
                res@metadata$units <- units
                ##1108 res@metadata$pressureType <- pressureType
                res@metadata$pressureType <- "sea"
            } else {
                stop("the first argument must contain salinity, temperature, and pressure")
            }
            if ("longitude" %in% names)
                res@metadata$longitude <- if (1 == length(longitude)) longitude else x$longitude
            if ("latitude" %in% names)
                res@metadata$latitude <- if (1 == length(latitude)) latitude else x$latitude
            if ("conductivity" %in% names) res@data$conductivity <- x$conductivity
            if ("COND" %in% names) res@data$conductivity <- x$COND # FIXME accept other WOCE names
            if ("quality" %in% names) res@data$quality <- x$quality
            if ("oxygen" %in% names) res@data$oxygen <- x$oxygen
            if ("nitrate" %in% names) res@data$nitrate <- x$nitrate
            if ("nitrite" %in% names) res@data$nitrite <- x$nitrite
            if ("phosphate" %in% names) res@data$phosphate <- x$phosphate
            if ("silicate" %in% names) res@data$silicate <- x$silicate
            if ("time" %in% names) res@data$time <- x$time
        }
    } else {
        oceDebug(debug, "salinity, temperature, pressure (etc) supplied\n")
        ## 3. explicit mode
        ##1108 if (missing(temperature) && missing(CT)) stop("must give temperature or CT")
        if (missing(temperature)) stop("must give temperature")
        if (missing(pressure)) stop("must give pressure")
        if (!missing(units))
            res@metadata$units <- units
        ##1108 res@metadata$pressureType <- pressureType
        res@metadata$pressureType <- "sea"
        salinity <- as.vector(salinity)
        temperature <- as.vector(temperature)
        pressure <- as.vector(pressure)
        if (!missing(pressureAtmospheric))
            pressure <- pressure - pressureAtmospheric
        ##1108 haveSA <- !missing(SA)
        ##1108 haveCT <- !missing(CT)
        ##1108 if (haveSA != haveCT)
        ##1108     stop("SA and CT must both be supplied, if either is")
        ##1108 if (!missing(SA)) {
        ##1108     n <- length(SA)
        ##1108     if (length(CT) != n)
        ##1108         stop("lengths of SA and CT must match")
        ##1108     if (missing(longitude)) {
        ##1108         longitude <- rep(300, n)
        ##1108         latitude <- rep(0, n)
        ##1108         warning("longitude and latitude set to default values, since none given")
        ##1108     }
        ##1108     salinity <- gsw::gsw_SP_from_SA(SA, pressure, longitude, latitude)
        ##1108     temperature <- gsw::gsw_t_from_CT(SA, CT, pressure)
        ##1108 }
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
                     pressure=pressure)
        if (!missing(conductivity)) data$conductivity <- as.vector(conductivity)
        ##1108 if (!missing(quality)) data$quality <- quality
        ##1108 if (!missing(oxygen)) data$oxygen <- oxygen
        ##1108 if (!missing(nitrate)) data$nitrate <- nitrate
        ##1108 if (!missing(nitrite)) data$nitrite <- nitrite
        ##1108 if (!missing(phosphate)) data$phosphate <- phosphate
        ##1108 if (!missing(silicate)) data$silicate <- silicate
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
            warning("the 'other' argument will be removed soon; use oceSetData() instead, as in the examples. See ?'oce-deprecated'.")
        }
        ## Handle missing value code (changes on July 24, 2016 fix issue 1028)
        if (!is.null(missingValue)) {
            for (dname in names(data)) {
                bad <- data[[dname]] == missingValue
                data[[dname]][bad] <- NA
            }
        }
        ##20150712 if (is.na(waterDepth)) {
        ##20150712     waterDepth <- max(abs(data$pressure), na.rm=TRUE)
        ##20150712     res@processingLog <- processingLogAppend(res@processingLog,
        ##20150712                                              "inferred water depth from maximum pressure")
        ##20150712 }
        names <- names(data)
        ##labels <- titleCase(names) # paste(toupper(substring(names,1,1)),substring(names,2),sep="")
        if (length(longitude) != length(latitude))
            stop("lengths of longitude and latitude must match, but they are ",
                 length(longitude), " and ", length(latitude), ", respectively")
        if (1 < length(longitude) && length(longitude) != length(salinity))
            stop("lengths of salinity and longitude must match but they are ",
                 length(longitude), " and ", length(salinity), ", respectively")
        ## FIXME: should sampleInterval be a default?
        ##res@metadata$names <- names
        ##res@metadata$labels <- labels
        res@metadata$filename <- filename
        res@metadata$ship <- ship
        ##1108 res@metadata$scientist <- scientist
        ##1108 res@metadata$institute <- institute
        ##1108 res@metadata$address <- address
        res@metadata$cruise <- cruise
        res@metadata$station <- station
        ##1108 res@metadata$date <- date
        res@metadata$startTime <- startTime
        ##1108 res@metadata$recovery <- recovery
        res@metadata$type <- type
        ##1108 res@metadata$model <- model
        res@metadata$serialNumber <- serialNumber
        ##1108 res@metadata$src <- src
        res@metadata$deploymentType <- deploymentType
        ## If lon and lat are vectors, place in data, with averages in metadata.
        if (length(latitude) == 1) {
            res@metadata$latitude <- latitude
        } else if (length(latitude) > 1) {
            if (length(latitude) != length(temperature))
                stop("lengths of latitude and temperature must match")
            data$latitude <- latitude
        }
        if (length(longitude) == 1) {
            res@metadata$longitude <- longitude
        } else if (length(longitude) > 1) {
            if (length(longitude) != length(temperature))
                stop("lengths of longitude and temperature must match")
            data$longitude <- longitude
        }
        res@data <- data
    }
    if (!is.null(ounits)) {
        oceDebug(debug, "copying units from first argument\n")
        res@metadata$units <- ounits
    } else if (!unitsGiven) {
        oceDebug(debug, "assuming standard modern units, since none provide or available in first argument\n")
        ## guess on units
        names <- names(res@data)
        if ("salinity" %in% names)
            res@metadata$units$salinity <- list(unit=expression(), scale="PSS-78")
        if ("temperature" %in% names)
            res@metadata$units$temperature <- list(unit=expression(degree*C), scale="ITS-90")
        if ("pressure" %in% names)
            res@metadata$units$pressure <- list(unit=expression(dbar), scale="")
    }
    ## the 'units' argument takes precedence over guesses
    dataNames <- names(res@data)
    unitsNames <- names(units)
    if (!is.null(flags))
        res@metadata$flags <- flags

    ## Default some units (FIXME: this may be a bad idea)
    if (is.null(res@metadata$units)) {
        if ("salinity" %in% dataNames && !("salinity" %in% unitsNames))
            res@metadata$units$salinity <- list(unit=expression(), scale="PSS-78")
        if ("temperature" %in% dataNames && !("temperature" %in% unitsNames))
            res@metadata$units$temperature <- list(unit=expression(degree*C), scale="ITS-90")
        if ("pressure" %in% dataNames && !("pressure" %in% unitsNames))
            res@metadata$units$pressure <- list(unit=expression(dbar), scale="")
    }
    ## FIXME: setting waterDepth can have tricky results ... we've had issues with this
    if (is.na(res@metadata$waterDepth) && !is.na(waterDepth))
        res@metadata$waterDepth <- waterDepth
    ## Remove lon and lat form metadata, if they are in data. This is so plot() will
    ## show multiple stations, as can be the case in converting from multi-station
    ## data.
    if ("longitude" %in% names(res@metadata) && "longitude" %in% names(res@data) &&
        "latitude" %in% names(res@metadata) && "latitude" %in% names(res@data))
    {
        res@metadata$longitude <- NULL
        res@metadata$latitude <- NULL
    }
    res@processingLog <- processingLogAppend(res@processingLog, paste(deparse(match.call()), sep="", collapse=""))
    oceDebug(debug, "} # as.ctd()\n", sep="", unindent=1, style="bold")
    res
}


#' Decimate a CTD profile
#'
#' Interpolate a CTD profile to specified pressure values.  This is used
#' by [sectionGrid()], but is also useful for dealing with individual
#' CTD/bottle profiles.
#'
#' The `"approx"` and `"approxML"` methods may be best for bottle data,
#' in which the usual task is
#' to interpolate from a coarse sampling grid to a finer one. The distinction
#' is that `"approxML"` assumes a mixed-layer above the top sample value. For CTD data, the
#' `"boxcar"` method may be the preferred choice, because the task is normally
#' to sub-sample, and some degree of smoothing is usually desired.  (The
#' `"lm"` method can be quite slow, and its results may be quite similar to those of the
#' boxcar method.)
#'
#' For widely-spaced data, a sort of numerical cabeling effect can result when
#' density is computed based on interpolated salinity and temperature.
#' See reference 2 for a discussion of this issue and possible solutions.
#'
#' @template flagDeletionTemplate
#'
#' @param x a [ctd-class] object.
#'
#' @param p pressure increment, or vector of pressures.  In the first case,
#' pressures from 0dbar to the rounded maximum pressure are used, incrementing by
#' `p` dbars.  If a vector of pressures is given, interpolation is done to
#' these pressures.
#'
#' @param method the method to be used for calculating decimated values.  This may
#' be a function or a string naming a built-in method.  The built-in methods are
#' as follows.
#'
#' * `"boxcar"` (based on a local average)
#'
#' * `"approx"` (based on linear
#' interpolation between neighboring points, using [approx()]
#' with the `rule` argument specified here)
#'
#' * `"approxML"` as `"approx"`,
#' except that a mixed layer is assumed to apply above the top data value; this
#' is done by setting the `yleft` argument to [approx()], and
#' by calling that function with `rule=c(2,1))`
#'
#' * `"lm"` (based on local
#' regression, with `e` setting the size of the local region);
#'
#' * `"rr"` for the Reiniger and Ross method, carried out with [oce.approx()];
#'
#' * `"unesco"` (for the UNESCO method, carried out with [oce.approx()].
#'
#' On the other hand, if `method` is a function, then it must take
#' three arguments, the first being pressure, the second being an arbitrary
#' variable in another column of the data, and the third being a vector of target
#' pressures at which the calculation is carried out, and the return value must be
#' a vector.  See \dQuote{Examples}.
#'
#' @param rule an integer that is passed to [approx()], in the
#' case where `method` is `"approx"`. Note that the default value
#' for `rule` is 1, which will inhibit extrapolation beyond the observed
#' pressure range. This is a change from the behaviour previous to May 8, 2017,
#' when a `rule` of 2 was used (without stating so as an argument).
#'
#' @param e is an expansion coefficient used to calculate the local neighbourhoods
#' for the `"boxcar"` and `"lm"` methods.  If `e=1`, then the
#' neighbourhood for the i-th pressure extends from the (`i-1`)-th pressure to
#' the (`i+1`)-th pressure.  At the endpoints it is assumed that the outside
#' bin is of the same pressure range as the first inside bin.  For other values of
#' `e`, the neighbourhood is expanded linearly in each direction.  If the
#' `"lm"` method produces warnings about "prediction from a rank-deficient
#' fit", a larger value of `"e"` should be used.
#'
#' @template debugTemplate
#'
#' @return A [ctd-class] object, with pressures that are as set by
#' the `"p"` parameter and all other properties modified appropriately.
#'
#' @seealso The documentation for [ctd-class] explains the structure of
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
#' ctd3 <- ctdDecimate(ctd, p=p, method=function(x, y, xout)
#'                     predict(smooth.spline(x, y, df=30), xout)$y)
#' lines(ctd3[["salinity"]], ctd3[["pressure"]], col="red")
#'
#'
#' @references
#' 1. R.F. Reiniger and C.K. Ross, 1968.  A method of interpolation with
#' application to oceanographic data.  *Deep Sea Research*, **15**,
#' 185-193.
#'
#' 2. Oguma, Sachiko, Toru Suzuki, Yutaka Nagata, Hidetoshi Watanabe, Hatsuyo Yamaguchi,
#' and Kimio Hanawa. \dQuote{Interpolation Scheme for Standard Depth Data Applicable for Areas
#' with a Complex Hydrographical Structure.} Journal of Atmospheric and Oceanic Technology
#' 21, no. 4 (April 1, 2004): 704-15.
#'
#' @author Dan Kelley
#'
#' @family things related to ctd data
ctdDecimate <- function(x, p=1, method="boxcar", rule=1, e=1.5, debug=getOption("oceDebug"))
{
    methodFunction <- is.function(method)
    if (!methodFunction) {
        methods <- c("boxcar", "approx", "approxML", "lm", "rr", "unesco")
        imethod <- pmatch(method, methods, nomatch=0)
        if (imethod > 0) method <- methods[imethod] else
            stop('unknown method "', method, '"; valid methods are: "', paste(methods, collapse='", "'), '"')
    }
    warningMessages <- NULL
    oceDebug(debug, "ctdDecimate(x, p, method=",
             if (methodFunction) "(a function)" else paste('"', method, '"', sep=""),
             ", rule=c(", paste(rule, collapse=","), ")",
             ", e=", e,
             "\", ...) {\n", sep="", unindent=1)
    ## if (!inherits(x, "ctd"))
    ##     stop("method is only for objects of class '", "ctd", "'")
    res <- x
    res[["flags"]] <- NULL
    warningMessages <- c(warningMessages,
                         "Removed flags from decimated ctd object")
    n <- length(x[["pressure"]])
    if (n < 2) {
        warning("too few data to ctdDecimate()")
        return(res)
    }
    ## Figure out pressure targets, pt
    if (length(p) == 1) {
        pt <- seq(0, p * floor(max(x[["pressure"]], na.rm=TRUE) / p), p)
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
                    dataNew[[datumName]] <- method(pressure, x@data[[datumName]], pt)
                }
            }
        }
        dataNew[["pressure"]] <- pt
    } else {
        if (method == "approxML") {
            numGoodPressures <- sum(!is.na(x[["pressure"]]))
            if (numGoodPressures > 0)
                tooDeep <- pt > max(x@data[["pressure"]], na.rm=TRUE)
            for (datumName in dataNames) {
                ## oceDebug(debug, "decimating \"", datumName, "\"\n", sep="")
                if (numGoodPressures < 2 || !length(x[[datumName]])) {
                    dataNew[[datumName]] <- rep(NA, npt)
                } else {
                    if (datumName != "pressure") {
                        wgood <- which(is.finite(x@data[[datumName]]))
                        good <- length(wgood)
                        if (good > 2) {
                            dataNew[[datumName]] <- approx(x@data[["pressure"]], x@data[[datumName]], pt, rule=c(2, 1), yleft=x@data[[datumName]][wgood[1]])$y
                            ##.message("yleft=", x@data[[datumName]][wgood[1]])
                            dataNew[[datumName]][tooDeep] <- NA
                        } else {
                            dataNew[[datumName]] <- rep(NA, npt)
                        }
                    }
                }
            }
        } else if (method == "approx") {
            numGoodPressures <- sum(!is.na(x[["pressure"]]))
            if (numGoodPressures > 0)
                tooDeep <- pt > max(x@data[["pressure"]], na.rm=TRUE)
            for (datumName in dataNames) {
                ## oceDebug(debug, "decimating \"", datumName, "\"\n", sep="")
                if (numGoodPressures < 2 || !length(x[[datumName]])) {
                    dataNew[[datumName]] <- rep(NA, npt)
                } else {
                    if (datumName != "pressure") {
                        good <- sum(!is.na(x@data[[datumName]]))
                        if (good > 2) {
                            dataNew[[datumName]] <- approx(x@data[["pressure"]], x@data[[datumName]], pt, rule=rule)$y
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
                    if (datumName != "pressure" && datumName != "flag") {
                        if (all(is.na(x@data[[datumName]]))) {
                            dataNew[[datumName]] <- rep(NA, npt)
                        } else {
                            dataNew[[datumName]] <- binMean1D(p, x@data[[datumName]], xbreaks=pbreaks)$result
                        }
                    }
                }
            }
        } else {
            for (i in 1:npt) {
                if (i==1) {
                    focus <- (x[["pressure"]] >= (pt[i] - e * (pt[i+1] - pt[ i ]))) &
                    (x[["pressure"]] <= (pt[i] + e * (pt[i+1] - pt[ i ])))
                } else if (i == npt) {
                    focus <- (x[["pressure"]] >= (pt[i] - e * (pt[ i ] - pt[i-1]))) &
                    (x[["pressure"]] <= (pt[i] + e * (pt[ i ] - pt[i-1])))
                } else {
                    focus <- (x[["pressure"]] >= (pt[i] - e * (pt[ i ] - pt[i-1]))) &
                    (x[["pressure"]] <= (pt[i] + e * (pt[i+1] - pt[ i ])))
                }
                if (sum(focus, na.rm=TRUE) > 0) {
                    if ("boxcar" == method) {
                        for (datumName in dataNames) {
                            if (!length(x[[datumName]])) {
                                dataNew[[datumName]] <- NULL
                            } else {
                                if (datumName != "pressure") {
                                    dataNew[[datumName]][i] <- mean(x@data[[datumName]][focus], na.rm=TRUE)
                                }
                            }
                        }
                    } else if ("lm" == method) {
                        ## FIXME: this is far too slow
                        xvar <- x@data[["pressure"]][focus]
                        for (datumName in dataNames) {
                            if (!length(x[[datumName]])) {
                                dataNew[[datumName]] <- NULL
                            } else {
                                if (datumName != "pressure") {
                                    yvar <- x@data[[datumName]][focus]
                                    m <- try(lm(yvar ~ xvar), silent=TRUE)
                                    if (class(m) != "try-error")
                                        dataNew[[datumName]][i] <- predict(m, newdata=list(xvar=pt[i]))
                                    else
                                        dataNew[[datumName]][i] <- NA
                                }
                            }
                        }
                    } else {
                        stop("impossible to get here -- developer error")
                    }
                } else {
                    ## No data in the focus region
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
    ##1108 if ('scan' %in% names(dataNew)) {
    ##1108     dataNew[['scan']] <- NULL
    ##1108     warningMessages <- c(warningMessages, "Removed scan field from decimated ctd object")
    ##1108 }
    if ('flag' %in% names(dataNew)) {
        dataNew[['flag']] <- NULL
        warningMessages <- c(warningMessages, "Removed flag field from decimated ctd object")
    }
    dataNew[["pressure"]] <- pt
    ## convert any NaN to NA
    for (i in seq_along(dataNew)) {
        dataNew[[i]][is.nan(dataNew[[i]])] <- NA
    }
    ##message("ctd.R:733 dataNew[['pressure']]: ", paste(dataNew[['pressure']], collapse=" "))
    res@data <- dataNew
    res@processingLog <- processingLogAppend(res@processingLog, paste(deparse(match.call()), sep="", collapse=""))
    for (w in warningMessages)
        res@processingLog <- processingLogAppend(res@processingLog, w)
    oceDebug(debug, "} # ctdDecimate()\n", unindent=1)
    res
}

#' Find Profiles within a Tow-Yow CTD Record
#'
#' Examine the pressure record looking for extended periods of either ascent or descent, and return
#' either indices to these events or a vector of CTD records containing the events.
#'
#' The method works by examining the pressure record.  First, this is smoothed using
#' `smoother()` (see \dQuote{Arguments}), and then the result is first-differenced
#' using [diff()].  Median values of the positive and
#' negative first-difference values are then multiplied by `cutoff`.  This establishes criteria
#' for any given point to be in an ascending profile, a descending profile, or a non-profile.
#' Contiguous regions are then found, and those that have fewer than `minLength` points are
#' discarded.  Then, those that have pressure ranges less than `minHeight` are discarded.
#'
#' Caution: this method is not well-suited to all datasets. For example, the default
#' value of `smoother` is [smooth.spline()], and this works well for just a few
#' profiles, but poorly for a tow-yo with a long sequence of profiles; in the latter case,
#' it can be preferable to use simpler smoothers (see \dQuote{Examples}). Also, depending
#' on the sampling protocol, it is often necessary to pass the resultant profiles through
#' [ctdTrim()], to remove artifacts such as an equilibration phase, etc.
#' Generally, one is well-advised to use the present function for a quick look at the data,
#' relying on e.g. [plotScan()] to identify profiles visually, for a final product.
#'
#' @param x a [ctd-class] object.
#'
#' @param cutoff criterion on pressure difference; see \dQuote{Details}.
#'
#' @param minLength lower limit on number of points in candidate profiles.
#'
#' @param minHeight lower limit on height of candidate profiles.
#'
#' @param smoother The smoothing function to use for identifying down/up
#' casts. The default is `smooth.spline`, which performs well for
#' a small number of cycles; see \dQuote{Examples} for a method that is
#' better for a long tow-yo. The return value from `smoother` must
#' be either a list containing an element named `y` or something
#' that can be coerced to a vector with [as.vector()]. To
#' turn smoothing off, so that cycles in pressure are determined by
#' simple first difference, set `smoother` to `NULL`.
#'
#' @param direction String indicating the travel direction to be selected.
#'
#' @param breaks optional integer vector indicating the indices of last
#' datum in each profile stored within `x`. Thus, the first profile
#' in the return value will contain the `x` data from indices 1
#' to `breaks[1]`.  If `breaks` is given, then all
#' other arguments except `x` are ignored. Using `breaks`
#' is handy in cases where other schemes fail, or when the author
#' has independent knowledge of how the profiles are strung together
#' in `x`.
#'
#' @param arr.ind Logical indicating whether the array indices should be returned;
#' the alternative is to return a vector of ctd objects.
#'
#' @param distinct An optional string indicating how to identify profiles
#' by unique values. Use `"location"`
#' to find profiles by a change in longitude and latitude, or use the name of any
#' of item in the `data` slot in `x`. In these cases, all the
#' other arguments except `x` are ignored. However, if `distinct`
#' is not supplied, the other arguments are handled as described above.
#'
#' @template debugTemplate
#'
#' @param ... Optional extra arguments that are passed to the smoothing function, `smoother`.
#'
#' @return If `arr.ind=TRUE`, a data frame with columns `start` and `end`, the indices
#' of the downcasts.  Otherwise, a vector of `ctd` objects. In this second case,
#' the station names are set to a form like `"10/3"`, for the third profile within an
#' original ctd object with station name `"10"`, or to `"3"`, if the original
#' ctd object had no station name defined.
#'
#' @seealso The documentation for [ctd-class] explains the structure
#' of CTD objects, and also outlines the other functions dealing with them.
#'
#' @examples
#' library(oce)
# These examples cannot be tested, because they are based on
# data objects that are not provided with oce.
#'
#'\dontrun{
#' # Example 1. Find profiles within a towyo file, as can result
#' # if the CTD is cycled within the water column as the ship
#' # moves.
#' profiles <- ctdFindProfiles(towyo)
#'}
#'
#'\dontrun{
#' # Example 2. Use a moving average to smooth pressure, instead of the
#' # default smooth.spline() method. This might avoid a tendency of
#' # the default scheme to miss some profiles in a long towyo.
#' movingAverage <- function(x, n = 11, ...)
#' {
#'     f <- rep(1/n, n)
#'     stats::filter(x, f, ...)
#' }
#' casts <- ctdFindProfiles(towyo, smoother=movingAverage)
#'}
#'
#'\dontrun{
#' # Example 3: glider data read into a ctd object. Chop
#' # into profiles by looking for pressure jumps exceeding
#' # 10 dbar.
#' breaks <- which(diff(gliderAsCtd[["pressure"]]) > 10)
#' profiles <- ctdFindProfiles(gliderAsCtd, breaks=breaks)
#'}
#'
#' @author Dan Kelley and Clark Richards
#'
#' @family things related to ctd data
ctdFindProfiles <- function(x, cutoff=0.5, minLength=10, minHeight=0.1*diff(range(x[["pressure"]])),
                            smoother=smooth.spline,
                            direction=c("descending", "ascending"),
                            breaks,
                            arr.ind=FALSE,
                            distinct,
                            debug=getOption("oceDebug"), ...)
{
    oceDebug(debug, "ctdFindProfiles(x, cutoff=", cutoff,
             ", minLength=", minLength,
             ", minHeight=", minHeight,
             ", direction=\"", direction, "\"",
             ", breaks=", if (missing(breaks)) "unspecified" else "specified",
             ", arr.ind=", arr.ind, ", debug=", debug, ") {\n", sep="", unindent=1)
    if (!inherits(x, "ctd"))
        stop("method is only for objects of class '", "ctd", "'")
    if (!missing(distinct)) {
        if (distinct == "location") {
            lon <- x[["longitude"]]
            lat <- x[["latitude"]]
            dist <- geodDist(lon, lat, alongPath=TRUE)
            i <- seq_along(x[["salinity"]])
            stnIndices <- split(i, factor(dist))
            nstn <- length(stnIndices)
            oceDebug(debug, "number of profiles found:", nstn, "\n")
            if (!nstn)
                return(NULL)
            casts <- vector("list", nstn)
            for (i in 1:nstn)
                casts[[i]] <- ctdTrim(x, "index", parameters=range(stnIndices[i]))
            oceDebug(debug, "} # ctdFindProfiles()\n", sep="", unindent=1)
            return(casts)
        } else if (distinct %in% names(x@data)) {
            u <- unique(x[[distinct]])
            nstn <- length(u)
            oceDebug(debug, "number of profiles found:", nstn, "\n")
            if (!nstn)
                return(NULL)
            casts <- vector("list", nstn)
            for (i in 1:nstn)
                casts[[i]] <- ctdTrim(x, method="index", parameters=x[[distinct]]==u[i])
            oceDebug(debug, "} # ctdFindProfiles()\n", sep="", unindent=1)
            return(casts)
        } else {
            stop("'distinct' not understood; it should be \"distance\" or something in names(x[[\"data\"]]")
        }
    } # else rest of code

    if (missing(breaks)) {
        ## handle case where 'breaks' was not given
        direction <- match.arg(direction)
        pressure <- fillGap(x[["pressure"]], rule=2)
        ps <- if (is.null(smoother)) pressure else smoother(pressure, ...)
        if (is.list(ps) && "y" %in% names(ps))
            ps <- ps$y
        ps <- as.numeric(ps) # discard return class of e.g. smooth()
        nps <- length(ps)
        dp <- diff(ps)
        dp <- c(dp[1], dp)
        dp <- fillGap(dp, rule=2)
        if (direction == "descending") {
            look <- dp > cutoff * median(dp[dp>0], na.rm=TRUE)
        } else if (direction == "ascending") {
            look <- dp < cutoff * median(dp[dp<0], na.rm=TRUE)
        } else {
            stop("direction must be either \"ascending\" or \"descending\"") # cannot reach here
        }
        ### if (debug>100) {                   # HIDDEN feature, may be removed at any time
        ###     par(mar=c(3,3,1,1),mgp=c(2,0.7,0))
        ###     plot(pressure,type='l')
        ###     lines(ps,col=3, lty='dotted')
        ###     mtext(direction, side=3, adj=1, line=0)
        ### }

        start <- which(diff(look) == 1)
        ## the first data are often a downcast, after all!
        if (look[1] && length(start) > 0 && start[1] != 1)
            start <- c(1, start)
        if (0 == length(start))
            start <- 1
        end <- which(diff(look) == -1)
        if (look[nps] && length(end) > 0 && end[length(end)] != 1)
            end <- c(end, nps)

        if (0 == length(end))
            end <- length(pressure)
        if (start[1] > end[1])
            start <- start[-1]
        oceDebug(debug, "start:", head(start), "... (before trimming)\n")
        oceDebug(debug, "end:", head(end), "... (before trimming)\n")
        start <- subset(start, start<max(end))
        end <- subset(end, end>min(start))
        oceDebug(debug, "start:", head(start), "... (after trimming)\n")
        oceDebug(debug, "end:", head(end), "... (after trimming)\n")
        if (length(end) > length(start))
            end <- end[seq_along(start)]
        keep <- abs(end - start) >= minLength
        oceDebug(debug, "start:", head(start[keep]), "... (using minLength)\n")
        oceDebug(debug, "end:", head(end[keep]), "... (using minLength)\n")
        psfilled <- fillGap(ps, rule=2)
        keep <- keep & (abs(psfilled[end] - psfilled[start]) >= minHeight)
        oceDebug(debug, "heights:", head(psfilled[end]-psfilled[start]), "...; compare with minHeight=", head(minHeight), "...\n")
        oceDebug(debug, "start:", head(start[keep]), "... (using minHeight)\n")
        oceDebug(debug, "end:", head(end[keep]), "... (using minHeight)\n")
        indices <- data.frame(start=start[keep], end=end[keep])
    } else {
        ## Obey user-defined breaks but note what we do by adding and subtracting
        ## 1 just after ... this is because of what we do with "e", ~13 lines below.
        indices <- data.frame(start=c(1, breaks+1), end=c(breaks, length(x[['pressure']])))
        indices$start <- indices$start + 1
        indices$end <- indices$end - 1
    }

    if (is.logical(arr.ind) && arr.ind) {
        oceDebug(debug, "} # ctdFindProfiles()\n", sep="", unindent=1)
        return(indices)
    } else {
        ncasts <- length(indices$start)
        casts <- vector("list", ncasts)
        npts <- length(x@data$pressure)
        for (i in 1:ncasts) {
            oceDebug(debug, "profile", i, "of", ncasts, "\n")
            ## Extend indices to catch turnaround spots (see ~13 lines above)
            e <- 1
            iStart <- max(1L, indices$start[i] - e)
            iEnd <- min(npts, indices$end[i] + e)
            cast <- ctdTrim(x, "index", parameters=c(iStart, iEnd))
            if (!is.null(x@metadata$station) && "" != x@metadata$station) {
                cast@metadata$station <- paste(x@metadata$station, i, paste="/")
            } else {
                cast@metadata$station <- i
            }
            cast@processingLog <- processingLogAppend(cast@processingLog,
                                                      paste(paste(deparse(match.call()), sep="", collapse=""),
                                                            " # profile ", i, " of ", ncasts))
            casts[[i]] <- cast
        }
        oceDebug(debug, "} # ctdFindProfiles()\n", sep="", unindent=1)
        return(casts)
    }
}

#' Trim Beginning and Ending of a CTD cast
#'
#' Often in CTD profiling, the goal is to isolate only the downcast, discarding measurements made in
#' the air, in an equilibration phase in which the device is held below the water surface, and then the
#' upcast phase that follows the downcast.  This is handled reasonably well by `ctdTrim` with
#' `method="downcast"`, although it is almost always best to use [plotScan()] to
#' investigate the data, and then use the `method="index"` or `method="scan"` method based on
#' visual inspection of the data.
#'
#' `ctdTrim` begins by examining the pressure differences between subsequent samples. If
#' these are all of the same value, then the input `ctd` object is returned, unaltered.
#' This handles the case of pressure-binned data. However, if the pressure difference
#' varies, a variety of approaches are taken to trimming the dataset.
#'
#' * If `method[1]` is `"downcast"` then an attempt is made to keep only data for
#' which the CTD is descending.  This is done in stages, with variants based on `method[2]`, if
#' supplied.
#'
#'   1. The pressure data are despiked with a smooth() filter with method "3R".
#'      This removes wild spikes that arise from poor instrument connections, etc.
#'
#'   2. *Step 2.* If no `parameters` are given, then any data with negative pressures
#'      are deleted.  If there is a parameter named `pmin`, then that pressure (in decibars)
#'      is used instead as the lower limit. This is a commonly-used setup, e.g.
#'      `ctdTrim(ctd, parameters=list(pmin=1))` removes the top decibar (roughly 1m) from
#'      the data.  Specifying `pmin` is a simple way to remove near-surface
#'      data, such as a shallow equilibration phase, and if specified will cause `ctdTrim`
#'      to skip step 4 below.
#'
#'   3. The maximum pressure is determined, and data acquired subsequent to
#'      that point are deleted.  This removes the upcast and any subsequent data.
#'
#'   4. If the `pmin` parameter is not specified, an attempt is made to remove an initial
#'      equilibrium phase by a regression of pressure on scan number.  There are three
#'      variants to this, depending on the value of the second `method` element.
#'      If `method` is `"A"` (or not given), the procedure is to
#'      call [nls()] to fit a piecewise linear model of pressure as a function of scan,
#'      in which pressure is constant for scan less than a critical value, and then
#'      linearly varying for with scan. This is  meant to handle the common situation
#'      in which the CTD is held at roughly constant depth (typically
#'      a metre or so) to equilibrate, before it is lowered through the water column.
#'      If `method` is `"B"`, the procedure is similar, except that the pressure
#'      in the surface region is taken to be zero (this does not make
#'      much sense, but it might help in some cases). Note that, prior to early 2016,
#'      method `"B"` was called method `"C"`; the old `"B"` method was judged useless
#'      and so it was removed.
#'
#' * If `method="upcast"`, a sort of reverse of `"downcast"` is used. This
#'   was added in late April 2017 and has not been well tested yet.
#'
#' * If `method="sbe"`, a method similar to that described
#'   in the SBE Data Processing manual is used to remove the "soak"
#'   period at the beginning of a cast (see Section 6 under subsection
#'   "Loop Edit"). The method is based on the soak procedure whereby
#'   the instrument sits at a fixed depth for a period of time, after
#'   which it is raised toward the surface before beginning the actual
#'   downcast. This enables equilibration of the sensors while still
#'   permitting reasonably good near-surface data. Parameters for the
#'   method can be passed using the `parameters` argument, which
#'   include `minSoak` (the minimum depth for the soak) and
#'   `maxSoak` the maximum depth of the soak. The method finds
#'   the minimum pressure prior to the `maxSoak` value being
#'   passed, each of which occurring after the scan in which the
#'   `minSoak` value was reached. For the method to work, the
#'   pre-cast pressure minimum must be less than the `minSoak`
#'   value. The default values of `minSoak` and `maxSoak`
#'   are 1 and 20 dbar, respectively.
#'
#' * If `method="index"` or `"scan"`, then each column of data is subsetted according to the
#'   value of `parameters`. If the latter is a logical vector of length matching data column
#'   length, then it is used directly for subsetting. If `parameters` is a numerical vector with
#'   two elements, then the index or scan values that lie between `parameters[1]`
#'   and `parameters[2]` (inclusive) are used for subsetting.  The
#'   two-element method is probably the most useful, with the values being determined by visual
#'   inspection of the results of [plotScan()]. While this may take a minute or two, the
#'   analyst should bear in mind that a deep-water CTD profile might take 6 hours, corresponding to
#'   ship-time costs exceeding a week of salary.
#'
#' * If `method="range"` then data are selected based on the value of the column named
#'   `parameters$item`.  This may be by range or by critical value.  By range: select values
#'   between `parameters$from` (the lower limit) and `parameters$to` (the upper limit) By
#'   critical value: select if the named column exceeds the value.  For example, \code{ctd2 <-
#'     ctdTrim(ctd, "range", parameters=list(item="scan", from=5))} starts at scan number 5 and
#'   continues to the end, while
#'   `ctdTrim(ctd,"range",parameters=list(item="scan",from=5,to=100))` also starts at scan 5,
#'   but extends only to scan 100.
#'
#' * If `method` is a function, then it must return a vector of [logical()]
#'   values, computed based on two arguments: `data` (a
#'   [list()]), and `parameters` as supplied to `ctdTrim`.  Both
#'   `inferWaterDepth` and `removeInversions` are ignored in the function case. See
#'   \dQuote{Examples}.
#'
#' @param x a [ctd-class] object.
#'
#' @param method A string (or a vector of two strings) specifying the trimming method, or a function to
#' be used to determine data indices to keep.  If `method` is not provided, `"downcast"` is
#' assumed. See \dQuote{Details}.
#'
#' @param removeDepthInversions Logical value indicating whether to remove any levels at which depth is
#' less than, or equal to, a depth above.  (This is needed if the object is to be assembled into a
#' section, unless [ctdDecimate()] will be used, which will remove the inversions.
#'
#' @param parameters A list whose elements depend on the method; see \dQuote{Details}.
#'
#' @param indices Logical value indicating what to return. If `indices=FALSE` (the default),
#' then the return value is a subsetted [ctd-class] object. If `indices=TRUE`,
#' then the return value is a logical vector that could be used to subset the data
#' with [subset,ctd-method()] or to set data-quality flags.
#'
#' @template debugTemplate
#'
#' @return Either a [ctd-class] object of or a logical vector of length
#' matching the data. The first option is the default. The second option,
#' achieved by setting `indices=FALSE`, may be useful in constructing
#' data flags to be inserted into the object.
#'
#' @examples
#'\dontrun{
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
#'}
#'
#' @references
#' The Seabird CTD instrument is described at
#' `http://www.seabird.com/products/spec_sheets/19plusdata.htm`.
#'
#' Seasoft V2: SBE Data Processing, SeaBird Scientific, 05/26/2016
#'
#' @author Dan Kelley and Clark Richards
#'
#' @family things related to ctd data
ctdTrim <- function(x, method, removeDepthInversions=FALSE, parameters=NULL,
                    indices=FALSE, debug=getOption("oceDebug"))
{
    oceDebug(debug, "ctdTrim() {\n", unindent=1)
    methodIsFunction <- !missing(method) && is.function(method)
    if (!inherits(x, "ctd"))
        stop("method is only for objects of class '", "ctd", "'")
    pressure <- fillGap(x[["pressure"]], rule=2)
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
                method <- method[1]
                submethod <- "A"
            } else if (length(method) == 2) {
                submethod <- method[2]
                method <- method[1]
            } else {
                stop("if provided, 'method' must be of length 1 or 2")
            }
        }
        method <- match.arg(method, c("downcast", "upcast", "index", "scan", "range", "sbe"))
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
            max.pressure <- smooth(pressure, kind="3R")[max.location]
            keep[max.location:n] <- FALSE
            oceDebug(debug, "removed data at indices from ", max.location,
                     " (where pressure is ", pressure[max.location], ") to the end of the data\n", sep="")
            if (!pminGiven) {
                ## new method, after Feb 2008
                submethodChoices <- c("A", "B")
                sm <- pmatch(submethod, submethodChoices)
                if (is.na(sm))
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
                ## bilinearA<-function(s, s0, p0, dpds) { # same model as B but results treated differently
                ##     oceDebug(debug-1, "bilinearA s0=", s0, "p0=", p0, "dpds=", dpds, "\n")
                ##     ifelse(s < s0, p0, p0+dpds * (s-s0))
                ## }
                ## bilinearB<-function(s, s0, dpds) {
                ##     oceDebug(debug-1, "bilinearB s0=", s0, "dpds=", dpds, "\n")
                ##     ifelse(s < s0, 0, dpds * (s-s0))
                ## }
                pp <- pressure[keep]
                pp <- despike(pp) # some, e.g. data(ctdRaw), have crazy points in air
                ss <- x[["scan"]][keep]
                end <- which(smooth(pp) > 1/2*max.pressure)[1]
                if (!is.na(end)) {
                    pp <- pp[1:end]
                    ss <- ss[1:end]
                }
                s0 <- ss[0.25*length(ss)]
                p0 <- pp[1]
                ##p1 <- max(pp) #pp[0.9*length(pp)]
                if (length(ss) > 2)
                    dpds0 <-  diff(range(pp, na.rm=TRUE)) / diff(range(ss, na.rm=TRUE))
                else
                    dpds0 <- 0
                ## Handle submethods.
                if (submethod == "A") {
                    oceDebug(debug, "method[2]=\"A\"\n")
                    m <- try(nls(pp ~ function(ss, s0, p0, dpds)
                                 {
                                     oceDebug(debug-1, "bilinearA s0=", s0, "p0=",
                                              p0, "dpds=", dpds, "\n")
                                     ifelse(s < s0, p0, p0+dpds * (s-s0))
                                 },
                                 start=list(s0=s0, p0=0, dpds=dpds0)), silent=TRUE)
                    scanStart <- if (class(m) == "try-error") 1 else max(1, floor(0.5 + coef(m)["s0"]))
                } else if (submethod == "B") {
                    oceDebug(debug, "method[3]=\"B\" so using two-segment model with zero near-surface pressure\n")
                    m <- try(nls(pp ~ function(ss, s0, dpds)
                                 {
                                     oceDebug(debug-1, "bilinearB s0=", s0, "dpds=", dpds, "\n")
                                     ifelse(s < s0, 0, dpds * (s-s0))
                                 },
                                 start=list(s0=s0, dpds=dpds0)), silent=TRUE)
                    scanStart <- if (class(m) == "try-error") 1 else max(1, floor(0.5 + coef(m)["s0"]))
                } else {
                    stop("unknown submethod '", submethod, "'")
                }
                oceDebug(debug-1, "scanStart:", scanStart, "\n")
                keep <- keep & (x[["scan"]] > scanStart)
            }
        } else if (method == "upcast") {
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
            pressureSmoothed <- smooth(pressure, kind="3R")
            max.location <- which.max(pressureSmoothed)
            max.pressure <- pressureSmoothed[max.location]
            keep[1:max.location] <- FALSE
            oceDebug(debug, "removed data at indices from 1 to ", max.location,
                     " (where pressure is ", pressure[max.location], "\n", sep="")
            if (!pminGiven) {
                ## new method, after Feb 2008
                submethodChoices <- c("A", "B")
                sm <- pmatch(submethod, submethodChoices)
                if (is.na(submethod))
                    stop("unknown submethod '", submethod, "'")
                submethod <- submethodChoices[sm]
                pp <- pressure[keep]
                pp <- despike(pp) # some, e.g. data(ctdRaw), have crazy points in air
                ss <- x[["scan"]][keep]
                end <- which(smooth(pp) > 1/2*max.pressure)[1]
                if (!is.na(end)) {
                    pp <- pp[1:end]
                    ss <- ss[1:end]
                }
                s0 <- ss[0.25*length(ss)]
                p0 <- pp[1]
                ##p1 <- max(pp) #pp[0.9*length(pp)]
                if (length(ss) > 2)
                    dpds0 <-  diff(range(pp, na.rm=TRUE)) / diff(range(ss, na.rm=TRUE))
                else
                    dpds0 <- 0
                ## Handle submethods.
                if (submethod == "A") {
                    oceDebug(debug, "method[2]=\"A\"\n")
                    m <- try(nls(pp ~ function(ss, s0, p0, dpds)
                                 {
                                     oceDebug(debug-1, "bilinearA s0=", s0, "p0=",
                                              p0, "dpds=", dpds, "\n")
                                     ifelse(s < s0, p0, p0+dpds * (s-s0))
                                 },
                                 start=list(s0=s0, p0=0, dpds=dpds0)), silent=TRUE)
                    scanStart <- if (class(m) == "try-error") 1 else max(1, floor(0.5 + coef(m)["s0"]))
                } else if (submethod == "B") {
                    oceDebug(debug, "method[3]=\"B\" so using two-segment model with zero near-surface pressure\n")
                    m <- try(nls(pp ~ function(ss, s0, dpds)
                                 {
                                     oceDebug(debug-1, "bilinearB s0=", s0, "dpds=", dpds, "\n")
                                     ifelse(s < s0, 0, dpds * (s-s0))
                                 },
                                 start=list(s0=s0, dpds=dpds0)), silent=TRUE)
                    scanStart <- if (class(m) == "try-error") 1 else max(1, floor(0.5 + coef(m)["s0"]))
                } else {
                    stop("unknown submethod '", submethod, "'")
                }
                oceDebug(debug-1, "scanStart:", scanStart, "\n")
                keep <- keep & (x[["scan"]] > scanStart)
            }

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
        } else if (method == "sbe") {
            oceDebug(debug, "Using method \"sbe\" for removing soak\n")
            if (!missing(parameters)) {
                if ("minSoak" %in% names(parameters)) {
                    minSoak <- parameters$minSoak
                } else {
                    minSoak <- 1
                }
                if ("maxSoak" %in% names(parameters)) {
                    maxSoak <- parameters$maxSoak
                } else {
                    maxSoak <- 20
                }
            } else {
                minSoak <- 1
                maxSoak <- 20
            }
            oceDebug(debug, "Using minSoak of ", minSoak, "\n")
            oceDebug(debug, "Using maxSoak of ", maxSoak, "\n")
            max.location <- which.max(smooth(pressure, kind="3R"))
            max.pressure <- smooth(pressure, kind="3R")[max.location]
            keep[max.location:n] <- FALSE
            oceDebug(debug, "removed data at indices from ", max.location,
                     " (where pressure is ", pressure[max.location], ") to the end of the data\n", sep="")
            pp <- pressure[keep]
            pp <- despike(pp) # some, e.g. data(ctdRaw), have crazy points in air
            ss <- x[["scan"]][keep]
            n <- length(pp)
            imin <- which(pp > minSoak & pp < maxSoak)[1]
            imax <- which(pp > maxSoak)[1]
            if (any(is.na(c(imin, imax)))) {
                stop("Trim parameters for \"sbe\" method not appropriate. Try different parameters or a different method")
            } else {
                ## The [1] below is to handle cases where digitization of the
                ## pressure channel gives more than one match.
                istart <- which(pp == min(pp[imin:imax]))[1]
                keep <- keep & (seq_along(x[["scan"]]) > istart)
                oceDebug(debug, "istart =", istart, "\n")
            }
        } else {
            stop("'method' not recognized; must be 'index', 'downcast', 'scan', 'range', or 'sbe'")
        }
    } else {
        keep <- method(data=x@data, parameters=parameters)
    }

    ## Handle depth inversions (may be a problem with noise)
    if (removeDepthInversions) {
        keep <- keep & c(TRUE, diff(pressure) >= 0)
    }
    if (indices) {
        res <- keep
    } else {
        ## Data
        if (is.data.frame(res@data)) {
            res@data <- res@data[keep, ]
        } else {
            for (i in seq_along(res@data)) {
                res@data[[i]] <- res@data[[i]][keep]
            }
        }
        ## Metadata
        for (i in seq_len(length(res@metadata$flags))) {
            res@metadata$flags[[i]] <- res@metadata$flags[[i]][keep]
        }
        res@processingLog <- processingLogAppend(res@processingLog, paste(deparse(match.call()), sep="", collapse=""))
    }
    ## if (waterDepthWarning)
    ##     res@processingLog <- processingLogAppend(res@processingLog, "inferred water depth from maximum pressure")
    oceDebug(debug, "} # ctdTrim()\n", unindent=1)
    res
}

#' Write a CTD Data Object as a CSV File
#'
#' Writes a comma-separated file containing the data frame stored in
#' the `data` slot of the first argument.  The file is suitable
#' for reading with a spreadsheet, or
#' with [read.csv()].  This output file will contain
#' some of the metadata in `x`, if `metadata` is `TRUE`.
#'
#' @param object a [ctd-class] object.
#'
#' @param file Either a character string (the file name) or a connection. If not
#' provided, `file` defaults to [stdout()].
#'
#' @param metadata a logical value indicating whether to put some selected
#' metadata elements at the start of the output file.
#'
#' @param flags a logical value indicating whether to show data-quality flags
#' as well as data.
#'
#' @param format string indicating the format to use. This may be `"csv"`
#' for a simple CSV format, or `"whp"` for the World Hydrographic
#' Program format, described in reference 1 and exemplified in reference 2.
#'
#' @seealso The documentation for [ctd-class] explains the structure
#' of CTD objects.
#'
#' @examples
#'\dontrun{
#' library(oce)
#' data(ctd)
#' write.ctd(ctd, "ctd.csv")
#' d <- read.csv("ctd.csv")
#' plot(as.ctd(d$salinity, d$temperature, d$pressure))
#'}
#'
#' @author Dan Kelley
#'
#' @references
#' The following links used to work, but failed as of December 2020.
#'
#' 1. `https://www.nodc.noaa.gov/woce/woce_v3/wocedata_1/whp/exchange/exchange_format_desc.htm`
#'
#' 2. `https://www.nodc.noaa.gov/woce/woce_v3/wocedata_1/whp/exchange/example_ct1.csv`
#'
#' @family things related to ctd data
write.ctd <- function(object, file, metadata=TRUE, flags=TRUE, format="csv")
{
    if (!inherits(object, "ctd"))
        stop("method is only for objects of class '", "ctd", "'")
    if (missing(file))
        file <- stdout()
    if (is.character(file)) {
        if (file == "")
            stop("'file' must be a non-empty string")
        con <- file(file, "w")
    } else if (inherits(file, "connection")) {
        con <- file
    }
    fc <- c("csv", "whp")
    fm <- pmatch(format, fc)
    if (is.na(fm))
        stop("unknown format '", format, "'; valid choices are \"csv\" and \"whp\"")
    format <- fc[fm]

    if (metadata) {
        if (format == "csv") {
            cat(paste("R/oce file exported at time ", format(presentTime(), "%Y-%m-%d %H:%M:%S %Z"), "\n", sep=""), file=con)
            cat(paste("Source file = \"", object[["filename"]], "\"\n", sep=""), file=con)
            cat(paste("Ship = ", object[["ship"]], "\n", sep=""), file=con)
            cat(paste("Cruise = ", object[["cruise"]], "\n", sep=""), file=con)
            cat(paste("Station = ", object[["station"]], "\n", sep=""), file=con)
            cat(paste("Longitude = ", object[["longitude"]][1], "\n", sep=""), file=con)
            cat(paste("Latitude = ", object[["latitude"]][1], "\n", sep=""), file=con)
            cat(paste("Depth = ", object[["waterDepth"]], "\n", sep=""), file=con)
            cat(paste("Start time = ", format(object[["startTime"]], "%Y-%m-%d %H:%M:%S %Z"), "\n", sep=""), file=con)
            cat("\n", file=con)
        } else if (format == "whp") {
            cat(paste("CTD,", format(object[["startTime"]], "%Y%m%d"), "divINSwho\n", sep=""), file=con)
            cat("NUMBER_HEADERS = 10\n", file=con)
            cat("EXPOCODE = UNKNOWN\n", file=con)
            cat("SECT = UNKNOWN\n", file=con)
            cat("STNNBR =", gsub(" ", "", object[["station"]]), "\n", file=con)
            cat("CASTNO = 1\n", file=con)
            cat("DATE = ", format(object[["startTime"]], "%Y%m%d"), "\n", file=con)
            cat("TIME = ", format(object[["startTime"]], "%H%M"), "\n", file=con)
            cat("LATITUDE = ", sprintf("%8.4f", object[["latitude"]]), "\n", sep="", file=con)
            cat("LONGITUDE = ", sprintf("%9.4f", object[["longitude"]]), "\n", sep="", file=con)
            cat("DEPTH = ", sprintf("%5.0f", object[["waterDepth"]]), "\n", sep="", file=con)
        } else {
            stop("unknown format \"", format, "\"; should be \"csv\" or \"whp\"")
        }
    }
    df <- as.data.frame(object@data)
    ## Optionally paste flags into the data frame, for display
    if (flags && length(object@metadata$flags)) {
        fdf <- as.data.frame(object@metadata$flags)
        names(fdf) <- paste(names(fdf), "Flag", sep="")
        df <- data.frame(df, fdf)
    }
    if (format == "csv") {
        write.table(df, col.names=TRUE, row.names=FALSE, sep=",", file=con)
    } else if (format == "whp") {
        names <- names(df)
        names(df) <- oceNames2whpNames(names)
        cat(paste(names(df), collapse=","), "\n", file=con)
        u <- unlist(lapply(object[["units"]], function(u) as.character(u$unit)))
        s <- unlist(lapply(object[["units"]], function(u) u$scale))
        cat(paste(oceUnits2whpUnits(u[names], s[names]), collapse=","), "\n", file=con)
        write.table(df, col.names=FALSE, row.names=FALSE, sep=",", file=con)
        cat("END_DATA\n", file=con)
    } else {
        stop("unknown format \"", format, "\"; should be \"csv\" or \"whp\"")
    }
    if (con != stdout())
        close(con)
}


#' Plot a ctd Object
#'
#' Plot CTD data, by default in a four-panel display showing (a) profiles of
#' salinity and temperature, (b) profiles of density and the square of buoyancy
#' frequency, (c) a TS diagram and (d) a coastline diagram indicating the station
#' location.
#'
#' @details
#' Creates a multi-panel summary plot of data measured in a CTD cast. The
#' default values of `which` and other arguments are chosen to be useful
#' for quick overviews of data. However, for detailed work it is common
#' to call the present function with just a single value of `which`, e.g.
#' with four calls to get four panels. The advantage of this is that it provides
#' much more control over the display, and also it permits the addition of extra
#' display elements (lines, points, margin notes, etc.) to the individual panels.
#'
#' Note that panels that draw more than one curve (e.g. `which="salinity+temperature"`
#' draws temperature and salinity profiles in one graph), the value of [par]`("usr")`
#' is established by the second profile to have been drawn. Some experimentation will
#' reveal what this profile is, for each permitted `which` case, although
#' it seems unlikely that this will help much ... the simple fact is that drawing two
#' profiles in one graph is useful for a quick overview, but not useful for e.g. interactive
#' analysis with [locator()] to flag bad data, etc.
#'
#' @param x a [ctd-class] object.
#'
#' @param which List of desired plot types, as given below. If `which` is not
#' supplied, a default will be used. This default will be `c(1,2,3,5)` if the
#' CTD is in profiling mode (i.e. if `deploymentType` in the `metadata`
#' slot equals `"profile"`,
#' or is missing) or
#' `"moored"`/`"thermosalinograph"`, the default will be `c(30, 3, 31, 5)`.  If it
#' is `"towyo"`, `c(30, 31, 32, 3)` will be used. Details are as
#' follows.
#'
#' * `which=1` or `which="salinity+temperature"` gives
#' a combined profile of temperature and salinity
#'
#' * `which=2` or `which="density+N2"` gives a combined
#' profile of \eqn{\sigma_\theta}{sigma-theta} and \eqn{N^2}{N^2}
#'
#' * `which=3` or `which="TS"` gives a TS plot
#'
#' * `which=4` or `which="text"` gives a textual
#' summary of some aspects of the data
#'
#' * `which=5` or `which="map"` gives a map plotted
#' with [plot,coastline-method()], with a dot for
#' the station location.  Notes near the top boundary of the map give the
#' station number, the sampling date, and the name of the chief scientist,
#' if these are known. Note that the longitude will be converted to a value
#' between -180 and 180 before plotting.  (See also notes
#' about `span`.)
#'
#' * `which=5.1` as for `which=5`, except that the file name
#' is drawn above the map
#'
#' * `which=6` or `which="density+dpdt"` gives a
#' profile of density and \eqn{dP/dt}{dP/dt}, which is useful for
#' evaluating whether the instrument is dropping properly through the
#' water column
#'
#' * `which=7` or `which="density+time"` gives a
#' profile of density and time
#'
#' * `which=8` or `which="index"` gives a profile of
#' index number (especially useful for [ctdTrim()])
#'
#' * `which=9` or `which="salinity"` gives a salinity profile
#'
#' * `which=10` or `which="temperature"` gives a temperature profile
#'
#' * `which=11` or `which="density"` gives a density profile
#'
#' * `which=12` or `which="N2"` gives an \eqn{N^2}{N^2} profile
#'
#' * `which=13` or `which="spice"` gives a spiciness profile
#'
#' * `which=14` or `which="tritium"` gives a tritium profile
#'
#' * `which=15` or `which="Rrho"` gives an Rrho profile
#'
#' * `which=16` or `which="RrhoSF"` gives an RrhoSF profile
#'
#' * `which=17` or `which="conductivity"` gives a conductivity profile
#'
#' * `which=20` or `which="CT"` gives a Conservative Temperature profile
#'
#' * `which=21` or `which="SA"` gives an Absolute Salinity profile
#'
#' * `which=30` gives a time series of Salinity
#'
#' * `which=31` gives a time series of Temperature
#'
#' * `which=32` gives a time series of pressure
#'
#' * `which=33` gives a time series of sigmaTheta
#'
#' @param col Color of lines or symbols.
#'
#' @param fill A legacy parameter that will be permitted only temporarily; see
#' \dQuote{History}.
#'
#' @param borderCoastline Color of coastlines and international borders, passed
#' to [plot,coastline-method()] if a map is included in `which`.
#'
#' @param colCoastline Fill color of coastlines and international borders, passed
#' to [plot,coastline-method()] if a map is included in `which`. Set to
#' `NULL` to avoid filling.
#'
#' @param eos String indicating the equation of state to be used, either
#' `"unesco"` or `"gsw"`.
#'
#' @param ref.lat Latitude of reference point for distance calculation.
#'
#' @param ref.lon Longitude of reference point for distance calculation.
#'
#' @param grid Set `TRUE` to get a grid on all plots.
#'
#' @param coastline A specification of the coastline to be used for
#' `which="map"`.  This may be a coastline object, whether built-in or
#' supplied by the user, or a character string.  If the later, it may be the
#' name of a built-in coastline (`"coastlineWorld"`,
#' `"coastlineWorldFine"`, or
#' `"coastlineWorldCoarse"`), or `"best"`, to choose
#' a suitable coastline for the locale, or `"none"` to prevent
#' the drawing of a coastline.  There is a speed penalty for providing
#' `coastline` as a character string, because it forces
#' [plot,coastline-method()] to load it on every call.  So, if
#' [plot,coastline-method()] is to be called several times for a given
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
#' @param densitylim Optional limits of density axis, whether that axis be horizontal
#' or vertical.
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
#' @param drawIsobaths An indication of whether to draw depth contours on
#' maps, in addition to the coastline. The argument has no effect except
#' for panels in which the value of `which` equals `"map"` or
#' the equivalent numerical code, `5`. If `drawIsobaths` is
#' `FALSE`, then no contours are drawn. If `drawIsobaths`
#' is `TRUE`, then contours are selected automatically,
#' using [pretty]`(c(0,300))` if the station depth is
#' under 100m or [pretty]`(c(0,5500))` otherwise.
#' If `drawIsobaths` is a numerical vector,
#' then the indicated depths are drawn. For plots drawn with `projection`
#' set to `NULL`, the contours are added with [contour()]
#' and otherwise [mapContour()] is used. To customize
#' the resultant contours, e.g. setting particular line types or colors,
#' users should call these functions directly (see e.g. Example 2).
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
#' @param lonlabels,latlabels Values that control the labelling of
#' longitude and latitude along the sides of the plot, used only if
#' `projection` is not `NULL`.  See [mapPlot()] for details.
#'
#' @param projection Projection for map, if desired.  If this is `NULL`, no
#' projection will be used; the map will simply show longitude and latitude in a
#' cartesian frame, scaled to retain shapes at the centre.  If this is the string
#' `"automatic"`, then either a Mercator or Stereographic projection will be
#' used, depending on whether the CTD station is within 70 degrees of the equator
#' or at higher latitudes.  Finally, if this is a string in the format used by
#' [mapPlot()], then it is is passed to that function.
#'
## @param parameters a **deprecated** argument that has been ignored
## since February 2016; see [oce-deprecated].
##
## @param orientation a **deprecated** argument that has been ignored
## since February 2016; see [oce-deprecated].
#'
#' @param latlon.pch Symbol code for sample location (ignored if no map plotted).
#'
#' @param latlon.cex Symbol expansion factor for sample location (ignored if no
#' map plotted).
#'
#' @param latlon.col Color of symbol for sample location (ignored if no map
#' plotted).
#'
#' @param cex Size to be used for plot symbols (see [par()]).
#'
#' @param cex.axis Size factor for axis labels (see [par()]).
#'
#' @param pch Code for plotting symbol (see [par()]).
#'
#' @param useSmoothScatter Boolean, set to `TRUE` to use
#' [smoothScatter()] instead of [plot()] to draw the plot.
#'
#' @param df Optional argument that is ignored except for plotting buoyancy
#' frequency; in that case, it is passed to [swN2()] as the argument
#' named `df`.
#'
#' @param keepNA logical value indicating whether `NA` values
#' will yield breaks in lines drawn if `type` is `b`, `l`, or `o`.
#' The default value is `FALSE`.  Setting `keepNA` to `TRUE`
#" can be helpful when working with multiple profiles
#' strung together into one [ctd-class] object, which otherwise
#' would have extraneous lines joining the deepest point in one
#' profile to the shallowest in the next profile.
#'
#' @param type The type of plot to draw, using the same scheme as
#' [plot()]. If supplied, this is increased to be the
#' same length as `which`, if necessary, and then supplied to
#' each of the individual plot calls. If it is not supplied,
#' then those plot calls use defaults (e.g. using a line for
#' [plotProfile()], using dots for [plotTS()],
#' etc).
#'
#' @param mgp Three-element numerical vector specifying axis-label geometry,
#' passed to [par()].
#' The default establishes tighter margins than in the usual R setup.
#'
#' @param mar Four-element numerical vector specifying margin geometry,
#' passed to [par()].
#' The default establishes tighter margins than in the usual R setup.
#' Note that the value of `mar` is ignored for the map panel
#' of multi-panel maps; instead, the present value of
#' [par]`("mar")` is used, which in the default call will
#' make the map plot region equal that of the previously-drawn
#' profiles and TS plot.
#'
#' @param inset Set to `TRUE` for use within [plotInset()].  The
#' effect is to prevent the present function from adjusting margins, which is
#' necessary because margin adjustment is the basis for the method used by
#' [plotInset()].
#'
#' @param add Logical, indication of whether to add to an existing plot.  This
#' only works if `length(which)=1`, and it will yield odd results if the
#' value of `which` does not match that in the previous plots.
#'
#' @template debugTemplate
#'
#' @param ... Optional arguments passed to plotting functions. A common example is
#' to set `df`, for use in [swN2()] calculations.
#'
#' @seealso
#' The documentation for [ctd-class] explains the structure of CTD
#' objects, and also outlines the other functions dealing with them.
#'
#' @section History:
#' Until February, 2016, [plot,ctd-method()] relied on a now-defunct argument
#' `fill` to control colors; `colCoastline` is to be used now, instead.
#' Also, now it is possible to set the color of coasts and international
#' boundaries, with `borderCoastline`.
#'
#' @examples
#' ## 1. simple plot
#' library(oce)
#' data(ctd)
#' plot(ctd)
#'
#' ## 2. how to customize depth contours
#' par(mfrow=c(1,2))
#' data(section)
#' stn <- section[["station", 105]]
#' plot(stn, which='map', drawIsobaths=TRUE)
#' plot(stn, which='map')
#' data(topoWorld)
#' tlon <- topoWorld[["longitude"]]
#' tlat <- topoWorld[["latitude"]]
#' tdep <- -topoWorld[["z"]]
#' contour(tlon, tlat, tdep, drawlabels=FALSE,
#'         levels=seq(1000,6000,1000), col='lightblue', add=TRUE)
#' contour(tlon, tlat, tdep, vfont=c("sans serif", "bold"),
#'         levels=stn[['waterDepth']], col='red', lwd=2, add=TRUE)
#'
#' @author Dan Kelley
#'
#' @family functions that plot oce data
#' @family things related to ctd data
#'
#' @aliases plot.ctd
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
                              drawIsobaths=FALSE, clongitude, clatitude, span, showHemi=TRUE,
                              lonlabels=TRUE, latlabels=TRUE,
                              projection=NULL,
                              latlon.pch=20, latlon.cex=1.5, latlon.col="red",
                              cex=1, cex.axis=par('cex.axis'),
                              pch=1,
                              useSmoothScatter=FALSE,
                              df,
                              keepNA=FALSE,
                              type,
                              mgp=getOption("oceMgp"),
                              mar=c(mgp[1]+1.5, mgp[1]+1.5, mgp[1]+1.5, mgp[1]+1),
                              inset=FALSE,
                              add=FALSE,
                              debug=getOption("oceDebug"),
                              ...)
          {
              if (!inherits(x, "ctd"))
                  stop("method is only for objects of class 'ctd'")
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
                  warning("In plot,ctd-method() : 'fill' is outdated; use 'colCoastline' instead", call.=FALSE)
              }
              if (missing(which)) {
                  oceDebug(debug, "plot,ctd-method(..., eos=\"", eos, "\", inset=", inset, ", ...) {\n", sep="", unindent=1)
                  dt <- x@metadata$deploymentType
                  if (is.null(dt)) {
                      which <- c(1, 2, 3, 5)
                      if (missing(type))
                          type <- c("l", "l", "p", "p")
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
                          which <- c(1, 2, 3, 5) # salinity+temperature density+N2 TS map
                          if (missing(type))
                              type <- c("l", "l", "p", "p")
                      } else if ("moored" == dt) {
                          which <- c(30, 31, 32, 5) # Sts Tts pts map
                          if (missing(type))
                              type <- c("l", "l", "l", "p")
                      } else if ("thermosalinograph" == dt) {
                          which <- c(30, 3, 31, 5) # Sts TS Tts
                          if (missing(type))
                              type <- c("l", "p", "l", "l")
                      } else if ("tsg" == dt) {
                          ## @richardsc -- do you think we still need this?
                          which <- c(30, 3, 31, 5) # Sts TS Tts map
                          if (missing(type))
                              type <- c("l", "p", "l", "p")
                      } else if ("towyo" == dt) {
                          which <- c(30, 3, 33, 5) # Sts TS pts map
                          if (missing(type))
                              type <- c("l", "l", "l", "p")
                      } else {
                          which <- c(1, 2, 3, 5) # salinity+temperature density+N2 TS map
                          if (missing(type))
                              type <- c("l", "l", "p", "p")
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
                  stop("in plot,ctd-method() : 'xlim' not allowed; use Slim, Tlim, etc", call.=FALSE)
              if ("ylim" %in% dotsNames)
                  stop("in plot,ctd-method() : 'ylim' not allowed; use plim, Tlim, etc", call.=FALSE)
              opar <- par(no.readonly=TRUE)
              if (add && lw > 1) {
                  warning("ignoring add=TRUE because length(which) > 1")
                  add <- FALSE
              }
              if (lw > 1) on.exit(par(opar))
              if (missing(type))
                  type <- rep("p", lw)
              if (length(type) < lw) {
                  type <- rep(type, lw)
              }
              if (length(pch) < lw)
                  pch <- rep(pch, lw) # FIXME: recycle more sensibly
              if (length(cex) < lw)
                  cex <- rep(cex, lw) # FIXME: recycle more sensibly
              ##dec_deg<-function(x, code="lat")
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
              if (!inset)
                  par(mar=mar)
              par(mgp=mgp)
              if (lw == 2) {
                  par(mfcol=c(2, 1))
              } else if (lw == 3) {
                  par(mfcol=c(3, 1))
              } else if (lw == 4) {
                  par(mfrow=c(2, 2))
              } else if (lw != 1) {
                  nnn <- floor(sqrt(lw))
                  par(mfcol=c(nnn, ceiling(lw/nnn)))
                  rm(nnn)
              }
              ##20181114 if (lw > 1) {
              ##20181114     ##oldpar <- par(no.readonly=TRUE)
              ##20181114     if (lw > 2) layout(matrix(1:4, nrow=2, byrow=TRUE)) else
              ##20181114         layout(matrix(1:2, nrow=2, byrow=TRUE))
              ##20181114     ##layout.show(lay)
              ##20181114     ##stop()
              ##20181114 }
              ## Ignore any bottom region consisting of NA for temperature and salinity, e.g.
              ## as created by as.section() or read.section().
              if (0 == length(x[["salinity"]])) {
                  warning("no data to plot in this object")
                  return(invisible(NULL))
              }
              last.good <- which(rev(is.na(x[["salinity"]]))==FALSE)[1]
              if (!is.na(last.good) && length(last.good) > 0) {
                  last.good <- length(x[["temperature"]]) - last.good + 1
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
                                       CT=20,
                                       SA=21,
                                       "Sts"=30,
                                       "Tts"=31,
                                       "pts"=32,
                                       "rhots"=33))
              for (w in seq_along(which)) {
                  if (is.na(which[w])) {
                      if (whichOrig[w] %in% names(x@data)) {
                          xlab <- resizableLabel(item=whichOrig[w], axis="x",
                                                 unit=x@metadata$units[[whichOrig[w]]], debug=debug-1)
                          oceDebug(debug, "about to call plotProfile() with which=\"", whichOrig[w], "\" and xlab=\"", as.character(xlab), "\"\n", sep="")
                          plotProfile(x, xtype=x[[whichOrig[w]]],
                                      xlab=xlab,# whichOrig[w],
                                      Slim=Slim, Tlim=Tlim, plim=plim,
                                      eos=eos,
                                      useSmoothScatter=useSmoothScatter,
                                      grid=grid, col.grid="lightgray", lty.grid="dotted",
                                      col=col, cex=cex, pch=pch,
                                      type=if (!missing(type)) type[w],
                                      keepNA=keepNA, inset=inset, add=add,
                                      debug=debug-1,
                                      ...)
                      } else {
                          warning("plot,ctd-method(): unknown plot type \"", whichOrig[w], "\" requested\n", call.=FALSE)
                      }
                      next
                  }
                  if (which[w] == 1) {
                      plotProfile(x, xtype="salinity+temperature",
                                  plim=plim, Slim=Slim, Tlim=Tlim,
                                  eos=eos,
                                  useSmoothScatter=useSmoothScatter,
                                  grid=grid, col.grid="lightgray", lty.grid="dotted",
                                  cex=cex[w], pch=pch[w],
                                  type=if (!missing(type)) type[w],
                                  keepNA=keepNA, inset=inset, add=add,
                                  debug=debug-1,
                                  ...)
                  } else if (which[w] == 2) {
                      plotProfile(x, xtype="density+N2",
                                  plim=plim, N2lim=N2lim, densitylim=densitylim,
                                  eos=eos,
                                  useSmoothScatter=useSmoothScatter,
                                  df=df,
                                  grid=grid, col.grid="lightgray", lty.grid="dotted",
                                  cex=cex[w], pch=pch[w],
                                  type=if (!missing(type)) type[w],
                                  keepNA=keepNA, inset=inset, add=add,
                                  debug=debug-1,
                                  ...)
                  } else if (which[w] == 6) {
                      plotProfile(x, xtype="density+dpdt",
                                  plim=plim, densitylim=densitylim, dpdtlim=dpdtlim,
                                  col=col,
                                  eos=eos,
                                  useSmoothScatter=useSmoothScatter,
                                  grid=grid, col.grid="lightgray", lty.grid="dotted",
                                  cex=cex[w], pch=pch[w],
                                  type=if (!missing(type)) type[w],
                                  keepNA=keepNA, inset=inset, add=add,
                                  debug=debug-1,
                                  ...)
                  } else if (which[w] == 7) {
                      plotProfile(x, xtype="density+time",
                                  plim=plim, densitylim=densitylim, timelim=timelim,
                                  useSmoothScatter=useSmoothScatter,
                                  grid=grid, col.grid="lightgray", lty.grid="dotted",
                                  cex=cex[w], pch=pch[w],
                                  ##type=type[w],
                                  keepNA=keepNA, inset=inset, add=add,
                                  debug=debug-1,
                                  ...)
                  } else if (which[w] == 8) {
                      plotProfile(x, xtype="index",
                                  plim=plim,
                                  col=col,
                                  eos=eos,
                                  useSmoothScatter=useSmoothScatter,
                                  grid=grid, col.grid="lightgray", lty.grid="dotted",
                                  cex=cex[w], pch=pch[w],
                                  type=if (!missing(type)) type[w],
                                  keepNA=keepNA, inset=inset, add=add,
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
                                  cex=cex[w], pch=pch[w],
                                  type=if (!missing(type)) type[w],
                                  keepNA=keepNA, inset=inset, add=add,
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
                                  cex=cex[w], pch=pch[w],
                                  type=if (!missing(type)) type[w],
                                  keepNA=keepNA, inset=inset, add=add,
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
                                  cex=cex[w], pch=pch[w],
                                  type=if (!missing(type)) type[w],
                                  keepNA=keepNA, inset=inset, add=add,
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
                                  cex=cex[w], pch=pch[w],
                                  type=if (!missing(type)) type[w],
                                  keepNA=keepNA, inset=inset, add=add,
                                  debug=debug-1,
                                  ...)
                  } else if (which[w] == 13) {
                      plotProfile(x, xtype="spice",
                                  plim=plim,
                                  col=col,
                                  eos=eos,
                                  useSmoothScatter=useSmoothScatter,
                                  grid=grid, col.grid="lightgray", lty.grid="dotted",
                                  cex=cex[w], pch=pch[w],
                                  type=if (!missing(type)) type[w],
                                  keepNA=keepNA, inset=inset, add=add,
                                  debug=debug-1,
                                  ...)
                  } else if (which[w] == 14) {
                      plotProfile(x, xtype="tritium",
                                  plim=plim,
                                  col=col,
                                  eos=eos,
                                  useSmoothScatter=useSmoothScatter,
                                  grid=grid, col.grid="lightgray", lty.grid="dotted",
                                  cex=cex[w], pch=pch[w],
                                  type=if (!missing(type)) type[w],
                                  keepNA=keepNA, inset=inset, add=add,
                                  debug=debug-1,
                                  ...)
                  } else if (which[w] == 15) {
                      plotProfile(x, xtype="Rrho",
                                  plim=plim,
                                  col=col,
                                  eos=eos,
                                  useSmoothScatter=useSmoothScatter,
                                  grid=grid, col.grid="lightgray", lty.grid="dotted",
                                  cex=cex[w], pch=pch[w],
                                  type=if (!missing(type)) type[w],
                                  keepNA=keepNA, inset=inset, add=add,
                                  debug=debug-1,
                                  ...)
                  } else if (which[w] == 16) {
                      plotProfile(x, xtype="RrhoSF",
                                  plim=plim,
                                  col=col,
                                  eos=eos,
                                  useSmoothScatter=useSmoothScatter,
                                  grid=grid, col.grid="lightgray", lty.grid="dotted",
                                  cex=cex[w], pch=pch[w],
                                  type=if (!missing(type)) type[w],
                                  keepNA=keepNA, inset=inset, add=add,
                                  debug=debug-1,
                                  ...)
                  } else if (which[w] == 17) {
                      plotProfile(x, xtype="conductivity", Clim=Clim, plim=plim,
                                  col=col,
                                  eos=eos,
                                  useSmoothScatter=useSmoothScatter,
                                  grid=grid, col.grid="lightgray", lty.grid="dotted",
                                  cex=cex[w], pch=pch[w],
                                  type=if (!missing(type)) type[w],
                                  keepNA=keepNA, inset=inset, add=add,
                                  debug=debug-1,
                                  ...)
                  } else if (which[w] == 3) {
                      oceDebug(debug, "which=3, for a TS diagram\n")
                      ##par(mar=c(3.5,3,2,2))
                      lwd.rho <- if ("lwd.rho" %in% names(dots)) dots$lwd.rho else par('lwd')
                      lty.rho <- if ("lty.rho" %in% names(dots)) dots$lty.rho else par('lty')
                      plotTS(x, Slim=Slim, Tlim=Tlim,
                             grid=grid, col.grid="lightgray", lty.grid="dotted",
                             eos=eos,
                             lwd.rho=lwd.rho, lty.rho=lty.rho,
                             useSmoothScatter=useSmoothScatter,
                             col=col, pch=pch, cex=cex,
                             type=if (!missing(type)) type[w],
                             inset=inset,
                             add=add,
                             debug=debug-1, ...) # FIXME use inset here
                  } else if (which[w] == 4) {
                      textItem<-function(xloc, yloc, item, label, cex=0.8, d.yloc=0.8) {
                          if (!is.null(item) && !is.na(item))
                              text(xloc, yloc, paste(label, item), adj=c(0, 0), cex=cex)
                          yloc - d.yloc
                      }
                      par(mar=c(0, 0, 0, 0))
                      plot.new()
                      plot.window(c(0, 10), c(0, 10))
                      xloc <- 0
                      yloc <- 8
                      cex <- 3/4
                      xm <- x@metadata
                      yloc <- textItem(xloc, yloc, xm$station,         " Station:  ", cex=cex)
                      if (!is.null(xm$filename) && nchar(xm$filename, "bytes") > 0) {
                          yloc <- textItem(xloc, yloc, xm$filename,    " File:     ", cex=cex)
                      }
                      if (!is.null(xm$scientist)) {
                          yloc <- textItem(xloc, yloc, xm$scientist,   " Scientist:", cex=cex)
                      }
                      if (!is.null(xm$institute)) {
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
                      ##     ##                               ",", dec_deg(ref.lat), ")=", kms), adj=c(0, 0), cex=cex)
                      ##     yloc <- yloc - d.yloc
                      ## }
                  } else if (which[w] == 5) {
                      oceDebug(debug, "drawing a map\n")
                      ## obey 'mar' only if length(which)==1.
                      omar <- mar # AAAAAAAA
                      if (length(which) > 1)
                          mar <- par("mar")
                      ## map
                      if (!is.null(x[["latitude"]]) &&
                          !is.null(x[["longitude"]]) &&
                          any(is.finite(x[["latitude"]])) &&
                          any(is.finite(x[["longitude"]]))) {
                          oceDebug(debug, "plot(ctd, ...) { # of type MAP\n")
                          ## Calculate span, if not given
                          if (missing(span)) {
                              oceDebug(debug, "'span' not given\n")
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
                              oceDebug(debug, "span not given; nearest land ", round(nearest, 0),
                                       "km, so set span=", round(span, 0), "\n")
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
                                  #library(ocedata) # FIXME: is this needed?
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
                          ## impose the right class, to overcome a bug in ocedata_0.1.6
                          if (!inherits(coastline, "coastline"))
                              class(coastline) <- structure("coastline", package="ocedata")
                          ## ensure that the object holds (or seems to hold) coastline data
                          if (2 != sum((c("longitude", "latitude") %in% names(coastline@data))))
                              stop("'coastline' must be a coastline object, or a string naming one")
                          if (!missing(clongitude) && !missing(clatitude) && !missing(span)) {
                              plot(coastline,
                                   clongitude=clongitude, clatitude=clatitude, span=span,
                                   projection=projection,
                                   border=borderCoastline, col=colCoastline,
                                   mgp=mgp, mar=mar, inset=inset, cex.axis=cex.axis,
                                   lonlabels=lonlabels, latlabels=latlabels,
                                   debug=debug-1)
                          } else {
                              if (missing(lonlim)) {
                                  mlon <- mean(x[["longitude"]], na.rm=TRUE)
                                  lonlim.c <- mlon + c(-1, 1) * min(abs(range(coastline[["longitude"]], na.rm=TRUE) - mlon))
                                  clon <- mean(lonlim.c)
                                  if (missing(latlim)) {
                                      mlat <- mean(x[["latitude"]], na.rm=TRUE)
                                      oceDebug(debug, "CASE 1: both latlim and lonlim missing; using projection=",
                                               if (is.null(projection)) "NULL" else projection, "\n")
                                      latlim.c <- mlat + c(-1, 1) * min(abs(range(coastline[["latitude"]], na.rm=TRUE) - mlat))
                                      latlim.c <- ifelse(latlim.c > 90, 89.99, latlim.c)
                                      oceDebug(debug, "about to plot coastline\n")
                                      oceDebug(debug, "clatitude=", mean(latlim.c), "\n")
                                      oceDebug(debug, "clongitude=", clon, "\n")
                                      oceDebug(debug, "span=", span, "\n")
                                      oceDebug(debug, "projection=", projection, "\n")
                                      oceDebug(debug, "ok, about to call plot(coastline)\n")
                                      plot(coastline,
                                           clongitude=standardizeLongitude(clon), clatitude=mean(latlim.c), span=span,
                                           projection=projection,
                                           border=borderCoastline, col=colCoastline,
                                           mgp=mgp, mar=mar, inset=inset, cex.axis=cex.axis,
                                           lonlabels=lonlabels, latlabels=latlabels,
                                           debug=debug-1)
                                      oceDebug(debug, " ... did plot(coastline)\n")
                                  } else {
                                      oceDebug(debug, "CASE 2: latlim given, lonlim missing\n")
                                      clat <- mean(latlim)
                                      plot(coastline,
                                           clongitude=standardizeLongitude(clon), clatitude=clat, span=span,
                                           projection=projection,
                                           border=borderCoastline, col=colCoastline,
                                           mgp=mgp, mar=mar, inset=inset, cex.axis=cex.axis,
                                           lonlabels=lonlabels, latlabels=latlabels,
                                           debug=debug-1)
                                  }
                                  if (is.numeric(which[w]) && round(which[w], 1) == 5.1) # HIDDEN FEATURE
                                      mtext(gsub(".*/", "", x@metadata$filename), side=3, line=0.1, cex=0.7*cex)
                              } else {
                                  oceDebug(debug, "lonlim was provided\n")
                                  clon <- mean(lonlim)
                                  if (missing(latlim)) {
                                      oceDebug(debug, "CASE 3: lonlim given, latlim missing\n")
                                      latlim.c <- mean(x@metadata$latitude, na.rm=TRUE) +
                                      c(-1, 1) * min(abs(range(coastline[["latitude"]], na.rm=TRUE) - x@metadata$latitude))
                                      clat <- mean(latlim.c)
                                      plot(coastline,
                                           clongitude=standardizeLongitude(clon), clatitude=clat, span=span,
                                           projection=projection,
                                           border=borderCoastline, col=colCoastline,
                                           mgp=mgp, mar=mar, inset=inset, cex.axis=cex.axis,
                                           lonlabels=lonlabels, latlabels=latlabels,
                                           debug=debug-1)
                                  } else {
                                      oceDebug(debug, "CASE 4: both latlim and lonlim given\n")
                                      clat <- mean(latlim)
                                      plot(coastline,
                                           clongitude=standardizeLongitude(clon), clatitude=clat, span=span,
                                           border=borderCoastline, col=colCoastline,
                                           projection=projection,
                                           mgp=mgp, mar=mar, inset=inset, cex.axis=cex.axis,
                                           lonlabels=lonlabels, latlabels=latlabels,
                                           debug=debug-1)
                                  }
                              }
                          }
                          ## draw isobaths
                          xlon <- x[["longitude"]]
                          xlat <- x[["latitude"]]
                          ok <- is.finite(xlon) & is.finite(xlat)
                          xlon <- xlon[ok]
                          xlat <- xlat[ok]
                          stationLon <- standardizeLongitude(xlon)
                          stationLat <- xlat
                          if (is.numeric(drawIsobaths) || (is.logical(drawIsobaths) && drawIsobaths)) {
                              data("topoWorld", package="oce", envir=environment())
                              topoWorld <- get("topoWorld")
                              topoLon <- topoWorld[["longitude"]]
                              topoLat <- topoWorld[["latitude"]]
                              topoDep <- -topoWorld[["z"]]
                              topoDep[topoDep < -1] <- -1 # don't want land contours
                              itopoLon <- which.min(abs(topoLon - stationLon))
                              itopoLat <- which.min(abs(topoLat - stationLat))
                              oceDebug(debug, "itopoLon=", itopoLon, ", lon=", topoLon[itopoLon], "\n")
                              oceDebug(debug, "itopoLat=", itopoLat, ", lon=", topoLat[itopoLat], "\n")
                              stationDep <- topoDep[itopoLon, itopoLat]
                              oceDebug(debug, "stationDep=", stationDep, "\n")
                              ## Auto-select depths differently for stations on the shelf or in
                              ## the deap sea. (Notice that the first level, which will be 0m, is
                              ## trimmed, to avoid messing up the coastline with a contour from
                              ## coarse topography.
                              levels <- if (is.logical(drawIsobaths))
                                  if (stationDep < 100) pretty(c(0, 500))[-1] else pretty(c(0, 5500))[-1]
                                  else drawIsobaths
                              if (is.null(projection)) {
                                  contour(topoLon, topoLat, topoDep, col='gray', vfont=c("sans serif", "bold"),
                                          levels=levels, add=TRUE)
                              } else {
                                  mapContour(topoLon, topoLat, topoDep, col='gray', levels=levels)
                              }
                          }
                          ## draw station location
                          if (is.null(projection)) {
                              points(stationLon, stationLat, cex=latlon.cex, col=latlon.col, pch=latlon.pch)
                          } else {
                              mapScalebar()
                              ## FIXME: used to be non-standardized lon below.
                              mapPoints(stationLon, stationLat, cex=latlon.cex, col=latlon.col, pch=latlon.pch)
                          }
                          ## draw some text in top margin
                          if (!is.null(x@metadata$station) && !is.na(x@metadata$station)) {
                              mtext(x@metadata$station,
                                    side=3, adj=0, cex=par("cex"), line=0.5)
                          }
                          if (!is.null(x@metadata$startTime) && !is.na(x@metadata$startTime) && 4 < nchar(x@metadata$startTime, "bytes")) {
                              mtext(format(x@metadata$startTime, "%Y-%m-%d %H:%M:%S"),
                                    side=3, adj=1, cex=par("cex"), line=0.5)
                          } else if (!is.null(x@data$time)) {
                              goodTimes <- which(!is.na(x@data$time))
                              if (length(goodTimes)) {
                                  mtext(format(x@data$time[goodTimes[1]], "%Y-%m-%d %H:%M:%S"),
                                        side=3, adj=1, cex=par("cex"), line=0.5)
                              }
                          }
                      }
                      mar <- omar # recover mar, which was altered for multi-panel plots
                      oceDebug(debug, "} # plot(ctd, ...) of type \"map\"\n", unindent=1)
                  } else if (which[w] == 20) { # CT
                      plotProfile(x, xtype="CT", xlab=resizableLabel("CT", debug=debug-1),
                                  Tlim=Tlim, plim=plim, eos="gsw",
                                  useSmoothScatter=useSmoothScatter,
                                  grid=grid, col.grid="lightgray", lty.grid="dotted",
                                  cex=cex[w], pch=pch[w],
                                  type=if (!missing(type)) type[w],
                                  keepNA=keepNA, inset=inset, add=add,
                                  debug=debug-1,
                                  ...)
                  } else if (which[w] == 21) { # SA
                      plotProfile(x, xtype="SA", xlab=resizableLabel("SA", debug=debug-1),
                                  Tlim=Tlim, plim=plim, eos="gsw",
                                  useSmoothScatter=useSmoothScatter,
                                  grid=grid, col.grid="lightgray", lty.grid="dotted",
                                  cex=cex[w], pch=pch[w],
                                  type=if (!missing(type)) type[w],
                                  keepNA=keepNA, inset=inset, add=add,
                                  debug=debug-1,
                                  ...)
                  } else if (which[w] ==30) {
                      ## S timeseries
                      oce.plot.ts(x[["time"]], x[["salinity"]], ylab=resizableLabel("S", "y", debug=debug-1))
                  } else if (which[w] ==31) {
                      ## T timeseries
                      oce.plot.ts(x[["time"]], x[["temperature"]], ylab=resizableLabel("T", "y", debug=debug-1))
                  } else if (which[w] ==32) {
                      ## p timeseries
                      oce.plot.ts(x[["time"]], x[["pressure"]], ylab=resizableLabel("p", "y", debug=debug-1))
                  } else if (which[w] ==33) {
                      ## sigmaTheta timeseries
                      oce.plot.ts(x[["time"]], x[["sigmaTheta"]], ylab=resizableLabel("sigmaTheta", "y", debug=debug-1))
                  } else {
                      stop("unknown value of which, ", which[w])
                  }
              }
              oceDebug(debug, "} # plot,ctd-method()\n", unindent=1)
              invisible(NULL)
          })


#' Subset a ctd Object
#'
#' Return a subset of a [ctd-class] object.
#'
#' This function is used to subset data within
#' a [ctd-class] object. There are two ways of working. If
#' `subset` is supplied, then it is a logical expression
#' that is evaluated within the environment of the `data`
#' slot of the object (see Example 1). Alternatively, if the
#' `...` list contains an expression defining `indices`,
#' then that expression is used to subset each item within the
#' `data` slot (see Example 2).
#'
#' @param x a [ctd-class] object.
#'
#' @param subset An expression indicating how to subset `x`.
#'
#' @param ... optional arguments, of which only the first is examined. The only
#' possibility is that this argument be named `indices`. See \dQuote{Details}.
#'
#' @return A [ctd-class] object.
#'
#' @examples
#' library(oce)
#' data(ctd)
#' plot(ctd)
#' ## Example 1
#' plot(subset(ctd, pressure<10))
#' ## Example 2
#' plot(subset(ctd, indices=1:10))
#'
#' @author Dan Kelley
#'
#' @family things related to ctd data
#' @family functions that subset oce objects
setMethod(f="subset",
          signature="ctd",
          definition=function(x, subset, ...) {
              subsetString <- paste(deparse(substitute(expr=subset, env=environment())), collapse=" ")
              dots <- list(...)
              dotsNames <- names(dots)
              indicesGiven <- length(dots) && ("indices" %in% dotsNames)
              if (indicesGiven) {
                  if (!missing(subset))
                      stop("cannot specify both 'subset' and 'indices'")
                  res <- x
                  indices <- dots[["indices"]]
                  for (i in seq_along(x@data)) {
                      res@data[[i]] <- x@data[[i]][indices]
                  }
                  for (i in seq_along(x@metadata$flags)) {
                      res@metadata$flags[[i]] <- x@metadata$flags[[i]][indices]
                  }
                  subsetString <- paste(deparse(substitute(expr=subset, env=environment())), collapse=" ")
                  res@processingLog <- processingLogAppend(res@processingLog,
                                                           paste("subset.ctd(x, subset=", subsetString, ")", sep=""))
                  return(res)
              }
              res <- new("ctd")
              res@metadata <- x@metadata
              res@processingLog <- x@processingLog
              ## FIXME: next 2 lines used to be in the loop but I don't see why, so moved out
              r <- eval(substitute(expr=subset, env=environment()), x@data, parent.frame(2))
              r <- r & !is.na(r)
              for (i in seq_along(x@data)) {
                  res@data[[i]] <- x@data[[i]][r]
              }
              for (i in seq_along(x@metadata$flags)) {
                  res@metadata$flags[[i]] <- x@metadata$flags[[i]][r]
              }
              names(res@data) <- names(x@data)
              subsetString <- paste(deparse(substitute(expr=subset, env=environment())), collapse=" ")
              res@processingLog <- processingLogAppend(res@processingLog,
                                                       paste("subset.ctd(x, subset=", subsetString, ")", sep=""))
              res
          })


#' Plot CTD data in a Low-Level Fashion
#'
#' Plot CTD data as time-series against scan number, to help with trimming
#' extraneous data from a CTD cast.
#'
#' @param x a [ctd-class] object.
#'
#' @param which Numerical vector numerical codes specifying the panels to draw: 1
#' for pressure vs scan, 2 for `diff(pressure)` vs scan, 3 for temperature vs
#' scan, and 4 for salinity vs scan.
#'
#' @param xtype Character string indicating variable for the x axis. May be
#' `"scan"` (the default) or `"time"`. In the former case, a
#' `scan` variable will be created using [seq_along()],
#' if necessary. In the latter case, an error results if the `data`
#' slot of `x` lacks a variable called `time`.
#'
#' @param flipy Logical value, ignored unless `which` is 1. If `flipy`
#' is `TRUE`, then a pressure plot will have high pressures at the bottom
#' of the axis.
#'
#' @param type Character indicating the line type, as for [plot.default()]. The default
#' is `"l"`, meaning to connect data with line segments. Another good choice is
#' `"o"`, to add points at the data.
#'
#' @param xlim Limits on the x value. The default, `NULL`, is to select this
#' from the data.
#'
#' @param ylim Limits on the y value. The default, `NULL`, is to select this
#' from the data.
#'
#' @param mgp Three-element numerical vector to use for [par]`(mgp)`, and also
#' for [par]`(mar)`, computed from this.  The default is tighter than the R
#' default, in order to use more space for the data and less for the axes.
#'
#' @param mar Four-element vector be used with [par]`("mar")`.  If set
#' to `NULL`, then [par]`("mar")` is used.  A good choice for a TS diagram
#' with a palette to the right is `mar=par("mar")+c(0, 0, 0, 1))`.
#'
#' @param ... Optional arguments passed to plotting functions.
#' @template debugTemplate
#'
#' @examples
#' library(oce)
#' data(ctdRaw)
#' plotScan(ctdRaw)
#' abline(v=c(130, 350), col='red') # useful for ctdTrim()
#'
#' @author Dan Kelley
#'
#' @family functions that plot oce data
#' @family things related to ctd data
plotScan <- function(x, which=1, xtype="scan", flipy=FALSE,
                     type='l', mgp=getOption("oceMgp"),
                     xlim=NULL, ylim=NULL,
                     mar=c(mgp[1]+1.5, mgp[1]+1.5, mgp[1], mgp[1]), ..., debug=getOption("oceDebug"))
{
    if (!inherits(x, "ctd"))
        stop("method is only for objects of class '", "ctd", "'")
    nw <- length(which)
    if (nw > 1)
        par(mfrow=c(nw, 1))
    par(mar=mar)
    par(mgp=mgp)
    xtype <- match.arg(xtype, c("scan", "time"))
    for (w in which) {
        if (xtype == "scan") {
            xvar <- if ("scan" %in% names(x@data)) x[["scan"]] else seq_along(x[["pressure"]])
            if (missing(xlim))
                xlim <- range(xvar, na.rm=TRUE)
            if (w == 1) {
                if (missing(ylim)) {
                    ylim <- range(x[["pressure"]], na.rm=TRUE)
                    if (flipy)
                        ylim <- rev(ylim)
                }
                plot(xvar, x[["pressure"]], xlab="Scan", ylab=resizableLabel("p", "y", debug=debug-1),
                     yaxs='r', type=type, xlim=xlim, ylim=ylim, ...)
            } else if (w == 2) {
                if (missing(ylim))
                    ylim <- range(diff(x[["pressure"]]), na.rm=TRUE)
                plot(xvar[-1], diff(x[["pressure"]]), xlab="Scan", ylab="diff(pressure)",
                     yaxs='r', type=type, xlim=xlim, ylim=ylim, ...)
            } else if (w == 3) {
                if (missing(ylim))
                    ylim <- range(diff(x[["temperature"]]), na.rm=TRUE)
                plot(xvar, x[["temperature"]], xlab="Scan", ylab=resizableLabel("T", "y", debug=debug-1),
                     yaxs='r', type=type, xlim=xlim, ylim=ylim, ...)
            } else if (w == 4) {
                if (missing(ylim))
                    ylim <- range(diff(x[["salinity"]]), na.rm=TRUE)
                plot(xvar, x[["salinity"]], xlab="Scan", ylab=resizableLabel("S", "y", debug=debug-1),
                     yaxs='r', type=type, xlim=xlim, ylim=ylim, ...)
            } else {
                stop("unknown 'which'; must be in 1:4")
            }
        } else if (xtype == "time") {
            time <- x[["time"]]
            if (is.null(time))
                stop("there is no 'time' in this ctd object")
            if (missing(xlim))
                xlim <- range(time)
            if (w == 1) {
                if (missing(ylim))
                    ylim <- range(x[["pressure"]], na.rm=TRUE)
                oce.plot.ts(time, x[["pressure"]], ylab=resizableLabel("p", "y", debug=debug-1),
                            yaxs='r', type=type, flipy=flipy, xlim=xlim, ylim=ylim, ...)
            } else if (w == 2) {
                if (missing(ylim))
                    ylim <- range(diff(x[["pressure"]]), na.rm=TRUE)
                oce.plot.ts(time[-1], diff(x[["pressure"]]), ylab="diff(pressure)",
                            yaxs='r', type=type, flipy=flipy, xlim=xlim, ylim=ylim, ...)
            } else if (w == 3) {
                if (missing(ylim))
                    ylim <- range(x[["temperature"]], na.rm=TRUE)
                oce.plot.ts(time, x[["temperature"]], ylab=resizableLabel("T", "y", debug=debug-1),
                            yaxs='r', type=type, flipy=flipy, xlim=xlim, ylim=ylim, ...)
            } else if (w == 4) {
                if (missing(ylim))
                    ylim <- range(x[["salinity"]], na.rm=TRUE)
                oce.plot.ts(time, x[["salinity"]], ylab=resizableLabel("S", "y", debug=debug-1),
                            yaxs='r', type=type, flipy=flipy, xlim=xlim, ylim=ylim, ...)
            } else {
                stop("unknown 'which'; must be in 1:4")
            }
        }
    }
}

#' Read a General CTD File
#'
#' @template readCtdTemplate
#'
#' @author Dan Kelley
#'
#' @param type If `NULL`, then the first line is studied, in order to
#' determine the file type, and control is dispatched to a specialized
#' function to handle that type.  Otherwise, `type` must be a string.
#' If `type="SBE19"` then a Seabird file format is assumed,
#' and control is dispatched to [read.ctd.sbe()].
#' If `type="WOCE"` then a WOCE-exchange file is assumed,
#' and control is dispatched to [read.ctd.woce()].
#' If `type="ITP"` then an ice-tethered profiler file is assumed,
#' and control is dispatched to [read.ctd.itp()].
#' If `type="ODF"` then an ODF file (used by the Canadian Department of
#' Fisheries and Ocean) is assumed,
#' and control is dispatched to [read.ctd.odf()].
#' Finally, if `type="ODV"` then an ODV file (used by Ocean Data View software) is assumed,
#' and control is dispatched to [read.ctd.odv()].
#'
#' @family functions that read ctd data
read.ctd <- function(file, type=NULL, columns=NULL, station=NULL, missingValue, deploymentType="unknown",
                     monitor=FALSE, debug=getOption("oceDebug"), processingLog, ...)
{
    if (!missing(file) && is.character(file) && 0 == file.info(file)$size)
        stop("empty file")
    oceDebug(debug, "read.ctd(..., type=", if (is.null(type)) "NULL" else "\"", type, "\") {\n", sep="")
    ## Special case: ruskin files are handled by read.rsk()
    if (is.character(file) && length(grep(".rsk$", file))) {
        return(read.rsk(file=file, debug=debug))
    }

    if (missing(processingLog)) processingLog <- paste(deparse(match.call()), sep="", collapse="")
    ##ofile <- file
    ##filename <- NULL
    if (is.null(type)) {
        if (is.character(file)) {
            if (length(grep(".rsk$", file))) {
                return(read.rsk(file=file, debug=debug))
            }
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
        } else if (1 == length(grep("^[ ]*ODF_HEADER,[ ]*$", line))) {
            type <- "ODF"
        } else {
            stop("Cannot discover type in line '", line, "'\n")
        }
    } else {
        if (!is.na(pmatch(type, "SBE19"))) {
            type <- "SBE19"
        } else if (!is.na(pmatch(type, "WOCE"))) {
            type <- "WOCE"
        ##issue_1696 } else {
        ##issue_1696     stop("type must be SBE19, WOCE, ODF, ODV, or ITP, not ", type)
        }
    }                                   # FIXME: should just use oce.magic() here
    res <- switch(type,
                  SBE19=read.ctd.sbe(file, columns=columns, station=station,
                                     missingValue=missingValue, deploymentType=deploymentType,
                                     monitor=monitor, debug=debug, processingLog=processingLog, ...),
                  WOCE=read.ctd.woce(file, columns=columns, station=station,
                                     missingValue=missingValue, deploymentType=deploymentType,
                                     monitor=monitor, debug=debug, processingLog=processingLog, ...),
                  ODF=read.ctd.odf(file, columns=columns, station=station,
                                   missingValue=missingValue, deploymentType=deploymentType,
                                   monitor=monitor, debug=debug, processingLog=processingLog, ...),
                  ITP=read.ctd.itp(file, columns=columns, station=station,
                                   missingValue=missingValue, deploymentType=deploymentType,
                                   monitor=monitor, debug=debug, processingLog=processingLog, ...),
                  ODV=read.ctd.odv(file, columns=columns, station=station,
                                   missingValue=missingValue, deploymentType=deploymentType,
                                   monitor=monitor, debug=debug, processingLog=processingLog, ...))
    res
}

#' Parse a Latitude or Longitude String
#'
#' Parse a latitude or longitude string, e.g. as in the header of a CTD file
#' The following formats are understood (for, e.g. latitude):
#'```
#' ** NMEA Latitude = 47 54.760 N
#' ** Latitude: 47 53.27 N
#'```
#' Note that [iconv()] is called to convert the string to ASCII before
#' decoding, to change any degree (or other non-ASCII) symbols to blanks.
#'
#' @param line a character string containing an indication of latitude or
#' longitude.
#'
#' @param debug a flag that turns on debugging.  Set to 1 to get a moderate
#' amount of debugging information, or to 2 to get more.
#'
#' @return A numerical value of latitude or longitude.
#'
#' @author Dan Kelley
#'
#' @seealso Used by [read.ctd()].
parseLatLon <- function(line, debug=getOption("oceDebug"))
{
    ## The following formats are understood (for, e.g. latitude)
    ## * NMEA Latitude = 47 54.760 N
    ## ** Latitude:      47 53.27 N
    x <- line
    ## degree signs will be '?' by prior conversion; make them blank
    x <- gsub("\\?", " ", x)
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
    ## One last pass to get rid of non-digits, so that e.d. "30d 10s'" will work
    x <- gsub("[a-zA-Z']", "", x)
    oceDebug(debug, "  step 4. \"", x, "\" (now should have no non-digit characters)\n", sep="")
    ## if single number, it's decimal degrees; if two numbers, degrees and then decimal minutes
    xx <- strsplit(x, '[ \\t]+')[[1]]
    if (1 == length(xx)) {
        res <- suppressWarnings(as.numeric(xx))
        oceDebug(debug, "  step 5. \"", res, "\" (inferred from single #, decimal degrees)\n", sep="")
    } else if (2 == length(xx)) {
        res <- suppressWarnings(as.numeric(xx[1]) + as.numeric(xx[2]) / 60)
        oceDebug(debug, "  step 5. \"", res, "\" (inferred from two #, degrees and decimal minutes)\n", sep="")
    } else if (3 == length(xx)) {
        res <- suppressWarnings(as.numeric(xx[1]) + as.numeric(xx[2]) / 60 + as.numeric(xx[3]) / 3600)
        oceDebug(debug, "  step 5. \"", res, "\" (inferred from three #, degrees, minutes, and seconds)\n", sep="")
    } else {
        ## 2014-06-17 it's annoying to see this error msg
        res <- NA
    }
    res <- res * sign
    if (is.na(res))
        warning("cannot decode longitude or latitude from '", line, "'")
    oceDebug(debug, "} # parseLatLon()\n", unindent=1)
    res
}

time.formats <- c("%b %d %Y %H:%M:%s", "%Y%m%d")




#' Plot Temperature-Salinity Diagram
#'
#' Creates a temperature-salinity plot for a CTD cast, with labeled isopycnals.
#'
#' The isopycnal curves (along which density is constant) are
#' drawn with [drawIsopycnals()], which also places
#' labels in the margins showing density minus 1000 \eqn{kg/m^3}{kg/m^3}.
#' If `trimIsopycnals` is `TRUE` (which is the default), these curves
#' are trimmed to the region within which the results of density calculation
#' in the chosen equation of state (`eos`) are considered to be reliable.
#'
#' With `eos="unesco"` this region includes
#' Practical Salinity from 0 to 42 and Potential Temperature
#' from -2C to 40C, in accordance with Fofonoff and Millard
#' (1983, page 23).
#'
#' With `eos="gsw"` the lower
#' limit of Absolute Salinity validity is taken as 0 g/kg,
#' in accordance with both McDougall et al. (2003 section 3)
#' and the TEOS-10/gsw Matlab code for the so-called "funnel" of validity.
#' However, an appropriate upper limit on Absolute Salinity is not as clear.
#' Here, the value 42 g/kg is chosen to match the "funnel" Matlab code
#' as of July 2020, but two other choices might have been
#' made. One is 50 g/kg, since [gsw::gsw_SA_from_rho()] returns `NA` values
#' for Absolute Salinities exceeding that value, and another is
#' 40 g/kg, as in McDougall et al. (2003 section 3).
#' The Conservative Temperature range is set to run from -2C
#' to 33C, as in McDougall et al. (2003 section 3), even though the
#' "funnel" imposes no upper limit on this variable.
#'
#' @param x a [ctd-class], [argo-class] or [section-class] object, or a list
#' containing solely [ctd-class] objects or [argo-class] objects.
#'
#' @param inSitu A boolean indicating whether to use in-situ temperature or
#' (the default) potential temperature, calculated with reference pressure
#' given by `referencePressure`.  This is ignored if `eos="gsw"`,
#' because those cases the y axis is necessarily the conservative formulation
#' of temperature.
#'
#' @param type representation of data, `"p"` for points, `"l"` for
#' connecting lines, or `"n"` for no indication.
#'
#' @param referencePressure reference pressure, to be used in calculating
#' potential temperature, if `inSitu` is `FALSE`.
#'
#' @param nlevels Number of automatically-selected isopycnal levels (ignored if
#' `levels` is supplied).
#'
#' @param levels Optional vector of desired isopycnal levels.
#'
#' @param grid a flag that can be set to `TRUE` to get a grid.
#'
#' @param col.grid color for grid.
#'
#' @param lty.grid line type for grid.
#'
#' @param rho1000 if TRUE, label isopycnals as e.g. 1024; if FALSE, label as
#' e.g. 24
#'
#' @param eos equation of state to be used, either `"unesco"` or
#' `"gsw"`.
#'
#' @param cex character-expansion factor for symbols, as in [par]`("cex")`.
#'
#' @param pch symbol type, as in [par]`("pch")`.
#'
#' @param bg optional color to be painted under plotting area, before
#' plotting.  (This is useful for cases in which `inset=TRUE`.)
#'
#' @param pt.bg inside color for symbols with `pch` in 21:25
#'
#' @param col color for symbols.
#'
#' @param col.rho color for isopycnal lines and their labels.
#'
#' @param cex.rho size of the isopycnal labels.
#'
#' @param rotate if TRUE, labels in right-hand margin are written vertically
#'
#' @param useSmoothScatter if TRUE, use [smoothScatter()] to plot the
#' points.
#'
#' @param xlab optional label for the x axis, with default "Salinity \[PSU\]".
#'
#' @param ylab optional label for the y axis, with default "Temperature \[C\]".
#'
#' @param Slim optional limits for salinity axis, otherwise inferred from visible data
#' (i.e. the data that have finite values for both salinity and temperature).
#'
#' @param Tlim as `Slim`, but for temperature.
#'
#' @param drawFreezing logical indication of whether to draw a freezing-point
#' line. This is based on zero pressure. If `eos="unesco"` then
#' [swTFreeze()] is used to compute the curve, whereas if
#' `eos="gsw"` then [gsw::gsw_CT_freezing()] is used;
#' in each case, zero pressure is used.
#'
#' @param trimIsopycnals logical value (`TRUE` by default) that
#' indicates whether to trim isopycnal curves
#' to the region of temperature-salinity space for which density
#' computations are considered to be valid in the context of the
#' chosen `eos`; see \dQuote{Details}.
#'
#' @param mgp 3-element numerical vector to use for `[par](mgp)`, and also
#' for [par]`(mar)`, computed from this.  The default is tighter than the R
#' default, in order to use more space for the data and less for the axes.
#'
#' @param mar value to be used with [par]`("mar")`.  If set to
#' `NULL`, then [par]`("mar")` is used.  A good choice for a TS diagram
#' with a palette to the right is `mar=par("mar")+c(0, 0, 0, 1))`.
#'
#' @param lwd line width of lines or symbols.
#'
#' @param lty line type of lines or symbols.
#'
#' @param lwd.rho line width for density curves.
#'
#' @param lty.rho line type for density curves.
#'
#' @param add a flag that controls whether to add to an existing plot.  (It
#' makes sense to use `add=TRUE` in the `panel` argument of a
#' [coplot()], for example.)
#'
#' @param inset set to `TRUE` for use within [plotInset()].  The
#' effect is to prevent the present function from adjusting margins, which is
#' necessary because margin adjustment is the basis for the method used by
#' [plotInset()].
#'
#' @param debug a flag that turns on debugging.  Set to 1 to get a moderate
#' amount of debugging information, or to 2 to get more.
#'
#' @param \dots optional arguments passed to plotting functions.
#'
#' @return A list is silently returned, containing `xat` and `yat`,
#' values that can be used by [oce.grid()] to add a grid to the plot.
#'
#' @author Dan Kelley
#'
#' @seealso [summary,ctd-method()] summarizes the information, while
#' [read.ctd()] scans it from a file.
#'
#' @examples
#' ## For a simple ctd object
#' library(oce)
#' data(ctd)
#' plotTS(ctd)
#'
#' ## For a section object (note the outlier!)
#' data(section)
#' plotTS(section)
#'
#' ## Adding a colormap based on a different variable, e.g. oxygen
#' marOrig <- par("mar") # so later plots with palettes have same margins
#' cm <- colormap(section[['oxygen']])
#' drawPalette(colormap=cm, zlab='Oxygen')
#' plotTS(section, pch=19, col=cm$zcol, mar=par('mar')) # the mar adjusts for the palette
#'
#' ## Coloring based on station:
#' Tlim <- range(section[['temperature']], na.rm=TRUE)
#' Slim <- range(section[['salinity']], na.rm=TRUE)
#' cm <- colormap(seq_along(section[['latitude', 'byStation']]))
#' par(mar=marOrig) # same as previous plot
#' drawPalette(colormap=cm, zlab='Latitude')
#' plotTS(section, Tlim=Tlim, Slim=Slim, pch=NA, mar=par('mar'))
#' jnk <- mapply(
#'     function(s, col) {
#'         plotTS(s, col=col, add=TRUE, type='l')
#'     },
#'     section[['station']], col=cm$zcol)
#'
#' ## Show TS for an argo object
#' data(argo)
#' plotTS(handleFlags(argo))
#'
#' @references
#'
#' * Fofonoff, N. P., and R. C. Millard.
#' "Algorithms for Computation of Fundamental Properties of Seawater."
#' UNESCO Technical Papers in Marine Research. SCOR working group on Evaluation of CTD data;
#' UNESCO/ICES/SCOR/IAPSO Joint Panel on Oceanographic Tables and Standards, 1983.
#' \url{https://unesdoc.unesco.org/ark:/48223/pf0000059832}.
#'
#' * McDougall, Trevor J., David R. Jackett, Daniel G. Wright, and Rainer Feistel.
#' "Accurate and Computationally Efficient Algorithms for Potential Temperature and Density of Seawater."
#' Journal of Atmospheric and Oceanic Technology 20, no. 5 (May 1, 2003): 730-41.
#' \code{https://journals.ametsoc.org/jtech/article/20/5/730/2543/Accurate-and-Computationally-Efficient-Algorithms}.
#'
#' @family functions that plot oce data
#' @family things related to ctd data
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
                    cex=par("cex"), col=par("col"), pch=par("pch"),
                    bg="white", pt.bg="transparent",
                    col.rho=gray(0.5),
                    cex.rho=3/4*par("cex"),
                    rotate=TRUE,
                    useSmoothScatter=FALSE,
                    xlab, ylab,
                    Slim, Tlim,
                    drawFreezing=TRUE,
                    trimIsopycnals=TRUE,
                    mgp=getOption("oceMgp"),
                    mar=c(mgp[1]+1.5, mgp[1]+1.5, mgp[1], mgp[1]),
                    lwd=par('lwd'), lty=par('lty'),
                    lwd.rho=par("lwd"), lty.rho=par("lty"),
                    add=FALSE, inset=FALSE,
                    debug=getOption("oceDebug"),
                    ...)
{
    oceDebug(debug, "plotTS(..., lwd.rho=", lwd.rho, ", lty.rho=", lty.rho, ",",
             "Slim=", if (!missing(Slim)) paste("c(", Slim[1], ",", Slim[2], ")") else "(missing)", ", ",
             "Tlim=", if (!missing(Tlim)) paste("c(", Tlim[1], ",", Tlim[2], ")") else "(missing)", ", ",
             "eos=\"", eos, "\", ",
             "mgp=c(", paste(mgp, collapse=","), "), ",
             "mar=c(", paste(mar, collapse=","), "), ",
             "debug=", debug, ", ...) {\n", sep="", unindent=1, style="bold")
    eos <- match.arg(eos, c("unesco", "gsw"))
    xat <- NULL
    yat <- NULL
    if (!inherits(x, "ctd")) {
        if (inherits(x, "section")) {
            oceDebug(debug, "x is a section object\n")
            if (eos == "gsw") {
                x <- as.ctd(x[["salinity"]], x[["temperature"]], x[["pressure"]],
                            longitude=x[["longitude"]], latitude=x[["latitude"]])
            } else {
                x <- as.ctd(x[["salinity"]], x[["temperature"]], x[["pressure"]])
            }
        } else if (inherits(x, "argo")) {
            oceDebug(debug, "x is an argo object\n")
            ## Copy fields into a CTD object.
            SP <- x[["salinity"]]
            dim <- dim(SP)
            SP <- as.vector(SP)
            t <- as.vector(x[["temperature"]])
            p <- as.vector(x[["pressure"]])
            longitude <- x[["longitude"]]
            latitude <- x[["latitude"]]
            if (length(longitude) < length(SP)) {
                ## Copy across depths. This is inside a conditional because
                ## possibly argo[["longitude"]] should mimic section[["longitude"]],
                ## in doing the lengthing by itself unless the second argument is
                ## "byStation" (issue 1273 ... under consideration 2017jul12)
                longitude <- rep(x[["longitude"]], each=dim[1])
                latitude <- rep(x[["latitude"]], each=dim[1])
            }
            x <- as.ctd(SP, t, p, longitude=longitude, latitude=latitude)
        } else if (is.list(x)) {
            oceDebug(debug, "x is a list\n")
            if (inherits(x[[1]], "ctd")) {
                oceDebug(debug, "x is a list of ctd objects\n")
                x <- if (eos == "gsw") {
                    as.ctd(salinity=unlist(lapply(x, function(xi) xi[["salinity"]])),
                           temperature=unlist(lapply(x, function(xi) xi[["temperature"]])),
                           pressure=unlist(lapply(x, function(xi) xi[["pressure"]])),
                           longitude=unlist(lapply(x, function(xi) xi[["longitude"]])),
                           latitude=unlist(lapply(x, function(xi) xi[["latitude"]])))
                } else {
                    as.ctd(unlist(lapply(x, function(xi) xi[["salinity"]])),
                           unlist(lapply(x, function(xi) xi[["temperature"]])),
                           unlist(lapply(x, function(xi) xi[["pressure"]])))
                }
            } else if (inherits(x[[1]], "argo")) {
                oceDebug(debug, "x is a list of argo objects\n")
                message("FIXME: this ought to be done with as.ctd() so other methods can do simiarly")
                message("FIXME: determine if 1-col or multi-col (affects latitude lookup)")
                x <- if (eos == "gsw") {
                    stop("FIXME: for argo (gsw)")
                } else {
                    stop("FIXME: for argo (unesco)")
                }
            } else {
                stop("If x is a list, it must hold 'ctd' or 'argo' objects")
            }
        } else {
            oceDebug(debug, "x is a not a ctd object, nor a list\n")
            names <- names(x)
            if ("temperature" %in% names && "salinity" %in% names) {
                if (eos == "gsw") {
                    if (!("longitude" %in% names))
                        stop("need to know longitude and latitude if eos=\"gsw\"")
                    x <- as.ctd(x$salinity, x$temperature, x$pressure, longitude=x$longitude, latitude=x$latitude)
                } else {
                    x <- as.ctd(x$salinity, x$temperature, x$pressure)
                }
            } else {
                if (eos == "gsw") {
                    x <- as.ctd(x[["salinity"]], x[["temperature"]], x[["pressure"]], longitude=x[["longitude"]],
                                latitude=x[["latitude"]])
                } else {
                    x <- as.ctd(x[["salinity"]], x[["temperature"]], x[["pressure"]])
                }
            }
        }
    } else {
        oceDebug(debug, "x is a ctd object\n")
    }
    if (eos == "gsw") {
        salinity <- x[["SA"]]
        y <- x[["CT"]]
        oceDebug(debug, "Absolute Salinity ranges ", paste(range(salinity,na.rm=TRUE), collapse=" to "), "\n")
    } else {
        salinity <- x[["salinity"]]
        y <- if (inSitu) x[["temperature"]] else swTheta(x, referencePressure=referencePressure, eos=eos)
        oceDebug(debug, "Practical Salinity ranges ", paste(range(salinity,na.rm=TRUE), collapse=" to "), "\n")
    }
    ## Can only plot if both S and T are finite, so we trim S and T, at
    ## this point called salinity and y, and also bg, col, cex, and pch.
    ## See https://github.com/dankelley/oce/issues/1730
    canPlot <- is.finite(salinity) & is.finite(y)
    if (length(col) == length(y))
        col <- col[canPlot]
    if (length(bg) == length(y))
        bg <- bg[canPlot]
    if (length(cex) == length(y))
        cex <- cex[canPlot]
    if (length(pch) == length(y))
        pch <- pch[canPlot]
    salinity <- salinity[canPlot]
    y <- y[canPlot]
    if (!any(is.finite(salinity))) {
        warning("no valid salinity data")
        return(invisible(list(xat=NULL, yat=NULL)))
    }
    if (!any(is.finite(y))) {
        warning("no valid temperature data")
        return(invisible(list(xat=NULL, yat=NULL)))
    }
    if (missing(Slim)) {
        Slim <- range(salinity, na.rm=TRUE)
        oceDebug(debug, "Slim was not given, so inferred Slim=c(", paste(Slim, collapse=","), ") from the data\n", sep="")
    }
    if (missing(Tlim)) {
        Tlim <- range(y, na.rm=TRUE)
        oceDebug(debug, "Tlim was not given, so inferred Tlim=c(", paste(Tlim, collapse=","), ") from the data\n", sep="")
    }
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
    ##axisNameLoc <- mgp[1]
    if (missing(xlab)) {
        if (eos == "gsw")
            xlab <- resizableLabel("absolute salinity", "x", debug=debug-1)
        else
            xlab <- resizableLabel("S", "x", debug=debug-1)
    }
    if (missing(ylab)) {
        if (eos == "gsw")
            ylab <- resizableLabel("conservative temperature", "y", debug=debug-1)
        else
            ylab <- if (inSitu) resizableLabel("T", "y", debug=debug-1) else resizableLabel("theta", "y", debug=debug-1)
    }
    if (useSmoothScatter) {
        smoothScatter(salinity, y,
                      xlab=xlab, ylab=ylab,
                      xaxs=if (min(salinity, na.rm=TRUE)==0) "i" else "r", # avoid plotting S<0
                      ## cex=cex, pch=pch, col=col, cex.axis=par("cex.axis"),
                      pch=pch, cex=cex, col=col,
                      xlim=Slim, ylim=Tlim,
                      ...)
    } else {
        if (add) {
            oceDebug(debug, "add=TRUE, so adding to an existing plot\n")
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
            return()
        } else {
            oceDebug(debug, "add=FALSE, so making new plot panel based on Slim and Tlim\n")
            plot(Slim, Tlim,
                 xlab=xlab, ylab=ylab,
                 xaxs=if (min(salinity, na.rm=TRUE)==0) "i" else "r", # avoid plotting S<0
                 cex=cex, pch=pch, col=col,# cex.axis=cex,
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
                   eos=eos,
                   trimIsopycnals=trimIsopycnals,
                   cex=cex.rho, col=col.rho, lwd=lwd.rho, lty=lty.rho)
    usr <- par("usr")
    Sr <- seq(max(0, usr[1]), usr[2], length.out=100)
    if (drawFreezing) {
        if (eos == "unesco") {
            lines(Sr, swTFreeze(salinity=Sr, pressure=0, eos=eos)) # old: darkblue that looked black
        } else if (eos == "gsw") {
            lines(Sr, gsw_CT_freezing(SA=Sr, p=0, saturation_fraction=1)) # Sr==SA since eos=="gsw"
        } else stop("unknown eos; must be \"unesco\" or \"gsw\"")
    }
    box()                              # redraw box (otherwise overdrawn with isopycnals)
    ## infer from par()
    xaxp <- par("xaxp")
    xat <- seq(xaxp[1], xaxp[2], length.out=1+xaxp[3])
    yaxp <- par("yaxp")
    yat <- seq(yaxp[1], yaxp[2], length.out=1+yaxp[3])
    oceDebug(debug, "} # plotTS(...)\n", sep="", unindent=1, style="bold")
    invisible(list(xat=xat, yat=yat))
}


#' Add Isopycnal Curves to TS Plot
#'
#' Adds isopycnal lines to an existing temperature-salinity plot.  This is
#' called by [plotTS()], and may be called by the user also, e.g. if
#' an image plot is used to show TS data density.
#'
#' @param nlevels suggested number of density levels (i.e. isopycnal curves);
#' ignored if `levels` is supplied.
#'
#' @param levels optional density levels to draw.
#'
#' @param rotate boolean, set to `TRUE` to write all density labels
#' horizontally.
#'
#' @param rho1000 boolean, set to `TRUE` to write isopycnal labels as e.g.
#' 1024 instead of 24.
#'
#' @param digits number of decimal digits to use in label (supplied to
#' [round()]).
#'
#' @param eos equation of state to be used, either `"unesco"` or
#' `"gsw"`.
#'
#' @param trimIsopycnals logical value (`TRUE` by default) that
#' indicates whether to trim isopycnal curves (if drawn)
#' to the region of temperature-salinity space for which density
#' computations are considered to be valid in the context of the
#' chosen `eos`; see the \dQuote{Details} of the documentation
#' for [plotTS()].
#'
#' @param cex size for labels.
#'
#' @param col color for lines and labels.
#'
#' @param lwd line width for isopycnal curves
#'
#' @param lty line type for isopycnal curves
#'
#' @return None.
#'
#' @seealso [plotTS()], which calls this.
#'
#' @references
#' * Fofonoff, N. P., and R. C. Millard.
#' "Algorithms for Computation of Fundamental Properties of Seawater."
#' UNESCO Technical Papers in Marine Research. SCOR working group on Evaluation of CTD data;
#' UNESCO/ICES/SCOR/IAPSO Joint Panel on Oceanographic Tables and Standards, 1983.
#' \url{https://unesdoc.unesco.org/ark:/48223/pf0000059832}.
#'
#' * McDougall, Trevor J., David R. Jackett, Daniel G. Wright, and Rainer Feistel.
#' "Accurate and Computationally Efficient Algorithms for Potential Temperature and Density of Seawater."
#' Journal of Atmospheric and Oceanic Technology 20, no. 5 (May 1, 2003): 730-41.
#' \code{https://journals.ametsoc.org/jtech/article/20/5/730/2543/Accurate-and-Computationally-Efficient-Algorithms}.
#'
#' @author Dan Kelley
drawIsopycnals <- function(nlevels=6, levels, rotate=TRUE, rho1000=FALSE, digits=2,
                           eos=getOption("oceEOS", default='gsw'),
                           trimIsopycnals=TRUE,
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
    } else if (eos == "unesco") {
        rhoCorners <- swSigma(Scorners, Tcorners, rep(0, 4), eos=eos)
    } else {
        stop("unknown eos, \"", eos, "\"; please use \"unesco\" or \"gsw\"")
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
        ## Eliminate NA (for crazy T)
        ok <- !is.na(Sline)
        ## Prevent drawing in the invalid temperature-salinity region (see Details)
        if (eos == "unesco")
            ok <- ok & swTFreeze(Sline, 0, eos="unesco") < Tline
        if (trimIsopycnals) {
            validTS <- if (eos == "unesco") {
                # The use of 'ok &' below prevents NAs from creeping back in.
                ok & -2 <= Tline & Tline <= 40 & 0 <= Sline & Sline <= 42
            } else {
                ok & -2 <= Tline & Tline <= 33 & 0 <= Sline & Sline <= 40
            }
            ok <- ok & validTS
        }
        if (sum(ok) > 2) {
            Sok <- Sline[ok]
            Tok <- Tline[ok]
            lines(Sok, Tok, col=col, lwd=lwd, lty=lty)
            if (cex > 0) {
                if (Sok[length(Sok)] > SAxisMax) {
                    ## to right of box
                    i <- match(TRUE, Sok > SAxisMax)
                    if (rotate)
                        mtext(rhoLabel, side=4, at=Tok[i], line=0.1, cex=cex, col=col)
                    else
                        text(usr[2], Tok[i], rhoLabel, pos=4, cex=cex/cex.par, col=col, xpd=TRUE)
                } else {
                    ## above box ... if the line got there
                    if (max(Tok) > (TAxisMax - 0.05 * (TAxisMax - TAxisMin)))
                        mtext(rhoLabel, side=3, at=Sline[Tn], line=0.1, cex=cex, col=col)
                }
            }
        }
    }
}


#' Plot a CTD Profile
#'
#' Plot a profile, showing variation of some quantity (or quantities) with
#' pressure, using the oceanographic convention of putting lower pressures
#' nearer the top of the plot. This works for any `oce` object that has a
#' pressure column in its `data` slot.
#' The colors (`col.salinity`, etc.) are only used if two profiles appear
#' on a plot.
#'
#' @param x a [ctd-class] object.
#'
#' @param xtype Item(s) plotted on the x axis, either a vector of length equal
#' to that of `pressure` in the `data` slot,
#' or a text code from the list below.
#'
#' * `"salinity"` Profile of salinity.
#'
#' * `"conductivity"` Profile of conductivity.
#'
#' * `"temperature"` Profile of *in-situ* temperature.
#'
#' * `"theta"` Profile of potential temperature.
#'
#' * `"density"` Profile of density (expressed as \eqn{\sigma_\theta}{sigma_theta}).
#'
#' * `"index"` Index of sample (useful for working with [ctdTrim()]).
#'
#' * `"salinity+temperature"` Profile of salinity and temperature within a single axis frame.
#'
#' * `"N2"` Profile of square of buoyancy frequency \eqn{N^2}{N^2},
#' calculated with [swN2()] with
#' an optional argument setting of `df=length(x[["pressure"]])/4` to do
#' some smoothing.
#'
#' * `"density+N2"` Profile of sigma0 and
#' the square of buoyancy frequency within a single axis frame.
#'
#' * `"density+dpdt"` Profile of sigma0 and dP/dt for the
#' sensor.  The latter is useful in indicating problems with the deployment.
#' It is calculated by first differencing pressure and then using a smoothing
#' spline on the result (to avoid grid-point wiggles that result because the
#' SBE software only writes 3 decimal places in pressure).  Note that dP/dt may
#' be off by a scale factor; this should not be a problem if there is a
#' `time` column in the `data` slot, or a `sample.rate` in the
#' `metadata` slot.
#'
#' * `"sigma0"`, `"sigma1"`, `"sigma2"`, `"sigma3"`
#' and `"sigma4"` Profile of potential density referenced
#' to 0dbar (i.e. the surface), 1000dbar, 2000dbar, 3000dbar, and 4000dbar.
#'
#' * `"spice"` Profile of spice.
#'
#' * `"Rrho"` Profile of Rrho, defined in the diffusive sense.
#'
#' * `"RrhoSF"` Profile of Rrho, defined in the salt-finger sense.
#'
#'
#' @param ytype variable to use on y axis. The valid choices are:
#' `"pressure"` (the default), `"z"`,
#' `"depth"` and `"sigmaTheta"`.
#'
#' @param eos equation of state to be used, either `"unesco"` or
#' `"gsw"`.
#'
#' @param xlab optional label for x axis (at top of plot). If not
#' provided, the value of `xtype` is used (along with the units
#' of the variable) to construct a label. For user-supplied `xlab`,
#' the unit (if any) must be specified as in e.g.
#' `xlab=expression("Speed ["*m/s*"]")`.
#'
#' @param ylab optional label for y axis.  See `xlab` for a note on
#' units.  Setting `ylab=""` prevents labelling the axis.
#'
#' @param lty line type for the profile.
#'
#' @param col color for a general profile.
#'
#' @param col.salinity color for salinity profile (see \dQuote{Details}).
#'
#' @param col.temperature color for temperature (see \dQuote{Details}).
#'
#' @param col.rho color for density (see \dQuote{Details}).
#'
#' @param col.N2 color for square of buoyancy frequency (see
#' \dQuote{Details}).
#'
#' @param col.dpdt color for dP/dt.
#'
#' @param col.time color for delta-time.
#'
#' @param pt.bg inside color for symbols with `pch` in 21:25
#'
#' @param grid logical, set to `TRUE` to get a grid.
#'
#' @param col.grid color for grid.
#'
#' @param lty.grid line type for grid.
#'
#' @param Slim Optional limit for S axis
#'
#' @param Clim Optional limit for conductivity axis
#'
#' @param Tlim Optional limit for T axis
#'
#' @param densitylim Optional limit for density axis
#'
#' @param N2lim Optional limit for N2 axis
#'
#' @param Rrholim Optional limit for Rrho axis
#'
#' @param dpdtlim Optional limit for dp/dt axis
#'
#' @param timelim Optional limit for delta-time axis
#'
#' @param plim Optional limit for pressure axis, ignored unless
#' `ytype=="pressure"`, in which case it takes precedence over
#' `ylim`.
#'
#' @param xlim Optional limit for x axis, which can apply to any plot type.
#' This is ignored if the plotted x variable is something for which a limit
#' may be specified with an argument, e.g. `xlim` is ignored for a
#' salinity profile, because `Slim` ought to be given in such a case.
#'
#' @param ylim Optional limit for y axis, which can apply to any plot type,
#' although is overridden by `plim` if `ytype` is `"pressure"`
#' or by `densitylim` if `ytype` is `"sigmaTheta"`.
#'
#' @param lwd lwd value for data line
#'
#' @param xaxs value of [par()] `xaxs` to use
#'
#' @param yaxs value of [par()] `yaxs` to use
#'
#' @param cex size to be used for plot symbols (see [par()])
#'
#' @param pch code for plotting symbol (see [par()]).
#'
#' @param useSmoothScatter boolean, set to `TRUE` to use
#' [smoothScatter()] instead of [plot()] to draw the plot.
#'
#' @param df optional argument, passed to [swN2()] if provided, and
#' if a plot using \eqn{N^2}{N^2} is requested.
#'
#' @param keepNA FALSE
#'
#' @param type type of plot to draw, using the same scheme as
#' [plot()].
#'
#' @param mgp 3-element numerical vector to use for [par]`(mgp)`, and also
#' for [par]`(mar)`, computed from this.  The default is tighter than the R
#' default, in order to use more space for the data and less for the axes.
#'
#' @param mar Four-element numerical value to be used to set the plot
#' margins, with a call to [par]`(mar)` prior to the plot.
#' If this is not supplied, a reasonable default will be set up.
#'
#' @param add A logical value that controls whether to add to an existing plot.  (It
#' makes sense to use `add=TRUE` in the `panel` argument of a
#' [coplot()], for example.)
#'
#' @param inset A logical value indicating whether to draw an inset plot.
#' Setting this to `TRUE` will prevent the present function from adjusting
#' the margins, which is
#' necessary because margin adjustment is the basis for the method used by
#' [plotInset()].
#'
#' @param debug a flag that turns on debugging.  Set to 1 to get a moderate
#' amount of debugging information, or to 2 to get more.
#'
#' @param \dots optional arguments passed to other functions.  A common example
#' is to set `df`, for use in [swN2()] calculations.
#'
#' @return None.
#'
#' @author Dan Kelley
#'
#' @seealso [read.ctd()] scans ctd information from a file,
#' [plot,ctd-method()] is a general plotting function for `ctd`
#' objects, and [plotTS()] plots a temperature-salinity diagrams.
#'
#' @examples
#' library(oce)
#' data(ctd)
#' plotProfile(ctd, xtype="temperature")
#'
#' @family functions that plot oce data
#' @family things related to ctd data
plotProfile <- function(x,
                        xtype="salinity+temperature", ytype="pressure",
                        eos=getOption("oceEOS", default="gsw"),
                        lty=1,
                        xlab=NULL, ylab=NULL,
                        col='black',
                        col.salinity="darkgreen",
                        col.temperature="red",
                        col.rho="blue",
                        col.N2="brown",
                        col.dpdt="darkgreen",
                        col.time="darkgreen",
                        pt.bg="transparent",
                        grid=TRUE,
                        col.grid="lightgray",
                        lty.grid="dotted",
                        Slim, Clim, Tlim, densitylim, N2lim, Rrholim, dpdtlim, timelim, plim,
                        xlim, ylim,
                        lwd=par("lwd"),
                        xaxs="r",
                        yaxs="r",
                        cex=1, pch=1,
                        useSmoothScatter=FALSE,
                        df,
                        keepNA=FALSE,
                        type='l',
                        mgp=getOption("oceMgp"),
                        mar,
                        add=FALSE,
                        inset=FALSE,
                        debug=getOption("oceDebug", 0),
                        ...)
{
    debug <- min(debug, 3)
    oceDebug(debug, "plotProfile(x, xtype=",
             ifelse(is.character(xtype), paste0("\"",xtype,"\""), "(numeric)"),
             ", xlab=", if (is.null(xlab)) "NULL" else paste('"', xlab, '"', sep=""),
             ", debug=", debug, ", ...) {\n", sep="", style="bold", unindent=1)
    eos <- match.arg(eos, c("unesco", "gsw"))
    if (missing(mar)) {
        ## default behaviour changed 20161020 for issue #1103
        mar <- c(1 + if (length(grep('\\+', xtype))) mgp[1] else 0, mgp[1]+1.5, mgp[1]+1.5, mgp[1])
        if (length(xtype) == 1 && xtype %in% names(x@data))
            mar[1] <- 1 # the bottom margin is wrong for e.g. NO2+NO3
    }
    plimGiven <- !missing(plim)

    plotJustProfile <- function(x, y, col="black", type="l", lty=lty,
                                xlim=NULL, ylim=NULL,
                                xlab=NULL,
                                lwd=par("lwd"),
                                cex=1, pch=1, pt.bg="transparent",
                                df=df, keepNA=FALSE, debug=getOption("oceDebug", 0))
    {
        oceDebug(debug, "plotJustProfile(...,",
                 argShow(col),
                 ", debug=", debug, ") {\n", sep="", style="bold", unindent=1)
        if (is.null(xlab))
            xlab <- ""
        x <- as.vector(x) # because e.g. argo may be a 1-col matrix
        y <- as.vector(y)
        if (!keepNA) {
            keep <- !is.na(x) & !is.na(y)
            x <- x[keep]
            y <- y[keep]
            if (length(x) < 1 || length(y) < 1) {
                warning("no good data to plot")
                return(invisible(NULL))
            }
        }
        if (type == 'l') {
            lines(x, y, col=col, lwd=lwd, lty=lty, ...)
        } else if (type == 's') {
            lines(x, y, col=col, lwd=lwd, lty=lty, type='s')
        } else if (type == 'p') {
            points(x, y, col=col, cex=cex, pch=pch, bg=pt.bg)
        } else if (type == 'o') {
            lines(x, y, col=col, lwd=lwd, lty=lty, ...)
            points(x, y, col=col, cex=cex, pch=pch, bg=pt.bg)
        } else if (type == 'b') {
            lines(x, y, col=col, lwd=lwd, lty=lty, ...)
            points(x, y, col=col, cex=cex, pch=pch, bg=pt.bg)
        } else if (type == 'n') {
            ; # skip it
        } else {
            lines(x, y, col=col, lwd=lwd, lty=lty)
        }
        oceDebug(debug, "} # plotJustProfile\n", style="bold", unindent=1)
    }                                  # plotJustProfile
    #if (!inherits(x, "ctd"))
    #    stop("method is only for objects of class '", "ctd", "'")
    xlimGiven <- !missing(xlim)
    ylimGiven <- !missing(ylim)
    densitylimGiven <- !missing(densitylim)
    dots <- list(...)
    ytypeChoices <- c("pressure", "z", "depth", "sigmaTheta")
    ytypeIndex <- pmatch(ytype, ytypeChoices)
    if (is.na(ytypeIndex))
        stop('ytype must be one of: "pressure", "z", "depth", "sigmaTheta", but it is "', ytype, '"')
    ytype <- ytypeChoices[ytypeIndex]
    if (!is.null(ylab)) {
        yname <- ylab
    } else {
        yname <- switch(ytype,
                        pressure=resizableLabel("p", "y", debug=debug-1),
                        z=resizableLabel("z", "y", debug=debug-1),
                        depth=resizableLabel("depth", "y", debug=debug-1),
                        sigmaTheta=resizableLabel("sigmaTheta", "y", debug=debug-1))
    }
    ## if plim given on a pressure plot, then it takes precedence over ylim;
    ## same for densitylim
    if (ytype == "pressure" && plimGiven)
        ylim <- plim
    if (ytype == "sigmaTheta" && densitylimGiven)
        ylim <- densitylim
    if (missing(ylim))
        ylim <- switch(ytype,
                       pressure=rev(range(x[["pressure"]], na.rm=TRUE)),
                       z=range(swZ(x[["pressure"]]), na.rm=TRUE),
                       depth=rev(range(x[["depth"]], na.rm=TRUE)),
                       sigmaTheta=rev(range(x[["sigmaTheta"]], na.rm=TRUE)))
    ## issue 1137 Dec 27, 2016
    ## Below, we used to trim the data to ylim, but this made it
    ## look as though there were no data at top and bottom of the plot.
    ## The new scheme is to retain 5% of data outside the limit, which
    ## should be OK for the usual R convention of a 4% gap at axis ends.
    if (ytype %in% c("pressure", "z", "depth", "sigmaTheta")) {
        yy <- x[[ytype]]
        extra <- 0.05 * diff(range(yy, na.rm=TRUE)) # note larger than 0.04, just in case
        if (is.na(extra)) examineIndices <- seq_along(yy)
        else examineIndices <- (min(ylim) - extra) <= yy & yy <= (max(ylim) + extra)
    } else {
        warning("unknown \"ytype\"; must be one of \"pressure\", \"z\", \"depth\" or \"sigmaTheta\"")
        examineIndices <- seq_along(x[["pressure"]]) # we know for sure this will work
    }
    ##1137 examineIndices <- switch(ytype,
    ##1137                    pressure=g[1]*min(ylim) <= x[["pressure"]] & x[["pressure"]] <= g[2]*max(ylim),
    ##1137                    z=g[1]*min(ylim) <= x[["z"]] & x[["z"]] <= g[2]*max(ylim),
    ##1137                    depth=g[1]*min(ylim) <= x[["depth"]] & x[["depth"]] <= g[2]*max(ylim),
    ##1137                    sigmaTheta=g[1]*min(ylim) <= x[["sigmaTheta"]] & x[["sigmaTheta"]] <= g[2]*max(ylim))
    if (0 == sum(examineIndices) && ytype == 'z' && ylim[1] >= 0 && ylim[2] >= 0) {
        warning("nothing is being plotted, because z is always negative and ylim specified a positive interval")
        return(invisible(NULL))
    }
    if (!is.list(x@data))
        x@data <- as.list(x@data)
    dataNames <- names(x@data)
    if (length(xtype) == length(x[["pressure"]]))
        xtype <- xtype[examineIndices]
    if (is.data.frame(x@data)) {
        x@data <- x@data[examineIndices, ]
    } else {
        for (dataName in dataNames) {
            x@data[[dataName]] <- x@data[[dataName]][examineIndices]
        }
    }
    axisNameLoc <- mgp[1]
    knowTimeUnit <- FALSE
    if ("time" %in% names(x@data)) {
        knowTimeUnit <- TRUE
        time <- x[["time"]]
    } else {
        time <- 0:(length(x[["pressure"]]) - 1)
        if (!is.null(x@metadata$sampleInterval) && !is.na(x@metadata$sampleInterval)) {
            knowTimeUnit <- TRUE
            time <- time * x@metadata$sampleInterval
        }
    }
    ## y <- if (ytype == "pressure") x[["pressure"]] else if (ytype == "z") x[["z"]]
    ##     else if (ytype == "depth") x[["depth"]] else if (ytype == "sigmaTheta") x[["sigmaTheta"]]
    if (ytype == "pressure") {
        y <- x[["pressure"]]
        if (plimGiven && !ylimGiven) {
            ylim <- plim
            ylimGiven <- TRUE
        }
    } else if (ytype == "z") {
        y <- x[["z"]]
    } else if (ytype == "depth") {
        y <- x[["depth"]]
    } else if (ytype == "sigmaTheta") {
        y <- x[["sigmaTheta"]]
    }
    y <- as.vector(y)

    if (!add)
        par(mar=mar, mgp=mgp)
    if (length(xtype) == length(y) && length(y) > 1) {
        oceDebug(debug, "case 1\n")
        if ('axes' %in% names(list(...))) {
            plot(xtype, y, xlab="", ylab=yname, type=type, xaxs=xaxs, yaxs=yaxs, ylim=ylim, col=col, lty=lty, cex=cex, pch=pch, ...)
            if (list(...)$axes) {
                axis(3)
                mtext(xlab, side=3, line=axisNameLoc, cex=par("cex"))
                axis(2)
            }
            box()
        } else {
            plot(xtype, y, xlab="", ylab=yname, type=type, axes=FALSE, xaxs=xaxs, yaxs=yaxs,
                 ylim=ylim, col=col, lty=lty, cex=cex, pch=pch, ...)
            axis(3)
            mtext(xlab, side=3, line=axisNameLoc, cex=par("cex"))
            axis(2)
            box()
        }
    } else if (is.numeric(xtype)) {
        oceDebug(debug, "xtype is numeric\n")
        if (length(xtype) != length(y))
            stop("length(xtype) must match number of levels in the CTD object")
        if (add) {
            lines(xtype, y, type=type, lty=lty, ...)
        } else {
            plot(xtype, y, xlab="", ylab=yname, type=type, axes=FALSE, xaxs=xaxs, yaxs=yaxs, ylim=ylim, lty=lty, cex=cex, pch=pch, ...)
            axis(3)
            mtext(xlab, side=3, line=axisNameLoc, cex=par("cex")) # no unit is provided
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
        index <- seq_along(x[["pressure"]])
        plot(index, x[["pressure"]], ylim=ylim, col=col, lty=lty, xlab="", ylab=yname,
             type=type, xaxs=xaxs, yaxs=yaxs, cex=cex, pch=pch, axes=FALSE)
        axis(3)
        mtext(if (is.null(xlab)) "index" else xlab, side=3, line=axisNameLoc, cex=par("cex")) # no unit is provided
        axis(2)
        box()
        if (grid) {
            at <- par("yaxp")
            abline(h=seq(at[1], at[2], length.out=at[3]+1), col=col.grid, lty=lty.grid)
        }
    } else if (xtype == "density+time") {
        if (add)
            warning("argument 'add' is ignored for xtype=\"density+time\"")
        sig0 <- if (eos == "unesco") swSigma0(x[["salinity"]], x[["temperature"]], x[["pressure"]]) else
            swSigma0(x[["salinity"]], x[["temperature"]], x[["pressure"]],
                     longitude=x[["longitude"]], latitude=x[["latitude"]], eos=eos)
        if (missing(densitylim))
            densitylim <- range(sig0, na.rm=TRUE)
        look <- if (keepNA) seq_along(y) else !is.na(sig0) & !is.na(y)
        look <- as.vector(look)
        plot(sig0[look], y[look], xlim=densitylim, ylim=ylim, cex=cex, pch=pch,
             type=type, col=col.rho, lty=lty, xlab="", ylab=yname, axes=FALSE, xaxs=xaxs, yaxs=yaxs, ...)
        axis(3, col=col.rho, col.axis=col.rho, col.lab=col.rho)
        mtext(resizableLabel(if (eos=="unesco") "sigmaTheta" else "sigma0"), side=3, line=axisNameLoc, col=col.rho, cex=par("cex"))
        axis(2)
        box()
        par(new=TRUE)                ## FIXME: this probably won't work if add=TRUE
        if (missing(timelim))
            timelim <- range(time, na.rm=TRUE)
        plot(time, y, xlim=timelim, ylim=ylim, type=type, xlab="", ylab=yname, axes=FALSE,
             lwd=lwd, col=col.time, xaxs=xaxs, yaxs=yaxs, lty=lty, cex=cex, pch=pch)
        axis(1, col=col.time, col.axis=col.time, col.lab=col.time)
        ## lines(time, y, lwd=lwd, col=col.time)
        if (knowTimeUnit) {
            if (getOption("oceUnitBracket") == '[') {
                mtext(expression(paste(Delta*t, " [s]")), side=1, line=axisNameLoc, cex=par("cex"), col=col.time)
            } else {
                mtext(expression(paste(Delta*t, " (s)")), side=1, line=axisNameLoc, cex=par("cex"), col=col.time)
            }
        } else {
            if (getOption("oceUnitBracket") == '[') {
                mtext(expression(paste(Delta*t, " [unknown unit]")), side=1, line=axisNameLoc, cex=par("cex"), col=col.time)
            } else {
                mtext(expression(paste(Delta*t, " (unknown unit)")), side=1, line=axisNameLoc, cex=par("cex"), col=col.time)
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
            densitylim <- range(x[["sigmaTheta"]], na.rm=TRUE)
        st <- swSigmaTheta(x)
        look <- if (keepNA) seq_along(y) else !is.na(st) & !is.na(y)
        look <- as.vector(look)
        plot(st[look], y[look],
             xlim=densitylim, ylim=ylim, col=col.rho, lty=lty, cex=cex, pch=pch,
             type=type, xlab="", ylab=yname, axes=FALSE, xaxs=xaxs, yaxs=yaxs, ...)
        axis(3, col=col.rho, col.axis=col.rho, col.lab=col.rho)
        if (getOption("oceUnitBracket") == '[') {
            mtext(expression(paste(sigma[theta], " [", kg/m^3, "] ")), side=3, line=axisNameLoc, col=col.rho, cex=par("cex"))
        } else {
            mtext(expression(paste(sigma[theta], " (", kg/m^3, ") ")), side=3, line=axisNameLoc, col=col.rho, cex=par("cex"))
        }
        axis(2)
        box()
        ## lines(st, y, col=col.rho, lwd=lwd)
        par(new=TRUE)
        dpdt <- diff(x[["pressure"]]) / diff(time)
        dpdt <- c(dpdt[1], dpdt)        # fake first point
        df <- min(max(x[["pressure"]], na.rm=TRUE) / 5, length(x[["pressure"]]) / 10) # FIXME: adjust params
        dpdt.sm <- smooth.spline(x[["pressure"]], dpdt, df=df)
        if (missing(dpdtlim))
            dpdtlim <- range(dpdt.sm$y)
        plot(dpdt.sm$y, dpdt.sm$x,
             xlim=dpdtlim, ylim=ylim, type=type, xlab="", ylab=yname, axes=FALSE, lwd=lwd, col=col.dpdt, cex=cex, pch=pch,
             xaxs=xaxs, yaxs=yaxs, lty=lty, ...)
        axis(1, col=col.dpdt, col.axis=col.dpdt, col.lab=col.dpdt)
        ## lines(dpdt.sm$y, dpdt.sm$x, lwd=lwd, col=col.dpdt)
        if (getOption("oceUnitBracket") == '[') {
            if (knowTimeUnit) {
                mtext(expression(dp/dt * " [dbar/s]"),
                      side=1, line=axisNameLoc, cex=par("cex"), col=col.dpdt)
            } else {
                mtext(expression(dp/dt * " [dbar/(time unit)]"),
                      side=1, line=axisNameLoc, cex=par("cex"), col=col.dpdt)
            }
        } else {
            if (knowTimeUnit) {
                mtext(expression(dp/dt * " (dbar/s)"),
                      side=1, line=axisNameLoc, cex=par("cex"), col=col.dpdt)
            } else {
                mtext(expression(dp/dt * " (dbar/(time unit))"),
                      side=1, line=axisNameLoc, cex=par("cex"), col=col.dpdt)
            }
        }
        box()
        if (grid) {
            at <- par("yaxp")
            abline(h=seq(at[1], at[2], length.out=at[3]+1), col=col.grid, lty=lty.grid)
            at <- par("xaxp")
            abline(v=seq(at[1], at[2], length.out=at[3]+1), col=col.grid, lty=lty.grid)
        }
    } else if (xtype == "S" || xtype == "SA" || xtype == "salinity") {
        oceDebug(debug, "recognized S, SA, or salinity\n")
        salinity <- if (eos == "gsw" || xtype == "SA") swAbsoluteSalinity(x) else x[["salinity"]]
        if (!any(is.finite(salinity))) {
            warning("no valid salinity data")
            return(invisible(NULL))
        }
        if (missing(Slim)) {
            if ("xlim" %in% names(dots)) Slim <- dots$xlim else Slim <- range(salinity, na.rm=TRUE)
        }
        if (useSmoothScatter) {
            smoothScatter(salinity, y, xlim=Slim, ylim=ylim, xlab="", ylab=yname, axes=FALSE, ...)
            axis(2)
            axis(3)
            box()
            if (eos == "gsw" || xtype == "SA") {
                mtext(if (is.null(xlab)) resizableLabel("absolute salinity", "x", unit=NULL, debug=debug-1) else xlab,
                      side=3, line=axisNameLoc, cex=par("cex"))
            } else {
                mtext(if (is.null(xlab)) resizableLabel("S", "x", unit=NULL, debug=debug-1) else xlab,
                      side=3, line=axisNameLoc, cex=par("cex"))
            }
        } else {
            look <- if (keepNA) seq_along(y) else !is.na(salinity) & !is.na(y)
            if (!add) {
                plot(salinity[look], y[look],
                     xlim=Slim, ylim=ylim, lty=lty, cex=cex, pch=pch,
                     type="n", xlab="", ylab=yname, axes=FALSE, xaxs=xaxs, yaxs=yaxs, ...)
                if (eos == "gsw" || xtype == "SA") {
                    mtext(if (is.null(xlab)) resizableLabel("absolute salinity", "x", unit=NULL, debug=debug-1) else xlab,
                          side=3, line=axisNameLoc, cex=par("cex"))
                } else {
                    mtext(if (is.null(xlab)) resizableLabel("S", "x", unit=NULL, debug=debug-1) else xlab,
                          side=3, line=axisNameLoc, cex=par("cex"))
                }
                mtext(yname, side=2, line=axisNameLoc, cex=par("cex"))
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
            plotJustProfile(salinity, y, type=type, col=col, lwd=lwd, lty=lty,
                            cex=cex, pch=pch, pt.bg=pt.bg,
                            keepNA=keepNA, debug=debug-1)
        }
    } else if (xtype == "C" || xtype == "conductivity") {
        if ('conductivity' %in% names(x@data)) {
            conductivity <- x[["conductivity"]]
        } else {
            conductivity <- swCSTp(x[['salinity']], x[['temperature']], x[['pressure']], eos=eos)
        }
        if (!any(is.finite(conductivity))) {
            warning("no valid conductivity data")
            return(invisible(NULL))
        }
        if (missing(Clim)) {
            if ("xlim" %in% names(dots)) Clim <- dots$xlim else Clim <- range(conductivity, na.rm=TRUE)
        }
        if (useSmoothScatter) {
            smoothScatter(conductivity, y, xlim=Clim, ylim=ylim, xlab="", ylab=yname, axes=FALSE,
                          cex=cex, pch=pch, col=col, ...)
            axis(2)
            axis(3)
            box()
            if (is.null(xlab)) {
                ## Look up conductivity unit (issue 731)
                unit <- x[["conductivityUnit"]]
                if (is.null(unit)) {
                    mtext(if (is.null(xlab)) resizableLabel("C", "x", debug=debug-1) else xlab,
                          side=3, line=axisNameLoc, cex=par("cex"))
                } else {
                    unitChar <- as.character(unit$unit)
                    if (0 == length(unitChar)) {
                        mtext(if (is.null(xlab)) resizableLabel("C", "x", debug=debug-1) else xlab,
                              side=3, line=axisNameLoc, cex=par("cex"))
                    } else if (unitChar == "ratio") {
                        mtext(if (is.null(xlab)) resizableLabel("C", "x", debug=debug-1) else xlab,
                              side=3, line=axisNameLoc, cex=par("cex"))
                    } else if (unitChar == "mS/cm") {
                        mtext(if (is.null(xlab)) resizableLabel("conductivity mS/cm", "x", debug=debug-1) else xlab,
                              side=3, line=axisNameLoc, cex=par("cex"))
                    } else if (unitChar == "S/m") {
                        mtext(if (is.null(xlab)) resizableLabel("conductivity S/m", "x", debug=debug-1) else xlab,
                              side=3, line=axisNameLoc, cex=par("cex"))
                    } else {
                        stop("unknown conductivity unit ", unit, "; should be 'ratio', 'mS/cm' or 'S/m'")
                    }
                }
            } else {
                mtext(xlab, side=3, line=axisNameLoc, cex=par("cex"))
            }
        } else {
            look <- if (keepNA) seq_along(y) else !is.na(conductivity) & !is.na(y)
            if (!add) {
                plot(conductivity[look], y[look],
                     xlim=Clim, ylim=ylim, lty=lty, cex=cex, pch=pch,
                     type="n", xlab="", ylab=yname, axes=FALSE, xaxs=xaxs, yaxs=yaxs, ...)
                ## Look up conductivity unit (issue 731)
                unit <- x[["conductivityUnit"]]
                if (is.null(unit)) {
                    mtext(if (is.null(xlab)) resizableLabel("C", "x", debug=debug-1) else xlab,
                          side=3, line=axisNameLoc, cex=par("cex"))
                } else {
                    unitChar <- as.character(unit$unit)
                    if (0 == length(unitChar)) {
                        mtext(if (is.null(xlab)) resizableLabel("C", "x", debug=debug-1) else xlab,
                              side=3, line=axisNameLoc, cex=par("cex"))
                    } else if (unitChar == "ratio") {
                        mtext(if (is.null(xlab)) resizableLabel("C", "x", debug=debug-1) else xlab,
                              side=3, line=axisNameLoc, cex=par("cex"))
                    } else if (unitChar == "mS/cm") {
                        mtext(if (is.null(xlab)) resizableLabel("conductivity mS/cm", "x", debug=debug-1) else xlab,
                              side=3, line=axisNameLoc, cex=par("cex"))
                    } else if (unitChar == "S/m") {
                        mtext(if (is.null(xlab)) resizableLabel("conductivity S/m", "x", debug=debug-1) else xlab,
                              side=3, line=axisNameLoc, cex=par("cex"))
                    } else {
                        stop("unknown conductivity unit ", unit[[1]], "; should be 'ratio', 'mS/cm' or 'S/m'")
                    }
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
                            "u", "v") || 0 < length(grep("^sigma[0-4]$", xtype))) {
        unit <- x@metadata$units[[xtype]][[1]]
        ##if (!(xtype %in% names(x@data)))
        ##    stop("no ", xtype, " in this station")
        xvar <- x[[xtype]]
        if (is.null(xvar))
            stop("no '", xtype, "' in this ctd object")
        if (all(is.na(xvar)))
            stop("all ", xtype, " values in this station are NA")
        if (useSmoothScatter) {
            oceDebug(debug, "scatter plot\n")
            smoothScatter(xvar, y, ylim=ylim, xlab="", ylab=resizableLabel("pressure", "y", debug=debug-1),
                          axes=FALSE, ...)
            axis(2)
            axis(3)
            box()
            ##mtext(resizableLabel(xtype, "x"), side=3, line=axisNameLoc, cex=par("cex"))
            unit <- x@metadata$units[[xtype]]
            mtext(if (is.null(xlab)) resizableLabel(xtype, "x", unit=unit, debug=debug-1) else xlab,
                  side=3, line=axisNameLoc, cex=par("cex"))
        } else {
            oceDebug(debug, "line plot\n")
            ##message("ctd.R:4811")
            #browser()
            look <- as.vector(if (keepNA) seq_along(y) else !is.na(xvar) & !is.na(y))
            if (!add) {
                oceDebug(debug, "add is FALSE so new plot\n")
                if (ylimGiven) {
                    oceDebug(debug, "ylimGiven is TRUE\n")
                    ylimsorted <- sort(ylim)
                    ## message("length(ylimsorted) ", length(ylimsorted))
                    ## message("ylimsorted vector? ", is.vector(ylimsorted))
                    ## message("length(look) ", length(look))
                    ## message("look vector? ", is.vector(look))
                    ## message("length(xvar) ", length(xvar))
                    ## message("xvar vector? ", is.vector(xvar))
                    ## message("length(y) ", length(y))
                    ## message("y vector? ", is.vector(y))
                    look <- look & (ylimsorted[1] <= y) & (y <= ylimsorted[2])
                    if (!xlimGiven)
                        xlim <- range(xvar[look], na.rm=TRUE)
                    plot(xvar[look], y[look], xlim=xlim, ylim=ylim,
                         lty=lty, type="n", xlab="", ylab=yname, axes=FALSE, xaxs=xaxs, yaxs=yaxs, ...)
                } else {
                    oceDebug(debug, "ylimGiven is FALSE\n")
                    ##message("B")
                    if (!xlimGiven)
                        xlim <- range(xvar[look], na.rm=TRUE)
                    plot(xvar[look], y[look],
                         xlab=if (is.null(xlab)) " " else xlab,
                         xlim=xlim, ylim=rev(range(y[look], na.rm=TRUE)),
                         lty=lty, type="n", ylab=yname, axes=FALSE, xaxs=xaxs, yaxs=yaxs, ...)
                }
                mtext(if (is.null(xlab)) resizableLabel(xtype, "x", unit=unit, debug=debug-1) else xlab,
                      side=3, line=axisNameLoc, cex=par("cex"))
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
            plotJustProfile(xvar[look], y[look], type=type, lwd=lwd, lty=lty,
                            cex=cex, col=col, pch=pch, pt.bg=pt.bg,
                            keepNA=keepNA, debug=debug-1)
        }
    } else if (xtype == "Rrho" || xtype == "RrhoSF") {
        Rrho <- swRrho(x, sense=if (xtype=="Rrho") "diffusive" else "finger")
        look <- if (keepNA) seq_along(y) else !is.na(Rrho) & !is.na(y)
        if (!add) {
            if (ylimGiven) {
                plot(Rrho, y[look], lty=lty,
                     xlim=if (!missing(Rrholim)) Rrholim, ylim=ylim, cex=cex, pch=pch,
                     type="n", xlab="", ylab=yname, axes=FALSE, xaxs=xaxs, yaxs=yaxs, ...)
            } else {
                plot(Rrho, y[look], lty=lty,
                     xlim=if (!missing(Rrholim)) Rrholim, ylim=rev(range(y[look])), cex=cex, pch=pch,
                     type="n", xlab="", ylab=yname, axes=FALSE, xaxs=xaxs, yaxs=yaxs, ...)
            }
            mtext(if (is.null(xlab)) expression(R[rho]) else xlab, side=3, line=axisNameLoc, cex=par("cex"))
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
    } else if (xtype == "T" || xtype == "CT" || xtype == "temperature") {
        oceDebug(debug, "recognized T, CT, or temperature\n")
        temperature <- if (eos == "gsw" || xtype == "CT") swConservativeTemperature(x) else x[["temperature"]]
        unit <- x@metadata$units[["temperature"]]
        if (!any(is.finite(temperature))) {
            warning("no valid temperature data")
            return(invisible(NULL))
        }
        if (missing(Tlim)) {
            if ("xlim" %in% names(dots)) Tlim <- dots$xlim else Tlim <- range(temperature, na.rm=TRUE)
        }
        if (useSmoothScatter) {
            smoothScatter(temperature, y, xlim=Tlim, ylim=ylim, xlab="", ylab=yname, axes=FALSE, ...)
            axis(2)
            axis(3)
            box()
            if (eos == "gsw" || xtype == "CT") {
                mtext(if (is.null(xlab)) resizableLabel("conservative temperature", "x", unit=unit, debug=debug-1) else xlab,
                      side=3, line=axisNameLoc, cex=par("cex"))
            } else {
                mtext(if (is.null(xlab)) resizableLabel("T", "x", unit=unit, debug=debug-1) else xlab,
                      side=3, line=axisNameLoc, cex=par("cex"))
            }
        } else {
            look <- if (keepNA) seq_along(y) else !is.na(x[["temperature"]]) & !is.na(y)
            if (!add) {
                plot(temperature[look], y[look], lty=lty,
                     xlim=Tlim, ylim=ylim, cex=cex, pch=pch,
                     type="n", xlab="", ylab="", axes=FALSE, xaxs=xaxs, yaxs=yaxs, ...)
                if (eos == "gsw") {
                    mtext(if (is.null(xlab)) resizableLabel("conservative temperature", "x", unit=unit, debug=debug-1) else xlab,
                          side=3, line=axisNameLoc, cex=par("cex"))
                } else {
                    mtext(if (is.null(xlab)) resizableLabel("T", "x", unit=unit, debug=debug-1) else xlab,
                          side=3, line=axisNameLoc, cex=par("cex"))
                }
                mtext(yname, side=2, line=axisNameLoc, cex=par("cex"))
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
        theta <- if ("theta" %in% names(x@data)) x@data$theta else swTheta(x)
        if (missing(Tlim)) {
            if ("xlim" %in% names(dots)) Tlim <- dots$xlim else Tlim <- range(theta, na.rm=TRUE)
        }
        if (useSmoothScatter) {
            smoothScatter(theta, y, xlim=Tlim, ylim=ylim, xlab="", ylab=yname, axes=FALSE, ...)
            axis(2)
            axis(3)
            box()
            if (eos == "gsw")
                mtext(if (is.null(xlab)) resizableLabel("conservative temperature", "x", debug=debug-1) else xlab,
                      side=3, line=axisNameLoc, cex=par("cex"))
            else
                mtext(if (is.null(xlab)) resizableLabel(theta, "x", debug=debug-1) else xlab,
                      side=3, line=axisNameLoc, cex=par("cex"))
        } else {
            look <- if (keepNA) seq_along(y) else !is.na(theta) & !is.na(y)
            if (!add) {
                plot(theta[look], y[look], lty=lty,
                     xlim=Tlim, ylim=ylim, cex=cex, pch=pch,
                     type="n", xlab="", ylab="", axes=FALSE, xaxs=xaxs, yaxs=yaxs, ...)
                if (eos == "gsw") {
                    mtext(if (is.null(xlab)) resizableLabel("conservative temperature", "x", debug=debug-1) else xlab,
                          side=3, line=axisNameLoc, cex=par("cex"))
                } else {
                    mtext(if (is.null(xlab)) resizableLabel("theta", "x", debug=debug-1) else xlab,
                          side=3, line=axisNameLoc, cex=par("cex"))
                }
                mtext(yname, side=2, line=axisNameLoc, cex=par("cex"))
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
        ## FIXME: do as theta above
        st <- swSigmaTheta(x)
        look <- if (keepNA) seq_along(y) else !is.na(st) & !is.na(y)
        ## FIXME: if this works, extend to other x types
        look <- look & (min(ylim) <= y & y <= max(ylim))
        if (!add) {
            if (densitylimGiven) {
                plot(st[look], y[look], xlim=densitylim, ylim=ylim, type="n", xlab="", ylab=yname,
                     axes=FALSE, xaxs=xaxs, yaxs=yaxs, lty=lty, cex=cex, pch=pch, ...)
            } else {
                plot(st[look], y[look], xlim=range(st[look], na.rm=TRUE), ylim=ylim, type="n", xlab="", ylab=yname,
                     axes=FALSE, xaxs=xaxs, yaxs=yaxs, lty=lty, cex=cex, pch=pch, ...)
            }
            if (getOption("oceUnitBracket") == '[') {
                mtext(if (is.null(xlab)) expression(paste(sigma[theta], " [", kg/m^3, "]")) else xlab,
                      side=3, line=axisNameLoc, cex=par("cex"))
            } else {
                mtext(if (is.null(xlab)) expression(paste(sigma[theta], " (", kg/m^3, ")")) else xlab,
                      side=3, line=axisNameLoc, cex=par("cex"))
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
        plotJustProfile(st, y, col=col, type=type, lwd=lwd, lty=lty,
                        cex=cex, pch=pch, pt.bg=pt.bg,
                        keepNA=keepNA, debug=debug-1)
    } else if (xtype == "density") {
        rho <- swRho(x)
        look <- if (keepNA) seq_along(y) else !is.na(rho) & !is.na(y)
        ## FIXME: if this works, extend to other x types
        look <- look & (min(ylim) <= y & y <= max(ylim))
        if (!add) {
            if (densitylimGiven) {
                plot(rho[look], y[look], xlim=densitylim, ylim=ylim, type="n", xlab="", ylab=yname,
                     axes=FALSE, xaxs=xaxs, yaxs=yaxs, lty=lty, cex=cex, pch=pch, ...)
            } else {
                plot(rho[look], y[look], xlim=range(rho[look], na.rm=TRUE), ylim=ylim, type="n", xlab="", ylab=yname,
                     axes=FALSE, xaxs=xaxs, yaxs=yaxs, lty=lty, cex=cex, pch=pch, ...)
            }
            if (getOption("oceUnitBracket") == '[') {
                mtext(if (is.null(xlab)) expression(paste(rho, " [", kg/m^3, "]")) else xlab,
                      side=3, line=axisNameLoc, cex=par("cex"))
            } else {
                mtext(if (is.null(xlab)) expression(paste(rho, " (", kg/m^3, ")")) else xlab,
                      side=3, line=axisNameLoc, cex=par("cex"))
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
        plotJustProfile(rho, y, col=col, type=type, lwd=lwd, lty=lty,
                        cex=cex, pch=pch, pt.bg=pt.bg,
                        keepNA=keepNA, debug=debug-1)
    } else if (xtype == "density+N2") {
        if (add)
            warning("argument 'add' is ignored for xtype=\"density+dpdt\"")
        sig0 <- swSigma0(x)
        if (!any(is.finite(sig0))) {
            warning("no valid sigma-0 data")
            return(invisible(NULL))
        }
        look <- if (keepNA) seq_along(y) else !is.na(sig0) & !is.na(y)
        if (missing(densitylim))
            densitylim <- range(sig0, na.rm=TRUE)
        plot(sig0[look], y[look], lty=lty,
             xlim=densitylim, ylim=ylim, cex=cex, pch=pch,
             type="n", xlab="", ylab=yname, axes=FALSE, xaxs=xaxs, yaxs=yaxs, ...)
        axis(3, col=col.rho, col.axis=col.rho, col.lab=col.rho)
        tmpsep <- getOption("oceUnitSep")
        sep <- if (!is.null(tmpsep)) tmpsep else ""
        if (getOption("oceUnitBracket") == '[') {
            label <- if (eos == "unesco") bquote(sigma[theta]*" ["*.(sep)*kg/m^3*.(sep)*"]") else
                bquote(sigma[0]*" ["*.(sep)*kg/m^3*.(sep)*"]")
        } else {
            label <- if (eos == "unesco") bquote(sigma[theta]*" ("*.(sep)*kg/m^3*.(sep)*")") else
                bquote(sigma[0]*" ("*.(sep)*kg/m^3*.(sep)*")")
        }
        mtext(resizableLabel(if (eos == "unesco") "sigmaTheta" else "sigma0"),
              side=3, line=axisNameLoc, col=col.rho, cex=par("cex"))
        axis(2)
        box()
        if (type == 'l') {
            lines(sig0, y, col=col.rho, lwd=lwd, lty=lty)
        } else if (type == 'p') {
            points(sig0, y, col=col.rho, pch=pch, cex=cex)
        } else {
            points(sig0, y, col=col.rho, pch=pch, cex=cex)
            lines(sig0, y, col=col.rho, lwd=lwd, lty=lty)
        }
        par(new=TRUE)
        N2 <- swN2(x, df=df)
        N2[!is.finite(N2)] <- NA
        if (missing(N2lim))
            N2lim <- range(N2, na.rm=TRUE)
        look <- if (keepNA) seq_along(y) else !is.na(N2) & !is.na(y)
        if (0 == sum(look)) {
            warning("no valid N2 data")
            return(invisible(NULL))
        }
        plot(N2[look], y[look], lty=lty,
             xlim=N2lim, ylim=ylim, cex=cex, pch=pch,
             type="n", xlab="", ylab="", axes=FALSE, lwd=lwd, xaxs=xaxs, yaxs=yaxs)
        axis(1, col=col.N2, col.axis=col.N2, col.lab=col.N2)

        if (type == 'l') {
            lines(N2, y, col=col.N2, lwd=lwd, lty=lty)
        } else if (type == 'p') {
            points(N2, y, col=col.N2, pch=pch, cex=cex)
        } else {
            points(N2, y, col=col.N2, pch=pch, cex=cex)
            lines(N2, y, col=col.N2, lwd=lwd, lty=lty)
        }
        mtext(resizableLabel("N2"), side=1, line=axisNameLoc, col=col.N2, cex=par("cex"))
        box()
        if (grid) {
            at <- par("yaxp")
            abline(h=seq(at[1], at[2], length.out=at[3]+1), col=col.grid, lty=lty.grid)
        }
    } else if (xtype == "N2") {
        N2 <- swN2(x, df=df)
        if (missing(N2lim))
            N2lim <- range(N2, na.rm=TRUE)
        look <- if (keepNA) seq_along(y) else !is.na(N2) & !is.na(y)
        if (!add) {
            plot(N2[look], y[look], lty=lty,
                 xlim=N2lim, ylim=ylim, cex=cex, pch=pch,
                 type="n", xlab="", ylab=yname, axes=FALSE)
            mtext(if (is.null(xlab)) resizableLabel("N2") else xlab,
                  side=3, line=axisNameLoc, cex=par("cex"))
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
        look <- if (keepNA) seq_along(y) else !is.na(spice) & !is.na(y)
        if (!add) {
            plot(spice[look], y[look], lty=lty,
                 ylim=ylim, cex=cex, pch=pch,
                 type="n", xlab="", ylab=yname, axes=FALSE)
            mtext(if (is.null(xlab)) resizableLabel("spice", "x", debug=debug-1) else xlab,
                  side=3, line=axisNameLoc, cex=par("cex"), xaxs=xaxs, yaxs=yaxs)
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
        salinity <- if (eos == "gsw") swAbsoluteSalinity(x) else x[["salinity"]]
        temperature <- if (eos == "gsw") swConservativeTemperature(x) else x[["temperature"]]
        if (!any(is.finite(salinity))) {
            warning("no valid salinity data")
            return(invisible(NULL))
        }
        if (!any(is.finite(temperature))) {
            warning("no valid temperature data")
            return(invisible(NULL))
        }
        if (missing(Slim)) Slim <- range(salinity, na.rm=TRUE)
        if (missing(Tlim)) Tlim <- range(temperature, na.rm=TRUE)
        look <- if (keepNA) seq_along(y) else !is.na(temperature) & !is.na(y)
        plot(temperature[look], y[look],
             xlim=Tlim, ylim=ylim, col=col.temperature, lty=lty, cex=cex, pch=pch,
             type=type, xlab="", ylab=yname, axes=FALSE, xaxs=xaxs, yaxs=yaxs)
        axis(3, col=col.temperature, col.axis=col.temperature, col.lab=col.temperature)
        if (is.null(getOption('plotProfileNoXLab'))) {
            if (eos == "gsw")
                mtext(resizableLabel("conservative temperature", "x", debug=debug-1),
                      side=3, line=axisNameLoc, col=col.temperature, cex=par("cex"))
            else
                mtext(resizableLabel("T", "x", debug=debug-1),
                      side=3, line=axisNameLoc, col=col.temperature, cex=par("cex"))
        }
        axis(2)
        box()
        ## lines(temperature, y, col=col.temperature, lwd=lwd)
        par(new=TRUE)
        look <- if (keepNA) seq_along(y) else !is.na(x[["salinity"]]) & !is.na(y)
        plot(salinity[look], y[look],
             xlim=Slim, ylim=ylim, col=col.salinity, lty=lty, cex=cex, pch=pch,
             type=type, xlab="", ylab="", axes=FALSE, xaxs=xaxs, yaxs=yaxs)
        axis(1, col=col.salinity, col.axis=col.salinity, col.lab=col.salinity)
        if (is.null(getOption('plotProfileNoXLab'))) {
            if (eos == "gsw")
                mtext(resizableLabel("absolute salinity", "x", debug=debug-1),
                      side=1, line=axisNameLoc, col=col.salinity, cex=par("cex"))
            else
                mtext(resizableLabel("S", "x", debug=debug-1),
                      side=1, line=axisNameLoc, col=col.salinity, cex=par("cex"))
        }
        box()
        if (grid) {
            at <- par("yaxp")
            abline(h=seq(at[1], at[2], length.out=at[3]+1), col=col.grid, lty=lty.grid)
        }
        ## lines(salinity, y, col=col.salinity, lwd=if (length(lwd)>1)lwd[2] else lwd[1])
    } else {
        ## Not a special case.
        oceDebug(debug, "plotting a general xtype, i.e. not a special case\n")
        w <- which(names(x@data) == xtype)
        if (length(w) < 1)
            stop("unknown xtype value (\"", xtype, "\")")
        # Try to compute a top-axis label with units, unless 'xlab' was given.
        if (is.null(xlab)) {
            label <- if (xtype %in% names(x@metadata$units)) {
                #. tmp <- getOption("oceUnitSep")
                #. sep <- if (!is.null(tmp)) tmp else ""
                #. if (getOption("oceUnitBracket") == "[") {
                #.     L <- paste(" [", sep, sep="")
                #.     R <- paste(sep, " ]", sep="")
                #. } else {
                #.     L <- paste(" (", sep, sep="")
                #.     R <- paste(sep, " )", sep="")
                #. }
                label <- resizableLabel(as.character(xtype), "x", unit=x@metadata$units[[xtype]]$unit)
            } else {
                as.character(xtype)
            }
        }
        look <- if (keepNA) seq_along(y) else !is.na(x@data[[xtype]]) & !is.na(y)
        dots <- list(...)
        ## message("names(dots)=", paste(names(dots), collapse=" "))
        if (!add) {
            par(mar=mar, mgp=mgp)
            xplot <- x@data[[xtype]][look]
            plot(xplot, y[look],
                 xlim=if (xlimGiven) xlim else range(xplot, na.rm=TRUE),
                 ylim=ylim, lty=lty, cex=cex, pch=pch,
                 type="n", xlab="", ylab="", axes=FALSE, xaxs=xaxs, yaxs=yaxs)
            axis(3)
            #mtext(resizableLabel("pressure", "y"), side=2, line=axisNameLoc, cex=par("cex"))
            mtext(yname, side=2, line=axisNameLoc, cex=par("cex"))
            ## label <- if (w <= length(x@metadata$labels)) x@metadata$labels[w] else
            ##     as.character(xtype)
            if (is.character(label) && label == "sigmaTheta")
                label <- resizableLabel("sigmaTheta", "x", debug=debug-1)
            ##issue1684/2020-04-20 label <- resizableLabel(label, "x", unit=x@metadata$units[[xtype]], debug=debug-1)
            ##issue1684/2020-04-20 oceDebug(debug, "x name computed as \"", paste0(as.character(label)), "\"\n", sep="")
            mtext(if (is.null(xlab)) label else xlab,
                  side=3, line=axisNameLoc, cex=par("cex"))
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
        # issue1791 } else {
        # issue1791     points(x@data[[w]], y, lwd=lwd, pch=pch, col=col, lty=lty, cex=cex)
        }
        if (grid) {
            at <- par("xaxp")
            abline(v=seq(at[1], at[2], length.out=at[3]+1), col=col.grid, lty=lty.grid)
            at <- par("yaxp")
            abline(h=seq(at[1], at[2], length.out=at[3]+1), col=col.grid, lty=lty.grid)
        }
    }
    oceDebug(debug, "} # plotProfile()\n", style="bold", unindent=1)
}
