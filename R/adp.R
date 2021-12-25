# vim: tw=80 shiftwidth=4 softtabstop=4 expandtab:

#' Class to Store adp (ADCP) Data
#'
#' This class stores data from acoustic Doppler profilers. Some manufacturers
#' call these ADCPs, while others call them ADPs; here the shorter form is
#' used by analogy to ADVs.
#'
#' @templateVar class adp
#'
#' @templateVar dataExample The key items stored in this slot include `time`, `distance`, and `v`, along with angles `heading`, `pitch` and `roll`.
#'
#' @templateVar metadataExample Examples that are of common interest include `oceCoordinate`, `orientation`, `frequency`, and `beamAngle`.
#'
#' @template slot_summary
#'
#' @template slot_put
#'
#' @template slot_get
#'
#' @section Reading/creating `adp` objects:
#'
#' The `metadata` slot contains various
#' items relating to the dataset, including source file name, sampling rate,
#' velocity resolution, velocity maximum value, and so on.  Some of these are
#' particular to particular instrument types, and prudent researchers will take
#' a moment to examine the whole contents of the metadata, either in summary
#' form (with `str(adp[["metadata"]])`) or in detail (with
#' `adp[["metadata"]]`).  Perhaps the most useful general properties are
#' `adp[["bin1Distance"]]` (the distance, in metres, from the sensor to
#' the bottom of the first bin), `adp[["cellSize"]]` (the cell height, in
#' metres, in the vertical direction, *not* along the beam), and
#' `adp[["beamAngle"]]` (the angle, in degrees, between beams and an
#' imaginary centre line that bisects all beam pairs).
#'
#' The diagram provided below indicates the coordinate-axis and beam-numbering
#' conventions for three- and four-beam ADP devices, viewed as though the
#' reader were looking towards the beams being emitted from the transducers.
#'
#' \if{html}{\figure{adp_beams.png}{options: width=400px alt="Figure: adp_beams.png"}}
#'
#' The bin geometry of a four-beam profiler is illustrated below, for
#' `adp[["beamAngle"]]` equal to 20 degrees, `adp[["bin1Distance"]]`
#' equal to 2m, and `adp[["cellSize"]]` equal to 1m.   In the diagram, the
#' viewer is in the plane containing two beams that are not shown, so the two
#' visible beams are separated by 40 degrees.  Circles indicate the centres of
#' the range-gated bins within the beams.  The lines enclosing those circles
#' indicate the coverage of beams that spread plus and minus 2.5 degrees from
#' their centreline.
#'
#' \if{html}{\figure{adpgeometry2.png}{options: width=400px alt="Figure: adpgeometry2.png"}}
#'
#' Note that `adp[["oceCoordinate"]]` stores the present coordinate system
#' of the object, and it has possible values `"beam"`, `"xyz"`, `"sfm"` or
#' `"enu"`.  (This should not be confused with
#' `adp[["originalCoordinate"]]`, which stores the coordinate system used
#' in the original data file.)
#'
#' The `data` slot holds some standardized items, and
#' many that vary from instrument to instrument.  One standard item is
#' `adp[["v"]]`, a three-dimensional numeric array of velocities in
#' m/s.  In this matrix, the first index indicates time, the second bin
#' number, and the third beam number. The meaning of beams number depends on
#' whether the object is in beam coordinates, frame coordinates, or earth
#' coordinates.  For example, if in earth coordinates, then beam 1 is
#' the eastward component of velocity.
#' Thus, for example,
#' \preformatted{
#' library(oce)
#' data(adp)
#' t <- adp[['time']]
#' d <- adp[['distance']]
#' eastward <- adp[['v']][,,1]
#' imagep(t, d, eastward, missingColor="gray")
#' }
#' plots an image of the eastward component of velocity as a function of time (the x axis)
#' and distance from sensor (y axis), since the `adp` dataset is
#' in earth coordinates. Note the semidurnal tidal signal, and the pattern of missing
#' data at the ocean surface (gray blotches at the top).
#'
#' Corresponding to the velocity array are two arrays of type raw, and
#' identical dimension, accessed by `adp[["a"]]` and `adp[["q"]]`,
#' holding measures of signal strength and data quality quality,
#' respectively.  (The exact meanings of these depend on the particular type
#' of instrument, and it is assumed that users will be familiar enough with
#' instruments to know both the meanings and their practical consequences in
#' terms of data-quality assessment, etc.)
#'
#' In addition to the arrays, there are time-based vectors.  The vector
#' `adp[["time"]]` (of length equal to the first index of
#' `adp[["v"]]`, etc.) holds times of observation.  Depending on type of
#' instrument and its configuration, there may also be corresponding vectors
#' for sound speed (`adp[["soundSpeed"]]`), pressure
#' (`adp[["pressure"]]`), temperature (`adp[["temperature"]]`),
#' heading (`adp[["heading"]]`) pitch (`adp[["pitch"]]`), and roll
#' (`adp[["roll"]]`), depending on the setup of the instrument.
#'
#' The precise meanings of the data items depend on the instrument type.  All
#' instruments have `v` (for velocity), `q` (for a measure of data
#' quality) and `a` (for a measure of backscatter amplitude, also called
#' echo intensity).
#' Teledyne-RDI profilers have an additional item `g` (for
#' percent-good).
#'
#' VmDas-equipped Teledyne-RDI profilers additional navigation data, with
#' details listed in the table below; note that the RDI documentation (reference 2) and
#' the RDI gui use inconsistent names for most items.
#'
#' \tabular{lll}{
#'   **Oce name**\tab **RDI doc name**\tab **RDI GUI name**\cr
#'   `avgSpeed`\tab Avg Speed\tab Speed/Avg/Mag\cr
#'   `avgMagnitudeVelocityEast`\tab Avg Mag Vel East\tab ?\cr
#'   `avgMagnitudeVelocityNorth`\tab Avg Mag Vel North\tab ?\cr
#'   `avgTrackMagnetic`\tab Avg Track Magnetic\tab Speed/Avg/Dir (?)\cr
#'   `avgTrackTrue`\tab Avg Track True\tab Speed/Avg/Dir (?)\cr
#'   `avgTrueVelocityEast`\tab Avg True Vel East\tab ?\cr
#'   `avgTrueVelocityNorth`\tab Avg True Vel North\tab ?\cr
#'   `directionMadeGood`\tab Direction Made Good\tab Speed/Made Good/Dir\cr
#'   `firstLatitude`\tab First latitude\tab Start Lat\cr
#'   `firstLongitude`\tab First longitude\tab Start Lon\cr
#'   `firstTime`\tab UTC Time of last fix\tab End Time\cr
#'   `lastLatitude`\tab Last latitude\tab End Lat\cr
#'   `lastLongitude`\tab Last longitude\tab End Lon\cr
#'   `lastTime`\tab UTC Time of last fix\tab End Time\cr
#'   `numberOfHeadingSamplesAveraged`\tab Number heading samples averaged\tab ?\cr
#'   `numberOfMagneticTrackSamplesAveraged`\tab Number of magnetic track samples averaged\tab ? \cr
#'   `numberOfPitchRollSamplesAvg`\tab Number of magnetic track samples averaged\tab ? \cr
#'   `numberOfSpeedSamplesAveraged`\tab Number of speed samples averaged\tab ? \cr
#'   `numberOfTrueTrackSamplesAvg`\tab Number of true track samples averaged\tab ? \cr
#'   `primaryFlags`\tab Primary Flags\tab ?\cr
#'   `shipHeading`\tab Heading\tab ?\cr
#'   `shipPitch`\tab Pitch\tab ?\cr
#'   `shipRoll`\tab Roll\tab ?\cr
#'   `speedMadeGood`\tab Speed Made Good\tab Speed/Made Good/Mag\cr
#'   `speedMadeGoodEast`\tab Speed MG East\tab ?\cr
#'   `speedMadeGoodNorth`\tab Speed MG North\tab ?\cr
#' }
#'
#' For Teledyne-RDI profilers, there are four three-dimensional arrays
#' holding beamwise data.  In these, the first index indicates time, the
#' second bin number, and the third beam number (or coordinate number, for
#' data in `xyz`, `sfm`, `enu` or `other` coordinate systems).  In
#' the list below, the quoted phrases are quantities as defined in Figure 9
#' of reference 1.
#'
#' * `v` is ``velocity'' in m/s, inferred from two-byte signed
#'   integer values (multiplied by the scale factor that is stored in
#'   `velocityScale` in the metadata).
#'
#' * `q` is ``correlation magnitude'' a one-byte quantity stored
#'   as type `raw` in the object. The values may range from 0 to 255.
#'
#' * `a` is ``backscatter amplitude``, also known as ``echo
#'   intensity'' a one-byte quantity stored as type `raw` in the object.
#'   The values may range from 0 to 255.
#'
#' * `g` is ``percent good'' a one-byte quantity stored as `raw`
#'   in the object.  The values may range from 0 to 100.
#'
#' Finally, there is a vector `adp[["distance"]]` that indicates the bin
#' distances from the sensor, measured in metres along an imaginary centre
#' line bisecting beam pairs.  The length of this vector equals
#' `dim(adp[["v"]])[2]`.
#'
#' @section Teledyne-RDI Sentinel V ADCPs: As of 2016-09-27 there is
#'     provisional support for the TRDI "SentinelV" ADCPs, which are 5
#'     beam ADCPs with a vertical centre beam. Relevant vertical beam
#'     fields are called `adp[["vv"]]`, `adp[["va"]]`,
#'     `adp[["vq"]]`, and `adp[["vg"]]` in analogy with the
#'     standard 4-beam fields.
#'
#' @section Accessing and altering information within [adp-class] objects:
#' *Extracting values* Matrix data may be accessed as illustrated
#' above, e.g.  or an adp object named `adv`, the data are provided by
#' `adp[["v"]]`, `adp[["a"]]`, and `adp[["q"]]`.  As a
#' convenience, the last two of these can be accessed as numeric (as opposed to
#' raw) values by e.g.  `adp[["a", "numeric"]]`.  The vectors are accessed
#' in a similar way, e.g. `adp[["heading"]]`, etc.  Quantities in the
#' `metadata` slot are also available by name, e.g.
#' `adp[["velocityResolution"]]`, etc.
#'
#' *Assigning values.* This follows the standard form, e.g. to increase
#' all velocity data by 1 cm/s, use `adp[["v"]] <- 0.01 + adp[["v"]]`.
#'
#' *Overview of contents* The `show` method (e.g.
#' `show(d)`) displays information about an ADP object named `d`.
#'
#' @section Dealing with suspect data:
#' There are many possibilities for confusion
#' with `adp` devices, owing partly to the flexibility that manufacturers
#' provide in the setup.  Prudent users will undertake many tests before trusting
#' the details of the data.  Are mean currents in the expected direction, and of
#' the expected magnitude, based on other observations or physical constraints?
#' Is the phasing of currents as expected?  If the signals are suspect, could an
#' incorrect scale account for it?  Could the transformation matrix be incorrect?
#' Might the data have exceeded the maximum value, and then ``wrapped around'' to
#' smaller values?  Time spent on building confidence in data quality is seldom
#' time wasted.
#'
#' @section References:
#' 1. Teledyne-RDI, 2007.
#' *WorkHorse commands and output data format.*
#' P/N 957-6156-00 (November 2007).
#'
#' 2. Teledyne-RDI, 2012. *VmDas User's Guide, Ver. 1.46.5*.
#'
#' @seealso
#' A file containing ADP data is usually recognized by Oce, and so
#' [read.oce()] will usually read the data.  If not, one may use the
#' general ADP function [read.adp()] or specialized variants
#' [read.adp.rdi()], [read.adp.nortek()],
#' [read.adp.ad2cp()],
#' [read.adp.sontek()] or [read.adp.sontek.serial()].
#'
#' ADP data may be plotted with [plot,adp-method()], which is a
#' generic function so it may be called simply as `plot`.
#'
#' Statistical summaries of ADP data are provided by the generic function
#' `summary`, while briefer overviews are provided with `show`.
#'
#' Conversion from beam to xyz coordinates may be done with
#' [beamToXyzAdp()], and from xyz to enu (east north up) may be done
#' with [xyzToEnuAdp()].  [toEnuAdp()] may be used to
#' transfer either beam or xyz to enu.  Enu may be converted to other coordinates
#' (e.g. aligned with a coastline) with [enuToOtherAdp()].
#'
#' @family classes provided by oce
#' @family things related to adp data
setClass("adp", contains="oce")

#' Sample adp (acoustic-doppler profiler) dataset
#'
#' This is degraded subsample of measurements that were made with an
#' upward-pointing, moored, ADP manufactured by Teledyne-RDI, as part of the St Lawrence
#' Internal Wave Experiment (SLEIWEX).
#'
#' @name adp
#'
#' @docType data
#'
#' @usage data(adp)
#'
#' @examples
#'\donttest{
#' library(oce)
#' data(adp)
#'
#' # Velocity components.  (Note: we should probably trim some bins at top.)
#' plot(adp)
#'
#' # Note that tides have moved the mooring.
#' plot(adp, which=15:18)
#'}
#'
#'
#' @source This file came from the SLEIWEX-2008 experiment.
#'
#' @family datasets provided with oce
#' @family things related to adp data
NULL

setMethod(f="initialize",
          signature="adp",
          definition=function(.Object, time, distance, v, a, q, oceCoordinate="enu", orientation="upward", ...) {
              .Object <- callNextMethod(.Object, ...)
              if (!missing(time)) .Object@data$time <- time
              if (!missing(distance)) {
                  .Object@data$distance <- distance
                  .Object@metadata$cellSize <- tail(diff(distance), 1) # first one has blanking, perhaps
              }
              if (!missing(v)) {
                  .Object@data$v <- v
                  .Object@metadata$numberOfBeams <- dim(v)[3]
                  .Object@metadata$numberOfCells <- dim(v)[2]
              }
              if (!missing(a)) .Object@data$a <- a
              if (!missing(q)) .Object@data$q <- q
              .Object@metadata$units$v <- list(unit=expression(m/s), scale="")
              .Object@metadata$units$distance <- list(unit=expression(m), scale="")
              .Object@metadata$oceCoordinate <- oceCoordinate # FIXME: should check that it is allowed
              .Object@metadata$orientation  <- orientation # FIXME: should check that it is allowed
              .Object@processingLog$time <- presentTime()
              .Object@processingLog$value <- "create 'adp' object"
              return(.Object)
          })


## DEVELOPERS: please pattern functions and documentation on this, for uniformity.
## DEVELOPERS: You will need to change the docs, and the 3 spots in the code
## DEVELOPERS: marked '# DEVELOPER 1:', etc.
#' @title Handle Flags in adp Objects
#'
#' @details
#' If `flags` and `actions` are not provided, the
#' default is to consider a flag value of 1 to indicate bad data,
#' and 0 to indicate good data. Note that it only makes sense to use
#' velocity (`v`) flags, because other flags are, at least
#' for some instruments, stored as `raw` quantities, and such
#' quantities may not be set to `NA`.
#'
#' @param object an [adp-class] object.
#'
#' @template handleFlagsTemplate
#'
#' @examples
#' # Flag low "goodness" or high "error beam" values.
#' library(oce)
#' data(adp)
#' # Same as Example 2 of ?'setFlags,adp-method'
#' v <- adp[["v"]]
#' i2 <- array(FALSE, dim=dim(v))
#' g <- adp[["g", "numeric"]]
#' # Thresholds on percent "goodness" and error "velocity"
#' G <- 25
#' V4 <- 0.45
#' for (k in 1:3)
#'     i2[,,k] <- ((g[,,k]+g[,,4]) < G) | (v[,,4] > V4)
#' adpQC <- initializeFlags(adp, "v", 2)
#' adpQC <- setFlags(adpQC, "v", i2, 3)
#' adpClean <- handleFlags(adpQC, flags=list(3), actions=list("NA"))
#' # Demonstrate (subtle) change graphically.
#' par(mfcol=c(2, 1))
#' plot(adp, which="u1")
#' plot(adpClean, which="u1")
#'
#' @family things related to adp data
setMethod("handleFlags", signature=c(object="adp", flags="ANY", actions="ANY", where="ANY", debug="ANY"),
          definition=function(object, flags=NULL, actions=NULL, where=NULL, debug=getOption("oceDebug")) {
              ## DEVELOPER 1: alter the next comment to explain your setup
              ## Flag=1 means bad velocity; 0 means good
              names <- names(object[["flags"]])
              for (name in names) {
                  for (j in 1:length(object[[name]])) {
                      if (any(class(object[[name]][j]) == "raw"))
                          stop("use adpConvertRawToNumeric() first to convert raw values to numeric")
                  }
              }
              if (is.null(flags)) {
                  flags <- defaultFlags(object)
                  if (is.null(flags))
                      stop("must supply 'flags', or use initializeFlagScheme() on the adp object first")
              }
              if (is.null(actions)) {
                  actions <- list("NA") # DEVELOPER 3: alter this line to suit a new data class
                  names(actions) <- names(flags)
              }
              if (any(names(actions)!=names(flags)))
                  stop("names of flags and actions must match")
              handleFlagsInternal(object=object, flags=flags, actions=actions, where=where, debug=debug)
          })

#' @templateVar class adp
#' @templateVar details There are no agreed-upon flag schemes for adp data.
#' @template initializeFlagsTemplate
setMethod("initializeFlags",
          c(object="adp", name="ANY", value="ANY", debug="ANY"),
          function(object, name=NULL, value=NULL, debug=getOption("oceDebug")) {
              oceDebug(debug, "setFlags,adp-method name=", name, ", value=", value, "\n")
              if (is.null(name))
                  stop("must supply 'name'")
              res <- initializeFlagsInternal(object, name, value, debug-1)
              res
          })


#' @templateVar class adp
#' @templateVar note The only flag that may be set is `v`, for the array holding velocity. See \dQuote{Indexing rules}, noting that adp data are stored in 3D arrays; Example 1 shows using a data frame for `i`, while Example 2 shows using an array.
#' @template setFlagsTemplate
#' @examples
#' library(oce)
#' data(adp)
#'
#' ## Example 1: flag first 10 samples in a mid-depth bin of beam 1
#' i1 <- data.frame(1:20, 40, 1)
#' adpQC <- initializeFlags(adp, "v", 2)
#' adpQC <- setFlags(adpQC, "v", i1, 3)
#' adpClean1 <- handleFlags(adpQC, flags=list(3), actions=list("NA"))
#' par(mfrow=c(2, 1))
#' ## Top: original, bottom: altered
#' plot(adp, which="u1")
#' plot(adpClean1, which="u1")
#'
#' ## Example 2: percent-good and error-beam scheme
#' v <- adp[["v"]]
#' i2 <- array(FALSE, dim=dim(v))
#' g <- adp[["g", "numeric"]]
#' # Thresholds on percent "goodness" and error "velocity"
#' G <- 25
#' V4 <- 0.45
#' for (k in 1:3)
#'     i2[,,k] <- ((g[,,k]+g[,,4]) < G) | (v[,,4] > V4)
#' adpQC2 <- initializeFlags(adp, "v", 2)
#' adpQC2 <- setFlags(adpQC2, "v", i2, 3)
#' adpClean2 <- handleFlags(adpQC2, flags=list(3), actions=list("NA"))
#' ## Top: original, bottom: altered
#' plot(adp, which="u1")
#' plot(adpClean2, which="u1") # differs at 8h and 20h
#'
#' @family things related to adp data
setMethod("setFlags",
          c(object="adp", name="ANY", i="ANY", value="ANY", debug="ANY"),
          function(object, name=NULL, i=NULL, value=NULL, debug=getOption("oceDebug")) {
              if (is.null(name))
                  stop("must specify 'name'")
              setFlagsInternal(object, name, i, value, debug-1)
          })


#' Summarize an ADP Object
#'
#' Summarize data in an `adp` object.
#'
#' Pertinent summary information is presented.
#'
#' @aliases summary.adp summary,adp,missing-method summary,adp-method
#'
#' @param object an object of class `"adp"`, usually, a result of a call
#' to [read.oce()], [read.adp.rdi()], or
#' [read.adp.nortek()].
#'
#' @param \dots further arguments passed to or from other methods.
#'
#' @return A matrix containing statistics of the elements of the `data`
#' slot.
#'
#' @author Dan Kelley
#'
#' @family things related to adp data
setMethod(f="summary",
          signature="adp",
          definition=function(object, ...) {
              mnames <- names(object@metadata)
              cat("ADP Summary\n-----------\n\n", ...)
              if ("instrumentType" %in% mnames)
                  cat(paste("* Instrument:         ", object@metadata$instrumentType, "\n", sep=""), ...)
              if ("manufacturere" %in% mnames)
                  cat("* Manufacturer:      ", object@metadata$manufacturer, "\n")
              if ("serialNumber" %in% mnames)
                  cat(paste("* Serial number:      ", object@metadata$serialNumber, "\n", sep=""), ...)
              if ("firmwareVersion" %in% mnames)
                  cat(paste("* Firmware version:   ", object@metadata$firmwareVersion, "\n", sep=""), ...)
              if ("filename" %in% mnames)
                  cat(paste("* Source filename:    ``", object@metadata$filename, "``\n", sep=""), ...)
              if ("latitude" %in% names(object@metadata)) {
                  cat(paste("* Location:           ",
                            if (is.na(object@metadata$latitude)) "unknown latitude" else sprintf("%.5f N", object@metadata$latitude), ", ",
                            if (is.na(object@metadata$longitude)) "unknown longitude" else sprintf("%.5f E",
                                                                                                   object@metadata$longitude),
                            "\n", sep=''))
              }
              v.dim <- dim(object[["v"]])
              isAD2CP <- is.ad2cp(object)
              if (!isAD2CP) {
                  cat("* Number of beams:    ", v.dim[3], "\n", sep="")
                  cat("* Number of profiles: ", v.dim[1], "\n", sep="")
                  cat("* Number of cells:    ", v.dim[2], "\n", sep="")
                  cat("* Number of beams:    ", v.dim[3], "\n", sep="")
                  cat("* Cell size:          ", object[["cellSize"]], "m\n", sep="")
              }
              if ("time" %in% names(object@data)) {
                  cat("* Summary of times between profiles:\n")
                  print(summary(diff(as.numeric(object@data$time))))
              }
              if (1 == length(agrep("nortek", object@metadata$manufacturer, ignore.case=TRUE))) {
                  resSpecific <- list(internalCodeVersion=object@metadata$internalCodeVersion,
                                      hardwareRevision=object@metadata$hardwareRevision,
                                      recSize=object@metadata$recSize*65536/1024/1024,
                                      velocityRange=object@metadata$velocityRange,
                                      firmwareVersion=object@metadata$firmwareVersion,
                                      config=object@metadata$config,
                                      configPressureSensor=object@metadata$configPressureSensor,
                                      configMagnetometerSensor=object@metadata$configMagnetometerSensor,
                                      configPressureSensor=object@metadata$configPressureSensor,
                                      configTiltSensor=object@metadata$configTiltSensor,
                                      configTiltSensorOrientation=object@metadata$configTiltSensorOrientation,
                                      serialNumberHead=object@metadata$serialNumberHead,
                                      blankingDistance=object@metadata$blankingDistance,
                                      measurementInterval=object@metadata$measurementInterval,
                                      deploymentName=object@metadata$deploymentName,
                                      velocityScale=object@metadata$velocityScale)
              } else if (1 == length(agrep("rdi", object@metadata$manufacturer, ignore.case=TRUE))) {
                  resSpecific <- list(instrumentSubtype=object@metadata[["instrumentSubtype"]],
                                      manufacturer=object@metadata$manufacturer,
                                      numberOfDataTypes=object@metadata$numberOfDataTypes,
                                      ensembleInFile=object@metadata$ensembleInFile,
                                      headingAlignment=object@metadata$headingAlignment,
                                      headingBias=object@metadata$headingBias,
                                      pingsPerEnsemble=object@metadata$pingsPerEnsemble,
                                      bin1Distance=object@metadata$bin1Distance,
                                      xmitPulseLength=object@metadata$xmitPulseLength,
                                      oceBeamSpreaded=object@metadata$oceBeamSpreaded,
                                      beamConfig=object@metadata$beamConfig)
              } else if (1 == length(agrep("sontek", object@metadata$manufacturer, ignore.case=TRUE))) {
                  resSpecific <- list(cpuSoftwareVerNum=object@metadata$cpuSoftwareVerNum,
                                      dspSoftwareVerNum=object@metadata$dspSoftwareVerNum,
                                      boardRev=object@metadata$boardRev,
                                      adpType=object@metadata$adpType,
                                      slantAngle=object@metadata$slantAngle,
                                      orientation=object@metadata$orientation)
              } else {
                  resSpecific <- list(orientation=object@metadata$orientation)
                  #stop("can only summarize ADP objects of sub-type \"rdi\", \"sontek\", or \"nortek\", not class ", paste(class(object),collapse=","))
              }
              ## 20170107: drop the printing of these. In the new scheme, we can subsample
              ## 20170107: files, and therefore do not read to the end, and it seems silly
              ## 20170107: to use time going through the whole file to find this out. If we
              ## 20170107: decide that this is needed, we could do a seek() to the end of the
              ## 20170107: and then go back to find the final time.

              ## cat(sprintf("* Measurements:       %s %s to %s %s sampled at %.4g Hz\n",
              ##             format(object@metadata$measurementStart), attr(object@metadata$measurementStart, "tzone"),
              ##             format(object@metadata$measurementEnd), attr(object@metadata$measurementEnd, "tzone"),
              ##             1 / object@metadata$measurementDeltat))
              ## subsampleStart <- object@data$time[1]
              ## subsampleDeltat <- as.numeric(object@data$time[2]) - as.numeric(object@data$time[1])
              ## subsampleEnd <- object@data$time[length(object@data$time)]
              ## cat(sprintf("* Subsample:          %s %s to %s %s sampled at %.4g Hz\n",
              ##             format(subsampleStart), attr(subsampleStart, "tzone"),
              ##             format(subsampleEnd),  attr(subsampleEnd, "tzone"),
              ##             1 / subsampleDeltat))
              metadataNames <- names(object@metadata)
              cat("* Frequency:         ", object[["frequency"]], "kHz\n", ...)
              if ("ensembleNumber" %in% names(object@metadata)) {
                  en <- object@metadata$ensembleNumber
                  nen <- length(en)
                  if (nen > 4)
                      cat("* Ensemble Numbers:   ", en[1], ", ", en[2], ", ..., ", en[nen-1L], ", ", en[nen], "\n", sep="")
                  else
                      cat("* Ensemble Numbers:   ", paste(en, collapse=", "), "\n", sep="")
              }
              if (!isAD2CP) {
                  if ("numberOfCells" %in% metadataNames) {
                      dist <- object[["distance"]]
                      if (object[["numberOfCells"]] > 1) {
                          cat(sprintf("* Cells:              %d, centered at %.3f m to %.3f m, spaced by %.3f m\n",
                                      object[["numberOfCells"]], dist[1], tail(dist, 1), diff(dist[1:2])), ...)
                      } else {
                          cat(sprintf("* Cells:              one cell, centered at %.3f m\n", dist[1]), ...)
                      }
                  }
                  originalCoordinate <- object[["originalCoordinate"]]
                  oceCoordinate <- object[["oceCoordinate"]]
                  cat("* Coordinate system: ",
                      if (is.null(originalCoordinate)) "?" else originalCoordinate, "[originally],",
                      if (is.null(oceCoordinate)) "?" else oceCoordinate, "[presently]\n", ...)
                  numberOfBeams <- object[["numberOfBeams"]]
                  beamAngle <- object[["beamAngle"]]
                  ## As of Aug 10, 2019, orientation may be a vector, so we summarize
                  ## a table of values, if so.
                  orientation <- object[["orientation"]]
                  if (length(orientation) > 1) {
                      torientation <- table(orientation)
                      orientation <- paste(unlist(lapply(names(torientation),
                                                         function(x)
                                                           paste(x, torientation[[x]], sep=":"))),
                                           collapse=", ")
                  }
                  beamUnspreaded <- object[["oceBeamUnspreaded"]]
                  cat("* Beams::\n")
                  if ("vv" %in% names(object@data))
                      cat("    Number:          ", if (is.null(numberOfBeams)) "?" else numberOfBeams, "slanted, plus 1 central\n")
                  else
                      cat("    Number:          ", if (is.null(numberOfBeams)) "?" else numberOfBeams, "slanted\n")

                  cat("    Slantwise Angle: ", if (is.null(beamAngle)) "?" else beamAngle , "\n")
                  if (numberOfBeams > 0)
                      cat("    Orientation:     ", if (is.null(orientation)) "?" else orientation, "\n")
                  cat("    Unspreaded:      ", if (is.null(beamUnspreaded)) "?" else beamUnspreaded, "\n")
              }
              transformationMatrix <- object[["transformationMatrix"]]
              if (!is.null(transformationMatrix) && dim(transformationMatrix)[2] >= 3) {
                  digits <- 4
                  cat("* Transformation matrix::\n")
                  cat("  ", format(transformationMatrix[1, ], width=digits+4, digits=digits, justify="right"), "\n")
                  cat("  ", format(transformationMatrix[2, ], width=digits+4, digits=digits, justify="right"), "\n")
                  cat("  ", format(transformationMatrix[3, ], width=digits+4, digits=digits, justify="right"), "\n")
                  if (object[["numberOfBeams"]] > 3)
                      cat("  ", format(transformationMatrix[4, ], width=digits+4, digits=digits, justify="right"), "\n")
              }
              if (isAD2CP) {
                  default <- ad2cpDefaultDataItem(object)
                  for (rt in object[["recordTypes"]]) {
                      if (rt != "text") {
                          isTheDefault <- rt == default
                          cat("* Record type '", rt, "'", if (isTheDefault) " (the default item)::\n" else "::\n", sep="")
                          cat("    Number of profiles: ", length(object[["time", rt]]), "\n")
                          cat("    Number of cells:    ", object[["numberOfCells", rt]], "\n")
                          cat("    Blanking distance:  ", object[["blankingDistance", rt]], "\n")
                          cat("    Cell size:          ", object[["cellSize", rt]], "\n")
                          numberOfBeams <- object[["numberOfBeams", rt]]
                          cat("    Number of beams:    ", numberOfBeams, "\n")
                          cat("    Beam angle:         ", if (numberOfBeams == 1) 0 else object[["beamAngle"]], "\n")
                          if (numberOfBeams > 1)
                              cat("    Coordinate system:  ", object[["oceCoordinate", rt]], "\n")
                      }
                  }
                  processingLogShow(object)
                  invisible(NULL)
              } else {
                  invisible(callNextMethod()) # summary
              }
          })

#' Concatenate adp objects
#'
#' @templateVar class adp
#'
#' @template concatenateTemplate
setMethod(f="concatenate",
          signature="adp",
          definition=function(object, ...) {
              rval <- callNextMethod() # do general work
              ## Make the metadata profile count match the data array dimensions.
              rval@metadata$numberOfSamples <- dim(rval@data$v)[1] # FIXME: handle AD2CP
              ## The general method didn't know that 'distance' was special, and should
              ## not be concatenated, so undo that.
              rval@data$distance <- object@data$distance # FIXME: handle AD2CP
              rval
          })



#' @title Extract Something from an adp Object
#'
#' @param x an [adp-class] object.
#'
#' @section Details of the Specialized Method:
#'
#' Note that the entries within [adp-class] objects vary greatly, from
#' instrument to instrument, and so are only sketched here, and in the output
#' from `[["?"]]`.
#'
#' * If `i` is `"?"`, then the return value is a list
#' containing four items, each of which is a character vector
#' holding the names of things that can be accessed with `[[`.
#' The `data` and `metadata` items hold the names of
#' entries in the object's data and metadata
#' slots, respectively. The `dataDerived`
#' and `metadataDerived` items are *not* authoritative, because
#' information provided by different instruments is so varied.
#'
#' * If `i` is `"u1"` then the return value is `v[,1]`. The
#' same holds for 2, etc., depending on the number of beams in
#' the instrument.
#'
#' * If `i` is `"a1"` then signal amplitude is returned, and similarly
#' for other digits. The results can be in [raw()] or numeric form,
#' as shown in the examples.
#'
#' * If `i` is `"q1"` then signal quality is returned, and similarly
#' for other digits.  As with amplitude, the result can be in [raw()]
#' or numeric form.
#'
#' * If `i` is `"coordinate"`, then the coordinate system is
#' retrieved.
#'
#' @template sub_subTemplate
#'
#' @examples
#' data(adp)
#' # Tests for beam 1, distance bin 1, first 5 observation times
#' adp[["v"]][1:5,1,1]
#' adp[["a"]][1:5,1,1]
#' adp[["a", "numeric"]][1:5,1,1]
#' as.numeric(adp[["a"]][1:5,1,1]) # same as above
#'
#' @author Dan Kelley
#'
#' @family things related to adp data
setMethod(f="[[",
          signature(x="adp", i="ANY", j="ANY"),
          definition=function(x, i, j, ...) {
              ##>message("top: i='", i, "'")
              if (length(i) != 1L)
                  stop("In [[,adp-method() : may only extract 1 item at a time.\n", call.=FALSE)
              ISAD2CP <- is.ad2cp(x)
              ##>message("ISAD2CP=", ISAD2CP)
              metadataDerived <- c("coordinate")
              numberOfBeams <- if (ISAD2CP) 4 else x@metadata$numberOfBeams
              if (is.null(numberOfBeams)) {
                  dataDerived <- c("a", "u", "q")
              } else {
                  dataDerived <- c(paste0("u", seq_len(numberOfBeams)),
                      paste0("a", seq_len(numberOfBeams)),
                      paste0("q", seq_len(numberOfBeams)))
              }
              if (i == "?")
                  return(list(metadata=sort(names(x@metadata)),
                          metadataDerived=sort(metadataDerived),
                          data=sort(names(x@data)),
                          dataDerived=sort(dataDerived)))
              if (i == "distance") {
                  ##>message("asking for 'distance'")
                  if (ISAD2CP) {
                      ## AD2CP is stored in a tricky way.
                      j <- if (missing(j)) ad2cpDefaultDataItem(x) else ad2cpDefaultDataItem(x, j)
                      res <- x@data[[j]]$blankingDistance + x@data[[j]]$cellSize*seq(1, x@data[[j]]$numberOfCells)
                  } else {
                      res <- x@data$distance
                  }
                  res
              } else if (i %in% c("originalCoordinate", "oceCoordinate",
                                  "cellSize", "blankingDistance", "orientation",
                                  "beamUnspreaded", # Note: beamAngle is handled later since it is in metadata
                                  "accelerometerx", "accelerometery", "accelerometerz",
                                  "orientation", "heading", "pitch", "roll",
                                  "ensemble", "time", "pressure", "soundSpeed",
                                  "temperature", "temperatureMagnetometer", "temperatureRTC",
                                  "nominalCorrelation",
                                  "powerLevel", "transmitEnergy",
                                  "v", "a", "q", "g",
                                  "echosounder", "AHRS", "altimeterDistance", "altimeterFigureOfMerit")) {
                  ##>message("asking for i='", i, "' which is in that long list")
                  ##message("i='", i, "'")
                  metadataNames <- names(x@metadata)
                  ##. dataNames <- names(x@data)
                  if (ISAD2CP) {
                      ## AD2CP has 'burst' data records in one list, with 'average' records in another one.
                      ## Permit e.g. "burst:numeric" and "burst numeric" ## FIXME: document this
                      returnNumeric <- FALSE # defult: leave 'raw' data as 'raw'.
                      if (missing(j)) {
                          ##>message("0 a")
                          j <- ad2cpDefaultDataItem(x)
                          returnNumeric <- FALSE
                          jorig <- "(missing)"
                          ##>message("'[[' is defaulting to '", j, "' type of data-record, since 'j' not specified", sep="")
                      } else {
                          jorig <- j
                          ##>message("0 a. j='", j, "'")
                          ## find out if numeric or raw, and clean 'j' of that flag once it is known
                          if (length(grep("numeric", j))) {
                              returnNumeric <- TRUE
                              j <- gsub("numeric", "", j)
                              ##>message("0 b. j='", j, "'")
                          } else if (length(grep("raw", j))) {
                              returnNumeric <- FALSE
                              j <- gsub("raw", "", j)
                              ##>message("0 c. j='", j, "'")
                          }
                          j <- gsub("[ :]+", "", j) # clean spaces or colons, if any
                          ## Look up this name
                          ##>message("0 d. j='", j, "'")
                          j <- ad2cpDefaultDataItem(x, j)
                          ##>message("0 e. j='", j, "'")
                      }
                      ##message("1. j = '", j, "'; jorig='", jorig, "'", sep="")
                      ##numericMode <- 1 == length(grep("numeric", j))
                      ##message("2. numericMode=", numericMode)
                      ##j <- gsub("[: ]?numeric", "", j)
                      ##message("3. j = '", j, "'", sep="")
                      #if (missing(j)) { # default to 'average', if it exists, or to 'burst' if that exists, or fail.
                      #    j <- if (length(x@data$average)) "average" else if (length(x@data$burst))
                      #        "burst" else stop("object's data slot does not contain either 'average' or 'burst'")
                      #}
                      ##message("4. j = '", j, "'", sep="")
                      ## Default to "average" if no j specified
                      if (1 == length(grep("^[ ]*$", j)))
                          j <- "average"
                      ##>message("5. j = '", j, "'", sep="")
                      j <- ad2cpDefaultDataItem(x, j)
                      ##>message("6. i='", i, "', j='", j, "', returnNumeric=", returnNumeric)
                      res <- x@data[[j]][[i]]
                      if (returnNumeric) {
                          ##>message("6-a.")
                          dimres <- dim(res)
                          res <- as.numeric(res)
                          dim(res) <- dimres
                      }
                      ##>message("7 res=", res)
                      res
                 } else {
                      if (!missing(j) && 1 == length(grep("numeric", j))) {
                          res <- x@data[[i]]
                          dim <- dim(res)
                          res <- as.numeric(res)
                          dim(res) <- dim
                          res
                      } else {
                          if (i %in% metadataNames) x@metadata[[i]] else x@data[[i]]
                      }
                  }
              } else if (i %in% c("numberOfBeams", "numberOfCells")) {
                  ##>message("asking for 'numberOfBeams' or 'numberOfCells'")
                  ##message("AA i=", i)
                  if (ISAD2CP) {
                      j <- if (missing(j)) ad2cpDefaultDataItem(x) else ad2cpDefaultDataItem(x, j)
                      x@data[[j]][[i]]
                  } else {
                      x@metadata[[i]]
                  }
              } else if (i == "transformationMatrix") {
                  ##>message("0000")
                  if (ISAD2CP) {
                      ##>message("AD2CP  tm...")
                      theta <- x@metadata$beamAngle * atan2(1, 1) / 45
                      ## The creation of a transformation matrix is covered in Section 5.3 of
                      ## RD Instruments. ADCP Coordinate Transformation. RD Instruments, July 1998.
                      TMc <- 1 # for convex (diverging) beam setup; use -1 for concave
                      TMa <- 1 / (2 * sin(theta))
                      TMb <- 1 / (4 * cos(theta))
                      TMd <- TMa / sqrt(2)
                      rbind(c(TMc*TMa, -TMc*TMa,        0,       0),
                            c(      0,        0, -TMc*TMa, TMc*TMa),
                            c(    TMb,      TMb,      TMb,     TMb),
                            c(    TMd,      TMd,     -TMd,    -TMd))
                  } else {
                      ## message("normal tm...")
                      x@metadata$transformationMatrix
                  }
              } else if (i == "recordTypes") {
                  ##>message("asking for 'recordTypes'")
                  ## FIXME: _AD2CPrecordtype_ update if new record types added to read.adp.ad2cp()
                  if (ISAD2CP) {
                      allowed <- c("burst", "average", "bottomTrack", "interleavedBurst", "burstAltimeter",
                                   "DVLBottomTrack", "echosounder", "waterTrack", "altimeter", "averageAltimeter", "text")
                      res <- allowed[allowed %in% names(x@data)]
                  } else {
                      res <- "depends on the data setup"
                  }
                  res
              } else if (i == "va") {
                  ##>message("asking for 'va'")
                  if (!"va" %in% names(x@data)) {
                      res <- NULL
                  } else {
                      if (!missing(j) && 1 == length(grep("numeric", j))) {
                          res <- x@data$va
                          dim <- dim(res)
                          res <- as.numeric(res)
                          dim(res) <- dim
                      } else {
                          res <- x@data$va
                      }
                  }
                  res
              } else if (i == "vq") {
                  ##>message("asking for 'vq'")
                  if (!"vq" %in% names(x@data)) {
                      res <- NULL
                  } else {
                      if (!missing(j) && 1 == length(grep("numeric", j))) {
                          res <- x@data$vq
                          dim <- dim(res)
                          res <- as.numeric(res)
                          dim(res) <- dim
                      } else {
                          res <- x@data$vq
                      }
                  }
                  res
              } else if (i == "vg") {
                  ##>message("asking for 'vg'")
                  if (!"vg" %in% names(x@data)) {
                      res <- NULL
                  } else {
                      if (!missing(j) && 1 == length(grep("numeric", j))) {
                          res <- x@data$vg
                          dim <- dim(res)
                          res <- as.numeric(res)
                          dim(res) <- dim
                      } else {
                          res <- x@data$vg
                      }
                  }
                  res
              } else if (i == "vv") {
                  ##>message("asking for 'vv'")
                  if (!"vv" %in% names(x@data)) {
                      res <- NULL
                  } else {
                      if (!missing(j) && 1 == length(grep("numeric", j))) {
                          res <- x@data$vv
                          dim <- dim(res)
                          res <- as.numeric(res)
                          dim(res) <- dim
                      } else {
                          res <- x@data$vv
                      }
                  }
                  res
               } else {
                  callNextMethod()     # [[
              }
          })

#' Replace Parts of an ADP Object
#'
#' In addition to the usual insertion of elements by name, note
#' that e.g. `pitch` gets stored into `pitchSlow`.
#'
#' @param x an [adp-class] object.
#'
#' @template sub_subsetTemplate
#'
#' @author Dan Kelley
#'
#' @family things related to adp data
setMethod(f="[[<-",
          signature="adp",
          definition=function(x, i, j, ..., value) {
              ## FIXME: use j for e.g. times
              if (i %in% names(x@metadata)) {
                  x@metadata[[i]] <- value
              } else if (i %in% names(x@data)) {
                  x@data[[i]] <- value
              } else {
                  x <- callNextMethod(x=x, i=i, j=j, ...=..., value=value) # [[<-
              }
              ## Not checking validity because user may want to shorten items one by one, and check validity later.
              ## validObject(x)
              invisible(x)
          })

setValidity("adp",
            function(object) {
                if (!("v" %in% names(object@data))) {
                    cat("object@data$v is missing")
                    return(FALSE)
                }
                if (!("a" %in% names(object@data))) {
                    cat("object@data$a is missing")
                    return(FALSE)
                }
                if (!("q" %in% names(object@data))) {
                    cat("object@data$q is missing")
                    return(FALSE)
                }
                mdim <- dim(object@data$v)
                if ("a" %in% names(object@data) && !all.equal(mdim, dim(object@data$a))) {
                    cat("dimension of 'a' is (", dim(object@data$a), "), which does not match that of 'v' (", mdim, ")\n")
                    return(FALSE)
                }
                if ("q" %in% names(object@data) && !all.equal(mdim, dim(object@data$q))) {
                    cat("dimension of 'a' is (", dim(object@data$a), "), which does not match that of 'v' (", mdim, ")\n")
                    return(FALSE)
                }
                if ("time" %in% names(object@data)) {
                    n <- length(object@data$time)
                    for (item in c("pressure", "temperature", "salinity", "depth", "heading", "pitch", "roll")) {
                        if (item %in% names(object@data) && length(object@data[[item]]) != n) {
                            cat("length of time vector is ", n, " but the length of ", item, " is ",
                                length(object@data[[item]]), "\n")
                            return(FALSE)
                        }
                    }
                    return(TRUE)
                }
            })


#' Subset an ADP Object
#'
#' Subset an adp (acoustic Doppler profile) object, in a manner that is function
#' is somewhat analogous to [subset.data.frame()].
#'
#' For any data type,
#' subsetting can be by `time`, `ensembleNumber`, or `distance`.
#' These may not be combined, but it is easy to use a string of calls to
#' carry out combined operations, e.g.
#' `subset(subset(adp,distance<d0), time<t0)`
#'
#' For the special
#' case of AD2CP data (see [read.adp.ad2cp()]), it is possible to subset
#' to the "average" data records with `subset="average"`, to the
#' "burst" records with `subset="burst"`, or to the "interleavedBurst"
#' with `subset="interleavedBurst"`; note that no warning is issued,
#' if this leaves an object with no useful data.
#'
#' @param x an [adp-class] object.
#'
#' @param subset A condition to be applied to the `data` portion of
#' `x`.  See \sQuote{Details}.
#'
#' @param ... Ignored.
#'
#' @return An [adp-class] object.
#'
#' @examples
#' library(oce)
#' data(adp)
#' # 1. Look at first part of time series, organized by time
#' earlyTime <- subset(adp, time < mean(range(adp[['time']])))
#' plot(earlyTime)
#'
#' # 2. Look at first ten ensembles (AKA profiles)
#' en <- adp[["ensembleNumber"]]
#' firstTen <- subset(adp, ensembleNumber < en[11])
#' plot(firstTen)
#'
#' @author Dan Kelley
#'
#' @family things related to adp data
#' @family functions that subset oce objects
setMethod(f="subset",
          signature="adp",
          definition=function(x, subset, ...) {
              subsetString <- paste(deparse(substitute(expr=subset, env=environment())), collapse=" ")
              res <- x
              dots <- list(...)
              debug <- getOption("oceDebug")
              if (length(dots) && ("debug" %in% names(dots)))
                  debug <- dots$debug
              if (missing(subset))
                  stop("must give 'subset'")
              if (grepl("time", subsetString) || grepl("ensembleNumber", subsetString)) {
                  if (grepl("time", subsetString)) {
                      oceDebug(debug, "subsetting an adp by time\n")
                      if (length(grep("distance", subsetString)))
                          stop("cannot subset by both time and distance; split into multiple calls")
                      keep <- eval(expr=substitute(expr=subset, env=environment()), envir=x@data, enclos=parent.frame(2))
                  } else if (grepl("ensembleNumber", subsetString)) {
                      oceDebug(debug, "subsetting an adp by ensembleNumber\n")
                      if (length(grep("distance", subsetString)))
                          stop("cannot subset by both ensembleNumber and distance; split into multiple calls")
                      if (!"ensembleNumber" %in% names(x@metadata))
                          stop("cannot subset by ensembleNumber because this adp object lacks that information")
                      ## FIXME: in other places, e.g. AllClass.R:350, we have parent.frame().  What is right?
                      keep <- eval(expr=substitute(expr=subset, env=environment()), envir=x@metadata, enclos=parent.frame(2))
                  } else {
                      stop("internal coding error -- please report to developers")
                  }
                  names <- names(x@data)
                  haveDia <- "timeDia" %in% names
                  if (haveDia) {
                      subsetDiaString <- gsub("time", "timeDia", subsetString)
                      keepDia <- eval(parse(text=subsetDiaString), x@data)
                      oceDebug(debug, "for diagnostics, keeping ", 100*sum(keepDia) / length(keepDia), "% of data\n")
                  }
                  oceDebug(debug, vectorShow(keep, "keeping bins:"))
                  oceDebug(debug, "number of kept bins:", sum(keep), "\n")
                  if (sum(keep) < 2)
                      stop("must keep at least 2 profiles")
                  res <- x
                  ## Update those metadata that have one value per ensemble
                  mnames <- names(x@metadata)
                  for (name in c("ensembleNumber", "orientation")) {
                      if (name %in% mnames)
                          res@metadata[[name]] <- x@metadata[[name]][keep]
                  }
                  ## FIXME: check to see if we handling slow timescale data properly
                  for (name in names(x@data)) {
                      if (length(grep("Dia$", name))) {
                          if ("distance" == name)
                              next
                          if (name == "timeDia" || is.vector(x@data[[name]])) {
                              oceDebug(debug, "subsetting x@data$", name, ", which is a vector\n", sep="")
                              res@data[[name]] <- x@data[[name]][keepDia]
                          } else if (is.matrix(x@data[[name]])) {
                              oceDebug(debug, "subsetting x@data$", name, ", which is a matrix\n", sep="")
                              res@data[[name]] <- x@data[[name]][keepDia, ]
                          } else if (is.array(x@data[[name]])) {
                              oceDebug(debug, "subsetting x@data$", name, ", which is an array\n", sep="")
                              res@data[[name]] <- x@data[[name]][keepDia, , , drop=FALSE]
                          }
                      } else {
                          if (name == "time" || is.vector(x@data[[name]])) {
                              if ("distance" == name)
                                  next
                              oceDebug(debug, "subsetting x@data$", name, ", which is a vector\n", sep="")
                              res@data[[name]] <- x@data[[name]][keep] # FIXME: what about fast/slow
                          } else if (is.matrix(x@data[[name]])) {
                              oceDebug(debug, "subsetting x@data$", name, ", which is a matrix\n", sep="")
                              res@data[[name]] <- x@data[[name]][keep, ]
                          } else if (is.array(x@data[[name]])) {
                              oceDebug(debug, "subsetting x@data$", name, ", which is an array\n", sep="")
                              res@data[[name]] <- x@data[[name]][keep, , , drop=FALSE]
                          }
                      }
                  }
                  if ("v" %in% names(x@metadata$flags)) {
                      dim <- dim(x@metadata$flags$v)
                      res@metadata$flags$v <- x@metadata$flags$v[keep, , , drop=FALSE]
                      oceDebug(debug, "subsetting flags$v original dim=",
                               paste(dim, collapse="x"), "; new dim=",
                               paste(dim(res@metadata$flags$v), collapse="x"))
                  }
              } else if (length(grep("distance", subsetString))) {
                  oceDebug(debug, "subsetting an adp by distance\n")
                  if (grepl("time|pressure", subsetString))
                      stop("cannot subset by both bin (e.g. distance) and profile (i.e. time or pressure)")
                  ## keep <- eval(substitute(subset), x@data, parent.frame(2))
                  keep <- eval(expr=substitute(expr=subset, env=environment()), envir=x@data, enclos=parent.frame(2))
                  oceDebug(debug, "subset() will retain", sum(keep), "of", length(keep), "bins in slant beams\n")
                  haveVerticalBeam <- "vv" %in% names(x@data) # assume, later, that va, vg, vq and vdistance exist
                  if (haveVerticalBeam) {
                      oceDebug(debug, "have a vertical beam\n")
                      vkeep <- eval(expr=substitute(expr=subset,env=environment()),envir=list(distance=x@data$vdistance),enclos=parent.frame(2))
                      oceDebug(debug, "subset() will retain", sum(vkeep), "of", length(vkeep), "bins in the vertical beam\n")
                  }
                  if (sum(keep) < 2)
                      stop("must keep at least 2 bins")
                  res <- x
                  res@data$distance <- x@data$distance[keep] # FIXME: broken for AD2CP
                  res@metadata$numberOfCells <- sum(keep)
                  if (haveVerticalBeam)
                      res@data$vdistance <- x@data$vdistance[vkeep]
                  for (name in names(x@data)) {
                      if (name == "time")
                          next
                      # Handle vertical beam.  These items are 2D fields, index1=profile index2=cell. We
                      # use vkeep, based on vdistance, for the subset.
                      if (haveVerticalBeam && (name %in% c("va", "vg", "vq", "vv"))) {
                          oceDebug(debug, "subsetting vertical beam item \"", name, "\"\n", sep="")
                          res@data[[name]] <- x@data[[name]][, vkeep, drop=FALSE]
                      } else {
                          if (is.array(x@data[[name]]) && 3 == length(dim(x@data[[name]]))) {
                              oceDebug(debug, "subsetting array data[[", name, "]] by distance\n")
                              oceDebug(debug, "before, dim(", name, ") =", dim(res@data[[name]]), "\n")
                              res@data[[name]] <- x@data[[name]][, keep, , drop=FALSE]
                              oceDebug(debug, "after, dim(", name, ") =", dim(res@data[[name]]), "\n")
                          }
                      }
                  }
                  oceDebug(debug, "names of flags: ", paste(names(x@metadata$flags), collapse=" "), "\n")
                  if ("v" %in% names(x@metadata$flags)) {
                      vdim <- dim(x@metadata$flags$v)
                      res@metadata$flags$v <- x@metadata$flags$v[, keep, , drop=FALSE]
                      oceDebug(debug, "subsetting flags$v original dim=",
                               paste(vdim, collapse="x"), "; new dim=",
                               paste(dim(res@metadata$flags$v), collapse="x"), "\n")
                  }
              } else if (grepl("pressure", subsetString)) {
                  # FIXME: should subset flags (https://github.com/dankelley/oce/issues/1837#issuecomment-862293585)
                  if (grepl("distance", subsetString))
                      stop("cannot subset by both profile (e.g. time or pressure) and bin (e.g. distance)")
                  ## keep <- eval(substitute(subset), x@data, parent.frame(2))
                  keep <- eval(expr=substitute(expr=subset, env=environment()), envir=x@data, enclos=parent.frame(2))
                  oceDebug(debug, "subset() will retain", sum(keep), "of", length(keep), "profiles\n")
                  nkeep <- length(keep)
                  res <- x
                  for (name in names(x@data)) {
                      if (name == "time") {
                          res@data$time <- x@data$time[keep]
                          oceDebug(debug, "  handled time\n")
                      } else if (name %in% c("va", "vg", "vq", "vv")) {
                          if (is.matrix(x@data[[name]]) && dim(x@data[[name]])[1] == nkeep) {
                              res@data[[name]] <- x@data[[name]][keep,]
                              oceDebug(debug, "  handled vertical beam", name, "\n")
                          } else {
                              oceDebug(debug, "  skipped vertical beam", name, "(first dimension mismatch)\n")
                          }
                      } else if (is.vector(x@data[[name]])) {
                          if (length(x@data[[name]]) == nkeep) {
                              res@data[[name]] <- x@data[[name]][keep]
                              oceDebug(debug, "  handled vector", name, "\n")
                          } else {
                              oceDebug(debug, "  skipped vector", name, "(length mismatch)\n")
                          }
                      } else if (is.matrix(x@data[[name]])) {
                          if (dim(x@data[[name]])[1] == nkeep) {
                              res@data[[name]] <- x@data[[name]][keep,]
                              oceDebug(debug, "  handled matrix", name, "\n")
                          } else {
                              oceDebug(debug, "  skipped matrix", name, "(first dimension mismatch)\n")
                          }
                      } else if (is.array(x@data[[name]])) {
                          if (dim(x@data[[name]])[1] == nkeep) {
                              res@data[[name]] <- x@data[[name]][keep,,]
                              oceDebug(debug, "  handled array", name, "\n")
                          } else {
                              oceDebug(debug, "  skipped array", name, "(first dimension mismatch)\n")
                          }
                      } else if (!is.null(x@data[[name]])) {
                          warning("In subset() : Skipping data@", name, " because it is not a vector, matrix, or array\n", sep="", call.=FALSE)
                      }
                  }
              } else if (length(grep("average", subsetString))) {
                  res@data$burst <- NULL
                  res@data$interleavedBurst <- NULL
              } else if (length(grep("burst", subsetString))) {
                  res@data$average <- NULL
                  res@data$interleavedBurst <- NULL
              } else if (length(grep("interleavedBurst", subsetString))) {
                  res@data$average <- NULL
                  res@data$burst <- NULL
              } else {
                  stop('subset should be by "distance", "time", "average", "burst", or "interleavedBurst"; "',
                       subsetString, '" is not permitted')
              }
              res@metadata$numberOfSamples <- dim(res@data$v)[1] # FIXME: handle AD2CP
              res@metadata$numberOfCells <- dim(res@data$v)[2] # FIXME: handle AD2CP
              res@processingLog <- processingLogAppend(res@processingLog, paste("subset.adp(x, subset=", subsetString, ")", sep=""))
              res
          })

#' Create an ADP Object
#'
#' @details
#' Construct an [adp-class] object.  Only a basic
#' subset of the typical `data` slot is represented in the arguments
#' to this function, on the assumption that typical usage in reading data
#' is to set up a nearly-blank [adp-class] object, the `data`
#' slot of which is then inserted.  However, in some testing situations it
#' can be useful to set up artificial `adp` objects, so the other
#' arguments may be useful.
#'
#' @param time of observations in POSIXct format
#'
#' @param distance to centre of bins
#'
#' @param v array of velocities, with first index for time, second for bin number, and third for beam number
#'
#' @param a amplitude, a [raw()] array with dimensions matching `u`
#'
#' @param q quality, a [raw()] array with dimensions matching `u`
#'
#' @param orientation a string indicating sensor orientation, e.g. `"upward"` and `"downward"`
#'
#' @param coordinate a string indicating the coordinate system, `"enu"`, `"beam"`, `"xy"`, or `"other"`
#'
#' @return An [adp-class] object.
#'
#' @examples
#' data(adp)
#' t <- adp[["time"]]
#' d <- adp[["distance"]]
#' v <- adp[["v"]]
#' a <- as.adp(time=t, distance=d, v=v)
#'\donttest{
#' plot(a)
#'}
#'
#' @author Dan Kelley
#'
#' @family things related to adp data
as.adp <- function(time, distance, v, a=NULL, q=NULL, orientation="upward", coordinate="enu")
{
    res <- new("adp", time=time, distance=distance, v=v, a=a, q=q)
    if (!missing(v)) {
        res@metadata$numberOfBeams <- dim(v)[3] # FIXME: handle AD2CP
        res@metadata$numberOfCells <- dim(v)[2] # FIXME: handle AD2CP
    }
    res@metadata$oceCoordinate <- coordinate # FIXME: handle AD2CP
    res@metadata$orientation <- orientation # FIXME: handle AD2CP
    res@metadata$cellSize <- if (missing(distance)) NA else diff(distance[1:2]) # FIXME: handle AD2CP
    res@metadata$units <- list(v="m/s", distance="m")
    res
}


## head.adp <- function(x, n=6L, ...)
## {
##     numberOfProfiles <- dim(x[["v"]])[1]
##     if (n < 0)
##         look <- seq.int(max(1, (1 + numberOfProfiles + n)), numberOfProfiles)
##     else
##         look <- seq.int(1, min(n, numberOfProfiles))
##     res <- x
##     for (name in names(x@data)) {
##         if ("distance" == name)
##             next
##         if (is.vector(x@data[[name]])) {
##             res@data[[name]] <- x@data[[name]][look]
##         } else if (is.matrix(x@data[[name]])) {
##             res@data[[name]] <- x@data[[name]][look,]
##         } else if (is.array(x@data[[name]])) {
##             res@data[[name]] <- x@data[[name]][look,,]
##         } else {
##             res@data[[name]] <- x@data[[name]][look] # for reasons unknown, 'time' is not a vector
##         }
##     }
##     res@processingLog <- processingLogAppend(res@processingLog, paste(deparse(match.call()), sep="", collapse=""))
##     res
## }

## tail.adp <- function(x, n = 6L, ...)
## {
##     numberOfProfiles <- dim(x[["v"]])[1]
##     if (n < 0)
##         look <- seq.int(1, min(numberOfProfiles, numberOfProfiles + n))
##     else
##         look <- seq.int(max(1, (1 + numberOfProfiles - n)), numberOfProfiles)
##     res <- x
##     for (name in names(x@data)) {
##         if (is.vector(x@data[[name]])) {
##             res@data[[name]] <- x@data[[name]][look]
##         } else if (is.matrix(x@data[[name]])) {
##             res@data[[name]] <- x@data[[name]][look,]
##         } else if (is.array(x@data[[name]])) {
##             res@data[[name]] <- x@data[[name]][look,,]
##         } else {
##             res@data[[name]] <- x@data[[name]][look] # for reasons unknown, 'time' is not a vector
##         }
##     }
##     res@processingLog <- processingLogAppend(res@processingLog, paste(deparse(match.call()), sep="", collapse=""))
##     res
## }



#' Get names of Acoustic-Doppler Beams
#'
#' @param x an [adp-class] object.
#'
#' @param which an integer indicating beam number.
#'
#' @return A character string containing a reasonable name for the beam, of the
#' form `"beam 1"`, etc., for beam coordinates, `"east"`, etc. for
#' enu coordinates, `"u"`, etc. for `"xyz"`, or `"u'"`, etc.,
#' for `"other"` coordinates.  The coordinate system is determined
#' with `x[["coordinate"]]`.
#'
#' @author Dan Kelley
#'
#' @seealso This is used by [read.oce()].
#' @family things related to adp data
#' @family things related to adv data
beamName <- function(x, which)
{

    bn <- x[["oceCoordinate"]]
    if (bn == "beam") {
        paste(gettext("beam", domain="R-oce"), 1:4)[which]
    } else if (bn == "enu") {
        c(gettext("east", domain="R-oce"),
          gettext("north", domain="R-oce"),
          gettext("up", domain="R-oce"),
          gettext("error", domain="R-oce"))[which]
    } else if (bn == "xyz") {
        c("u", "v", "w", "e")[which]
    } else if (bn == "other") {
        c("u'", "v'", "w'", "e")[which]
    } else {
        " "
    }
}


#' Read an ADP File
#'
#' Read an ADP data file, producing an [adp-class] object.
#'
#' Several file types can be handled.  Some of
#' these functions are wrappers that map to device names, e.g.
#' `read.aquadoppProfiler` does its work by calling
#' `read.adp.nortek`; in this context, it is worth noting that the
#' ``aquadopp'' instrument is a one-cell profiler that might just as well have
#' been documented under the heading [read.adv()].
#'
#' @param manufacturer an optional character string indicating the manufacturer, used by
#' the general function `read.adp` to select a subsidiary function to use. If this
#' is not given, then [oceMagic()] is used to try to infer the type. If this
#' is provided, then the value `"rdi"` will cause [read.adp.rdi()]
#' to be used, `"nortek"` will cause [read.adp.nortek()] to be used,
#' and `"sontek"` will cause [read.adp.sontek()] to be used.
#'
#' @param despike if `TRUE`, [despike()] will be used to clean
#' anomalous spikes in heading, etc.
#'
#' @template adpTemplate
#'
#' @author Dan Kelley and Clark Richards
#'
#' @family things related to adp data
read.adp <- function(file, from, to, by, tz=getOption("oceTz"),
                     longitude=NA, latitude=NA,
                     manufacturer,
                     monitor=FALSE, despike=FALSE, processingLog,
                     debug=getOption("oceDebug"),
                     ...)
{
    if (!interactive())
        monitor <- FALSE
    fromGiven <- !missing(from) # FIXME document THIS
    toGiven <- !missing(to) # FIXME document THIS
    byGiven <- !missing(by) # FIXME document THIS
    oceDebug(debug, "read.adp(\"", file, "\"",
             ", from=", if (fromGiven) format(from) else "(missing)",
             ", to=", if (toGiven) format(to) else "(missing)",
             ", by=", if (byGiven) format(by) else "(missing)",
             ", manufacturer=\"", if (missing(manufacturer)) "(missing)" else manufacturer, "\", ...) {\n",
             sep="", unindent=1)
    if (!fromGiven)
        from <- 1
    if (!byGiven)
        by <- 1
    if (!toGiven)
        to <- 0
    if (is.character(file) && 0 == file.info(file)$size)
        stop("empty file")

    if (missing(manufacturer)) {
        oceDebug(debug, "using read.oce() since 'manufacturer' argument is missing\n")
        res <- read.oce(file=file, from=from, to=to, by=by, tz=tz,
                        longitude=longitude, latitude=latitude,
                        debug=debug-1, monitor=monitor, despike=despike,
                        ...)
    } else {
        manufacturer <- pmatch(manufacturer, c("rdi", "nortek", "sontek"))
        oceDebug(debug, "inferred manufacturer to be \"", manufacturer, "\"\n")
        res <- if (manufacturer == "rdi") {
            read.adp.rdi(file=file, from=from, to=to, by=by, tz=tz,
                         longitude=longitude, latitude=latitude,
                         debug=debug-1, monitor=monitor, despike=despike,
                         processingLog=processingLog, ...)
        } else if (manufacturer == "nortek") {
            read.adp.nortek(file=file, from=from, to=to, by=by, tz=tz,
                            longitude=longitude, latitude=latitude,
                            debug=debug-1, monitor=monitor, despike=despike,
                            processingLog=processingLog, ...)
        } else if (manufacturer == "sontek") {
            read.adp.sontek(file=file, from=from, to=to, by=by, tz=tz,
                            longitude=longitude, latitude=latitude,
                            debug=debug-1, monitor=monitor, despike=despike,
                            processingLog=processingLog, ...)
        }
    }
    oceDebug(debug, "} # read.adp()\n", unindent=1)
    res
}


#' Plot an adp Object
#'
#' Create a summary plot of data measured by an acoustic doppler profiler.
#'
#' The plot may have one or more panels, with the content being controlled by
#' the `which` argument.
#'
#' * `which=1:4` (or `which="u1"` to `"u4"`) yield a
#' distance-time image plot of a velocity component.  If `x` is in
#' `beam` coordinates (signalled by
#' `metadata$oce.coordinate=="beam"`), this will be the beam velocity,
#' labelled `b[1]` etc.  If `x` is in xyz coordinates (sometimes
#' called frame coordinates, or ship coordinates), it will be the velocity
#' component to the right of the frame or ship (labelled `u` etc).
#' Finally, if `x` is in `"enu"` coordinates, the image will show the
#' the eastward component (labelled `east`).  If `x` is in
#' `"other"` coordinates, it will be component corresponding to east,
#' after rotation (labelled `u\'`).  Note that the coordinate is set by
#' [read.adp()], or by [beamToXyzAdp()],
#' [xyzToEnuAdp()], or [enuToOtherAdp()].
#'
#' * `which=5:8` (or `which="a1"` to `"a4"`) yield
#' distance-time images of backscatter intensity of the respective beams.  (For
#' data derived from Teledyne-RDI instruments, this is the item called ``echo
#' intensity.'')
#'
#' * `which=9:12` (or `which="q1"` to `"q4"`) yield
#' distance-time images of signal quality for the respective beams.  (For RDI
#' data derived from instruments, this is the item called ``correlation
#' magnitude.'')
#'
#' * `which=60` or `which="map"` draw a map of location(s).
#'
#' * `which=70:73` (or `which="g1"` to `"g4"`) yield
#' distance-time images of percent-good for the respective beams.  (For data
#' derived from Teledyne-RDI instruments, which are the only instruments that
#' yield this item, it is called ``percent good.'')
#'
#' * `which=80:83` (or `which="vv"`, `which="va"`,
#' `which="vq"`, and `which="vg"`) yield distance-time
#' images of the vertical beam fields for a 5 beam "SentinelV" ADCP
#' from Teledyne RDI.
#'
#' * `which="vertical"` yields a two panel distance-time
#' image of vertical beam velocity and amplitude.
#'
#' * `which=13` (or `which="salinity"`) yields a time-series plot
#' of salinity.
#'
#' * `which=14` (or `which="temperature"`) yields a time-series
#' plot of temperature.
#'
#' * `which=15` (or `which="pressure"`) yields a time-series plot
#' of pressure.
#'
#' * `which=16` (or `which="heading"`) yields a time-series plot
#' of instrument heading.
#'
#' * `which=17` (or `which="pitch"`) yields a time-series plot of
#' instrument pitch.
#'
#' * `which=18` (or `which="roll"`) yields a time-series plot of
#' instrument roll.
#'
#' * `which=19` yields a time-series plot of distance-averaged
#' velocity for beam 1, rightward velocity, eastward velocity, or
#' rotated-eastward velocity, depending on the coordinate system.
#'
#' * `which=20` yields a time-series of distance-averaged velocity for
#' beam 2, foreward velocity, northward velocity, or rotated-northward
#' velocity, depending on the coordinate system.
#'
#' * `which=21` yields a time-series of distance-averaged velocity for
#' beam 3, up-frame velocity, upward velocity, or rotated-upward velocity,
#' depending on the coordinate system.
#'
#' * `which=22` yields a time-series of distance-averaged velocity for
#' beam 4, for `beam` coordinates, or velocity estimate, for other
#' coordinates.  (This is ignored for 3-beam data.)
#'
#' * `which="progressiveVector"` (or `which=23`) yields a progressive-vector diagram in the horizontal
#' plane, plotted with `asp=1`.  Normally, the depth-averaged velocity
#' components are used, but if the `control` list contains an item named
#' `bin`, then the depth bin will be used (with an error resulting if the
#' bin is out of range).
#'
#' * `which=24` yields a time-averaged profile of the first component
#' of velocity (see `which=19` for the meaning of the component, in
#' various coordinate systems).
#'
#' * `which=25` as for 24, but the second component.
#'
#' * `which=26` as for 24, but the third component.
#'
#' * `which=27` as for 24, but the fourth component (if that makes
#' sense, for the given instrument).
#'
#' * `which=28` or `"uv"` yields velocity plot in the horizontal
#' plane, i.e. `u[2]` versus `u[1]`.  If the number of data points is small, a
#' scattergraph is used, but if it is large, [smoothScatter()] is
#' used.
#'
#' * `which=29` or `"uv+ellipse"` as the `"uv"` case, but
#' with an added indication of the tidal ellipse, calculated from the eigen
#' vectors of the covariance matrix.
#'
#' * `which=30` or `"uv+ellipse+arrow"` as the
#' `"uv+ellipse"` case, but with an added arrow indicating the mean
#' current.
#'
#' * `which=40` or `"bottomRange"` for average bottom range from
#' all beams of the instrument.
#'
#' * `which=41` to `44` (or `"bottomRange1"` to
#' `"bottomRange4"`) for bottom range from beams 1 to 4.
#'
#' * `which=50` or `"bottomVelocity"` for average bottom velocity
#' from all beams of the instrument.
#'
#' * `which=51` to `54` (or `"bottomVelocity1"` to
#' `"bottomVelocity4"`) for bottom velocity from beams 1 to 4.
#'
#' * `which=55` (or `"heaving"`) for time-integrated,
#' depth-averaged, vertical velocity, i.e. a time series of heaving.
#'
#' * `which=100` (or `"soundSpeed"`) for a time series of sound speed.
#'
#'
#' In addition to the above, the following shortcuts are defined:
#'
#' * `which="velocity"` equivalent to `which=1:3` or `1:4`
#' (depending on the device) for velocity components.
#'
#' * `which="amplitude"` equivalent to `which=5:7`
#' or `5:8` (depending on the device) for backscatter intensity
#' components.
#'
#' * `which="quality"` equivalent to `which=9:11` or `9:12`
#' (depending on the device) for quality components.
#'
#' * `which="hydrography"` equivalent to `which=14:15`
#' for temperature and pressure.
#'
#' * `which="angles"` equivalent to `which=16:18` for
#' heading, pitch and roll.
#'
#' The color scheme for image plots (`which` in 1:12) is provided by the
#' `col` argument, which is passed to [image()] to do the actual
#' plotting.  See \dQuote{Examples} for some comparisons.
#'
#' A common quick-look plot to assess mooring movement is to use
#' `which=15:18` (pressure being included to signal the tide, and tidal
#' currents may dislodge a mooring or cause it to settle).
#'
#' By default, `plot,adp-method` uses a `zlim` value for the
#' [image()] that is constructed to contain all the data, but to be
#' symmetric about zero.  This is done on a per-panel basis, and the scale is
#' plotted at the top-right corner, along with the name of the variable being
#' plotted. You may also supply `zlim` as one of the \dots{} arguments,
#' but be aware that a reasonable limit on horizontal velocity components is
#' unlikely to be of much use for the vertical component.
#'
#' A good first step in the analysis of measurements made from a moored device
#' (stored in `d`, say) is to do `plot(d, which=14:18)`.  This shows
#' time series of water properties and sensor orientation, which is helpful in
#' deciding which data to trim at the start and end of the deployment, because
#' they were measured on the dock or on the ship as it travelled to the mooring
#' site.
#'
#' @param x an [adp-class] object.
#'
#' @param which list of desired plot types.  These are graphed in panels
#' running down from the top of the page.  If `which` is not given,
#' the plot will show images of the distance-time dependence of velocity
#' for each beam. See \dQuote{Details} for the meanings of various values of `which`.
#'
#' @param j optional string specifying a sub-class of `which`. For
#' Nortek Aquadopp profilers, this may either be `"default"` (or missing)
#' to get the main signal, or `"diagnostic"` to get a diagnostic
#' signal. For Nortek AD2CP profiles, this may be any one of
#' `"average"` (or missing) for averaged data, `"burst"`
#' for burst data, or `"interleaved burst"` for interleaved burst data;
#' more data types are provided by that instrument, and may be added here
#' at some future time.
#'
#' @param col optional indication of color(s) to use.  If not provided, the
#' default for images is `oce.colorsPalette(128,1)`, and for lines and
#' points is black.
#'
#' @param breaks optional breaks for color scheme
#'
#' @param zlim a range to be used as the `zlim` parameter to the
#' [imagep()] call that is used to create the image.  If omitted,
#' `zlim` is set for each panel individually, to encompass the data of the
#' panel and to be centred around zero.  If provided as a two-element vector,
#' then that is used for each panel.  If provided as a two-column matrix, then
#' each panel of the graph uses the corresponding row of the matrix; for
#' example, setting `zlim=rbind(c(-1,1),c(-1,1),c(-.1,.1))` might make
#' sense for `which=1:3`, so that the two horizontal velocities have one
#' scale, and the smaller vertical velocity has another.
#'
#' @param titles optional vector of character strings to be used as labels for
#' the plot panels.  For images, these strings will be placed in the right hand
#' side of the top margin.  For timeseries, these strings are ignored.  If this
#' is provided, its length must equal that of `which`.
#'
#' @param lwd if the plot is of a time-series or scattergraph format with
#' lines, this is used in the usual way; otherwise, e.g. for image formats,
#' this is ignored.
#'
#' @param type if the plot is of a time-series or scattergraph format, this is
#' used in the usual way, e.g. `"l"` for lines, etc.; otherwise, as for
#' image formats, this is ignored.
#'
#' @param ytype character string controlling the type of the y axis for images
#' (ignored for time series).  If `"distance"`, then the y axis will be
#' distance from the sensor head, with smaller distances nearer the bottom of
#' the graph.  If `"profile"`, then this will still be true for
#' upward-looking instruments, but the y axis will be flipped for
#' downward-looking instruments, so that in either case, the top of the graph
#' will represent the sample nearest the sea surface.
#'
#' @param drawTimeRange boolean that applies to panels with time as the
#' horizontal axis, indicating whether to draw the time range in the top-left
#' margin of the plot.
#'
#' @param useSmoothScatter boolean that indicates whether to use
#' [smoothScatter()] in various plots, such as `which="uv"`.  If
#' not provided a default is used, with [smoothScatter()] being used
#' if there are more than 2000 points to plot.
#'
#' @param missingColor color used to indicate `NA` values in images (see
#' [imagep()]); set to `NULL` to avoid this indication.
#'
#' @template mgpTemplate
#'
#' @template marTemplate
#'
#' @param mai.palette margins, in inches, to be added to those calculated for
#' the palette; alter from the default only with caution
#'
#' @param tformat optional argument passed to [oce.plot.ts()], for
#' plot types that call that function.  (See [strptime()] for the
#' format used.)
#'
#' @param marginsAsImage boolean, `TRUE` to put a wide margin to the right
#' of time-series plots, even if there are no images in the `which` list.
#' (The margin is made wide if there are some images in the sequence.)
#'
#' @param cex numeric character expansion factor for plot symbols; see [par()].
#'
#' @param cex.axis,cex.lab character expansion factors for axis numbers and axis names; see [par()].
#'
#' @param xlim optional 2-element list for `xlim`, or 2-column matrix, in
#' which case the rows are used, in order, for the panels of the graph.
#'
#' @param ylim optional 2-element list for `ylim`, or 2-column matrix, in
#' which case the rows are used, in order, for the panels of the graph.
#'
#' @param control optional list of parameters that may be used for different
#' plot types.  Possibilities are `drawBottom` (a boolean that indicates
#' whether to draw the bottom) and `bin` (a numeric giving the index of
#' the bin on which to act, as explained in \dQuote{Details}).
#'
#' @param useLayout set to `FALSE` to prevent using [layout()]
#' to set up the plot.  This is needed if the call is to be part of a sequence
#' set up by e.g. `par(mfrow)`.
#'
#' @param coastline a `coastline` object, or a character string naming
#' one.  This is used only for `which="map"`.  See notes at
#' [plot,ctd-method()] for more information on built-in coastlines.
#'
#' @param span approximate span of map in km
#'
#' @param main main title for plot, used just on the top panel, if there are
#' several panels.
#'
#' @param grid if `TRUE`, a grid will be drawn for each panel.  (This
#' argument is needed, because calling [grid()] after doing a
#' sequence of plots will not result in useful results for the individual
#' panels.
#'
#' @param grid.col color of grid
#'
#' @param grid.lty line type of grid
#'
#' @param grid.lwd line width of grid
#' @template debugTemplate
#'
#' @param \dots optional arguments passed to plotting functions.  For example,
#' supplying `despike=TRUE` will cause time-series panels to be de-spiked
#' with [despike()].  Another common action is to set the color for
#' missing values on image plots, with the argument `missingColor` (see
#' [imagep()]).  Note that it is an error to give `breaks` in
#' \dots{}, if the formal argument `zlim` was also given, because they
#' could contradict each other.
#'
#' @return A list is silently returned, containing `xat` and `yat`,
#' values that can be used by [oce.grid()] to add a grid to the plot.
#'
#' @examples
#' library(oce)
#' data(adp)
#' plot(adp, which=1:3)
#' plot(adp, which='temperature', tformat='%H:%M')
#'
#' @author Dan Kelley
#'
#' @family functions that plot oce data
#' @family things related to adp data
#'
#' @aliases plot.adp
## DEVELOPER NOTE: update first test in tests/testthat/test_adp.R if a new 'which' is handled
setMethod(f="plot",
          signature=signature("adp"),
          definition=function(x, which, j,
                              col, breaks, zlim,
                              titles,
                              lwd=par('lwd'),
                              type='l',
                              ytype=c("profile", "distance"),
                              drawTimeRange=getOption("oceDrawTimeRange"),
                              useSmoothScatter,
                              missingColor="gray",
                              mgp=getOption("oceMgp"),
                              mar=c(mgp[1]+1.5, mgp[1]+1.5, 1.5, 1.5),
                              mai.palette=rep(0, 4),
                              tformat,
                              marginsAsImage=FALSE,
                              cex=par("cex"), cex.axis=par("cex.axis"), cex.lab=par("cex.lab"),
                              xlim, ylim,
                              control,
                              useLayout=FALSE,
                              coastline="coastlineWorld", span=300,
                              main="",
                              grid=FALSE, grid.col="darkgray", grid.lty="dotted", grid.lwd=1,
                              debug=getOption("oceDebug"),
                              ...)
          {
              debug <- max(0, min(debug, 4))
              theCall <- gsub(" = [^,)]*", "", deparse(expr=match.call()))
              theCall <- paste(theCall, collapse=" ")
              theCall <- gsub("[ ]+", " ", theCall)
              theCall <- gsub(".local", "plot,adp-method", theCall)
              oceDebug(debug, theCall, " {\n", style="bold", sep="", unindent=1)
              #> oceDebug(debug, "plot,adp-method(x, ",
              #>          argShow(mar),
              #>          "\n", sep="", unindent=1, style="bold")
              #> oceDebug(debug, "                ",
              #>          argShow(mgp),
              #>          "\n", sep="", unindent=1, style="bold")
              #> oceDebug(debug, "                ",
              #>          argShow(which),
              #>          "\n", sep="", unindent=1, style="bold")
              #> oceDebug(debug, "                ",
              #>          argShow(cex),
              #>          argShow(cex.axis),
              #>          argShow(cex.lab),
              #>          "\n", sep="", unindent=1, style="bold")
              #> oceDebug(debug, "                ",
              #>          argShow(breaks),
              #>          argShow(j),
              #>          "...) {\n", sep="", unindent=1, style="bold")
              dots <- list(...)
              dotsNames <- names(dots)
              # Catch some errors users might make
              if ("colormap" %in% dotsNames)
                  warning("In plot,adp-method() : \"colormap\" is handled by this function\n", call.=FALSE)


              ## oceDebug(debug, "par(mar)=", paste(par('mar'), collapse=" "), "\n")
              ## oceDebug(debug, "par(mai)=", paste(par('mai'), collapse=" "), "\n")
              ## oceDebug(debug, "par(mfg)=", paste(par('mfg'), collapse=" "), "\n")
              ## oceDebug(debug, "mai.palette=", paste(mai.palette, collapse=" "), "\n")
              instrumentType <- x[["instrumentType"]]
              if (is.null(instrumentType))
                  instrumentType <- "" # simplifies later checks
              oceDebug(debug, "instrumentType=\"", instrumentType, "\"\n", sep="")
              ## interpret mode, j
              if (missing(j))
                  j <- ""
              if (instrumentType == "aquadopp") {
                  if (!missing(j) && j == "diagnostic") {
                      if (x[["numberOfCells"]] != 1) {
                          warning("This object claims to be Nortek Aquadopp, but there is more than 1 cell, so it must not be; so j=\"diagnostic\" is being ignored")
                          j <- 'normal'
                      }
                      if (!("timeDia" %in% names(x@data))) {
                          warning("This instrument did not record Diagnostic data, so j=\"diagnostic\" is being ignored")
                          j <- "normal"
                      }
                  }
              } else if (instrumentType == "AD2CP") {
                  jOrig <- j
                  j <- ad2cpDefaultDataItem(x, j)
                  if (j != jOrig)
                      oceDebug(debug, "given the object contents, 'j' was changed from \"", jOrig, "\" to \"", j, "\", for this Nortek AD2CP instrument\n", sep="")
              }

              if (missing(which)) {
                  ## Note that j is ignored for e.g. RDI adp.
                  which <- 1:dim(x[["v", j]])[3]
                  oceDebug(debug, "setting which=c(", paste(which, collapse=","), "), based on the data\n", sep="")
              }
              colGiven <- !missing(col)
              breaksGiven <- !missing(breaks)
              zlimGiven <- !missing(zlim)
              if (breaksGiven && zlimGiven)
                  stop("cannot supply both zlim and breaks")
              ylimGiven <- !missing(ylim)
              oceDebug(debug, 'ylimGiven=', ylimGiven, '\n')
              res <- list(xat=NULL, yat=NULL)

             if (ylimGiven)
                  oceDebug(debug, "ylim=c(", paste(ylim, collapse=", "), ")\n")
              if (!inherits(x, "adp"))
                  stop("method is only for objects of class '", "adp", "'")
              if (!(is.null(x@metadata$haveActualData) || x@metadata$haveActualData))
                  stop("In plot,adp-method() : there are no profiles in this dataset", call.=FALSE)
              opar <- par(no.readonly = TRUE)
              nw <- length(which)
              fac <- if (nw < 3) 1 else 0.66 # try to emulate par(mfrow)
              ## par(cex=cex*fac, cex.axis=fac*cex.axis, cex.lab=fac*cex.lab) # BUILD-TEST FAILURE
              ## par(cex=cex*fac)                                             # OK
              oceDebug(debug, "adp.R:1759 cex=", cex, ", original par('cex')=", par('cex'), "\n")
              ##par(cex=cex*fac, cex.axis=fac*cex.axis)                         # OK
              par(cex.axis=fac*cex.axis, cex.lab=fac*cex.lab) # OK
              oceDebug(debug, "adp.R:1761 ... after par() call, have par('cex')=", par('cex'), "\n")
              rm(fac)
              numberOfBeams <- x[["numberOfBeams", j]]
              oceDebug(debug, "numberOfBeams=", numberOfBeams, " (note: j=\"", j, "\")\n", sep="")
              numberOfCells <- x[["numberOfCells", j]]
              oceDebug(debug, "numberOfCells=", numberOfCells, " (note: j=\"", j, "\")\n", sep="")

              if (nw == 1) {
                  pm <- pmatch(which, c("velocity", "amplitude", "quality", "hydrography", "angles"))
                  ## FIXME: decide what to do about 5-beam ADCPs
                  if (!is.na(pm)) {
                      if (pm == 1)
                          which <- 0 + seq(1, min(4, numberOfBeams)) # 5th beam not included
                      else if (pm == 2)
                          which <- 4 + seq(1, min(4, numberOfBeams)) # 5th beam not included
                      else if (pm == 3)
                          which <- 8 + seq(1, min(4, numberOfBeams)) # 5th beam not included
                      else if (pm == 4)
                          which <- 14:15
                      else if (pm == 5)
                          which <- 16:18
                      nw <- length(which)
                  }
              }
              if (!missing(titles) && length(titles) != nw)
                  stop("length of 'titles' must equal length of 'which'")
              if (nw > 1)
                  on.exit(par(opar))
              if (is.numeric(which)) {
                  whichFraction <- which - floor(which)
                  which <- floor(which)
              } else {
                  whichFraction <- rep(0, length(which))
              }
              par(mgp=mgp, mar=mar, cex=cex)
              ytype <- match.arg(ytype)
              ## user may specify a matrix for xlim and ylim
              if (ylimGiven) {
                  if (is.matrix(ylim)) {
                      if (dim(ylim)[2] != nw) {
                          ylim2 <- matrix(ylim, ncol=2, nrow=nw, byrow=TRUE) # FIXME: is this what I want?
                      }
                  } else {
                      ylim2 <- matrix(ylim, ncol=2, nrow=nw, byrow=TRUE) # FIXME: is this what I want?
                  }
                  class(ylim2) <- class(ylim)
                  ylim <- ylim2
              }
              xlimGiven <- !missing(xlim)
              if (xlimGiven) {
                  if (is.matrix(xlim)) {
                      if (dim(xlim)[2] != nw) {
                          xlim2 <- matrix(xlim, ncol=2, nrow=nw) # FIXME: is this what I want?
                      }
                  } else {
                      if (length(xlim) != 2)
                          stop("xlim must be a vector of length 2, or a 2-column matrix")
                      xlim2 <- matrix(xlim[1:2], ncol=2, nrow=nw, byrow=TRUE)
                  }
                  class(xlim2) <- class(xlim)
                  attr(xlim2, "tzone") <- attr(xlim, "tzone")
                  xlim <- xlim2
              }
              if (missing(zlim)) {
                  zlimGiven <- FALSE
                  zlimAsGiven <- NULL
              } else {
                  zlimGiven <- TRUE
                  if (is.vector(zlim)) {
                      if (length(zlim) == 2) {
                          zlimAsGiven <- matrix(rep(zlim, length(which)), ncol=2, byrow=TRUE)
                      } else {
                          stop("zlim must be a vector of length 2, or a matrix with 2 columns")
                      }
                  } else {
                      ## FIXME: should this be made into a matrix?
                      zlimAsGiven <- zlim
                  }
              }

              ylimAsGiven <- if (ylimGiven) ylim else NULL
              if (missing(lwd))
                  lwd <- rep(par('lwd'), length.out=nw)
              else
                  lwd <- rep(lwd, length.out=nw)
              if (missing(main))
                  main <- rep('', length.out=nw)
              else
                  main <- rep(main, length.out=nw)
              ## oceDebug(debug, "later on in plot,adp-method:\n")
              ## oceDebug(debug, "  par(mar)=", paste(par('mar'), collapse=" "), "\n")
              ## oceDebug(debug, "  par(mai)=", paste(par('mai'), collapse=" "), "\n")

              oceDebug(debug, "which:", which, "\n")
              whichOrig <- which
              which <- oce.pmatch(which,
                                  list(u1=1, u2=2, u3=3, u4=4,
                                       a1=5, a2=6, a3=7, a4=8,
                                       q1=9, q2=10, q3=11, q4=12,
                                       g1=70, g2=71, g3=72, g4=73,
                                       salinity=13,
                                       temperature=14,
                                       pressure=15,
                                       heading=16,
                                       pitch=17,
                                       roll=18,
                                       progressiveVector=23,
                                       uv=28,
                                       "uv+ellipse"=29,
                                       "uv+ellipse+arrow"=30,
                                       bottomRange=40,
                                       bottomRange1=41, bottomRange2=42, bottomRange3=43, bottomRange4=44,
                                       bottomVelocity=50,
                                       bottomVelocity1=51, bottomVelocity2=52, bottomVelocity3=53, bottomVelocity4=54,
                                       heaving=55,
                                       map=60,
                                       soundSpeed=100,
                                       velocity=1:3,
                                       amplitude=5:7,
                                       quality=9:11,
                                       hydrography=14:15,
                                       angles=16:18,
                                       vertical=80:81,
                                       vv=80, va=81, vq=82, vg=83))
              nw <- length(which) # may be longer with e.g. which='velocity'
              if (any(is.na(which)))
                  stop("plot,adp-method(): unrecognized 'which' code: ", paste(whichOrig[is.na(which)], collapse=" "),
                       call.=FALSE)
              oceDebug(debug, "which:", which, "(after conversion to numerical codes)\n")
              ## FIXME: delete this comment-block after key plot types are checked.
              ## I had this as a test, in early Nov 2018. But now, I prefer
              ##OLD if ("instrumentType" %in% names(x@metadata) && !is.null(x@metadata$instrumentType) && x@metadata$instrumentType == "AD2CP") {
              ##OLD     if (!all(which %in% 1:4))
              ##OLD         warning("In plot,adp-method() : only 'which' <5 has been tested", call.=FALSE)
              ##OLD }
              images <- c(1:12, 70:73, 80:83)
              timeseries <- c(13:22, 40:44, 50:54, 55, 100)
              spatial <- 23:27
              #speed <- 28

              tt <- x[["time", j]]
              ##ttDia <- x@data$timeDia  # may be null
              class(tt) <- c("POSIXct", "POSIXt") # otherwise image() gives warnings
              if (!zlimGiven && all(which %in% 5:8)) {
                  ## single scale for all 'a' (amplitude) data
                  zlim <- range(abs(as.numeric(x[["a"]][, , which[1]-4])), na.rm=TRUE) # FIXME name of item missing, was ma
                  if (length(which) > 1) {
                      for (w in 2:length(which)) {
                          zlim <- range(abs(c(zlim, x[["a"]][, , which[w]-4])), na.rm=TRUE) # FIXME: check name
                      }
                  }
              }
              ##oceDebug(debug, "useLayout=", useLayout, "\n")
              showBottom <- ("bottomRange" %in% names(x@data)) && !missing(control) && !is.null(control["drawBottom"])
              if (showBottom)
                  bottom <- apply(x@data$bottomRange, 1, mean, na.rm=TRUE)
              oceDebug(debug, "showBottom=", showBottom, "\n")
              oceDebug(debug, "cex=", cex, ", par('cex')=", par('cex'), "\n")
              if (useLayout) {
                  if (any(which %in% images) || marginsAsImage) {
                      w <- 1.5
                      lay <- layout(matrix(1:(2*nw), nrow=nw, byrow=TRUE), widths=rep(c(1, lcm(w)), nw))
                      oceDebug(debug, "calling layout(matrix...)\n")
                      oceDebug(debug, "using layout, since this is an image, or has marginsAsImage\n")
                  } else {
                      if (nw != 1 || which != 23) {
                          lay <- layout(cbind(1:nw))
                          oceDebug(debug, "calling layout(cbind(1:", nw, ")\n")
                          oceDebug(debug, "using layout\n")
                      }
                  }
              } else {
                  if (nw > 1) {
                      par(mfrow=c(nw, 1))
                      oceDebug(debug, "calling par(mfrow=c(", nw, ", 1)\n")
                  }
              }
              flipy <- ytype == "profile" && x@metadata$orientation[1] == "downward"
              ##message("numberOfBeams=", numberOfBeams)
              ##message("numberOfCells=", numberOfCells)
              haveTimeImages <- any(which %in% images) && 1 < numberOfCells
              oceDebug(debug, 'haveTimeImages=', haveTimeImages, '(if TRUE, it means any timeseries graphs get padding on RHS)\n')
              par(mar=mar, mgp=mgp)
              if (haveTimeImages) {
                  oceDebug(debug, "setting up margin spacing before plotting\n")
                  oceDebug(debug, "before: ", vectorShow(par("mar")))
                  ## Since zlim not given, this just does calculations
                  drawPalette(#cex.axis=cex * (1 - min(nw / 8, 1/4)),
                              debug=debug-1)
                  oceDebug(debug, "after: ", vectorShow(par("mar")))
              }
              omar <- par("mar")
              oceDebug(debug, vectorShow(omar))
              ##oceDebug(debug, "drawTimeRange=", drawTimeRange, "\n", sep="")
              oceDebug(debug, "cex=", cex, ", par('cex')=", par('cex'), "\n")
              for (w in 1:nw) {
                  oceDebug(debug, "plot,adp-method top of loop (before setting par('mar'))\n")
                  oceDebug(debug, vectorShow(par("mar")))
                  oceDebug(debug, vectorShow(par("mai")))
                  oceDebug(debug, vectorShow(omar))
                  par(mar=omar) # ensures all panels start with original mar
                  oceDebug(debug, "which[", w, "]=", which[w], "\n", sep="")
                  if (which[w] %in% images) {
                      ## image types
                      skip <- FALSE
                      numberOfBeams <- x[["numberOfBeams", j]]
                      v <- x[["v"]]
                      if (which[w] %in% 1:4) {
                          ## velocity
                          if (instrumentType == "aquadopp" && j == "diagnostic") {
                              oceDebug(debug, "a diagnostic velocity component image/timeseries\n")
                              z <- x@data$vDia[, , which[w]]
                              zlab <- if (missing(titles)) paste(beamName(x, which[w]), "Dia", sep="") else titles[w]
                              xdistance <- x[["distance", j]]
                              oceDebug(debug, vectorShow(xdistance))
                              y.look <- if (ylimGiven) (ylimAsGiven[w, 1] <= xdistance & xdistance <= ylimAsGiven[w, 2]) else rep(TRUE, length(xdistance))
                              zlim <- if (zlimGiven) zlimAsGiven[w, ] else
                                  max(abs(x@data$vDia[, y.look, which[w]]), na.rm=TRUE) * c(-1, 1)
                          } else {
                              oceDebug(debug, "a velocity component image/timeseries\n")
                              z <- x[["v", j]][, , which[w]]
                              oceDebug(debug, "class(z) after subsetting for 3rd dimension:: ", class(z), "\n")
                              ## oceDebug(debug, "dim(z): ", paste(dim(z), collapse="x"), "\n")
                              zlab <- if (missing(titles)) beamName(x, which[w]) else titles[w]
                              oceDebug(debug, "zlab:", zlab, "\n")
                              xdistance <- x[["distance", j]]
                              oceDebug(debug, vectorShow(xdistance))
                              y.look <- if (ylimGiven) ylimAsGiven[w, 1] <= xdistance & xdistance <= ylimAsGiven[w, 2] else rep(TRUE, length(xdistance))
                              oceDebug(debug, vectorShow(y.look))
                              if (0 == sum(y.look))
                                  stop("no data in the provided ylim=c(", paste(ylimAsGiven[w, ], collapse=","), ")")
                              zlim <- if (zlimGiven) {
                                  zlimAsGiven[w, ]
                              } else {
                                  if (breaksGiven) {
                                      NULL
                                  } else {
                                      if (is.array(z)){
                                          max(abs(z[, y.look]), na.rm=TRUE) * c(-1, 1)
                                      } else {
                                          max(abs(z), na.rm=TRUE) * c(-1, 1)
                                      }
                                  }
                              }
                          }
                      } else if (which[w] %in% 5:8) {
                          oceDebug(debug, "which[", w, "]=", which[w], "; this is some type of amplitude\n", sep="")
                          ## amplitude
                          if (j == "diagnostic" && "aDia" %in% names(x@data)) {
                              oceDebug(debug, "a diagnostic amplitude component image/timeseries\n")
                              z <- x[["aDia", "numeric"]][, , which[w]-4]
                              xdistance <- x[["distance", j]]
                              oceDebug(debug, vectorShow(xdistance))
                              y.look <- if (ylimGiven) ylimAsGiven[1] <= xdistance & xdistance <= ylimAsGiven[2] else rep(TRUE, length(xdistance))
                              oceDebug(debug, vectorShow(y.look))
                              zlim <- if (zlimGiven) zlimAsGiven[w, ] else {
                                  if (breaksGiven) NULL else range(z[, y.look], na.rm=TRUE)
                              }
                              zlab <- c(expression(aDia[1]), expression(a[2]), expression(aDia[3]), expression(aDia[4]))[which[w]-4]
                          } else {
                              oceDebug(debug, "an amplitude component image/timeseries\n")
                              a <- x[["a", paste(j, "numeric")]]
                              z <- a[, , which[w]-4]
                              dim(z) <- dim(a)[1:2]
                              oceDebug(debug, "accessed data, of dim=", paste(dim(z), collapse="x"), "\n")
                              ##OLD dim(z) <- dim(x@data$a)[1:2] # FIXME: why was this here?
                              xdistance <- x[["distance", j]]
                              oceDebug(debug, vectorShow(xdistance))
                              y.look <- if (ylimGiven) ylimAsGiven[1] <= xdistance & xdistance <= ylimAsGiven[2] else rep(TRUE, length(xdistance))
                              oceDebug(debug, vectorShow(y.look))
                              zlim <- if (zlimGiven) zlimAsGiven[w, ] else {
                                  if (breaksGiven) NULL else range(as.numeric(z[, y.look]), na.rm=TRUE)
                              }
                              oceDebug(debug, "zlim: ", paste(zlim, collapse=" "), "\n")
                              zlab <- c(expression(a[1]), expression(a[2]), expression(a[3]), expression(a[4]))[which[w]-4]
                              oceDebug(debug, "zlab: '", as.character(zlab), "'\n")
                          }
                      } else if (which[w] %in% 9:12) {
                          oceDebug(debug, " which[",w,"]=",which[w],": quality or correlation\n",sep="")
                          ## correlation, or quality. First, try 'q', then 'amp'
                          q <- x[["q", paste(j, "numeric")]]
                          if (!is.null(q)) {
                              oceDebug(debug, "[['q']] works for this object\n")
                              z <- q[, , which[w]-8]
                              dim(z) <- dim(q)[1:2]
                              rm(q)
                              zlim <- c(0, 256)
                              zlab <- c(expression(q[1]), expression(q[2]), expression(q[3]))[which[w]-8]
                          } else {
                              amp <- x[["amp"]]
                              if (!is.null(amp)) {
                                  oceDebug(debug, "[['amp']] works for this object\n")
                                  z <- amp[, , which[w]-8]
                                  dim(z) <- dim(amp)[1:2]
                                  rm(amp)
                                  zlim <- c(0, max(z, na.rm=TRUE))
                                  zlab <- c(expression(amp[1]), expression(amp[2]), expression(amp[3]))[which[w]-8]
                              } else {
                                  stop("In plot,adp-method() : ADP object lacks both 'q' and 'amp' data items", call.=FALSE)
                              }
                          }
                      } else if (which[w] %in% 70:(69+x[["numberOfBeams"]])) {
                          ## correlation
                          xg <- x[["g", paste(j, "numeric")]]
                          if (!is.null(xg)) {
                              z <- as.numeric(xg[, , which[w]-69])
                              dim(z) <- dim(xg)[1:2]
                              rm(xg)
                              zlim <- c(0, 100)
                              zlab <- c(expression(g[1]), expression(g[2]), expression(g[3]))[which[w]-8]
                          } else {
                              stop("In plot,adp-method() : ADP object lacks a 'g' data item", call.=FALSE)
                          }
                      } else if (which[w] == 80) {
                          ## vertical beam velocity
                          z <- x[["vv", j]]
                          if (!is.null(z)) {
                              oceDebug(debug, "vertical beam velocity\n")
                              zlab <- if (missing(titles)) expression(w[vert]) else titles[w]
                              xdistance <- x[["distance", j]]
                              y.look <- if (ylimGiven) ylimAsGiven[w, 1] <= xdistance & xdistance <= ylimAsGiven[w, 2] else rep(TRUE, length(xdistance))
                              if (0 == sum(y.look))
                                  stop("no data in the provided ylim=c(", paste(ylimAsGiven[w, ], collapse=","), ")")
                              zlim <- if (zlimGiven) zlimAsGiven[w, ] else {
                                  if (breaksGiven) NULL else c(-1, 1)
                              }
                          } else {
                              stop("In plot,adp-method() : ADP object lacks a 'vv' data item, so which=80 and which=\"vv\" cannot work", call.=FALSE)
                          }
                      } else if (which[w] == 81) {
                          ## vertical beam amplitude
                          z <- x[["va", paste(j, "numeric")]]
                          if (!is.null(z)) {
                              oceDebug(debug, "vertical beam amplitude\n")
                              xdistance <- x[["distance", j]]
                              y.look <- if (ylimGiven) ylimAsGiven[1] <= xdistance & xdistance <= ylimAsGiven[2] else rep(TRUE, length(xdistance))
                              zlim <- if (zlimGiven) zlimAsGiven[w, ] else {
                                  if (breaksGiven) NULL else range(as.numeric(x@data$va[, y.look]), na.rm=TRUE)
                              }
                              zlab <- expression(a[vert])
                          } else {
                              stop("In plot,adp-method() : ADP object lacks a 'va' data item, so which=81 and which=\"va\" cannot work", call.=FALSE)
                          }
                      } else if (which[w] == 82) {
                          ## vertical beam correlation
                          z <- x[["vq", paste(j, "numeric")]]
                          if (!is.null(z)) {
                              oceDebug(debug, "vertical beam correlation\n")
                              xdistance <- x[["distance", j]]
                              y.look <- if (ylimGiven) ylimAsGiven[1] <= xdistance & xdistance <= ylimAsGiven[2] else rep(TRUE, length(xdistance))
                              zlim <- if (zlimGiven) zlimAsGiven[w, ] else {
                                  if (breaksGiven) NULL else range(as.numeric(x@data$vq[, y.look]), na.rm=TRUE)
                              }
                              zlab <- expression(q[vert])
                          } else {
                              stop("In plot,adp-method() : ADP object lacks a 'vq' data item, so which=82 and which=\"vq\" cannot work", call.=FALSE)
                          }
                      } else if (which[w] == 83) {
                          ## vertical beam percent good
                          z <- x[["vg", paste(j, "numeric")]]
                          if (!is.null(z)) {
                              oceDebug(debug, "vertical beam percent good\n")
                              xdistance <- x[["distance", j]]
                              y.look <- if (ylimGiven) ylimAsGiven[1] <= xdistance & xdistance <= ylimAsGiven[2] else rep(TRUE, length(xdistance))
                              zlim <- if (zlimGiven) zlimAsGiven[w, ] else {
                                  if (breaksGiven) NULL else range(x[["vg", "numeric"]][, y.look], na.rm=TRUE)
                              }
                              zlab <- expression(g[vert])
                          } else {
                              stop("In plot,adp-method() : ADP object lacks a 'vg' data item, so which=83 and which=\"vg\" cannot work", call.=FALSE)
                          }
                      } else {
                          skip <- TRUE
                      }
                      if (!skip) {
                          if (numberOfCells > 1) {
                              if (xlimGiven) {
                                  oceDebug(debug, "about to call imagep() with xlim given and par('cex')=", par("cex"), ", cex=", cex, "\n", sep="")
                                  oceDebug(debug, "xlimGiven case\n")
                                  ats <- imagep(x=tt, y=x[["distance", j]], z=z,
                                                xlim=xlim[w, ],
                                                zlim=zlim,
                                                flipy=flipy,
                                                col=if (colGiven) col else {
                                                    if (missing(breaks)) oce.colorsPalette(128, 1)
                                                    else oce.colorsPalette(length(breaks)-1, 1)
                                                },
                                                breaks=breaks,
                                                ylab=resizableLabel("distance km"),
                                                xlab="Time",
                                                zlab=zlab,
                                                tformat=tformat,
                                                drawTimeRange=drawTimeRange,
                                                drawContours=FALSE,
                                                missingColor=missingColor,
                                                mgp=mgp,
                                                mar=omar,
                                                mai.palette=mai.palette,
                                                cex=1,
                                                main=main[w],
                                                debug=debug-1,
                                                ...)
                              } else {
                                  oceDebug(debug, "about to call imagep() with time[1]=", format(tt[[1]], "%Y-%m-%d %H:%M:%S"), "\n")
                                  ats <- imagep(x=tt, y=x[["distance", j]], z=z,
                                                zlim=zlim,
                                                flipy=flipy,
                                                ylim=if (ylimGiven) ylim[w, ] else range(x[["distance", j]], na.rm=TRUE),
                                                col=if (colGiven) col else { if (missing(breaks)) oce.colorsPalette(128, 1) else oce.colorsPalette(length(breaks)-1, 1) },
                                                breaks=breaks,
                                                ylab=resizableLabel("distance"),
                                                xaxs="i",
                                                xlab="Time",
                                                zlab=zlab,
                                                tformat=tformat,
                                                drawTimeRange=drawTimeRange,
                                                drawContours=FALSE,
                                                missingColor=missingColor,
                                                mgp=mgp,
                                                mar=mar,
                                                mai.palette=mai.palette,
                                                cex=1,
                                                main=main[w],
                                                debug=debug-1,
                                                ...)
                              }
                              if (showBottom)
                                  lines(x[["time", j]], bottom)
                          } else {
                              col <- if (colGiven) rep(col, length.out=nw) else rep("black", length.out=nw)
                              time  <- if (j== "diagnostic") x@data$timeDia else x[["time"]]
                              tlim <- range(time)
                              ats <- oce.plot.ts(time, z, ylab=zlab,
                                                 xlim=if (xlimGiven) xlim[w, ] else tlim,
                                                 ylim=if (ylimGiven) ylim[w, ],
                                                 xaxs="i",
                                                 col=col[w],
                                                 lwd=lwd[w],
                                                 cex=1, cex.axis=1, cex.lab=1,
                                                 main=main[w],
                                                 type=type,
                                                 mgp=mgp,
                                                 mar=omar,
                                                 tformat=tformat,
                                                 debug=debug-1)
                              res$xat <- ats$xat
                              res$yat <- ats$yat
                          }
                      }
                      drawTimeRange <- FALSE
                  } else if (which[w] %in% timeseries) {
                      ## time-series types
                      col <- if (colGiven) rep(col, length.out=nw) else rep("black", length.out=nw)
                      oceDebug(debug, "graph ", w, " is a timeseries\n", sep="")
                      ##par(mgp=mgp, mar=mar, cex=cex)
                      tlim <- range(x[["time", j]])
                      if (which[w] == 13) {
                          oceDebug(debug, "which[", w, "] == 13 (salinity)\n", sep="")
                          if (haveTimeImages) drawPalette(debug=debug-1)
                          ats <- oce.plot.ts(x[["time", j]], x[["salinity", j]],
                                             xlim=if (xlimGiven) xlim[w, ] else tlim,
                                             ylim=if (ylimGiven) ylim[w, ],
                                             xaxs="i",
                                             col=col[w],
                                             lwd=lwd[w],
                                             cex=1, cex.axis=1, cex.lab=1,
                                             main=main[w],
                                             ylab=resizableLabel("S"),
                                             type=type,
                                             mgp=mgp,
                                             mar=omar,
                                             drawTimeRange=drawTimeRange,
                                             tformat=tformat,
                                             debug=debug-1)
                      } else if (which[w] == 14) {
                          oceDebug(debug, "which[", w, "] == 14 (temperature)\n", sep="")
                          if (haveTimeImages) drawPalette(debug=debug-1, mai=mai.palette)
                          if (j == "diagnostic" && "temperatureDia" %in% names(x@data)) {
                              ats <- oce.plot.ts(x@data$timeDia, x@data$temperatureDia,
                                                 xlim=if (xlimGiven) xlim[w, ] else tlim,
                                                 ylim=if (ylimGiven) ylim[w, ],
                                                 xaxs="i",
                                                 col=col[w],
                                                 lwd=lwd[w],
                                                 cex=1, cex.axis=1, cex.lab=1,
                                                 main=main[w],
                                                 ylab=expression(paste("Diagnostic T [ ", degree, "C ]")),
                                                 type=type,
                                                 mgp=mgp,
                                                 mar=omar,
                                                 tformat=tformat,
                                                 debug=debug-1)
                          } else {
                              ats <- oce.plot.ts(x[["time", j]], x[["temperature", j]],
                                                 xlim=if (xlimGiven) xlim[w, ] else tlim,
                                                 ylim=if (ylimGiven) ylim[w, ],
                                                 xaxs="i",
                                                 col=col[w],
                                                 lwd=lwd[w],
                                                 cex=1, cex.axis=1, cex.lab=1,
                                                 main=main[w],
                                                 ylab=expression(paste("T [ ", degree, "C ]")),
                                                 type=type,
                                                 mgp=mgp,
                                                 mar=omar,
                                                 tformat=tformat,
                                                 debug=debug-1)
                          }
                      } else if (which[w] == 15) {
                          if (haveTimeImages) drawPalette(debug=debug-1, mai=mai.palette)
                          oceDebug(debug, "which[", w, "] == 15 (pressure)\n", sep="")
                          if (j == "diagnostic" && "pressureDia" %in% names(x@data)) {
                              ats <- oce.plot.ts(x@data$timeDia, x@data$pressureDia,
                                                 xlim=if (xlimGiven) xlim[w, ] else tlim,
                                                 ylim=if (ylimGiven) ylim[w, ],
                                                 xaxs="i",
                                                 col=col[w],
                                                 lwd=lwd[w],
                                                 cex=1, cex.axis=1, cex.lab=1,
                                                 main=main[w],
                                                 ylab="pDia",
                                                 type=type,
                                                 mgp=mgp,
                                                 mar=omar,
                                                 drawTimeRange=drawTimeRange,
                                                 tformat=tformat,
                                                 debug=debug-1)
                          } else {
                              oceDebug(debug, "about to do non-diagnostic pressure plot, with cex=", cex, ", par(\"cex\")=", par("cex"), ", nw=", nw, ", cex sent to oce.plots=", cex*(1-min(nw/8, 1/4)), "\n", sep="")
                              oceDebug(debug, vectorShow(mar))
                              oceDebug(debug, vectorShow(par("mar")))
                              oceDebug(debug, vectorShow(par("mai")))
                              oceDebug(debug, vectorShow(haveTimeImages))
                              oceDebug(debug, "time[1]=", format(x[["time",j]][1], "%Y-%m-%d %H:%M:%S"), "\n")
                              ats <- oce.plot.ts(x[["time", j]], x[["pressure", j]],
                                                 xlim=if (xlimGiven) xlim[w, ] else tlim,
                                                 ylim=if (ylimGiven) ylim[w, ],
                                                 xaxs="i",
                                                 col=col[w],
                                                 lwd=lwd[w],
                                                 cex=1, cex.axis=1, cex.lab=1,
                                                 main=main[w],
                                                 ylab=resizableLabel("p"),
                                                 type=type,
                                                 mgp=mgp,
                                                 mar=omar,
                                                 drawTimeRange=drawTimeRange,
                                                 tformat=tformat,
                                                 debug=debug-1)
                          }
                      } else if (which[w] == 16) {
                          if (haveTimeImages) drawPalette(debug=debug-1, mai=mai.palette)
                          if (j == "diagnostic" && "headingDia" %in% names(x@data)) {
                              ats <- oce.plot.ts(x@data$timeDia, x@data$headingDia,
                                                 xlim=if (xlimGiven) xlim[w, ] else tlim,
                                                 ylim=if (ylimGiven) ylim[w, ],
                                                 xaxs="i",
                                                 col=col[w],
                                                 lwd=lwd[w],
                                                 cex=1, cex.axis=1, cex.lab=1,
                                                 main=main[w],
                                                 ylab="headingDia",
                                                 type=type,
                                                 mgp=mgp,
                                                 mar=omar,
                                                 drawTimeRange=drawTimeRange,
                                                 tformat=tformat,
                                                 debug=debug-1)
                          } else {
                              ats <- oce.plot.ts(x[["time", j]], x[["heading", j]],
                                                 xlim=if (xlimGiven) xlim[w, ] else tlim,
                                                 ylim=if (ylimGiven) ylim[w, ],
                                                 xaxs="i",
                                                 col=col[w],
                                                 lwd=lwd[w],
                                                 cex=1, cex.axis=1, cex.lab=1,
                                                 main=main[w],
                                                 ylab=resizableLabel("heading"),
                                                 type=type,
                                                 mgp=mgp,
                                                 mar=omar,
                                                 drawTimeRange=drawTimeRange,
                                                 tformat=tformat,
                                                 debug=debug-1)
                          }
                      } else if (which[w] == 17) {
                          if (haveTimeImages) drawPalette(debug=debug-1, mai=mai.palette)
                          if (j == "diagnostic" && "pitchDia" %in% names(x@data)) {
                              ats <- oce.plot.ts(x@data$timeDia, x@data$pitchDia,
                                                 xlim=if (xlimGiven) xlim[w, ] else tlim,
                                                 ylim=if (ylimGiven) ylim[w, ],
                                                 xaxs="i",
                                                 col=col[w],
                                                 lwd=lwd[w],
                                                 cex=1, cex.axis=1, cex.lab=1,
                                                 main=main[w],
                                                 ylab="pitchDia",
                                                 type=type,
                                                 mgp=mgp,
                                                 mar=omar,
                                                 drawTimeRange=drawTimeRange,
                                                 tformat=tformat,
                                                 debug=debug-1)
                          } else {
                              ats <- oce.plot.ts(x[["time", j]], x[["pitch", j]],
                                                 xlim=if (xlimGiven) xlim[w, ] else tlim,
                                                 ylim=if (ylimGiven) ylim[w, ],
                                                 xaxs="i",
                                                 col=col[w],
                                                 lwd=lwd[w],
                                                 cex=1, cex.axis=1, cex.lab=1,
                                                 main=main[w],
                                                 ylab=resizableLabel("pitch"),
                                                 type=type,
                                                 mgp=mgp,
                                                 mar=omar,
                                                 drawTimeRange=drawTimeRange,
                                                 tformat=tformat,
                                                 debug=debug-1)
                          }
                      } else if (which[w] == 18) {
                          if (haveTimeImages) drawPalette(debug=debug-1, mai=mai.palette)
                          if (j == "diagnostic" && "rollDia" %in% names(x@data)) {
                              ats <- oce.plot.ts(x@data$timeDia, x@data$rollDia,
                                                 xlim=if (xlimGiven) xlim[w, ] else tlim,
                                                 ylim=if (ylimGiven) ylim[w, ],
                                                 xaxs="i",
                                                 col=col[w],
                                                 lwd=lwd[w],
                                                 cex=1, cex.axis=1, cex.lab=1,
                                                 main=main[w],
                                                 ylab="rollDia",
                                                 type=type,
                                                 mgp=mgp,
                                                 mar=omar,
                                                 drawTimeRange=drawTimeRange,
                                                 tformat=tformat,
                                                 debug=debug-1)
                          } else {
                              ats <- oce.plot.ts(x[["time", j]], x[["roll", j]],
                                                 xlim=if (xlimGiven) xlim[w, ] else tlim,
                                                 ylim=if (ylimGiven) ylim[w, ],
                                                 xaxs="i",
                                                 col=col[w],
                                                 lwd=lwd[w],
                                                 cex=1, cex.axis=1, cex.lab=1,
                                                 main=main[w],
                                                 ylab=resizableLabel("roll"),
                                                 type=type,
                                                 mgp=mgp,
                                                 mar=omar,
                                                 drawTimeRange=drawTimeRange,
                                                 tformat=tformat,
                                                 debug=debug-1)
                          }
                      } else if (which[w] == 19) {
                          if (x[["numberOfBeams"]] > 0) {
                              if (haveTimeImages) drawPalette(debug=debug-1, mai=mai.palette)
                              ats <- oce.plot.ts(x[["time", j]], apply(x[["v", j]][, , 1], 1, mean, na.rm=TRUE),
                                                 xlim=if (xlimGiven) xlim[w, ] else tlim,
                                                 ylim=if (ylimGiven) ylim[w, ],
                                                 xaxs="i",
                                                 col=col[w],
                                                 lwd=lwd[w],
                                                 cex=1, cex.axis=1, cex.lab=1,
                                                 main=main[w],
                                                 ylab=beamName(x, 1),
                                                 type=type,
                                                 mgp=mgp,
                                                 mar=omar,
                                                 #mai.palette=mai.palette,
                                                 drawTimeRange=drawTimeRange,
                                                 tformat=tformat,
                                                 debug=debug-1)
                          } else {
                              stop("In plot,adp-method() : cannot plot beam/velo 1 because the device no beams", call.=FALSE)
                          }
                      } else if (which[w] == 20) {
                          if (x[["numberOfBeams"]] > 1) {
                              if (haveTimeImages) drawPalette(debug=debug-1, mai=mai.palette)
                              ats <- oce.plot.ts(x[["time", j]], apply(x[["v", j]][, , 2], 1, mean, na.rm=TRUE),
                                                 xlim=if (xlimGiven) xlim[w, ] else tlim,
                                                 ylim=if (ylimGiven) ylim[w, ],
                                                 xaxs="i",
                                                 col=col[w],
                                                 lwd=lwd[w],
                                                 cex=1, cex.axis=1, cex.lab=1,
                                                 main=main[w],
                                                 ylab=beamName(x, 2),
                                                 type=type,
                                                 mgp=mgp,
                                                 mar=omar,
                                                 #mai.palette=mai.palette,
                                                 drawTimeRange=drawTimeRange,
                                                 tformat=tformat,
                                                 debug=debug-1)
                          } else {
                              stop("In plot,adp-method() : cannot plot beam/velo 2 because the device has only ", x[["numberOfBeams"]], " beams", call.=FALSE)
                          }
                      } else if (which[w] == 21) {
                          if (x[["numberOfBeams"]] > 2) {
                              if (haveTimeImages) drawPalette(debug=debug-1, mai=mai.palette)
                              ats <- oce.plot.ts(x[["time", j]], apply(x[["v", j]][, , 3], 1, mean, na.rm=TRUE),
                                                 xlim=if (xlimGiven) xlim[w, ] else tlim,
                                                 ylim=if (ylimGiven) ylim[w, ],
                                                 xaxs="i",
                                                 col=col[w],
                                                 lwd=lwd[w],
                                                 cex=1, cex.axis=1, cex.lab=1,
                                                 main=main[w],
                                                 ylab=beamName(x, 3),
                                                 type=type,
                                                 mgp=mgp,
                                                 mar=omar,
                                                 #mai.palette=mai.palette,
                                                 drawTimeRange=drawTimeRange,
                                                 tformat=tformat,
                                                 debug=debug-1)
                          } else {
                              stop("In plot,adp-method() : cannot plot beam/velo 3 because the device has only", x[["numberOfBeams"]], "beams", call.=FALSE)
                          }
                      } else if (which[w] == 22) {
                          if (x[["numberOfBeams"]] > 3) {
                              if (haveTimeImages) drawPalette(debug=debug-1, mai=mai.palette)
                              ats <- oce.plot.ts(x[["time", j]], apply(x[["v", j]][, , 4], 1, mean, na.rm=TRUE),
                                                 xlim=if (xlimGiven) xlim[w, ] else tlim,
                                                 ylim=if (ylimGiven) ylim[w, ],
                                                 xaxs="i",
                                                 col=col[w],
                                                 lwd=lwd[w],
                                                 cex=1, cex.axis=1, cex.lab=1,
                                                 main=main[w],
                                                 ylab=beamName(x, 4),
                                                 type=type,
                                                 mgp=mgp,
                                                 mar=omar,
                                                 #mai.palette=mai.palette,
                                                 drawTimeRange=drawTimeRange,
                                                 tformat=tformat,
                                                 debug=debug-1)
                          } else {
                              stop("In plot,adp-method() : cannot plot beam/velo 4 because the device has only", x[["numberOfBeams"]], "beams", call.=FALSE)
                          }
                      } else  if (which[w] == 55) {
                          ## heaving
                          if (haveTimeImages) drawPalette(debug=debug-1, mai=mai.palette)
                          dt <- as.numeric(x[["time"]][2]) - as.numeric(x[["time"]][1])
                          ats <- oce.plot.ts(x[["time", j]], dt * cumsum(apply(x[["v", j]][, , 3], 1, mean, na.rm=TRUE)),
                                             xlim=if (xlimGiven) xlim[w, ] else tlim,
                                             ylim=if (ylimGiven) ylim[w, ],
                                             xaxs="i",
                                             col=col[w],
                                             lwd=lwd[w],
                                             cex=1, cex.axis=1, cex.lab=1,
                                             main=main[w],
                                             ylab="Heaving [m]",
                                             type=type,
                                             mgp=mgp,
                                             mar=omar,
                                             #mai.palette=mai.palette,
                                             drawTimeRange=drawTimeRange,
                                             tformat=tformat,
                                             debug=debug-1)
                          drawTimeRange <- FALSE
                      } else if (which[w] == 100) {
                          oceDebug(debug, "draw(ctd, ...) of type 'soundSpeed'\n")
                          if (haveTimeImages) drawPalette(debug=debug-1, mai=mai.palette)
                          ats <- oce.plot.ts(x[["time", j]], x[["soundSpeed", j]],
                                             xlim=if (xlimGiven) xlim[w, ] else tlim,
                                             ylim=if (ylimGiven) ylim[w, ],
                                             xaxs="i",
                                             col=col[w],
                                             lwd=lwd[w],
                                             cex=1, cex.axis=1, cex.lab=1,
                                             main=main[w],
                                             ylab="Sound Speed [m/s]",
                                             type=type,
                                             mgp=mgp,
                                             mar=omar,
                                             tformat=tformat,
                                             debug=debug-1)
                      } else if (which[w] %in% 40:44) {
                          ## bottomRange
                          par(mar=c(mgp[1]+1, mgp[1]+1, 1, 1))
                          n <- prod(dim(x[["v"]])[1:2])
                          if ("br" %in% names(x@data)) {
                              if (which[w] == 40) {
                                  R <- apply(x@data$br, 1, mean, na.rm=TRUE)
                                  ats <- oce.plot.ts(x[["time", j]], R,
                                                     ylab="Bottom range [m]",
                                                     type=type,
                                                     xlim=if (xlimGiven) xlim[w, ] else tlim,
                                                     ylim=if (ylimGiven) ylim[w, ] else range(R, na.rm=TRUE),
                                                     cex=1, cex.axis=1, cex.lab=1,
                                                     tformat=tformat,
                                                     mar=omar,
                                                     debug=debug-1)
                              } else {
                                  R <- x@data$br[, which[w]-40]
                                  ats <- oce.plot.ts(x[["time"]], R,
                                                     ylab=paste("Beam", which[w]-40, "bottom range [m]"),
                                                     type=type,
                                                     xlim=if (xlimGiven) xlim[w, ] else tlim,
                                                     ylim=if (ylimGiven) ylim[w, ] else range(R, na.rm=TRUE),
                                                     cex=1, cex.axis=1, cex.lab=1,
                                                     tformat=tformat,
                                                     mar=omar,
                                                     debug=debug-1)
                              }
                          } else {
                              stop("In plot,adp-method() : ADP object lacks bottom-tracking data, so which=40:44 and which=\"bottomRange[*]\" cannot work", call.=FALSE)
                          }
                      } else if (which[w] %in% 50:54) {
                          ## bottom velocity
                          par(mar=c(mgp[1]+1, mgp[1]+1, 1, 1))
                          n <- prod(dim(x[["v"]])[1:2])
                          if ("bv" %in% names(x@data)) {
                              if (which[w] == 50) {
                                  V <- apply(x@data$bv, 1, mean, na.rm=TRUE)
                                  ats <- oce.plot.ts(x[["time"]], V,
                                                     ylab="Bottom speed [m/s]",
                                                     type=type,
                                                     xlim=if (xlimGiven) xlim[w, ] else tlim,
                                                     ylim=if (ylimGiven) ylim[w, ] else range(V, na.rm=TRUE),
                                                     tformat=tformat,
                                                     cex=1, cex.axis=1, cex.lab=1,
                                                     mar=omar,
                                                     debug=debug-1)
                              } else {
                                  V <- x@data$bv[, which[w]-50]
                                  ats <- oce.plot.ts(x[["time"]], V,
                                                     ylab=paste("Beam", which[w]-50, "bottom velocity [m/s]"),
                                                     type=type,
                                                     xlim=if (xlimGiven) xlim[w, ] else tlim,
                                                     ylim=if (ylimGiven) ylim[w, ] else range(V, na.rm=TRUE),
                                                     tformat=tformat,
                                                     cex=1, cex.axis=1, cex.lab=1,
                                                     mar=omar,
                                                     debug=debug-1)
                              }
                          } else {
                              stop("In plot,adp-method() : ADP object lacks bottom-tracking data, so which=50:54 and which=\"bottomVelocity[*]\" cannot work", call.=FALSE)
                          }
                      }

                      ## FIXME delete the next block, after testing.
                      if (marginsAsImage && useLayout)  {
                          ## FIXME: I think this should be deleted
                          ## blank plot, to get axis length same as for images
                          omar <- par("mar")
                          par(mar=c(mar[1], 1/4, mgp[2]+1/2, mgp[2]+1))
                          plot(1:2, 1:2, type='n', axes=FALSE, xlab="", ylab="", cex=1, cex.axis=1, cex.lab=1)
                          par(mar=omar)
                      }
                  } else if (which[w] %in% spatial) {
                      ## various spatial types
                      if (which[w] == 23) {
                          ## progressive vector
                          par(mar=c(mgp[1]+1, mgp[1]+1, 1, 1))
                          if (j == 'diagnostic')
                              dt <- as.numeric(difftime(x@data$timeDia[2], x@data$timeDia[1], units="sec")) # FIXME: should not assume all equal
                          else
                              dt <- as.numeric(difftime(x[["time"]][2], x[["time"]][1], units="sec")) # FIXME: should not assume all equal
                          mPerKm <- 1000
                          if (j == 'diagnostic') {
                              U <- x@data$vDia[, 1, 1]
                              V <- x@data$vDia[, 1, 2]
                              ttt <- x@data$timeDia
                          } else {
                              U <- x[["v", j]][, , 1]
                              V <- x[["v", j]][, , 2]
                              ttt <- x[["time", j]]
                          }
                          if (!missing(control) && !is.null(control$bin)) {
                              if (control$bin < 1)
                                  stop("In plot,adp-method() : cannot have control$bin less than 1, but got ", control$bin, call.=FALSE)
                              max.bin <- dim(x[["v"]])[2]
                              if (control$bin > max.bin)
                                  stop("In plot,adp-method() : cannot have control$bin larger than ", max.bin, " but got ", control$bin, call.=FALSE)
                              u <- U[, control$bin] #EAC: bug fix, attempt to subset 2D matrix by 3 dimensions
                              v <- V[, control$bin]
                          } else {
                              if (x[["numberOfCells", j]] > 1) {
                                  u <- apply(U, 1, mean, na.rm=TRUE)
                                  v <- apply(V, 1, mean, na.rm=TRUE)
                              } else {
                                  u <- U
                                  v <- V
                              }
                          }
                          u[is.na(u)] <- 0        # zero out missing
                          v[is.na(v)] <- 0
                          xDist <- integrateTrapezoid(ttt, u, 'cA') / mPerKm
                          yDist<- integrateTrapezoid(ttt, v, 'cA') / mPerKm
                          plot(xDist, yDist, xlab="km", ylab="km", type='l', asp=1,
                               col=if (colGiven) col else "black",
                               cex=1, cex.axis=1, cex.lab=1,
                               ...)
                          xaxp <- par("xaxp")
                          xat <- seq(xaxp[1], xaxp[2], length.out=1+xaxp[3])
                          yaxp <- par("yaxp")
                          yat <- seq(yaxp[1], yaxp[2], length.out=1+yaxp[3])
                          ats <- list(xat=xat, yat=yat)
                      } else if (which[w] %in% 24:27) {
                          par(mar=c(mgp[1]+1, mgp[1]+1, 1, 1))
                          if (which[w] == 27 && x[["numberOfBeams"]] < 4) {
                              stop("In plot,adp-method() : cannot use which=27 for a 3-beam instrument", call.=FALSE)
                          } else {
                              value <- apply(x[["v", j]][, , which[w]-23], 2, mean, na.rm=TRUE)
                              yy <- x[["distance", j]]
                              if (ytype == "profile" && x@metadata$orientation[1] == "downward" && !ylimGiven) {
                                  plot(value, yy, xlab=beamName(x, which[w]-23),
                                       ylab=resizableLabel("distance"), type='l', ylim=rev(range(yy)),
                                       cex=1, cex.axis=1, cex.lab=1,
                                       ...)
                              } else {
                                  plot(value, yy, xlab=beamName(x, 1),
                                       ylab=resizableLabel("distance"), type='l',
                                       cex=1, cex.axis=1, cex.lab=1,
                                       ...)
                              }
                              xaxp <- par("xaxp")
                              xat <- seq(xaxp[1], xaxp[2], length.out=1+xaxp[3])
                              yaxp <- par("yaxp")
                              yat <- seq(yaxp[1], yaxp[2], length.out=1+yaxp[3])
                              ats <- list(xat=xat, yat=yat)
                          }
                      }
                  } else if (which[w] %in% 28:30) {
                      ## "uv", "uv+ellipse", or "uv+ellipse+arrow"
                      par(mar=c(mgp[1]+1, mgp[1]+1, 1, 1))
                      n <- dim(x[["v", j]])[1]
                      if (!missing(control) && !is.null(control$bin)) {
                          if (control$bin < 1)
                              stop("In plot,adp-method() : cannot have control$bin less than 1, but got ", control$bin, call.=FALSE)
                          max.bin <- dim(x[["v"]])[2]
                          if (control$bin > max.bin)
                              stop("In plot,adp-method() : cannot have control$bin larger than ", max.bin, " but got ", control$bin, call.=FALSE)
                          u <- x[["v", j]][, control$bin, 1]
                          v <- x[["v", j]][, control$bin, 2]
                      } else {
                          if (x[["numberOfCells", j]] > 1) {
                              u <- apply(x[["v"]][, , 1], 1, mean, na.rm=TRUE)
                              v <- apply(x[["v"]][, , 2], 1, mean, na.rm=TRUE)
                          } else {
                              u <- x[["v", j]][, 1, 1]
                              v <- x[["v", j]][, 1, 2]
                          }
                      }
                      oceDebug(debug, "uv type plot\n")
                      if (n < 5000 || (!missing(useSmoothScatter) && !useSmoothScatter)) {
                          if ("type" %in% names(dots)) {
                              plot(u, v,
                                   xlab=resizableLabel("u"),
                                   ylab=resizableLabel("v"),
                                   asp=1, col=if (colGiven) col else "black",
                                   xlim=if (xlimGiven) xlim[w, ] else range(u, na.rm=TRUE),
                                   ylim=if (ylimGiven) ylim[w, ] else range(v, na.rm=TRUE),
                                   cex=1, cex.axis=1, cex.lab=1,
                                   ...)
                          } else {
                              plot(u, v,
                                   xlab=resizableLabel("u"),
                                   ylab=resizableLabel("v"),
                                   type='n', asp=1,
                                   xlim=if (xlimGiven) xlim[w, ] else range(u, na.rm=TRUE),
                                   ylim=if (ylimGiven) ylim[w, ] else range(v, na.rm=TRUE),
                                   cex=1, cex.axis=1, cex.lab=1,
                                   ...)
                              points(u, v, cex=cex/2, col=if (colGiven) col else "black")
                          }
                      } else {
                          smoothScatter(u, v,
                                        xlab=resizableLabel("u"),
                                        ylab=resizableLabel("v"),
                                        asp=1,
                                        xlim=if (xlimGiven) xlim[w, ] else range(u, na.rm=TRUE),
                                        ylim=if (ylimGiven) ylim[w, ] else range(v, na.rm=TRUE),
                                        cex=1, cex.axis=1, cex.lab=1,
                                        ...)
                      }
                      xaxp <- par("xaxp")
                      xat <- seq(xaxp[1], xaxp[2], length.out=1+xaxp[3])
                      yaxp <- par("yaxp")
                      yat <- seq(yaxp[1], yaxp[2], length.out=1+yaxp[3])
                      ats <- list(xat=xat, yat=yat)

                      if (main[w] != "") {
                          oceDebug(debug, "about to title the plot with character size ", cex.lab*par("cex"), "\n")
                          mtext(main[w], adj=1, cex=cex.lab*par("cex"))
                      }
                      if (which[w] >= 29 && which[w] < 40) {
                          ok <- !is.na(u) & !is.na(v)
                          e <- eigen(cov(data.frame(u[ok], v[ok])))
                          major <- sqrt(e$values[1])  # major
                          minor <- sqrt(e$values[2])  # minor
                          theta <- seq(0, 2*pi, length.out=360/5)
                          xx <- major * cos(theta)
                          yy <- minor * sin(theta)
                          theta0 <- atan2(e$vectors[2, 1], e$vectors[1, 1])
                          ##cat("major", major, "minor", minor, "theta0", theta0, "\n")
                          rotate <- rbind(c(cos(theta0), -sin(theta0)),
                                          c(sin(theta0), cos(theta0)))
                          xxyy <- rotate %*% rbind(xx, yy)
                          col <- if (colGiven) col else "black"
                          lines(xxyy[1, ], xxyy[2, ], lwd=4, col="white")
                          lines(xxyy[1, ], xxyy[2, ], lwd=2, col=col)
                          res$ellipseMajor <- major
                          res$ellipseMinor <- minor
                          res$ellipseAngle <- theta
                          if (which[w] >= 30) {
                              if (!missing(control) && !is.null(control$bin)) {
                                  if (control$bin < 1)
                                      stop("In plot,adp-method() : cannot have control$bin less than 1, but got ", control$bin, call.=FALSE)
                                  max.bin <- dim(x[["v"]])[2]
                                  if (control$bin > max.bin)
                                      stop("In plot,adp-method() : cannot have control$bin larger than ", max.bin, " but got ", control$bin, call.=FALSE)
                                  umean <- mean(x[["v", j]][, control$bin, 2], na.rm=TRUE)
                                  vmean <- mean(x[["v", j]][, control$bin, 2], na.rm=TRUE)
                              } else {
                                  umean <- mean(x[["v", j]][, , 1], na.rm=TRUE)
                                  vmean <- mean(x[["v", j]][, , 2], na.rm=TRUE)
                              }
                              res$meanU <- umean
                              res$meanV <- vmean
                              arrows(0, 0, umean, vmean, lwd=4, length=1/10, col="white")
                              arrows(0, 0, umean, vmean, lwd=2, length=1/10, col=col)
                          }
                      }
                  } else if (which[w] == 60) {
                      oceDebug(debug, "draw(adp, ...) of type MAP\n")
                      ## get coastline file
                      if (is.character(coastline)) {
                          if (coastline == "none") {
                              if (!is.null(x@metadata$station) && !is.na(x@metadata$station)) {
                                  plot(x@metadata$longitude, x@metadata$latitude, xlab="", ylab="",
                                       cex=1, cex.axis=1, cex.lab=1)
                              } else {
                                  stop("In plot,adp-method() : no latitude or longitude in object's metadata, so cannot draw map", call.=FALSE)
                              }
                          } else {
                              ## named coastline
                              if (!exists(paste("^", coastline, "$", sep=""))) {
                                  ## load it, if necessary
                                  if (requireNamespace("ocedata", quietly=TRUE)) {
                                      if (coastline == "best") {
                                          best <- coastlineBest(span=span, debug=debug-1)
                                          data(list=best, package="oce", envir=environment())
                                          coastline <- get(best)
                                      } else if (coastline == "coastlineWorld") {
                                          data("coastlineWorld", package="oce", envir=environment())
                                          coastline <- get("coastlineWorld")
                                      } else if (coastline == "coastlineWorldFine") {
                                          data("coastlineWorldFine", package="ocedata", envir=environment())
                                          coastline <- get("coastlineWorldFine")
                                      } else if (coastline == "coastlineWorldMedium") {
                                          data("coastlineWorldMedium", package="ocedata", envir=environment())
                                          coastline <- get("coastlineWorldMedium")
                                      }  else {
                                          stop("there is no built-in coastline file of name \"", coastline, "\"")
                                      }
                                  } else {
                                      data("coastlineWorld", package="oce", envir=environment())
                                      coastline <- get("coastlineWorld")
                                  }
                              }
                          }
                          ## FIXME: span should be an arg
                          if ("firstLatitude" %in% names(x@data)) {
                              lat <- x[["firstLatitude"]]
                              lon <- x[["firstLongitude"]]
                              ##asp <- 1 / cos(mean(lat, na.rm=TRUE) * pi / 180)
                              plot(coastline, clatitude=mean(lat, na.rm=TRUE),
                                   clongitude=mean(lon, na.rm=TRUE),
                                   span=span,
                                   cex=1, cex.axis=1, cex.lab=1)
                              points(lon, lat)
                          } else if ("latitude" %in% names(x@metadata)) {
                              lat <- x[["latitude"]]
                              lon <- x[["longitude"]]
                              if (is.finite(lat) && is.finite(lon)) {
                                  plot(coastline, clatitude=lat, clongitude=lon, span=50,
                                       cex=1, cex.axis=1, cex.lab=1)
                                  points(x[["longitude"]], x[["latitude"]], cex=2*par('cex'))
                              } else {
                                  stop("In plot,adp-method() : nothing to map", call.=FALSE)
                              }
                          } else {
                              stop("In plot,adp-method() : nothing to map", call.=FALSE)
                          }
                      }
                  } else {
                      stop("In plot,adp-method() : unknown value of which (", which[w], ")", call.=FALSE)
                  }
                  if (is.logical(grid[1]) && grid[1])
                      grid(col=grid.col, lty=grid.lty, lwd=grid.lwd)
                  oceDebug(debug, "plot,adp-method bottom of loop, before reseting par('mar'):\n")
                  oceDebug(debug, vectorShow(par("mar")))
                  oceDebug(debug, vectorShow(par("mai")))
                  par(mar=omar)        # prevent margin creep if we have non-images after images (issue 1632 item 2)
                  oceDebug(debug, "...after reseting par('mar').\n")
                  oceDebug(debug, vectorShow(par("mar")))
                  oceDebug(debug, vectorShow(par("mai")))
              }
              par(cex=opar$cex, cex.axis=opar$cex.axis, cex.lab=opar$cex.lab)
              if (exists("ats")) {
                  res$xat <- ats$xat
                  res$yat <- ats$yat
              }
              oceDebug(debug, "} # plot,adp-method()\n", unindent=1, style="bold")
              invisible(res)
          })



#' Convert an ADP Object to ENU Coordinates
#'
#' @param x an [adp-class] object.
#'
#' @param declination magnetic declination to be added to the heading, to get
#' ENU with N as "true" north.
#'
#' @template debugTemplate
#'
#' @author Dan Kelley
#'
#' @seealso See [read.adp()] for notes on functions relating to
#' `"adp"` objects.  Also, see [beamToXyzAdp()] and
#' [xyzToEnuAdp()].
#'
#' @references
#' 1. @template nortekCoordTemplate
#'
#' @family things related to adp data
toEnuAdp <- function(x, declination=0, debug=getOption("oceDebug"))
{
    debug <- if (debug > 0) 1 else 0
    oceDebug(debug, "toEnuAdp() {\n", unindent=1)
    coord <- x[["oceCoordinate"]]
    if (coord == "beam") {
        x <- xyzToEnuAdp(beamToXyzAdp(x, debug=debug-1), declination=declination, debug=debug-1)
    } else if (coord == "xyz") {
        x <- xyzToEnuAdp(x, declination=declination, debug=debug-1)
    } else if (coord == "sfm") {
        x <- xyzToEnuAdp(x, declination=declination, debug=debug-1)
    } else if (coord == "enu") {
        warning("toEnuAdp cannot convert, object is already in cooordinate system ENU, returning argument as-is")
    } else {
        warning("toEnuAdp cannot convert from coordinate system ", coord, " to ENU, so returning argument as-is")
    }
    oceDebug(debug, "} # toEnuAdp()\n", unindent=1)
    x
}


#' Adjust ADP Signal for Spherical Spreading
#'
#' Compensate ADP signal strength for spherical spreading.
#'
#' First, beam echo intensity is converted from counts to decibels, by
#' multiplying by `count2db`.  Then, the signal decrease owing to
#' spherical spreading is compensated for by adding the term
#' \eqn{20\log10(r)}{20*log10(r)}, where \eqn{r}{r} is the distance from the
#' sensor head to the water from which scattering is occurring.  \eqn{r}{r} is
#' given by `x[["distance"]]`.
#'
#' @param x an [adp-class] object.
#'
#' @param count2db a set of coefficients, one per beam, to convert from beam
#' echo intensity to decibels.
#'
#' @param asMatrix a boolean that indicates whether to return a numeric matrix,
#' as opposed to returning an updated object (in which the matrix is cast to a
#' raw value).
#'
#' @template debugTemplate
#'
#' @return An [adp-class] object.
#'
#' @author Dan Kelley
#'
#' @references The coefficient to convert to decibels is a personal
#' communication.  The logarithmic term is explained in textbooks on acoustics,
#' optics, etc.
#'
#' @examples
#' library(oce)
#' data(adp)
#' plot(adp, which=5) # beam 1 echo intensity
#' adp.att <- beamUnspreadAdp(adp)
#' plot(adp.att, which=5) # beam 1 echo intensity
#' ## Profiles
#' par(mar=c(4, 4, 1, 1))
#' a <- adp[["a", "numeric"]]             # second arg yields matrix return value
#' distance <- adp[["distance"]]
#' plot(apply(a,2,mean), distance, type='l', xlim=c(0,256))
#' lines(apply(a,2,median), distance, type='l',col='red')
#' legend("topright",lwd=1,col=c("black","red"),legend=c("original","attenuated"))
#' ## Image
#' plot(adp.att, which="amplitude",col=oce.colorsViridis(100))
#'
#' @family things related to adp data
beamUnspreadAdp <- function(x, count2db=c(0.45, 0.45, 0.45, 0.45), asMatrix=FALSE, debug=getOption("oceDebug"))
{
    oceDebug(debug, "beamUnspreadAdp(...) {\n", unindent=1)
    if (!inherits(x, "adp"))
        stop("method is only for objects of class '", "adp", "'")
    ## make compatible with old function name (will remove in Jan 2013)
    if (!is.null(x@metadata$oceBeamUnattenuated) && x@metadata$oceBeamUnattenuated) {
        warning("the beams are already unspreaded in this dataset.")
        return(x)
    }
    if (!is.null(x@metadata$oceBeamUnspreaded) && x@metadata$oceBeamUnspreaded) {
        warning("the beams are already unspreaded in this dataset")
        return(x)
    }
    numberOfProfiles <- dim(x@data$a)[1]
    oceDebug(debug, "numberOfProfiles=", numberOfProfiles, "\n")
    correction <- matrix(rep(20 * log10(x[["distance"]]), numberOfProfiles),
                         nrow=numberOfProfiles, byrow=TRUE)
    if (asMatrix) {
        res <- array(double(), dim=dim(x@data$a))
        for (beam in 1:x[["numberOfBeams"]]) {
            oceDebug(debug, "beam=", beam, "\n")
            res[, , beam] <- count2db[beam] * as.numeric(x@data$a[, , beam]) + correction
        }
    } else {
        res <- x
        for (beam in 1:x[["numberOfBeams"]]) {
            oceDebug(debug, "beam=", beam, "\n")
            tmp <- floor(count2db[beam] * as.numeric(x@data$a[, , beam]) + correction)
            tmp[tmp < 0] <- 0
            tmp[tmp > 255] <- 255
            res@data$a[, , beam] <- as.raw(tmp)
        }
        res@metadata$oceBeamUnspreaded <- TRUE
    }
    res@processingLog <- processingLogAppend(res@processingLog, paste(deparse(expr=match.call()), sep="", collapse=""))
    oceDebug(debug, "} # beamUnspreadAdp()\n", unindent=1)
    res
}


#' Convert ADP From Beam to XYZ Coordinates
#'
#' Convert ADP velocity components from a beam-based coordinate system to a
#' xyz-based coordinate system. The action depends on the type of object.
#' Objects creating by reading RDI Teledyne, Sontek, and some Nortek
#' instruments are handled directly. However, Nortek
#' data stored in in the AD2CP format are handled by the specialized
#' function [beamToXyzAdpAD2CP()], the documentation for which
#' should be consulted, rather than the material given blow.
#'
#' For a 3-beam Nortek `aquadopp` object, the beams are transformed into
#' velocities using the matrix stored in the header.
#'
#' For 4-beam objects (and for the slanted 4 beams of 5-beam
#' objects), the along-beam velocity components \eqn{B_1}{B1}
#' \eqn{B_2}{B1}, \eqn{B_3}{B3}, and \eqn{B_4}{B4}
#' are converted to Cartesian velocity components \eqn{u}{u}
#' \eqn{v}{v} and \eqn{w}{w}
#' using formulae from section 5.5 of *RD Instruments* (1998), viz. the
#' along-beam velocity components \eqn{B_1}{B1}, \eqn{B_2}{B2}, \eqn{B_3}{B3},
#' and \eqn{B_4}{B4} are used to calculate velocity components in a cartesian
#' system referenced to the instrument using the following formulae:
#' \eqn{u=ca(B_1-B_2)}{u=c*a*(B1-B2)}, \eqn{v=ca(B_4-B_3)}{v=c*a*(B4-B3)},
#' \eqn{w=-b(B_1+B_2+B_3+B_4)}{w=-b*(B1+B2+B3+B4)}. In addition to these,
#' an estimate of the
#' error in velocity is computed as
#' \eqn{e=d(B_1+B_2-B_3-B_4)}{e=d*(B1+B2-B3-B4)}.
#' The geometrical factors in these formulae are:
#' `c` is +1 for convex beam geometry or -1 for concave beam geometry,
#' \eqn{a=1/(2\sin\theta)}{a=1/(2*sin(theta))}
#' where \eqn{\theta}{theta} is the angle the beams make to the axial direction
#' (which is available as `x[["beamAngle"]]`),
#' \eqn{b=1/(4\cos\theta)}{b=1/(4*cos(theta))}, and
#' \eqn{d=a/\sqrt{2}}{d=a/sqrt(2)}.
#'
#' @param x an [adp-class] object.
#'
#' @template debugTemplate
#'
#' @return An object with the first 3 velocity indices having been altered to
#' represent velocity components in xyz (or instrument) coordinates.  (For
#' `rdi` data, the values at the 4th velocity index are changed to
#' represent the "error" velocity.)
#' To indicate the change, the value of `x[["oceCoordinate"]]` is
#' changed from `beam` to `xyz`.
#'
#' @author Dan Kelley
#'
#' @seealso See [read.adp()] for other functions that relate to
#' objects of class `"adp"`.
#'
#' @references
#' 1. Teledyne RD Instruments. \dQuote{ADCP Coordinate Transformation: Formulas and
#' Calculations,} January 2010. P/N 951-6079-00.
#'
#' 2. WHOI/USGS-provided Matlab code for beam-enu transformation
#' \samp{http://woodshole.er.usgs.gov/pubs/of2005-1429/MFILES/AQDPTOOLS/beam2enu.m}
#'
#' @family things related to adp data
beamToXyzAdp <- function(x, debug=getOption("oceDebug"))
{
    if (!inherits(x, "adp"))
        stop("method is only for objects of class \"adp\"")
    if (x[["oceCoordinate"]] != "beam")
        stop("input must be in beam coordinates")
    if (is.ad2cp(x)) {
        oceDebug(debug, "beamToXyzAdp(x, debug=", debug, ") {\n", sep="", unindent=1)
        res <- beamToXyzAdpAD2CP(x=x, debug=debug - 1)
        oceDebug(debug, "} # beamToXyzAdp()\n", unindent=1)
        return(res)
    }
    oceDebug(debug, "beamToXyzAdp(x, debug=", debug, ") {\n", sep="", unindent=1)
    nb <- x[["numberOfBeams"]]
    if (is.null(nb))
        stop("missing x[[\"numberOfBeams\"]]")
    tm <- x[["transformationMatrix"]]
    if (is.null(tm))
        stop("missing x[[\"transformationMatrix\"]]")
    if (!all.equal(dim(tm), c(nb, nb)))
        stop("number of beams, ", nb, ", contradicts the ", dim(tm)[1], "x", dim(tm)[2], " transformationMatrix")
    manufacturer <- x[["manufacturer"]]
    if (is.null(manufacturer))
        stop("cannot rotate the data, since there is no 'manufacturer' entry in the metadata slot")
    oceDebug(debug, "transformation matrix follows\n")
    if (debug > 0)
        print(tm)
    res <- x
    V <- x[["v"]]
    if (length(grep(".*rdi.*", manufacturer))) {
        if (nb != 4)
            stop("can only handle 4-beam ADP units from RDI")
        res@data$v[,,1] <- tm[1,1]*V[,,1] + tm[1,2]*V[,,2] + tm[1,3]*V[,,3] + tm[1,4]*V[,,4]
        res@data$v[,,2] <- tm[2,1]*V[,,1] + tm[2,2]*V[,,2] + tm[2,3]*V[,,3] + tm[2,4]*V[,,4]
        res@data$v[,,3] <- tm[3,1]*V[,,1] + tm[3,2]*V[,,2] + tm[3,3]*V[,,3] + tm[3,4]*V[,,4]
        res@data$v[,,4] <- tm[4,1]*V[,,1] + tm[4,2]*V[,,2] + tm[4,3]*V[,,3] + tm[4,4]*V[,,4]
        if ("bv" %in% names(x@data)) {
            ## bottom velocity
            V <- x@data$bv
            res@data$bv[,1] <- tm[1,1]*V[,1] + tm[1,2]*V[,2] + tm[1,3]*V[,3] + tm[1,4]*V[,4]
            res@data$bv[,2] <- tm[2,1]*V[,1] + tm[2,2]*V[,2] + tm[2,3]*V[,3] + tm[2,4]*V[,4]
            res@data$bv[,3] <- tm[3,1]*V[,1] + tm[3,2]*V[,2] + tm[3,3]*V[,3] + tm[3,4]*V[,4]
            res@data$bv[,4] <- tm[4,1]*V[,1] + tm[4,2]*V[,2] + tm[4,3]*V[,3] + tm[4,4]*V[,4]
        }
        res@metadata$oceCoordinate <- "xyz"
    } else if (length(grep(".*nortek.*", manufacturer))) {
        if (nb == 3) {
            res@data$v[,,1] <- tm[1,1]*V[,,1] + tm[1,2]*V[,,2] + tm[1,3]*V[,,3]
            res@data$v[,,2] <- tm[2,1]*V[,,1] + tm[2,2]*V[,,2] + tm[2,3]*V[,,3]
            res@data$v[,,3] <- tm[3,1]*V[,,1] + tm[3,2]*V[,,2] + tm[3,3]*V[,,3]
            if ("bv" %in% names(x@data)) {
                ## bottom velocity
                V <- x@data$bv
                res@data$bv[,1] <- tm[1,1]*V[,1] + tm[1,2]*V[,2] + tm[1,3]*V[,3]
                res@data$bv[,2] <- tm[2,1]*V[,1] + tm[2,2]*V[,2] + tm[2,3]*V[,3]
                res@data$bv[,3] <- tm[3,1]*V[,1] + tm[3,2]*V[,2] + tm[3,3]*V[,3]
            }
            res@metadata$oceCoordinate <- "xyz"
        } else if (nb == 4) {
            stop("the only 4-beam Nortek format supported is AD2CP")
        } else {
            stop("can only handle 3-beam and 4-beam ADP units from nortek")
        }
    } else if (length(grep(".*sontek.*", manufacturer))) {
        res@data$v[,,1] <- tm[1,1]*V[,,1] + tm[1,2]*V[,,2] + tm[1,3]*V[,,3]
        res@data$v[,,2] <- tm[2,1]*V[,,1] + tm[2,2]*V[,,2] + tm[2,3]*V[,,3]
        res@data$v[,,3] <- tm[3,1]*V[,,1] + tm[3,2]*V[,,2] + tm[3,3]*V[,,3]
        if ("bv" %in% names(x@data)) {
            ## bottom velocity
            V <- x@data$bv
            res@data$bv[,1] <- tm[1,1]*V[,1] + tm[1,2]*V[,2] + tm[1,3]*V[,3]
            res@data$bv[,2] <- tm[2,1]*V[,1] + tm[2,2]*V[,2] + tm[2,3]*V[,3]
            res@data$bv[,3] <- tm[3,1]*V[,1] + tm[3,2]*V[,2] + tm[3,3]*V[,3]
        }
        res@metadata$oceCoordinate <- "xyz"
    } else {
        stop("adp type must be either \"rdi\" or \"nortek\" or \"sontek\"")
    }
    res@processingLog <- processingLogAppend(res@processingLog, paste(deparse(expr=match.call()), sep="", collapse=""))
    oceDebug(debug, "} # beamToXyzAdp()\n", unindent=1)
    res
}

#' Convert AD2CP-style adp data From Beam to XYZ Coordinates
#'
#' This looks at all the items in the `data` slot of `x`, to
#' see if they contain an array named `v` that holds velocity.
#' If that velocity has 4 components, and if `oceCoordinate` for
#' the item is `"beam"`, then
#' along-beam velocity components \eqn{B_1}{B1}
#' \eqn{B_2}{B1}, \eqn{B_3}{B3}, and \eqn{B_4}{B4}
#' are converted to instrument-oriented Cartesian velocity components \eqn{u}{u}
#' \eqn{v}{v} and \eqn{w}{w}
#' using the convex-geometry formulae from section 5.5 of reference 1,
#' viz.
#' \eqn{u=ca(B_1-B_2)}{u=a*(B1-B2)}, \eqn{v=ca(B_4-B_3)}{v=a*(B4-B3)},
#' \eqn{w=-b(B_1+B_2+B_3+B_4)}{w=-b*(B1+B2+B3+B4)}. In addition to these,
#' an estimate of the
#' error in velocity is computed as
#' \eqn{e=d(B_1+B_2-B_3-B_4)}{e=d*(B1+B2-B3-B4)}.
#' The geometrical factors in these formulae are:
#' \eqn{a=1/(2\sin\theta)}{a=1/(2*sin(theta))}
#' where \eqn{\theta}{theta} is the angle the beams make to the axial direction
#' (which is available as `x[["beamAngle"]]`),
#' \eqn{b=1/(4\cos\theta)}{b=1/(4*cos(theta))}, and
#' \eqn{d=a/\sqrt{2}}{d=a/sqrt(2)}.
#'
#' @param x an [adp-class] object.
#'
#' @template debugTemplate
#'
#' @references
#' 1. Teledyne RD Instruments.
#' \dQuote{ADCP Coordinate Transformation: Formulas and Calculations,}
#' January 2010. P/N 951-6079-00.
#
#' @family things related to adp data
beamToXyzAdpAD2CP <- function(x, debug=getOption("oceDebug"))
{
    debug <- if (debug > 0) 1 else 0
    oceDebug(debug, "beamToXyzAdpAD2CP(x, debug=", debug, ") {\n", sep="", unindent=1)
    if (!inherits(x, "adp"))
        stop("method is only for objects of class \"adp\"")
    if (!is.ad2cp(x))
        stop("method is only for AD2CP objects")
    if (!is.ad2cp(x))
        stop("only 4-beam AD2CP data are handled")
    res <- x
    for (item in names(x@data)) {
        oceDebug(debug, "item='", item, "'...\n", sep="")
        ## Do not try to alter unsuitable items, e.g. the vertical beam, the altimeter, etc.
        if (is.list(x@data[[item]]) && "v" %in% names(x@data[[item]])) {
            if (x@data[[item]]$oceCoordinate == "beam") {
                numberOfBeams <- x@data[[item]]$numberOfBeams
                oceDebug(debug, "  numberOfBeams=", numberOfBeams, "\n")
                if (4 == numberOfBeams) {
                    v <- x@data[[item]]$v
                    ## Possibly speed things up by reducing need to index 4 times.
                    v1 <- v[,,1]
                    v2 <- v[,,2]
                    v3 <- v[,,3]
                    v4 <- v[,,4]
                    rm(v)              # perhaps help by reducing memory pressure a bit
                    beamAngle <- x@metadata$beamAngle
                    if (is.null(beamAngle))
                        stop("cannot look up beamAngle")
                    theta <- beamAngle * atan2(1, 1) / 45
                    TMc <- 1 # for convex (diverging) beam setup; use -1 for concave
                    TMa <- 1 / (2 * sin(theta))
                    TMb <- 1 / (4 * cos(theta))
                    TMd <- TMa / sqrt(2)
                    tm <- rbind(c(TMc*TMa, -TMc*TMa,        0,       0),
                                c(      0,        0, -TMc*TMa, TMc*TMa),
                                c(    TMb,      TMb,      TMb,     TMb),
                                c(    TMd,      TMd,     -TMd,    -TMd))
                    ## TIMING new way:
                    ## TIMING    user  system elapsed
                    ## TIMING  11.661  27.300  89.293
                    ## TIMING old way:
                    ## TIMING    user  system elapsed
                    ## TIMING  15.977  24.182  88.971
                    ## TIMING cat("new way:\n")
                    ## TIMING print(system.time({
                    ## TIMING     v1 <- V[,,1]
                    ## TIMING     v2 <- V[,,2]
                    ## TIMING     v3 <- V[,,3]
                    ## TIMING     v4 <- V[,,4]
                    ## TIMING     res@data[[j]]$v[,,1] <- tm[1,1]*v1 + tm[1,2]*v2 + tm[1,3]*v3 + tm[1,4]*v4
                    ## TIMING     res@data[[j]]$v[,,2] <- tm[2,1]*v1 + tm[2,2]*v2 + tm[2,3]*v3 + tm[2,4]*v4
                    ## TIMING     res@data[[j]]$v[,,3] <- tm[3,1]*v1 + tm[3,2]*v2 + tm[3,3]*v3 + tm[3,4]*v4
                    ## TIMING     res@data[[j]]$v[,,4] <- tm[4,1]*v1 + tm[4,2]*v2 + tm[4,3]*v3 + tm[4,4]*v4
                    ## TIMING     rm(v1, v2, v3, v4)
                    ## TIMING }))
                    ## TIMING cat("old way:\n")
                    ## TIMING print(system.time({
                    ## TIMING     res@data[[j]]$v[,,1] <- tm[1,1]*V[,,1] + tm[1,2]*V[,,2] + tm[1,3]*V[,,3] + tm[1,4]*V[,,4]
                    ## TIMING     res@data[[j]]$v[,,2] <- tm[2,1]*V[,,1] + tm[2,2]*V[,,2] + tm[2,3]*V[,,3] + tm[2,4]*V[,,4]
                    ## TIMING     res@data[[j]]$v[,,3] <- tm[3,1]*V[,,1] + tm[3,2]*V[,,2] + tm[3,3]*V[,,3] + tm[3,4]*V[,,4]
                    ## TIMING     res@data[[j]]$v[,,4] <- tm[4,1]*V[,,1] + tm[4,2]*V[,,2] + tm[4,3]*V[,,3] + tm[4,4]*V[,,4]
                    ## TIMING }))
                    res@data[[item]]$v[,,1] <- tm[1,1]*v1 + tm[1,2]*v2 + tm[1,3]*v3 + tm[1,4]*v4
                    res@data[[item]]$v[,,2] <- tm[2,1]*v1 + tm[2,2]*v2 + tm[2,3]*v3 + tm[2,4]*v4
                    res@data[[item]]$v[,,3] <- tm[3,1]*v1 + tm[3,2]*v2 + tm[3,3]*v3 + tm[3,4]*v4
                    res@data[[item]]$v[,,4] <- tm[4,1]*v1 + tm[4,2]*v2 + tm[4,3]*v3 + tm[4,4]*v4
                    res@data[[item]]$oceCoordinate <- "xyz"
                    res@metadata$oceCoordinate <- NULL # remove, just in case it got added by mistake
                    oceDebug(debug, "  converted from 'beam' to 'xyz'\n")
                } else {
                    oceDebug(debug, "  skipping, since not 4 beams\n")
                }
            } else {
                oceDebug(debug, "  skipping, since not in 'beam' coordinate\n")
            }
        } else {
            oceDebug(debug, "  skipping, since not a list\n")
        }
    }
    res@processingLog <- processingLogAppend(res@processingLog,
                                             paste("beamToXyzAdpAD2CP(x",
                                                   ", debug=", debug, ")", sep=""))
    oceDebug(debug, "} # beamToXyzAdpAD2CP()\n", unindent=1)
    res
}

#' Convert ADP From XYZ to ENU Coordinates
#'
#' Convert ADP velocity components from a xyz-based coordinate system to
#' an enu-based coordinate system, by using the instrument's recording of
#' information relating to heading, pitch, and roll. The action is based
#' on what is stored in the data, and so it depends greatly on instrument type
#' and the style of original data format. This function handles data from
#' RDI Teledyne, Sontek, and some Nortek instruments directly. However, Nortek
#' data stored in in the AD2CP format are handled by the specialized
#' function [xyzToEnuAdpAD2CP()], the documentation for which
#' should be consulted, rather than the material given blow.
#'
#' The first step is to convert the (x,y,z) velocity components (stored in the
#' three columns of `x[["v"]][,,1:3]`) into what RDI (reference 1, pages 11 and 12)
#' calls "ship" (or "righted") components.  For example, the z coordinate,
#' which may point upwards or downwards depending on instrument orientation, is
#' mapped onto a "mast" coordinate that points more nearly upwards than
#' downward.  The other ship coordinates are called "starboard" and "forward",
#' the meanings of which will be clear to mariners.  Once the (x,y,z)
#' velocities are converted to ship velocities, the orientation of the
#' instrument is extracted from heading, pitch, and roll vectors stored in the
#' object.  These angles are defined differently for RDI and Sontek profilers.
#'
#' The code handles every case individually, based on the table given below.
#' The table comes from Clark Richards, a former PhD student at Dalhousie
#' University (reference 2), who developed it based on instrument documentation,
#' discussion on user groups, and analysis of measurements acquired with RDI
#' and Sontek acoustic current profilers in the SLEIWEX experiment.  In the
#' table, (X, Y, Z) denote instrument-coordinate velocities, (S, F, M) denote
#' ship-coordinate velocities, and (H, P, R) denote heading, pitch, and roll.
#'
#' \tabular{rrrrrrrrrrrr}{ **Case** \tab **Mfr.** \tab
#' **Instr.** **Orient.** \tab **H** \tab **P** \tab
#' **R** \tab **S** \tab **F** \tab **M**\cr 1 \tab RDI
#' \tab ADCP \tab up \tab H \tab arctan(tan(P)*cos(R)) \tab R \tab -X \tab Y
#' \tab -Z\cr 2 \tab RDI \tab ADCP \tab down \tab H \tab arctan(tan(P)*cos(R))
#' \tab -R \tab X \tab Y \tab Z\cr 3 \tab Nortek \tab ADP \tab up \tab H-90
#' \tab R \tab -P \tab X \tab Y \tab Z\cr 4 \tab Nortek \tab ADP \tab down \tab
#' H-90 \tab R \tab -P \tab X \tab -Y \tab -Z\cr 5 \tab Sontek \tab ADP \tab up
#' \tab H-90 \tab -P \tab -R \tab X \tab Y \tab Z\cr 6 \tab Sontek \tab ADP
#' \tab down \tab H-90 \tab -P \tab -R \tab X \tab Y \tab Z\cr 7 \tab Sontek
#' \tab PCADP \tab up \tab H-90 \tab R \tab -P \tab X \tab Y \tab Z\cr 8 \tab
#' Sontek \tab PCADP \tab down \tab H-90 \tab R \tab -P \tab X \tab Y \tab Z\cr
#' }
#'
#' Finally, a standardized rotation matrix is used to convert from ship
#' coordinates to earth coordinates.  As described in the RDI coordinate
#' transformation manual (reference 1, pages 13 and 14), this matrix is based on sines
#' and cosines of heading, pitch, and roll If `CH` and `SH` denote
#' cosine and sine of heading (after adjusting for declination), with similar
#' terms for pitch and roll using second letters `P` and `R`, the
#' rotation matrix is
#'
#' \preformatted{ rbind(c( CH*CR + SH*SP*SR, SH*CP, CH*SR - SH*SP*CR), c(-SH*CR
#' + CH*SP*SR, CH*CP, -SH*SR - CH*SP*CR), c( -CP*SR, SP, CP*CR)) }
#'
#' This matrix is left-multiplied by a matrix with three rows, the top a vector
#' of "starboard" values, the middle a vector of "forward" values, and the
#' bottom a vector of "mast" values.  Finally, the columns of
#' `data$v[,,1:3]` are filled in with the result of the matrix
#' multiplication.
#'
#' @param x an [adp-class] object.
#'
#' @param declination magnetic declination to be added to the heading after
#' "righting" (see below), to get ENU with N as "true" north.
#'
#' @template debugTemplate
#'
#' @return An object with `data$v[,,1:3]` altered appropriately, and
#' `x[["oceCoordinate"]]` changed from `xyz` to `enu`.
#'
#' @author Dan Kelley and Clark Richards
#'
## @section Limitations:
## For AD2CP objects, created by[read.adp.ad2cp()],
## the transformation to ENU coordinates is only possible if the instrument
## orientation is `"AHRS"`. Other orientations may be added, if users
## indicat a need for them, and supply the developers with test file (including
## at least a few expected results).
#'
#' @references
#' 1. Teledyne RD Instruments. \dQuote{ADCP Coordinate Transformation: Formulas and Calculations,}
#' January 2010. P/N 951-6079-00.
#'
#' 2. Clark Richards, 2012, PhD Dalhousie University Department of
#' Oceanography.
#'
#' @family things related to adp data
xyzToEnuAdp <- function(x, declination=0, debug=getOption("oceDebug"))
{
    debug <- if (debug > 0) 1 else 0
    if (!inherits(x, "adp"))
        stop("method is only for objects of class '", "adp", "'")
    ## Treat AD2CP differently because e.g. if it has AHRS, then there is may be need or
    ## benefit in extracting heading, etc., as for the other cases. Also, the orientation
    ## names are different for this type, so isolating the code makes things clearer
    ## and easier to maintain.  (FIXME: consider splitting the RDI and Sontek cases, too.)
    if (is.ad2cp(x))
        return(xyzToEnuAdpAD2CP(x=x, declination=declination, debug=debug))
    oceDebug(debug, "xyzToEnuAdp(x, declination=", declination, ", debug=", debug, ") {\n", sep="", unindent=1)
    ## Now, address non-AD2CP cases.
    manufacturer <- x[["manufacturer"]]
    oceCoordinate = x[["oceCoordinate"]]
    orientation = x[["orientation"]][1]
    if (is.null(orientation)) {
        warning("instrument orientation is not stored in x; assuming it is \"upward\"")
        orientation <- "upward"
    }
    if (is.null(oceCoordinate) || (oceCoordinate != "xyz" & oceCoordinate != "sfm"))
        stop("input must be in xyz or sfm coordinates")
    heading <- x[["heading"]]
    pitch <- x[["pitch"]]
    roll <- x[["roll"]]
    res <- x
    isAD2CP <- is.ad2cp(x)
    haveBv <- "bv" %in% names(x@data)
    ## Case-by-case alteration of heading, pitch and roll, so we can use one formula for all.
    if (1 == length(agrep("rdi", manufacturer, ignore.case=TRUE))) {
        ## "teledyne rdi"
        ## h/p/r and s/f/m from Clark Richards pers. comm. 2011-03-14, revised 2011-03-15
        if (oceCoordinate == "sfm" & !res@metadata$tiltUsed) {
            oceDebug(debug, "Case 1: RDI ADCP in SFM coordinates.\n")
            oceDebug(debug, "        No coordinate changes required prior to ENU.\n")
            starboard <- res@data$v[, , 1] # p11 "RDI Coordinate Transformation Manual" (July 1998)
            forward <- res@data$v[, , 2] # p11 "RDI Coordinate Transformation Manual" (July 1998)
            mast <- res@data$v[, , 3] # p11 "RDI Coordinate Transformation Manual" (July 1998)
            if (haveBv) {
                ## bottom velocity
                starboardBv <- res@data$bv[, 1]
                forwardBv <- res@data$bv[, 2]
                mastBv <- res@data$bv[, 3]
            }
        } else if (oceCoordinate == "sfm" & res@metadata$tiltUsed) {
          oceDebug(debug, "Case 2: RDI ADCP in SFM coordinates, but with tilts already applied.\n")
          oceDebug(debug, "        No coordinate changes required prior to ENU.\n")
          starboard <- res@data$v[, , 1] # p11 "RDI Coordinate Transformation Manual" (July 1998)
          forward <- res@data$v[, , 2] # p11 "RDI Coordinate Transformation Manual" (July 1998)
          mast <- res@data$v[, , 3] # p11 "RDI Coordinate Transformation Manual" (July 1998)
          pitch <- rep(0, length(heading))
          roll <- rep(0, length(heading))
          if (haveBv) {
            ## bottom velocity
            starboardBv <- res@data$bv[, 1]
            forwardBv <- res@data$bv[, 2]
            mastBv <- res@data$bv[, 3]
          }
        } else if (orientation == "upward") {
            oceDebug(debug, "Case 3: RDI ADCP in XYZ coordinates with upward-pointing sensor.\n")
            oceDebug(debug, "        Using S=-X, F=Y, and M=-Z.\n")
            ## As an alternative to the next three lines, could just add 180 degrees to roll
            starboard <- -res@data$v[, , 1] # p11 "RDI Coordinate Transformation Manual" (July 1998)
            forward <- res@data$v[, , 2] # p11 "RDI Coordinate Transformation Manual" (July 1998)
            mast <- -res@data$v[, , 3] # p11 "RDI Coordinate Transformation Manual" (July 1998)
            if (haveBv) {
                ## bottom velocity
                starboardBv <- -res@data$bv[, 1]
                forwardBv <- res@data$bv[, 2]
                mastBv <- -res@data$bv[, 3]
            }
            oceDebug(debug, "  defined starboard, etc\n")
        } else if (orientation == "downward") {
            oceDebug(debug, "Case 4: RDI ADCP in XYZ coordinates with downward-pointing sensor.\n")
            oceDebug(debug, "        Using roll=-roll, S=X, F=Y, and M=Z.\n")
            roll <- -roll
            starboard <- res@data$v[, , 1] # p11 "RDI Coordinate Transformation Manual" (July 1998)
            forward <- res@data$v[, , 2] # p11 "RDI Coordinate Transformation Manual" (July 1998)
            mast <- res@data$v[, , 3] # p11 "RDI Coordinate Transformation Manual" (July 1998)
            if (haveBv) {
                ## bottom velocity
                starboardBv <- res@data$bv[, 1]
                forwardBv <- res@data$bv[, 2]
                mastBv <- res@data$bv[, 3]
            }
        } else {
            stop("need orientation='upward' or 'downward', not '", orientation, "'")
        }
    } else if (1 == length(agrep("nortek", manufacturer))) {
        V <- x[["v"]]
        if (orientation == "upward") {
            ## h/p/r and s/f/m from Clark Richards pers. comm. 2011-03-14
            oceDebug(debug, "Case 3: Nortek ADP with upward-pointing sensor.\n")
            oceDebug(debug, "        Using heading=heading-90, pitch=roll, roll=-pitch, S=X, F=Y, and M=Z.\n")
            heading <- heading - 90
            tmp <- pitch
            pitch <- roll
            roll <- -tmp
            starboard <- V[, , 1]
            forward <- V[, , 2]
            mast <- V[, , 3]
            if (!isAD2CP && haveBv) {
                ## bottom velocity
                starboardBv <- res@data$bv[, 1]
                forwardBv <- res@data$bv[, 2]
                mastBv <- res@data$bv[, 3]
            }
        } else if (orientation == "downward") {
            oceDebug(debug, "Case 4: Nortek ADP with downward-pointing sensor.\n")
            oceDebug(debug, "        Using heading=heading-90, pitch=roll, roll=-pitch, S=X, F=-Y, and M=-Z.\n")
            heading <- heading - 90
            tmp <- pitch
            pitch <- roll
            roll <- -tmp
            starboard <- V[, , 1]
            forward <- -V[, , 2]
            mast <- -V[, , 3]
            if (!isAD2CP && haveBv) {
                ## bottom velocity
                starboardBv <- res@data$bv[, 1]
                forwardBv <- -res@data$bv[, 2]
                mastBv <- res@data$bv[, 3]
            }
        } else {
            stop("need orientation='upward' or 'downward', not '", orientation, "'")
        }
    } else if (1 == length(agrep("sontek", manufacturer))) {
        ## "sontek"
        if (orientation == "upward") {
            oceDebug(debug, "Case 5: Sontek ADP with upward-pointing sensor.\n")
            oceDebug(debug, "        Using heading=heading-90, pitch=-pitch, roll=-roll, S=X, F=Y, and M=Z.\n")
            heading <- heading - 90
            pitch <- -pitch
            roll <- -roll
            starboard <- res@data$v[, , 1]
            forward <- res@data$v[, , 2]
            mast <- res@data$v[, , 3]
            if (haveBv) {
                ## bottom velocity
                starboardBv <- res@data$bv[, 1]
                forwardBv <- res@data$bv[, 2]
                mastBv <- res@data$bv[, 3]
            }
        } else if (orientation == "downward") {
            oceDebug(debug, "Case 6: Sontek ADP with downward-pointing sensor.\n")
            oceDebug(debug, "        Using heading=heading-90, pitch=-pitch, roll=-roll, S=X, F=Y, and M=Z.\n")
            heading <- heading - 90
            pitch <- -pitch
            roll <- -roll
            starboard <- res@data$v[, , 1]
            forward <- res@data$v[, , 2]
            mast <- res@data$v[, , 3]
            if (haveBv) {
                ## bottom velocity
                starboardBv <- res@data$bv[, 1]
                forwardBv <- res@data$bv[, 2]
                mastBv <- res@data$bv[, 3]
            }
        } else {
            stop("need orientation='upward' or 'downward', not '", orientation, "'")
        }
    } else {
        stop("unrecognized manufacturer; should be 'teledyne rdi', 'sontek', or 'nortek', but is '",
             manufacturer, "'")
    }
    oceDebug(debug, vectorShow(heading, "heading (after adjustment)"))
    oceDebug(debug, vectorShow(pitch, "pitch (after adjustment)"))
    oceDebug(debug, vectorShow(roll, "roll (after adjustment)"))
    nc <- dim(x@data$v)[2]         # numberOfCells
    np <- dim(x@data$v)[1]         # number of profiles
    if (length(heading) < np)
        heading <- rep(heading, length.out=np)
    if (length(pitch) < np)
        pitch <- rep(pitch, length.out=np)
    if (length(roll) < np)
        roll <- rep(roll, length.out=np)
    ## ADP and ADV calculations are both handled by sfm_enu for non-AD2CP.
    for (c in 1:nc) {
        enu <- do_sfm_enu(heading + declination, pitch, roll, starboard[, c], forward[, c], mast[, c])
        res@data$v[, c, 1] <- enu$east
        res@data$v[, c, 2] <- enu$north
        res@data$v[, c, 3] <- enu$up
    }
    if (haveBv) {
        enu <- do_sfm_enu(heading + declination, pitch, roll, starboardBv, forwardBv, mastBv)
        res@data$bv[, 1] <- enu$east
        res@data$bv[, 2] <- enu$north
        res@data$bv[, 3] <- enu$up
    }
    res@metadata$oceCoordinate <- "enu"
    res@processingLog <- processingLogAppend(res@processingLog,
                                       paste("xyzToEnuAdp(x", ", declination=", declination, ", debug=", debug, ")", sep=""))
    oceDebug(debug, "} # xyzToEnuAdp()\n", unindent=1)
    res
}

#' Convert ADP2CP adp object From XYZ to ENU Coordinates
#'
#' **This function will b in active development through the early
#' months of 2019, and both the methodology and user interface may change
#' without notice. Only developers (or invitees) should be trying to
#' use this function.**
#'
#' @param x an [adp-class] object created by [read.adp.ad2cp()].
#'
#' @param declination IGNORED at present, but will be used at some later time.
#' @template debugTemplate
#'
#' @return An object with `data$v[,,1:3]` altered appropriately, and
#' `x[["oceCoordinate"]]` changed from `xyz` to `enu`.
#'
#' @author Dan Kelley
#'
#' @section Limitations:
#' This only works if the instrument orientation is `"AHRS"`, and even
#' that is not tested yet. Plus, as noted, the declination is ignored.
#'
#' @references
#' 1. Nortek AS. \dQuote{Signature Integration 55|250|500|1000kHz.} Nortek AS, 2017.
#'
#' 2. Nortek AS. \dQuote{Signature Integration 55|250|500|1000kHz.} Nortek AS, 2018.
#' https://www.nortekgroup.com/assets/software/N3015-007-Integrators-Guide-AD2CP_1018.pdf.
#'
#' @family things related to adp data
xyzToEnuAdpAD2CP <- function(x, declination=0, debug=getOption("oceDebug"))
{
    debug <- if (debug > 0) 1 else 0
    oceDebug(debug, "xyzToEnuAdpAD2CP(x, declination=", declination, ", debug=", debug, ") {\n", sep="", unindent=1)
    if (!inherits(x, "adp"))
        stop("method is only for objects of class '", "adp", "'")
    if (!is.ad2cp(x))
        stop("this function only works for adp objects created by read.adp.ad2cp()")
    if (0 != declination)
        stop("nonzero declination is not handled yet; please contact the author if you ned this") # FIXME
    res <- x
    ## FIXME: deal with other ad2cp orientations. Can (should) we use a methodology
    ## similar to the non-ad2cp, for non-AHRS cases?
    ## FIXME: do a loop like this for beamToXyzAdpAD2CP() also.
    for (item in names(x@data)) {
        ## Do not try to rotate non-rotatable items, e.g. the vertical beam, the altimeter, etc.
        if (is.list(x@data[[item]])) {
            numberOfBeams <- x@data[[item]]$numberOfBeams
            ##. message("  numberOfBeams=", numberOfBeams)
            if (!is.null(numberOfBeams) && numberOfBeams == 4) {
                orientation <- x@data[[item]]$orientation
                if (is.null(orientation))
                    stop("no known orientation for '", item, "' in the object data slot")
                ## FIXME: handle 'xup', 'xdown', 'yup', 'ydown', 'zup', 'zdown'
                if (orientation[1] != "AHRS")
                    stop("only the 'AHRS' orientation is handled, but '", item, "' has orientation '", orientation[1], "'")
                AHRS <- x@data[[item]]$AHRS
                if (is.null(AHRS))
                    stop("'", item, "' within the object data slot does not contain coordinate-change matrix 'AHRS'")
                oceCoordinate <- x@data[[item]]$oceCoordinate
                if (is.null(oceCoordinate))
                    stop("'", item, "' within the object data slot has no 'oceCoordinate'")
                ## If the item is already in 'enu', we just leave it alone
                ##. message("oceCoordinate: '", oceCoordinate, "'")
                if (oceCoordinate == "xyz") {
                    V <- x@data[[item]]$v
                    if (is.null(V))
                        stop("'", item, "' within the object data slot does not contain velocity 'v'")
                    nc <- dim(V)[2]
                    ## DEVELOPER NOTE
                    ##
                    ## I thought it might be faster to use C++ for the calculations, since the memory pressure ought to
                    ## be a bit smaller (because there is no need to rep() the AHRS values across cells). However, I
                    ## tried a test, but the results, below, suggest the R method is much faster. Also, it will be
                    ## easier for others to modify, I think, so we will use it.
                    ##
                    ## Speed test with 292M file:
                    ##
                    ## C++ method
                    ##  user  system elapsed
                    ## 2.553   0.952   3.511
                    ##> message("C++ method")
                    ##> for (cell in 1:nc) {
                    ##>     res@data[[item]]$v[, cell, 1:3] <- do_ad2cp_ahrs(V[, cell, 1:3], AHRS)
                    ##
                    ## R method
                    ## user  system elapsed
                    ## 0.400   0.139   0.540
                    ##
                    ##> message("R method")
                    e <- V[,,1]*rep(AHRS[,1], times=nc) + V[,,2]*rep(AHRS[,2], times=nc) + V[,,3]*rep(AHRS[,3], times=nc)
                    n <- V[,,1]*rep(AHRS[,4], times=nc) + V[,,2]*rep(AHRS[,5], times=nc) + V[,,3]*rep(AHRS[,6], times=nc)
                    u <- V[,,1]*rep(AHRS[,7], times=nc) + V[,,2]*rep(AHRS[,8], times=nc) + V[,,3]*rep(AHRS[,9], times=nc)
                    ## FIXME: perhaps use the declination now, rotating e and n.  But first, we will need to know
                    ## what declination was used by the instrument, in its creation of AHRS.
                    res@data[[item]]$v[,,1] <- e
                    res@data[[item]]$v[,,2] <- n
                    res@data[[item]]$v[,,3] <- u
                    res@data[[item]]$oceCoordinate <- "enu"
                } else if (oceCoordinate == "beam") {
                    stop("cannot convert from beam to Enu coordinates; use beamToXyz() first")
                }
            }
        }
    }
    res@processingLog <- processingLogAppend(res@processingLog,
                                             paste("xyzToEnuAdpAD2CP(x",
                                                   ", declination=", declination,
                                                   ", debug=", debug, ")", sep=""))
    oceDebug(debug, "} # xyzToEnuAdpAD2CP()\n", unindent=1)
    res
}

#' Convert ADP ENU to Rotated Coordinate
#'
#' Convert ADP velocity components from an enu-based coordinate system to
#' another system, perhaps to align axes with the coastline.
#'
#' The supplied angles specify rotations to be made around the axes for which
#' heading, pitch, and roll are defined.  For example, an eastward current will
#' point southeast if `heading=45` is used.
#'
#' The returned value has heading, pitch, and roll matching those of `x`,
#' so these angles retain their meaning as the instrument orientation.
#'
#' NOTE: this function works similarly to [xyzToEnuAdp()], except
#' that in the present function, it makes no difference whether the instrument
#' points up or down, etc.
#'
#' @param x an [adp-class] object.
#'
#' @param heading number or vector of numbers, giving the angle, in degrees, to
#' be added to the heading.  See \dQuote{Details}.
#'
#' @param pitch as `heading` but for pitch.
#'
#' @param roll as `heading` but for roll.
#'
#' @return An object with `data$v[,1:3,]` altered appropriately, and
#' `metadata$oce.coordinate` changed from `enu` to `other`.
#'
#' @author Dan Kelley
#'
#' @seealso See [read.adp()] for other functions that relate to
#' objects of class `"adp"`.
#'
#' @references
#' 1. Teledyne RD Instruments. \dQuote{ADCP Coordinate Transformation: Formulas and
#' Calculations,} January 2010. P/N 951-6079-00.
#'
#' @examples
#'
#' library(oce)
#' data(adp)
#' o <- enuToOtherAdp(adp, heading=-31.5)
#' plot(o, which=1:3)
#'
#' @family things related to adp data
enuToOtherAdp <- function(x, heading=0, pitch=0, roll=0)
{
    if (!inherits(x, "adp"))
        stop("method is only for objects of class '", "adp", "'")
    if (is.ad2cp(x))
        stop("this function does not work yet for AD2CP data")
    oceCoordinate <- x[["oceCoordinate"]]
    if (oceCoordinate != "enu")
        stop("input must be in enu coordinates, but it is in ", oceCoordinate, " coordinates")
    res <- x
    np <- dim(x[["v"]])[1]           # number of profiles
    if (length(heading) != np)
        heading <- rep(heading, length.out=np)
    if (length(pitch) != np)
        pitch <- rep(pitch, length.out=np)
    if (length(roll) != np)
        roll <- rep(roll, length.out=np)
    nc <- dim(x[["v"]])[2]           # numberOfCells
    for (c in 1:nc) {
        other <- do_sfm_enu(heading, pitch, roll, x[["v"]][, c, 1], x[["v"]][, c, 2], x[["v"]][, c, 3])
        res@data$v[, c, 1] <- other$east
        res@data$v[, c, 2] <- other$north
        res@data$v[, c, 3] <- other$up
    }
    if ("bv" %in% names(x@data)) {
        other <- do_sfm_enu(heading, pitch, roll, x@data$bv[, 1], x@data$bv[, 2], x@data$bv[, 3])
        res@data$bv[, 1] <- other$east
        res@data$bv[, 2] <- other$north
        res@data$bv[, 3] <- other$up
    }
    res@metadata$oceCoordinate <- "other"
    res@processingLog <- processingLogAppend(res@processingLog, paste(deparse(expr=match.call()), sep="", collapse=""))
    res
}

peek.ahead <- function(file, bytes=2, debug=!TRUE)
{
    pos <- seek(file)
    res <- readBin(file, "raw", n=bytes, size=1)
    oceDebug(debug, "peeked at", paste("0x", paste(res, sep=" "), sep=""), "\n")
    seek(file, pos)
    res
}

display.bytes <- function(b, label="", ...)
{
    n <- length(b)
    cat("\n", label, " (", n, "bytes)\n", sep="", ...)
    print(b, ...)
}


#' Subtract Bottom Velocity from ADP
#'
#' Subtracts bottom tracking velocities from an `"adp"` object. Works for
#' all coordinate systems (`beam`, `xyz`, and `enu`).
#'
#' @param x an [adp-class] object that contains bottom-tracking velocities.
#'
#' @param despike either a logical value or a univariate function. This
#' controls whether the bottom velocity (`bv`) values should be altered before they are
#' subtracted from the beam velocities. If it is `TRUE` then the `bv` values are despiked
#' first by calling [despike()]. If it is a function, then that function is used instead of
#' [despike()], e.g. `function(x) despike(x, reference="smooth")` would change the reference
#' function for despiking from its default of `"median"`.
#'
#' @template debugTemplate
#'
#' @author Dan Kelley and Clark Richards
#'
#' @seealso See [read.adp()] for notes on functions relating to
#' `"adp"` objects, and [adp-class] for notes on the ADP
#' object class.
#'
#' @family things related to adp data
subtractBottomVelocity <- function(x, despike=FALSE, debug=getOption("oceDebug"))
{
    oceDebug(debug, "subtractBottomVelocity(x) {\n", unindent=1)
    if (!("bv" %in% names(x@data))) {
        warning("there is no bottom velocity in this object")
        return(x)
    }
    res <- x
    numberOfBeams <- dim(x[["v"]])[3] # could also get from metadata but this is less brittle
    for (beam in 1:numberOfBeams) {
        oceDebug(debug, "beam #", beam, "\n")
        if (is.logical(despike)) {
            if (despike == FALSE) {
                res@data$v[, , beam] <- x[["v"]][, , beam] - x@data$bv[, beam]
            } else {
                res@data$v[, , beam] <- x[["v"]][, , beam] - despike(x@data$bv[, beam])
            }
        } else if (is.function(despike)) {
            res@data$v[, , beam] <- x[["v"]][, , beam] - despike(x@data$bv[, beam])
        } else {
            stop("despike must be a logical value or a function")
        }
    }
    oceDebug(debug, "} # subtractBottomVelocity()\n", unindent=1)
    res@processingLog <- processingLogAppend(res@processingLog, paste(deparse(expr=match.call()), sep="", collapse=""))
    res
}


#' Bin-map an ADP object
#'
#' Bin-map an ADP object, by interpolating velocities, backscatter amplitudes,
#' etc., to uniform depth bins, thus compensating for the pitch and roll of the
#' instrument.  This only makes sense for ADP objects that are in beam
#' coordinates.
#'
#' @param x an [adp-class] object.
#'
#' @template debugTemplate
#'
#' @return An [adp-class] object.
#'
#' @section Bugs: This only works for 4-beam RDI ADP objects.
#'
#' @author Dan Kelley and Clark Richards
#'
#' @seealso See [adp-class] for a discussion of `adp` objects
#' and notes on the many functions dealing with them.
#'
#' @references The method was devised by Clark Richards for use in his PhD work
#' at Department of Oceanography at Dalhousie University.
#'
#' @examples
#'\dontrun{
#' library(oce)
#' beam <- read.oce("/data/archive/sleiwex/2008/moorings/m09/adp/rdi_2615/raw/adp_rdi_2615.000",
#'                  from=as.POSIXct("2008-06-26", tz="UTC"),
#'                  to=as.POSIXct("2008-06-26 00:10:00", tz="UTC"),
#'                  longitude=-69.73433, latitude=47.88126)
#' beam2 <- binmapAdp(beam)
#' plot(enuToOther(toEnu(beam), heading=-31.5))
#' plot(enuToOther(toEnu(beam2), heading=-31.5))
#' plot(beam, which=5:8) # backscatter amplitude
#' plot(beam2, which=5:8)
#'}
#'
#' @family things related to adp data
binmapAdp <- function(x, debug=getOption("oceDebug"))
{
    oceDebug(debug, "binmap(x, debug) {\n", unindent=1)
    if (!inherits(x, "adp"))
        stop("x must be an \"adp\" object")
    v <- x[["v"]]
    a <- x[["a"]] ## FIXME: should ensure that this exist
    q <- x[["q"]]
    g <- x[["g"]]
    if (4 != dim(v)[3])
        stop("binmap() only works for 4-beam instruments")
    theta <- x[['beamAngle']]           # FIXME: check that not missing or weird
    distance <- x[["distance"]]
    roll <- x[["roll"]]
    pitch <- x[["pitch"]]
    ## Below, we loop through the profiles.  I tried an experiment in
    ## vectorizing across the loop, by combining into a single vector
    ## for (distance, cr, ...), but it was no faster, and the code was
    ## more complicated to read.
    vbm <- array(double(), dim=dim(v))
    abm <- array(raw(), dim=dim(v))
    qbm <- array(raw(), dim=dim(v))
    gbm <- array(raw(), dim=dim(v))
    nprofile <- dim(v)[1]
    res <- x
    for (profile in 1:nprofile) {
        r <- roll[profile]
        p <- pitch[profile]
        cr <- cos(r * pi / 180)
        sr <- sin(r * pi / 180)
        cp <- cos(p * pi / 180)
        sp <- sin(p * pi / 180)
        tt <- tan(theta * pi / 180)
        z1 <- distance * (cr - tt * sr) * cp

        ##if (profile == 1) {
        ##    cat('R : r', r, 'p', p, 'cr', cr, 'sr', sr, 'cp', cp, 'sp', sp, 'tt', tt, '\n')
        ##    cat("R : z1      ", format(z1[1:8], width=11, digits=7), '\n')
        ##}

        z2 <- distance * (cr + tt * sr) * cp
        z3 <- distance * (cp + tt * sp) * cr
        z4 <- distance * (cp - tt * sp) * cr
        ## FIXME: check on whether we can speed things up by using e.g. x[["v"]]
        ## instead of v, which would lower the memory requirements.

        ## v=velocity
        ## Need to check all four beams that there are more than 2
        ## non-NA values in the profiles, otherwise set to 0
        checkNA <- sum(!is.na(v[profile, , 1])) > 1 & sum(!is.na(v[profile, , 2])) > 1 & sum(!is.na(v[profile, , 3])) > 1 & sum(!is.na(v[profile, , 4])) > 1
        if (checkNA) {
            vbm[profile, , 1] <- approx(z1, v[profile, , 1], distance)$y
            vbm[profile, , 2] <- approx(z2, v[profile, , 2], distance)$y
            vbm[profile, , 3] <- approx(z3, v[profile, , 3], distance)$y
            vbm[profile, , 4] <- approx(z4, v[profile, , 4], distance)$y
        } else {
            vbm[profile, , 1] <- NA
            vbm[profile, , 2] <- NA
            vbm[profile, , 3] <- NA
            vbm[profile, , 4] <- NA
        }
        ## a
        rule <- 2                      # FIXME: is is OK to extend data to edges?
        abm[profile, , 1] <- oce.as.raw(approx(z1, as.numeric(a[profile, , 1], rule=rule), distance)$y)
        abm[profile, , 2] <- oce.as.raw(approx(z2, as.numeric(a[profile, , 2], rule=rule), distance)$y)
        abm[profile, , 3] <- oce.as.raw(approx(z3, as.numeric(a[profile, , 3], rule=rule), distance)$y)
        abm[profile, , 4] <- oce.as.raw(approx(z4, as.numeric(a[profile, , 4], rule=rule), distance)$y)
        ## q
        qbm[profile, , 1] <- oce.as.raw(approx(z1, as.numeric(q[profile, , 1], rule=rule), distance)$y)
        qbm[profile, , 2] <- oce.as.raw(approx(z2, as.numeric(q[profile, , 2], rule=rule), distance)$y)
        qbm[profile, , 3] <- oce.as.raw(approx(z3, as.numeric(q[profile, , 3], rule=rule), distance)$y)
        qbm[profile, , 4] <- oce.as.raw(approx(z4, as.numeric(q[profile, , 4], rule=rule), distance)$y)
        ## g
        gbm[profile, , 1] <- oce.as.raw(approx(z1, as.numeric(g[profile, , 1], rule=rule), distance)$y)
        gbm[profile, , 2] <- oce.as.raw(approx(z2, as.numeric(g[profile, , 2], rule=rule), distance)$y)
        gbm[profile, , 3] <- oce.as.raw(approx(z3, as.numeric(g[profile, , 3], rule=rule), distance)$y)
        gbm[profile, , 4] <- oce.as.raw(approx(z4, as.numeric(g[profile, , 4], rule=rule), distance)$y)
    }
    res@data$v <- vbm
    ##cat("R : v1      ", format(v[1,1:8,1], width=11, digits=7), '\n')
    ##cat("R : V1      ", format(vbm[1,1:8,1], width=11, digits=7), '\n')
    res@data$a <- abm
    res@data$q <- qbm
    res@data$g <- gbm
    res
}

#' Ensemble Average an ADP Object in Time
#'
#' Ensemble averaging of `adp` objects is often necessary to
#' reduce the uncertainty in velocity estimates from single
#' pings. Many types of ADPs can be configured to perform the
#' ensemble averaging during the data collection, due to memory
#' limitations for long deployments. In cases where the instrument is
#' not memory limited, it may be desirable to perform the ensemble
#' averaging during post-processing, thereby reducing the overall
#' size of the data set and decreasing the uncertainty of the
#' velocity estimates (by averaging out Doppler noise).
#'
#' @param x an [adp-class] object.
#'
#' @param n number of pings to average together.
#'
#' @param leftover a logical value indicating how to proceed in cases
#' where `n` does not divide evenly into the number of ensembles
#' in `x`. If `leftover` is `FALSE` (the default) then any extra
#' ensembles at the end of `x` are ignored. Otherwise, they are used
#' to create a final ensemble in the returned value.
#'
#' @param na.rm a logical value indicating whether NA values should be stripped
#' before the computation proceeds
#'
#' @param ... extra arguments to be passed to the `mean()` function.
#'
#' @return A new [adp-class] object with ensembles averaged as specified. E.g. for an `adp` object with 100 pings and `n=5` the number of rows of the data arrays will be reduced by a factor of 5.
#'
#' @author Clark Richards and Dan Kelley
#'
#' @examples
#' library(oce)
#' data(adp)
#' adpAvg <- adpEnsembleAverage(adp, n=2)
#' plot(adpAvg)
#'
#' @family things related to adp data
adpEnsembleAverage <- function(x, n=5, leftover=FALSE, na.rm=TRUE, ...)
{
    if (!inherits(x, 'adp')) stop('Must be an object of class adp')
    res <- new('adp', distance=x[['distance']])
    res@metadata <- x@metadata
    d <- x@data
    t <- as.POSIXct(d$time) # ensure POSIXct so next line works right
    ntx <- length(t)
    pings <- seq_along(t)
    ## Note the limits of the breaks, below. We start at 0 to catch the first
    ## pings value. If leftover is TRUE, we also extend at the right, to catch
    ## the fractional chunk that will exist at the end, if n does not divide into ntx.
    breaks <- if (leftover) seq(0, ntx+n, n) else seq(0, ntx, n)
    fac <- cut(pings, breaks=breaks, labels=FALSE) # used to split() data items
    ##res@data$time <- numberAsPOSIXct(binAverage(pings, t, xinc=n)$y)
    res@data$time <- numberAsPOSIXct(as.numeric(lapply(split(as.numeric(t), fac), mean, na.rm=na.rm, ...)))
    for (field in names(d)) {
        if (field != 'time' & field != 'distance') {
            if (is.vector(d[[field]])) {
                ##res@data[[field]] <- binAverage(pings, d[[field]], xinc=n)$y
                res@data[[field]] <- as.numeric(lapply(split(as.numeric(d[[field]]), fac), mean, na.rm=na.rm, ...))
            } else if (is.array(d[[field]])) {
                fdim <- dim(d[[field]])
                res@data[[field]] <- array(NA, dim=c(length(res@data[['time']]), fdim[-1]))
                for (j in 1:tail(fdim, 1)) {
                    if (length(fdim) == 2) { # for fields like bottom range
                        ##res@data[[field]][, j] <- binAverage(pings, d[[field]][, j], xinc=n)$y
                        res@data[[field]][, j] <- unlist(lapply(split(as.numeric(d[[field]][, j]), fac), mean, na.rm=na.rm, ...))
                    } else if (length(fdim) == 3) { # for array fields like v, a, q, etc
                        for (i in 1:fdim[2]) {
                            ##res@data[[field]][, i, j] <- binAverage(pings, d[[field]][, i, j], xinc=n)$y
                            res@data[[field]][, i, j] <- unlist(lapply(split(as.numeric(d[[field]][, i, j]), fac), mean, na.rm=na.rm, ...))
                        }
                    }
                }
                if (is.raw(d[[field]])) {
                    dims <- dim(res@data[[field]])
                    res@data[[field]] <- array(as.raw(res@data[[field]]), dim=dims)
                }
            }
        }
    }
    res@metadata$numberOfSamples <- length(res@data$time) # FIXME: handle AD2CP
    res@processingLog <- processingLogAppend(res@processingLog, paste(deparse(match.call()), sep="", collapse=""))
    res
}

# vim: tw=120 shiftwidth=4 softtabstop=4 expandtab:

#' Convert Raw to Numeric Values For adp Objects
#'
#' Convert variables in an [adp-class] object from raw to numeric format.
#'
#' @param object an [adp-class] object.
#'
#' @param variables variables stored in an [adp-class] object that has
#' the same dimensional as `v` and is stored in a raw format.
#'
#' @return `adpConvertRawToNumeric` returns an [adp-class] object whose specified
#' variables have been converted from raw to numerical format.
#' @template debugTemplate
#' @family things related to adp data
#' @author Jaimie Harbin and Dan Kelley
#' @export
#'
#' @examples
#' library(oce)
#' data(adp)
#' adp[["a"]][,,1][,1]
#' ADP <- adpConvertRawToNumeric(adp)
#' ADP[["a"]][,,1][,1]
adpConvertRawToNumeric <- function(object=NULL, variables=NULL, debug=getOption("oceDebug"))
{
    oceDebug(debug, "adpConvertRawToNumeric() {\n", sep="", unindent=1, style="bold")
    if (!inherits(object, "adp"))
        stop("object must be an adp object")
    v <- object[["v"]]
    if (!exists("v"))
        stop("the adp object must contain \"v\"")
    # Find relevant variables if variable=NULL
    dimNeeded <- dim(object[["v"]])
    if (is.null(variables)) {
        dataNames <- names(object@data)
        keep <- sapply(dataNames,
            function(variableTrial) {
                dimtest <- dim(object[[variableTrial]])
                length(dimtest) == 3 && all(dimtest == dimNeeded)
            })
        variables <- dataNames[keep]
        oceDebug(debug, "inferred variables:", paste(variables, collapse=", "), "\n")
    }
    for (variable in variables) {
        # Catch errors
        if (length(dim(object[[variable]])) != 3)
            stop("\"", variable, "\" does not have three dimensions")
        if (all(dim(object[[variable]]) != dimNeeded))
            stop("\"", variable, "\" must have dimension ", paste(dimNeeded, collapse="x"))
        # Convert this relevant variable from raw to numeric 
        object[[variable]] <- object[[variable, "numeric"]]
    }
    object@processingLog <- processingLogAppend(object@processingLog, paste(deparse(match.call()), sep="", collapse=""))
    oceDebug(debug, "} # adpConvertRawToNumeric()\n", sep="", unindent=1, style="bold")
    object
}

# vim: tw=120 shiftwidth=4 softtabstop=4 expandtab:

#' Flag adp Data Past Water Column Boundary
#'
#' Flag variables with the same dimension of `v` in
#' an [adp-class] object that are beyond the water column boundary.
#' Currently, this operation can only be performed on [adp-class]
#' objects that contain bottom ranges. Commonly, [handleFlags()] would
#' then be used to remove such data.
#'
#' This works by fitting a smoothing spline to a bottom range with a defined
#' number of degrees of freedom. For each time, it then searches to determine
#' which associated distances are greater than the predicted smooth spline
#' multiplied by \eqn{1-trim}.
#'
#' @param x an [adp-class] object containing bottom ranges.
#'
#' @param fields a variable contained within `x` indicating which field to flag.
#'
#' @param df the degrees of freedom to use during the smoothing spline operation.
#'
#' @param trim a scale factor for boundary trimming (see \dQuote{Details}).
#'
#' @param good number stored in flags to indicate good data.
#'
#' @param bad number stored in flags to indicate bad data.
#' @template debugTemplate
#'
#' @return `adpFlagPastBoundary` returns an [adp-class] object with flags
#' adjusted in the specified fields if data are beyond the water column boundary.
#'
#' @author Jaimie Harbin, Clark Richards, and Dan Kelley
#'
#' @family things related to adp data
#'
#' @export
adpFlagPastBoundary <- function(x=NULL, fields=NULL, df=20, trim=0.15, good=1, bad=4, debug=getOption("oceDebug"))
{
    oceDebug(debug, "adpFlagPastBoundary() {\n", sep="", unindent=1, style="bold")
    if (!inherits(x, "adp"))
        stop("x must be an adp object")
    if (!"br" %in% names(x@data))
        stop("adpFlagPastBoundary can only flag fields that have the same dimension as \"v\"")
    dimNeeded <- dim(x[["v"]])
    if (is.null(fields)) {
        dataNames <- names(x@data)
        keep <- sapply(dataNames,
            function(variableTrial) {
                dimtest <- dim(x[[variableTrial]])
                length(dimtest) == 3 && all(dimtest == dimNeeded)
            })
        fields <- dataNames[keep]
        oceDebug(debug, "inferred fields:", paste(fields, collapse=", "), "\n")
    }
    mask <- array(good, dim=dim(x[["v"]]))
    time <- x[["time"]]
    for (kbeam in seq_len(x[["numberOfBeams"]])) {
        timeSeconds <- as.numeric(time)
        br <- x[["br"]][,kbeam]
        ok <- is.finite(br)
        X <- timeSeconds[ok]
        y <- br[ok]
        s <- smooth.spline(X, y, df=df)
        p <- predict(s, timeSeconds)$y
        for (itime in seq_along(x[["time"]])) {
            jbad <- x[["distance"]] > (1-trim)*p[itime]
            mask[itime, jbad, kbeam] <- bad
        }
    }
    for (f in fields) {
        x[["flags"]][[f]] <- mask
        oceDebug(debug, "handled field '",f, "'\n", sep="")
    }
    x@processingLog <- processingLogAppend(x@processingLog, paste(deparse(match.call()), sep="", collapse=""))
    oceDebug(debug, "} # adpFlagPastBoundary()\n", sep="", unindent=1, style="bold")
    return(x)
}
