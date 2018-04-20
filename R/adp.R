## vim: tw=120 shiftwidth=4 softtabstop=4 expandtab:

#' Class to Store adp (ADCP) Data
#'
#' This class stores data from acoustic Doppler profilers. Some manufacturers
#' call these ADCPs, while others call them ADPs; here the shorter form is
#' used by analogy to ADVs.
#'
#' @templateVar class adp
#'
#' @templateVar dataExample The key items stored in this slot include \code{time}, \code{distance}, and \code{v}, along with angles \code{heading}, \code{pitch} and \code{roll}.
#'
#' @templateVar metadataExample Examples that are of common interest include \code{oceCoordinate}, \code{orientation}, \code{frequency}, and \code{beamAngle}.
#'
#' @template slot_summary
#'
#' @template slot_put
#'
#' @template slot_get
#'
#' @section Reading/creating \code{adp} objects:
#'
#' The \code{metadata} slot contains various
#' items relating to the dataset, including source file name, sampling rate,
#' velocity resolution, velocity maximum value, and so on.  Some of these are
#' particular to particular instrument types, and prudent researchers will take
#' a moment to examine the whole contents of the metadata, either in summary
#' form (with \code{str(adp[["metadata"]])}) or in detail (with
#' \code{adp[["metadata"]]}).  Perhaps the most useful general properties are
#' \code{adp[["bin1Distance"]]} (the distance, in metres, from the sensor to
#' the bottom of the first bin), \code{adp[["cellSize"]]} (the cell height, in
#' metres, in the vertical direction, \emph{not} along the beam), and
#' \code{adp[["beamAngle"]]} (the angle, in degrees, between beams and an
#' imaginary centre line that bisects all beam pairs).
#'
#' The diagram provided below indicates the coordinate-axis and beam-numbering
#' conventions for three- and four-beam ADP devices, viewed as though the
#' reader were looking towards the beams being emitted from the transducers.
#'
#' \if{html}{\figure{adp_beams.png}{options: width=400px alt="Figure: adp_beams.png"}}
#'
#' The bin geometry of a four-beam profiler is illustrated below, for
#' \code{adp[["beamAngle"]]} equal to 20 degrees, \code{adp[["bin1Distance"]]}
#' equal to 2m, and \code{adp[["cellSize"]]} equal to 1m.   In the diagram, the
#' viewer is in the plane containing two beams that are not shown, so the two
#' visible beams are separated by 40 degrees.  Circles indicate the centres of
#' the range-gated bins within the beams.  The lines enclosing those circles
#' indicate the coverage of beams that spread plus and minus 2.5 degrees from
#' their centreline.
#'
#' \if{html}{\figure{adpgeometry2.png}{options: width=400px alt="Figure: adpgeometry2.png"}}
#'
#' Note that \code{adp[["oceCoordinate"]]} stores the present coordinate system
#' of the object, and it has possible values \code{"beam"}, \code{"xyz"}, \code{"sfm"} or
#' \code{"enu"}.  (This should not be confused with
#' \code{adp[["originalCoordinate"]]}, which stores the coordinate system used
#' in the original data file.)
#'
#' The \code{data} slot holds some standardized items, and
#' many that vary from instrument to instrument.  One standard item is
#' \code{adp[["v"]]}, a three-dimensional numeric array of velocities in
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
#' and distance from sensor (y axis), since the \code{adp} dataset is
#' in earth coordinates. Note the semidurnal tidal signal, and the pattern of missing
#' data at the ocean surface (gray blotches at the top).
#'
#' Corresponding to the velocity array are two arrays of type raw, and
#' identical dimension, accessed by \code{adp[["a"]]} and \code{adp[["q"]]},
#' holding measures of signal strength and data quality quality,
#' respectively.  (The exact meanings of these depend on the particular type
#' of instrument, and it is assumed that users will be familiar enough with
#' instruments to know both the meanings and their practical consequences in
#' terms of data-quality assessment, etc.)
#'
#' In addition to the arrays, there are time-based vectors.  The vector
#' \code{adp[["time"]]} (of length equal to the first index of
#' \code{adp[["v"]]}, etc.) holds times of observation.  Depending on type of
#' instrument and its configuration, there may also be corresponding vectors
#' for sound speed (\code{adp[["soundSpeed"]]}), pressure
#' (\code{adp[["pressure"]]}), temperature (\code{adp[["temperature"]]}),
#' heading (\code{adp[["heading"]]}) pitch (\code{adp[["pitch"]]}), and roll
#' (\code{adp[["roll"]]}), depending on the setup of the instrument.
#'
#' The precise meanings of the data items depend on the instrument type.  All
#' instruments have \code{v} (for velocity), \code{q} (for a measure of data
#' quality) and \code{a} (for a measure of backscatter amplitude, also called
#' echo intensity).
#' Teledyne-RDI profilers have an additional item \code{g} (for
#' percent-good).
#'
#' VmDas-equipped Teledyne-RDI profilers additional navigation data, with
#' details listed in the table below; note that the RDI documentation [2] and
#' the RDI gui use inconsistent names for most items.
#'
#' \tabular{lll}{
#'   \strong{Oce name}\tab \strong{RDI doc name}\tab \strong{RDI GUI name}\cr
#'   \code{avgSpeed}\tab Avg Speed\tab Speed/Avg/Mag\cr
#'   \code{avgMagnitudeVelocityEast}\tab Avg Mag Vel East\tab ?\cr
#'   \code{avgMagnitudeVelocityNorth}\tab Avg Mag Vel North\tab ?\cr
#'   \code{avgTrackMagnetic}\tab Avg Track Magnetic\tab Speed/Avg/Dir (?)\cr
#'   \code{avgTrackTrue}\tab Avg Track True\tab Speed/Avg/Dir (?)\cr
#'   \code{avgTrueVelocityEast}\tab Avg True Vel East\tab ?\cr
#'   \code{avgTrueVelocityNorth}\tab Avg True Vel North\tab ?\cr
#'   \code{directionMadeGood}\tab Direction Made Good\tab Speed/Made Good/Dir\cr
#'   \code{firstLatitude}\tab First latitude\tab Start Lat\cr
#'   \code{firstLongitude}\tab First longitude\tab Start Lon\cr
#'   \code{firstTime}\tab UTC Time of last fix\tab End Time\cr
#'   \code{lastLatitude}\tab Last latitude\tab End Lat\cr
#'   \code{lastLongitude}\tab Last longitude\tab End Lon\cr
#'   \code{lastTime}\tab UTC Time of last fix\tab End Time\cr
#'   \code{numberOfHeadingSamplesAveraged}\tab Number heading samples averaged\tab ?\cr
#'   \code{numberOfMagneticTrackSamplesAveraged}\tab Number of magnetic track samples averaged\tab ? \cr
#'   \code{numberOfPitchRollSamplesAvg}\tab Number of magnetic track samples averaged\tab ? \cr
#'   \code{numberOfSpeedSamplesAveraged}\tab Number of speed samples averaged\tab ? \cr
#'   \code{numberOfTrueTrackSamplesAvg}\tab Number of true track samples averaged\tab ? \cr
#'   \code{primaryFlags}\tab Primary Flags\tab ?\cr
#'   \code{shipHeading}\tab Heading\tab ?\cr
#'   \code{shipPitch}\tab Pitch\tab ?\cr
#'   \code{shipRoll}\tab Roll\tab ?\cr
#'   \code{speedMadeGood}\tab Speed Made Good\tab Speed/Made Good/Mag\cr
#'   \code{speedMadeGoodEast}\tab Speed MG East\tab ?\cr
#'   \code{speedMadeGoodNorth}\tab Speed MG North\tab ?\cr
#' }
#'
#' For Teledyne-RDI profilers, there are four three-dimensional arrays
#' holding beamwise data.  In these, the first index indicates time, the
#' second bin number, and the third beam number (or coordinate number, for
#' data in \code{xyz}, \code{sfm}, \code{enu} or \code{other} coordinate systems).  In
#' the list below, the quoted phrases are quantities as defined in Figure 9
#' of reference 1.
#'
#' \itemize{
#'
#'   \item \code{v} is ``velocity'' in m/s, inferred from two-byte signed
#'   integer values (multiplied by the scale factor that is stored in
#'   \code{velocityScale} in the metadata).
#'
#'   \item \code{q} is ``correlation magnitude'' a one-byte quantity stored
#'   as type \code{raw} in the object. The values may range from 0 to 255.
#'
#'   \item \code{a} is ``backscatter amplitude``, also known as ``echo
#'   intensity'' a one-byte quantity stored as type \code{raw} in the object.
#'   The values may range from 0 to 255.
#'
#'   \item \code{g} is ``percent good'' a one-byte quantity stored as \code{raw}
#'   in the object.  The values may range from 0 to 100.
#'
#' }
#'
#' Finally, there is a vector \code{adp[["distance"]]} that indicates the bin
#' distances from the sensor, measured in metres along an imaginary centre
#' line bisecting beam pairs.  The length of this vector equals
#' \code{dim(adp[["v"]])[2]}.
#'
#' @section Teledyne-RDI Sentinel V ADCPs: As of 2016-09-27 there is
#'     provisional support for the TRDI "SentinelV" ADCPs, which are 5
#'     beam ADCPs with a vertical centre beam. Relevant vertical beam
#'     fields are called \code{adp[["vv"]]}, \code{adp[["va"]]},
#'     \code{adp[["vq"]]}, and \code{adp[["vg"]]} in analogy with the
#'     standard 4-beam fields.
#'
#' @section Accessing and altering information within \code{adp-class} objects:
#' \emph{Extracting values} Matrix data may be accessed as illustrated
#' above, e.g.  or an adp object named \code{adv}, the data are provided by
#' \code{adp[["v"]]}, \code{adp[["a"]]}, and \code{adp[["q"]]}.  As a
#' convenience, the last two of these can be accessed as numeric (as opposed to
#' raw) values by e.g.  \code{adp[["a", "numeric"]]}.  The vectors are accessed
#' in a similar way, e.g. \code{adp[["heading"]]}, etc.  Quantities in the
#' \code{metadata} slot are also available by name, e.g.
#' \code{adp[["velocityResolution"]]}, etc.
#'
#' \emph{Assigning values.} This follows the standard form, e.g. to increase
#' all velocity data by 1 cm/s, use \code{adp[["v"]] <- 0.01 + adp[["v"]]}.
#'
#' \emph{Overview of contents} The \code{show} method (e.g.
#' \code{show(d)}) displays information about an ADP object named \code{d}.
#'
#' @section Dealing with suspect data:
#' There are many possibilities for confusion
#' with \code{adp} devices, owing partly to the flexibility that manufacturers
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
#' 1. Teledyne-RDI, 2007. \emph{WorkHorse commands and output data
#' format.} P/N 957-6156-00 (November 2007).
#'
#' 2. Teledyne-RDI, 2012. \emph{VmDas User's Guide, Ver. 1.46.5}.
#'
#' @seealso
#' A file containing ADP data is usually recognized by Oce, and so
#' \code{\link{read.oce}} will usually read the data.  If not, one may use the
#' general ADP function \code{\link{read.adp}} or specialized variants
#' \code{\link{read.adp.rdi}}, \code{\link{read.adp.nortek}} or
#' \code{\link{read.adp.sontek}} or \code{\link{read.adp.sontek.serial}}.
#'
#' ADP data may be plotted with \code{\link{plot,adp-method}}, which is a
#' generic function so it may be called simply as \code{plot}.
#'
#' Statistical summaries of ADP data are provided by the generic function
#' \code{summary}, while briefer overviews are provided with \code{show}.
#'
#' Conversion from beam to xyz coordinates may be done with
#' \code{\link{beamToXyzAdp}}, and from xyz to enu (east north up) may be done
#' with \code{\link{xyzToEnuAdp}}.  \code{\link{toEnuAdp}} may be used to
#' transfer either beam or xyz to enu.  Enu may be converted to other coordinates
#' (e.g. aligned with a coastline) with \code{\link{enuToOtherAdp}}.
#'
#' @family classes provided by \code{oce}
#' @family things related to \code{adp} data
setClass("adp", contains="oce")

#' ADP (acoustic-doppler profiler) dataset
#'
#' This is degraded subsample of measurements that were made with an
#' upward-pointing ADP manufactured by Teledyne-RDI, as part of the St Lawrence
#' Internal Wave Experiment (SLEIWEX).
#'
#' @name adp
#'
#' @docType data
#'
#' @usage data(adp)
#'
#' @examples
#' \dontrun{
#' library(oce)
#' data(adp)
#'
#' # Velocity components.  (Note: we should probably trim some bins at top.)
#' plot(adp)
#'
#' # Note that tides have moved the mooring.
#' plot(adp, which=15:18)
#' }
#'
#'
#' @source This file came from the SLEIWEX-2008 experiment.
#'
#' @family datasets provided with \code{oce}
#' @family things related to \code{adp} data
NULL

setMethod(f="initialize",
          signature="adp",
          definition=function(.Object, time, distance, v, a, q, oceCoordinate="enu", orientation="upward") {
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
              .Object@processingLog$time <- as.POSIXct(Sys.time())
              .Object@processingLog$value <- "create 'adp' object"
              return(.Object)
          })


#' Summarize an ADP Object
#'
#' Summarize data in an \code{adp} object.
#'
#' Pertinent summary information is presented.
#'
#' @aliases summary.adp summary,adp,missing-method summary,adp-method
#' @param object an object of class \code{"adp"}, usually, a result of a call
#' to \code{\link{read.oce}}, \code{\link{read.adp.rdi}}, or
#' \code{\link{read.adp.nortek}}.
#' @param \dots further arguments passed to or from other methods.
#' @return A matrix containing statistics of the elements of the \code{data}
#' slot.
#' @author Dan Kelley
#'
#' @family things related to \code{adp} data
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
              v.dim <- dim(object@data$v)
              cat("* Number of profiles:", v.dim[1], "\n")
              cat("* Number of cells:   ", v.dim[2], "\n")
              cat("* Number of beams:   ", v.dim[3], "\n")
              cat("* Cell size:         ", object@metadata$cellSize, "m\n")
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
              if (object@metadata$numberOfCells > 1)
                  cat(sprintf("* Cells:              %d, centered at %.3f m to %.3f m, spaced by %.3f m\n",
                              object@metadata$numberOfCells, object@data$distance[1],  tail(object@data$distance, 1), diff(object@data$distance[1:2])),  ...)
              else
                  cat(sprintf("* Cells:              one cell, centered at %.3f m\n", object@data$distance[1]), ...)

              cat("* Coordinate system: ", object@metadata$originalCoordinate, "[originally],", object@metadata$oceCoordinate, "[presently]\n", ...)
              cat("* Frequency:         ", object@metadata$frequency, "kHz\n", ...)
              if ("oceBeamUnspreaded" %in% mnames)
                  cat("* Beams:             ", object@metadata$numberOfBeams, if (!is.null(object@metadata$oceBeamUnspreaded) &
                                                                                  object@metadata$oceBeamUnspreaded) "beams (attenuated)" else "beams (not attenuated)",
                      "oriented", object@metadata$orientation, "with angle", object@metadata$beamAngle, "deg to axis\n", ...)
              if (!is.null(object@metadata$transformationMatrix)) {
                  digits <- 4
                  cat("* Transformation matrix::\n\n")
                  cat("  ", format(object@metadata$transformationMatrix[1, ], width=digits+4, digits=digits, justify="right"), "\n")
                  cat("  ", format(object@metadata$transformationMatrix[2, ], width=digits+4, digits=digits, justify="right"), "\n")
                  cat("  ", format(object@metadata$transformationMatrix[3, ], width=digits+4, digits=digits, justify="right"), "\n")
                  if (object@metadata$numberOfBeams > 3)
                      cat("  ", format(object@metadata$transformationMatrix[4, ], width=digits+4, digits=digits, justify="right"), "\n")
              }
              invisible(callNextMethod()) # summary
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
              rval@metadata$numberOfSamples <- dim(rval@data$v)[1]
              ## The general method didn't know that 'distance' was special, and should
              ## not be concatenated, so undo that.
              rval@data$distance <- object@data$distance
              rval
          })

#' @title Extract Something from of an adp Object
#'
#' In addition to the usual extraction of elements by name, some shortcuts
#' are also provided, e.g. \code{x[["u1"]]} retrieves \code{v[,1]}, and similarly
#' for the other velocity components. The \code{a} and \code{q}
#' data can be retrieved in \code{\link{raw}} form or numeric
#' form (see examples). The coordinate system may be
#' retrieved with e.g. \code{x[["coordinate"]]}.
#'
#' @param x An \code{adp} object, i.e. one inheriting from \code{\link{adp-class}}.
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
#' @family things related to \code{adp} data
setMethod(f="[[",
          signature(x="adp", i="ANY", j="ANY"),
          definition=function(x, i, j, ...) {
              if (i == "a") {
                  if (!missing(j) && j == "numeric") {
                      res <- x@data$a
                      dim <- dim(res)
                      res <- as.numeric(res)
                      dim(res) <- dim
                  } else {
                      res <- x@data$a
                  }
                  res
              } else if (i == "q") {
                  if (!missing(j) && j == "numeric") {
                      res <- x@data$q
                      dim <- dim(res)
                      res <- as.numeric(res)
                      dim(res) <- dim
                  } else {
                      res <- x@data$q
                  }
                  res
              } else if (i == "g") {
                  if (!missing(j) && j == "numeric") {
                      res <- x@data$g
                      dim <- dim(res)
                      res <- as.numeric(res)
                      dim(res) <- dim
                  } else {
                      res <- x@data$g
                  }
                  res
              } else if (i == "va") {
                  if (!missing(j) && j == "numeric") {
                      res <- x@data$va
                      dim <- dim(res)
                      res <- as.numeric(res)
                      dim(res) <- dim
                  } else {
                      res <- x@data$va
                  }
                  res
              } else if (i == "vq") {
                  if (!missing(j) && j == "numeric") {
                      res <- x@data$vq
                      dim <- dim(res)
                      res <- as.numeric(res)
                      dim(res) <- dim
                  } else {
                      res <- x@data$vq
                  }
                  res
              } else if (i == "vg") {
                  if (!missing(j) && j == "numeric") {
                      res <- x@data$vg
                      dim <- dim(res)
                      res <- as.numeric(res)
                      dim(res) <- dim
                  } else {
                      res <- x@data$vg
                  }
                  res
              } else if (i == "coordinate") {
                  res <- x@metadata$oceCoordinate
              } else {
                  callNextMethod()     # [[
              }
          })

#' @title Replace Parts of an ADP Object
#' @param x An \code{adp} object, i.e. one inheriting from \code{\link{adp-class}}.
#' @template sub_subsetTemplate
#'
#' @details
#' In addition to the usual insertion of elements by name, note
#' that e.g. \code{pitch} gets stored into \code{pitchSlow}.
#'
#' @author Dan Kelley
#'
#' @family things related to \code{adp} data
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
#' is somewhat analogous to \code{\link{subset.data.frame}}.  Subsetting can be by
#' \code{time} or \code{distance}, but these may not be combined; use a sequence
#' of calls to subset by both.
#'
#' @param x An \code{adp} object, i.e. one inheriting from \code{\link{adp-class}}.
#'
#' @param subset A condition to be applied to the \code{data} portion of
#' \code{x}.  See \sQuote{Details}.
#'
#' @param ... Ignored.
#'
#' @return A new \code{\link{adp-class}} object.
#'
#' @examples
#' library(oce)
#' data(adp)
#' # First part of time series
#' plot(subset(adp, time < mean(range(adp[['time']]))))
#'
#' @author Dan Kelley
#'
#' @family things related to \code{adp} data
#' @family functions that subset \code{oce} objects
setMethod(f="subset",
          signature="adp",
          definition=function(x, subset, ...) {
              subsetString <- paste(deparse(substitute(subset)), collapse=" ")
              res <- x
              dots <- list(...)
              debug <- getOption("oceDebug")
              if (length(dots) && ("debug" %in% names(dots)))
                  debug <- dots$debug
              if (missing(subset))
                  stop("must give 'subset'")
              if (length(grep("time", subsetString))) {
                  oceDebug(debug, "subsetting an adp by time\n")
                  if (length(grep("distance", subsetString)))
                      stop("cannot subset by both time and distance; split into multiple calls")
                  keep <- eval(substitute(subset), x@data, parent.frame(2))
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
                  ## FIXME: are we handling slow timescale data?
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
              } else if (length(grep("distance", subsetString))) {
                  oceDebug(debug, "subsetting an adp by distance\n")
                  if (length(grep("time", subsetString)))
                      stop("cannot subset by both time and distance; split into multiple calls")
                  keep <- eval(substitute(subset), x@data, parent.frame(2))
                  oceDebug(debug, vectorShow(keep, "keeping bins:"), "\n")
                  if (sum(keep) < 2)
                      stop("must keep at least 2 bins")
                  res <- x
                  res@data$distance <- x@data$distance[keep]
                  for (name in names(x@data)) {
                      if (name == "time")
                          next
                      if (is.array(x@data[[name]]) && 3 == length(dim(x@data[[name]]))) {
                          oceDebug(debug, "subsetting array data[[", name, "]] by distance\n")
                          oceDebug(debug, "before, dim(", name, ") =", dim(res@data[[name]]), "\n")
                          res@data[[name]] <- x@data[[name]][, keep, , drop=FALSE]
                          oceDebug(debug, "after, dim(", name, ") =", dim(res@data[[name]]), "\n")
                      }
                  }
              } else if (length(grep("pressure", subsetString))) {
                  keep <- eval(substitute(subset), x@data, parent.frame(2))
                  res <- x
                  res@data$v <- res@data$v[keep, , ]
                  res@data$a <- res@data$a[keep, , ]
                  res@data$q <- res@data$q[keep, , ]
                  res@data$time <- res@data$time[keep]
                  ## the items below may not be in the dataset
                  names <- names(res@data)
                  if ("bottomRange" %in% names) res@data$bottomRange <- res@data$bottomRange[keep, ]
                  if ("pressure" %in% names) res@data$pressure <- res@data$pressure[keep]
                  if ("temperature" %in% names) res@data$temperature <- res@data$temperature[keep]
                  if ("salinity" %in% names) res@data$salinity <- res@data$salinity[keep]
                  if ("depth" %in% names) res@data$depth <- res@data$depth[keep]
                  if ("heading" %in% names) res@data$heading <- res@data$heading[keep]
                  if ("pitch" %in% names) res@data$pitch <- res@data$pitch[keep]
                  if ("roll" %in% names) res@data$roll <- res@data$roll[keep]
              } else {
                  stop("should express the subset in terms of distance or time")
              }
              res@metadata$numberOfSamples <- dim(res@data$v)[1]
              res@metadata$numberOfCells <- dim(res@data$v)[2]
              res@processingLog <- processingLogAppend(res@processingLog, paste("subset.adp(x, subset=", subsetString, ")", sep=""))
              res
          })

#' Create an ADP Object
#'
#' @details
#' Construct an object of \code{\link{adp-class}}.  Only a basic
#' subset of the typical \code{data} slot is represented in the arguments
#' to this function, on the assumption that typical usage in reading data
#' is to set up a nearly-blank \code{\link{adp-class}} object, the \code{data}
#' slot of which is then inserted.  However, in some testing situations it
#' can be useful to set up artificial \code{adp} objects, so the other
#' arguments may be useful.
#'
#' @param time of observations in POSIXct format
#' @param distance to centre of bins
#' @param v array of velocities, with first index for time, second for bin number, and third for beam number
#' @param a amplitude, a \code{\link{raw}} array with dimensions matching \code{u}
#' @param q quality, a \code{\link{raw}} array with dimensions matching \code{u}
#' @param orientation a string indicating sensor orientation, e.g. \code{"upward"} and \code{"downward"}
#' @param coordinate a string indicating the coordinate system, \code{"enu"}, \code{"beam"}, \code{"xy"}, or \code{"other"}
#' @return An object of \code{\link{adp-class}}.
#'
#' @examples
#' data(adp)
#' t <- adp[["time"]]
#' d <- adp[["distance"]]
#' v <- adp[["v"]]
#' a <- as.adp(time=t, distance=d, v=v)
#' \dontrun{
#' plot(a)
#' }
#'
#' @author Dan Kelley
#'
#' @family things related to \code{adp} data
as.adp <- function(time, distance, v, a=NULL, q=NULL, orientation="upward", coordinate="enu")
{
    res <- new("adp", time=time, distance=distance, v=v, a=a, q=q)
    if (!missing(v)) {
        res@metadata$numberOfBeams <- dim(v)[3]
        res@metadata$numberOfCells <- dim(v)[2]
    }
    res@metadata$oceCoordinate <- coordinate
    res@metadata$orientation <- orientation
    res@metadata$cellSize <- if (missing(distance)) NA else diff(distance[1:2])
    res@metadata$units <- list(v="m/s", distance="m")
    res
}


## head.adp <- function(x, n=6L, ...)
## {
##     numberOfProfiles <- dim(x@data$v)[1]
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
##     numberOfProfiles <- dim(x@data$v)[1]
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
#' @param x An \code{adp} object, i.e. one inheriting from \code{\link{adp-class}}.
#' @param which an integer indicating beam number.
#' @return A character string containing a reasonable name for the beam, of the
#' form \code{"beam 1"}, etc., for beam coordinates, \code{"east"}, etc. for
#' enu coordinates, \code{"u"}, etc. for \code{"xyz"}, or \code{"u'"}, etc.,
#' for \code{"other"} coordinates.  The coordinate system is determined
#' with \code{x[["coordinate"]]}.
#' @author Dan Kelley
#' @seealso This is used by \code{\link{read.oce}}.
#' @family things related to \code{adp} data
#' @family things related to \code{adv} data
beamName <- function(x, which)
{
    if (x@metadata$oceCoordinate == "beam") {
        paste(gettext("beam", domain="R-oce"), 1:4)[which]
    } else if (x@metadata$oceCoordinate == "enu") {
        c(gettext("east", domain="R-oce"),
          gettext("north", domain="R-oce"),
          gettext("up", domain="R-oce"),
          gettext("error", domain="R-oce"))[which]
    } else if (x@metadata$oceCoordinate == "xyz") {
        c("u", "v", "w", "e")[which]
    } else if (x@metadata$oceCoordinate == "other") {
        c("u'", "v'", "w'", "e")[which]
    } else {
        " "
    }
}


#' Read an ADP File
#'
#' Read an ADP data file, producing an \code{adp} object, i.e. one inheriting
#' from \code{\link{adp-class}}.
#'
#' Several file types can be handled.  Some of
#' these functions are wrappers that map to device names, e.g.
#' \code{read.aquadoppProfiler} does its work by calling
#' \code{read.adp.nortek}; in this context, it is worth noting that the
#' ``aquadopp'' instrument is a one-cell profiler that might just as well have
#' been documented under the heading \code{\link{read.adv}}.
#'
#' @param manufacturer a character string indicating the manufacturer, used by
#' the general function \code{read.adp} to select a subsidiary function to use,
#' such as \code{read.adp.nortek}.
#' @param despike if \code{TRUE}, \code{\link{despike}} will be used to clean
#' anomalous spikes in heading, etc.
#' @template adpTemplate
#'
#' @author Dan Kelley and Clark Richards
#'
#' @family things related to \code{adp} data
read.adp <- function(file, from, to, by, tz=getOption("oceTz"),
                     longitude=NA, latitude=NA,
                     manufacturer=c("rdi", "nortek", "sontek"),
                     monitor=FALSE, despike=FALSE, processingLog,
                     debug=getOption("oceDebug"),
                     ...)
{
    oceDebug(debug, "read.adp(...,from=", from,
             ",to=", if (missing(to)) "(missing)" else to,
             ",by=", by,
             ",manufacturer=", if (missing(manufacturer)) "(missing)" else manufacturer, ",...)\n")
    manufacturer <- match.arg(manufacturer)
    if (monitor)
        cat(file, "\n", ...)
    if (manufacturer == "rdi")
        read.adp.rdi(file=file, from=from, to=to, by=by, tz=tz,
                     longitude=longitude, latitude=latitude,
                     debug=debug-1, monitor=monitor, despike=despike,
                     processingLog=processingLog, ...)
    else if (manufacturer == "nortek")
        read.adp.nortek(file=file, from=from, to=to, by=by, tz=tz,
                        longitude=longitude, latitude=latitude,
                        debug=debug-1, monitor=monitor, despike=despike,
                        processingLog=processingLog, ...)
    else if (manufacturer == "sontek")
        read.adp.sontek(file=file, from=from, to=to, by=by, tz=tz,
                        longitude=longitude, latitude=latitude,
                        debug=debug-1, monitor=monitor, despike=despike,
                        processingLog=processingLog, ...)
}


#' Plot ADP Data
#'
#' Create a summary plot of data measured by an acoustic doppler profiler.
#'
#' The plot may have one or more panels, with the content being controlled by
#' the \code{which} argument.
#'
#' \itemize{
#'
#' \item \code{which=1:4} (or \code{which="u1"} to \code{"u4"}) yield a
#' distance-time image plot of a velocity component.  If \code{x} is in
#' \code{beam} coordinates (signalled by
#' \code{metadata$oce.coordinate=="beam"}), this will be the beam velocity,
#' labelled \code{b[1]} etc.  If \code{x} is in xyz coordinates (sometimes
#' called frame coordinates, or ship coordinates), it will be the velocity
#' component to the right of the frame or ship (labelled \code{u} etc).
#' Finally, if \code{x} is in \code{"enu"} coordinates, the image will show the
#' the eastward component (labelled \code{east}).  If \code{x} is in
#' \code{"other"} coordinates, it will be component corresponding to east,
#' after rotation (labelled \code{u\'}).  Note that the coordinate is set by
#' \code{\link{read.adp}}, or by \code{\link{beamToXyzAdp}},
#' \code{\link{xyzToEnuAdp}}, or \code{\link{enuToOtherAdp}}.
#'
#' \item \code{which=5:8} (or \code{which="a1"} to \code{"a4"}) yield
#' distance-time images of backscatter intensity of the respective beams.  (For
#' data derived from Teledyn-RDI instruments, this is the item called ``echo
#' intensity.'')
#'
#' \item \code{which=9:12} (or \code{which="q1"} to \code{"q4"}) yield
#' distance-time images of signal quality for the respective beams.  (For RDI
#' data derived from instruments, this is the item called ``correlation
#' magnitude.'')
#'
#' \item \code{which=60} or \code{which="map"} draw a map of location(s).
#'
#' \item \code{which=70:73} (or \code{which="g1"} to \code{"g4"}) yield
#' distance-time images of percent-good for the respective beams.  (For data
#' derived from Teledyne-RDI instruments, which are the only instruments that
#' yield this item, it is called ``percent good.'')
#'
#' \item \code{which=80:83} (or \code{which="vv"}, \code{which="va"},
#' \code{which="vq"}, and \code{which="vg"}) yield distance-time
#' images of the vertical beam fields for a 5 beam "SentinelV" ADCP
#' from Teledyne RDI.
#'
#' \item \code{which="vertical"} yields a two panel distance-time
#' image of vertical beam velocity and amplitude.
#'
#' \item \code{which=13} (or \code{which="salinity"}) yields a time-series plot
#' of salinity.
#'
#' \item \code{which=14} (or \code{which="temperature"}) yields a time-series
#' plot of temperature.
#'
#' \item \code{which=15} (or \code{which="pressure"}) yields a time-series plot
#' of pressure.
#'
#' \item \code{which=16} (or \code{which="heading"}) yields a time-series plot
#' of instrument heading.
#'
#' \item \code{which=17} (or \code{which="pitch"}) yields a time-series plot of
#' instrument pitch.
#'
#' \item \code{which=18} (or \code{which="roll"}) yields a time-series plot of
#' instrument roll.
#'
#' \item \code{which=19} yields a time-series plot of distance-averaged
#' velocity for beam 1, rightward velocity, eastward velocity, or
#' rotated-eastward velocity, depending on the coordinate system.
#'
#' \item \code{which=20} yields a time-series of distance-averaged velocity for
#' beam 2, foreward velocity, northward velocity, or rotated-northward
#' velocity, depending on the coordinate system.
#'
#' \item \code{which=21} yields a time-series of distance-averaged velocity for
#' beam 3, up-frame velocity, upward velocity, or rotated-upward velocity,
#' depending on the coordinate system.
#'
#' \item \code{which=22} yields a time-series of distance-averaged velocity for
#' beam 4, for \code{beam} coordinates, or velocity estimate, for other
#' coordinates.  (This is ignored for 3-beam data.)
#'
#' \item \code{which=23} yields a progressive-vector diagram in the horizontal
#' plane, plotted with \code{asp=1}.  Normally, the depth-averaged velocity
#' components are used, but if the \code{control} list contains an item named
#' \code{bin}, then the depth bin will be used (with an error resulting if the
#' bin is out of range).
#'
#' \item \code{which=24} yields a time-averaged profile of the first component
#' of velocity (see \code{which=19} for the meaning of the component, in
#' various coordinate systems).
#'
#' \item \code{which=25} as for 24, but the second component.
#'
#' \item \code{which=26} as for 24, but the third component.
#'
#' \item \code{which=27} as for 24, but the fourth component (if that makes
#' sense, for the given instrument).
#'
#' \item \code{which=28} or \code{"uv"} yields velocity plot in the horizontal
#' plane, i.e. u[2] versus u[1].  If the number of data points is small, a
#' scattergraph is used, but if it is large, \code{\link{smoothScatter}} is
#' used.
#'
#' \item \code{which=29} or \code{"uv+ellipse"} as the \code{"uv"} case, but
#' with an added indication of the tidal ellipse, calculated from the eigen
#' vectors of the covariance matrix.
#'
#' \item \code{which=30} or \code{"uv+ellipse+arrow"} as the
#' \code{"uv+ellipse"} case, but with an added arrow indicating the mean
#' current.
#'
#' \item \code{which=40} or \code{"bottomRange"} for average bottom range from
#' all beams of the instrument.
#'
#' \item \code{which=41} to \code{44} (or \code{"bottomRange1"} to
#' \code{"bottomRange4"}) for bottom range from beams 1 to 4.
#'
#' \item \code{which=50} or \code{"bottomVelocity"} for average bottom velocity
#' from all beams of the instrument.
#'
#' \item \code{which=51} to \code{54} (or \code{"bottomVelocity1"} to
#' \code{"bottomVelocity4"}) for bottom velocity from beams 1 to 4.
#'
#' \item \code{which=55} (or \code{"heaving"}) for time-integrated,
#' depth-averaged, vertical velocity, i.e. a time series of heaving.
#'
#' \item \code{which=100} (or \code{"soundSpeed"}) for a time series of sound
#' speed.
#'
#' } In addition to the above, there are some groupings defined: \itemize{
#' \item \code{which="velocity"} equivalent to \code{which=1:3} (velocity
#' components) \item \code{which="amplitude"} equivalent to \code{which=5:7}
#' (backscatter intensity components) \item \code{which="quality"} equivalent
#' to \code{which=9:11} (quality components) \item \code{which="hydrography"}
#' equivalent to \code{which=14:15} (temperature and pressure) \item
#' \code{which="angles"} equivalent to \code{which=16:18} (heading, pitch and
#' roll) }
#'
#' The color scheme for image plots (\code{which} in 1:12) is provided by the
#' \code{col} argument, which is passed to \code{\link{image}} to do the actual
#' plotting.  See \dQuote{Examples} for some comparisons.
#'
#' A common quick-look plot to assess mooring movement is to use
#' \code{which=15:18} (pressure being included to signal the tide, and tidal
#' currents may dislodge a mooring or cause it to settle).
#'
#' By default, \code{plot,adp-method} uses a \code{zlim} value for the
#' \code{\link{image}} that is constructed to contain all the data, but to be
#' symmetric about zero.  This is done on a per-panel basis, and the scale is
#' plotted at the top-right corner, along with the name of the variable being
#' plotted. You may also supply \code{zlim} as one of the \dots{} arguments,
#' but be aware that a reasonable limit on horizontal velocity components is
#' unlikely to be of much use for the vertical component.
#'
#' A good first step in the analysis of measurements made from a moored device
#' (stored in \code{d}, say) is to do \code{plot(d, which=14:18)}.  This shows
#' time series of water properties and sensor orientation, which is helpful in
#' deciding which data to trim at the start and end of the deployment, because
#' they were measured on the dock or on the ship as it travelled to the mooring
#' site.
#'
#' @param x An \code{adp} object, i.e. one inheriting from \code{\link{adp-class}}.
#' @param which list of desired plot types.  These are graphed in panels
#' running down from the top of the page.  See \dQuote{Details} for the
#' meanings of various values of \code{which}.
#' @param mode a string indicating whether to plot the conventional signal
#' (\code{normal}) or or, in the special case of Aquadopp single-bin profilers,
#' possibly the \code{diagnostic} signal.  This argument is ignored except in
#' the case of Aquadopp instruments.  Perhaps a third option will become
#' available in the future, for the \code{burst} mode that some instruments
#' provide.
#' @param col optional indication of color(s) to use.  If not provided, the
#' default for images is \code{oce.colorsPalette(128,1)}, and for lines and
#' points is black.
#' @param breaks optional breaks for color scheme
#' @param zlim a range to be used as the \code{zlim} parameter to the
#' \code{\link{imagep}} call that is used to create the image.  If omitted,
#' \code{zlim} is set for each panel individually, to encompass the data of the
#' panel and to be centred around zero.  If provided as a two-element vector,
#' then that is used for each panel.  If provided as a two-column matrix, then
#' each panel of the graph uses the corresponding row of the matrix; for
#' example, setting \code{zlim=rbind(c(-1,1),c(-1,1),c(-.1,.1))} might make
#' sense for \code{which=1:3}, so that the two horizontal velocities have one
#' scale, and the smaller vertical velocity has another.
#' @param titles optional vector of character strings to be used as labels for
#' the plot panels.  For images, these strings will be placed in the right hand
#' side of the top margin.  For timeseries, these strings are ignored.  If this
#' is provided, its length must equal that of \code{which}.
#' @param lwd if the plot is of a time-series or scattergraph format with
#' lines, this is used in the usual way; otherwise, e.g. for image formats,
#' this is ignored.
#' @param type if the plot is of a time-series or scattergraph format, this is
#' used in the usual way, e.g. \code{"l"} for lines, etc.; otherwise, as for
#' image formats, this is ignored.
#' @param ytype character string controlling the type of the y axis for images
#' (ignored for time series).  If \code{"distance"}, then the y axis will be
#' distance from the sensor head, with smaller distances nearer the bottom of
#' the graph.  If \code{"profile"}, then this will still be true for
#' upward-looking instruments, but the y axis will be flipped for
#' downward-looking instruments, so that in either case, the top of the graph
#' will represent the sample nearest the sea surface.
#'
#' @param drawTimeRange boolean that applies to panels with time as the
#' horizontal axis, indicating whether to draw the time range in the top-left
#' margin of the plot.
#' @param useSmoothScatter boolean that indicates whether to use
#' \code{\link{smoothScatter}} in various plots, such as \code{which="uv"}.  If
#' not provided a default is used, with \code{\link{smoothScatter}} being used
#' if there are more than 2000 points to plot.
#' @param missingColor color used to indicate \code{NA} values in images (see
#' \code{\link{imagep}}); set to \code{NULL} to avoid this indication.
#' @template mgpTemplate
#' @template marTemplate
#' @param mai.palette margins, in inches, to be added to those calculated for
#' the palette; alter from the default only with caution
#' @param tformat optional argument passed to \code{\link{oce.plot.ts}}, for
#' plot types that call that function.  (See \code{\link{strptime}} for the
#' format used.)
#' @param marginsAsImage boolean, \code{TRUE} to put a wide margin to the right
#' of time-series plots, even if there are no images in the \code{which} list.
#' (The margin is made wide if there are some images in the sequence.)
#' @param cex size of labels on axes; see \code{\link[graphics]{par}}("cex").
#' @param cex.axis see \code{\link[graphics]{par}}("cex.axis").
#' @param cex.main see \code{\link[graphics]{par}}("cex.main").
#' @param xlim optional 2-element list for \code{xlim}, or 2-column matrix, in
#' which case the rows are used, in order, for the panels of the graph.
#' @param ylim optional 2-element list for \code{ylim}, or 2-column matrix, in
#' which case the rows are used, in order, for the panels of the graph.
#' @param control optional list of parameters that may be used for different
#' plot types.  Possibilities are \code{drawBottom} (a boolean that indicates
#' whether to draw the bottom) and \code{bin} (a numeric giving the index of
#' the bin on which to act, as explained in \dQuote{Details}).
#' @param useLayout set to \code{FALSE} to prevent using \code{\link{layout}}
#' to set up the plot.  This is needed if the call is to be part of a sequence
#' set up by e.g. \code{par(mfrow)}.
#' @param coastline a \code{coastline} object, or a character string naming
#' one.  This is used only for \code{which="map"}.  See notes at
#' \code{\link{plot,ctd-method}} for more information on built-in coastlines.
#' @param span approximate span of map in km
#' @param main main title for plot, used just on the top panel, if there are
#' several panels.
#' @param grid if \code{TRUE}, a grid will be drawn for each panel.  (This
#' argument is needed, because calling \code{\link{grid}} after doing a
#' sequence of plots will not result in useful results for the individual
#' panels.
#' @param grid.col color of grid
#' @param grid.lty line type of grid
#' @param grid.lwd line width of grid
#' @template debugTemplate
#' @param \dots optional arguments passed to plotting functions.  For example,
#' supplying \code{despike=TRUE} will cause time-series panels to be de-spiked
#' with \code{\link{despike}}.  Another common action is to set the color for
#' missing values on image plots, with the argument \code{missingColor} (see
#' \code{\link{imagep}}).  Note that it is an error to give \code{breaks} in
#' \dots{}, if the formal argument \code{zlim} was also given, because they
#' could contradict each other.
#' @return A list is silently returned, containing \code{xat} and \code{yat},
#' values that can be used by \code{\link{oce.grid}} to add a grid to the plot.
#' @examples
#' library(oce)
#' data(adp)
#' plot(adp, which=1:3)
#' plot(adp, which='temperature', tformat='%H:%M')
#'
#' @author Dan Kelley
#' @family functions that plot \code{oce} data
#' @family things related to \code{adp} data
#' @aliases plot.adp
setMethod(f="plot",
          signature=signature("adp"),
          definition=function(x, which=1:dim(x@data$v)[3], mode=c("normal", "diagnostic"),
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
                              cex=par("cex"), cex.axis=par("cex.axis"), cex.main=par("cex.main"),
                              xlim, ylim,
                              control,
                              useLayout=FALSE,
                              coastline="coastlineWorld", span=300,
                              main="",
                              grid=FALSE, grid.col='darkgray', grid.lty='dotted', grid.lwd=1,
                              debug=getOption("oceDebug"),
                              ...)
          {
              debug <- max(0, min(debug, 4))
              if ("adorn" %in% names(list(...)))
                  warning("In plot,adp-method() : the 'adorn' argument was removed in November 2017", call.=FALSE)
              colGiven <- !missing(col)
              breaksGiven <- !missing(breaks)
              zlimGiven <- !missing(zlim)
              if (breaksGiven && zlimGiven)
                  stop("cannot supply both zlim and breaks")
              ylimGiven <- !missing(ylim)
              oceDebug(debug, 'ylimGiven=', ylimGiven, '\n')
              res <- list(xat=NULL, yat=NULL)
              mode <- match.arg(mode)
              if (mode == "diagnostic") {
                  if (x@metadata$instrumentType != "aquadopp") {
                      warning("This instrument is not a Nortek Aquadopp, so mode=\"diagnostic\" is being ignored")
                      mode <- 'normal'
                  }
                  if (x@metadata$numberOfCells != 1) {
                      warning("This instrument seems to be a Nortek Aquadopp, but it has more than 1 cell, so it must not be; so mode=\"diagnostic\" is being ignored")
                      mode <- 'normal'
                  }
                  if (!("timeDia" %in% names(x@data))) {
                      warning("This instrument did not record Diagnostic data, so mode=\"diagnostic\" is being ignored")
                      mode <- 'normal'
                  }
              }
              oceDebug(debug, "plot,adp-method(x, which=\"", paste(which, collapse=","),
                       "\", breaks=", if (missing(breaks)) "(missing)" else
                           paste("c(", paste(breaks, collapse=", "), ")", sep=""),
                       ", mode=\"", mode, "\", ...) {\n", sep="", unindent=1)
              oceDebug(debug, "par(mar)=", paste(par('mar'), collapse=" "), "\n")
              oceDebug(debug, "par(mai)=", paste(par('mai'), collapse=" "), "\n")
              oceDebug(debug, "par(mfg)=", paste(par('mfg'), collapse=" "), "\n")
              oceDebug(debug, "mai.palette=", paste(mai.palette, collapse=" "), "\n")
              if (ylimGiven)
                  oceDebug(debug, "ylim=c(", paste(ylim, collapse=", "), ")\n")
              if (!inherits(x, "adp"))
                  stop("method is only for objects of class '", "adp", "'")
              if (!(is.null(x@metadata$haveActualData) || x@metadata$haveActualData)) {
                  warning("there are no profiles in this dataset")
                  return()
              }
              opar <- par(no.readonly = TRUE)
              nw <- length(which)
              nbeams  <- x@metadata$numberOfBeams
              if (nw == 1) {
                  pm <- pmatch(which, c("velocity", "amplitude", "quality", "hydrography", "angles"))
                  if (!is.na(pm)) {
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
              dots <- list(...)
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
              oceDebug(debug, "later on in plot,adp-method:\n")
              oceDebug(debug, "  par(mar)=", paste(par('mar'), collapse=" "), "\n")
              oceDebug(debug, "  par(mai)=", paste(par('mai'), collapse=" "), "\n")

              oceDebug(debug, "which:", which, "\n")
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
                                       progressivevector=23,
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
              oceDebug(debug, "which:", which, "(after conversion to numerical codes)\n")
              images <- c(1:12, 70:73, 80:83)
              timeseries <- c(13:22, 40:44, 50:54, 55, 100)
              spatial <- 23:27
              #speed <- 28

              tt <- x@data$time
              ##ttDia <- x@data$timeDia  # may be null
              class(tt) <- "POSIXct"              # otherwise image() gives warnings
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
                  bottom <- apply(x@data$bottomRange, 1, mean)
              oceDebug(debug, "showBottom=", showBottom, "\n")
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
              flipy <- ytype == "profile" && x@metadata$orientation == "downward"
              numberOfCells <- x[["numberOfCells"]]
              haveTimeImages <- any(which %in% images) && 1 < numberOfCells
              oceDebug(debug, 'haveTimeImages=', haveTimeImages, '(if TRUE, it means any timeseries graphs get padding on RHS)\n')
              for (w in 1:nw) {
                  oceDebug(debug, "which[", w, "]=", which[w], "; drawTimeRange=", drawTimeRange, "\n")
                  if (which[w] %in% images) {
                      ## image types
                      skip <- FALSE
                      if (which[w] %in% 1:(x@metadata$numberOfBeams)) {
                          ## velocity
                          if (mode == "diagnostic") {
                              oceDebug(debug, "a diagnostic velocity component image/timeseries\n")
                              z <- x@data$vDia[, , which[w]]
                              zlab <- if (missing(titles)) paste(beamName(x, which[w]), "Dia", sep="") else titles[w]
                              y.look <- if (ylimGiven) ylimAsGiven[w, 1] <= x@data$distance & x@data$distance <= ylimAsGiven[w, 2] else rep(TRUE, length(x@data$distance))
                              zlim <- if (zlimGiven) zlimAsGiven[w, ] else
                                  max(abs(x@data$vDia[, y.look, which[w]]), na.rm=TRUE) * c(-1, 1)
                          } else {
                              oceDebug(debug, "a velocity component image/timeseries\n")
                              z <- x@data$v[, , which[w]]
                              zlab <- if (missing(titles)) beamName(x, which[w]) else titles[w]
                              y.look <- if (ylimGiven) ylimAsGiven[w, 1] <= x@data$distance & x@data$distance <= ylimAsGiven[w, 2] else rep(TRUE, length(x@data$distance))
                              if (0 == sum(y.look))
                                  stop("no data in the provided ylim=c(", paste(ylimAsGiven[w, ], collapse=","), ")")
                              zlim <- if (zlimGiven) zlimAsGiven[w, ] else {
                                  if (breaksGiven) NULL else max(abs(x@data$v[, y.look, which[w]]), na.rm=TRUE) * c(-1, 1)
                              }
                          }
                          oceDebug(debug, 'flipy=', flipy, '\n')
                      } else if (which[w] %in% 5:(4+x@metadata$numberOfBeams)) {
                          ## amplitude
                          if (mode == "diagnostic" && "aDia" %in% names(x@data)) {
                              oceDebug(debug, "a diagnostic amplitude component image/timeseries\n")
                              z <- as.numeric(x@data$aDia[, , which[w]-4])
                              dim(z) <- dim(x@data$aDia)[1:2]
                              y.look <- if (ylimGiven) ylimAsGiven[1] <= x@data$distance & x@data$distance <= ylimAsGiven[2] else rep(TRUE, length(x@data$distance))
                              zlim <- if (zlimGiven) zlimAsGiven[w, ] else {
                                  if (breaksGiven) NULL else range(as.numeric(x@data$aDia[, y.look, ]), na.rm=TRUE)
                              }
                              zlab <- c(expression(aDia[1]), expression(a[2]), expression(aDia[3]), expression(aDia[4]))[which[w]-4]
                          } else {
                              oceDebug(debug, "an amplitude component image/timeseries\n")
                              z <- as.numeric(x@data$a[, , which[w]-4])
                              dim(z) <- dim(x@data$a)[1:2]
                              y.look <- if (ylimGiven) ylimAsGiven[1] <= x@data$distance & x@data$distance <= ylimAsGiven[2] else rep(TRUE, length(x@data$distance))
                              zlim <- if (zlimGiven) zlimAsGiven[w, ] else {
                                  if (breaksGiven) NULL else range(as.numeric(x@data$a[, y.look, ]), na.rm=TRUE)
                              }
                              zlab <- c(expression(a[1]), expression(a[2]), expression(a[3]), expression(a[4]))[which[w]-4]
                          }
                      } else if (which[w] %in% 9:(8+x@metadata$numberOfBeams)) {
                          ## correlation
                          if ("q" %in% names(x@data)) {
                              z <- as.numeric(x@data$q[, , which[w]-8])
                              dim(z) <- dim(x@data$q)[1:2]
                              zlim <- c(0, 256)
                              zlab <- c(expression(q[1]), expression(q[2]), expression(q[3]))[which[w]-8]
                          } else if ("amp" %in% names(x@data)) {
                              z <- as.numeric(x@data$amp[, , which[w]-8])
                              dim(z) <- dim(x@data$amp)[1:2]
                              zlim <- c(0, max(as.numeric(x@data$amp)))
                              zlab <- c(expression(amp[1]), expression(amp[2]), expression(amp[3]))[which[w]-8]
                          }
                      } else if (which[w] %in% 70:(69+x@metadata$numberOfBeams)) {
                          ## correlation
                          if ("g" %in% names(x@data)) {
                              z <- as.numeric(x@data$g[, , which[w]-69])
                              dim(z) <- dim(x@data$g)[1:2]
                              zlim <- c(0, 100)
                              zlab <- c(expression(g[1]), expression(g[2]), expression(g[3]))[which[w]-8]
                          } else {
                              warning("ADP object lacks a 'g' data item")
                          }
                      } else if (which[w] == 80) {
                          ## vertical beam velocity
                          if ("vv" %in% names(x@data)) {
                              oceDebug(debug, "vertical beam velocity\n")
                              z <- x@data$vv
                              zlab <- if (missing(titles)) expression(w[vert]) else titles[w]
                              y.look <- if (ylimGiven) ylimAsGiven[w, 1] <= x@data$distance & x@data$distance <= ylimAsGiven[w, 2] else rep(TRUE, length(x@data$distance))
                              if (0 == sum(y.look))
                                  stop("no data in the provided ylim=c(", paste(ylimAsGiven[w, ], collapse=","), ")")
                              zlim <- if (zlimGiven) zlimAsGiven[w, ] else {
                                  if (breaksGiven) NULL else c(-1, 1)
                              }
                          } else {
                              warning("ADP object lacks a 'vv' data item")
                          }
                      } else if (which[w] == 81) {
                          ## vertical beam amplitude
                          if ("va" %in% names(x@data)) {
                              oceDebug(debug, "vertical beam amplitude\n")
                              z <- as.numeric(x@data$va)
                              dim(z) <- dim(x@data$va)
                              y.look <- if (ylimGiven) ylimAsGiven[1] <= x@data$distance & x@data$distance <= ylimAsGiven[2] else rep(TRUE, length(x@data$distance))
                              zlim <- if (zlimGiven) zlimAsGiven[w, ] else {
                                  if (breaksGiven) NULL else range(as.numeric(x@data$va[, y.look]), na.rm=TRUE)
                              }
                              zlab <- expression(a[vert])
                          } else {
                              warning("ADP object lacks a 'va' data item")
                          }
                      } else if (which[w] == 82) {
                          ## vertical beam correlation
                          if ("vq" %in% names(x@data)) {
                              oceDebug(debug, "vertical beam correlation\n")
                              z <- as.numeric(x@data$vq)
                              dim(z) <- dim(x@data$vq)
                              y.look <- if (ylimGiven) ylimAsGiven[1] <= x@data$distance & x@data$distance <= ylimAsGiven[2] else rep(TRUE, length(x@data$distance))
                              zlim <- if (zlimGiven) zlimAsGiven[w, ] else {
                                  if (breaksGiven) NULL else range(as.numeric(x@data$vq[, y.look]), na.rm=TRUE)
                              }
                              zlab <- expression(q[vert])
                          } else {
                              warning("ADP object lacks a 'vq' data item")
                          }
                      } else if (which[w] == 83) {
                          ## vertical beam percent good
                          if ("vg" %in% names(x@data)) {
                              oceDebug(debug, "vertical beam percent good\n")
                              z <- as.numeric(x@data$vg)
                              dim(z) <- dim(x@data$vg)
                              y.look <- if (ylimGiven) ylimAsGiven[1] <= x@data$distance & x@data$distance <= ylimAsGiven[2] else rep(TRUE, length(x@data$distance))
                              zlim <- if (zlimGiven) zlimAsGiven[w, ] else {
                                  if (breaksGiven) NULL else range(as.numeric(x@data$vg[, y.look]), na.rm=TRUE)
                              }
                              zlab <- expression(g[vert])
                          } else {
                              warning("ADP object lacks a 'vq' data item")
                          }
                      } else {
                          skip <- TRUE
                      }
                      if (!skip) {
                          if (numberOfCells > 1) {
                              if (xlimGiven) {
                                  ats <- imagep(x=tt, y=x@data$distance, z=z,
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
                                                mar=mar,
                                                mai.palette=mai.palette,
                                                cex=cex * (1 - min(nw / 8, 1/4)), # FIXME: should emulate par(mfrow)
                                                main=main[w],
                                                debug=debug-1,
                                                ...)
                              } else {
                                  ats <- imagep(x=tt, y=x@data$distance, z=z,
                                                zlim=zlim,
                                                flipy=flipy,
                                                ylim=if (ylimGiven) ylim[w, ] else
                                                    range(x@data$distance, na.rm=TRUE),
                                                    col=if (colGiven) col else {
                                                        if (missing(breaks)) oce.colorsPalette(128, 1)
                                                        else oce.colorsPalette(length(breaks)-1, 1)
                                                    },
                                                    breaks=breaks,
                                                    ylab=resizableLabel("distance"),
                                                    xlab="Time",
                                                    zlab=zlab,
                                                    tformat=tformat,
                                                    drawTimeRange=drawTimeRange,
                                                    drawContours=FALSE,
                                                    missingColor=missingColor,
                                                    mgp=mgp,
                                                    mar=mar,
                                                    mai.palette=mai.palette,
                                                    cex=cex * (1 - min(nw / 8, 1/4)),
                                                    main=main[w],
                                                    debug=debug-1,
                                                    ...)
                              }
                              if (showBottom)
                                  lines(x@data$time, bottom)
                          } else {
                              col <- if (colGiven) rep(col, length.out=nw) else rep("black", length.out=nw)
                              time  <- if (mode== "diagnostic") x@data$timeDia else x@data$time
                              tlim <- range(time)
                              ats <- oce.plot.ts(time, z, ylab=zlab,
                                                 xlim=if (xlimGiven) xlim[w, ] else tlim,
                                                 ylim=if (ylimGiven) ylim[w, ],
                                                 xaxs="i",
                                                 col=col[w],
                                                 lwd=lwd[w],
                                                 cex=cex * (1 - min(nw / 8, 1/4)),
                                                 cex.axis=cex * (1 - min(nw / 8, 1/4)),
                                                 main=main[w],
                                                 type=type,
                                                 mgp=mgp,
                                                 mar=c(mgp[1], mgp[1]+1.5, 1.5, 1.5),
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
                      oceDebug(debug, "graph", w, "is a timeseries\n")
                      ##par(mgp=mgp, mar=mar, cex=cex)
                      tlim <- range(x@data$time)
                      if (which[w] == 13) {
                          if (haveTimeImages) drawPalette(debug=debug-1)
                          ats <- oce.plot.ts(x@data$time, x[["salinity"]],
                                             xlim=if (xlimGiven) xlim[w, ] else tlim,
                                             ylim=if (ylimGiven) ylim[w, ],
                                             xaxs="i",
                                             col=col[w],
                                             lwd=lwd[w],
                                             cex=cex * (1 - min(nw / 8, 1/4)),
                                             cex.axis=cex * (1 - min(nw / 8, 1/4)),
                                             main=main[w],
                                             ylab=resizableLabel("S"),
                                             type=type,
                                             mgp=mgp,
                                             mar=if (haveTimeImages) par('mar') else c(mgp[1], mgp[1]+1.5, 1.5, 1.5),
                                             drawTimeRange=drawTimeRange,
                                             tformat=tformat,
                                             debug=debug-1)
                      } else if (which[w] == 14) {
                          if (haveTimeImages) drawPalette(debug=debug-1, mai=mai.palette)
                          if (mode == "diagnostic" && "temperatureDia" %in% names(x@data)) {
                              ats <- oce.plot.ts(x@data$timeDia, x@data$temperatureDia,
                                                 xlim=if (xlimGiven) xlim[w, ] else tlim,
                                                 ylim=if (ylimGiven) ylim[w, ],
                                                 xaxs="i",
                                                 col=col[w],
                                                 lwd=lwd[w],
                                                 cex=cex * (1 - min(nw / 8, 1/4)),
                                                 cex.axis=cex * (1 - min(nw / 8, 1/4)),
                                                 main=main[w],
                                                 ylab=expression(paste("Diagnostic T [ ", degree, "C ]")),
                                                 type=type,
                                                 mgp=mgp,
                                                 mar=if (haveTimeImages) par('mar') else c(mgp[1], mgp[1]+1.5, 1.5, 1.5),
                                                 tformat=tformat,
                                                 debug=debug-1)
                          } else {
                              ats <- oce.plot.ts(x@data$time, x@data$temperature,
                                                 xlim=if (xlimGiven) xlim[w, ] else tlim,
                                                 ylim=if (ylimGiven) ylim[w, ],
                                                 xaxs="i",
                                                 col=col[w],
                                                 lwd=lwd[w],
                                                 cex=cex * (1 - min(nw / 8, 1/4)),
                                                 cex.axis=cex * (1 - min(nw / 8, 1/4)),
                                                 main=main[w],
                                                 ylab=expression(paste("T [ ", degree, "C ]")),
                                                 type=type,
                                                 mgp=mgp,
                                                 mar=if (haveTimeImages) par('mar') else c(mgp[1], mgp[1]+1.5, 1.5, 1.5),
                                                 tformat=tformat,
                                                 debug=debug-1)
                          }
                      } else if (which[w] == 15) {
                          if (haveTimeImages) drawPalette(debug=debug-1, mai=mai.palette)
                          if (mode == "diagnostic" && "pressureDia" %in% names(x@data)) {
                              ats <- oce.plot.ts(x@data$timeDia, x@data$pressureDia,
                                                 xlim=if (xlimGiven) xlim[w, ] else tlim,
                                                 ylim=if (ylimGiven) ylim[w, ],
                                                 xaxs="i",
                                                 col=col[w],
                                                 lwd=lwd[w],
                                                 cex=cex * (1 - min(nw / 8, 1/4)),
                                                 cex.axis=cex * (1 - min(nw / 8, 1/4)),
                                                 main=main[w],
                                                 ylab="pDia",
                                                 type=type,
                                                 mgp=mgp,
                                                 mar=if (haveTimeImages) par('mar') else c(mgp[1], mgp[1]+1.5, 1.5, 1.5),
                                                 drawTimeRange=drawTimeRange,
                                                 tformat=tformat,
                                                 debug=debug-1)
                          } else {
                              ats <- oce.plot.ts(x@data$time, x@data$pressure,
                                                 xlim=if (xlimGiven) xlim[w, ] else tlim,
                                                 ylim=if (ylimGiven) ylim[w, ],
                                                 xaxs="i",
                                                 col=col[w],
                                                 lwd=lwd[w],
                                                 cex=cex * (1 - min(nw / 8, 1/4)),
                                                 cex.axis=cex * (1 - min(nw / 8, 1/4)),
                                                 main=main[w],
                                                 ylab=resizableLabel("p"),
                                                 type=type,
                                                 mgp=mgp,
                                                 mar=if (haveTimeImages) par('mar') else c(mgp[1], mgp[1]+1.5, 1.5, 1.5),
                                                 drawTimeRange=drawTimeRange,
                                                 tformat=tformat,
                                                 debug=debug-1)
                          }
                      } else if (which[w] == 16) {
                          if (haveTimeImages) drawPalette(debug=debug-1, mai=mai.palette)
                          if (mode == "diagnostic" && "headingDia" %in% names(x@data)) {
                              ats <- oce.plot.ts(x@data$timeDia, x@data$headingDia,
                                                 xlim=if (xlimGiven) xlim[w, ] else tlim,
                                                 ylim=if (ylimGiven) ylim[w, ],
                                                 xaxs="i",
                                                 col=col[w],
                                                 lwd=lwd[w],
                                                 cex=cex * (1 - min(nw / 8, 1/4)),
                                                 cex.axis=cex * (1 - min(nw / 8, 1/4)),
                                                 main=main[w],
                                                 ylab="headingDia",
                                                 type=type,
                                                 mgp=mgp,
                                                 mar=if (haveTimeImages) par('mar') else c(mgp[1], mgp[1]+1.5, 1.5, 1.5),
                                                 drawTimeRange=drawTimeRange,
                                                 tformat=tformat,
                                                 debug=debug-1)
                          } else {
                              ats <- oce.plot.ts(x@data$time, x@data$heading,
                                                 xlim=if (xlimGiven) xlim[w, ] else tlim,
                                                 ylim=if (ylimGiven) ylim[w, ],
                                                 xaxs="i",
                                                 col=col[w],
                                                 lwd=lwd[w],
                                                 cex=cex * (1 - min(nw / 8, 1/4)),
                                                 cex.axis=cex * (1 - min(nw / 8, 1/4)),
                                                 main=main[w],
                                                 ylab=resizableLabel("heading"),
                                                 type=type,
                                                 mgp=mgp,
                                                 mar=if (haveTimeImages) par('mar') else c(mgp[1], mgp[1]+1.5, 1.5, 1.5),
                                                 drawTimeRange=drawTimeRange,
                                                 tformat=tformat,
                                                 debug=debug-1)
                          }
                      } else if (which[w] == 17) {
                          if (haveTimeImages) drawPalette(debug=debug-1, mai=mai.palette)
                          if (mode == "diagnostic" && "pitchDia" %in% names(x@data)) {
                              ats <- oce.plot.ts(x@data$timeDia, x@data$pitchDia,
                                                 xlim=if (xlimGiven) xlim[w, ] else tlim,
                                                 ylim=if (ylimGiven) ylim[w, ],
                                                 xaxs="i",
                                                 col=col[w],
                                                 lwd=lwd[w],
                                                 cex=cex * (1 - min(nw / 8, 1/4)),
                                                 cex.axis=cex * (1 - min(nw / 8, 1/4)),
                                                 main=main[w],
                                                 ylab="pitchDia",
                                                 type=type,
                                                 mgp=mgp,
                                                 mar=if (haveTimeImages) par('mar') else c(mgp[1], mgp[1]+1.5, 1.5, 1.5),
                                                 drawTimeRange=drawTimeRange,
                                                 tformat=tformat,
                                                 debug=debug-1)
                          } else {
                              ats <- oce.plot.ts(x@data$time, x@data$pitch,
                                                 xlim=if (xlimGiven) xlim[w, ] else tlim,
                                                 ylim=if (ylimGiven) ylim[w, ],
                                                 xaxs="i",
                                                 col=col[w],
                                                 lwd=lwd[w],
                                                 cex=cex * (1 - min(nw / 8, 1/4)),
                                                 cex.axis=cex * (1 - min(nw / 8, 1/4)),
                                                 main=main[w],
                                                 ylab=resizableLabel("pitch"),
                                                 type=type,
                                                 mgp=mgp,
                                                 mar=if (haveTimeImages) par('mar') else c(mgp[1], mgp[1]+1.5, 1.5, 1.5),
                                                 drawTimeRange=drawTimeRange,
                                                 tformat=tformat,
                                                 debug=debug-1)
                          }
                      } else if (which[w] == 18) {
                          if (haveTimeImages) drawPalette(debug=debug-1, mai=mai.palette)
                          if (mode == "diagnostic" && "rollDia" %in% names(x@data)) {
                              ats <- oce.plot.ts(x@data$timeDia, x@data$rollDia,
                                                 xlim=if (xlimGiven) xlim[w, ] else tlim,
                                                 ylim=if (ylimGiven) ylim[w, ],
                                                 xaxs="i",
                                                 col=col[w],
                                                 lwd=lwd[w],
                                                 cex=cex * (1 - min(nw / 8, 1/4)),
                                                 cex.axis=cex * (1 - min(nw / 8, 1/4)),
                                                 main=main[w],
                                                 ylab="rollDia",
                                                 type=type,
                                                 mgp=mgp,
                                                 mar=if (haveTimeImages) par('mar') else c(mgp[1], mgp[1]+1.5, 1.5, 1.5),
                                                 drawTimeRange=drawTimeRange,
                                                 tformat=tformat,
                                                 debug=debug-1)
                          } else {
                              ats <- oce.plot.ts(x@data$time, x@data$roll,
                                                 xlim=if (xlimGiven) xlim[w, ] else tlim,
                                                 ylim=if (ylimGiven) ylim[w, ],
                                                 xaxs="i",
                                                 col=col[w],
                                                 lwd=lwd[w],
                                                 cex=cex * (1 - min(nw / 8, 1/4)),
                                                 cex.axis=cex * (1 - min(nw / 8, 1/4)),
                                                 main=main[w],
                                                 ylab=resizableLabel("roll"),
                                                 type=type,
                                                 mgp=mgp,
                                                 mar=if (haveTimeImages) par('mar') else c(mgp[1], mgp[1]+1.5, 1.5, 1.5),
                                                 drawTimeRange=drawTimeRange,
                                                 tformat=tformat,
                                                 debug=debug-1)
                          }
                      } else if (which[w] == 19) {
                          if (x@metadata$numberOfBeams > 0) {
                              if (haveTimeImages) drawPalette(debug=debug-1, mai=mai.palette)
                              ats <- oce.plot.ts(x@data$time, apply(x@data$v[, , 1], 1, mean, na.rm=TRUE),
                                                 xlim=if (xlimGiven) xlim[w, ] else tlim,
                                                 ylim=if (ylimGiven) ylim[w, ],
                                                 xaxs="i",
                                                 col=col[w],
                                                 lwd=lwd[w],
                                                 cex=cex * (1 - min(nw / 8, 1/4)),
                                                 cex.axis=cex * (1 - min(nw / 8, 1/4)),
                                                 main=main[w],
                                                 ylab=beamName(x, 1),
                                                 type=type,
                                                 mgp=mgp,
                                                 mar=if (haveTimeImages) par('mar') else c(mgp[1], mgp[1]+1.5, 1.5, 1.5),
                                                 mai.palette=mai.palette,
                                                 drawTimeRange=drawTimeRange,
                                                 tformat=tformat,
                                                 debug=debug-1)
                          } else {
                              warning("cannot plot beam/velo 1 because the device no beams")
                          }
                      } else if (which[w] == 20) {
                          if (x@metadata$numberOfBeams > 1) {
                              if (haveTimeImages) drawPalette(debug=debug-1, mai=mai.palette)
                              ats <- oce.plot.ts(x@data$time, apply(x@data$v[, , 2], 1, mean, na.rm=TRUE),
                                                 xlim=if (xlimGiven) xlim[w, ] else tlim,
                                                 ylim=if (ylimGiven) ylim[w, ],
                                                 xaxs="i",
                                                 col=col[w],
                                                 lwd=lwd[w],
                                                 cex=cex * (1 - min(nw / 8, 1/4)),
                                                 cex.axis=cex * (1 - min(nw / 8, 1/4)),
                                                 main=main[w],
                                                 ylab=beamName(x, 2),
                                                 type=type,
                                                 mgp=mgp,
                                                 mar=if (haveTimeImages) par('mar') else c(mgp[1], mgp[1]+1.5, 1.5, 1.5),
                                                 mai.palette=mai.palette,
                                                 drawTimeRange=drawTimeRange,
                                                 tformat=tformat,
                                                 debug=debug-1)
                          } else {
                              warning("cannot plot beam/velo 2 because the device has only ", x@metadata$numberOfBeams, " beams")
                          }
                      } else if (which[w] == 21) {
                          if (x@metadata$numberOfBeams > 2) {
                              if (haveTimeImages) drawPalette(debug=debug-1, mai=mai.palette)
                              ats <- oce.plot.ts(x@data$time, apply(x@data$v[, , 3], 1, mean, na.rm=TRUE),
                                                 xlim=if (xlimGiven) xlim[w, ] else tlim,
                                                 ylim=if (ylimGiven) ylim[w, ],
                                                 xaxs="i",
                                                 col=col[w],
                                                 lwd=lwd[w],
                                                 cex=cex * (1 - min(nw / 8, 1/4)),
                                                 cex.axis=cex * (1 - min(nw / 8, 1/4)),
                                                 main=main[w],
                                                 ylab=beamName(x, 3),
                                                 type=type,
                                                 mgp=mgp,
                                                 mar=if (haveTimeImages) par('mar') else c(mgp[1], mgp[1]+1.5, 1.5, 1.5),
                                                 mai.palette=mai.palette,
                                                 drawTimeRange=drawTimeRange,
                                                 tformat=tformat,
                                                 debug=debug-1)
                          } else {
                              warning("cannot plot beam/velo 3 because the device has only", x@metadata$numberOfBeams, "beams")
                          }
                      } else if (which[w] == 22) {
                          if (x@metadata$numberOfBeams > 3) {
                              if (haveTimeImages) drawPalette(debug=debug-1, mai=mai.palette)
                              ats <- oce.plot.ts(x@data$time, apply(x@data$v[, , 4], 1, mean, na.rm=TRUE),
                                                 xlim=if (xlimGiven) xlim[w, ] else tlim,
                                                 ylim=if (ylimGiven) ylim[w, ],
                                                 xaxs="i",
                                                 col=col[w],
                                                 lwd=lwd[w],
                                                 cex=cex * (1 - min(nw / 8, 1/4)),
                                                 cex.axis=cex * (1 - min(nw / 8, 1/4)),
                                                 main=main[w],
                                                 ylab=beamName(x, 4),
                                                 type=type,
                                                 mgp=mgp,
                                                 mar=if (haveTimeImages) par('mar') else c(mgp[1], mgp[1]+1.5, 1.5, 1.5),
                                                 mai.palette=mai.palette,
                                                 drawTimeRange=drawTimeRange,
                                                 tformat=tformat,
                                                 debug=debug-1)
                          } else {
                              warning("cannot plot beam/velo 4 because the device has only", x@metadata$numberOfBeams, "beams")
                          }
                      } else  if (which[w] == 55) {
                                       ## heaving
                          if (haveTimeImages) drawPalette(debug=debug-1, mai=mai.palette)
                          dt <- as.numeric(x@data$time[2]) - as.numeric(x@data$time[1])
                          ats <- oce.plot.ts(x@data$time, dt * cumsum(apply(x@data$v[, , 3], 1, mean)),
                                             xlim=if (xlimGiven) xlim[w, ] else tlim,
                                             ylim=if (ylimGiven) ylim[w, ],
                                             xaxs="i",
                                             col=col[w],
                                             lwd=lwd[w],
                                             cex=cex * (1 - min(nw / 8, 1/4)),
                                             cex.axis=cex * (1 - min(nw / 8, 1/4)),
                                             main=main[w],
                                             ylab="Heaving [m]",
                                             type=type,
                                             mgp=mgp,
                                             mar=if (haveTimeImages) par('mar') else c(mgp[1], mgp[1]+1.5, 1.5, 1.5),
                                             mai.palette=mai.palette,
                                             drawTimeRange=drawTimeRange,
                                             tformat=tformat,
                                             debug=debug-1)
                          drawTimeRange <- FALSE
                      } else if (which[w] == 100) {
                          oceDebug(debug, "draw(ctd, ...) of type 'soundSpeed'\n")
                          if (haveTimeImages) drawPalette(debug=debug-1, mai=mai.palette)
                          ats <- oce.plot.ts(x[["time"]], x[["soundSpeed"]],
                                             xlim=if (xlimGiven) xlim[w, ] else tlim,
                                             ylim=if (ylimGiven) ylim[w, ],
                                             xaxs="i",
                                             col=col[w],
                                             lwd=lwd[w],
                                             cex=cex * (1 - min(nw / 8, 1/4)),
                                             cex.axis=cex * (1 - min(nw / 8, 1/4)),
                                             main=main[w],
                                             ylab="Sound Speed [m/s]",
                                             type=type,
                                             mgp=mgp,
                                             mar=if (haveTimeImages) par('mar') else c(mgp[1], mgp[1]+1.5, 1.5, 1.5),
                                             tformat=tformat,
                                             debug=debug-1)
                      } else if (which[w] %in% 40:44) {
                          ## bottomRange
                          par(mar=c(mgp[1]+1, mgp[1]+1, 1, 1))
                          n <- prod(dim(x@data$v)[1:2])
                          if ("br" %in% names(x@data)) {
                              if (which[w] == 40) {
                                  R <- apply(x@data$br, 1, mean, na.rm=TRUE)
                                  ats <- oce.plot.ts(x@data$time, R,
                                                     ylab="Bottom range [m]",
                                                     type=type,
                                                     xlim=if (xlimGiven) xlim[w, ] else tlim,
                                                     ylim=if (ylimGiven) ylim[w, ] else range(R, na.rm=TRUE),
                                                     tformat=tformat,
                                                     debug=debug-1)
                              } else {
                                  R <- x@data$br[, which[w]-40]
                                  ats <- oce.plot.ts(x@data$time, R,
                                                     ylab=paste("Beam", which[w]-40, "bottom range [m]"),
                                                     type=type,
                                                     xlim=if (xlimGiven) xlim[w, ] else tlim,
                                                     ylim=if (ylimGiven) ylim[w, ] else range(R, na.rm=TRUE),
                                                     tformat=tformat,
                                                     debug=debug-1)
                              }
                          } else {
                              warning("cannot handle which= ", which[w], " because this instrument lacked bottom tracking")
                          }
                      } else if (which[w] %in% 50:54) {
                          ## bottom velocity
                          par(mar=c(mgp[1]+1, mgp[1]+1, 1, 1))
                          n <- prod(dim(x@data$v)[1:2])
                          if ("bv" %in% names(x@data)) {
                              if (which[w] == 50) {
                                  V <- apply(x@data$bv, 1, mean, na.rm=TRUE)
                                  ats <- oce.plot.ts(x@data$time, V,
                                                     ylab="Bottom speed [m/s]",
                                                     type=type,
                                                     xlim=if (xlimGiven) xlim[w, ] else tlim,
                                                     ylim=if (ylimGiven) ylim[w, ] else range(V, na.rm=TRUE),
                                                     tformat=tformat,
                                                     debug=debug-1)
                              } else {
                                  V <- x@data$bv[, which[w]-50]
                                  ats <- oce.plot.ts(x@data$time, V,
                                                     ylab=paste("Beam", which[w]-50, "bottom velocity [m/s]"),
                                                     type=type,
                                                     xlim=if (xlimGiven) xlim[w, ] else tlim,
                                                     ylim=if (ylimGiven) ylim[w, ] else range(V, na.rm=TRUE),
                                                     tformat=tformat,
                                                     debug=debug-1)
                              }
                          } else {
                              warning("cannot handle which= ", which[w], " because this instrument lacked bottom tracking")
                          }
                      }

                      ## FIXME delete the next block, after testing.
                      if (marginsAsImage && useLayout)  {
                          ## FIXME: I think this should be deleted
                          ## blank plot, to get axis length same as for images
                          omar <- par("mar")
                          par(mar=c(mar[1], 1/4, mgp[2]+1/2, mgp[2]+1))
                          plot(1:2, 1:2, type='n', axes=FALSE, xlab="", ylab="")
                          par(mar=omar)
                      }
                  } else if (which[w] %in% spatial) {
                      ## various spatial types
                      if (which[w] == 23) {
                          ## progressive vector
                          par(mar=c(mgp[1]+1, mgp[1]+1, 1, 1))
                          if (mode == 'diagnostic')
                              dt <- as.numeric(difftime(x@data$timeDia[2], x@data$timeDia[1], units="sec")) # FIXME: should not assume all equal
                          else
                              dt <- as.numeric(difftime(x@data$time[2], x@data$time[1], units="sec")) # FIXME: should not assume all equal
                          mPerKm <- 1000
                          if (mode == 'diagnostic') {
                              U <- x@data$vDia[, 1, 1]
                              V <- x@data$vDia[, 1, 2]
                              ttt <- x@data$timeDia
                          } else {
                              U <- x@data$v[, , 1]
                              V <- x@data$v[, , 2]
                              ttt <- x@data$time
                          }
                          if (!missing(control) && !is.null(control$bin)) {
                              if (control$bin < 1)
                                  stop("cannot have control$bin less than 1, but got ", control$bin)
                              max.bin <- dim(x@data$v)[2]
                              if (control$bin > max.bin)
                                  stop("cannot have control$bin larger than ", max.bin, " but got ", control$bin)
                              u <- U[, control$bin, 1]
                              v <- V[, control$bin, 2]
                          } else {
                              if (x@metadata$numberOfCells > 1) {
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
                          plot(xDist, yDist, xlab="km", ylab="km", type='l', asp=1, col=if (colGiven) col else "black", ...)
                          xaxp <- par("xaxp")
                          xat <- seq(xaxp[1], xaxp[2], length.out=1+xaxp[3])
                          yaxp <- par("yaxp")
                          yat <- seq(yaxp[1], yaxp[2], length.out=1+yaxp[3])
                          ats <- list(xat=xat, yat=yat)
                      } else if (which[w] %in% 24:27) {
                          par(mar=c(mgp[1]+1, mgp[1]+1, 1, 1))
                          if (which[w] == 27 && x@metadata$numberOfBeams < 4) {
                              warning("cannot use which=27 for a 3-beam instrument")
                          } else {
                              value <- apply(x@data$v[, , which[w]-23], 2, mean, na.rm=TRUE)
                              yy <- x@data$distance
                              if (ytype == "profile" && x@metadata$orientation == "downward" && !ylimGiven) {
                                  plot(value, yy, xlab=beamName(x, which[w]-23),
                                       ylab=resizableLabel("distance"), type='l', ylim=rev(range(yy)), ...)
                              } else {
                                  plot(value, yy, xlab=beamName(x, 1),
                                       ylab=resizableLabel("distance"), type='l', ...)
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
                      n <- dim(x@data$v)[1]
                      if (!missing(control) && !is.null(control$bin)) {
                          if (control$bin < 1)
                              stop("cannot have control$bin less than 1, but got ", control$bin)
                          max.bin <- dim(x@data$v)[2]
                          if (control$bin > max.bin)
                              stop("cannot have control$bin larger than ", max.bin, " but got ", control$bin)
                          u <- x@data$v[, control$bin, 1]
                          v <- x@data$v[, control$bin, 2]
                      } else {
                          if (x@metadata$numberOfCells > 1) {
                              u <- apply(x@data$v[, , 1], 1, mean, na.rm=TRUE)
                              v <- apply(x@data$v[, , 2], 1, mean, na.rm=TRUE)
                          } else {
                              u <- x@data$v[, 1, 1]
                              v <- x@data$v[, 1, 2]
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
                                   ...)
                          } else {
                              plot(u, v,
                                   xlab=resizableLabel("u"),
                                   ylab=resizableLabel("v"),
                                   type='n', asp=1,
                                   xlim=if (xlimGiven) xlim[w, ] else range(u, na.rm=TRUE),
                                   ylim=if (ylimGiven) ylim[w, ] else range(v, na.rm=TRUE),
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
                                        ...)
                      }
                      xaxp <- par("xaxp")
                      xat <- seq(xaxp[1], xaxp[2], length.out=1+xaxp[3])
                      yaxp <- par("yaxp")
                      yat <- seq(yaxp[1], yaxp[2], length.out=1+yaxp[3])
                      ats <- list(xat=xat, yat=yat)

                      if (main[w] != "")
                          mtext(main[w], adj=1)
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
                                      stop("cannot have control$bin less than 1, but got ", control$bin)
                                  max.bin <- dim(x@data$v)[2]
                                  if (control$bin > max.bin)
                                      stop("cannot have control$bin larger than ", max.bin, " but got ", control$bin)
                                  umean <- mean(x@data$v[, control$bin, 2], na.rm=TRUE)
                                  vmean <- mean(x@data$v[, control$bin, 2], na.rm=TRUE)
                              } else {
                                  umean <- mean(x@data$v[, , 1], na.rm=TRUE)
                                  vmean <- mean(x@data$v[, , 2], na.rm=TRUE)
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
                                  plot(x@metadata$longitude, x@metadata$latitude, xlab="", ylab="")
                              } else {
                                  warning("no latitude or longitude in object's metadata, so cannot draw map")
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
                                  }
                              }
                          }
                          ## FIXME: span should be an arg
                          if ("firstLatitude" %in% names(x@data)) {
                              lat <- x[["firstLatitude"]]
                              lon <- x[["firstLongitude"]]
                              ##asp <- 1 / cos(mean(lat, na.rm=TRUE) * pi / 180)
                              plot(coastline, clatitude=mean(lat, na.rm=TRUE), clongitude=mean(lon, na.rm=TRUE), span=span)
                              points(lon, lat)
                              #plot(lon, lat, asp=asp, xlab="Latitude", ylab="Longitude")
                              #lines(coastline[["longitude"]], coastline[["latitude"]], col='gray')
                          } else if ("latitude" %in% names(x@metadata)) {
                              lat <- x[["latitude"]]
                              lon <- x[["longitude"]]
                              if (is.finite(lat) && is.finite(lon)) {
                                  plot(coastline, clatitude=lat, clongitude=lon, span=50)
                                  points(x[["longitude"]], x[["latitude"]], cex=2*par('cex'))
                              } else {
                                  warning("nothing to map")
                              }
                          } else {
                              warning("nothing to map")
                          }
                      }
                  } else {
                      stop("unknown value of which (", which[w], ")")
                  }
                  if (grid)
                      grid(col=grid.col, lty=grid.lty, lwd=grid.lwd)
              }
              par(cex=opar$cex)
              oceDebug(debug, "} # plot,adp-method()\n", unindent=1)
              if (exists("ats")) {
                  res$xat <- ats$xat
                  res$yat <- ats$yat
              }
              invisible(res)
          })



#' Convert an ADP Object to ENU Coordinates
#'
#' @param x an \code{adp} object, i.e. one inheriting from \code{\link{adp-class}}.
#' @param declination magnetic declination to be added to the heading, to get
#' ENU with N as "true" north.
#' @template debugTemplate
#' @author Dan Kelley
#' @seealso See \code{\link{read.adp}} for notes on functions relating to
#' \code{"adp"} objects.  Also, see \code{\link{beamToXyzAdp}} and
#' \code{\link{xyzToEnuAdp}}.
#' @references
#' \url{https://www.nortekgroup.com/faq/how-is-a-coordinate-transformation-done}
#' @family things related to \code{adp} data
toEnuAdp <- function(x, declination=0, debug=getOption("oceDebug"))
{
    oceDebug(debug, "toEnuAdp() {\n", unindent=1)
    coord <- x@metadata$oceCoordinate
    if (coord == "beam") {
        x <- xyzToEnuAdp(beamToXyzAdp(x, debug=debug-1), declination=declination, debug=debug-1)
    } else if (coord == "xyz") {
        x <- xyzToEnuAdp(x, declination=declination, debug=debug-1)
    } else if (coord == "sfm") {
        x <- xyzToEnuAdp(x, declination=declination, debug=debug-1)
    } else if (coord == "enu") {
        ;
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
#' multiplying by \code{count2db}.  Then, the signal decrease owing to
#' spherical spreading is compensated for by adding the term
#' \eqn{20\log10(r)}{20*log10(r)}, where \eqn{r}{r} is the distance from the
#' sensor head to the water from which scattering is occurring.  \eqn{r}{r} is
#' given by \code{x[["distance"]]}.
#'
#' @param x An \code{adp} object, i.e. one inheriting from \code{\link{adp-class}}.
#' @param count2db a set of coefficients, one per beam, to convert from beam
#' echo intensity to decibels.
#' @param asMatrix a boolean that indicates whether to return a numeric matrix,
#' as opposed to returning an updated object (in which the matrix is cast to a
#' raw value).
#' @template debugTemplate
#' @return An object of \code{\link[base]{class}} \code{"adp"}.
#' @author Dan Kelley
#' @references The coefficient to convert to decibels is a personal
#' communication.  The logarithmic term is explained in textbooks on acoustics,
#' optics, etc.
#' @examples
#'
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
#' plot(adp.att, which="amplitude",col=oce.colorsJet(100))
#'
#' @family things related to \code{adp} data
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
    correction <- matrix(rep(20 * log10(x@data$distance), numberOfProfiles),
                         nrow=numberOfProfiles, byrow=TRUE)
    if (asMatrix) {
        res <- array(double(), dim=dim(x@data$a))
        for (beam in 1:x@metadata$numberOfBeams) {
            oceDebug(debug, "beam=", beam, "\n")
            res[, , beam] <- count2db[beam] * as.numeric(x@data$a[, , beam]) + correction
        }
    } else {
        res <- x
        for (beam in 1:x@metadata$numberOfBeams) {
            oceDebug(debug, "beam=", beam, "\n")
            tmp <- floor(count2db[beam] * as.numeric(x@data$a[, , beam]) + correction)
            tmp[tmp < 0] <- 0
            tmp[tmp > 255] <- 255
            res@data$a[, , beam] <- as.raw(tmp)
        }
        res@metadata$oceBeamUnspreaded <- TRUE
        res@processingLog <- processingLogAppend(res@processingLog, paste(deparse(match.call()), sep="", collapse=""))
    }
    oceDebug(debug, "} # beamUnspreadAdp()\n", unindent=1)
    res
}


#' Convert ADP From Beam to XYZ Coordinates
#'
#' Convert ADP velocity components from a beam-based coordinate system to a
#' xyz-based coordinate system.
#'
#' The action depends on the type of object.
#'
#' For a 3-beam \code{aquadopp} object, the beams are transformed into
#' velocities using the matrix stored in the header.
#'
#' For 4-beam \code{rdi} object, the beams are converted to velocity components
#' using formulae from section 5.5 of \emph{RD Instruments} (1998), viz. the
#' along-beam velocity components \eqn{B_1}{B1}, \eqn{B_2}{B2}, \eqn{B_3}{B3},
#' and \eqn{B_4}{B4} are used to calculate velocity components in a cartesian
#' system referenced to the instrument using the following formulae:
#' \eqn{u=ca(B_1-B_2)}{u=c*a*(B1-B2)}, \eqn{v=ca(B_4-B_3)}{v=c*a*(B4-B3)},
#' \eqn{w=-b(B_1+B_2+B_3+B_4)}{w=-b*(B1+B2+B3+B4)}, and an estimate of the
#' error in velocity is calculated using \eqn{e=d(B_1+B_2-B_3-B_4)}{e=d*(B1+
#' B2 - B3 - B4)}
#'
#' (Note that the multiplier on \eqn{e}{e} is subject to discussion; RDI
#' suggests one multiplier, but some oceanographers favour another.)
#'
#' In the above, \eqn{c=1}{c=1} if the beam geometry is convex, and
#' \eqn{c=-1}{c=-1} if the beam geometry is concave,
#' \eqn{a=1/(2\sin\theta)}{a=1/(2*sin(theta))},
#' \eqn{b=1/(4\cos\theta)}{b=1/(4*cos(theta))} and
#' \eqn{d=a/\sqrt{2}}{d=a/sqrt(2)}, where \eqn{\theta}{theta} is the angle the
#' beams make to the instrument \dQuote{vertical}.
#'
#' @param x an object of class \code{"adp"}.
#' @template debugTemplate
#' @return An object with the first 3 velocity indices having been altered to
#' represent velocity components in xyz (or instrument) coordinates.  (For
#' \code{rdi} data, the values at the 4th velocity index are changed to
#' represent the "error" velocity.)
#'
#' To indicate the change, the value of \code{metadata$oce.orientation} is
#' changed from \code{beam} to \code{xyz}.
#' @author Dan Kelley
#' @seealso See \code{\link{read.adp}} for other functions that relate to
#' objects of class \code{"adp"}.
#' @references
#'
#' 1. R D Instruments, 1998. \emph{ADP Coordinate Transformation, formulas and
#' calculations.} P/N 951-6079-00 (July 1998).
#'
#' 2. WHOI/USGS-provided Matlab code for beam-enu transformation
#' \samp{http://woodshole.er.usgs.gov/pubs/of2005-1429/MFILES/AQDPTOOLS/beam2enu.m}
#'
#' @family things related to \code{adp} data
beamToXyzAdp <- function(x, debug=getOption("oceDebug"))
{
    debug <- if (debug > 0) 1 else 0
    oceDebug(debug, "beamToXyzAdp(x, debug=", debug, ") {\n", sep="", unindent=1)
    if (!inherits(x, "adp"))
        stop("method is only for objects of class \"adp\"")
    if (x@metadata$oceCoordinate != "beam")
        stop("input must be in beam coordinates")
    if (length(grep(".*rdi.*", x@metadata$manufacturer))) {
        if (x@metadata$numberOfBeams != 4)
            stop("can only handle 4-beam ADP units from RDI")
        res <- x
        if (is.null(x@metadata$transformationMatrix))
            stop("missing x@metadata$transformationMatrix")
        oceDebug(debug, "manufacturer: rdi\n")
        tm <- x@metadata$transformationMatrix
        if (!all.equal(dim(tm), c(4, 4)))
            stop("x@metadata$transformationMatrix must be a 4x4 matrix")
        if (debug) {
            cat("Transformation matrix:\n")
            print(tm)
        }
        V <- x@data$v[, , 1:4]
        res@data$v[, , 1] <- tm[1, 1] * V[, , 1] + tm[1, 2] * V[, , 2] + tm[1, 3] * V[, , 3] + tm[1, 4] * V[, , 4]
        res@data$v[, , 2] <- tm[2, 1] * V[, , 1] + tm[2, 2] * V[, , 2] + tm[2, 3] * V[, , 3] + tm[2, 4] * V[, , 4]
        res@data$v[, , 3] <- tm[3, 1] * V[, , 1] + tm[3, 2] * V[, , 2] + tm[3, 3] * V[, , 3] + tm[3, 4] * V[, , 4]
        res@data$v[, , 4] <- tm[4, 1] * V[, , 1] + tm[4, 2] * V[, , 2] + tm[4, 3] * V[, , 3] + tm[4, 4] * V[, , 4]
        if ("bv" %in% names(x@data)) {
            ## bottom velocity
            V <- x@data$bv
            res@data$bv[, 1] <- tm[1, 1] * V[, 1] + tm[1, 2] * V[, 2] + tm[1, 3] * V[, 3] + tm[1, 4] * V[, 4]
            res@data$bv[, 2] <- tm[2, 1] * V[, 1] + tm[2, 2] * V[, 2] + tm[2, 3] * V[, 3] + tm[2, 4] * V[, 4]
            res@data$bv[, 3] <- tm[3, 1] * V[, 1] + tm[3, 2] * V[, 2] + tm[3, 3] * V[, 3] + tm[3, 4] * V[, 4]
            res@data$bv[, 4] <- tm[4, 1] * V[, 1] + tm[4, 2] * V[, 2] + tm[4, 3] * V[, 3] + tm[4, 4] * V[, 4]
        }
    } else if (length(grep(".*nortek.*", x@metadata$manufacturer))) {
        if (x@metadata$numberOfBeams != 3)
            stop("can only handle 3-beam ADP units from nortek")
        if (is.null(x@metadata$transformationMatrix))
            stop("missing x@metadata$transformationMatrix")
        tm <- x@metadata$transformationMatrix
        if (!all.equal(dim(tm), c(3, 3)))
            stop("x@metadata$transformationMatrix must be a 3x3 matrix")
        oceDebug(debug, "manufacturer: nortek; transformationMatrix is as given below\n")
        if (debug > 0)
            print(tm)
        res <- x
        V <- x@data$v[, , 1:3]
        res@data$v[, , 1] <- tm[1, 1] * V[, , 1] + tm[1, 2] * V[, , 2] + tm[1, 3] * V[, , 3]
        res@data$v[, , 2] <- tm[2, 1] * V[, , 1] + tm[2, 2] * V[, , 2] + tm[2, 3] * V[, , 3]
        res@data$v[, , 3] <- tm[3, 1] * V[, , 1] + tm[3, 2] * V[, , 2] + tm[3, 3] * V[, , 3]
        if ("bv" %in% names(x@data)) {
            ## bottom velocity
            V <- x@data$bv
            res@data$bv[, 1] <- tm[1, 1] * V[, 1] + tm[1, 2] * V[, 2] + tm[1, 3] * V[, 3]
            res@data$bv[, 2] <- tm[2, 1] * V[, 1] + tm[2, 2] * V[, 2] + tm[2, 3] * V[, 3]
            res@data$bv[, 3] <- tm[3, 1] * V[, 1] + tm[3, 2] * V[, 2] + tm[3, 3] * V[, 3]
        }
    } else if (length(grep(".*sontek.*", x@metadata$manufacturer))) {
        if (x@metadata$numberOfBeams != 3)
            stop("can only handle 3-beam ADP units from sontek")
        if (is.null(x@metadata$transformationMatrix))
            stop("missing x@metadata$transformationMatrix")
        tm <- x@metadata$transformationMatrix
        if (!all.equal(dim(tm), c(3, 3)))
            stop("x@metadata$transformationMatrix must be a 3x3 matrix")
        oceDebug(debug, "manufacturer: sontek; transformationMatrix is as given below\n")
        if (debug > 0)
            print(tm)
        res <- x
        V <- x@data$v[, , 1:3]
        res@data$v[, , 1] <- tm[1, 1] * V[, , 1] + tm[1, 2] * V[, , 2] + tm[1, 3] * V[, , 3]
        res@data$v[, , 2] <- tm[2, 1] * V[, , 1] + tm[2, 2] * V[, , 2] + tm[2, 3] * V[, , 3]
        res@data$v[, , 3] <- tm[3, 1] * V[, , 1] + tm[3, 2] * V[, , 2] + tm[3, 3] * V[, , 3]
        if ("bv" %in% names(x@data)) {
            ## bottom velocity
            V <- x@data$bv
            res@data$bv[, 1] <- tm[1, 1] * V[, 1] + tm[1, 2] * V[, 2] + tm[1, 3] * V[, 3]
            res@data$bv[, 2] <- tm[2, 1] * V[, 1] + tm[2, 2] * V[, 2] + tm[2, 3] * V[, 3]
            res@data$bv[, 3] <- tm[3, 1] * V[, 1] + tm[3, 2] * V[, 2] + tm[3, 3] * V[, 3]
        }
    } else {
        stop("adp type must be either \"rdi\" or \"nortek\" or \"sontek\"")
    }
    res@metadata$oceCoordinate <- "xyz"
    res@processingLog <- processingLogAppend(res@processingLog, paste(deparse(match.call()), sep="", collapse=""))
    oceDebug(debug, "} # beamToXyzAdp()\n", unindent=1)
    res
}


#' Convert ADP From XYZ to ENU Coordinates
#'
#' Convert ADP velocity components from a xyz-based coordinate system to
#' an enu-based coordinate system, by using the instrument's recording of
#' heading, pitch, and roll.
#'
#' The first step is to convert the (x,y,z) velocity components (stored in the
#' three columns of \code{x[["v"]][,,1:3]}) into what RDI [1, pages 11 and 12]
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
#' University [2], who developed it based on instrument documentation,
#' discussion on user groups, and analysis of measurements acquired with RDI
#' and Sontek acoustic current profilers in the SLEIWEX experiment.  In the
#' table, (X, Y, Z) denote instrument-coordinate velocities, (S, F, M) denote
#' ship-coordinate velocities, and (H, P, R) denote heading, pitch, and roll.
#'
#' \tabular{rrrrrrrrrrrr}{ \strong{Case} \tab \strong{Mfr.} \tab
#' \strong{Instr.} \strong{Orient.} \tab \strong{H} \tab \strong{P} \tab
#' \strong{R} \tab \strong{S} \tab \strong{F} \tab \strong{M}\cr 1 \tab RDI
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
#' transformation manual [1, pages 13 and 14], this matrix is based on sines
#' and cosines of heading, pitch, and roll If \code{CH} and \code{SH} denote
#' cosine and sine of heading (after adjusting for declination), with similar
#' terms for pitch and roll using second letters \code{P} and \code{R}, the
#' rotation matrix is
#'
#' \preformatted{ rbind(c( CH*CR + SH*SP*SR, SH*CP, CH*SR - SH*SP*CR), c(-SH*CR
#' + CH*SP*SR, CH*CP, -SH*SR - CH*SP*CR), c( -CP*SR, SP, CP*CR)) }
#'
#' This matrix is left-multiplied by a matrix with three rows, the top a vector
#' of "starboard" values, the middle a vector of "forward" values, and the
#' bottom a vector of "mast" values.  Finally, the columns of
#' \code{data$v[,,1:3]} are filled in with the result of the matrix
#' multiplication.
#'
#' @param x An \code{adp} object, i.e. one inheriting from \code{\link{adp-class}}.
#' @param declination magnetic declination to be added to the heading after
#' "righting" (see below), to get ENU with N as "true" north.
#' @template debugTemplate
#' @return An object with \code{data$v[,,1:3]} altered appropriately, and
#' \code{metadata$oce.orientation} changed from \code{xyz} to \code{enu}.
#' @author Dan Kelley and Clark Richards
#' @references
#' 1. RD Instruments, 1998.  \emph{ADCP Coordinate
#' Transformation, formulas and calculations.} P/N 951-6079-00 (July 1998).
#'
#' 2. Clark Richards, 2012, PhD Dalhousie University Department of
#' Oceanography.
#'
#' @family things related to \code{adp} data
xyzToEnuAdp <- function(x, declination=0, debug=getOption("oceDebug"))
{
    ##cat("adp.R:xyzToEnuAdp(): called as", paste(deparse(match.call()), sep="", collapse=""), "\n")
    debug <- if (debug > 0) 1 else 0
    oceDebug(debug, "xyzToEnuAdp(x, declination=", declination, ", debug=", debug, ") {\n", sep="", unindent=1)
    if (!inherits(x, "adp"))
        stop("method is only for objects of class '", "adp", "'")
    if (x@metadata$oceCoordinate != "xyz" & x@metadata$oceCoordinate != "sfm")
        stop("input must be in xyz or sfm coordinates")
    res <- x
    heading <- res@data$heading
    pitch <- res@data$pitch
    roll <- res@data$roll
    ## Case-by-case alteration of heading, pitch and roll, so we can use one formula for all.
    ## There are three instrumentType values, ("teledyn rdi", "nortek", and "sontek"), and
    ## three orientation values ("upward", "downward", and "sideward").
    haveBv <- "bv" %in% names(x@data)
    if (1 == length(agrep("rdi", x@metadata$manufacturer, ignore.case=TRUE))) {
        ## "teledyn rdi"
        ## h/p/r and s/f/m from Clark Richards pers. comm. 2011-03-14, revised 2011-03-15
        if (res@metadata$oceCoordinate == "sfm") {
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
        } else if (res@metadata$oceCoordinate == "sfm" & res@metadata$tiltUsed) {
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
        } else if (res@metadata$orientation == "upward") {
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
        } else if (res@metadata$orientation == "downward") {
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
            stop("need metadata$orientation='upward' or 'downward', not '", x@metadata$orientation, "'")
        }
    } else if (1 == length(agrep("nortek", x@metadata$manufacturer))) {
        ## "nortek"
        ## h/p/r and s/f/m from Clark Richards pers. comm. 2011-03-14
        if (res@metadata$orientation == "upward") {
            oceDebug(debug, "Case 3: Nortek ADP with upward-pointing sensor.\n")
            oceDebug(debug, "        Using heading=heading-90, pitch=roll, roll=-pitch, S=X, F=Y, and M=Z.\n")
            heading <- heading - 90
            tmp <- pitch
            pitch <- roll
            roll <- -tmp
            starboard <- res@data$v[, , 1]
            forward <- res@data$v[, , 2]
            mast <- res@data$v[, , 3]
            if (haveBv) {
                ## bottom velocity
                starboardBv <- res@data$bv[, 1]
                forwardBv <- res@data$bv[, 2]
                mastBv <- res@data$bv[, 3]
            }
        } else if (res@metadata$orientation == "downward") {
            oceDebug(debug, "Case 4: Nortek ADP with downward-pointing sensor.\n")
            oceDebug(debug, "        Using heading=heading-90, pitch=roll, roll=-pitch, S=X, F=-Y, and M=-Z.\n")
            heading <- heading - 90
            tmp <- pitch
            pitch <- roll
            roll <- -tmp
            starboard <- res@data$v[, , 1]
            forward <- -res@data$v[, , 2]
            mast <- -res@data$v[, , 3]
            if (haveBv) {
                ## bottom velocity
                starboardBv <- res@data$bv[, 1]
                forwardBv <- -res@data$bv[, 2]
                mastBv <- res@data$bv[, 3]
            }
        } else {
            stop("need metadata$orientation='upward' or 'downward', not '", x@metadata$orientation, "'")
        }
    } else if (1 == length(agrep("sontek", x@metadata$manufacturer))) {
        ## "sontek"
        if (res@metadata$orientation == "upward") {
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
        } else if (res@metadata$orientation == "downward") {
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
            stop("need metadata$orientation='upward' or 'downward', not '", x@metadata$orientation, "'")
        }
    } else {
        stop("unrecognized manufacturer; should be 'teledyne rdi', 'sontek', or 'nortek', but is '",
             x@metadata$manufacturer, "'")
    }
    oceDebug(debug, vectorShow(heading, "heading (after adjustment)"))
    oceDebug(debug, vectorShow(pitch, "pitch (after adjustment)"))
    oceDebug(debug, vectorShow(roll, "roll (after adjustment)"))
    nc <- dim(x@data$v)[2]           # numberOfCells
    np <- dim(x@data$v)[1]           # number of profiles
    if (length(heading) < np)
        heading <- rep(heading, length.out=np)
    if (length(pitch) < np)
        pitch <- rep(pitch, length.out=np)
    if (length(roll) < np)
        roll <- rep(roll, length.out=np)
    ## ADP and ADV calculations are both handled by sfm_enu
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
                                       paste("xyzToEnu(x", ", declination=", declination, ", debug=", debug, ")", sep=""))
    oceDebug(debug, "} # xyzToEnuAdp()\n", unindent=1)
    res
}


#' Convert ADP ENU to Rotated Coordinate
#'
#' Convert ADP velocity components from an enu-based coordinate system to
#' another system, perhaps to align axes with the coastline.
#'
#' The supplied angles specify rotations to be made around the axes for which
#' heading, pitch, and roll are defined.  For example, an eastward current will
#' point southeast if \code{heading=45} is used.
#'
#' The returned value has heading, pitch, and roll matching those of \code{x},
#' so these angles retain their meaning as the instrument orientation.
#'
#' NOTE: this function works similarly to \code{\link{xyzToEnuAdp}}, except
#' that in the present function, it makes no difference whether the instrument
#' points up or down, etc.
#'
#' @param x An \code{adp} object, i.e. one inheriting from \code{\link{adp-class}}.
#' @param heading number or vector of numbers, giving the angle, in degrees, to
#' be added to the heading.  See \dQuote{Details}.
#' @param pitch as \code{heading} but for pitch.
#' @param roll as \code{heading} but for roll.
#' @return An object with \code{data$v[,1:3,]} altered appropriately, and
#' \code{metadata$oce.coordinate} changed from \code{enu} to \code{other}.
#' @author Dan Kelley
#' @seealso See \code{\link{read.adp}} for other functions that relate to
#' objects of class \code{"adp"}.
#' @references RD Instruments, 1998. \emph{ADP Coordinate Transformation,
#' formulas and calculations.} P/N 951-6079-00 (July 1998)
#' @examples
#'
#' library(oce)
#' data(adp)
#' o <- enuToOtherAdp(adp, heading=-31.5)
#' plot(o, which=1:3)
#'
#' @family things related to \code{adp} data
enuToOtherAdp <- function(x, heading=0, pitch=0, roll=0)
{
    if (!inherits(x, "adp"))
        stop("method is only for objects of class '", "adp", "'")
    if (x@metadata$oceCoordinate != "enu")
        stop("input must be in enu coordinates, but it is in ", x@metadata$oceCoordinate, " coordinates")
    res <- x
    np <- dim(x@data$v)[1]           # number of profiles
    if (length(heading) != np)
        heading <- rep(heading, length.out=np)
    if (length(pitch) != np)
        pitch <- rep(pitch, length.out=np)
    if (length(roll) != np)
        roll <- rep(roll, length.out=np)
    nc <- dim(x@data$v)[2]           # numberOfCells
    for (c in 1:nc) {
        other <- do_sfm_enu(heading, pitch, roll, x@data$v[, c, 1], x@data$v[, c, 2], x@data$v[, c, 3])
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
    res@processingLog <- processingLogAppend(res@processingLog, paste(deparse(match.call()), sep="", collapse=""))
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
#' Subtracts bottom tracking velocities from an \code{"adp"} object. Works for
#' all coordinate systems (\code{beam}, \code{xyz}, and \code{enu}).
#'
#' @param x an object of class \code{"adp"}, which contains bottom tracking
#' velocities.
#' @template debugTemplate
#' @author Dan Kelley and Clark Richards
#' @seealso See \code{\link{read.adp}} for notes on functions relating to
#' \code{"adp"} objects, and \code{\link{adp-class}} for notes on the ADP
#' object class.
## @family things related to \code{adp} data
subtractBottomVelocity <- function(x, debug=getOption("oceDebug"))
{
    oceDebug(debug, "subtractBottomVelocity(x) {\n", unindent=1)
    if (!("bv" %in% names(x@data))) {
        warning("there is no bottom velocity in this object")
        return(x)
    }
    res <- x
    numberOfBeams <- dim(x@data$v)[3] # could also get from metadata but this is less brittle
    for (beam in 1:numberOfBeams) {
        oceDebug(debug, "beam #", beam, "\n")
        res@data$v[, , beam] <- x@data$v[, , beam] - x@data$bv[, beam]
    }
    oceDebug(debug, "} # subtractBottomVelocity()\n", unindent=1)
    res@processingLog <- processingLogAppend(res@processingLog, paste(deparse(match.call()), sep="", collapse=""))
    res
}


#' Bin-map an ADP object
#'
#' Bin-map an ADP object, by interpolating velocities, backscatter amplitudes,
#' etc., to uniform depth bins, thus compensating for the pitch and roll of the
#' instrument.  This only makes sense for ADP objects that are in beam
#' coordinates.
#'
#' @param x an \code{adp} object, i.e. one inheriting from \code{\link{adp-class}}.
#' @template debugTemplate
#' @return An object of \code{\link[base]{class}} \code{"adp"}.
#' @section Bugs: This only works for 4-beam RDI ADP objects.
#' @author Dan Kelley and Clark Richards
#' @seealso See \code{\link{adp-class}} for a discussion of \code{adp} objects
#' and notes on the many functions dealing with them.
#' @references The method was devised by Clark Richards for use in his PhD work
#' at Department of Oceanography at Dalhousie University.
#' @examples
#'
#' \dontrun{
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
#' }
#'
#' @family things related to \code{adp} data
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
#' Ensemble averaging of \code{adp} objects is often necessary to
#' reduce the uncertainty in velocity estimates from single
#' pings. Many types of ADPs can be configured to perform the
#' ensemble averaging during the data collection, due to memory
#' limitations for long deployments. In cases where the instrument is
#' not memory limited, it may be desirable to perform the ensemble
#' averaging during post-processing, thereby reducing the overall
#' size of the data set and decreasing the uncertainty of the
#' velocity estimates (by averaging out Doppler noise).
#' 
#' @param x an \code{adp} object, i.e. one inheriting from \code{\link{adp-class}}.
#' @param n number of pings to average together.
#' @param leftover a logical value indicating how to proceed in cases
#' where \code{n} does not divide evenly into the number of ensembles
#' in \code{x}. If \code{leftover} is \code{FALSE} (the default) then any extra
#' ensembles at the end of \code{x} are ignored. Otherwise, they are used
#' to create a final ensemble in the returned value.
#' @param na.rm a logical value indicating whether NA values should be stripped 
#' before the computation proceeds
#' @param ... extra arguments to be passed to the \code{mean()} function.
#'
#' @return A reduced object of \code{\link{adp-class}} with ensembles averaged as specified. E.g. for an \code{adp} object with 100 pings and \code{n=5} the number of rows of the data arrays will be reduced by a factor of 5.
#' @author Clark Richards and Dan Kelley
#' @examples
#'
#' library(oce)
#' data(adp)
#' adpAvg <- adpEnsembleAverage(adp, n=2)
#' plot(adpAvg)
#' 
#' @family things related to \code{adp} data
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
    res@metadata$numberOfSamples <- length(res@data$time)
    res@processingLog <- processingLogAppend(res@processingLog, paste(deparse(match.call()), sep="", collapse=""))
    res
}
