## vim:textwidth=128:expandtab:shiftwidth=4:softtabstop=4


#' Class to Store landsat Data
#'
#' This class holds landsat data. Such are available at several
#' websites (e.g. reference 1).
#' Although the various functions may work for other satellites, the
#' discussion here focusses on Landsat 8 and Landsat 7.
#'
#' @templateVar class landsat
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
#' @section Data storage:
#'
#' The data are stored with 16-bit resolution.  Oce
#' breaks these 16 bits up into most-significant and least-significant bytes.
#' For example, the aerosol band of a Landsat object named `x` are
#' contained within `x@@data$aerosol$msb` and `x@@data$aerosol$lsb`,
#' each of which is a matrix of raw values.  The results may be combined as e.g.
#' \preformatted{
#' 256L*as.integer(x@@data[[i]]$msb) + as.integer(x@@data[[i]]$lsb)
#' }
#' and this is what is returned by executing `x[["aerosol"]]`.
#'
#' Landsat data files typically occupy approximately a
#' gigabyte of storage.  That means that corresponding Oce objects are about
#' the same size, and this can pose significant problems on computers with
#' less than 8GB of memory.  It is sensible to specify bands of interest when
#' reading data with [read.landsat()], and also to use
#' [landsatTrim()] to isolate geographical regions that need
#' processing.
#'
#' Experts may need to get direct access to the data, and this is easy because
#' all Landsat objects (regardless of satellite) use a similar storage form.
#' Band information is stored in byte form, to conserve space.  Two bytes are
#' used for each pixel in Landsat-8 objects, with just one for other objects.
#' For example, if a Landsat-8 object named `L` contains the `tirs1`
#' band, the most- and least-significant bytes will be stored in matrices
#' `L@@data$tirs1$msb` and `L@@data$tirs1$lsb`.  A similar Landsat-7
#' object would have the same items, but `msb` would be just the value
#' `0x00`.
#'
#' Derived bands, which may be added to a landsat object with
#' [landsatAdd()], are not stored in byte matrices.  Instead they
#' are stored in numerical matrices, which means that they use 4X more storage
#' space for Landsat-8 images, and 8X more storage space for other satellites.
#' A computer needs at least 8GB of RAM to work with such data.
#'
#' @section Landsat 8:
#'
#' The Landsat 8 satellite has 11 frequency bands, listed below (see reference 2]).
#' \preformatted{
#' .------------------------------------------------------------------------------.
#' | Band | Band                      | Band         | Wavelength    | Resolution |
#' | No.  | Contents                  | Name         | (micrometers) |   (meters) |
#' |------+---------------------------+--------------+---------------+------------|
#' |    1 | Coastal aerosol           | aerosol      |  0.43 -  0.45 |         30 |
#' |    2 | Blue                      | blue         |  0.45 -  0.51 |         30 |
#' |    3 | Green                     | green        |  0.53 -  0.59 |         30 |
#' |    4 | Red                       | red          |  0.64 -  0.67 |         30 |
#' |    5 | Near Infrared (NIR)       | nir          |  0.85 -  0.88 |         30 |
#' |    6 | SWIR 1                    | swir1        |  1.57 -  1.65 |         30 |
#' |    7 | SWIR 2                    | swir2        |  2.11 -  2.29 |         30 |
#' |    8 | Panchromatic              | panchromatic |  0.50 -  0.68 |         15 |
#' |    9 | Cirrus                    | cirrus       |  1.36 -  1.38 |         30 |
#' |   10 | Thermal Infrared (TIRS) 1 | tirs1        | 10.60 - 11.19 |        100 |
#' |   11 | Thermal Infrared (TIRS) 2 | tirs2        | 11.50 - 12.51 |        100 |
#' .------------------------------------------------------------------------------.
#' }
#' In addition to the above, setting `band="terralook"` may be used as
#' an abbreviation for `band=c("red", "green", "nir")`.
#'
#' Band 8 is panchromatic, and has the highest resolution.  For convenience of
#' programming, [read.landsat()] subsamples the `tirs1` and
#' `tirs2` bands to the 30m resolution of the other bands.  See Reference
#' 3 for information about the evolution of Landsat 8 calibration
#' coefficients, which as of summer 2014 are still subject to change.
#'
#' @section Landsat 7:
#'
#' Band information is as follows (from reference 8).  The names are not official, but
#' are set up to roughly correspond with Landsat-8 names, according to wavelength.
#' An exception is the Landsat-7 bands named `tirs1` and `tirs2`, which
#' are at two different gain settings, with identical wavelength span for
#' each, which roughly matches the range of the Landsat-8 bands `tirs1`
#' and `tirs2` combined.  This may seem confusing, but it lets code like
#' `plot(im, band="tirs1")` to work with both Landsat-8 and Landsat-7.
#'
#' \preformatted{
#' .------------------------------------------------------------------------------.
#' | Band | Band                      | Band         | Wavelength    | Resolution |
#' | No.  | Contents                  | Name         | (micrometers) |   (meters) |
#' |------+---------------------------+--------------+---------------+------------|
#' |    1 | Blue                      | blue         |  0.45 -  0.52 |         30 |
#' |    2 | Green                     | green        |  0.52 -  0.60 |         30 |
#' |    3 | Red                       | red          |  0.63 -  0.69 |         30 |
#' |    4 | Near IR                   | nir          |  0.77 -  0.90 |         30 |
#' |    5 | SWIR                      | swir1        |  1.55 -  1.75 |         30 |
#' |    6 | Thermal IR                | tirs1        | 10.4  - 12.50 |         30 |
#' |    7 | Thermal IR                | tirs2        | 10.4  - 12.50 |         30 |
#' |    8 | SWIR                      | swir2        |  2.09 -  2.35 |         30 |
#' |    9 | Panchromatic              | panchromatic |  0.52 -  0.90 |         15 |
#' .------------------------------------------------------------------------------.
#' }
#'
#' @seealso
#'
#' Data from AMSR satellites are handled with [amsr-class].
#'
#' A file containing Landsat data may be read with [read.landsat()] or
#' [read.oce()], and one such file is provided by the \CRANpkg{ocedata}
#' package as a dataset named `landsat`.
#'
#' Plots may be made with [plot,landsat-method()].  Since plotting can be quite
#' slow, decimation is available both in the plotting function and as the separate
#' function [decimate()].  Images may be subsetted with
#' [landsatTrim()].
#'
#' @references
#' 1. See the USGS "glovis" web site.
#'
#' 2. see landsat.gsfc.nasa.gov/?page_id=5377
#'
#' 3. see landsat.usgs.gov/calibration_notices.php
#'
#' 4. \url{http://dankelley.github.io/r/2014/07/01/landsat.html}
#'
#' 5. \url{http://scienceofdoom.com/2010/12/27/emissivity-of-the-ocean/}
#'
#' 6. see landsat.usgs.gov/Landsat8_Using_Product.php
#'
#' 7. see landsathandbook.gsfc.nasa.gov/pdfs/Landsat7_Handbook.pdf
#'
#' 8. see landsat.usgs.gov/band_designations_landsat_satellites.php
#'
#' 9. Yu, X. X. Guo and Z. Wu., 2014. Land Surface Temperature Retrieval from
#' Landsat 8 TIRS-Comparison between Radiative Transfer Equation-Based Method,
#' Split Window Algorithm and Single Channel Method, *Remote Sensing*, 6,
#' 9829-9652.  \url{http://www.mdpi.com/2072-4292/6/10/9829}
#'
#' 10. Rajeshwari, A., and N. D. Mani, 2014.  Estimation of land surface
#' temperature of Dindigul district using Landsat 8 data. *International
#'     Journal of Research in Engineering and Technology*, 3(5), 122-126.
#' `http://www.academia.edu/7655089/ESTIMATION_OF_LAND_SURFACE_TEMPERATURE_OF_DINDIGUL_DISTRICT_USING_LANDSAT_8_DATA`
#'
#' 11. Konda, M. Imasato N., Nishi, K., and T. Toda, 1994.  Measurement of the Sea
#' Surface Emissivity.  *Journal of Oceanography*, 50, 17:30.
#' \url{http://www.terrapub.co.jp/journals/JO/pdf/5001/50010017.pdf}
#'
#' @concept satellite
#'
#' @author Dan Kelley and Clark Richards
#'
#' @family things related to landsat data
setClass("landsat", contains="satellite")


#' Sample landsat Dataset
#'
#' This is a subset of the Landsat-8 image designated LC80080292014065LGN00, an
#' image from March 2014 that covers Nova Scotia and portions of the Bay of
#' Fundy and the Scotian Shelf. The image is decimated to reduce the memory
#' requirements of this package, yielding a spatial resolution of about 2km.
#'
#' @details
#' The original data were downloaded from the USGS earthexplorer website, although
#' other sites can also be used to uncover it by name.  The original
#' data were decimation by a factor of 100 to reduce the file size from about 1GB
#' to under 100Kb.
#'
#' @name landsat
#' @docType data
#'
#' @family datasets provided with oce
#'
#' @family things related to landsat data
NULL


setMethod(f="show",
          signature="landsat",
          definition=function(object) {
              cat("Landsat object, ID", object@metadata$header$landsat_scene_id, "\n")
              dataNames <- names(object@data)
              if (length(dataNames)) {
                  cat("Data (bands or calculated):\n")
                  for (b in seq_along(dataNames)) {
                      dim <- if (is.list(object@data[[b]])) dim(object@data[[b]]$lsb) else dim(object@data[[b]])
                      cat("  \"", dataNames[b], "\" has dimension c(", dim[1], ",", dim[2], ")\n", sep='')
                  }
              } else {
                  cat("Object contains no band data.\n")
              }
          })

setMethod(f="initialize",
          signature="landsat",
          definition=function(.Object, filename, ...) {
              .Object <- callNextMethod(.Object, ...)
              if (!missing(filename))
                  .Object@metadata$filename <- filename
              .Object@processingLog$time <- presentTime()
              .Object@processingLog$value <- "create 'landsat' object"
              return(.Object)
          })

#' Summarize a landsat Object
#'
#' Provides a summary of a some information about a [landsat-class] object.
#'
#' @param object A [landsat-class] object.
#'
#' @param ... Ignored.
#'
#' @concept satellite
#'
#' @author Dan Kelley
#'
#' @family things related to landsat data
setMethod(f="summary",
          signature="landsat",
          definition=function(object, ...) {
              cat("Landsat Summary\n---------------\n\n")
              showMetadataItem(object, "filename",   "Data file:           ")
              showMetadataItem(object, "time",       "Time:                ")
              showMetadataItem(object, "spacecraft", "Spacecraft:          ")
              cat(sprintf("* Header file:         %s\n", object@metadata$headerfilename))
              cat(sprintf("* UTM zone:             %d (used for whole image)\n", object@metadata$zoneUTM))
              cat(sprintf("* UTM lower left:      %7.0f easting %7.0f northing (m)\n",
                          object@metadata$llUTM$easting,
                          object@metadata$llUTM$northing))
              cat(sprintf("* UTM upper right:     %7.0f easting %7.0f northing (m)\n",
                          object@metadata$urUTM$easting,
                          object@metadata$urUTM$northing))
              cat(sprintf("* Lower left:          %fE %fN\n", object@metadata$lllon, object@metadata$lllat))
              cat(sprintf("* Lower right:         %fE %fN\n", object@metadata$lrlon, object@metadata$lrlat))
              cat(sprintf("* Upper right:         %fE %fN\n", object@metadata$urlon, object@metadata$urlat))
              cat(sprintf("* Upper left:          %fE %fN\n", object@metadata$ullon, object@metadata$ullat))
              for (name in names(object@data)) {
                  object@data[[name]] <- object[[name]] # translate to science units
              }
              invisible(callNextMethod()) # summary
          })


#' @title Extract Something From a landsat Object
#'
#' @param x a [landsat-class] object.
#'
#' @templateVar class landsat
#'
#' @template sub_subTemplate
#'
#' @section Details of the specialized `landsat` method:
#'
#' Users are isolated from the details of the two-byte storage system
#' by using the `[[` operator.
#'
#' *Accessing band data.*  The data may be accessed with e.g.
#' `landsat[["panchromatic"]]`, for the panchromatic band.  If a new
#' ``band'' is added with [landsatAdd()], it may be referred by
#' name.  In all cases, a second argument can be provided, to govern
#' decimation.  If this is missing, all the relevant data are returned.  If
#' this is present and equal to `TRUE`, then the data will be
#' automatically decimated (subsampled) to give approximately 800 elements in
#' the longest side of the matrix.  If this is present and numerical, then its
#' value governs decimation.  For example,
#' \code{landsat[["panchromatic",TRUE]]} will auto-decimate, typically
#' reducing the grid width and height from 16000 to about 800.  Similarly,
#' \code{landsat[["panchromatic",10]]} will reduce width and height to about
#' 1600.  On machines with limited RAM (e.g. under about 6GB), decimation is a
#' good idea in almost all processing steps.  It also makes sense for
#' plotting, and in fact is done through the `decimate` argument of
#' [plot,landsat-method()].
#'
#' *Accessing derived data.*  One may retrieve several derived quantities
#' that are calculated from data stored in the object:
#' `landsat[["longitude"]]` and `landsat[["latitude"]]` give pixel
#' locations.  Accessing `landsat[["temperature"]]` creates an estimate
#' of ground temperature as follows (see reference 4).  First, the ``count value'' in
#' band 10, denoted \eqn{b_{10}}{b_10} say, is scaled with coefficients stored
#' in the image metadata using
#' \eqn{\lambda_L=b_{10}M_L+A_L}{lambda_L=b_10*M_L+A_L} where \eqn{M_L}{M_L}
#' and \eqn{A_L}{A_L} are values stored in the metadata (e.g.  the first in
#' `landsat@@metadata$header$radiance_mult_band_10`) Then the result is
#' used, again with coefficients in the metadata, to compute Celcius
#' temperature \eqn{T=K_2/ln(\epsilon
#'     K_1/\lambda_L+1)-273.15}{T=K_2/ln(epsilon*K_1/\lambda_L+1)-273.15}.
#' The value of the emissivity \eqn{\epsilon}{epsilon} is set to unity by
#' [read.landsat()], although it can be changed easily later, by
#' assigning a new value to `landsat@@metadata$emissivity`. The default
#' emissivity value set by [read.landsat()] is from reference 11, and is
#' within the oceanic range suggested by reference 5. Adjustment is as simple as
#' altering `landsat@@metadata$emissivity`. This value can be a single
#' number meant to apply for the whole image, or a matrix with dimensions
#' matching those of band 10.  The matrix case is probably more useful for
#' images of land, where one might wish to account for the different
#' emissivities of soil and vegetation, etc.; for example, Table 4 of
#' reference 9 lists 0.9668 for soil and 0.9863 for vegetation,
#' while Table 5 of reference 10 lists 0.971 and 0.987 for the same quantities.
#'
#' *Accessing metadata.* Anything in the metadata can be accessed by
#' name, e.g. `landsat[["time"]]`.  Note that some items are simply
#' copied over from the source data file and are not altered by e.g.
#' decimation.  An exception is the lat-lon box, which is altered by
#' [landsatTrim()].
#'
#' @concept satellite
#'
#' @author Dan Kelley
#'
#' @family things related to landsat data
setMethod(f="[[",
          signature(x="landsat", i="ANY", j="ANY"),
          definition=function(x, i, j, ...) {
              debug <- getOption("oceDebug")
              oceDebug(debug, "landsat [[ {\n", unindent=1)
              if (missing(i))
                  stop("Must name a landsat item to retrieve, e.g. '[[\"panchromatic\"]]'", call.=FALSE)
              i <- i[1]                # drop extras if more than one given
              if (!is.character(i))
                  stop("landsat item must be specified by name", call.=FALSE)
              ## Handle cases one by one, starting with simplest.
              if (!(is.na(pmatch(i, "longitude")))) {
                  ## FIXME: ignoring decimation (may be best, anyway)
                  b1 <- x@data[[1]]
                  dim <- if (is.list(b1)) dim(b1$lsb) else dim(b1)
                  oceDebug(debug, "} # landsat [[\n", unindent=1)
                  return(x@metadata$lllon + seq(0, 1, length.out=dim[1]) * (x@metadata$urlon - x@metadata$lllon))
              }
              if (!(is.na(pmatch(i, "latitude")))) {
                  ## FIXME: ignoring decimation (may be best, anyway)
                  b1 <- x@data[[1]]
                  dim <- if (is.list(b1)) dim(b1$lsb) else dim(b1)
                  oceDebug(debug, "} # landsat [[\n", unindent=1)
                  return(x@metadata$lllat + seq(0, 1, length.out=dim[2]) * (x@metadata$urlat - x@metadata$lllat))
              }
              if (is.character(i) && !is.na(pmatch(i, names(x@metadata)))) {
                  oceDebug(debug, "} # landsat [[\n", unindent=1)
                  return(x@metadata[[i]])
              }
              d <- NULL                # check for this later to see found data
              if (!is.na(pmatch(i, "temperature"))) {
                  if (!("tirs1" %in% names(x@data))) stop("cannot compute temperature without \"tirs1\" band")
                  if (!is.list(x@data$tirs1)) stop("the \"tirs1\" band is not stored in two-byte format")
                  ## First, determine whether decimation is needed.
                  decimate <- 1        # decimation step
                  emissivity <- if ("emissivity" %in% names(x@metadata)) x@metadata$emissivity else 1
                  dim <- dim(x@data$tirs1$lsb)
                  maxdim <- max(dim)
                  ilook <- seq.int(1L, dim[1], by=1L)
                  jlook <- seq.int(1L, dim[2], by=1L)
                  if (!missing(j)) {
                      if (is.logical(j)) {
                         decimate <- if (j) max(as.integer(round(maxdim / 800)), 1) else 1
                      } else {
                          if (round(j) < 1) stop("cannot decimate by a step smaller than 1, but got ", j)
                          decimate <- as.integer(round(j))
                          if (decimate > min(dim)) stop("cannot decimate by a step larger than image dimension")
                      }
                      ilook <- seq.int(1, dim[1], by=decimate)
                      jlook <- seq.int(1, dim[2], by=decimate)
                  }
                  spacecraft <- if (is.null(x@metadata$spacecraft)) "LANDSAT_8" else x@metadata$spacecraft
                  if (spacecraft == "LANDSAT_8") {
                      oceDebug(debug, "temperature for landsat-8\n")
                      if (!("tirs1" %in% names(x@data)))
                          stop("cannot calculate Landsat temperature because no \"tirs1\" band in object", call.=FALSE)
                      ML <- x@metadata$header$radiance_mult_band_10
                      AL <- x@metadata$header$radiance_add_band_10
                      K1 <- x@metadata$header$k1_constant_band_10
                      K2 <- x@metadata$header$k2_constant_band_10
                      oceDebug(debug, "ML=", ML, "# @metadata$header$radiance_mult_band_10\n")
                      oceDebug(debug, "AL=", AL, "# @metadata$header$radiance_add_band_10\n")
                      oceDebug(debug, "K1=", K1, "# @metadata$header$k1_constant_band_10\n")
                      oceDebug(debug, "K2=", K2, "# @metadata$header$k2_constant_band_10\n")
                      if (is.matrix(emissivity))
                          emissivity <- emissivity[ilook, jlook]
                      msb <- x@data$tirs1$msb[ilook, jlook]
                      lsb <- x@data$tirs1$lsb[ilook, jlook]
                      dim <- dim(msb)
                      d <- 256L*as.integer(msb) + as.integer(lsb)
                      na <- d == 0
                      ## rm(x) # may help if space is tight
                      Llambda <- ML * d + AL
                      ## avoid warnings on the 0 Llambda values (from band gaps)
                      options <- options('warn')
                      options(warn=-1)
                      d <- K2 / log(emissivity * K1 / Llambda + 1)
                      options(warn=options$warn)
                      d <- d - 273.15
                      d[na] <- NA
                      dim(d) <- dim
                      oceDebug(debug, "} # landsat [[\n", unindent=1)
                      return(d)
                  } else if (spacecraft == "LANDSAT_7") {
                      ## band 6, tirs1
                      oceDebug(debug, "temperature for landsat-7\n")
                      ML <- x@metadata$header$radiance_mult_band_6_vcid_1
                      AL <- x@metadata$header$radiance_add_band_6_vcid_1
                      K1 <- 666.09  # Landsat7_Handbook.pdf Table 11.5
                      K2 <- 1282.71 # Landsat7_Handbook.pdf Table 11.5
                      oceDebug(debug, "ML=", ML, "# @metadata$header$radiance_mult_band_6_vcid_1\n")
                      oceDebug(debug, "AL=", AL, "# @metadata$header$radiance_add_band_6_vcid_1\n")
                      oceDebug(debug, "K1=", K1, "# Landsat7_Handbook.pdf Table 11.5\n")
                      oceDebug(debug, "K2=", K2, "# Landsat7_Handbook.pdf Table 11.5\n")
                      ## d <- 256L*as.integer(x@data$tirs1$msb) + as.integer(x@data$tirs1$lsb)
                      if (is.matrix(emissivity))
                          emissivity <- emissivity[ilook, jlook]
                      d <- x@data$tirs1$lsb[ilook, jlook]
                      dim <- dim(d)
                      d <- as.integer(d)
                      na <- d == 0
                      rm(x) # may help if space is tight
                      Llambda <- ML * d + AL
                      ## avoid warnings on the 0 Llambda values (from band gaps)
                      options <- options('warn')
                      options(warn=-1)
                      d <- K2 / log(emissivity * K1 / Llambda + 1)
                      options(warn=options$warn)
                      d <- d - 273.15
                      d[na] <- NA
                      dim(d) <- dim
                      oceDebug(debug, "} # landsat [[\n", unindent=1)
                      return(d)
                  } else if (spacecraft == "LANDSAT_5") {
                      ## band 6, tirs1
                      message("FIXME: should handle temperature for landsat-5\n")
                      K1 <- 607.76     # Landsat7_Handbook.pdf Table 11.5
                      K2 <- 1260.56    # Landsat7_Handbook.pdf Table 11.5
                      message("K1=", K1, " # Landsat7_Handbook.pdf Table 11.5")
                      message("K2=", K2, " # Landsat7_Handbook.pdf Table 11.5")
                      d <- as.integer(x@data$tirs1$lsb[ilook, jlook])
                      stop("landsat-5 is not converted AT ALL\n")
                  } else {
                      stop("unknown satellite: ", x@metadata$spacecraft)
                  }
                  oceDebug(debug, "} # landsat [[\n", unindent=1)
                  return(d)
              }
              ## message("i:", i, " before")
              iorig <- i
              if (is.character(i)) {
                  ii <- pmatch(i, names(x@data))
                  if (!is.na(ii))
                      i <- ii
              }
              if (is.na(ii))
                  stop("this landsat object lacks a band named \"", i, "\"", call.=FALSE)
              oceDebug(getOption("oceDebug"), "band:", iorig, "\n")
              isList <- is.list(x@data[[i]])
              if (isList) {
                  msb <- x@data[[i]]$msb
                  lsb <- x@data[[i]]$lsb
              } else {
                  d <- x@data[[i]]
              }
              rm(x)                    # may help if memory is tight
              dim <- if (isList) dim(lsb) else dim(d) # altered if decimation
              ##message("dim=c(", dim[1], ",", dim[2], ") originally")
              ## e.g. image[["panchromatic", TRUE]]
              if (!missing(j) && is.logical(j) && j) {
                  ##message("autodecimate if image is large")
                  maxdim <- max(dim)
                  if (maxdim > 800) {
                      decimate <- max(as.integer(round(maxdim / 800)), 1)
                      ##message("autodecimate by factor ", decimate)
                      ilook <- seq.int(1, dim[1], by=decimate)
                      jlook <- seq.int(1, dim[2], by=decimate)
                      if (isList) {
                          lsb <- lsb[ilook, jlook] # rewrite in place, possibly saving memory
                          res <- if (is.null(dim(msb))) as.integer(lsb) else
                              256L*as.integer(msb[ilook, jlook]) + as.integer(lsb)
                          dim(res) <- dim(lsb)
                          oceDebug(getOption("oceDebug"), "} # \"[[\"\n", unindent=1)
                          return(res)
                      } else {
                          oceDebug(getOption("oceDebug"), "} # \"[[\"\n", unindent=1)
                          return(d[ilook, jlook])
                      }
                  }
              }
              ## e.g. image[["panchromatic", 10]]
              if (!missing(j) && is.numeric(j)) {
                  j <- as.integer(round(j))
                  if (j > 1) {
                      ##message("decimate by factor ", j)
                      ilook <- seq.int(1, dim[1], by=j)
                      jlook <- seq.int(1, dim[2], by=j)
                      if (isList) {
                          if (!is.null(dim(msb)))
                              msb <- msb[ilook, jlook]
                          lsb <- lsb[ilook, jlook]
                          res <- 256L*as.integer(msb) + as.integer(lsb)
                          dim(res) <- dim(lsb)
                          oceDebug(debug, "} # landsat [[\n", unindent=1)
                          return(res)
                      } else {
                          d <- d[ilook, jlook]
                          oceDebug(debug, "} # landsat [[\n", unindent=1)
                          return(d)
                      }
                  }
              }
              ## OK, no decimation is requested, so just return the desired value.
              if (isList) {
                  res <- 256L*as.integer(msb) + as.integer(lsb)
                  dim(res) <- dim(lsb)
                  oceDebug(debug, "} # landsat [[\n", unindent=1)
                  return(res)
              } else {
                  oceDebug(debug, "} # landsat [[\n", unindent=1)
                  return(d)
              }
          })


#' @title Replace Parts of a landsat Object
#'
#' @param x a [landsat-class] object.
#'
#' @template sub_subsetTemplate
#'
#' @family things related to landsat data
setMethod(f="[[<-",
          signature(x="landsat", i="ANY", j="ANY"),
          definition=function(x, i, j, ..., value) {
              callNextMethod(x=x, i=i, j=j, ..., value=value) # [[<-
          })


#' Plot a landsat Object
#'
#' Plot the data within a landsat image, or information computed from the
#' data. The second category includes possibilities such as an estimate of
#' surface temperature and the `"terralook"` estimate of a natural-color
#' view.
#'
#' @details
#' For Landsat-8 data, the `band` may be
#' one of: `"aerosol"`, `"blue"`, `"green"`, `"red"`,
#' `"nir"`, `"swir1"`, `"swir2"`, `"panchromatic"`,
#' `"cirrus"`, `"tirs1"`, or `"tirs2"`.
#'
#' For Landsat-7 data, `band` may be one of `"blue"`, `"green"`, `"red"`,
#' `"nir"`, `"swir1"`, `"tirs1"`, `"tirs2"`,
#' `"swir2"`, or `"panchromatic"`.
#'
#' For Landsat data prior to
#' Landsat-7, `band` may be one of `"blue"`, `"green"`,
#' `"red"`, `"nir"`, `"swir1"`, `"tirs1"`,
#' `"tirs2"`, or `"swir2"`.
#'
#' If `band` is not given, the
#' (`"tirs1"`) will be used if it exists in the object data, or
#' otherwise the first band will be used.
#'
#' In addition to the above there are also some pseudo-bands that
#' can be plotted, as follows.
#'
#' * Setting `band="temperature"` will plot an estimate
#' of at-satellite brightness temperature, computed from the
#' `tirs1` band.
#'
#' * Setting `band="terralook"` will plot a sort of natural
#' color by combining the `red`, `green`, `blue` and
#' `nir` bands according to the formula provided at
#' `https://lta.cr.usgs.gov/terralook/what_is_terralook` (a
#' website that worked once, but failed as of Feb 2, 2017), namely
#' that the `red`-band data are provided as the `red`
#' argument of the [rgb()] function, while
#' the `green` argument is computed as
#' 2/3 of the `green`-band data plus 1/3 of the `nir`-band data, and
#' the `blue` argument is computed as 2/3 of the `green`-band
#' data minus 1/3 of the `nir`-band data. (This is not a typo: the
#' `blue` band is not used.)
#'
#'
#' @param x a [landsat-class] object.
#'
#' @param band If given, the name of the band.  For Landsat-8 data, this may be
#' one of: `"aerosol"`, `"blue"`, `"green"`, `"red"`,
#' `"nir"`, `"swir1"`, `"swir2"`, `"panchromatic"`,
#' `"cirrus"`, `"tirs1"`, or `"tirs2"`.  For Landsat-7 data,
#' this may be one of `"blue"`, `"green"`, `"red"`,
#' `"nir"`, `"swir1"`, `"tirs1"`, `"tirs2"`,
#' `"swir2"`, or `"panchromatic"`.  For Landsat data prior to
#' Landsat-7, this may be one of `"blue"`, `"green"`,
#' `"red"`, `"nir"`, `"swir1"`, `"tirs1"`,
#' `"tirs2"`, or `"swir2"`.  If `band` is not given, the
#' (`"tirs1"`) will be used if it exists in the object data, or
#' otherwise the first band will be used.  In addition to the above, using
#' `band="temperature"` will plot an estimate of at-satellite
#' brightness temperature, computed from the `tirs1` band, and
#' `band="terralook"` will plot a sort of natural color by combining
#' the `red`, `green`, `blue` and `nir` bands
#' according to the formula provided at
#' `https://lta.cr.usgs.gov/terralook/what_is_terralook` (a
#' website that worked once, but failed as of Feb 2, 2017).
#'
#' @param which Desired plot type; 1=image, 2=histogram.
#'
#' @param decimate An indication of the desired decimation,
#' passed to [imagep()] for image plots.
#' The default yields faster plotting.  Some decimation is sensible for
#' full-size images, since no graphical displays can show 16 thousand pixels
#' on a side.
#'
#' @param zlim Either a pair of numbers giving the limits for the colorscale,
#' or `"histogram"` to have a flattened histogram (i.e. to maximally
#' increase contrast throughout the domain.)  If not given, the 1 and 99
#' percent quantiles are calculated and used as limits.
#'
#' @param utm A logical value indicating whether to use UTS (easting and northing) instead
#' of longitude and latitude on plot.
#'
#' @param col Either a function yielding colors, taking a single integer
#' argument with the desired number of colors, or the string
#' `"natural"`, which combines the information in the `red`,
#' `green` and `blue` bands and produces a natural-hue image.  In
#' the latter case, the band designation is ignored, and the object must
#' contain the three color bands.
#'
#' @param drawPalette Indication of the type of palette to draw, if
#' any. See [imagep()] for details.
#'
#' @param showBandName A logical indicating whether the band name is to
#' plotted in the top margin, near the right-hand side.
#'
#' @param alpha.f Argument used if `col="natural"`, to adjust colors
#' with [adjustcolor()].
#'
#' @param red.f Argument used if `col="natural"`, to adjust colors with
#' [adjustcolor()].  Higher values of `red.f` cause red hues
#' to be emphasized (e.g. dry land).
#'
#' @param green.f Argument used if `col="natural"`, to adjust colors with
#' [adjustcolor()].  Higher values of `green.f` emphasize
#' green hues (e.g. forests).
#'
#' @param blue.f Argument used if `band="terralook"`, to adjust colors with
#' [adjustcolor()].  Higher values of `blue.f` emphasize blue
#' hues (e.g. ocean).
#'
#' @param offset Argument used if `band="terralook"`, to adjust colors with
#' [adjustcolor()].
#'
#' @param transform Argument used if `band="terralook"`, to adjust colors
#' with [adjustcolor()].
#'
#' @param debug Set to a positive value to get debugging information during
#' processing.
#'
#' @param ... optional arguments passed to plotting functions.
#'
#' @concept satellite
#'
#' @author Dan Kelley
#'
#' @family things related to landsat data
#' @family functions that plot oce data
#'
#' @aliases plot.landsat
setMethod(f="plot",
          signature=signature("landsat"),
          definition=function(x, band, which=1, decimate=TRUE, zlim, utm=FALSE,
                              col=oce.colorsPalette,
                              drawPalette=TRUE,
                              showBandName=TRUE,
                              alpha.f=1, red.f=1.7, green.f=1.5, blue.f=6,
                              offset=c(0, -0.05, -0.2, 0),
                              transform=diag(c(red.f, green.f, blue.f, alpha.f)),
                              debug=getOption("oceDebug"), ...)
          {
              oceDebug(debug, "plot,landsat-method(..., which=c(", paste(which, collapse=","),
                       "), decimate=", decimate,
                       ", zlim=", if (missing(zlim)) "(missing)" else zlim,
                       ", ...) {\n", sep="", unindent=1)
              if (!length(x@data)) {
                  warning("In plot,landsat-method(): object contains no band data\n", call.=FALSE)
                  return(invisible(NULL))
              }
              terralook <- FALSE
              datanames <- names(x@data)
              spacecraft <- if (is.null(x@metadata$spacecraft)) "LANDSAT_8" else x@metadata$spacecraft
              d <- NULL
              kelley <- FALSE # FIXME:kelley
              if (which == 1) {
                  if (!missing(band) && is.character(band) && !is.na(pmatch(band, "kelley"))) {
                      kelley <- TRUE
                      if (missing(drawPalette)) drawPalette <- FALSE
                      if (!("red" %in% datanames && "green" %in% datanames && "blue" %in% datanames))
                          stop("band=\"kelley\" requires landsat object to contain \"red\", \"green\" and \"blue\"")
                      message("'kelley' band -- TEMPORARY test; set debug=3 for more")
                      r <- x[["red", decimate]]
                      g <- x[["green", decimate]]
                      b <- x[["blue", decimate]]
                      dim <- dim(r)
                      if (spacecraft == "LANDSAT_8") {
                          oceDebug(debug, "colors for landsat 8 (range 0 to 2^16-1)\n")
                          colors <- rgb(r, g, b, maxColorValue=2^16-1)
                      } else {
                          oceDebug(debug, "colors for landsat 7 (range 0 to 2^8-1)\n")
                          colors <- rgb(r, g, b, maxColorValue=2^8-1)
                      }
                      rm(list=c("r", "g", "b")) # clean up asap
                      col <- unique(colors)
                      d <- array(match(colors, col), dim=dim) # method of Clark Richards
                      oceDebug(debug, "color compaction: ", floor(prod(dim)/length(col)), '\n')
                      oceDebug(debug, "adjusting colors: orig=", paste(head(col), collapse=" "), "\n")
                      col <- adjustcolor(col, alpha.f=alpha.f, red.f=red.f, green.f=green.f, blue.f=blue.f,
                                         offset=offset, transform=transform)
                      oceDebug(debug, "adjusting colors: new=", paste(head(col), collapse=" "), "\n")
                      oceDebug(debug, "finished constucting image\n")
                  } else if (!missing(band) && is.character(band) && !is.na(pmatch(band, "terralook"))) {
                      terralook <- TRUE
                      if (missing(drawPalette)) drawPalette <- FALSE
                      if (!("red" %in% datanames && "green" %in% datanames && "nir" %in% datanames))
                          stop("band=\"terralook\" requires landsat object to contain \"red\", \"green\" and \"nir\"")
                      oceDebug(debug, "extracting red data\n")
                      r <- x[["red", decimate]]
                      oceDebug(debug, "range(red): ", paste(range(r), collapse=" to "), "\n")
                      dim <- dim(r)
                      oceDebug(debug, "extracting green data\n")
                      g23 <- 2 / 3 * x[["green", decimate]]
                      oceDebug(debug, "range(green): ", paste(range(g23), collapse=" to "), "\n")
                      oceDebug(debug, "extracting nir data\n")
                      nir3 <- x[["nir", decimate]]/3
                      oceDebug(debug, "range(nir/3): ", paste(range(nir3), collapse=" to "), "\n")
                      ## na <- r==0 && g23==0 && nir3==0
                      ## formula from what_is_terralook website
                      g <- g23 + nir3
                      b <- g23 - nir3
                      rm(list=c("g23", "nir3")) # clean up asap
                      g[g<0] <- 0
                      b[b<0] <- 0
                      if (spacecraft == "LANDSAT_8") {
                          oceDebug(debug, "colors for landsat 8 (range 0 to 2^16-1)\n")
                          colors <- rgb(r, g, b, maxColorValue=2^16-1)
                      } else {
                          oceDebug(debug, "colors for landsat 7 (range 0 to 2^8-1)\n")
                          colors <- rgb(r, g, b, maxColorValue=2^8-1)
                      }
                      rm(list=c("r", "g", "b")) # clean up asap
                      col <- unique(colors)
                      d <- array(match(colors, col), dim=dim) # method of Clark Richards
                      oceDebug(debug, "color compaction: ", floor(prod(dim)/length(col)), '\n')
                      oceDebug(debug, "adjusting colors: orig=", paste(head(col), collapse=" "), "\n")
                      col <- adjustcolor(col, alpha.f=alpha.f, red.f=red.f, green.f=green.f, blue.f=blue.f,
                                         offset=offset, transform=transform)
                      oceDebug(debug, "adjusting colors: new=", paste(head(col), collapse=" "), "\n")
                      oceDebug(debug, "finished constucting image\n")
                      ## end of band="terralook"; plot below
                  } else {
                      ## not band="terralook"
                      if (missing(band)) {
                          if ("tirs1" %in% names(x@data)) {
                              ## different meanings landsat-8 and previous
                              oceDebug(debug, "using tirs1\n")
                              d <- x[["tirs1", decimate]]
                              band <- "tirs1"
                          }  else {
                              oceDebug(debug, "using band named", datanames[1], "\n")
                              d <- x[[datanames[1], decimate]]
                              band <- datanames[1]
                          }
                          d[d == 0] <- NA # only makes sense for count data
                      } else {
                          ## See if band is stored in this object
                          knownBands <- c("temperature", datanames)
                          band <- band[1]
                          i <- pmatch(band, knownBands)
                          if (is.na(i))
                              stop("this landsat object has no band named \"", band, "\"", call.=FALSE)
                          band <- knownBands[i]
                          d <- x[[band, decimate]]
                          if (!any(!is.na(d))) {
                              if (band[1] == "temperature") {
                                  stop("cannot compute landsat temperature; see e.g. http://landsat.usgs.gov/mission_headlines2015.php",
                                       call.=FALSE)
                              } else {
                                  stop("landsat object has only missing values in the \"", band, "\" band", call.=FALSE)
                              }
                          }
                          ##if (0 == sum(d, na.rm=TRUE))
                          if (all(d == 0))
                              stop("landsat object has only zero values in the \"", band, "\" band", call.=FALSE)
                          if (is.na(pmatch(band, "temperature")))
                              d[d == 0] <- NA  # only makes sense for count data
                      }
                  }
                  dim <- dim(d)
                  lon <- x@metadata$lllon + seq(0, 1, length.out=dim[1]) * (x@metadata$urlon - x@metadata$lllon)
                  lat <- x@metadata$lllat + seq(0, 1, length.out=dim[2]) * (x@metadata$urlat - x@metadata$lllat)
                  oceDebug(debug, "old lon range: ", paste(range(lon), collapse=" to "), "\n")
                  oceDebug(debug, "old lat range: ", paste(range(lat), collapse=" to "), "\n")
                  LON0 <- 0.5 * (x@metadata$lllon + x@metadata$ullon)
                  LON1 <- 0.5 * (x@metadata$lrlon + x@metadata$urlon)
                  LAT0 <- 0.5 * (x@metadata$lllat + x@metadata$lrlat)
                  LAT1 <- 0.5 * (x@metadata$ullat + x@metadata$urlat)
                  lon <- LON0 + seq(0, 1, length.out=dim[1]) * (LON1 - LON0)
                  lat <- LAT0 + seq(0, 1, length.out=dim[2]) * (LAT1 - LAT0)
                  oceDebug(debug, "new lon range: ", paste(range(lon), collapse=" to "), "\n")
                  oceDebug(debug, "new lat range: ", paste(range(lat), collapse=" to "), "\n")
                  oceDebug(debug, "dim: ", paste(dim, collapse=" "), "\n")

                  asp <- 1 / cos(0.5 * (x@metadata$lllat + x@metadata$urlat) * pi / 180)
                  if (missing(zlim) && !terralook && !kelley) # FIXME:kelley
                      zlim <- quantile(d, c(0.01, 0.99), na.rm=TRUE)
                  if (utm) {
                      if (!("llUTM" %in% names(x@metadata))) {
                          x@metadata$llUTM <- lonlat2utm(x@metadata$lllon, x@metadata$lllat, zone=x@metadata$zoneUTM)
                          x@metadata$urUTM <- lonlat2utm(x@metadata$urlon, x@metadata$urlat, zone=x@metadata$zoneUTM)
                      }
                      imagep(x=0.001*seq(x@metadata$llUTM$easting, x@metadata$urUTM$easting, length.out=dim[1]),
                             y=0.001*seq(x@metadata$llUTM$northing, x@metadata$urUTM$northing, length.out=dim[2]),
                             z=d, asp=1, zlim=zlim, col=col, decimate=FALSE,
                             drawPalette=drawPalette, debug=debug-1, ...)
                  } else {
                      if ("breaks" %in% names(list(...))) {
                          imagep(x=lon, y=lat, z=d, asp=asp, col=col, decimate=FALSE,
                                 drawPalette=drawPalette, debug=debug-1, ...)
                      } else {
                          imagep(x=lon, y=lat, z=d, asp=asp, zlim=zlim, col=col, decimate=FALSE,
                                 drawPalette=drawPalette, debug=debug-1, ...)
                      }
                  }
                  if (showBandName && !terralook && !kelley)
                      mtext(band, side=3, adj=1, line=0, cex=1)
              } else if (which == 2) {
                  if (missing(band)) {
                      if ("tirs1" %in% names(x@data)) {
                          ## different meanings landsat-8 and previous
                          oceDebug(debug, "using tirs1\n")
                          d <- x[["tirs1", decimate]]
                          band <- "tirs1"
                      }  else {
                          oceDebug(debug, "using band named", datanames[1], "\n")
                          d <- x[[datanames[1], decimate]]
                          band <- datanames[1]
                      }
                  } else {
                      d <- x[[band]]
                  }
                  d[d == 0] <- NA # ignore 'data' outside footprint

                  if ("breaks" %in% names(list(...))) {
                      hist(d, xlab="Value", main="", ...)
                  } else {
                      hist(d, xlab="Value", main="", breaks=100, ...)
                  }
                  if (showBandName)
                      mtext(band, side=3, adj=1)
              } else {
                  stop("unknown value of 'which'")
              }
              oceDebug(debug, "} # plot,landsat-method()\n", unindent=1)
          })


read.landsatmeta <- function(file, debug=getOption("oceDebug"))
{
    getItem <- function(info, name, numeric=TRUE)
    {
        line <- grep(paste("^[ ]*", name, "[ ]*=[ ]*", sep=""), info)
        res <- NULL
        if (length(line)) {
            res <- strsplit(info[line[1]], "=")[[1]][2]
            res <- gsub("^[ ]+", "", res)
            res <- gsub("[ ]+$", "", res)
        }
        res <- if (numeric) as.numeric(res) else gsub("\"", "", res)
        ##oceDebug(debug, "read item", name, "\n")
        res
    }
    info <- readLines(file, warn=FALSE)
    date <- getItem(info, "DATE_ACQUIRED", numeric=FALSE)
    centerTime <- getItem(info, "SCENE_CENTER_TIME", numeric=FALSE)
    time <- as.POSIXct(paste(date, centerTime), tz="UTC")
    spacecraft <- getItem(info, "SPACECRAFT_ID", numeric=FALSE)
    id <- getItem(info, "LANDSAT_SCENE_ID", numeric=FALSE)
    ## Bounding region (not a latlon box!)
    ullat <- getItem(info, "CORNER_UL_LAT_PRODUCT")
    ullon <- getItem(info, "CORNER_UL_LON_PRODUCT")
    urlat <- getItem(info, "CORNER_UR_LAT_PRODUCT")
    urlon <- getItem(info, "CORNER_UR_LON_PRODUCT")
    lllat <- getItem(info, "CORNER_LL_LAT_PRODUCT")
    lllon <- getItem(info, "CORNER_LL_LON_PRODUCT")
    lrlat <- getItem(info, "CORNER_LR_LAT_PRODUCT")
    lrlon <- getItem(info, "CORNER_LR_LON_PRODUCT")
    zoneUTM <- getItem(info, "UTM_ZONE")
    llUTM <- list(easting=getItem(info, "CORNER_LL_PROJECTION_X_PRODUCT"),
                  northing=getItem(info, "CORNER_LL_PROJECTION_Y_PRODUCT"),
                  zone=zoneUTM)
    urUTM <- list(easting=getItem(info, "CORNER_UR_PROJECTION_X_PRODUCT"),
                  northing=getItem(info, "CORNER_UR_PROJECTION_Y_PRODUCT"),
                  zone=zoneUTM)
    ## Cell sizes
    gridCellSizePanchromatic <- getItem(info, "GRID_CELL_SIZE_PANCHROMATIC")
    gridCellSizeReflective <- getItem(info, "GRID_CELL_SIZE_REFLECTIVE")
    gridCellSizeThermal <- getItem(info, "GRID_CELL_SIZE_THERMAL")
    ## ## Image dimensions
    ## l <- getItem(info, "PANCHROMATIC_LINES")
    ## s <- getItem(info, "PANCHROMATIC_SAMPLES")
    ## dimPanchromatic <- c(l, s)         # or reverse?
    ## l <- getItem(info, "REFLECTIVE_LINES")
    ## s <- getItem(info, "REFLECTIVE_SAMPLES")
    ## dimReflective <- c(l, s)
    ## l <- getItem(info, "THERMAL_LINES")
    ## s <- getItem(info, "THERMAL_SAMPLES")
    ## dimThermal <- c(l, s)
    ## Select just certain lines.  The header is short, so doing it by
    ## steps, just in case the data format changes later and adjustment
    ## is required.
    info2 <- info[grep("GROUP", info, invert=TRUE)] # delete grouping commands
    info3 <- info2[grep("=", info2)] # select assignments
    info4 <- gsub("^\\s+", "", info3) # remove leading whitespace
    info5 <- gsub("\\s+$", "", info4) # remove trailing whitespace
    S <- strsplit(info5, ' = ')
    names <- as.character(lapply(S, function(s) s[[1]]))
    values <- gsub('"', '', as.character(lapply(S, function(s) s[[2]]))) # FIXME: some are numeric
    header <- as.vector(values)
    names(header) <- tolower(names)
    header <- as.list(header)
    ## Make numeric if possible
    for (i in seq_along(header)) {
        try(header[[i]] <- scan(text=header[[i]], quiet=TRUE), silent=TRUE)
    }
    ## Band (L4TM, L5TM, and L7ETM+) names from http://landsat.usgs.gov/best_spectral_bands_to_use.php
    if ("LANDSAT_4" == spacecraft)  {
        bandnames <- c("blue", "green", "red", "nir", "swir1", "tirs1", "tirs2", "swir2")
        filesuffices <- c("B1", "B2", "B3", "B4", "B5", "B6_VCID_1", "B6_VCID_2", "B7")
    } else if ("LANDSAT_5" == spacecraft)  {
        bandnames <- c("blue", "green", "red", "nir", "swir1", "tirs1", "tirs2", "swir2")
        filesuffices <- c("B1", "B2", "B3", "B4", "B5", "B6_VCID_1", "B6_VCID_2", "B7")
    } else if ("LANDSAT_7" == spacecraft)  {
        bandnames <- c("blue", "green", "red", "nir", "swir1", "tirs1", "tirs2", "swir2", "panchromatic")
        filesuffices <- c("B1", "B2", "B3", "B4", "B5", "B6_VCID_1", "B6_VCID_2", "B7", "B8")
    } else if ("LANDSAT_8" == spacecraft)  {
        bandnames <- c("aerosol", "blue", "green", "red", "nir", "swir1", "swir2", "panchromatic", "cirrus", "tirs1", "tirs2")
        filesuffices <- c("B1", "B2", "B3", "B4", "B5", "B6", "B7", "B8", "B9", "B10", "B11")
    } else {
        stop("spacecraft type ", spacecraft, " cannot be handled yet")
    }
    filesuffices <- paste(filesuffices, ".TIF", sep="")
    list(header=header,
         time=time, spacecraft=spacecraft, id=id,
         bandnames=bandnames, filesuffices=filesuffices,
         ullat=ullat, ullon=ullon, urlat=urlat, urlon=urlon, ## possibly not needed with UTM
         lllat=lllat, lllon=lllon, lrlat=lrlat, lrlon=lrlon, ## possibly not needed with UTM
         llUTM=llUTM, urUTM=urUTM, zoneUTM=zoneUTM,
         gridCellSizePanchromatic=gridCellSizePanchromatic,
         gridCellSizeReflective=gridCellSizeReflective,
         gridCellSizeThermal=gridCellSizeThermal)
         ##dimPanchromatic=dimPanchromatic,
         ##dimReflective=dimReflective,
         ##dimThermal=dimThermal)
}


#' Read a landsat File Directory
#'
#' Read a landsat data file, producing an object of [landsat-class].
#' The actual reading is done with [tiff::readTIFF()] in the
#' \CRANpkg{tiff} package, so that package must be installed for
#' `read.landsat` to work.
#'
#' @details
#' Landsat data are provided in directories that contain TIFF files and header
#' information, and `read.landsat` relies on a strict convention for the
#' names of the files in those directories.  Those file names were found by
#' inspection of some data, on the assumption that similar patterns will hold for
#' other datasets for any given satellite. This is a brittle approach and it
#' should be born in mind if `read.landsat` fails for a given dataset.
#'
#' For Landsat 8, there are 11 bands, with names `"aerosol"` (band 1),
#' `"blue"` (band 2), `"green"` (band 3), `"red"` (band 4),
#' `"nir"` (band 5), `"swir1"` (band 6), `"swir2"` (band 7),
#' `"panchromatic"` (band 8), `"cirrus"` (band 9), `"tirs1"` (band
#' 10), and `"tirs2"` (band 11).
#' In addition to the above, setting `band="terralook"` may be used as
#' an abbreviation for `band=c("red", "green", "nir")`.
#'
#' For Landsat 7, there 8 bands, with names `"blue"` (band 1), `"green"`
#' (band 2), `"red"` (band 3), `"nir"` (band 4), `"swir1"` (band
#' 5), `"tir1"` (band 6A), `"tir2"` (band 6B), `"swir2"` (band 7)
#' and `"panchromatic"` (band 8).
#'
#' For Landsat 4 and 5, the bands similar to Landsat 7 but without
#' `"panchromatic"` (band 8).
#'
#
#' @param file A connection or a character string giving the name of the file to
#' load.  This is a directory name containing the data.
#'
#' @param band The bands to be read, by default all of the bands.  Use
#' `band=NULL` to skip the reading of bands, instead reading only the
#' image metadata, which is often enough to check if the image is of
#' interest in a given study. See \sQuote{Details} for the names of the
#' bands, some of which are pseudo-bands, computed from the actual data.
#'
#' @param emissivity Value of the emissivity of the surface, stored as
#' `emissivity` in the `metadata` slot of the
#' resultant object. This is used in the
#' calculation of surface temperature, as explained in the discussion of
#' accessor functions for [landsat-class]. The default value is
#' from Konda et al.  (1994). These authors suggest an uncertainty of 0.04,
#' but a wider range of values can be found in the literature.  The value of
#' `metadata$emissivity` is easy to alter, either as a single value or
#' as a matrix, yielding flexibility of calculation.
#'
#' @param decimate optional positive integer indicating the degree to which
#' the data should be subsampled after reading and before storage. Setting
#' this to 10 can speed up reading by a factor of 3 or more, but higher values
#' have diminishing effect.  In exploratory work, it is useful to set
#' `decimate=10`, to plot the image to determine a subregion
#' of interest, and then to use [landsatTrim()] to trim the image.
#'
#' @param debug a flag that turns on debugging.  Set to 1 to get a moderate
#' amount of debugging information, or to 2 to get more.
#'
#' @section Storage requirements:
#'
#' Landsat data files (directories, really) are large, accounting for
#' approximately 1 gigabyte each.  The storage of the Oce object is
#' similar (see [landsat-class]).  In R, many operations involving
#' copying data, so that dealing with full-scale landsat images can overwhelm
#' computers with storage under 8GB.  For this reason, it is typical to read just
#' the bands that are of interest.  It is also helpful to use
#' [landsatTrim()] to trim the data to a geographical range, or
#' to use [decimate()] to get a coarse view of the domain, especially
#' early in an analysis.
#'
#' @return A [landsat-class] object, with the conventional Oce
#' slots `metadata`, `data` and `processingLog`.  The
#' `metadata` is mainly intended for use by Oce functions, but for generality
#' it also contains an entry named `header` that represents the full image
#' header in a list (with names made lower-case).  The `data` slot holds
#' matrices of the data in the requested bands, and users may add extra matrices
#' if desired, e.g. to store calculated quantities.
#'
#' @seealso
#'
#' See the documentation for the [landsat-class] class
#' for more information on `landsat` objects,
#' especially band information.  Use [landsatTrim()] to trim Landsat
#' objects geographically and [landsatAdd()] to add new ``bands.''  The
#' accessor operator (`[[`) is used to access band information, full or
#' decimated, and to access certain derived quantities.  A sample dataset named
#' [landsat()] is provided by the \CRANpkg{oce} package.
#'
#' @references
#'
#' 1. Konda, M. Imasato N., Nishi, K., and T. Toda, 1994.  Measurement of the Sea
#' Surface Emissivity.  *Journal of Oceanography*, 50, 17:30.  Available at
#' \url{http://www.terrapub.co.jp/journals/JO/pdf/5001/50010017.pdf} as of
#' February 2015.
#'
#' @author Dan Kelley
#'
#' @concept satellite
#'
#' @family things related to landsat data
read.landsat <- function(file, band="all", emissivity=0.984, decimate, debug=getOption("oceDebug"))
{
    if (!missing(file) && is.character(file) && 0 == file.info(file)$size)
        stop("empty file")
    oceDebug(debug, "read.landsat(file=\"", file, "\",",
             if (length(band) > 1) paste("band=c(\"", paste(band, collapse="\",\""), "\")", sep="") else
                 paste("band=\"", band, "\"", sep=""),
                 ", debug=", debug, ") {\n", sep="", unindent=1)
    if (band[1] == "terralook")
        band <- c("red", "green", "nir")
    decimateGiven <- !missing(decimate)
    if (decimateGiven && decimate < 1)
        warning("invalid value of decimate (", decimate, ") being ignored")
    if (!requireNamespace("tiff", quietly=TRUE))
        stop('must install.packages("tiff") to read landsat data')
    res <- new("landsat")
    file <- gsub("/$", "", file)
    actualfilename <- gsub("/$", "", file) # permit e.g. "LE71910202005194ASN00/"
    actualfilename <- gsub(".*/", "", actualfilename)
    headerfilename <- paste(file, "/", actualfilename, "_MTL.txt", sep="")
    header <- read.landsatmeta(headerfilename, debug=debug-1)
    oceDebug(debug, "file type: ", header$spacecraft, "\n")
    ## convert to numerical bands (checks also that named bands are OK)
    ##bandOrig <- band
    if (!is.null(band)) {
        if (band[1] == "all") {
            band <- header$bandnames
        }
        band2 <- rep(NA, length(band))
        for (b in seq_along(band)) {
            if (is.character(band[b])) {
                ##message("b:", b, " band[b]:", band[b], " bandnames:", paste(header$bandnames, sep=","))
                m <- pmatch(band[b], header$bandnames, nomatch=0)
                if (0 == m)
                    stop('band "', band[b], '" unknown; must be one of: ', paste(header$bandnames, collapse=", "))
                else
                    band2[b] <- m
            } else {
                band2[b] <- band[b]
            }
        }
        band <- band2
        oceDebug(debug, "numerical version of band=c(", paste(band, collapse=","), ")\n", sep="")
    }
    for (name in names(header))
        res@metadata[[name]] <- header[[name]]
    res@metadata$spacecraft <- header$spacecraft
    res@metadata$id <- header$id
    res@metadata$emissivity <- emissivity
    res@metadata$filename <- file
    res@metadata$headerfilename <- headerfilename
    ## Bandnames differ by satellite.
    res@metadata$bands <- band
    actualfilename <- gsub(".*/", "", file)
##    res@metadata[["bandfiles"]] <- paste(file,"/",actualfilename,"_B",band,".TIF",sep="")
    options <- options('warn') # avoid readTIFF() warnings about geo tags
    options(warn=-1)
    ##> cat("BEFORE\n")
    ##> print(presentTime())
    for (b in seq_along(band)) {
        ## 'band' is numeric
        ## message("b:", b, " band: ", header$bandnames[b], " suffix: ", header$filesuffices[b])
        ##bandfilename <- paste(file, "/", actualfilename, "_B", band[b], ".TIF", sep="")
        bandfilename <- paste(file, "/", actualfilename, "_", header$filesuffices[band[b]], sep="") # FIXME: 1 more layer of indexing?
        ## message(bandfilename)
        ##res@metadata[["filename"]] <- bandfilename
        ##> oceDebug(debug, "reading \"", header$bandnames[band[b]], "\" band in \"", bandfilename, "\"\n", sep="")
        ## FIXME: should also handle JPG data (i.e. previews)
        ##> cat("reading ", header$bandnames[band[b]], "\n")
        ##> print(system.time(
        d <- tiff::readTIFF(bandfilename)
        if (decimateGiven) {
            dim <- dim(d)
            d <- d[seq.int(1, dim[1], by=decimate), seq.int(1, dim[2], by=decimate)]
        }
        ##>))
        ##> cat("---DONE reading ", header$bandnames[band[b]], "\n")
        ## if (FALSE && !is.null(getOption("testLandsat1"))) { # FIXME: disable
        ##bandname <- header$bandnames[band[b]] # FIXME: 1 more layer of indexing?
        #if (is.null(x@metadata$spacecraft) || x@metadata$spacecraft == "LANDSAT_7") {
        ##> print("assembling bytes")
        ##> print(system.time({
        if ("LANDSAT_8" == header$spacecraft) {
            dd <- do_landsat_numeric_to_bytes(d, 16L)
            res@data[[header$bandnames[band[b]]]] <- list(msb=do_landsat_transpose_flip(dd$msb),
                                                          lsb=do_landsat_transpose_flip(dd$lsb))
        } else {
            ## FIXME: assume all others are 1-byte, like LANDSAT_7
            dd <- do_landsat_numeric_to_bytes(d, 8L)
            res@data[[header$bandnames[band[b]]]] <- list(msb=0,
                                                          lsb=do_landsat_transpose_flip(dd$lsb))
        }
        ##> }))
        ##> print("--DONE assembling bytes")
    }
    ##> cat("AFTER\n")
    ##> print(presentTime())
    options(warn=options$warn)
    res@metadata$satellite <- "landsat"
    res@processingLog <- processingLogAppend(res@processingLog,
                                        paste(deparse(match.call()), sep="", collapse=""))
    oceDebug(debug, "} # read.landsat()\n", unindent=1)
    res
}


#' Add a Band to a landsat Object
#'
#' Add a band to a [landsat-class] object. Note that
#' it will be stored in numeric form, not raw form, and therefore
#' it will require much more storage than data read with
#' [read.landsat()].
#'
#' @param x a [landsat-class] object.
#'
#' @param data A matrix of data, with dimensions matching that of entries already in `x`.
#'
#' @param name The name to be used for the data, i.e. the data can later be
#' accessed with `d[[name]]` where `d` is the name of the return value
#' from the present function.
#'
#' @param debug A flag that turns on debugging.  Set to 1 to get a moderate amount of debugging
#' information, or a higher value for more debugging.
#'
#' @return A [landsat-class] object, with a new data band.
#'
#' @seealso
#' The documentation for the [landsat-class] class explains the
#' structure of landsat objects, and also outlines the other functions dealing
#' with them.
#'
#' @author Dan Kelley
#'
#' @concept satellite
#'
#' @family things related to landsat data
landsatAdd <- function(x, data, name, debug=getOption("oceDebug"))
{
    if (!is.matrix(data))
        stop("data must be a matrix")
    if (missing(name))
        stop("must provide a name for the data")
    dimNew <- dim(data)
    dimOld <- dim(x@data[[1]]$msb)
    if (any(dimNew != dimOld))
        stop("dim(data) = c(", dimNew[1], ",", dimNew[2], ") must match existing dimension c(", dimOld[1], ",", dimOld[2], ")")
    res <- x
    res@data[[name]] <- data
    res
}

#' Trim a landsat Image to a Geographical Region
#'
#' Trim a landsat image to a latitude-longitude box. This is only an approximate
#' operation, because landsat images are provided in x-y coordinates, not
#' longitude-latitude coordinates.
#'
#' @details
#' As of June 25, 2015, the matrices storing the image data are trimmed to indices
#' determined by linear interpolation based on the location of the `ll` and
#' `ur` corners within the lon-lat corners specified in the image data. (A
#' previous version trimmed in UTM space, and in fact this may be done in future
#' also, if a problem in lonlat/utm conversion is resolved.) An error results if
#' there is no intersection between the trimming box and the image box.
#'
#' @param x a [landsat-class] object.
#'
#' @param ll A list containing `longitude` and `latitude`, for the
#' lower-left corner of the portion of the image to retain, or a vector
#' with first element longitude and second element latitude. If provided,
#' then `ur` must also be provided, but `box` cannot.
#'
#' @param ur A list containing `longitude` and `latitude`, for the
#' upper-right corner of the portion of the image to retain, or a vector
#' with first element longitude and second element latitude. If provided,
#' then `ll` must also be provided, but `box` cannot.
#'
#' @param box A list containing `x` and `y` (each of length 2),
#' corresponding to the values for `ll` and `ur`, such as would
#' be produced by a call to `locator(2)`. If provided, neither
#' `ll` nor `ur` may be provided.
#'
#' @param debug A flag that turns on debugging.  Set to 1 to get a moderate
#' amount of debugging information, or a higher value for more debugging.
#'
#' @return A [landsat-class] object, with data having
#' been trimmed as specified.
#'
#' @seealso
#' The documentation for the [landsat-class] class explains the
#' structure of landsat objects, and also outlines the other functions dealing
#' with them.
#'
#' @author Dan Kelley and Clark Richards
#'
#' @concept satellite
#'
#' @family things related to landsat data
landsatTrim <- function(x, ll, ur, box, debug=getOption("oceDebug"))
{
    if (!inherits(x, "landsat"))
        stop("method is only for landsat objects")
    if (missing(ll) != missing(ur))
        stop("must provide both ll and ur, or neither")
    if (!missing(ll) && !missing(box))
        stop("cannot provide both box and (ll, ur)")
    if (missing(box)) {
        if (is.null(names(ll)))
            ll <- list(longitude=ll[1], latitude=ll[2])
        if (is.null(names(ur)))
            ur <- list(longitude=ur[1], latitude=ur[2])
    } else {
        ll <- list(longitude=box$x[1], latitude=box$y[1])
        ur <- list(longitude=box$x[2], latitude=box$y[2])
    }
    oceDebug(debug, "ll:", ll$longitude, "E, ", ll$latitude, "N\n", sep="")
    oceDebug(debug, "ur:", ur$longitude, "E, ", ur$latitude, "N\n", sep="")
    if (2 != sum(c("longitude", "latitude") %in% names(ll)))
        stop("'ll' must have named items 'longitude' and 'latitude'")
    if (2 != sum(c("longitude", "latitude") %in% names(ur)))
        stop("'ur' must have named items 'longitude' and 'latitude'")
    ## Trim to box, either by lon-lat (old way) or UTM (new way)
    ll$longitude <- max(ll$longitude, x@metadata$lllon)
    ur$longitude <- min(ur$longitude, x@metadata$urlon)
    ll$latitude <- max(ll$latitude, x@metadata$lllat)
    ur$latitude <- min(ur$latitude, x@metadata$urlat)
    ##utm <- TRUE                        # FIXME: make this an arg
    if (!("llUTM" %in% names(x@metadata))) {
        oceDebug(debug, "adding llUTM and urUTM to metadata\n")
        x@metadata$llUTM <- lonlat2utm(x@metadata$lllon, x@metadata$lllat, zone=x@metadata$zoneUTM)
        x@metadata$urUTM <- lonlat2utm(x@metadata$urlon, x@metadata$urlat, zone=x@metadata$zoneUTM)
    }
    oceDebug(debug, "metadata$zoneUTM:", x@metadata$zoneUTM, "\n")
    ##if (FALSE) {
    ##    llTrimUTM <- lonlat2utm(ll, zone=x@metadata$llUTM$zone)
    ##    urTrimUTM <- lonlat2utm(ur, zone=x@metadata$llUTM$zone)
    ##} else {
    ##    llTrimUTM <- lonlat2utm(ll, zone=x@metadata$zoneUTM)
    ##    urTrimUTM <- lonlat2utm(ur, zone=x@metadata$zoneUTM)
    ##}
    llTrimUTM <- lonlat2utm(ll, zone=x@metadata$zoneUTM)
    urTrimUTM <- lonlat2utm(ur, zone=x@metadata$zoneUTM)

    oldEastingRange <- c(x@metadata$llUTM$easting, x@metadata$urUTM$easting)
    oldNorthingRange <- c(x@metadata$llUTM$northing, x@metadata$urUTM$northing)
    trimmedEastingRange <- c(llTrimUTM$easting, urTrimUTM$easting)
    trimmedNorthingRange <- c(llTrimUTM$northing, urTrimUTM$northing)
    eStart <- (trimmedEastingRange[1] - oldEastingRange[1])/diff(oldEastingRange)
    eEnd <- (trimmedEastingRange[2] - oldEastingRange[1])/diff(oldEastingRange)
    eStart <- min(max(eStart, 0), 1)
    eEnd <- min(max(eEnd, 0), 1)
    nStart <- (trimmedNorthingRange[1] - oldNorthingRange[1])/diff(oldNorthingRange)
    nEnd <- (trimmedNorthingRange[2] - oldNorthingRange[1])/diff(oldNorthingRange)
    nStart <- min(max(nStart, 0), 1)
    nEnd <- min(max(nEnd, 0), 1)
    oceDebug(debug, "llTrimUTM:", paste(llTrimUTM, collapse=" "), "\n")
    oceDebug(debug, "urTrimUTM:", paste(urTrimUTM, collapse=" "), "\n")
    oceDebug(debug, "oldEastingRange:     ", paste(oldEastingRange, collapse=" "), "\n")
    oceDebug(debug, "oldNorthingRange:    ", paste(oldNorthingRange, collapse=" "), "\n")
    oceDebug(debug, "trimmedEastingRange: ", paste(round(trimmedEastingRange), collapse=" "), "\n")
    oceDebug(debug, "trimmedNorthingRange:", paste(round(trimmedNorthingRange), collapse=" "), "\n")
    oceDebug(debug, "eStart:", eStart, ", eEnd:", eEnd, "before trimming to (0,1)\n")
    oceDebug(debug, "      :", eStart, ", eEnd:", eEnd, "after trimming\n")
    oceDebug(debug, "nStart:", nStart, ", nEnd:", nEnd, "before trimming to (0,1)\n")
    oceDebug(debug, "      :", nStart, ", nEnd:", nEnd, "after trimming\n")
    oceDebug(debug, "Easting  trim range: eStart:", eStart, ", eEnd:", eEnd, "\n")
    oceDebug(debug, "Northing trim range: nStart:", nStart, ", nEnd:", nEnd, "\n")
    #if (eStart < 0 || eStart > 1) stop("internal error trimming (eStart)")
    #if (eEnd < 0 || eEnd > 1) stop("internal error trimming (eEnd)")
    #if (nStart < 0 || nStart > 1) stop("internal error trimming (nStart)")
    #if (nEnd < 0 || nEnd > 1) stop("internal error trimming (nEnd)")


    ## istart <- round((ll$longitude - x@metadata$lllon) / (x@metadata$urlon-x@metadata$lllon) * dim[1])
    ## iend <- round((ur$longitude - x@metadata$lllon) / (x@metadata$urlon-x@metadata$lllon) * dim[1])
    ## istart <- round((ll$longitude - x@metadata$lllon) / (x@metadata$urlon-x@metadata$lllon) * dim[1])
    ## iend <- round((ur$longitude - x@metadata$lllon) / (x@metadata$urlon-x@metadata$lllon) * dim[1])


    ## Convert lat-lon limits to i-j indices
    for (b in seq_along(x@data)) {
        oceDebug(debug, "Trimming band", x@metadata$bands[b], "\n")
        isList <- is.list(x@data[[b]])
        dim <- if (isList) dim(x@data[[b]]$lsb) else dim(x@data[[b]])
        ilim <- round(c(1 + (dim[1]-1) / (x@metadata$urlon-x@metadata$lllon) * (ll$longitude-x@metadata$lllon),
                        1 + (dim[1]-1) / (x@metadata$urlon-x@metadata$lllon) * (ur$longitude-x@metadata$lllon)))
        ilim[1] <- max(1, ilim[1])
        ilim[2] <- min(ilim[2], dim[1])
        oceDebug(debug, "ilim:", ilim[1], "to", ilim[2], "\n")
        ilimUTM <- 1 + round( (dim[1] - 1) * c(eStart, eEnd) )
        ilim <- ilimUTM # FIXME: clean up this code
        oceDebug(debug, "ilimUTM:", ilimUTM[1], "to", ilimUTM[2], "\n")
        jlim <- round(c(1 + (dim[2]-1) / (x@metadata$urlat-x@metadata$lllat) * (ll$latitude-x@metadata$lllat),
                        1 + (dim[2]-1) / (x@metadata$urlat-x@metadata$lllat) * (ur$latitude-x@metadata$lllat)))
        jlim[1] <- max(1, jlim[1])
        jlim[2] <- min(jlim[2], dim[2])
        oceDebug(debug, "jlim:", jlim[1], "to", jlim[2], "\n")
        jlimUTM <- 1 + round( (dim[2] - 1) * c(nStart, nEnd) )
        jlim <- jlimUTM # FIXME: clean up this code
        oceDebug(debug, "jlimUTM:", jlimUTM[1], "to", jlimUTM[2], "\n")
        if (jlim[2] <= jlim[1] || ilim[2] <= ilim[1])
            stop("no intersection between landsat image and trimming box.")
        oceDebug(debug, "  trimming i to range ", ilim[1], ":", ilim[2], ", percent range ",
                 ilim[1]/dim[1], " to ", ilim[2]/dim[1], sep="", "\n")
        oceDebug(debug, "  trimming j to range ", jlim[1], ":", jlim[2], ", percent range ",
                 jlim[1]/dim[2], " to ", jlim[2]/dim[2], sep="", "\n")
        if (isList) {
            if (!is.null(dim(x@data[[b]]$msb)))
                x@data[[b]]$msb <- x@data[[b]]$msb[seq.int(ilim[1], ilim[2]), seq.int(jlim[1], jlim[2])]
            x@data[[b]]$lsb <- x@data[[b]]$lsb[seq.int(ilim[1], ilim[2]), seq.int(jlim[1], jlim[2])]
        } else {
            x@data[[b]] <- x@data[[b]][seq.int(ilim[1], ilim[2]), seq.int(jlim[1], jlim[2])]
        }
    }
    ## FIXME: there is diminishing need for the ll and ur numbers in lon-lat space
    x@metadata$lllon <- ll$longitude
    x@metadata$ullon <- ll$longitude
    x@metadata$lrlon <- ur$longitude
    x@metadata$urlon <- ur$longitude
    x@metadata$lllat <- ll$latitude
    x@metadata$lrlat <- ll$latitude
    x@metadata$urlat <- ur$latitude
    x@metadata$ullat <- ur$latitude
    oceDebug(debug, "OLD:",
            "lllon=", x@metadata$lllon,
            "lrlon=", x@metadata$lrlon,
            "ullon=", x@metadata$ullon,
            "urlon=", x@metadata$urlon, "\n        ",
            "lllat=", x@metadata$lllat,
            "lrlat=", x@metadata$lrlat,
            "ullat=", x@metadata$ullat,
            "urlat=", x@metadata$urlat, "\n")

    ##> ## a regression saves writing messy formulae that will be hard to debug
    ##> xx <- c(1, dim[1])
    ##> XX <- c(x@metadata$lllon, x@metadata$urlon)
    ##> mx <- lm(XX ~ xx)
    ##> ppx <- predict(mx, new=data.frame(xx=ilim))
    ##> newlllon <- newullon <- ppx[1]
    ##> newurlon <- newlrlon <- ppx[2]
    ##> yy <- c(1, dim[2])
    ##> YY <- c(x@metadata$lllat, x@metadata$urlat)
    ##> my <- lm(YY ~ yy)
    ##> ppy <- predict(my, new=data.frame(yy=jlim))
    ##> newlllat <- newullat <- ppy[1]
    ##> newurlat <- newlrlat <- ppy[2]
    ##> x@metadata$lllon <- x@metadata$ullon <- ppx[1]
    ##> x@metadata$lrlon <- x@metadata$urlon <- ppx[2]
    ##> x@metadata$lllat <- x@metadata$lrlat <- ppy[1]
    ##> x@metadata$ullat <- x@metadata$urlat <- ppy[2]

    x@metadata$llUTM <- llTrimUTM
    x@metadata$urUTM <- urTrimUTM
    llE <- llTrimUTM$easting
    llN <- llTrimUTM$northing
    urE <- urTrimUTM$easting
    urN <- urTrimUTM$northing
    zone <- llTrimUTM$zone
    ## hemisphere <- llTrimUTM$hemisphere # this fails in S hemisphere.
    hemisphere <- "N"
    ## Go around the rectangle (in UTM space) to calculate the polygon (in lon-lat space)
    t <- utm2lonlat(easting=llE, northing=llN, zone=zone, hemisphere=hemisphere)
    x@metadata$lllon <- t$longitude
    x@metadata$lllat <- t$latitude
    t <- utm2lonlat(easting=llE, northing=urN, zone=zone, hemisphere=hemisphere)
    x@metadata$ullon <- t$longitude
    x@metadata$ullat <- t$latitude
    t <- utm2lonlat(easting=urE, northing=llN, zone=zone, hemisphere=hemisphere)
    x@metadata$lrlon <- t$longitude
    x@metadata$lrlat <- t$latitude
    t <- utm2lonlat(easting=urE, northing=urN, zone=zone, hemisphere=hemisphere)
    x@metadata$urlon <- t$longitude
    x@metadata$urlat <- t$latitude


    oceDebug(debug, "NEW:",
            "lllon=", x@metadata$lllon,
            "lrlon=", x@metadata$lrlon,
            "ullon=", x@metadata$ullon,
            "urlon=", x@metadata$urlon, "\n        ",
            "lllat=", x@metadata$lllat,
            "lrlat=", x@metadata$lrlat,
            "ullat=", x@metadata$ullat,
            "urlat=", x@metadata$urlat, "\n")
    ##? x@metadata$llUTM <- llTrimUTM
    ##? x@metadata$urUTM <- urTrimUTM
    ##? llE <- llTrimUTM$easting
    ##? llN <- llTrimUTM$northing
    ##? urE <- urTrimUTM$easting
    ##? urN <- urTrimUTM$northing
    ##? zone <- llTrimUTM$zone
    ##? hemisphere <- llTrimUTM$hemisphere
    ## Go around the rectangle (in UTM space) to calculate the polygon (in lon-lat space)
    ##? oceDebug(debug, "llE: ", llE, "llN:", llN, "\n")
    ##? oceDebug(debug, "urE: ", urE, "urN:", urN, "\n")
    ##? t <- utm2lonlat(easting=llE, northing=llN, zone=zone, hemisphere=hemisphere)
    ##? x@metadata$lllon <- t$longitude
    ##? x@metadata$lllat <- t$latitude
    ##? t <- utm2lonlat(easting=llE, northing=urN, zone=zone, hemisphere=hemisphere)
    ##? x@metadata$ullon <- t$longitude
    ##? x@metadata$ullat <- t$latitude
    ##? t <- utm2lonlat(easting=urE, northing=llN, zone=zone, hemisphere=hemisphere)
    ##? x@metadata$lrlon <- t$longitude
    ##? x@metadata$lrlat <- t$latitude
    ##? t <- utm2lonlat(easting=urE, northing=urN, zone=zone, hemisphere=hemisphere)
    ##? x@metadata$urlon <- t$longitude
    ##? x@metadata$urlat <- t$latitude

    x@processingLog <- processingLogAppend(x@processingLog,
                                           sprintf("landsatTrim(x, ll=list(longitude=%f, latitude=%f), ur=list(longitude=%f, latitude=%f))",
                                                   ll$longitude, ll$latitude, ur$longitude, ur$latitude))
    x
}
