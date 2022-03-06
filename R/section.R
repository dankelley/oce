# vim:textwidth=80:expandtab:shiftwidth=4:softtabstop=4:foldmethod=marker

#' Class to Store Hydrographic Section Data
#'
#' This class stores data from oceanographic section surveys.
#'
#' Sections can be read with [read.section()] or created with
#' [read.section()] or created from CTD objects by using
#' [as.section()] or by adding a ctd station to an existing section with
#' [sectionAddStation()].
#'
#' Sections may be sorted with [sectionSort()], subsetted with
#' [subset,section-method()], smoothed with [sectionSmooth()], and
#' gridded with [sectionGrid()].  A "spine" may be added to a section
#' with [addSpine()].  Sections may be summarized with
#' [summary,section-method()] and plotted
#' with [plot,section-method()].
#'
#' The sample dataset [section()] contains data along WOCE line A03.
#'
#' @templateVar class section
#'
#' @templateVar dataExample {}
#'
#' @templateVar metadataExample Examples that are of common interest include `stationId`, `longitude`, `latitude` and `time`.
#'
#' @template slot_summary
#'
#' @template slot_put
#'
#' @template slot_get
#'
#' @examples
#' library(oce)
#' data(section)
#' plot(section[['station', 1]])
#' pairs(cbind(z=-section[["pressure"]],T=section[["temperature"]],S=section[["salinity"]]))
#' ## T profiles for first few stations in section, at common scale
#' par(mfrow=c(3,3))
#' Tlim <- range(section[["temperature"]])
#' ylim <- rev(range(section[["pressure"]]))
#' for (stn in section[["station", 1:9]])
#'     plotProfile(stn, xtype='potential temperature', ylim=ylim, Tlim=Tlim)
#'
#' @author Dan Kelley
#'
#' @family classes provided by oce
#' @family things related to section data
setClass("section", contains="oce")


#' Hydrographic section
#'
#' This is line A03 (ExpoCode 90CT40_1, with nominal sampling date 1993-09-11).
#' The chief scientist was Tereschenkov of SOI, working aboard the Russian ship
#' Multanovsky, undertaking a westward transect from the Mediterranean outflow
#' region across to North America, with a change of heading in the last few dozen
#' stations to run across the nominal Gulf Stream axis.
#' The data flags follow the "WHP Bottle"convention, set by
#' [initializeFlagScheme,section-method()] to `"WHP bottle"`.  This convention
#' used to be described at the link
#' `https://www.nodc.noaa.gov/woce/woce_v3/wocedata_1/whp/exchange/exchange_format_desc.htm`
#' but that was found to fail in December 2020.
#'
#' @examples
#'\dontrun{
#' library(oce)
#' # Gulf Stream
#' data(section)
#' GS <- subset(section, 109<=stationId&stationId<=129)
#' GSg <- sectionGrid(GS, p=seq(0, 5000, 100))
#' plot(GSg, map.xlim=c(-80,-60))
#'}
#'
#' @name section
#'
#' @docType data
#'
#' @usage data(section)
#'
#' @source This is based on the WOCE file named `a03_hy1.csv`, downloaded
#' from \url{https://cchdo.ucsd.edu/cruise/90CT40_1}, 13 April 2015.
#'
#' @family datasets provided with oce
#' @family things related to section data
NULL

setMethod(f="initialize",
          signature="section",
          definition=function(.Object, filename="", sectionId="", ...) {
              .Object <- callNextMethod(.Object, ...)
              .Object@metadata$units <- NULL # senseless keeping these from oce()
              .Object@metadata$flags <- NULL # senseless keeping these from oce()
              .Object@metadata$filename <- filename
              .Object@metadata$sectionId <- sectionId
              .Object@processingLog$time <- presentTime()
              .Object@processingLog$value <- "create 'section' object"
              return(.Object)
          })

## DEVELOPERS: please pattern functions and documentation on this, for uniformity.
## DEVELOPERS: You will need to change the docs, and the 3 spots in the code
## DEVELOPERS: marked '# DEVELOPER 1:', etc.
#' @title Handle flags in Section Objects
#'
#' @details
#' The default for `flags` is based on
#' calling [defaultFlags()] based on the
#' `metadata` in the first station in the section. If the
#' other stations have different flag schemes (which seems highly
#' unlikely for archived data), this will not work well, and indeed
#' the only way to proceed would be to use [handleFlags,ctd-method()]
#' on the stations, individually.
#'
#' @param object A [section-class] object.
#'
#' @template handleFlagsTemplate
#'
#' @references
#' The following link used to work, but was found to fail in December 2020.
#' 1. `https://www.nodc.noaa.gov/woce/woce_v3/wocedata_1/whp/exchange/exchange_format_desc.htm`
#'
#' @examples
#' library(oce)
#' data(section)
#' section2 <- handleFlags(section, flags=c(1,3:9))
#' par(mfrow=c(2, 1))
#' plotTS(section)
#' plotTS(section2)
#'
#' @family things related to section data
#' @aliases handleFlags.section
setMethod("handleFlags", signature=c(object="section", flags="ANY", actions="ANY", where="ANY", debug="ANY"),
    definition=function(object, flags=NULL, actions=NULL, where=where, debug=getOption("oceDebug")) {
        ## DEVELOPER 1: alter the next comment to explain your setup
        if (is.null(flags)) {
            flags <- defaultFlags(object[["station", 1]])
            if (is.null(flags))
                stop("must supply 'flags', or use initializeFlagScheme() on the section object first")
        }
        if (is.null(actions)) {
            actions <- list("NA") # DEVELOPER 2: alter this line to suit a new data class
            names(actions) <- names(flags)
        }
        if (any(sort(names(actions)) != sort(names(flags))))
            stop("names of flags and actions must match")
        res <- object
        for (i in seq_along(res@data$station)) {
            res@data$station[[i]] <- handleFlags(res@data$station[[i]], flags, actions, where=where, debug)
        }
        res
    })

#' @templateVar class section
#' @templateVar details This applies `initializeFlagScheme` for each `ctd` station within the `stations` element of the `data` slot.
#' @template initializeFlagSchemeTemplate
#'
#' @examples
#'\dontrun{
#' data(section)
#' section <- read.section("a03_hy1.csv", sectionId="a03", institute="SIO",
#'                         ship="R/V Professor Multanovskiy", scientist="Vladimir Tereschenkov")
#' sectionWithFlags <- initializeFlagScheme(section, "WHP bottle")
#' station1 <- sectionWithFlags[["station", 1]]
#' str(station1[["flagScheme"]])
#'}
setMethod("initializeFlagScheme",
          c(object="section", name="ANY", mapping="ANY", default="ANY", update="ANY", debug="ANY"),
          function(object, name=NULL, mapping=NULL, default=NULL, update=NULL, debug=getOption("oceDebug")) {
              res <- object
              for (i in seq_along(object@data$station)) {
                  res@data$station[[i]] <- initializeFlagScheme(object@data$station[[i]],
                                                                name=name,
                                                                mapping=mapping,
                                                                default=default,
                                                                update=update,
                                                                debug=debug-1)
              }
              res@processingLog <-
                  processingLogAppend(res@processingLog,
                                      paste("initializeFlagScheme(object",
                                            ", name=\"", name, "\"",
                                            ", mapping=", gsub("[ ]*", "", paste(as.character(deparse(mapping)))), ")",
                                            ", default=", gsub("[ ]*", "", paste(as.character(deparse(default)))), ")",
                                            sep=""))
              res
          })

#' Summarize a Section Object
#'
#' Pertinent summary information is presented, including station locations,
#' distance along track, etc.
#'
#' @param object An object of class `"section"`, usually, a result of a call
#' to [read.section()], [read.oce()], or
#' [as.section()].
#'
#' @param ... Further arguments passed to or from other methods.
#'
#' @return `NULL`
#'
#' @examples
#' library(oce)
#' data(section)
#' summary(section)
#'
#' @family things related to section data
#'
#' @author Dan Kelley
#' @aliases summary.section
setMethod(f="summary",
          signature="section",
          definition=function(object, ...) {
              numStations <- length(object@data$station)
              cat("Section Summary\n---------------\n\n")
              cat("* Source: \"", object@metadata$filename, "\"\n", sep="")
              cat("* ID:     \"", object@metadata$sectionId, "\"\n", sep="")
              ##stn.sum <- matrix(nrow=numStations, ncol=5)
              if (numStations > 0) {
                  cat("* Overview of stations\n")
                  cat(sprintf("%5s %5s %8s %8s %7s %5s %16s\n", "Index", "ID", "Lon", "Lat", "Levels", "Depth", "StartTime"))
                  for (i in seq_len(numStations)) {
                      ##stn <- object@data$station[[i]]
                      thisStn <- object@data$station[[i]]
                      id <- if (!is.null(thisStn@metadata$station) && "" != thisStn@metadata$station)
                          thisStn@metadata$station else ""
                      depth <- if ("waterDepth" %in% names(thisStn@metadata)) thisStn@metadata$waterDepth else NA
                      cat(sprintf("%5d %5s %8.4f %8.4f %7.0f %5.0f %16s\n",
                                  i, id,
                                  thisStn[["longitude"]][1], thisStn[["latitude"]][1],
                                  length(thisStn@data$pressure), depth,
                                  format(thisStn[["startTime"]][1], "%Y-%m-%d %H:%M")))
                  }
                  names <- names(object@data$station[[1]]@metadata$flags)
                  if (!is.null(names)) {
                      cat("* Data-quality flags\n")
                      width <- 1 + max(nchar(names))
                      ## assume all stations have identical flags
                      for (name in names) {
                          padding <- rep(" ", width - nchar(name))
                          cat("    ", name, ":", padding, sep="")
                          flags <- NULL
                          flagName <- paste0(name, "Flag")
                          for (i in seq_len(numStations)) {
                              flags <- c(flags, as.numeric(object@data$station[[i]][[flagName]]))
                          }
                          flagTable <- table(flags)
                          flagTableLength <- length(flagTable)
                          if (flagTableLength) {
                              for (i in seq_len(flagTableLength)) {
                                  cat("\"", names(flagTable)[i], "\"", " ", flagTable[i], "", sep="")
                                  if (i != flagTableLength) cat(", ") else cat("\n")
                              }
                          }
                      }
                  }
              } else {
                  cat("* No stations\n")
              }
              if ("spine" %in% names(object@metadata)) {
                  if (2 == length(object@metadata$spine) &&
                      2 == sum(c("latitude", "longitude") %in% names(object@metadata$spine))) {
                      spine <- object@metadata$spine
                      cat("* Section spine\n")
                      cat(sprintf("    %8s %8s\n", "Lon", "Lat"))
                      for (i in seq_along(spine$longitude)) {
                          cat(sprintf("    %8.4f %8.4f\n", spine$longitude[i], spine$latitude[i]))
                      }
                  } else {
                      warning("malformed section spine")
                  }
              }
              processingLogShow(object)
              invisible(NULL)
          })


#' Extract Something From a Section Object
#'
#' @param x a [section-class] object.
#'
#' @templateVar class section
#'
#' @section Details of the Specialized Method:
#'
#' There are several possibilities, depending on the nature of `i`.
#'
#' * If `i` is `"?"`, then the return value is a list containing four items,
#' each of which is a character vector holding the names of things that can be
#' accessed with `[[`. This list is compiled by examining all the stations in
#' the object, and reporting an entry if it is found in any one of them. The
#' `data` and `metadata` items hold the names of entries in the object's data
#' and metadata slots, respectively. The `dataDerived` and `metadataDerived`
#' items hold data-like and metadata-like things that can be derived from these.
#'
#' * If `i` is `"station"`, then `[[` will return a [list()] of [ctd-class]
#' objects holding the station data. If `j` is also given, it specifies a
#' station (or set of stations) to be returned. if `j` contains just a single
#' value, then that station is returned, but otherwise a list is returned. If
#' `j` is an integer, then the stations are specified by index, but if it is
#' character, then stations are specified by the names stored within their
#' metadata. (Missing stations yield `NULL` in the return value.)
#'
#' * If `i` is `"station ID"`, then the IDs of the stations in the
#' section are returned.
#'
#' * If `i` is `"dynamic height"`, then an estimate of dynamic
#' height is returned, as calculated with [`swDynamicHeight`]`(x)`.
#'
#' * If `i` is `"distance"`, then the distance along the section is
#' returned, using [geodDist()].
#'
#' * If `i` is `"depth"`, then a vector containing the depths
#' of the stations is returned.
#'
#' * If `i` is `"z"`, then a vector containing the z
#' coordinates is returned.
#'
#' * If `i` is `"theta"` or `"potential temperature"`, then
#' the potential temperatures of all the stations are returned in one
#' vector.  Similarly, `"spice"` returns the property known
#' as spice, using [swSpice()].
#'
#' * If `i` is a string ending with `"Flag"`, then the characters
#' prior to that ending are taken to be the name of a variable contained
#' within the stations in the section. If this flag is available in
#' the first station of the section, then the flag values are looked
#' up for every station.
#'
#' If `j` is `"byStation"`, then a list is returned, with
#' one (unnamed) item per station.
## #' If `j` is `"grid:distance-pressure"`, then a gridded
## #' representation of `i` is returned, as a list with elements
## #' `distance` (in km), `pressure` (in dbar) and
## #' `field` (in whatever unit is used for `i`). See Example
## #' for in the documentation for [plot,section-method()].
#'
#' @examples
#' data(section)
#' length(section[["latitude"]])
#' length(section[["latitude", "byStation"]])
#' # Vector of all salinities, for all stations
#' Sv <- section[["salinity"]]
#' # List of salinities, grouped by station
#' Sl <- section[["salinity", "byStation"]]
#' # First station salinities
#' Sl[[1]]
#'
#' @template sub_subTemplate
#'
#' @author Dan Kelley
#'
#' @family things related to section data
setMethod(f="[[",
          signature(x="section", i="ANY", j="ANY"),
          definition=function(x, i, j, ...) {
              # This is broken down into 3 cases.
              #
              # Case 1. Things that can be computed without looking deeply
              # enough within @data$station to determine computable things.
              # This determination is a bit slow, and is taken up in SECTION 2.
              #
              # Case 1.1 "station".
              if (i == "station") {
                  # All stations.
                  if (missing(j))
                      return(x@data$station)
                  # A subset of stations, specified with j, which is either a
                  # character value for station ID(s) or a numeric value for
                  # sequence number(s).
                  nj <- length(j)
                  if (is.character(j)) { # station ID(s)
                      stationNames <- unlist(lapply(x@data$station,
                              function(station)
                                  station@metadata$station))
                      if (nj == 1L) {
                          w <- which(stationNames == j)
                          res <- if (length(w)) x@data$station[[w[1]]] else NULL
                      } else {
                          res <- vector("list", nj)
                          for (jj in seq_len(nj)) {
                              w <- which(stationNames == j[jj])
                              res[[jj]] <- if (length(w)) x@data$station[[w[1]]] else NULL
                          }
                      }
                  } else {             # sequence number(s)
                      if (nj == 1L) {
                          res <- x@data$station[[j]]
                      } else {
                          res <- vector("list", nj)
                          for (jj in seq_len(nj)) {
                              res[[jj]] <- x@data$station[[j[jj]]]
                          }
                      }
                  }
                  return(res)
              }
              # Case 1.2: data-quality flags.
              if (1 == length(grep(".*Flag$", i))) {
                  baseName <- gsub("Flag$", "", i)
                  # FIXME: should check all stations, not just the first
                  if (baseName %in% names(x@data$station[[1]]@metadata$flags)) {
                      return(unlist(lapply(x@data$station, function(ctd) ctd[[i]])))
                  } else {
                      stop("First station lacks a '", baseName, "' flag, so assuming the same for all.")
                  }
              }
              # Case 1.3: things stored in section@metadata.
              if (i %in% names(x@metadata)) {
                  if (i %in% c("longitude", "latitude")) {
                      if (!missing(j) && j == "byStation") {
                          return(x@metadata[[i]])
                      } else {
                          res <- NULL
                          for (stn in seq_along(x@data$station))
                              res <- c(res, rep(x@data$station[[stn]]@metadata[[i]], length(x@data$station[[stn]][["salinity"]])))
                          return(res)
                      }
                  } else {
                      return(x@metadata[[i]])
                  }
              }
              # Case 1.4: station IDs.
              if (i == "station ID")
                  return(unlist(lapply(x@data$station, function(stn) stn[["station"]])))
              # Case 1.5: dynamic height.
              if (i == "dynamic height")
                  return(swDynamicHeight(x))
              # Case 1.6: distance along track.
              if (i == "distance") {
                  res <- NULL
                  for (stn in seq_along(x@data$station)) {
                      distance <- geodDist(x@data$station[[stn]][["longitude"]][1],
                                           x@data$station[[stn]][["latitude"]][1],
                                           x@data$station[[1]][["longitude"]][1],
                                           x@data$station[[1]][["latitude"]][1])
                      if (!missing(j) && j == "byStation")
                          res <- c(res, distance)
                      else
                          res <- c(res, rep(distance, length(x@data$station[[stn]]@data$temperature)))

                  }
                  return(res)
              }
              # Case 1.7: time.
              if (i == "time")
                  return(numberAsPOSIXct(unlist(lapply(x@data$station, function(stn) stn[["time"]]))))

              # Case 2. We are now done with things that can be determined by
              # looking one level deep.  To extract individual data, we will
              # need to know what is in each of the stations (and what can be
              # computed from this content).
              metadataStn <- dataStn <- metadataDerivedStn <- dataDerivedStn <- NULL
              for (station in x@data$station) {
                  q <- station[["?"]]
                  metadataStn <- c(metadataStn, q$metadata)
                  metadataDerivedStn <- c(metadataDerivedStn, q$metadataDerived)
                  dataStn <- c(dataStn, q$data)
                  dataDerivedStn <- c(dataDerivedStn, q$dataDerived)
              }
              metadataStn <- sort(unique(metadataStn))
              metadataDerivedStn <- sort(c(
                      "distance",
                      paste("station", "ID"),
                      paste("dynamic", "height"),
                      unique(metadataDerivedStn)))
              dataStn <- sort(unique(dataStn))
              dataDerivedStn <- sort(unique(dataDerivedStn))
              # Case 2.1: indication of available values for i.
              if (i == "?") {
                  return(list(metadata=metadataStn,
                          metadataDerived=metadataDerivedStn,
                          data=dataStn,
                          dataDerived=dataDerivedStn))
              }
              # Case 2.2: something inside stations (or computable from such).
              res <- NULL
              nstation <- length(x@data$station)
              if (i %in% c(dataStn, dataDerivedStn)) {
                  if (!missing(j) && substr(j, 1, 4) == "grid") {
                      if (j == "grid:distance-pressure") {
                          numStations <- length(x@data$station)
                          p1 <- x[["station", 1]][["pressure"]]
                          np1 <- length(p1)
                          field <- matrix(NA, nrow=numStations, ncol=np1)
                          if (numStations > 1) {
                              field[1, ] <- x[["station", 1]][[i]]
                              for (istn in 2:numStations) {
                                  pi <- x[["station", istn]][["pressure"]]
                                  if (length(pi) != np1 || any(pi != p1)) {
                                      warning("returning NULL because this section is not gridded")
                                      return(NULL)
                                  }
                                  field[istn, ] <- x[["station", istn]][[i]]
                              }
                              res <- list(distance=x[["distance", "byStation"]], pressure=p1, field=field)
                              return(res)
                          } else {
                              warning("returning NULL because this section contains only 1 station")
                              return(NULL)
                          }
                      } else {
                          warning("returning NULL because only grid:distance-pressure is permitted")
                          return(NULL)
                      }
                  } else {
                      # Note that nitrite and nitrate might be computed, not stored
                      if (!missing(j) && j == "byStation") {
                          res <- vector("list", nstation)
                          for (istation in seq_len(nstation))
                              res[[istation]] <- x@data$station[[istation]][[i]]
                      } else {
                          res <- NULL
                          for (station in x[["station"]])
                              res <- c(res, station[[i]])
                      }
                      return(res)
                  }
              }
              # Case 3: unknown, so drop down a level.
              callNextMethod()
          })                           # [[

#' Replace Parts of a Section Object
#'
#' @param x a [section-class] object.
#'
#' @template sub_subsetTemplate
#'
#' @examples
#' # 1. Change section ID from a03 to A03
#' data(section)
#' section[["sectionId"]]
#' section[["sectionId"]] <- toupper(section[["sectionId"]])
#' section[["sectionId"]]
#' # 2. Add a millidegree to temperatures at station 10
#' section[["station", 10]][["temperature"]] <-
#'     1e-3 + section[["station", 10]][["temperature"]]
#'
#' @author Dan Kelley
#'
#' @family things related to section data
setMethod(f="[[<-",
          signature(x="section", i="ANY", j="ANY"),
          definition=function(x, i, j, ..., value) {
              if (i == "station") {
                  if (missing(j)) {
                      x@data$station <- value
                  } else {
                      x@data$station[[j]] <- value
                  }
                  x
              } else {
                  callNextMethod(x=x, i=i, j=j, ...=..., value=value) # [[<-
              }
          })


setMethod(f="show",
          signature="section",
          definition=function(object) {
              id <- object@metadata$sectionId
              n <- length(object@data$station)
              if (n == 0) {
                  cat("Section has no stations\n")
              } else {
                  if (is.null(id) || id == "")
                      cat("Unnamed section has ", n, " stations:\n", sep="")
                  else
                      cat("Section '", id, "' has ", n, " stations:\n", sep="")
                  cat(sprintf("%5s %5s %8s %8s %7s %5s\n", "Index", "ID", "Lon", "Lat", "Levels", "Depth"))
                  ##cat(sprintf("%4s %5s %10.2f %10.2f %10.0f\n", "Index", "ID", "Lon", "Lat", "Depth\n"))
                  for (i in 1:n) {
                      thisStn <- object@data$station[[i]]
                      id <- if (!is.null(thisStn@metadata$station) && "" != thisStn@metadata$station)
                          thisStn@metadata$station else ""
                      depth <- if (is.null(thisStn@metadata$waterDepth))
                          max(thisStn@data$pressure, na.rm=TRUE) else thisStn@metadata$waterDepth
                      cat(sprintf("%5d %5s %8.3f %8.3f %7.0f %5.0f\n",
                                  i, id,
                                  thisStn[["longitude"]][1], thisStn[["latitude"]][1],
                                  length(thisStn@data$pressure), depth))
                  }
              }
          })

#' Subset a Section Object
#'
#' Return a subset of a section object.
#'
#' This function is used to subset data within the
#' stations of a section, or to choose a subset of the stations
#' themselves. The first case is handled with the `subset` argument,
#' while the second is handled if `...` contains a vector named
#' `indices`. Either `subset` or `indices` must be
#' provided, but not both.
#'
#' **In the "subset" method**, `subset` indicates
#' either stations to be kept, or data to be kept within the stations.
#'
#' The first step in processing is to check for the presence of certain
#' key words in the `subset` expression. If `distance`
#' is present, then stations are selected according to a condition on the
#' distance (in km) from the first station to the given station (Example 1).
#' If either `longitude` or `latitude` is given, then
#' stations are selected according to the stated condition (Example 2).
#' If `stationId` is present, then selection is in terms of the
#' station ID (not the sequence number) is used (Example 3). In all of these
#' cases, stations are either selected in their entirety or dropped
#' in their entirety.
#'
#' If none of these keywords is present, then the `subset` expression is
#' evaluated in the context of the `data` slot of each of the CTD stations
#' stored within `x`. (Note that this slot does not normally
#' contain any of the keywords that are listed in the previous
#' paragraph; it does, then odd results may occur.) Each station is examined
#' in turn, with `subset` being evaluated individually in each. The evaluation
#' produces a logical vector. If that vector has length 1 (Examples 4 and 5)
#' then the station is retained or discarded, accordingly. If the vector is longer,
#' then the logical vector is used as a sieve to subsample that individual CTD
#' profile.
#'
#' **In the "indices" method**, stations are selected using
#' `indices`, which may be a vector of integers that indicate sequence
#' number, or a logical vector, again indicating which stations to keep.
#'
#' @param x a [section-class] object.
#'
#' @param subset an optional indication of either the stations to be kept,
#' or the data to be kept within the stations.  See \dQuote{Details}.
#'
#' @param ... optional arguments, of which only the first is examined. The
#' possibilities for this argument are `indices`, which must be a
#' vector of station indices (see Example 6), or `within`, which must be
#' a list or data frame, containing items named either `x` and `y`
#' or `longitude` and `latitude` (see Example 7). If `within`
#' is given, then `subset` is ignored.
#'
#' @return A [section-class] object.
#'
#' @examples
#' library(oce)
#' data(section)
#'
#' # Example 1. Stations within 500 km of the first station
#' starting <- subset(section, distance < 500)
#'
#' # Example 2. Stations east of 50W
#' east <- subset(section, longitude > (-50))
#'
#' # Example 3. Gulf Stream
#' GS <- subset(section, 109 <= stationId & stationId <= 129)
#'
#' # Example 4. Only stations with more than 5 pressure levels
#' long <- subset(section, length(pressure) > 5)
#'
#' # Example 5. Only stations that have some data in top 50 dbar
#' surfacing <- subset(section, min(pressure) < 50)
#'
#' # Example 6. Similar to #4, but done in more detailed way
#' long <- subset(section,
#'    indices=unlist(lapply(section[["station"]],
#'                   function(s)
#'                     5 < length(s[["pressure"]]))))
#'
#' # Example 7. Subset by a polygon determined with locator()
#'\dontrun{
#' par(mfrow=c(2, 1))
#' plot(section, which="map")
#' bdy <- locator(4) # choose a polygon near N. America
#' GS <- subset(section, within=bdy)
#' plot(GS, which="map")
#'}
#'
#' @author Dan Kelley
#'
#' @family functions that subset oce objects
#' @family things related to section data
#' @aliases subset.section
setMethod(f="subset",
          signature="section",
          definition=function(x, subset, ...) {
              subsetString <- paste(deparse(substitute(expr=subset, env=environment())), collapse=" ")
              res <- x
              dots <- list(...)
              dotsNames <- names(dots)
              indicesGiven <- length(dots) && ("indices" %in% dotsNames)
              withinGiven <- length(dots) && ("within" %in% dotsNames)
              debug <- getOption("oceDebug")
              if (length(dots) && ("debug" %in% names(dots)))
                  debug <- dots$debug
              if (indicesGiven) {
                  ## select a portion of the stations
                  if (!missing(subset))
                      stop("cannot specify both 'subset' and 'indices'")
                  oceDebug(debug, "subsetting by indices\n")
                  res <- new("section")
                  indices <- dots$indices
                  n <- length(indices)
                  if (is.logical(indices))
                      indices <- (1:n)[indices]
                  if (min(indices) < 1) stop("cannot have negative indices")
                  if (max(indices) > length(x@data$station)) stop("cannot indices exceeding # stations")
                  stn <- x@metadata$stationId[indices]
                  lat <- x@metadata$lat[indices]
                  lon <- x@metadata$lon[indices]
                  time <- x@metadata$time[indices]
                  station <- vector("list", length(indices))
                  for (i in seq_along(indices)) {
                      station[[i]] <- x@data$station[[indices[i]]]
                  }
                  data <- list(station=station)
                  res@metadata$stationId <- stn
                  res@metadata$longitude <- lon
                  res@metadata$latitude <- lat
                  res@metadata$time <- time
                  res@data <- data
                  res@processingLog <- x@processingLog
                  res@processingLog <- processingLogAppend(res@processingLog, paste("subset(x, indices=c(", paste(dots$indices, collapse=","), "))", sep=""))
              } else if (withinGiven) {
                  polygon <- dots$within
                  if (!is.data.frame(polygon) && !is.list(polygon))
                      stop("'within' must be a data frame or a polygon")
                  polygonNames <- names(polygon)
                  ## {{{ OLD 'sp::point.in.polygon' method
                  lonp <- if ("x" %in% polygonNames) {
                      polygon$x
                  } else if ("longitude" %in% polygonNames) {
                      polygon$longitude
                  } else {
                      stop("'within' must contain either 'x' or 'longitude'")
                  }
                  latp <- if ("y" %in% polygonNames) {
                      polygon$y
                  } else if ("latitude" %in% polygonNames) {
                      polygon$latitude
                  } else {
                      stop("'within' must contain either 'y' or 'latitude'")
                  }
                  lon <- x[["longitude", "byStation"]]
                  lat <- x[["latitude", "byStation"]]
                  if (requireNamespace("sp", quietly=TRUE)) {
                      keep <- 1==sp::point.in.polygon(lon, lat, lonp, latp)
                  } else {
                      stop("subset,section-method cannot use 'within' because the 'sp' package is not installed")
                  }
                  ## }}}
                  ## {{{ NEW 'sf' method
                  polyNew <- sf::st_polygon(list(outer=cbind(c(lonp, lonp[1]), c(latp, latp[1]))))
                  pointsNew <- sf::st_multipoint(cbind(lon, lat))
                  inside <- sf::st_intersection(pointsNew, polyNew)
                  keepNew <- matrix(pointsNew %in% inside, ncol=2)[,1]
                  if (!all.equal(keepNew, keep)) {
                      warning("subset,section-method error: 'keep' disagreement with trial 'sf' method. Please post an issue on www.github.com/dankelley/oce/issues\n")
                  }
                  ## }}}
                  res <- x
                  res@metadata$stationId <- x@metadata$stationId[keep]
                  res@metadata$longitude <- x@metadata$longitude[keep]
                  res@metadata$latitude <- x@metadata$latitude[keep]
                  res@metadata$time <- x@metadata$time[keep]
                  res@data$station <- x@data$station[keep]
                  res@processingLog <- processingLogAppend(res@processingLog,
                                                           paste("subset(x, within) kept ", sum(keep), " of ",
                                                                 length(keep), " stations", sep=""))
              } else {
                  if (missing(subset))
                      stop("must give 'subset' or (in ...) 'indices'")
                  oceDebug(debug, "subsetting by 'subset'\n")
                  ##subsetString <- deparse(substitute(subset))
                  ##oceDebug(debug, "subsetString='", subsetString, "'\n")
                  res <- x
                  if (grepl("stationId", subsetString)) {
                      keep <- eval(expr=substitute(expr=subset, env=environment()),
                                   envir=data.frame(stationId=as.numeric(x@metadata$stationId)))
                      res@metadata$stationId <- x@metadata$stationId[keep]
                      res@metadata$longitude <- x@metadata$longitude[keep]
                      res@metadata$latitude <- x@metadata$latitude[keep]
                      res@metadata$time <- x@metadata$time[keep]
                      res@data$station <- x@data$station[keep]
                      res@processingLog <- processingLogAppend(res@processingLog, paste("subset(x, subset=", subsetString, ")", sep=""))
                  } else if (grepl("distance", subsetString)) {
                      l <- list(distance=geodDist(res))
                      keep <- eval(expr=substitute(expr=subset, env=environment()), envir=l, enclos=parent.frame(2))
                      res@metadata$longitude <- res@metadata$longitude[keep]
                      res@metadata$latitude <- res@metadata$latitude[keep]
                      res@metadata$stationId <- res@metadata$stationId[keep]
                      res@metadata$time <- x@metadata$time[keep]
                      res@data$station <- res@data$station[keep]
                  } else if (grepl("levels", subsetString)) {
                      levels <- unlist(lapply(x[["station"]], function(stn) length(stn[["pressure"]])))
                      keep <- eval(expr=substitute(expr=subset, env=environment()), envir=list(levels=levels))
                      res@metadata$longitude <- res@metadata$longitude[keep]
                      res@metadata$latitude <- res@metadata$latitude[keep]
                      res@metadata$stationId <- res@metadata$stationId[keep]
                      res@metadata$time <- x@metadata$time[keep]
                      res@data$station <- res@data$station[keep]
                  } else if (grepl("latitude", subsetString) || grepl("longitude", subsetString)) {
                      n <- length(x@data$station)
                      keep <- vector(length=n)
                      for (i in 1:n)
                          keep[i] <- eval(expr=substitute(expr=subset, env=environment()), envir=x@data$station[[i]]@metadata, enclos=parent.frame(2))
                      nn <- sum(keep)
                      station <- vector("list", nn)
                      stn <- NULL # we can later index this to accumulate
                      lon <- NULL
                      lat <- NULL
                      time <- NULL
                      j <- 1
                      for (i in 1:n) {
                          if (keep[i]) {
                              stn[j] <- x@metadata$stationId[i]
                              lon[j] <- x@metadata$longitude[i]
                              lat[j] <- x@metadata$latitude[i]
                              station[[j]] <- x@data$station[[i]]
                              time[[j]] <- x@data$time[[i]]
                              j <- j + 1
                          }
                      }
                      res <- new("section")
                      res@data$station <- station
                      res@metadata$header <- x@metadata$header
                      res@metadata$sectionId <- x@metadata$sectionId
                      res@metadata$stationId <- stn
                      res@metadata$longitude <- lon
                      res@metadata$latitude <- lat
                      res@metadata$time <- time
                      res@processingLog <- x@processingLog
                  } else {
                      res <- new("section")
                      res@data$station <- list()
                      res@metadata$header <- x@metadata$header
                      res@metadata$sectionId <- NULL # R will let us index into these later
                      res@metadata$stationId <- NULL
                      res@metadata$longitude <- NULL
                      res@metadata$latitude <- NULL
                      res@metadata$time <- NULL
                      res@processingLog <- x@processingLog
                      n <- length(x@data$station)
                      j <- 1
                      for (i in 1:n) {
                          r <- eval(expr=substitute(expr=subset, env=environment()), envir=x@data$station[[i]]@data, enclos=parent.frame(2))
                          oceDebug(debug, "i=", i, ", j=", j, ", sum(r)=", sum(r), "\n", sep="")
                          if (sum(r) > 0) {
                              ## copy whole station  ...
                              res@data$station[[j]] <- x@data$station[[i]]
                              ## ... but if we are looking for a subset, go through the data fields and do that
                              if (length(r) > 1) {
                                  ## Select certain levels. This occurs e.g. for
                                  ##    subset(sec, S > 35)
                                  ## but not for station-by-station selection, e.g. as a result of
                                  ##    subset(sec, length(S) > 3)
                                  ## since the length of the latter is 1, which means to copy
                                  ## the whole station.
                                  for (field in names(res@data$station[[j]]@data)) {
                                      oceDebug(debug, "    field='", field, "', i=", i, ", j=", j, " (case A)\n", sep="")
                                      res@data$station[[j]]@data[[field]] <- res@data$station[[j]]@data[[field]][r]
                                  }
                              }
                              ## copy section metadata
                              res@metadata$stationId[j] <- x@metadata$stationId[i]
                              res@metadata$latitude[j] <- x@metadata$latitude[i]
                              res@metadata$longitude[j] <- x@metadata$longitude[i]
                              res@metadata$time[j] <- x@metadata$time[i]
                              j <- j + 1
                          } else {
                              oceDebug(debug, "    skipping this station\n")
                          }
                      }
                      ## if (j <= n) {
                      ##     for (jj in seq.int(n, j)) {
                      ##         oceDebug(debug, "erase item at j =", jj, "\n")
                      ##         res@data$station[[jj]] <- NULL
                      ##         res@metadata$stationId[jj] <- TRUE
                      ##         res@metadata$latitude[jj] <- TRUE
                      ##         res@metadata$longitude[jj] <- TRUE
                      ##     }
                      ## }
                  }
                  res@processingLog <- processingLogAppend(res@processingLog, paste("subset(x, subset=", subsetString, ")", sep=""))
              }
              res
          })


#' Sort a Section
#'
#' Sections created with [as.section()] have "stations" that are in the
#' order of the CTD objects (or filenames for such objects) provided.  Sometimes,
#' this is not the desired order, e.g. if file names discovered with
#' [dir()] are in an order that makes no sense.  (For example, a
#' practioner might name stations `"stn1"`, `"stn2"`, etc., not
#' realizing that this will yield an unhelpful ordering, by file name, if there
#' are more than 9 stations.) The purpose of `sectionSort` is to permit
#' reordering the constituent stations in sensible ways.
#'
#' @param section A `section` object containing the section whose stations
#' are to be sorted.
#'
#' @param by An optional string indicating how to reorder.  If not provided,
#' `"stationID"` will be assumed.  Other choices are `"distance"`, for
#' distance from the first station, `"longitude"`, for longitude,
#' `"latitude"` for latitude, and `"time"`, for time.
#'
#' @return object A [section-class] object that has been smoothed,
#' so its data fields will station-to-station variation than
#' is the case for the input section, \code{x}.
#'
#' @examples
#'\dontrun{
#' # Eastern North Atlantic, showing Mediterranean water;
#' # sorting by longitude makes it easier to compare
#' # the map and the section.
#' library(oce)
#' data(section)
#' s <- sectionGrid(subset(section, -30 <= longitude))
#' ss <- sectionSort(ss, by="longitude")
#' plot(ss)
#'}
#'
#' @author Dan Kelley
#'
#' @family things related to section data
sectionSort <- function(section, by)
{
    if (missing(by)) {
        by <- "stationId"
    } else {
        byChoices <- c("stationId", "distance", "longitude", "latitude", "time", "spine")
        iby <- match(by, byChoices, nomatch=0)
        if (0 == iby)
            stop('unknown by value "', by, '"; should be one of: ', paste(byChoices, collapse=" "))
        by <- byChoices[iby]
    }
    res <- section
    if (by == "stationId") {
        o <- order(section@metadata$stationId)
    } else if (by == "distance") {
        o <- order(section[["distance", "byStation"]])
    } else if (by == "longitude") {
        o <- order(section[["longitude", "byStation"]])
    } else if (by == "latitude") {
        o <- order(section[["latitude", "byStation"]])
    } else if (by == "time") {
        ## FIXME: should check to see if startTime exists first?
        times <- unlist(lapply(section@data$station, function(x) x@metadata$startTime))
        o <- order(times)
    } else if (by == "spine") {
        stop("not implemented for spine")
    } else {
        o <- seq_along(section[["station"]]) ## cannot ever get here, actually
    }
    res@metadata$stationId <- res@metadata$stationId[o]
    res@metadata$longitude <- res@metadata$longitude[o]
    res@metadata$latitude <- res@metadata$latitude[o]
    if ("time" %in% names(res@metadata))
        res@metadata$time <- res@metadata$time[o]
    res@data$station <- res@data$station[o]
    res@processingLog <- processingLogAppend(res@processingLog, paste(deparse(match.call()), sep="", collapse=""))
    res
}

#' Add a CTD Station to a Section
#'
#' Add a CTD profile to an existing section.
#'
#' @section Historical note:
#' Until March 2015, this operation was carried out with the `+` operator,
#' but at that time, the syntax was flagged by the development version of R, so it
#' was changed to the present form.
#'
#' @param section A section to which a station is to be added.
#'
#' @param station A ctd object holding data for the station to be added.
#'
#' @aliases sectionAddCtd
#' @return A [section-class] object.
#'
#' @examples
#' library(oce)
#' data(ctd)
#' ctd2 <- ctd
#' ctd2[["temperature"]] <- ctd2[["temperature"]] + 0.5
#' ctd2[["latitude"]] <- ctd2[["latitude"]] + 0.1
#' section <- as.section(c("ctd", "ctd2"))
#' ctd3 <- ctd
#' ctd3[["temperature"]] <- ctd[["temperature"]] + 1
#' ctd3[["latitude"]] <- ctd[["latitude"]] + 0.1
#' ctd3[["station"]] <- "Stn 3"
#' sectionAddStation(section, ctd3)
#'
#' @author Dan Kelley
#'
#' @family things related to section data
sectionAddStation <- function(section, station)
{
    if (missing(section)) stop("must provide a section to which the ctd is to be added")
    if (!inherits(section, "section")) stop("'section' is not a 'section' object")
    if (missing(station)) return(section)
    if (!inherits(station, "ctd")) stop("'station' is not a 'ctd' object")
    res <- section
    n.orig <- length(section@data$station)
    s <- vector("list", n.orig + 1)
    for (i in 1:n.orig)
        s[[i]] <- section@data$station[[i]]
    s[[n.orig + 1]] <- station
    res@data$station <- s
    res@metadata$longitude <- c(res@metadata$longitude, station@metadata$longitude)
    res@metadata$latitude <- c(res@metadata$latitude, station@metadata$latitude)
    res@metadata$stationId <- c(res@metadata$stationId, station@metadata$station)
    res@processingLog <- processingLogAppend(res@processingLog, paste(deparse(match.call()), sep="", collapse=""))
    res
}
sectionAddCtd <- sectionAddStation


#' Plot a section Object
#'
#' Creates a summary plot for a CTD section, with one panel for each value of
#' `which`.
#'
#' The type of plot is governed by `which`, as follows.
#' * `which=0` or `"potential temperature"` for potential temperature contours
#' * `which=1` or `"temperature"` for in-situ temperature contours (the default)
#' * `which=2` or `"salinity"` for salinity contours
#' * `which=3` or `"sigmaTheta"` for sigma-theta contours
#' * `which=4` or `"nitrate"` for nitrate concentration contours
#' * `which=5` or `"nitrite"` for nitrite concentration contours
#' * `which=6` or `"oxygen"` for oxygen concentration  contours
#' * `which=7` or `"phosphate"` for phosphate concentration contours
#' * `which=8` or `"silicate"` for silicate concentration contours
#' * `which=9` or `"u"` for eastward velocity
#' * `which=10` or `"uz"` for vertical derivative of eastward velocity
#' * `which=11` or `"v"` for northward velocity
#' * `which=12` or `"vz"` for vertical derivative of northward velocity
#' * `which=20` or `"data"` for a dot for each data location
#' * `which=99` or `"map"` for a location map
#'
#' The y-axis for the contours is pressure, plotted in the conventional reversed
#' form, so that the water surface appears at the top of the plot.  The x-axis is
#' more complicated. If `at` is not supplied, then the routine calculates x
#' as the distance between the first station in the section and each of the other
#' stations. (This will produce an error if the stations are not ordered
#' geographically, because the [contour()] routine cannot handle
#' non-increasing axis coordinates.) If `at` is specified, then it is taken
#' to be the location, in arbitrary units, along the x-axis of labels specified by
#' `labels`; the way this works is designed to be the same as for
#' [axis()].
#'
#' @param x a [section-class] object.
#'
#' @param which a list of desired plot types, as explained in \dQuote{Details}.
#' There may be up to four panels in total, and the desired plots are placed in
#' these panels, in reading order.  If only one panel is plotted, `par` is
#' not adjusted, which makes it easy to add to the plot with subsequent plotting
#' commands.
#'
#' @template eosTemplate
#'
#' @param at If `NULL` (the default), the x axis will indicate the distance
#' of the stations from the first in the section.  (This may give errors in the
#' contouring routine, if the stations are not present in a geographical order.)
#' If a list, then it indicates the values at which stations will be plotted.
#'
#' @param labels Either a logical, indicating whether to put labels on the x axis,
#' or a vector that is a list of labels to be placed at the x positions indicated
#' by `at`.
#'
#' @param grid If `TRUE`, points are drawn at data locations.
#'
#' @param contourLevels Optional contour levels.
#'
#' @param contourLabels Optional contour labels.
#'
#' @param stationIndices Optional list of the indices of stations to use.  Note
#' that an index is *not* a station number, e.g. to show the first 4
#' stations, use `station.indices=1:4`.
#'
#' @param coastline Either a [coastline-class] object to be used,
#' or a string.  In the second case, the permitted
#' choices are `"best"` (the default) to pick
#' a variant that suits the scale, `"coastlineWorld"` for the coarse
#' version that is provided by \CRANpkg{oce},
#' `"coastlineWorldMedium"` or `"coastlineWorldFine"` for two
#' coastlines provided by the \CRANpkg{ocedata} package, or `"none"`, to avoid
#' drawing a coastline.
#'
#' @param xlim Optional limit for x axis (only in sections, not map).
#'
#' @param ylim Optional limit for y axis (only in sections, not map)
#'
#' @param zlim,zbreaks,zcol Elements that control colours for `image` and `points`
#' plot types, i.e. if `ztype` is either `"points"` or `"image"`.
#' `zlim` is a two-element numerical vector specifying the
#' limit on the plotted field.  If not provided, it defaults to the data
#' range.
#' `zbreaks` controls the colour breaks, in a manner that is similar to
#' the [image()] parameter named `breaks`.  If not provided, `zbreaks` is
#' inferred from `zlim`.
#' `zcol`, which controls the colour scheme, may be a vector of colours
#' (of length 1 less than `zbreaks`), or a function that takes an
#' integer as its sole argument and returns that number of colours.
#' If not provided, `zcol` defaults to [oceColorsViridis()].
#' These three parameters are used in Example 6, an illustration of
#' Atlantic salinity along 36N.
#'
#' @param map.xlim,map.ylim Optional limits for station map; `map.ylim` is
#' ignored if `map.xlim` is provided.
#'
#' @param clongitude,clatitude,span Optional map centre position and span (km).
#'
#' @param projection Parameter specifying map
#' projection; see [mapPlot()].  If `projection="automatic"`,
#' however, a projection is devised from the data, with `stereographic` if
#' the mean latitude exceeds 70N and `mollweide` otherwise.
#'
#' @param xtype Type of x axis, for contour plots, either `"distance"` for
#' distance (in km) to the first point in the section, `"track"` for distance
#' along the cruise track, `"longitude"`, `"latitude"`,
#' `"time"` or `"spine"` (distance along a spine that was added
#' with [addSpine()]).  Note that if the x values are not in order, they will be put in
#' order, and since that might not make physical sense, a warning will be issued.
#'
#' @param longitude0,latitude0 Location of the point from which distance is measured.
#' These values are ignored unless `xtype` is `"distance"`.
#'
#' @param ytype Type of y axis for contour plots, either `"pressure"` for
#' pressure (in dbar, with zero at the surface) or `"depth"` for depth (in m
#' below the surface, calculated from pressure with [swDepth()]).
#'
#' @param ztype String indicating whether to how to indicate the "z"
#' data (in the R sense, i.e. this could be salinity, temperature, etc; it does
#' not mean the vertical coordinate) The choices are: `"contour"` for
#' contours, `"image"` for an image (drawn with [imagep()] with
#' `filledContours=TRUE`), or `"points"` to draw points.
#' In the first two cases, the data must be gridded, with identical pressures at
#' each station.
#'
#' @param legend.loc Location of legend, as supplied to [legend()], or
#' set to the empty string to avoid plotting a legend.
#'
#' @param legend.text character value indicating the text for the legend.
#' If this is NULL (the default) then the legend is automatically
#' constructed by [labelWithUnit()], based on the value of `which`.
#'
#' @param showStations Logical indicating whether to draw station numbers on maps.
#'
#' @param showStart Logical indicating whether to indicate the first station with
#'
#' @param stationTicks A logical value indicating whether to indicate station
#' locations with ticks at the top margin of cross-section plots. Setting this
#' parameter to `FALSE` frees the user up to do their own labelling
#' at this spot.
#'
#' @param showBottom An indication of whether (and how) to indicate the ocean bottom.
#' If `FALSE`, then the bottom is not rendered. If `TRUE`, then it
#' is rendered with a gray polygon. If `showBottom` is a character string,
#' then there are three possibilities: is the string is `"polygon"` then
#' a polygon is drawn, if it is `"lines"` then a line is drawn, and if it
#' is `"points"` then points are drawn. If `showBottom` is
#' a [topo-class] object,
#' then the station locations are
#' interpolated to that topography and the results are shown with a polygon.
#' In this last case, the interpolation is set at a grid that is roughly
#' in accordance with the resolution of the latitudes in the `topo` object.
#' See \dQuote{Examples}.
#'
#' @param showBottom a value indicating whether (and how) to indicate the
#' ocean bottom on cross-section views.  There are three possibilities.
#' (a) If `showBottom` is `FALSE`, then the bottom is not rendered.  If it
#' is `TRUE`, then the  bottom is rendered with a gray polygon.
#' (b) If `showBottom` is the character value `"polygon"`, then a polygon is drawn,
#' and similarly lines are drawn for `"lines"`, and points for `"points"`.
#' (c) If `showBottom` is a [topo-class] object, then the station locations are
#' interpolated to that topography and the results are shown with a polygon.
#' See \dQuote{Examples}.
#'
#' @param showSpine logical value used if `which="map"`.  If `showSpine` is
#' `TRUE` and `section` has had a spine added with [addSpine()], then
#' the spine is drawn in blue.
#'
#' @param drawPalette Logical value indicating whether to draw a palette when `ztype="image"`
#' ignored otherwise.
#'
#' @param axes Logical value indicating whether to draw axes.
#'
#' @param mgp A 3-element numerical vector to use for `par(mgp)`, and also for
#' `par(mar)`, computed from this. If not provided, this defaults to
#' [`getOption`]`("oceMgp")`.
#'
#' @param mar Value to be used with [`par`]`("mar")`. If not provided,
#' a default is set up.
#'
#' @param col Color for line types.  If not provided, this defaults to
#' [`par`]`("col")`.  See `zcol`, for `ztype="image"` and `ztype="points"`.
#'
#' @param cex Numerical character-expansion factor, which defaults to [`par`]`("cex")`.
#'
#' @param pch Indication of symbol type; defaults to [`par`]`("pch")`.
#'
#' @param labcex Size of characters in contour labels (passed to
#' [contour()]).
#'
#' @template debugShortTemplate
#'
#' @param ... Optional arguments passed to the contouring function.
#'
#' @return If the original section was gridded, the return value is that section.
#' Otherwise, the gridded section that was constructed for the plot is returned.
#' In both cases, the value is returned silently. The
#' purpose of returning the section is to enable subsequent processing
#' of the grid, including adding elements to the plot (see example 5).
#'
#' @seealso The documentation for [section-class] explains the
#' structure of section objects, and also outlines the other functions dealing
#' with them.
#'
#' @examples
#' library(oce)
#' data(section)
#' sg <- sectionGrid(section)
#'
#' # 1. start of section, default fields.
#' plot(head(section))
#'
#' # 2. Gulf Stream
#' GS <- subset(section, 109<=stationId&stationId<=129)
#' GSg <- sectionGrid(GS, p=seq(0, 2000, 100))
#' plot(GSg, which=c(1, 99), map.ylim=c(34, 42))
#' par(mfrow=c(2, 1))
#' plot(GS, which=1, ylim=c(2000, 0), ztype='points',
#'      zbreaks=seq(0,30,2), pch=20, cex=3)
#' plot(GSg, which=1, ztype='image', zbreaks=seq(0,30,2))
#'
#' par(mfrow=c(1, 1))
#'
#' # 3. Image, with colored dots to indicate grid-data mismatch.
#'\dontrun{
#' plot(GSg, which=1, ztype='image')
#' T <- GS[['temperature']]
#' col <- oceColorsViridis(100)[rescale(T, rlow=1, rhigh=100)]
#' points(GS[['distance']],GS[['depth']],pch=20,cex=3,col='white')
#' points(GS[['distance']],GS[['depth']],pch=20,cex=2.5,col=col)
#'}
#'
## #' ## 4. Image of temperature, with a high-salinity contour on top;
## #' ##    note the Mediterranean water.
## #' sec <- plot(section, which='temperature', ztype='image')
## #' S <- sec[["salinity", "grid:distance-pressure"]]
## #' contour(S$distance, S$pressure, S$field, level=35.8, lwd=3, add=TRUE)
## #'
## #' ## 5. Contours of salinity, with dots for high pressure and spice
## #' plot(section, which='salinity')
## #' distance <- section[["distance"]]
## #' depth <- section[["depth"]]
## #' spice <- section[["spice"]]
## #' look <- spice > 1.8 & depth > 500
## #' points(distance[look], depth[look], col='red')
#'
#'\dontrun{
#' # 4. Image of Absolute Salinity, with 4-minute bathymetry
#' # It's easy to calculate the desired area for the bathymetry,
#' # but for brevity we'll hard-code it. Note that download.topo()
#' # caches the file locally.
#' f <- download.topo(west=-80, east=0, south=35, north=40, resolution=4)
#' t <- read.topo(f)
#' plot(section, which="SA", xtype="longitude", ztype="image", showBottom=t)
#'}
#'
#'\dontrun{
#' # 5. Temperature with salinity added in red
#' s <- plot(section, which="temperature")
#' distance <- s[["distance", "byStation"]]
#' depth <- s[["station", 1]][["depth"]]
#' salinity <- matrix(s[["salinity"]], byrow=TRUE, nrow=length(s[["station"]]))
#' contour(distance, depth, salinity, col=2, add=TRUE)
#'}
#'
#'\dontrun{
#' # 6. Image with controlled colours
#' plot(section, which="salinity", ztype="image",
#'     zlim=c(35, 37.5),
#'     zbreaks=seq(35, 37.5, 0.25),
#'     zcol=oceColorsTurbo)
#'}
#'
#' @author Dan Kelley
#'
#' @family functions that plot oce data
#' @family things related to section data
#'
#' @aliases plot.section
setMethod(f="plot",
          signature=signature("section"),
          definition=function(x,
                              which=c(1, 2, 3, 99),
                              eos,
                              at=NULL,
                              labels=TRUE,
                              grid=FALSE,
                              contourLevels=NULL,
                              contourLabels=NULL,
                              stationIndices,
                              coastline="best",
                              xlim=NULL, ylim=NULL,
                              zlim=NULL, zbreaks=NULL, zcol=NULL,
                              map.xlim=NULL, map.ylim=NULL,
                              clongitude, clatitude, span,
                              projection=NULL,
                              xtype="distance", ytype="depth", ztype="contour",
                              longitude0, latitude0,
                              legend.loc="bottomright",
                              legend.text=NULL,
                              showStations=FALSE,
                              showStart=TRUE,
                              stationTicks=TRUE,
                              showBottom=TRUE,
                              showSpine=TRUE,
                              drawPalette=TRUE,
                              axes=TRUE, mgp, mar,
                              col, cex, pch,
                              labcex=1,
                              debug, ...)
          {
              if (missing(debug))
                  debug <- getOption("oceDebug")
              debug <- if (debug > 4) 4 else floor(0.5 + debug)
              if (missing(eos))
                  eos <- getOption("oceEOS", default="gsw")
              xtype <- match.arg(xtype, c("distance", "track", "longitude", "latitude", "time", "spine"))
              ytype <- match.arg(ytype, c("depth", "pressure"))
              ztype <- match.arg(ztype, c("contour", "image", "points"))
              drawPoints <- ztype == "points"
              if (!inherits(coastline, "coastline"))
                  coastline <- match.arg(coastline,
                                         c("best", "coastlineWorld", "coastlineWorldMedium",
                                           "coastlineWorldFine", "none"))
              if (missing(mgp))
                  mgp <- getOption("oceMgp")
              if (missing(mar))
                  mar <- c(mgp[1]+1, mgp[1]+1.5, mgp[2]+1, mgp[2]+0.5)
              if (missing(col))
                  col <- par("col")
              if (missing(cex))
                  cex <- par("cex")
              if (missing(pch))
                  pch <- par("pch")
              ## L and R are used much later, for constructing labels
              L <- if (getOption("oceUnitBracket") == "[") " [" else " ("
              R <- if (getOption("oceUnitBracket") == "[")  "]" else  ")"

              ## Make 'which' be character, to simplify following code
              ##oceDebug(debug, "which=c(", paste(which, collapse=","), ")\n")
              lw <- length(which)
              legend.text <- rep(legend.text, lw)
              whichOriginal <- which
              ##which <- oce.pmatch(which,
              ##                    list(temperature=1, salinity=2,
              ##                         sigmaTheta=3, nitrate=4, nitrite=5, oxygen=6,
              ##                         phosphate=7, silicate=8,
              ##                         u=9, uz=10, v=11, vz=12, # lowered adcp
              ##                         data=20, map=99))
              if (is.numeric(which)) {
                  which[which==0] <- "potential temperature"
                  which[which==1] <- "temperature"
                  which[which==2] <- "salinity"
                  which[which==3] <- "sigmaTheta"
                  which[which==4] <- "nitrate"
                  which[which==5] <- "nitrite"
                  which[which==6] <- "oxygen"
                  which[which==7] <- "phosphate"
                  which[which==8] <- "silicate"
                  which[which==9] <- "u"
                  which[which==10] <- "uz"
                  which[which==11] <- "v"
                  which[which==12] <- "vz"
                  which[which==20] <- "data"
                  which[which==99] <- "map"
              }
              ##oceDebug(debug, "which=c(", paste(which, collapse=","), ")\n")
              oceDebug(debug, "plot.section(, ..., which=c(",
                       paste(which, collapse=","), "), eos=\"", eos,
                       "\", ztype=\"", ztype, "\", ...) {\n", sep="", unindent=1)
              ## Ensure data on levels, for plots requiring pressure (e.g. sections). Note
              ## that we break out of the loop, once we grid the section.
              if (is.na(which[1]) || which[1] != "data" || which[1] != 'map') {
                  p1 <- x[["station", 1]][["pressure"]]
                  numStations <- length(x@data$station)
                  for (ix in 2:numStations) {
                      thisStation <- x@data$station[[ix]]
                      thisPressure <- thisStation[["pressure"]]
                      if ("points" != ztype && !identical(p1, thisPressure)) {
                          oceDebug(debug, "gridding section because pressures at station ", ix, " differ from those at station 1\n")
                          x <- sectionGrid(x, debug=debug-1)
                          break
                      }
                  }
              }
              res <- x # will now be gridded (either originally or through above code)

              ## Trim stations that have zero good data FIXME: brittle to addition of new metadata
              haveData <- unlist(lapply(x@data$station,
                                        function(stn) 0 < length(stn[['pressure']])))
              x@data$station <- x@data$station[haveData]
              x@metadata$stationId <- x@metadata$stationId[haveData]
              x@metadata$latitude <- x@metadata$latitude[haveData]
              x@metadata$longitude <- x@metadata$longitude[haveData]
              x@metadata$time <- x@metadata$time[haveData]
              plotSubsection <- function(xx, yy, zz,
                  which.xtype, which.ytype,
                  variable="temperature", vtitle="T", unit=NULL,
                  eos=getOption("oceEOS", default="gsw"),
                  indicate.stations=TRUE,
                  contourLevels=NULL, contourLabels=NULL,
                  showStations=FALSE,
                  xlim=NULL, ylim=NULL,
                  clongitude, clatitude, span,
                  projection=NULL,
                  zbreaks=NULL, zcol=NULL,
                  ztype=c("contour", "image", "points"),
                  legend=TRUE,
                  debug=0,
                  axes=TRUE,
                  col=par("col"),
                  ...)
              {
                  oceDebug(debug, "plotSubsection(variable=\"", variable,
                           "\", eos=\"", eos,
                           "\", which.xtype=\"", which.xtype,
                           "\", ztype=\"", ztype,
                           "\", zcol=", if (missing(zcol)) "(missing)" else "(provided)",
                           "\", span=", if (missing(span)) "(missing)" else span,
                           ", showStations=", showStations,
                           ", axes=", axes, ", ...) {\n", sep="", unindent=1)
                  ztype <- match.arg(ztype)
                  drawPoints <- "points" == ztype
                  omar <- par('mar')
                  xIsTime <- inherits(xx, "POSIXt")

                  canPlot <- TRUE      # assume we can plot; use this instead of nested 'break's

                  if (as.character(variable) == "map") {
                      lat <- array(NA_real_, numStations)
                      lon <- array(NA_real_, numStations)
                      for (i in 1:numStations) {
                          thisStation <- x[["station", stationIndices[i]]]
                          lon[i] <- mean(thisStation[["longitude"]], na.rm=TRUE)
                          lat[i] <- mean(thisStation[["latitude"]], na.rm=TRUE)
                      }
                      ## lon[lon<0] <- lon[lon<0] + 360
                      asp <- 1 / cos(mean(range(lat, na.rm=TRUE))*pi/180)
                      latm <- mean(lat, na.rm=TRUE)
                      lonm <- mean(lon, na.rm=TRUE)
                      if (missing(span)) {
                          lonr <- lonm + sqrt(2) * (range(lon, na.rm=TRUE) - mean(lon, na.rm=TRUE)) # expand range
                          latr <- latm + sqrt(2) * (range(lat, na.rm=TRUE) - mean(lat, na.rm=TRUE))
                      } else {
                          ## FIXME: the sqrt(2) below helps in a test case ... not sure it make sense though --DK
                          lonr <- lonm + span / 111.1 * c(-0.5, 0.5) / cos(pi/180*latm) / sqrt(2)
                          latr <- latm + span / 111.1 * c(-0.5, 0.5) / sqrt(2)
                          ##DEBUG message("KELLEY span=", span)
                          ##DEBUG message("KELLEY lonm=", lonm, " lonr=", paste(lonr, collapse=", "))
                          ##DEBUG message("KELLEY latm=", latm, " latr=", paste(latr, collapse=", "))
                      }

                      ## FIXME: this coastline code is reproduced in section.R; it should be DRY
                      haveCoastline <- FALSE
                      if (inherits(coastline, "coastline")) {
                          haveCoastline <- TRUE
                          oceDebug(debug, "using coastline object given as an argument\n")
                      } else {
                          if (!is.character(coastline))
                              stop("coastline must be a character string")
                          haveOcedata <- requireNamespace("ocedata", quietly=TRUE)
                          if (coastline == "best") {
                              if (haveOcedata) {
                                  bestcoastline <- coastlineBest(lonRange=lonr, latRange=latr)
                                  oceDebug(debug, "'best' coastline is: \"", bestcoastline, '\"\n', sep="")
                                  if (bestcoastline == "coastlineWorld") {
                                      data(list=bestcoastline, package="oce", envir=environment())
                                  } else {
                                      data(list=bestcoastline, package="ocedata", envir=environment())
                                  }
                                  coastline <- get(bestcoastline)
                              } else {
                                  oceDebug(debug, "using \"coastlineWorld\" because ocedata package not installed\n")
                                  data("coastlineWorld", package="oce", envir=environment())
                                  coastline <- get("coastlineWorld")
                              }
                              haveCoastline <- TRUE
                          } else {
                              if (coastline != "none") {
                                  if (coastline == "coastlineWorld") {
                                      data("coastlineWorld", package="oce", envir=environment())
                                      coastline <- get("coastlineWorld")
                                  } else if (haveOcedata && coastline == "coastlineWorldFine") {
                                      data("coastlineWorldFine", package="ocedata", envir=environment())
                                      coastline <- get("coastlineWorldFine")
                                  } else if (haveOcedata && coastline == "coastlineWorldMedium") {
                                      data("coastlineWorldMedium", package="ocedata", envir=environment())
                                      coastline <- get("coastlineWorldMedium")
                                  }  else {
                                      stop("there is no built-in coastline file of name \"", coastline, "\"")
                                  }
                                  haveCoastline <- TRUE
                              }
                          }
                      }

                      ## FIXME: I think both should have missing() means auto-pick and NULL means none
                      if (!is.null(projection)) {
                          stnlats <- x[["latitude", "byStation"]]
                          stnlons <- x[["longitude", "byStation"]]
                          if (is.null(map.xlim)) map.xlim <- range(stnlons)
                          if (is.null(map.ylim)) map.ylim <- range(stnlats)
                          id <- pmatch(projection, "automatic")
                          if (!is.na(id)) {
                              meanlat <- mean(stnlats, na.rm=TRUE)
                              meanlon <- mean(stnlons, na.rm=TRUE)
                              ## NOTE: mercator messes up filling for data(section) but mollweide is okay
                              projection <- if (meanlat > 70) "stereographic" else "mollweide"
                              orientation <- c(90, meanlon, 0)
                              oceDebug(debug, "using", projection, "projection (chosen automatically)\n")
                          } else {
                              oceDebug(debug, "using", projection, "projection (specified)\n")
                          }
                          mapPlot(coastline, longitudelim=map.xlim, latitudelim=map.ylim, projection=projection, col='gray')
                          spine <- x[["spine"]]
                          if (showSpine && !is.null(spine))
                              mapLines(spine$longitude, spine$latitude, col="blue", lwd=1.4*par("lwd"))
                          mapPoints(x[['longitude', 'byStation']], x[['latitude', 'byStation']],
                                    col=col, pch=3, lwd=1/2)
                          if (xtype == "distance" && showStart) {
                              mapPoints(lon[1], lat[1], col=col, pch=22, cex=3*par("cex"), lwd=1/2)
                          }
                          return()     ## NOTE early return
                      } else {
                          if (!is.null(map.xlim)) {
                              map.xlim <- sort(map.xlim)
                              plot(lonr, latr, xlim=map.xlim, asp=asp, type='n',
                                   xlab=gettext("Longitude", domain="R-oce"),
                                   ylab=gettext("Latitude", domain="R-oce"))
                          } else if (!is.null(map.ylim)) {
                              map.ylim <- sort(map.ylim)
                              plot(lonr, latr, ylim=map.ylim, asp=asp, type='n',
                                   xlab=gettext("Longitude", domain="R-oce"),
                                   ylab=gettext("Latitude", domain="R-oce"))
                          } else {
                              ##DEBUG message("CCC lonr=", paste(lonr, collapse=","))
                              ##DEBUG message("CCC latr=", paste(latr, collapse=","))
                              ##DEBUG message("CCC asp=", paste(asp, collapse=","))
                              plot(lonr, latr, asp=asp, type='n',
                                   xlab=gettext("Longitude", domain="R-oce"),
                                   ylab=gettext("Latitude", domain="R-oce"))
                          }
                      }
                      if (haveCoastline) {
                          if (!is.null(coastline@metadata$fillable) && coastline@metadata$fillable) {
                              polygon(coastline[["longitude"]], coastline[["latitude"]], col="lightgray", lwd=3/4)
                              polygon(coastline[["longitude"]]+360, coastline[["latitude"]], col="lightgray", lwd=3/4)
                              ## redraw box, if we have axes. This is necessary because polygon will color
                              ## over the axis box, if land goes past the edge of the view
                              if (axes)
                                  box()
                          } else {
                              lines(coastline[["longitude"]], coastline[["latitude"]], col="darkgray")
                              lines(coastline[["longitude"]]+360, coastline[["latitude"]], col="darkgray")
                          }
                      }
                      spine <- x[["spine"]]
                      if (showSpine && !is.null(spine))
                          lines(spine$longitude, spine$latitude, col="blue", lwd=1.4*par("lwd"))

                      ## add station data
                      lines(lon, lat, col="lightgray")
                      ## replot with shifted longitude
                      col <- if ("col" %in% names(list(...))) list(...)$col else "black"
                      points(lon, lat, col=col, pch=3, lwd=1/2)
                      points(lon - 360, lat, col=col, pch=3, lwd=1/2)
                      if (showStations) {
                          stationId <- x[['station ID']]
                          text(lon, lat, stationId, pos=2, cex=cex)
                          text(lon-360, lat, stationId, pos=2, cex=cex)
                      }
                      if (xtype == "distance" && showStart) {
                          points(lon[1], lat[1], col=col, pch=22, cex=3*par("cex"), lwd=1/2)
                          points(lon[1] - 360, col=col, lat[1], pch=22, cex=3*par("cex"), lwd=1/2)
                      }
                      if (indicate.stations) {
                          dy <- 5 * mean(diff(sort(x@metadata$latitude)), na.rm=TRUE)
                          dx <- 5 * mean(diff(sort(x@metadata$longitude)), na.rm=TRUE)
                          ylab <- x@metadata$latitude[1]  - dy * sign(x@metadata$latitude[2]  - x@metadata$latitude[1])
                          xlab <- x@metadata$longitude[1] - dx * sign(x@metadata$longitude[2] - x@metadata$longitude[1])
                          ## text(xlab, ylab, x@metadata$stationId[1])
                          xlab <- x@metadata$longitude[numStations] - dx * sign(x@metadata$longitude[numStations-1] - x@metadata$longitude[numStations])
                          ylab <- x@metadata$latitude[numStations]  - dy * sign(x@metadata$latitude[numStations-1]  - x@metadata$latitude[numStations])
                          ## text(xlab, ylab, x@metadata$stationId[numStations])
                      }
                  } else {
                      ## not a map
                      z <- x[[variable]]
                      zAllMissing <- all(is.na(z))
                      ##> message("zAllMissing=", zAllMissing)
                      ##> message("drawPoints=", drawPoints)
                      ##> message("ztype='", ztype, "'")
                      if ((drawPoints || ztype == "image") && !zAllMissing) {
                          ##> message("is.null(zbreaks)=", is.null(zbreaks))
                          if (is.null(zbreaks)) {
                              if (is.null(zlim)) {
                                  ## Use try() to quiet warnings if all data are NA
                                  zRANGE <- try(range(z, na.rm=TRUE), silent=TRUE)
                                  if (is.null(zcol) || is.function(zcol)) {
                                      zbreaks <- seq(zRANGE[1], zRANGE[2], length.out=200)
                                  } else {
                                      zbreaks <- seq(zRANGE[1], zRANGE[2], length.out=length(zcol) + 1)
                                  }
                              } else {
                                  zbreaks <- seq(zlim[1], zlim[2], length.out=200)
                              }
                          }
                          nbreaks <- length(zbreaks)
                          if (nbreaks > 0) {
                              if (is.null(zcol)) {
                                  ## col <- oceColorsJet(nbreaks - 1)
                                  zcol <- oceColorsViridis(nbreaks - 1)
                              }
                              if (is.function(zcol))
                                  zcol <- zcol(nbreaks - 1)
                              zlim <- range(zbreaks)
                              if(drawPalette == TRUE) {
                              drawPalette(zlim=range(zbreaks), breaks=zbreaks, col=zcol)}
                          }
                      }


                      ## FIXME: contours don't get to plot edges
                      xxrange <- range(xx, na.rm=TRUE)
                      yyrange <- range(yy, na.rm=TRUE)

                      ylim <- if (!is.null(ylim)) sort(-abs(ylim)) else yyrange
                      par(xaxs="i", yaxs="i")
                      ylab <- if ("ylab" %in% names(list(...))) {
                          list(...)$ylab
                      } else {
                          if (which.ytype==1) {
                              resizableLabel("p")
                          } else {
                              resizableLabel("depth")
                          }
                      }
                      if (is.null(at)) {
                          if ("xlab" %in% names(list(...))) {
                              xlab <- list(...)$xlab
                          } else {
                              xlab <- switch(which.xtype,
                                             resizableLabel("distance km"),
                                             resizableLabel("along-track distance km"),
                                             gettext("Longitude", domain="R-oce"),
                                             gettext("Latitude", domain="R-oce"),
                                             gettext("Time", domain="R-oce"),
                                             resizableLabel("along-spine distance km"))
                          }
                          plot(xxrange, yyrange,
                               xaxs="i", yaxs="i",
                               xlim=xlim,
                               ylim=ylim,
                               col="white",
                               xlab=xlab,
                               ylab=ylab,
                               axes=FALSE)
                          if (axes) {
                              ## oceDebug(debug, "drawing axes\n")
                              axis(4, labels=FALSE)
                              ytics <- axis(2, labels=FALSE)
                              axis(2, at=ytics, labels=-ytics)
                              ## If constructing labels for time, need to check xlim
                              if (xIsTime) {
                                  if (!is.null(xlim)) {
                                      ## FIXME: might need to check whether/how xx used later
                                      XX <- xx[xlim[1] <= xx & xx <= xlim[2]]
                                      axis(1, at=pretty(XX), labels=pretty(XX))
                                  } else {
                                      axis(1, at=pretty(xx), labels=pretty(xx))
                                  }
                              } else {
                                  axis(1)
                              }
                              ## oceDebug(debug, "finished drawing axes\n")
                          }
                          box()
                      } else {
                          plot(xxrange, yyrange,
                               xaxs="i", yaxs="i",
                               ##                     ylim=rev(yyrange),
                               xlim=xlim, ylim=ylim,
                               col="white",
                               xlab="", ylab=ylab, axes=FALSE)
                          if (axes) {
                              axis(1, at=at, labels=labels)
                              axis(2)
                              axis(4, labels=FALSE)
                          }
                          box()
                      }
                      ## Bottom trace
                      usr <- par("usr")
                      graph.bottom <- usr[3]
                      waterDepth <- NULL
                      ## For ztype == "points", plot the points.  Otherwise, collect them in zz
                      ## for the contour or image plot.
                      for (i in 1:numStations) {
                          thisStation <- x[["station", i]]
                          p <- thisStation[["pressure"]] # assume that we always have pressure
                          np <- length(p)
                          ##oceDebug(debug, "filling matrix with \"", variable, "\" data at station", i, "\n", sep="")
                          if (variable != "data") {
                              ## CAUTION: the assignment to 'v' and 'zz' is tricky:
                              ## 1. not all datasets will have computed items (e.g. density and potential
                              ## temperature) so we compute them, discarding any data. (Is that sensible?)
                              ## 2. things are different in gsw and unesco, and someone might say
                              ## "potential temperature" in either system, so which do we compute??
                              ## 3. there was some code reworking in early May 2019, relating to issues:
                              ## https://github.com/dankelley/oce/issues/1539
                              ## and
                              ## https://github.com/dankelley/oce/issues/1540
                              ## and there is a chance of breakage starting at that time.
                              v <- thisStation[[variable]]
                              if (is.null(v))
                                  v <- rep(NA, length(p))
                              if (drawPoints) {
                                  p <- thisStation[["pressure"]]
                                  points(rep(xx[i], np), -p,
                                         pch=pch, cex=cex,
                                         col=zcol[rescale(v, xlow=zlim[1], xhigh=zlim[2], rlow=1, rhigh=nbreaks)])
                              } else {
                                  ## Compute sigma0 and sigmaTheta, whether they are in the dataset or not
                                  zz[i, ] <- rev(v)
                              }
                          }
                          if (grid && !drawPoints)
                              points(rep(xx[i], length(yy)), yy, col="gray", pch=20, cex=1/3)
                          temp <- x@data$station[[stationIndices[i]]]@data$temperature
                          len <- length(temp)
                          if ("waterDepth" %in% names(x@data$station[[stationIndices[i]]]@metadata)
                              && is.finite(x@data$station[[stationIndices[i]]]@metadata$waterDepth)) {
                              wd <- x@data$station[[stationIndices[i]]]@metadata$waterDepth
                          } else {
                              wd <- NA
                          }
                          in.land <- which(is.na(x@data$station[[stationIndices[i]]]@data$temperature[-3])) # skip first 3 points
                          waterDepth <- c(waterDepth, wd)
                      }
                      if (!grid && axes && stationTicks)
                          Axis(side=3, at=xx, labels=FALSE, tcl=-1/3, lwd=0.5) # station locations
                      bottom.x <- c(xx[1], xx, xx[length(xx)])
                      bottom.y <- if (any(is.finite(waterDepth))) c(graph.bottom, -waterDepth, graph.bottom)
                          else rep(NA, length(bottom.x)+2)
                      ## Put x in order, if it's not already
                      xx[!is.finite(xx)] <- NA # for issue 1583: grid larger than data range can get NaN values
                      ox <- order(xx)
                      xxOrig <- xx
                      ii <- seq_along(xxOrig) # so we can use it later for drawing bottoms
                      if (any(xx[ox] != xx, na.rm=TRUE)) { # for issue 1583: handle the NA just inserted
                          xx <- xx[ox]
                          zz <- zz[ox, ] ## FIXME keep this???
                          ii <- ii[ox]
                          bottom.x <- c(min(xxOrig), xxOrig[ox], max(xxOrig))
                          bottom.y <- c(graph.bottom, -waterDepth[ox], graph.bottom)
                      }
                      # cannot contour with duplicates in x or y; the former is the only problem
                      xx.unique <- c(TRUE, 0 != diff(xx))
                      yy.unique <- c(TRUE, 0 != diff(yy))
                      xx.unique <- xx.unique & !is.na(xx.unique)
                      yy.unique <- yy.unique & !is.na(yy.unique)
                      # a problem with mbari data revealed that we need to chop NA valaues too
                      if (variable == "data") {
                          for (i in 1:numStations) {
                              thisStation <- x[["station", i]]
                              pressure <- thisStation[["pressure"]]
                              if (which.xtype == 4) {
                                  longitude <- mean(thisStation[["longitude"]], na.rm=TRUE)
                                  points(rep(longitude, length(pressure)), -pressure, cex=cex, pch=pch, col=col)
                              } else {
                                  ## FIXME: shouldn't the next line work for all types??
                                  points(rep(xx[i], length(pressure)), -pressure, cex=cex, pch=pch, col=col)
                              }
                          }
                      } else if (!drawPoints) {
                          ## Use try() to quiet warnings if all data are NA
                          if (zAllMissing) {
                              if (nchar(legend.loc)) {
                                  #>if (is.character(vtitle) && vtitle == "sigmaTheta") {
                                  #>    vtitle <- if (eos == "gsw") expression(sigma[0]) else expression(sigma[theta])
                                  #>    unit <- expression(kg/m^3)
                                  #>    vtitle <- bquote(.(vtitle[[1]])*.(L)*.(unit[[1]])*.(R))
                                  #>}
                                  legend(legend.loc, legend=vtitle, bg="white", x.intersp=0, y.intersp=0.5, cex=1)
                              }
                              return()
                          }
                          zrange <- try(range(zz[xx.unique, yy.unique], na.rm=TRUE), silent=TRUE)
                          if (!is.null(contourLevels) && !is.null(contourLabels)) {
                              oceDebug(debug, "user-supplied contourLevels: ", contourLevels, "\n")
                              if (ztype == 'contour') {
                                  contour(x=xx[xx.unique], y=yy[yy.unique], z=zz[xx.unique, yy.unique],
                                          axes=FALSE, add=TRUE,
                                          levels=contourLevels, labels=contourLabels,
                                          col=col,
                                          xaxs="i", yaxs="i",
                                          labcex=labcex, ...)
                              } else if (ztype == "image") {
                                  zz[zz < min(zbreaks)] <- min(zbreaks)
                                  zz[zz > max(zbreaks)] <- max(zbreaks)
                                  if (is.function(zcol))
                                      zcol <- zcol(1+length(zbreaks))
                                  .filled.contour(x=xx[xx.unique], y=yy[yy.unique], z=zz[xx.unique, yy.unique],
                                                  levels=zbreaks, col=zcol)
                              } else {
                                  stop("unknown ztype: \"", ztype, "\" [2]")
                              }
                          } else {
                              oceDebug(debug, "automatically-calculated contourLevels\n")
                              zrange <- range(zz[xx.unique, yy.unique], na.rm=TRUE)
                              if (ztype == 'contour') {
                                  zzrange <- range(zz[xx.unique, yy.unique], na.rm=TRUE)
                                  if (any(!is.finite(zzrange)))
                                      stop("cannot draw a contour diagram because all values are NA or Inf")
                                  contour(x=xx[xx.unique], y=yy[yy.unique], z=zz[xx.unique, yy.unique],
                                          add=TRUE, col=col, labcex=labcex, ...)
                              } else if (ztype == "image") {
                                  zz[zz < min(zbreaks)] <- min(zbreaks)
                                  zz[zz > max(zbreaks)] <- max(zbreaks)
                                  ## FIXME: testing here
                                  if (is.function(zcol))
                                      zcol <- zcol(1+length(zbreaks))
                                  .filled.contour(x=xx[xx.unique], y=yy[yy.unique], z=zz[xx.unique, yy.unique],
                                                  levels=zbreaks, col=zcol)
                              } else if (ztype == "points") {
                                  ## nothing to do now
                              } else {
                                  stop("unknown ztype: \"", ztype, "\" [3]")
                              }
                          }
                      }
                      if (is.character(showBottom) || (is.logical(showBottom) && showBottom)) {
                          type <- "polygon"
                          if (is.character(showBottom))
                              type <- showBottom
                          if (length(bottom.x) == length(bottom.y)) {
                              bottom <- par('usr')[3]
                              if (type == "polygon") {
                                  polygon(bottom.x, bottom.y, col="lightgray")
                              } else if (type == "lines") {
                                  for (s in seq_along(bottom.x))
                                      lines(rep(bottom.x[s], 2), c(bottom.y[s], bottom), col="lightgray")
                              } else if (type == "points") {
                                  for (s in seq_along(bottom.x))
                                      points(rep(bottom.x[s], 2), c(bottom.y[s], bottom), col="lightgray")
                              }
                          }
                          box()
                      } else if (inherits(showBottom, "topo")) {
                          oceDebug(debug, "using a topo object for the bottom\n")
                          ## Fine longitude and latitude: roughly
                          topoResolution <- geodDist(0, 0, 0, diff(showBottom[["latitude"]][1:2]))
                          slon <- x[["longitude", "byStation"]]
                          slat <- x[["latitude", "byStation"]]
                          sectionSpan <- geodDist(min(slon, na.rm=TRUE), min(slat, na.rm=TRUE),
                                                  max(slon, na.rm=TRUE), max(slat, na.rm=TRUE))
                          nin <- length(slon)
                          ## double up on resolution, although perhaps not needed
                          nout <- as.integer(1 + 2 * sectionSpan / topoResolution)
                          blon <- approx(1:nin, slon[ii], n=nout)$y
                          blat <- approx(1:nin, slat[ii], n=nout)$y
                          bottom.y <- topoInterpolate(blon, blat, showBottom)
                          bottom.x <- approx(1:nin, xx, n=nout)$y
                          bottom.x <- c(bottom.x[1], bottom.x, tail(bottom.x, 1))
                          usr3 <- par('usr')[3]
                          bottom.y <- c(usr3, bottom.y, usr3)
                          polygon(bottom.x, bottom.y, col="lightgray")
                      }
                      ##axis(1, pretty(xxOrig))
                      if (axes) {
                          if (xIsTime) {
                              if (!is.null(xlim)) {
                                  ## FIXME: might need to check whether/how xx used later
                                  XX <- xx[xlim[1] <= xx & xx <= xlim[2]]
                                  axis(1, at=pretty(XX), labels=pretty(XX))
                              } else {
                                  axis(1, at=pretty(xx), labels=pretty(xx))
                              }
                          }
                      }
                      #if (is.character(vtitle) && vtitle == "sigmaTheta") {
                      #    vtitle <- expression(sigma[theta])
                      #    unit <- expression(kg/m^3)
                      #}
                      #vtitleOrig <- vtitle
                      #vtitle <- if (length(unit) == 0) vtitle else bquote(.(vtitle[[1]])*.(L)*.(unit[[1]])*.(R))
                      if (nchar(legend.loc)) {
                          legend(legend.loc, legend=vtitle, bg="white", x.intersp=0, y.intersp=0.5, cex=1)
                      }
                      ##lines(xx, -waterDepth[ox], col='red')

                      ## undo negation of the y coordinate, so further can can make sense
                      usr <- par('usr')
                      ##message("usr=", paste(par('usr'), collapse=" "))
                      par('usr'=c(usr[1], usr[2], -usr[3], usr[4]))
                  }
                  par(mar=omar)
                  oceDebug(debug, "} # plotSubsection()\n", unindent=1)
              }                        # plotSubsection()
              opar <- par(no.readonly = TRUE)
              if (length(which) > 1) on.exit(par(opar))
              which.xtype <- match(xtype, c("distance", "track", "longitude", "latitude", "time", "spine"), nomatch=0)
              if (0 == which.xtype)
                  stop('xtype must be one of: "distance", "track", "longitude", "latitude", "time", or "spine", not "', xtype, '" as provided')
              which.ytype <- pmatch(ytype, c("pressure", "depth"), nomatch=0)
              if (missing(stationIndices)) {
                  numStations <- length(x@data$station)
                  stationIndices <- 1:numStations
              } else {
                  numStations <- length(stationIndices)
              }
              if (numStations < 2)
                  stop("In plot() :\n  cannot plot a section containing fewer than 2 stations",
                       call.=FALSE)
              firstStation <- x@data$station[[stationIndices[1]]]
              num.depths <- length(firstStation@data$pressure)
              zz <- array(NA_real_, dim=c(numStations, num.depths))
              xx <- rep(NA, numStations)
              yy <- rep(NA, num.depths)
              if (is.null(at)) {
                  lon0 <- if (missing(longitude0)) mean(firstStation[["longitude"]], na.rm=TRUE) else longitude0
                  lat0 <- if (missing(latitude0)) mean(firstStation[["latitude"]], na.rm=TRUE) else latitude0
                  # oceDebug(debug, vectorShow(lon0))
                  # oceDebug(debug, vectorShow(lat0))
                  for (ix in 1:numStations) {
                      j <- stationIndices[ix]
                      if (which.xtype == 1) { # distance from first station
                          xx[ix] <- geodDist(lon0, lat0,
                                             mean(x@data$station[[j]][["longitude"]], na.rm=TRUE),
                                             mean(x@data$station[[j]][["latitude"]], na.rm=TRUE))
                      } else if (which.xtype == 2) { # distance along the cruise track
                          if (ix == 1) {
                              xx[ix] <- 0
                          } else {
                              xx[ix] <- xx[ix-1] + geodDist(mean(x@data$station[[stationIndices[ix-1]]][["longitude"]], na.rm=TRUE),
                                                            mean(x@data$station[[stationIndices[ix-1]]][["latitude"]], na.rm=TRUE),
                                                            mean(x@data$station[[j]][["longitude"]], na.rm=TRUE),
                                                            mean(x@data$station[[j]][["latitude"]], na.rm=TRUE))
                          }
                      } else if (which.xtype == 3) { # longitude
                          xx[ix] <- mean(x@data$station[[j]][["longitude"]], na.rm=TRUE)
                      } else if (which.xtype == 4) { # latitude
                          xx[ix] <- mean(x@data$station[[j]][["latitude"]], na.rm=TRUE)
                      } else if (which.xtype == 5) { # time
                          ## use ix as a desparate last measure, if there are no times.
                          if (!is.null(x@data$station[[j]]@metadata$startTime)) {
                              xx[ix] <- as.POSIXct(x@data$station[[j]]@metadata$startTime)
                          } else if (!is.null(x@metadata$time[[j]])) {
                              xx[ix] <- x@metadata$time[[j]]
                          } else {
                              xx[ix] <- ix
                              if (ix == 1)
                                  warning("In plot,section-method() :\n  section stations do not contain startTime; using integers for time axis",
                                          call.=FALSE)
                          }
                      }
                  }
              } else {
                  xx <- at
              }
              ##> message("which.xtype: ", which.xtype)
              if (which.xtype == 5) {
                  xx <- numberAsPOSIXct(xx)
              } else if (which.xtype == 6) {
                  ## see https://github.com/dankelley/oce-issues/blob/master/16xx/1678
                  if (!("spine" %in% names(x@metadata))) {
                      stop("In plot,section-metod() :\n  this section has no spine; use addSpine() to add a spine", call.=FALSE)
                  }
                  spine <- x@metadata$spine
                  ## Parametric lon=lon(s), at=lat(s)
                  s <- seq(0, 1, length.out=length(spine$longitude))
                  lonfun <- approxfun(spine$longitude ~ s)
                  latfun <- approxfun(spine$latitude ~ s)
                  ## Create many points on the spine
                  spineSegments <- 1000
                  ss <- seq(0, 1, length.out=spineSegments)
                  stnLon <- x[["longitude", "byStation"]]
                  stnLat <- x[["latitude", "byStation"]]
                  closest <- rep(NA, length=length(stnLon))
                  ## find distance (used in following loop; uses global 'i')
                  for (i in seq_along(stnLon)) {
                      closest[i] <- which.min(sapply(ss,
                                                     function(t) {
                                                         lonSpine <- lonfun(t)
                                                         latSpine <- latfun(t)
                                                         geodDist(lonSpine, latSpine, stnLon[i], stnLat[i])
                                                     }))
                  }
                  ## Map points back to the spine
                  longitudeRemapped <- lonfun(ss[closest])
                  latitudeRemapped <- latfun(ss[closest])
                  xx <- geodDist(longitudeRemapped, latitudeRemapped, alongPath=TRUE)
              }
              ## Grid is regular (so need only first station) unless which=="data"
              ## FIXME: why checking just first which[] value?
              if (which.ytype == 1) {
                  if (!is.na(which[1]) && which[1] == "data" || ztype == "points") {
                      yy <- c(0, -max(x[["pressure"]]))
                  } else {
                      yy <- rev(-x@data$station[[stationIndices[1]]]@data$pressure)
                  }
              } else if (which.ytype == 2) {
                  if (!is.na(which[1]) && which[1] == "data" || ztype == "points") {
                      yy <- c(-max(x[["pressure"]], na.rm=TRUE), 0)
                  } else {
                      ##> message("stationIndices[1]: ", stationIndices[1])
                      ##> message("station 1 pressure before setting yy: ",
                      ##>         paste(x@data$station[[1]]@data$pressure, collapse=" "))
                      yy <- rev(-swDepth(x@data$station[[stationIndices[1]]]@data$pressure))
                      ##> message("CHECK(section.R:1028) p: ", paste(x@data$station[[1]]@data$pressure, " "), " (should be independent of variable plotted)")
                  }
              } else {
                  stop("unknown ytype")
              }
              #> oceDebug(debug, vectorShow(yy))
              #> message("station 1 pressure: ", paste(x@data$station[[1]]@data$pressure, collapse=" "))
              par(mgp=mgp, mar=mar)
              if (lw > 1) {
                  if (lw > 2)
                      layout(matrix(1:4, nrow=2, byrow=TRUE))
                  else
                      layout(matrix(1:2, nrow=2, byrow=TRUE))
              }
              ## dataNames <- names(x[["station", 1]][["data"]])
              L <- if (getOption("oceUnitBracket") == "[") " [" else " ("
              R <- if (getOption("oceUnitBracket") == "[")  "]" else  ")"
              available <- sort(unique(c("data", "map", unlist(c(x[["?"]][c("data", "dataDerived")])))))
              for (w in 1:lw) {
                  oceDebug(debug, "handling which[", w, "]=\"", which[w], "\"\n", sep="")
                  if (!which[w] %in% available)
                      stop("in plot(section) : which='", which[w], "' is not available; please try one of c(\"",
                          paste(available, collapse="\",\""),
                          "\")", call.=FALSE)
                  station1 <- x[["station", 1]]
                  #OLD unit <- station1[[paste(which[w], "Unit", sep="")]][[1]] # FIXME: what if not in that station?
                  if (!missing(contourLevels)) {
                      # contourLevels given
                      oceDebug(debug, "contourLevels was given\n")
                      #> cat(vectorShow(contourLabels))
                      if (is.null(contourLabels))
                          contourLabels <- format(contourLevels)
                      vtitle <- labelWithUnit(which[w],
                          unit=station1[[paste0(which[w],"Unit")]])
                      plotSubsection(xx, yy, zz,
                          which.xtype=which.xtype,
                          which.ytype=which.ytype,
                          variable=which[w], # which[w],
                          vtitle=if (is.null(legend.text[w])) vtitle else legend.text[w],
                          eos=eos,
                          levels=contourLevels, labels=contourLabels,
                          xlim=xlim, ylim=ylim, ztype=ztype,
                          axes=axes, col=col, debug=debug-1, ...)
                  } else {
                      # contourLevels not given
                      oceDebug(debug, "contourLevels was not given\n")
                      if (which[w] != "map" && which[w] != 99) {
                          vtitle <- labelWithUnit(which[w],
                              unit=station1[[paste0(which[w],"Unit")]])
                          plotSubsection(xx, yy, zz,
                              which.xtype=which.xtype,
                              which.ytype=which.ytype,
                              variable=which[w],
                              vtitle=if (is.null(legend.text[w])) vtitle else legend.text[w],
                              eos=eos,
                              xlim=xlim, ylim=ylim, ztype=ztype,
                              zbreaks=zbreaks, zcol=zcol,
                              axes=axes, col=col, debug=debug-1, ...)
                      }
                  }
                  if (!is.na(which[w]) && which[w] == 20)
                      plotSubsection(xx, yy, zz,
                          which.xtype=which.xtype, which.ytype=which.ytype,
                          variable="data", vtitle="", unit=NULL,
                          xlim=xlim, ylim=ylim, col=col, legend=FALSE,
                          debug=debug-1, ...)
                  if (!is.na(which[w]) && (which[w] == 99 || which[w] == "map")) {
                      plotSubsection(xx, yy, zz,
                          which.xtype=which.xtype, which.ytype=which.ytype,
                          variable="map", vtitle="", unit=NULL,
                          indicate.stations=FALSE,
                          clongitude=clongitude, clatitude=clatitude, span=span,
                          projection=projection,
                          debug=debug-1, ...)
                  }
              }
              oceDebug(debug, "} # plot.section()\n", unindent=1)
              invisible(res)
          })


#' Read a Section File
#'
#' Read a file that contains a series of `ctd` profiles that make up an
#' oceanographic section.
#' Only *exchange BOT* comma-separated value format is permitted at this time,
#' but other formats may be added later.  It should also be noted that the parsing
#' scheme was developed after inspection of the A03 data set (see Examples). This
#' may cause problems if the format is not universal. For example, the header must
#' name the salinity column "`CTDSAL`"; if not, salinity values will not be
#' read from the file.
#'
#' @section Disambiguating salinity:
#' WOCE datasets commonly have a column named `CTDSAL` for salinity inferred
#' from a CTD and `SALNTY` (not a typo) for salinity derived from bottle data.
#' If only one of these is present in the data file, the data will be called
#' `salinity` in the `data` slot of the return value. However, if both
#' are present, then `CTDSAL` is stored as `salinity` and `SALNTY`
#' is stored as `salinityBottle`.
#'
#' @param file A file containing a set of CTD observations.  At present, only the
#' *exchange BOT* format is accepted (see \sQuote{Details}).
#'
#' @param directory A character string indicating the name of a  directory that
#' contains a set of CTD files that hold individual stations in the section.
#'
#' @param sectionId Optional string indicating the name for the section.  If not
#' provided, the section ID is determined by examination of the file header.
#'
#' @param ship Name of the ship carrying out the sampling.
#'
#' @param scientist Name of chief scientist aboard ship.
#'
#' @param institute Name of chief scientist's institute.
#'
#' @param flags Ignored, and deprecated (will be disallowed in a future version).
#'
#' @param missingValue Numerical value used to indicate missing data.
#'
#' @template debugTemplate
#'
#' @param processingLog If provided, the action item to be stored in the log.  This
#' is typically only provided for internal calls; the default that it provides is
#' better for normal calls by a user.
#'
#' @return A [section-class] object.
#'
#' @references
#' Several repository sites provide section data. A reasonably stable example is
#' \url{https://cchdo.ucsd.edu}, but a search on \code{"WOCE bottle data"} should
#' turn up other sites, if this ceases to exist. Only
#' the so-called *exchange BOT* data format can be processed by [read.section()]
#' at this time. Data names are inferred from column headings using
#' [woceNames2oceNames()].
#'
#' @author Dan Kelley
#'
#' @family things related to section data
read.section <- function(file, directory, sectionId="", flags,
    ship="", scientist="", institute="",
    missingValue=-999,
    debug=getOption("oceDebug"), processingLog)
{
    if (!missing(file) && is.character(file) && 0 == file.info(file)$size)
        stop("empty file")
    oceDebug(debug, "read.section(file=\"", file, "\", ...) {\n", unindent=1)
    if (!missing(directory)) {
        if (!missing(file))
            stop("cannot specify both 'file' and 'directory'")
        files <- list.files(directory)
        nstations <- length(files)
        stations <- vector("list", nstations)
        for (i in seq_along(files)) {
            name <- paste(directory, files[i], sep='/')
            stations[[i]] <- ctdTrim(read.oce(name))
        }
        return(as.section(stations))
    }
    if (is.character(file)) {
        filename <- file
        file <- file(file, "r")
        on.exit(close(file))
    }
    if (!inherits(file, "connection")) {
        stop("argument `file' must be a character string or connection")
    }
    res <- new("section")
    if (!isOpen(file)) {
        filename <- "(connection)"
        open(file, "r")
        on.exit(close(file))
    }
    if (!missing(flags))
        warning("'flags' is ignored, and will be disallowed in an upcoming CRAN release")
    ##>     ## flag=2 for good data [WOCE]
    ##>     if (missing(flags))
    ##>         flags <- c(2)
    # Skip header
    lines <- readLines(file)
    if ("BOTTLE" != substr(lines[1], 1, 6))
        stop("only type \"BOTTLE\" understood, but got header line\n", lines[1], "\n")
    if (nchar(sectionId) < 1)
        sectionId <- substr(lines[1], 8, nchar(lines[1]))
    n <- length(lines)
    header <- lines[1]
    for (l in (2:n)) {
        oceDebug(debug>4, lines[l], "\n")
        if ("#" != substr(lines[l], 1, 1)) {
            header <- c(header, lines[l])
            break
        }
    }
    header.length <- l + 1
    ccc <- textConnection(lines[header.length - 1])
    var.names <- scan(ccc, sep=",", what="", quiet=TRUE)
    dataNamesOriginal <- var.names
    close(ccc)
    ccc <- textConnection(lines[header.length])
    var.units <- scan(ccc, sep=",", what="", quiet=TRUE)
    close(ccc)
    if (length(var.units) != length(var.names))
        stop("length mismatch in variable lists")
    header <- lines[1:header.length]
    nd <- n - header.length - 1
    nv <- length(var.names)
    col.start <- 3                     # FIXME: why is this value used?  It fails on some (Arctic) data.
    col.start <- 1
    data <- array(dim=c(nd, nv - col.start + 1))
    flags <- list()
    stnSectionId <- vector("character", nd)
    stnId <- vector("character", nd)
    for (l in ( (header.length + 1):(n-1)) ) {
        ## last line is END_DATA
        contents <- strsplit(lines[l], split=",")[[1]]
        stnSectionId[l - header.length] <- sub(" *", "", contents[2])
        stnId[l - header.length] <- sub("^ *", "", contents[3])
        data[l - header.length, ] <- contents[col.start:nv]
        ## FIXME: maybe should just scan this thing; it might work better anyway
    }
    ## salinityUnit <- NULL
    ## if (1 == length(w <- which(var.names=="CTDPRS"))) {
    ##     pressure <- as.numeric(data[, w - col.start + 1])
    ##     pressureUnit <- as.unit(var.units[w], list(unit=expression(dbar), scale=""))
    ##     if (1 == length(wf <- which(var.names=="CTDPRS_FLAG_W")))
    ##         flags$pressure <- as.numeric(data[, wf - col.start + 1])
    ## } else stop("no column named \"CTDPRS\"")
    ## if (1==length(w <- which(var.names=="CTDTMP"))) {
    ##     temperature <- as.numeric(data[, w - col.start + 1])
    ##     temperatureUnit <- as.unit(var.units[w], list(unit=expression(degree*C), scale="ITS-90"))
    ##     if (1 == length(wf <- which(var.names=="CTDTMP_FLAG_W")))
    ##         flags$temperature <- as.numeric(data[, wf - col.start + 1])
    ## } else stop("no column named \"CTDTMP\"")

    ## ## oceDebug(debug, "var.names=", paste(var.names, sep=","), "\n")
    ## ## haveSalinity <- FALSE
    ## ## salinityUnit <- NULL
    ##
    ## ## For salinity, use CTDSAL if it exists, otherwise use 'SALNTY', if it exists. If
    ## ## both exist, store SALNTY as 'salinityBottle'.
    ## haveTwoSalinities <- length(which(var.names=="CTDSAL")) && length(which(var.names=="CTDSAL"))
    ## salinityBottle <- NULL # so we can check later
    ## if (1 == length(w <- which(var.names=="CTDSAL"))) {
    ##     haveSalinity <- TRUE
    ##     salinity <- as.numeric(data[, w - col.start + 1])
    ##     salinityUnit <- as.unit(var.units[w], list(unit=expression(), scale="PSS-78"))
    ##     if (1 == length(wf <- which(var.names=="CTDSAL_FLAG_W")))
    ##         flags$salinity <- as.numeric(data[, wf - col.start + 1])
    ## }
    ## if (1 == length(w <- which(var.names=="SALNTY"))) { # spelling not a typo
    ##     haveSalinity <- TRUE
    ##     if (haveTwoSalinities) {
    ##         salinityBottle <- as.numeric(data[, w - col.start + 1])
    ##         salinityBottleUnit <- as.unit(var.units[w], list(unit=expression(), scale="PSS-78"))
    ##         if (1 == length(wf <- which(var.names=="SALNTY_FLAG_W")))
    ##             flags$salinityBottle <- as.numeric(data[, wf - col.start + 1])
    ##     } else {
    ##         salinity <- as.numeric(data[, w - col.start + 1])
    ##         salinityUnit <- as.unit(var.units[w], list(unit=expression(), scale="PSS-78"))
    ##         if (1 == length(wf <- which(var.names=="SALNTY_FLAG_W")))
    ##             flags$salinity <- as.numeric(data[, wf - col.start + 1])
    ##     }
    ## }
    ## if (!haveSalinity) stop("no column named \"CTDSAL\" or \"SALNTY\"")
    if (length(which(var.names=="DATE")))
        stn.date <- as.character(data[, which(var.names=="DATE") - col.start + 1])
    else
        stop("no column named \"DATE\"")
    if (length(which(var.names=="TIME")))
        stn.time <- as.character(data[, which(var.names=="TIME") - col.start + 1])
    else
        stop("no column named \"TIME\"")
    ## nolint start (long lines)

    ## EXPOCODE,SECT_ID,STNNBR,CASTNO,SAMPNO,BTLNBR,BTLNBR_FLAG_W,DATE,TIME,LATITUDE,LONGITUDE,DEPTH,CTDPRS,CTDTMP,CTDSAL,CTDSAL_FLAG_W,SALNTY,SALNTY_FLAG_W,OXYGEN,OXYGEN_FLAG_W,SILCAT,SILCAT_FLAG_W,NITRIT,NITRIT_FLAG_W,NO2+NO3,NO2+NO3_FLAG_W,PHSPHT,PHSPHT_FLAG_W

    ## nolint end (long lines)

    ## oxygenUnit <- NULL
    ## if (1 == length(w <- which(var.names=="OXYGEN"))) {
    ##     oxygen <- as.numeric(data[, w - col.start + 1])
    ##     oxygen[oxygen == missingValue] <- NA
    ##     oxygenUnit <- as.unit(var.units[w], list(unit=expression(mu*mol/kg), scale=""))
    ##     if (1 == length(wf <- which(var.names=="OXYGEN_FLAG_W")))
    ##         flags$oxygen <- as.numeric(data[, wf - col.start + 1])
    ## } else oxygen <- NULL
    ## silicateUnit <- NULL
    ## if (1 == length(w <- which(var.names=="SILCAT"))) {
    ##     silicate <- as.numeric(data[, w - col.start + 1])
    ##     silicate[silicate == missingValue] <- NA
    ##     silicateUnit <- as.unit(var.units[w], list(unit=expression(mu*mol/kg), scale=""))
    ##     if (1 == length(wf <- which(var.names=="SILCAT_FLAG_W")))
    ##         flags$silicate <- as.numeric(data[, wf - col.start + 1])
    ## } else silicate <- NULL
    ## nitriteUnit <- NULL
    ## if (1 == length(w <- which(var.names=="NITRIT"))) {
    ##     nitrite <- as.numeric(data[, w - col.start + 1])
    ##     nitrite[nitrite == missingValue] <- NA
    ##     nitriteUnit <- as.unit(var.units[w], expression(unit=expression(mu*mol/kg), scale=""))
    ##     if (1 == length(wf <- which(var.names=="NITRIT_FLAG_W")))
    ##         flags$nitrite <- as.numeric(data[, wf - col.start + 1])
    ## } else nitrite <- NULL
    ## nitrateUnit <- NULL
    ## if (1 == length(w <- which(var.names=="NITRAT"))) {
    ##     nitrate <- as.numeric(data[,which(var.names=="NITRAT") - col.start + 1])
    ##     nitrate[nitrate == missingValue] <- NA
    ##     nitrateUnit <- as.unit(var.units[w], expression(unit=expression(mu*mol/kg), scale=""))
    ##     if (1 == length(wf <- which(var.names=="NITRAT_FLAG_W")))
    ##         flags$nitrate <- as.numeric(data[, wf - col.start + 1])
    ## } else nitrate <- NULL
    ## haveNO2plusNO3 <- FALSE
    ## if (1 == length(w <- which(var.names=="NO2+NO3"))) {
    ##     haveNO2plusNO3 <- TRUE
    ##     no2plusno3 <- as.numeric(data[, w - col.start + 1])
    ##     no2plusno3[no2plusno3 == missingValue] <- NA
    ##     if (is.null(nitrate)) {
    ##         nitrate <- no2plusno3 - nitrite
    ##         rm(no2plusno3)
    ##     } else {
    ##         if (is.null(nitrite)) {
    ##             nitrite <- no2plusno3 - nitrate
    ##             rm(no2plusno3)
    ##         } else {
    ##             warning("cannot determine nitrate and nitrite")
    ##             nitrite <- nitrate <- NULL
    ##         }
    ##     }
    ##     if (1 == length(wf <- which(var.names=="NO2+NO3_FLAG_W")))
    ##         flags$nitrate <- as.numeric(data[, wf - col.start + 1])
    ##     ## http://woce.nodc.noaa.gov/woce_v3/wocedata_1/whp/exchange/exchange_format_desc.htm
    ## }
    ## phosphateUnit <- NULL
    ## if (1 == length(w <- which(var.names=="PHSPHT"))) {
    ##     phosphate <- as.numeric(data[, w - col.start + 1])
    ##     phosphate[phosphate == missingValue] <- NA
    ##     phosphateUnit <- as.unit(var.units[w], expression(unit=expression(mu*mol/kg), scale=""))
    ##     if (1 == length(wf <- which(var.names=="PHSPHT_FLAG_W")))
    ##         flags$phosphate  <- as.numeric(data[, wf - col.start + 1])
    ## } else phosphate <- NULL
    waterDepth  <- as.numeric(data[, which(var.names=="DEPTH") - col.start + 1])
    waterDepth <- ifelse(waterDepth == missingValue, NA, waterDepth)
    ## FIXME: we have both 'latitude' and 'lat'; this is too confusing
    longitude <- as.numeric(data[, which(var.names=="LONGITUDE") - col.start + 1])
    latitude  <- as.numeric(data[, which(var.names=="LATITUDE") - col.start + 1])
    stationId <- data[, which(var.names=="STNNBR") - col.start + 1]
    stationId <- sub(" *$", "", sub("^ *", "", stationId)) #remove blanks
    stationList <- unique(stationId)
    numStations <- length(stationList)
    station <- vector("list", numStations)
    stn <- vector("character", numStations)
    lon <- vector("numeric", numStations)
    lat <- vector("numeric", numStations)
    time <- vector("numeric", numStations) # has to be numeric
    ## tref <- as.POSIXct("2000-01-01 00:00:00", tz="UTC")
    ## trefn <- as.numeric(tref)


    ## We will trim out metadata columns, in assembling the 'data' slot of
    ## the ctd objects that will make up the section. The pattern below is based
    ## on one particular file (provided with oce in inst/extdata), which has
    ## the following column names.
    ## "EXPOCODE" "SECT_ID" "STNNBR" "CASTNO" "SAMPNO" "BTLNBR" "BTLNBR_FLAG_W"
    ## "DATE" "TIME" "LATITUDE" "LONGITUDE" "DEPTH" "CTDPRS" "CTDTMP"
    ## "CTDSAL" "CTDSAL_FLAG_W" "SALNTY" "SALNTY_FLAG_W" "OXYGEN" "OXYGEN_FLAG_W"
    colSkip <- var.names %in% c("EXPOCODE", "SECT_ID", "STNNBR", "CASTNO", "SAMPNO",
                                "BTLNBR", "BTLNBR_FLAG_W",
                                "DATE", "TIME", "LATITUDE", "LONGITUDE", "DEPTH")
    dataNamesOriginal <- as.list(var.names[!colSkip])
    #dataNamesOriginal <- var.names[!colSkip]
    dataNames <- woceNames2oceNames(var.names)[!colSkip]
    names(dataNamesOriginal) <- dataNames
    dataUnits <- list()
    for (idata in seq_along(dataNames)) {
        n <- dataNames[idata]
        dataUnits[[dataNames[idata]]] <- unitFromString(var.units[!colSkip][idata])
    }
    ## print(data.frame(dataNames, dataNamesOriginal))
    ## print(dataUnits)

    ## print(data.frame(oceNames, dataNamesOriginal))
    ## Names and units are the same for every station, so determine them
    ## before going through the data.
    for (i in 1:numStations) {
        select <- which(stationId == stationList[i])
        ## "199309232222"
        ## "1993-09-23 22:22:00"
        time[i] <- as.numeric(strptime(paste(stn.date[select[1]], stn.time[select[1]], sep=""), "%Y%m%d%H%M", tz="UTC"))
        stn[i] <- sub("^ *", "", stationId[select[1]])
        oceDebug(debug, "reading station i=", i, ", stn=", stn[i], "\n")
        lon[i] <- longitude[select[1]]
        lat[i] <- latitude[select[1]]
        ## ## FIXME: chop flags up
        ## flagsSelected <- flags
        ## for (name in names(flagsSelected)) {
        ##     flagsSelected[[name]] <- flags[[name]][select]
        ## }
        ## ##> message("flagsSelected:"); str(flagsSelected)
        ## thisStation <- as.ctd(salinity=salinity[select],
        ##                     temperature=temperature[select],
        ##                     pressure=pressure[select],
        ##                        oxygen=if(!is.null(oxygen))oxygen[select],
        ##                        nitrate=if(!is.null(nitrate))nitrate[select],
        ##                        nitrite=if(!is.null(nitrite))nitrite[select],
        ##                        phosphate=if(!is.null(phosphate))phosphate[select],
        ##                        silicate=if(!is.null(silicate))silicate[select],
        ##                        flags=flagsSelected,
        ##                     ship=ship,
        ##                     startTime=numberAsPOSIXct(time[i]),
        ##                     scientist=scientist,
        ##                     institute=institute,
        ##                     longitude=lon[i], latitude=lat[i],
        ##                     cruise=stnSectionId[select[1]],
        ##                     station=stn[i],
        ##                     waterDepth=waterDepth[select[1]],
        ##                     src=filename)
        select <- which(stationId == stationList[i])
        thisStation <- new("ctd")
        thisStation@data <- list() # start over, then insert one by one
        ## colNames <- oceNames[!colSkip]
        DATA <- data[, !colSkip]
        isFlag <- rep(TRUE, sum(!colSkip))
        for (idata in seq_along(dataNames)) {
            ## Split flags into metadata
            isFlag[idata] <- grepl("Flag$", dataNames[idata])
            if (isFlag[idata]) {
                thisStation@metadata$flags[[gsub("Flag$", "", dataNames[idata])]] <- as.numeric(DATA[select, idata])
            } else {
                ## message("colNames[", idata, "]: ", colNames[idata])
                tmp <- as.numeric(DATA[select, idata])
                tmp[tmp == missingValue] <- NA
                thisStation@data[[dataNames[idata]]] <- tmp
            }
        }
        ##thisStation@metadata$names <- dataNames[!isFlag]
        ##thisStation@metadata$labels <- dataNames[!isFlag]
        ##thisStation@metadata$dataNamesOriginal <- dataNamesOriginal[!isFlag]
        thisStation@metadata$dataNamesOriginal <- dataNamesOriginal
        thisStation@metadata$src <- filename
        thisStation@metadata$startTime <- numberAsPOSIXct(time[i])
        thisStation@metadata$longitude <- lon[i]
        thisStation@metadata$latitude <- lat[i]
        thisStation@metadata$stn <- sub("^ *", "", stationId[select[1]])
        thisStation@metadata$time <- as.numeric(strptime(paste(stn.date[select[1]], stn.time[select[1]], sep=""), "%Y%m%d%H%M", tz="UTC"))
        thisStation@metadata$station <- sub("^ *", "", stationId[select[1]])
        thisStation@metadata$longitude <- longitude[select[1]]
        thisStation@metadata$latitude <- latitude[select[1]]
        thisStation@metadata$waterDepth <- waterDepth[select[1]]
        thisStation@metadata$units <- dataUnits
        ## if (length(salinityBottle)) {
        ##     thisStation@metadata$units$salinityBottle <- salinityBottleUnit
        ##     thisStation@data$salinityBottle <- salinityBottle[select]
        ## }
        ## 20160504: I no longer think we should store made-up columns; an accessor
        ## 20160504: could use the following code, though, so I'll keep it here, commented-out.
        ## 20160504: ## Nitrite and Nitrate are tricky since they can be contained
        ## 20160504: ## in the file individually or in combination, with a column
        ## 20160504: ## that is the sum of NO2 and NO3.
        ## 20160504: if (haveNO2plusNO3) {
        ## 20160504:     if (is.null(nitriteUnit)) {
        ## 20160504:         if (!is.null(nitrateUnit)) {
        ## 20160504:             thisStation@metadata$units$nitrate <- nitrateUnit
        ## 20160504:             thisStation@metadata$units$nitrite <- nitrateUnit
        ## 20160504:         }
        ## 20160504:     } else {
        ## 20160504:         if (!is.null(nitriteUnit)) {
        ## 20160504:             thisStation@metadata$units$nitrate <- nitriteUnit
        ## 20160504:             thisStation@metadata$units$nitrite <- nitriteUnit
        ## 20160504:         }
        ## 20160504:     }
        ## 20160504: } else {
        ## 20160504:     if (!is.null(nitrateUnit)) thisStation@metadata$units$nitrate <- nitrateUnit
        ## 20160504:     if (!is.null(nitriteUnit)) thisStation@metadata$units$nitrite <- nitriteUnit
        ## 20160504: }
        ## if (!is.null(oxygenUnit)) thisStation@metadata$units$oxygen <- oxygenUnit
        ## if (!is.null(silicateUnit)) thisStation@metadata$units$silicate <- silicateUnit
        ## if (!is.null(phosphateUnit)) thisStation@metadata$units$phosphate <- phosphateUnit
        oceDebug(debug, "    ", length(select), " levels ", lat[i], "N,", lon[i], "W", " (", format(thisStation@metadata$startTime), ")\n")
        station[[i]] <- thisStation
    }
    res@metadata$header <- header
    res@metadata$sectionId <- sectionId
    res@metadata$stationId <- stn
    res@metadata$longitude <- lon
    res@metadata$latitude <- lat
    res@metadata$time <- numberAsPOSIXct(time)
    res@metadata$filename <- filename
    res@data <- list(station=station)
    if (missing(processingLog))
        processingLog <- paste(deparse(match.call()), sep="", collapse="")
    ##hitem <- processingLogItem(processingLog)
    if (missing(processingLog))
        processingLog <- paste(deparse(match.call()), sep="", collapse="")
    res@processingLog <- processingLogAppend(res@processingLog, processingLog)
    oceDebug(debug, "} # read.section()\n", unindent=1)
    res
}

#' Grid a Section in Pressure Space
#'
#' Grid a section, by interpolating to fixed pressure levels.  The
#' `"approx"`, `"boxcar"` and `"lm"` methods are described in the
#' documentation for [ctdDecimate()], which is used to do this
#' processing.
#'
#' The default `"approx"` method is best for bottle data, the
#' `"boxcar"` is best for ctd data, and the `"lm"` method is probably
#' too slow to recommend for exploratory work, in which it is common to do trials
#' with a variety of `"p"` values.
#'
#' The stations in the returned value have flags with names that match those
#' of the corresponding stations in the original `section`, but the values
#' of these flags are all set to `NA`. This recognizes that it makes
#' no sense to grid flag values, but that there is merit in initializing
#' a flag system, for possible use in later processing steps.
#'
#' @param section A `section` object containing the section to be gridded.
#'
#' @param p Optional indication of the pressure levels to which interpolation
#' should be done.  If this is not supplied, the pressure levels will be
#' calculated based on the typical spacing in the ctd profiles stored within
#' `section`.  If `p="levitus"`, then pressures will be set to be those
#' of the Levitus atlas, given by [standardDepths()].
#' If `p` is a single numerical value,
#' it is taken as the number of subdivisions to use in a call to [seq()]
#' that has range from 0 to the maximum pressure in `section`.  Finally, if a
#' vector numerical values is provided, perhaps. constructed with [seq()]
#' or [`standardDepths`]`(5)` (as in the examples),
#' then it is used as is, after trimming any values that exceed the maximum
#' pressure in the station data stored within `section`.
#'
#' @param method The method to use to decimate data within the stations; see
#' [ctdDecimate()], which is used for the decimation.
#'
#' @param trim Logical value indicating whether to trim gridded pressures
#' to the range of the data in `section`.
#'
#' @template debugTemplate
#'
#' @param ... Optional arguments to be supplied to [ctdDecimate()],
#' e.g. `rule` controls extrapolation beyond the observed pressure range,
#' in the case where `method` equals `"approx"`.
#'
#' @return A [section-class] object that contains stations whose
#' pressure values match identically, and that has all flags set to `NA`.
#'
#' @examples
#' # Gulf Stream
#' library(oce)
#' data(section)
#' GS <- subset(section, 109<=stationId&stationId<=129)
#' GSg <- sectionGrid(GS, p=seq(0, 5000, 100))
#' plot(GSg, map.xlim=c(-80,-60))
#' # Show effects of various depth schemes
#' par(mfrow=c(3, 1))
#' default <- sectionGrid(GS)
#' approxML <- sectionGrid(GS, method="approxML")
#' standardDepths5 <- sectionGrid(GS, p=standardDepths(5))
#' plot(default, which="temperature", ztype="image", ylim=c(200,0))
#' mtext("default sectionGrid()")
#' plot(approxML, which="temperature", ztype="image", ylim=c(200,0))
#' mtext("sectionGrid(..., method=\"approxML\")")
#' plot(standardDepths5, which="temperature", ztype="image", ylim=c(200,0))
#' mtext("sectionGrid(..., p=standardDepths(5))")
#'
#' @author Dan Kelley
#'
#' @family things related to section data
sectionGrid <- function(section, p, method="approx", trim=TRUE, debug=getOption("oceDebug"), ...)
{
    oceDebug(debug, "sectionGrid(section, p, ",
             "method=\"", if (is.function(method)) "(function)" else method, "\", ",
             "p=", if (missing(p)) "(missing)" else paste("c(",paste(p, collapse=","),")", sep=""), ", ",
             "trim=", trim, ", ...) {\n",
             sep="", unindent=1)
    ##warningMessages <- NULL
    n <- length(section@data$station)
    nxg <- n
    oceDebug(debug, "have ", nxg, " stations in this section\n")
    pMax <- max(section[["pressure"]], na.rm=TRUE)
    if (missing(p)) {
        dplist <- vector("numeric", n)
        ## p.max <- 0
        for (i in 1:n) {
            p <- section@data$station[[i]]@data$pressure
            dplist[i] <- mean(diff(p), na.rm=TRUE)
        }
        dp <- mean(dplist, na.rm=TRUE) / 5 # make it a little smaller
        pt <- pretty(c(0, pMax), n=min(200, floor(abs(pMax / dp))))
        oceDebug(debug, "p not given, so inferring from data\n")
        ## oceDebug(debug, "pMax=", pMax, "; dp=", dp, "\n")
        ## oceDebug(debug, "pt=", pt, "\n")
        ## oceDebug(debug, "length(pt)=", length(pt), "\n")
    } else {
        if (length(p) == 1) {
            if (p=="levitus") {
                pt <- standardDepths()
            } else {
                if (!is.numeric(p))
                    stop("p must be numeric")
                if (p <= 0)
                    stop("p must be a positive number")
                pt <- seq(0, pMax, p)
            }
        } else {
            pt <- p
        }
    }
    oceDebug(debug, vectorShow(pt))
    if (trim) {
        ## allow one extra level, for bracketing
        w <- which(pt > pMax)
        if (length(w)) {
            pt <- pt[1:w[1]]
        }
        oceDebug(debug, "trimmed ", vectorShow(pt))
    }
    ## BUG should handle all variables (but how to interpolate on a flag?)
    res <- section
    ##OLD npt <- length(pt)
    nstation <- length(res[["station"]])
    nyg <- length(pt)

    ## The core of the work -- use ctdDecimate() on each station
    for (i in seq_len(nstation)) {
        oceDebug(debug, "decimating station i=", i, "\n")
        suppressWarnings(
            res@data$station[[i]] <- ctdDecimate(section@data$station[[i]],
                p=pt, method=method, debug=debug-1, ...))
    }
    ## Find all units in *any* station, and then insert them into *all* stations.
    units <- list()
    for (i in seq_len(nstation)) {
        for (unitName in names(section@data$station[[i]]@metadata$units)) {
            if (!(unitName %in% names(units))) {
                units[unitName] <- section@data$station[[i]]@metadata$units[unitName]
            }
        }
    }
    for (i in seq_len(nxg)) {
        res@data$station[[i]]@metadata$units <- units
        res@data$station[[i]]@metadata$flags <- list()
        for (flagname in names(section@data$station[[i]]@metadata$flags)) {
            ## Note that the flags are named after those of the flags for *that particular*
            ## station in the input object. This is different from sectionSmooth().
            ##> message("sectionGrid ... i ", i, " (nxg=", nxg, "), flagname '", flagname, "'")
            res@data$station[[i]]@metadata$flags[[flagname]] <- rep(NA, nyg)
        }
    }
    res@processingLog <- processingLogAppend(res@processingLog, paste(deparse(match.call()), sep="", collapse=""))
    ##for (w in warningMessages)
    ##    res@processingLog <- processingLogAppend(res@processingLog, w)
    oceDebug(debug, "} # sectionGrid\n", unindent=1)
    res
}


#' Smooth a Section
#'
#' Smooth a section, in any of several ways, working either in the vertical
#' direction or in both the vertical and lateral directions.
#'
#' This function produces smoothed fields that might be useful in
#' simplifying graphical elements created with [plot,section-method()].
#' As with any smoothing method, a careful analyst will compare the results
#' against the raw data, e.g. using [plot,section-method()].
#' In addition the problem of falsely widening narrow features such as
#' fronts, there is also the potential to get unphysical results with
#' spars sampling near topographic features such as bottom slopes and ridges.
#'
#' The `method` argument selects between three possible methods.
#'
#' * For `method="spline"`, i.e. the default, the section is smoothed
#' laterally, using [smooth.spline()] on individual pressure levels.
#' (If the pressure levels do not match up, [sectionGrid()] should
#' be used first to create a `section` object that can be used here.)
#' The `df` argument sets the degree of freedom of the spline, with
#' larger values indicating less smoothing.
#'
#' * For the (much slower) `method="barnes"` method, smoothing is done across
#' both horizontal and vertical coordinates, using [interpBarnes()].
#' The output station locations are computed by linear interpolation of
#' input locations, using [approx()] on the original
#' longitudes and longitudes of stations, with the independent variable
#' being the distance along the track, computed with [geodDist()].
#' The values of `xg`, `yg`, `xgl` and `ygl` control
#' the smoothing.
#'
#' * If `method` is a function, then that function is applied to
#' the (distance, pressure) data for each variable at a grid defined by
#' `xg`, `xgl`, `yg` and `ygl`. The function must
#' be of the form `function(x,y,F,xg,xr,yg,yr)`, and must
#' return a matrix of the gridded result, with first index indicating
#' the "grid" station number and second index indicating "grid" pressure.
#' The `x` value that is supplied to this function is set as
#' the distance along the section, as computed with [geodDist()],
#' and repeated for each of the points at each station.  The corresponding
#' pressures are provided in `y`, and the value to be gridded is
#' in `v`, which will be `temperture` on one call to the function,
#' `salinity` on another call, etc. The other quantities
#' have the meanings as described below.  See the \dQuote{Examples}
#' for a description of how to set up and use a function for the gridding
#' method known as Kriging.
#'
#'
#' @param section A `section` object containing the section to be smoothed.
#' For `method="spline"`, the pressure levels must match for each station in
#' the section.
#'
#' @param method A string or a function that specifies the method to use; see \sQuote{Details}.
#'
#' @param x Optional numerical vector, of the same length as the number of stations in `section`,
#' which will be used in gridding in the lateral direction. If not provided, this
#' defaults to [`geodDist`]`(section)`.
#'
#' @param xg,xgl ignored in the `method="spline"` case, but passed to
#' [interpBarnes()] if `method="barnes"`, to kriging
#' functions if `method="kriging"`, or to `method` itself, if it
#' is a function.
#' If `xg` is supplied, it defines the x component of the grid, which by
#' default is the terms of station distances, x, along the track of the section. (Note
#' that the grid `xg` is trimmed to the range of the data `x`, because otherwise
#' it would be impossible to interpolate between stations to infer water depth,
#' longitude, and latitude, which are all stored within the stations in the
#' returned `section` object.)
#' Alternatively, if `xgl` is supplied, the x grid is established using [seq()],
#' to span the data with `xgl` elements. If neither of these is supplied, the output
#' x grid will equal the input x grid.
#'
#' @param yg,ygl similar to `xg` and `xgl`, but for pressure. (Note that
#' trimming to the input `y` is not done, as it is for `xg` and `x`.)
#" If `yg` is not given, it is determined from the deepest station in the section.
#' If `ygl` was not given, then a grid is constructed to span the pressures
#' of that deepest station with `ygl` elements. On the other hand,
#' if `ygl` was not given, then the y grid will constructed from the
#' pressure levels in the deepest station.
#'
#' @param xr,yr influence ranges in x (along-section distance) and y (pressure),
#' passed to [interpBarnes()] if `method="barnes"` or to
#' `method`, if the latter is a function. If missing, `xr` defaults to
#' 1.5X the median inter-station distance and `yr` defaults to 0.2X
#' the pressure range. Since these defaults have changed over the evolution
#' of `sectionSmooth`, analysts ought to supply `xr` and `yr`
#' in the function call, tailoring them to particular applications, and
#' making the code more resistant to changes in `sectionSmooth`.
#'
#' @param df Degree-of-freedom parameter, passed to [smooth.spline()]
#' if `method="spline"`, and ignored otherwise. If `df` is not provided,
#' it defaults to 1/5-th of the number of stations containing non-`NA`
#' data at the particular pressure level being processed, as `sectionSmooth`
#' works its way through the water column.
#'
#' @param gamma,iterations,trim,pregrid Values passed to
#' [interpBarnes()], if `method="barnes"`, and
#' ignored otherwise. `gamma` is the factor by which
#' `xr` and `yr` are reduced on each of succeeding iterations.
#' `iterations` is the number of iterations to do.
#' `trim` controls whether the gridded data are set to
#' `NA` in regions with sparse data
#' coverage. `pregrid` controls whether data are to be
#' pre-gridded with [binMean2D()] before being passed to
#' [interpBarnes()].
#'
#' @param debug A flag that turns on debugging.  Set to 1 to get a moderate amount
#' of debugging information, or to 2 to get more.
#'
#' @param ... Optional extra arguments, passed to either
#' [smooth.spline()], if `method="spline"`, and ignored otherwise.
#'
#' @return A [section-class] object of that has been smoothed in some way.
#' Every data field that is in even a single station of the input object
#' is inserted into every station in the returned value, and therefore
#' the units represent all the units in any of the stations, as do the
#' flag names. However, the flags are all set to `NA` values.
#'
#' @examples
#' # Unsmoothed (Gulf Stream)
#' library(oce)
#' data(section)
#' gs <- subset(section, 115<=stationId&stationId<=125)
#' par(mfrow=c(2, 2))
#'
#' plot(gs, which="temperature")
#' mtext("unsmoothed")
#'
#' # Spline
#' gsg <- sectionGrid(gs, p=seq(0, 5000, 100))
#' gsSpline <- sectionSmooth(gsg, "spline")
#' plot(gsSpline, which="temperature")
#' mtext("spline-smoothed")
#'
#' # Barnes
#' gsBarnes <- sectionSmooth(gs, "barnes", xr=50, yr=200)
#' plot(gsBarnes, which="temperature")
#' mtext("Barnes-smoothed")
#'
#' # Kriging. This works one day, fails another, on linux/R-devel CRAN
#' # machines, so it is marked as don't run.
#'\dontrun{
#'if (requireNamespace("automap",quietly=TRUE)&&requireNamespace("sp",quietly=TRUE)) {
#'  krig <- function(x, y, F, xg, xr, yg, yr) {
#'    xy <- data.frame(x=x/xr, y=y/yr)
#'    K <- automap::autoKrige(F~1, remove_duplicates=TRUE,
#'                            input_data=sp::SpatialPointsDataFrame(xy, data.frame(F)),
#'                            new_data=sp::SpatialPoints(expand.grid(xg/xr, yg/yr)))
#'    matrix(K$krige_output@data$var1.pred, nrow=length(xg), ncol=length(yg))
#'  }
#'  gsKrig <- sectionSmooth(gs, krig)
#'  plot(gsKrig, which="temperature")
#'  mtext("Kriging-smoothed")
#'}
#'}
#'
#' @author Dan Kelley
#'
#' @family things related to section data
sectionSmooth <- function(section, method="spline",
                          x,
                          xg, yg, xgl, ygl, xr, yr,
                          df, gamma=0.5, iterations=2, trim=0, pregrid=FALSE,
                          debug=getOption("oceDebug"), ...)
{
    if (!inherits(section, "section"))
        stop("method is only for objects of class '", "section", "'")
    if (!is.function(method) && !(is.character(method) && (method %in% c("barnes", "kriging", "spline"))))
        stop('method must be "barnes", "kriging", "spline", or an R function')
    ## pin debug, since we only call one function, interpBarnes() that uses debug
    debug <- if (debug > 2) 2 else if (debug < 0) 0 else debug
    oceDebug(debug, "sectionSmooth(section,method=\"",
             if (is.character(method)) method else "(function)", "\", ...) {\n", sep="", unindent=1)
    stations <- section[["station"]]
    nstn <- length(stations)
    if (nstn < 2) {
        warning("station has <2 stations, so sectionSmooth() is returning it, unaltered")
        return(x)
    }
    xrGiven <- !missing(xr)
    yrGiven <- !missing(yr)
    dfGiven <- !missing(df)
    if (missing(x))
        x <- geodDist(section)
    nx <- length(x)
    if (nx != length(section@data$station))
        stop("length(x)=", nx, " does not match number of stations=", length(section@data$station))
    ## Rearange the input stations into order set by x.
    o <- order(x)
    x <- x[o]
    section@data$station <- section@data$station[o]
    P <- section[["pressure"]]
    maxPressure <- max(P, na.rm=TRUE)
    if (missing(xg)) {
        xg <- if (missing(xgl)) x else seq(min(x), max(x), length.out=xgl)
        oceDebug(debug, "defaulted", vectorShow(xg))
    } else {
        oceDebug(debug, "user-supplied", vectorShow(xg))
    }
    if (missing(yg)) {
        deepest <- which.max(unlist(lapply(section[["station"]], function(ctd) max(ctd[["pressure"]], na.rm=TRUE))))
        yg <- if (missing(ygl)) section[["station", deepest]][["pressure"]] else seq(0, maxPressure, length.out=ygl)
        oceDebug(debug, "defaulted", vectorShow(yg))
    } else {
        oceDebug(debug, "user-supplied", vectorShow(yg))
    }
    ## Trim xg to the data range in x (issue 1583)
    oceDebug(debug, "original data", vectorShow(x))
    keep <- min(x) <= xg & xg <= max(x)
    oceDebug(debug, "before trimming", vectorShow(xg))
    xg <- xg[keep]
    oceDebug(debug, "after trimming", vectorShow(xg))
    stn1pressure <- stations[[1]][["pressure"]]
    if (identical(method, "spline") && !identical(yg, stn1pressure))
        stop("for method=\"spline\", yg must match the pressure vector in first station")
    nxg <- length(xg)
    nyg <- length(yg)

    ## varsAll holds the names of all variables in the section.
    res <- section
    varsAll <- unique(unlist(lapply(stations, function(ctd) names(ctd[["data"]]))))
    ## vars holds just the names of variables that get smoothed. First, we remove
    ## things that simply cannot be smoothed...
    vars <- varsAll[!varsAll %in% c("flag", "quality", "scan", "time")]
    ## ..., second, we remove things that will be computed from 'x' and 'xg'.
    vars <- vars[!vars %in% c("latitude", "longitude", "pressure")]
    ## ... finally, remove 'depth', which is kind of a surrogate for pressure, I think.
    vars <- vars[!vars %in% c("depth")]
    flagnames <- unique(unlist(lapply(stations, function(ctd) names(ctd@metadata$flags))))

    ## start with existing station, to get processing log, section ID, etc., but
    ## recreate @data$station
    res@data$station <- list("vector", nxg)
    for (istn in seq_len(nxg)) {
        res@data$station[[istn]] <- new('ctd')
        res@data$station[[istn]][["pressure"]] <- yg
    }
    if (is.character(method) && method == "spline") {
        oceDebug(debug, "using spline method\n")
        ## Since we are smoothing along lines of constant pressure, we must
        ## first ensure that the stations have identical pressures.
        npressure <- length(stn1pressure)
        for (istn in 2:nstn) {
            thisp <- stations[[istn]][["pressure"]]
            if (length(thisp) != npressure || any(thisp != stn1pressure))
                stop("pressure mismatch between station 1 and station", istn, "; try using sectionGrid() first")
        }
        ## The work will be done in matrices, for code clarity and speed.
        VAR <- array(double(), dim=c(nstn, npressure))
        oceDebug(debug, "dim matrix for input grid=", paste(dim(VAR), collapse="X"), "\n")
        for (var in vars) {
            ##? res@data$station[[istn]][[var]] <- rep(NA, npressure)
            oceDebug(debug, "smoothing '", var, "'\n", sep="")
            for (istn in seq_len(nx)) {
                ##message("istn=", istn)
                VAR[istn, ] <- if (var %in% names(stations[[istn]][["data"]])) stations[[istn]][[var]] else rep(NA, npressure)
            }
            ## Smooth at each pressure value (turn off warnings because smooth.spline is confusingly chatty)
            owarn <- options('warn')$warn
            options(warn=-1)
            VARS <- apply(VAR, 2,
                          function(varj)
                          {
                              ok <- is.finite(varj)
                              varjok <- varj[ok]
                              xok <- x[ok]
                              nok <- sum(ok)
                              if (!dfGiven) {
                                  df <- floor(nok / 3)
                              }
                              ##message("in fcn, df=", df)
                              if (df > 1) {
                                  ##message("xok:", paste(xok, collapse=" "))
                                  ##message("varjok:", paste(varjok, collapse=" "))
                                  predict(smooth.spline(xok, varjok, df=df), xg)$y
                              } else {
                                  if (nok > 2) approx(xok, varjok, xout=xg, rule=1)$y else rep(NA, nxg)
                              }
                          })
            options(warn=owarn)
            oceDebug(debug, "dim matrix for output grid=", paste(dim(VARS), collapse="X"), "\n")
            for (istn in seq_len(nxg)) {
                res@data$station[[istn]][[var]] <- VARS[istn, ]
            }
        }
    } else {
        if (is.character(method)) {
            if (method == "barnes")
                oceDebug(debug, "using method=\"barnes\"\n")
            else if (method == "kriging")
                oceDebug(debug, "using method=\"kriging\"\n")
            else
                stop("unknown string method=\"", method, "\"; it must be \"barnes\" or \"kriging\"")
        } else if (is.function(method)) {
            oceDebug(debug, "using method=(function)\n")
        } else {
            stop("method must be a string or a function")
        }
        ## either "barnes" or a function
        ## Find names of all variables in all stations; previous to 2019 May 2,
        ## we only got names from the first station.
        XI <- geodDist(section)
        X <- unlist(lapply(seq_along(XI), function(i) rep(XI[i], length(section[["station", i]][["pressure"]]))))
        ## Set up defaults for xr and yr, if not specified in function call.
        if (!xrGiven) {
            xr <- 1.5 * median(diff(sort(x)))
            oceDebug(debug, "xr defaulting to", xr, "(1.5X the median distance between stations)\n")
        }
        if (!yrGiven) {
            yr <- 0.2 * diff(range(P, na.rm=TRUE))
            oceDebug(debug, "yr defaulting to", yr, "(0.2X the pressure range across all stations)\n")
        }
        ## Smooth each variable separately
        for (var in vars) {
            v <- NULL
            oceDebug(debug, "smoothing '", var, "' near section.R:2908\n", sep="")
            ## collect data
            v <- unlist(lapply(section[["station"]],
                               function(CTD)
                                   if (var %in% names(CTD[["data"]])) CTD[[var]] else
                                       rep(NA, length(CTD[["pressure"]]))))
            ## ignore NA values (for e.g. a station that lacks a particular variable)
            ok <- is.finite(X) & is.finite(P) & is.finite(v)
            if (is.character(method)) {
                if (method == "barnes") {
                    smu <- interpBarnes(X[ok], P[ok], v[ok], xg=xg, yg=yg, xgl=length(xg), ygl=length(yg),
                                        xr=xr, yr=yr, gamma=gamma, iterations=iterations, trim=trim,
                                        debug=debug-1)
                    ## rename to match names if method is a function.
                    smu$z <- smu$zg
                    smu$x <- smu$xg
                    smu$y <- smu$yg
                    if (all(is.na(smu$z)))
                        warning("All \"", var, "\" data are NA, so gridded field is a matrix of NA values\n")
                } else if (method == "kriging") {
                    if (requireNamespace("automap", quietly=TRUE) &&
                        requireNamespace("sp", quietly=TRUE)) {
                        krigFunction <- function(x, y, F, xg, xr, yg, yr) {
                            xy <- data.frame(x=x/xr, y=y/yr)
                            K <- automap::autoKrige(F~1, remove_duplicates=TRUE,
                                                    input_data=sp::SpatialPointsDataFrame(xy, data.frame(F)),
                                                    new_data=sp::SpatialPoints(expand.grid(xg/xr, yg/yr)))
                            matrix(K$krige_output@data$var1.pred, nrow=length(xg), ncol=length(yg))
                        }
                        owarn <- options('warn')$warn
                        options(warn=-1) # silence autoKrige chattiness on e.g. method selection
                        capture.output(
                                       {
                                           smu <- list(z=krigFunction(X[ok], P[ok], v[ok], xg=xg, xr=xr, yg=yg, yr=yr))
                                       }
                        )
                        options(warn=owarn)
                    } else {
                        stop('method="kriging" requires packages "automap" and "sp" to be installed\n')
                    }
                } else {
                    stop('method must be "barnes", "kriging", "spline", "barnes" or a function')
                }
            } else {
                ## method is not a character. It must be a function, but let's check again, anyway.
                if (is.function(method)) {
                    smu <- list(z=method(X[ok], P[ok], v[ok], xg=xg, xr=xr, yg=yg, yr=yr),
                                x=xg, y=yg)
                } else {
                    stop('method must be "barnes", "kriging", "spline", "barnes" or a function')
                }
            }
            ## cat("smu$y:\n");print(smu$y)
            for (istn in seq_len(nxg)) {
                ##cat("istn=", istn, "\n", sep="")
                res@data$station[[istn]]@data[[var]] <- smu$z[istn, ]
                ##cat("  set '", var, "'\n", sep="")
                res@data$station[[istn]]@data[["pressure"]] <- smu$y
                ##cat("  set pressure\n", sep="")
            }
        }
    }
    oceDebug(debug, "smoothing portion completed (near section.R line 2920)\n")
    ## Set up section-level and station-level metadata
    ##longitudeNew <- approx(x, section[["longitude", "byStation"]], xg)$y # FIXME
    ##latitudeNew <- approx(x, section[["latitude", "byStation"]], xg)$y
    res@metadata$longitude <- approx(x, section[["longitude", "byStation"]], xg)$y
    res@metadata$latitude <- approx(x, section[["latitude", "byStation"]], xg)$y
    if (any(!is.finite(res@metadata$longitude)))
        warning("some gridded longitudes are NA\n")
    if (any(!is.finite(res@metadata$latitude)))
        warning("some gridded latitudes are NA\n")
    for (i in seq_along(xg)) {
        res@data$station[[i]]@metadata$longitude <- res@metadata$longitude[i]
        res@data$station[[i]]@metadata$latitude <- res@metadata$latitude[i]
        res@data$station[[i]]@metadata$stationId <- if (nxg < 10) sprintf("x%d", istn)
            else if (nxg < 100) sprintf("x%02d", istn)
            else if (nxg < 1000) sprintf("x%03d", istn)
            else if (nxg < 10000) sprintf("x%04d", istn)
            else sprintf("x%d", istn) # Just give up on fanciness
    }
    nstation <- length(res[["station"]])
    waterDepthOriginal <- unlist(lapply(section[["station"]], function(STN) STN[["waterDepth"]]))
    if (length(waterDepthOriginal) > 0) {
        waterDepthNew <- approx(x, waterDepthOriginal, xg, rule=2)$y
        for (i in seq_len(nstation)) {
            res@data$station[[i]]@metadata$waterDepth <- waterDepthNew[i]
        }
    }
    oceDebug(debug > 3, "about to create units\n")
    ## Insert uniform units and flags into each station.
    units <- list()
    for (i in seq_along(section[["station"]])) {
        oceDebug(debug > 3, "station i=", i, ", nstation=", nstation, ", length(section@data$section)=", length(section@data$station), "\n", sep="")
        for (unitName in names(section@data$station[[i]]@metadata$units)) {
            oceDebug(debug > 3, "unitName='", unitName, "'\n", sep="")
            if (!(unitName %in% names(units))) {
                oceDebug(debug > 3, " ... installing unitName='", unitName, "'\n", sep="")
                units[unitName] <- section@data$station[[i]]@metadata$units[unitName]
                oceDebug(debug > 3, " ... installation was ok\n")
            }
        }
    }
    oceDebug(debug > 3, "installing units\n")
    oceDebug(debug > 3, "length(res@data$station)=", length(res@data$station), "\n")
    oceDebug(debug > 3, "nxg=", nxg, "\n")
    for (i in seq_len(nxg)) {
        res@data$station[[i]]@metadata$units <- units
        ## Note that we put in flags for *all* variables in the output file. This
        ## is different from the action in sectionGrid(), which inserts flags
        ## tailored to individual stations. However, smoothing creates new stations,
        ## and puts *all* variables in *each* station, so we need flags for everything.
        res@data$station[[i]]@metadata$flags <- list()
        for (flagname in flagnames) {
            ##> message("sectionSmooth ... i ", i, " (nxg=", nxg, "), flagname '", flagname, "'")
            res@data$station[[i]]@metadata$flags[[flagname]] <- rep(NA, nyg)
        }
    }
    res@processingLog <- processingLogAppend(res@processingLog, paste(deparse(match.call()), sep="", collapse=""))
    oceDebug(debug, "} # sectionSmooth()\n", unindent=1)
    res
}


#' Create a Section
#'
#' Create a section based on columnar data, or a set of [oce-class]
#' objects that can be coerced to a section. There are three cases.
#'
#' Case 1. If the first argument is a numerical vector, then it is taken to be the
#' salinity, and [factor()] is applied to `station` to break the
#' data up into chunks that are assembled into [ctd-class] objects with
#' [as.ctd()] and combined to make a [section-class] object
#' to be returned. This mode of operation is provided as a convenience for datasets
#' that are already partly processed; if original CTD data are available, the next
#' mode is preferred, because it permits the storage of much more data and metadata
#' in the CTD object.
#'
#' Case 2. If the first argument is a list containing oce objects, then those
#' objects are taken as profiles of something.  A requirement for this
#' to work is that every element of the list contains both `longitude`
#' and `latitude` in either the `metadata` or `data` slot (in
#' the latter case, the mean value is recorded in the section object)
#' and that every element also contains `pressure` in its `data` slot.
#'
#' Case 3. If the first argument is a [argo-class] object, then
#' the profiles it contains are turned into [ctd-class] objects,
#' and these are assembled into a section to be returned.
#'
#' @param salinity This may be a numerical vector, in which case it is interpreted
#' as the salinity, and the other arguments are used for the other components of
#' [ctd-class] objects. Alternatively, it may be one of a variety of
#' other objects from which the CTD objects can be inferred, in which case the
#' other arguments are ignored; see \sQuote{Details}.
#'
#' @param temperature Temperature, in a vector holding values for all stations.
#'
#' @param pressure Pressure, in a vector holding values for all stations.
#'
#' @param longitude Longitude, in a vector holding values for all stations.
#'
#' @param latitude Latitude, in a vector holding values for all stations.
#'
#' @param station Station identifiers, in a vector holding values for all stations.
#'
#' @param sectionId Section identifier.
#'
#' @param debug an integer value that controls whether `as.section()` prints information
#' during its work.  The function works quietly if this is 0 and prints out some
#' information if it is positive.
#'
#' @return An object of [section-class].
#'
#' @examples
#' library(oce)
#' data(ctd)
#' ## vector of names of CTD objects
#' fake <- ctd
#' fake[["temperature"]] <- ctd[["temperature"]] + 0.5
#' fake[["salinity"]] <- ctd[["salinity"]] + 0.1
#' fake[["longitude"]] <- ctd[["longitude"]] + 0.01
#' fake[["station"]] <- "fake"
#' sec1 <- as.section(c("ctd", "fake"))
#' summary(sec1)
#' ## vector of CTD objects
#' ctds <- vector("list", 2)
#' ctds[[1]] <- ctd
#' ctds[[2]] <- fake
#' sec2 <- as.section(ctds)
#' summary(sec2)
#' ## argo data (a subset)
#' data(argo)
#' sec3 <- as.section(subset(argo, profile<5))
#' summary(sec3)
#'
#' @author Dan Kelley
#'
#' @family things related to section data
as.section <- function(salinity, temperature, pressure, longitude, latitude, station, sectionId="", debug=getOption("oceDebug"))
{
    debug <- as.integer(min(1, max(0, debug))) # make it be 0 or 1
    oceDebug(debug, "as.section() {\n", sep="", style="bold", unindent=1)
    if (missing(salinity))
        stop("argument 'salinity' is missing")
    res <- new("section", sectionId="")
    if (is.numeric(salinity)) {
        if (missing(temperature)) stop("must provide temperature")
        if (missing(temperature)) stop("must provide temperature")
        if (missing(pressure)) stop("must provide pressure")
        if (missing(longitude)) stop("must provide longitude")
        if (missing(latitude)) stop("must provide latitude")
        if (missing(station)) stop("must provide station")
        stationFactor <- factor(station)
        stationLevels <- levels(stationFactor)
        nstation <- length(stationLevels)
        ctds <- vector("list", nstation)
        for (i in 1:nstation) {
            ## message("NUMERIC CASE. i: ", i, ", name:", stationLevels[i])
            look <- station==stationLevels[i]
            ctds[[i]] <- as.ctd(salinity[look], temperature[look], pressure[look],
                                longitude=longitude[look][1],
                                latitude=latitude[look][1],
                                station=stationLevels[i])
        }
    } else if (inherits(salinity, "argo")) {
        tmp <- salinity
        nstation <- length(tmp[['longitude']])
        station <- 1:nstation
        longitude <- tmp[['longitude']]
        latitude <- tmp[['latitude']]
        salinity <- tmp[['salinity']]
        temperature <- tmp[['temperature']]
        pressure <- tmp[['pressure']]
        stationFactor <- factor(station)
        stationLevels <- levels(stationFactor)
        ctds <- vector("list", nstation)
        ## N will hold the names of extra data for the CTDs
        N <- names(tmp@data)
        if ("time" %in% N) N <- N[-which(N=="time")]
        if ("longitude" %in% N) N <- N[-which(N=="longitude")]
        if ("latitude" %in% N) N <- N[-which(N=="latitude")]
        if ("salinity" %in% N) N <- N[-which(N=="salinity")]
        if ("temperature" %in% N) N <- N[-which(N=="temperature")]
        if ("pressure" %in% N) N <- N[-which(N=="pressure")]
        time <- tmp[['time']]
        for (i in 1:nstation) {
            ctds[[i]] <- as.ctd(salinity[, i], temperature[, i], pressure[, i],
                                longitude=longitude[i], latitude=latitude[i],
                                startTime=as.POSIXct(time[i]), station=as.character(station[i]))
            for (Ni in seq_along(N)) {
                ctds[[i]] <- oceSetData(ctds[[i]], name=N[Ni], value=tmp[[N[Ni]]][, i],
                                        unit=tmp[['units']][[N[Ni]]])
            }
        }
    } else if (inherits(salinity, "list")) {
        oceDebug(debug, "first argument is a list (assumed to be a list of oce objects)\n")
        thelist <- salinity            # prevent accidental overwriting
        if (!length(thelist))
            stop("no data in this list")
        if (inherits(thelist[[1]], "oce")) {
            nstation <- length(salinity)
            ctds <- vector("list", nstation)
            badDepths <- NULL
            for (i in seq_len(nstation)) {
                oceDebug(debug, "processing item", i, "of", nstation, "\n")
                if (!("pressure" %in% names(thelist[[i]]@data)))
                    stop("cannot create a section from this first argument, because its ", i, "-th element lacks pressure")
                # Replace NA water depth with highest pressure. Note that this action is skipped
                # if there is no water depth (e.g. if the first argument is a list of Argo objects).
                # See https://github.com/dankelley/oce/issues/1797
                if ("waterDepth" %in% names(thelist[[i]]@metadata)) {
                    if (is.na(thelist[[i]]@metadata$waterDepth)) {
                        thelist[[i]]@metadata$waterDepth <- max(thelist[[i]]@data$pressure, na.rm=TRUE)
                        badDepths <- c(badDepths, i)
                    }
                } else {
                    thelist[[i]]@metadata$waterDepth <- NA
                }
                ctds[[i]] <- as.ctd(thelist[[i]])
            }
            if (length(badDepths))
                warning("estimated waterDepth as max(pressure) for CTDs numbered ",
                        paste(abbreviateVector(badDepths), collapse=" "))
        } else {
            stop("first argument must be a salinity vector, or a list of oce objects")
        }
    } else if (is.character(salinity)) {
        ## vector of names of CTD objects
        nstation <- length(salinity)
        ctds <- vector("list", nstation)
        for (i in 1:nstation)
            ctds[[i]] <- get(salinity[i], parent.frame())
    } else {
        stop("first argument is not understood")
    }
    ## In each case, we now have a vector of CTD objects.
    res@metadata$sectionId <- ""
    res@metadata$stationId <- unlist(lapply(ctds, function(x) x[["station"]][1]))
    res@metadata$longitude <- unlist(lapply(ctds, function(x) mean(x[["longitude"]], na.rm=TRUE)))
    res@metadata$latitude <- unlist(lapply(ctds, function(x) mean(x[["latitude"]], na.rm=TRUE)))
    res@metadata$time <- numberAsPOSIXct(unlist(lapply(ctds, function(x) x[["time"]][1])))
    res@data <- list(station=ctds)
    res@processingLog <- processingLogAppend(res@processingLog, paste(deparse(match.call()), sep="", collapse=""))
    oceDebug(debug, "} # as.section()\n", sep="", style="bold", unindent=1)
    res
}

#' Add a spine to a section object
#'
#' The purpose of this is to permit plotting with `xtype="spine"`, so that
#' the section plot will display the distance of stations projected
#' onto the spine.
#'
#' @param section a [section-class] object.
#' @param spine either a list or a data frame, containing numeric items named
#' `longitude` and `latitude`, defining a path along the spine.
#' @template debugTemplate
#'
#' @return A [section-class] object with a spine added.
#'
#' @examples
#' library(oce)
#' data(section)
#' sectionWest <- subset(section, longitude < -60)
#' spine <- list(longitude=c(-74.5, -69.2, -55), latitude=c(38.6, 36.25, 36.25))
#' sectionWithSpine <- addSpine(sectionWest, spine)
#' plot(sectionWithSpine, which="map")
#' plot(sectionWithSpine, xtype="distance", which="temperature")
#' plot(sectionWithSpine, xtype="spine", which="temperature")
#'
#' @author Dan Kelley
addSpine <- function(section, spine, debug=getOption("oceDebug"))
{
    oceDebug(debug, "addSpine(..., spine=", argShow(spine), ") {\n", sep="", style="bold", unindent=1)
    if (missing(section))
        stop("must provide 'section' argument")
    if (!inherits(section, "section"))
        stop("'section' must be a section object, e.g. created by read.section() or as.section()")
    if (missing(spine))
        stop("must provide 'spine' argument")
    res <- section
    if (2 == length(spine) && 2 == sum(c("latitude", "longitude") %in% names(spine))) {
        if (length(spine$longitude) != length(spine$latitude))
            stop("unequal lengths of spine longitude (", length(spine$longitude),
                 ") and latitude (", length(spine$latitude), ")")
        if (length(spine$longitude) < 2)
            stop("length of spine longitude must exceed 2, but it is ", length(spine$longitude))
        res@metadata$spine <- spine
    } else {
        stop("'spine' must be a list or data frame containing two items, named 'longitude' and 'latitude'")
    }
    oceDebug(debug, "} # addSpine()\n", sep="", style="bold", unindent=1)
    res
}

#' Try to Reduce Section Longitude Range
#'
#' [longitudeTighten] shifts some longitudes in its first argument by 360 degrees, if doing
#' so will reduce the overall longitude span.
#'
#' This function can be helpful in cases where the CTD stations within a section
#' cross the cut point of the longitude convention, which otherwise might
#' yield ugly plots if [plot,section-method()] is used with `xtype="longitude"`.
#' This problem does occur with CTD objects ordered by time of sampling,
#' but was observed in December 2020 for a GO-SHIPS dataset downloaded from
#' `https://cchdo.ucsd.edu/data/15757/a10_1992_ct1`.
#'
#' @param section a [section-class] object.
#'
#' @return A [section-class] object based on its first argument, but with
#' longitudes shifted in its `metadata` slot, and also in the `metadata` slots
#' of each of the [ctd-class] objects that are stored in the `station` item
#' in its `data` slot.
#'
#' @author Dan Kelley
longitudeTighten <- function(section)
{
    if (!inherits(section, "section"))
        stop("'section' must be an object created with read.section() or as.section()")
    res <- section
    longitude <- section[["longitude", "byStation"]]
    longitudeShifted <- ifelse(longitude > 180, longitude - 360, longitude)
    if (diff(range(longitude)) > diff(range(longitudeShifted)))
        longitude <- longitudeShifted
    res <- oceSetMetadata(res, "longitude", longitude, "longitudeTighten")
    ctds <- section@data$station
    for (i in seq_along(ctds))
        ctds[[i]]@metadata$longitude <- longitude[i]
    res@data$station <- ctds
    res
}

