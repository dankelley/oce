## vim:textwidth=128:expandtab:shiftwidth=4:softtabstop=4

#' @title Class to Store Hydrographic Section Data
#' 
#' @description
#' Class to store hydrographic section data, with standard slots \code{metadata},
#' \code{data} and \code{processingLog}.
#'
#' A \code{\link{list}} of stations is retrieved by \code{s[["station"]]}.
#' Individual stations are retrieved by providing a station number as a second
#' argument in the index, e.g.  the first station is \code{s[["station", 1]]}
#' (which is a \code{\link{ctd-class}} object).
#'         
#' Aggregated values of the quantities measured at each level of the CTD
#' profiles contained within the section may be accessed as e.g.
#' \code{section[["salinity"]]}.  This works for any quantity whose name is
#' present in the constituent profiles.   
#' 
#' Since it is often useful to pair such quantities with locations,
#' \code{section[["longitude"]]} and \code{section[["latitude"]]} return vectors
#' with values repeated for each level in each CTD (see the \code{pairs()} call in
#'                                                  the example section).  If just
#' one latitude or longitude is desired per station, e.g.
#' \code{section[["latitude", "byStation"]]} may be used.  Station-by-station
#' values of dynamic height are provided by e.g.
#' \code{section[["dynamic height"]]}.
#' 
#' The depths of all data are obtained from e.g.  \code{section[["depth"]]}, and
#' the distances along the transect, measured from the first station, are obtained
#' from e.g.  \code{section[["distance"]]}.
#'
#' @seealso
#' Sections can be read with \code{\link{read.section}} or created with
#' \code{\link{read.section}} or created from CTD objects by using
#' \code{\link{as.section}} or by adding a ctd station to an existing section with
#' \code{\link{sectionAddStation}}.
#' 
#' Sections may be sorted with \code{\link{sectionSort}}, subsetted with
#' \code{\link{subset,section-method}}, smoothed with \code{\link{sectionSmooth}}, and
#' gridded with \code{\link{sectionGrid}}.  Gridded sections may be plotted with
#' \code{\link{plot,section-method}}.  
#'     
#' Statistical summaries are provided by \code{\link{summary,section-method}}, while
#' overviews are provided by \code{show.section}.  
#'     
#' The sample dataset \code{\link{section}} contains data along WOCE line A03.
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
#' for (stn in section[["station",1:9]])
#'     plotProfile(stn, xtype='temperature', ylim=ylim, Tlim=Tlim)
#'
#' @author Dan Kelley
#'
#' @family classes provided by \code{oce}
#' @family things related to \code{section} data
setClass("section", contains="oce")


#' @title Hydrographic section
#' 
#' @description
#' This is line A03 (ExpoCode 90CT40_1, with nominal sampling date 1993-09-11).
#' The chief scientist was Tereschenkov of SOI, working aboard the Russian ship
#' Multanovsky, undertaking a westward transect from the Mediterranean outflow
#' region across to North America, with a change of heading in the last few dozen
#' stations to run across the nominal Gulf Stream axis.
#' The data flags follow the WHP CTD convention, i.e. 1 for uncalibrated,
#' 2 for an acceptable measurement, 3 for a questionable measurement, 4
#' for a bad measurement, etc; see \url{https://www.nodc.noaa.gov/woce/woce_v3/wocedata_1/whp/exchange/exchange_format_desc.htm}
#' for further details.
#' 
#' @examples
#' \dontrun{
#' library(oce)
#' # Gulf Stream
#' data(section)
#' GS <- subset(section, 109<=stationId&stationId<=129)
#' GSg <- sectionGrid(GS, p=seq(0, 5000, 100))
#' plot(GSg, map.xlim=c(-80,-60))
#' }
#' 
#' @name section
#' 
#' @docType data
#' 
#' @usage data(section)
#' 
#' @source This is based on the WOCE file named \code{a03_hy1.csv}, downloaded
#' from \url{http://cchdo.ucsd.edu/cruise/90CT40_1}, 13 April 2015.
#' 
#' @family datasets provided with \code{oce}
#' @family things related to \code{section} data
NULL

setMethod(f="initialize",
          signature="section",
          definition=function(.Object, filename="", sectionId="") {
              .Object@metadata$filename <- filename
              .Object@metadata$sectionId <- sectionId
              .Object@processingLog$time <- as.POSIXct(Sys.time())
              .Object@processingLog$value <- "create 'section' object"
              return(.Object)
          })

## DEVELOPERS: please pattern functions and documentation on this, for uniformity.
## DEVELOPERS: You will need to change the docs, and the 3 spots in the code
## DEVELOPERS: marked '# DEVELOPER 1:', etc.
#' @title Handle flags in Section Objects
#' @details
#' If \code{flags} and \code{actions} are not provided, the
#' default is to use WHP (World Hydrographic Program) flags [1], in which the
#' value 2 indicates good data, and other values indicate either unchecked,
#' suspicious, or bad data. Any data not flagged as good are set
#' to \code{NA} in the returned value. Since WHP flag codes run
#' from 1 to 9, this default is equivalent to
#' setting \code{flags=list(all=c(1, 3:9))} along with
#' \code{action=list("NA")}.
#' @param object An object of \code{\link{section-class}}.
#' @template handleFlagsTemplate
#' @references
#' 1. \url{https://www.nodc.noaa.gov/woce/woce_v3/wocedata_1/whp/exchange/exchange_format_desc.htm}
#' @examples
#' library(oce)
#' data(section)
#' section2 <- handleFlags(section)
#' par(mfrow=c(2,1))
#' plotTS(section)
#' plotTS(section2)
#'
#' @family things related to \code{section} data
setMethod("handleFlags",
          c(object="section", flags="ANY", actions="ANY"),
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
              res <- object
              for (i in seq_along(res@data$station)) {
                  res@data$station[[i]] <- handleFlags(res@data$station[[i]], flags, actions)
              }
              res
          })


#' @title Summarize a Section Object
#' 
#' @description
#' Pertinent summary information is presented, including station locations,
#' distance along track, etc.
#' 
#' @param object An object of class \code{"section"}, usually, a result of a call
#' to \code{\link{read.section}}, \code{\link{read.oce}}, or
#' \code{\link{as.section}}.
#' 
#' @param ... Further arguments passed to or from other methods.
#' 
#' @return \code{NULL}
#' 
#' @examples
#' library(oce)
#' data(section)
#' summary(section)
#' 
#' @family things related to \code{section} data
#' 
#' @author Dan Kelley
setMethod(f="summary",
          signature="section",
          definition=function(object, ...) {
              numStations <- length(object@data$station)
              lat1 <- object@data$station[[1]]@metadata$latitude
              lon1 <- object@data$station[[1]]@metadata$longitude
              cat("Section Summary\n---------------\n\n")
              cat("* Source: \"", object@metadata$filename, "\"\n", sep="")
              cat("* ID:     \"", object@metadata$sectionId, "\"\n",sep="")
              stn.sum <- matrix(nrow=numStations, ncol=5)
              if (numStations > 0) {
                  cat("* Summary of", numStations, "stations (first column is station ID)\n")
                  for (i in 1:numStations) {
                      stn <- object@data$station[[i]]
                      stn.sum[i, 1] <- stn@metadata$longitude
                      stn.sum[i, 2] <- stn@metadata$latitude
                      stn.sum[i, 3] <- length(stn@data$pressure)
                      if (!is.null(stn@metadata$waterDepth) && is.finite(stn@metadata$waterDepth)) {
                          stn.sum[i, 4] <- stn@metadata$waterDepth
                      } else {
                          temp <- stn@data$temperature
                          wdi <- length(temp) - which(!is.na(rev(temp)))[1] + 1
                          stn.sum[i, 4] <- stn@data$pressure[wdi]
                      }
                      stn.sum[i, 5] <- geodDist(lon1, lat1, stn@metadata$longitude, stn@metadata$latitude)
                  }
                  colnames(stn.sum) <- c("Long.", "Lat.", "Levels", "Depth", "Distance")
                  rownames(stn.sum) <- object@metadata$stationId
                  print(stn.sum, indent="    ")
              } else {
                  cat("* No stations\n")
              }
              processingLogShow(object)
              invisible(NULL)
          })

#' @title Extract Something From a Section Object
#' @param x A \code{section} object, i.e. one inheriting from \code{\link{section-class}}.
#' @family things related to \code{section} data
#' @examples
#' data(section)
#' length(section[["latitude"]])
#' length(section[["latitude", "byStation"]])
#'
#' @section Details of the specialized section method:
#' If \code{i} is the string \code{"station"}, then the method 
#' will return a \code{\link{list}} of 
#' \code{\link{ctd-class}} objects holding the station data. If \code{j} 
#' is also given and is an integer, then just the j-th station in the section is returned.
#'
#' If \code{i} is \code{"station ID"}, then the IDs of the stations in the 
#' section are returned.
#'
#' If \code{i} is \code{"dynamic height"}, then an estimate of dynamic
#' height is returned (as calculated with \code{\link{swDynamicHeight}(x)}).
#'
#' If \code{i} is \code{"distance"}, then the distance along the section is
#' returned, using \code{\link{geodDist}}.
#'
#' If \code{i} is \code{"depth"}, then a vector containing the depths
#' of the stations is returned.
#'
#' If \code{i} is \code{"theta"} or \code{"potential temperature"}, then
#' the potential temperatures of all the stations are returned in one 
#' vector.
#'
#' If \code{i} is a string ending with \code{"Flag"}, then the characters
#' prior to that ending are taken to be the name of a variable contained
#' within the stations in the section. If this flag is available in 
#' the first station of the section, then the flag values are looked
#' up for every station.
#'
#' If none of the conditions listed above holds, the general
#' method is used (see \sQuote{Details of the general method}).
#'
#' @examples
#' data(section)
#' length(section[["latitude"]])
#' length(section[["latitude", "byStation"]])
#'
#' @template sub_subTemplate
#' @author Dan Kelley
setMethod(f="[[",
          signature(x="section", i="ANY", j="ANY"),
          definition=function(x, i, j, ...) {
              ## Data-quality flags are a special case
              res <- NULL
              if (1 == length(grep(".*Flag$", i))) {
                  baseName <- gsub("Flag$", "", i)
                  if (baseName %in% names(x@data$station[[1]]@metadata$flags)) {
                      res <- unlist(lapply(x@data$station, function(ctd) ctd[[i]]))
                      return(res)
                  } else {
                      stop("the stations within this section do not contain a '", baseName, "' flag")
                  }
              }
              ## some derived things (not all ... be sure to document when adding things!)
              if (i == "theta" || i == "potential temperature") {
                  res <- unlist(lapply(x@data$station, function(ctd) ctd[[i]]))
                  return(res)
              }
              if (i %in% names(x@metadata)) {
                  if (i %in% c("longitude", "latitude")) {
                      if (!missing(j) && "byStation" == j) {
                          return(x@metadata[[i]])
                      } else {
                          res <- NULL
                          for (stn in seq_along(x@data$station))
                              res <- c(res, rep(x@data$station[[stn]]@metadata[[i]], length(x@data$station[[stn]]@data$salinity)))
                          return(res)
                      }
                  } else {
                      return(x@metadata[[i]])
                  }
              } else if (i %in% names(x@data$station[[1]]@data)) {
                  res <- NULL
                  for (stn in seq_along(x@data$station))
                      res <- c(res, x@data$station[[stn]]@data[[i]])
              } else if ("station" == i) {
                  if (missing(j)) {
                      res <- x@data$station
                  } else {
                      nj <- length(j)
                      if (nj == 1) {
                          res <- x@data$station[[j]]
                      } else {
                          res <- vector("list", nj)
                          for (jj in j)
                              res[[jj]] <- x@data$station[[jj]]
                      }
                  }
              } else if ("station ID" == i) {
                  res <- NULL
                  for (stn in x[['station']])
                      res <- c(res, stn[['station']])
              } else if ("dynamic height" == i) {
                  res <- swDynamicHeight(x)
              } else if ("distance" == i) {
                  res <- NULL
                  for (stn in seq_along(x@data$station)) {
                      distance <- geodDist(x@data$station[[stn]]@metadata$longitude,
                                           x@data$station[[stn]]@metadata$latitude,
                                           x@data$station[[1]]@metadata$longitude,
                                           x@data$station[[1]]@metadata$latitude)
                      if (!missing(j) && j == "byStation")
                          res <- c(res, distance)
                      else
                          res <- c(res, rep(distance, length(x@data$station[[stn]]@data$temperature)))

                  }
              } else if ("depth" == i) {
                  res <- NULL
                  for (stn in seq_along(x@data$station))
                      res <- c(res, x@data$station[[stn]]@data$pressure) # FIXME not really depth
              ##?20160328? } else {
              ##?20160328?     res <- unlist(lapply(x@data$station, function(X) X[[i]]))
              } else {
                  callNextMethod()
              }
              res
          })

#' @title Replace Parts of a Section Object
#' @param x A \code{section} object, i.e. inheriting from \code{\link{section-class}}
#' @family things related to \code{section} data
#' @template sub_subsetTemplate
#' @examples
#' # Change section ID from a03 to A03
#' data(section)
#' section[["sectionId"]]
#' section[["sectionId"]] <- toupper(section[["sectionId"]])
#' section[["sectionId"]]
#' @author Dan Kelley
setMethod(f="[[<-",
          signature(x="section", i="ANY", j="ANY"),
          definition=function(x, i, j, value) {
              callNextMethod(x=x, i=i, j=j, value=value)
          })


setMethod(f="show",
          signature="section",
          definition=function(object) {
              id <- object@metadata$sectionId
              if (id == "")
                  cat("Section has stations:\n", sep="")
              else
                  cat("Section named '", id, "' has stations:\n", sep="")
              for (i in seq_along(object@data$station)) {
                  thisStn <- object@data$station[[i]]
                  cat("    ")
                  if (!is.null(thisStn@metadata$station) && "" != thisStn@metadata$station)
                      cat(thisStn@metadata$station, " ")
                  waterDepth <- if (is.null(thisStn@metadata$waterDepth)) max(thisStn@data$pressure, na.rm=TRUE) else thisStn@metadata$waterDepth
                  cat(sprintf("%.5f N   %.5f E   %.0f m", thisStn@metadata$latitude,
                              thisStn@metadata$longitude,
                              waterDepth))
                  cat("\n")
              }
          })

#' @title Subset a Section Object
#' 
#' @description
#' This function is somewhat analogous to \code{\link{subset.data.frame}}.  The
#' condition set by \code{subset} may be in terms of \code{stationId} or any
#' combination of \code{longitude}, \code{latitude} and \code{time}.  However,
#' \code{stationId} may not be combined with the others; to get that effect, call
#' this function more than once.
#' 
#' @param x A \code{\link{section-class}} object.
#' 
#' @param subset A condition to be applied to the \code{data} portion of \code{x}.
#' See \sQuote{Details}.
#' 
#' @param ... May include \code{debug}, to set a debugging level.
#' 
#' @return A new \code{section} object.
#' 
#' @examples
#' library(oce)
#' data(section)
#' GS <- subset(section, 109<=stationId&stationId<=129)
#' 
#' @family things related to \code{section} data
#' 
#' @author Dan Kelley
setMethod(f="subset",
          signature="section",
          definition=function(x, subset, ...) {
              subsetString <- paste(deparse(substitute(subset)), collapse=" ")
              res <- x
              dots <- list(...)
              dotsNames <- names(dots)
              indicesGiven <- length(dots) && ("indices" %in% dotsNames)
              debug <- getOption("oceDebug")
              if (length(dots) && ("debug" %in% names(dots)))
                  debug <- dots$debug
              if (indicesGiven) {        # select a portion of the stations
                  if (!missing(subset))
                      stop("cannot give both 'subset' and 'indices'")
                  oceDebug(debug, "subsetting by indices\n")
                  res <- new("section")
                  indices <- dots$indices
                  n <- length(indices)
                  if (is.logical(indices))
                      indices <- (1:n)[indices]
                  station <- vector("list", n)
                  stn <- vector("character", n)
                  lon <- vector("numeric", n)
                  lat <- vector("numeric", n)
                  for (i in 1:n) {
                      ii <- indices[i]
                      stn[i] <- x@metadata$stationId[ii]
                      lat[i] <- x@metadata$latitude[ii]
                      lon[i] <- x@metadata$longitude[ii]
                      station[[i]] <- x@data$station[[ii]]
                  }
                  data <- list(station=station)
                  res@metadata$stationId <- stn
                  res@metadata$longitude <- lon
                  res@metadata$latitude <- lat
                  res@data <- data
                  res@processingLog <- x@processingLog
                  res@processingLog <- processingLogAppend(res@processingLog, paste("subset(x, indices=c(", paste(dots$indices, collapse=","), "))", sep=""))
              } else if (length(grep("stationId", subsetString))) {
                  keep <- eval(substitute(subset),
                               envir=data.frame(stationId=as.numeric(x@metadata$stationId)))
                  res@metadata$stationId <- x@metadata$stationId[keep]
                  res@metadata$longitude <- x@metadata$longitude[keep]
                  res@metadata$latitude <- x@metadata$latitude[keep]
                  res@metadata$time <- x@metadata$time[keep]
                  res@data$station <- x@data$station[keep]
                  res@processingLog <- processingLogAppend(res@processingLog, paste("subset(x, subset=", subsetString, ")", sep=""))
              } else {                        # subset within the stations
                  if ("indices" %in% dotsNames)
                      stop("2. cannot give both 'subset' and 'indices'")
                  oceDebug(debug, "subsetting by 'subset'\n")
                  ##subsetString <- deparse(substitute(subset))
                  ##oceDebug(debug, "subsetString='", subsetString, "'\n")
                  res <- x
                  if (length(grep("distance", subsetString))) {
                      l <- list(distance=geodDist(res))
                      keep <- eval(substitute(subset), l, parent.frame(2))
                      res@metadata$longitude <- res@metadata$longitude[keep]
                      res@metadata$latitude <- res@metadata$latitude[keep]
                      res@metadata$stationId <- res@metadata$stationId[keep]
                      res@data$station <- res@data$station[keep]
                  } else if (length(grep("latitude", subsetString)) || length(grep("longitude", subsetString))) {
                      n <- length(x@data$station)
                      keep <- vector(length=n)
                      for (i in 1:n)
                          keep[i] <- eval(substitute(subset), x@data$station[[i]]@metadata, parent.frame(2))
                      nn <- sum(keep)
                      station <- vector("list", nn)
                      stn <- vector("character", nn)
                      lon <- vector("numeric", nn)
                      lat <- vector("numeric", nn)
                      j <- 1
                      for (i in 1:n) {
                          if (keep[i]) {
                              stn[j] <- x@metadata$stationId[i]
                              lon[j] <- x@metadata$longitude[i]
                              lat[j] <- x@metadata$latitude[i]
                              station[[j]] <- x@data$station[[i]]
                              j <- j + 1
                          }
                      }
                      res <- new('section')
                      res@data$station <- station
                      res@metadata$header <- x@metadata$header
                      res@metadata$sectionId <- x@metadata$sectionId
                      res@metadata$stationId <- stn
                      res@metadata$longitude <- lon
                      res@metadata$latitude <- lat
                      res@processingLog <- x@processingLog
                  } else {
                      n <- length(x@data$station)
                      r <- eval(substitute(subset), x@data$station[[1]]@data, parent.frame(2))
                      for (i in 1:n) {
                          res@data$station[[i]]@data <- x@data$station[[i]]@data[r,]
                      }
                  }
                  res@processingLog <- processingLogAppend(res@processingLog, paste("subset(x, subset=", subsetString, ")", sep=""))
              }
              res
          })

 
#' @title Sort a Section
#' 
#' @description
#' Sections created with \code{\link{as.section}} have "stations" that are in the
#' order of the CTD objects (or filenames for such objects) provided.  Sometimes,
#' this is not the desired order, e.g. if file names discovered with
#' \code{\link{dir}} are in an order that makes no sense.  (For example, a
#' practioner might name stations \code{"stn1"}, \code{"stn2"}, etc., not
#' realizing that this will yield an unhelpful ordering, by file name, if there
#' are more than 9 stations.) The purpose of \code{sectionSort} is to permit
#' reordering the constituent stations in sensible ways.
#' 
#' @param section A \code{section} object containing the section whose stations
#' are to be sorted.
#' 
#' @param by An optional string indicating how to reorder.  If not provided,
#' \code{"stationID"} will be assumed.  Other choices are \code{"distance"}, for
#' distance from the first station, \code{"longitude"}, for longitude,
#' \code{"latitude"} for latitude, and \code{"time"}, for time.
#' 
#' @return An object of \code{\link{section-class}} that has less lateral
#' variation than the input section.
#' 
#' @examples
#' \dontrun{
#' # Eastern North Atlantic, showing Mediterranean water;
#' # sorting by longitude makes it easier to compare
#' # the map and the section.
#' library(oce)
#' data(section)
#' s <- sectionGrid(subset(section, -30 <= longitude))
#' ss <- sectionSort(ss, by="longitude")
#' plot(ss)
#' }
#' 
#' @author Dan Kelley
#' 
#' @family things related to \code{section} data
sectionSort <- function(section, by)
{
    if (missing(by)) {
        by <- "stationId"
    } else {
        byChoices <- c("stationId", "distance", "longitude", "latitude", "time")
        iby <- pmatch(by, byChoices, nomatch=0)
        if (0 == iby)
            stop('unknown by value "', by, '"; should be one of: ', paste(byChoices, collapse=" "))
        by <- byChoices[iby]
    }
    res <- section
    if (by == "stationId") {
	o <- order(section@metadata$stationId)
    } else if (by == "time") {
        ## FIXME: should check to see if startTime exists first?
        times <- unlist(lapply(section@data$station, function(x) x@metadata$startTime))
        o <- order(times)
    } else {
	o <- order(section[[by, "byStation"]])
    }
    res@metadata$stationId <- res@metadata$stationId[o]
    res@metadata$longitude <- res@metadata$longitude[o]
    res@metadata$latitude <- res@metadata$latitude[o]
    res@metadata$time <- res@metadata$time[o] ## FIXME: do all sections have time?
    res@data$station <- res@data$station[o]
    res@processingLog <- processingLogAppend(res@processingLog, paste(deparse(match.call()), sep="", collapse=""))
    res
}

#' @title Make a Section (DEFUNCT)
#'
#' @description
#' This is a defunct function; use \code{\link{as.section}} instead, and
#' see \link{oce-defunct} for more on the oce procedure for retiring functions.
#' @param item Ignored, since this function is defunct
#' @param ... Ignored, since this function is defunct
makeSection <- function(item, ...)
{
    .Defunct("as.section",
             msg="makeSection() was marked 'defunct' in March 2016, after having been marked 'deprecated' for a CRAN release cycle.section() instead. See ?'oce-defunct'.")
##     .Deprecated("as.section",
##                 msg="makeSection() will be removed soon; use as.section() instead. See ?'oce-deprecated'.")
##     if (inherits(item, "ctd")) {
## 	extra.args <- list(...)
## 	numStations <- 1 + length(extra.args)
## 	station <- vector("list", numStations)
## 	stn <- vector("character", numStations)
## 	lon <- vector("numeric", numStations)
## 	lat <- vector("numeric", numStations)
## 	stn[1] <- item@metadata$station
## 	lon[1] <- item@metadata$longitude
## 	lat[1] <- item@metadata$latitude
## 	station[[1]] <- item
## 	if (numStations > 1) {
## 	    for (i in 2:numStations) {
##                 ## message("DAN ", i)
##                 thisStn <- extra.args[[i-1]]
## 		stn[i] <- thisStn@metadata$station
## 		lon[i] <- thisStn@metadata$longitude
## 		lat[i] <- thisStn@metadata$latitude
## 		station[[i]] <- thisStn
## 	    }
## 	}
##     } else if (inherits(item, "list") && !inherits(item[[1]], "oce")) {
##         stop("cannot yet handle a list of non-oce objects")
##     } else if (inherits(item, "list") && inherits(item[[1]], "oce")) {
## 	numStations <- length(item)
## 	station <- vector("list", numStations)
## 	stn <- vector("character", numStations)
## 	lon <- vector("numeric", numStations)
## 	lat <- vector("numeric", numStations)
## 	if (numStations < 1) 
##             stop("need more than 1 item in the list, to create a section")
##         ## 2015-12-06 if (inherits(item[[1]], "oce")) {
##         for (i in 1:numStations) {
##             if (!inherits(item[[i]], "oce"))
##                 stop("list cannot be a mixture of oce and non-oce items")
##             thisItem <- item[[i]]
##             stn[i] <- if (is.null(thisItem@metadata$station)) i else thisItem@metadata$station
##             lon[i] <- thisItem@metadata$longitude
##             lat[i] <- thisItem@metadata$latitude
##             station[[i]] <- thisItem
##         }
##         ## 2015-12-06 } else {
##         ## 2015-12-06: this code block could not be run 
##         ## 2015-12-06 ## demand that items contain @data$pressure
##         ## 2015-12-06 if ("pressure" %in% names(item[[1]]) || "pressure" %in% names(item[[1]]@data)) {
##         ## 2015-12-06     stop("items must contain pressure")
##         ## 2015-12-06 }
##         ## 2015-12-06 for (thisItem in item) {
##         ## 2015-12-06     names <- names(thisItem)
##         ## 2015-12-06     if (!("longitude" %in% names)) stop("each item entry must contain longitude")
##         ## 2015-12-06     if (!("latitude" %in% names)) stop("each item must entry contain latitude")
##         ## 2015-12-06     ## FIXME: maybe permits 'depth' here
##         ## 2015-12-06     if (!("pressure" %in% names)) stop("each item must entry contain pressure")
##         ## 2015-12-06     if (!("station" %in% names)) thisItem$station <- seq_along(thisItem$longitude)
##         ## 2015-12-06     len <- length(thisItem$pressure)
##         ## 2015-12-06     print(names)
##         ## 2015-12-06     names <- names[names!="longitude"]
##         ## 2015-12-06     names <- names[names!="latitude"]
##         ## 2015-12-06     names <- names[names!="station"]
##         ## 2015-12-06     print(names)
##         ## 2015-12-06     data <- list()
##         ## 2015-12-06     for (name in names) {
##         ## 2015-12-06         if (length(name) == len) {
##         ## 2015-12-06             data[[name]] <- thisItem[[name]]
##         ## 2015-12-06         }
##         ## 2015-12-06     }
##         ## 2015-12-06     str(data)
##         ## 2015-12-06 }
##         ## 2015-12-06 }
##     } else if (class(item) == "character") {
##         numStations <- length(item)
## 	station <- vector("list", numStations)
## 	stn <- vector("character", numStations)
## 	lon <- vector("numeric", numStations)
## 	lat <- vector("numeric", numStations)
## 	if (numStations <= 1)
## 	    stop("need more than 1 station to make a section")
## 	if (exists(item[1])) {
## 	    ## ctd objects
## 	    ##oceDebug(1, "ctd objects\n")
## 	    for (i in 1:numStations) {
##                 thisItem <- get(item[[i]])
## 		stn[i] <- thisItem@metadata$station
## 		lon[i] <- thisItem@metadata$longitude
## 		lat[i] <- thisItem@metadata$latitude
## 		station[[i]] <- thisItem
## 	    }
## 	} else {
## 	    ## ctd filenames
## 	    ##oceDebug(1, "ctd files\n")
## 	    for (i in 1:numStations) {
## 		##oceDebug(1, "file named", item[i], "\n")
## 		ctd <- read.ctd(item[i])
## 		stn[i] <- ctd@metadata$station
## 		lon[i] <- ctd@metadata$longitude
##                 lat[i] <- ctd@metadata$latitude
##                 station[[i]] <- ctd
## 	    }
## 	}
##     } else {
## 	stop("first argument must be a \"ctd\" object, a \"list\" of ctd objects, or a vector of character strings naming ctd objects")
##     }
##     res <- new("section")
##     res@metadata$sectionId <- ""
##     res@metadata$stationId <- stn
##     res@metadata$longitude <- lon
##     res@metadata$latitude <- lat
##     res@data <- list(station=station)
##     res@processingLog <- processingLogAppend(res@processingLog, paste(deparse(match.call()), sep="", collapse=""))
##     res
}


#' @title Add a CTD Station to a Section
#' 
#' @description
#' Add a CTD profile to an existing section.
#'
#' @section Historical note:
#' Until March 2015, this operation was carried out with the \code{+} operator,
#' but at that time, the syntax was flagged by the development version of R, so it
#' was changed to the present form.
#' 
#' @param section A section to which a station is to be added.
#' 
#' @param station A ctd object holding data for the station to be added.
#' 
#' @aliases sectionAddCtd
#' @return An object of \code{\link[base]{class}} \code{section}.
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
#' @family things related to \code{section} data
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


#' @title Plot a Section
#' 
#' @description
#' Creates a summary plot for a CTD section, with one panel for each value of
#' \code{which}.
#'
#' @details
#' The type of plot is governed by \code{which}, as follows.
#' 
#' \itemize{  
#'     \item \code{which=1} or \code{"temperature"} for temperature contours (the default)
#'     \item \code{which=2} or \code{"salinity"} for salinity contours
#'     \item \code{which=3} or \code{"sigmaTheta"} for sigma-theta contours
#'     \item \code{which=4} or \code{"nitrate"} for nitrate concentration contours
#'     \item \code{which=5} or \code{"nitrite"} for nitrite concentration contours
#'     \item \code{which=6} or \code{"oxygen"} for oxygen concentration  contours
#'     \item \code{which=7} or \code{"phosphate"} for phosphate concentration contours
#'     \item \code{which=8} or \code{"silicate"} for silicate concentration contours
#'     \item \code{which=9} or \code{"u"} for eastward velocity
#'     \item \code{which=10} or \code{"uz"} for vertical derivative of eastward velocity
#'     \item \code{which=11} or \code{"v"} for northward velocity
#'     \item \code{which=12} or \code{"vz"} for vertical derivative of northward velocity
#'     \item \code{which=20} or \code{"data"} for a dot for each data location
#'     \item \code{which=99} or \code{"map"} for a location map
#' }
#' 
#' The y-axis for the contours is pressure, plotted in the conventional reversed
#' form, so that the water surface appears at the top of the plot.  The x-axis is
#' more complicated. If \code{at} is not supplied, then the routine calculates x
#' as the distance between the first station in the section and each of the other
#' stations. (This will produce an error if the stations are not ordered
#' geographically, because the \code{\link{contour}} routine cannot handle
#' non-increasing axis coordinates.) If \code{at} is specified, then it is taken
#' to be the location, in arbitrary units, along the x-axis of labels specified by
#' \code{labels}; the way this works is designed to be the same as for
#' \code{\link{axis}}.
#' 
#' 
#' @param x a \code{section} object, e.g. as created by \code{\link{as.section}}
#' or \code{\link{read.section}}.
#' 
#' @param which a list of desired plot types, as explained in \dQuote{Details}.
#' There may be up to four panels in total, and the desired plots are placed in
#' these panels, in reading order.  If only one panel is plotted, \code{par} is
#' not adjusted, which makes it easy to add to the plot with subsequent plotting
#' commands.
#' 
#' @param eos Character string indicating the equation of state to be used, either
#' \code{"unesco"} or \code{"gsw"}.
#' 
#' @param at If \code{NULL} (the default), the x axis will indicate the distance
#' of the stations from the first in the section.  (This may give errors in the
#' contouring routine, if the stations are not present in a geographical order.)
#' If a list, then it indicates the values at which stations will be plotted.
#' 
#' @param labels Either a logical, indicating whether to put labels on the x axis,
#' or a vector that is a list of labels to be placed at the x positions indicated
#' by \code{at}. 
#' 
#' @param grid If \code{TRUE}, points are drawn at data locations.
#' 
#' @param contourLevels Optional contour levels.
#' 
#' @param contourLabels Optional contour labels.
#' 
#' @param stationIndices Optional list of the indices of stations to use.  Note
#' that an index is \emph{not} a station number, e.g. to show the first 4
#' stations, use \code{station.indices=1:4}.
#' 
#' @param coastline String giving the coastline to be used in a station map, or
#' \code{"best"} to pick the one with highest resolution, or \code{"none"} to
#' avoid drawing the coastline.
#' 
#' @param xlim Optional limit for x axis (only in sections, not map).
#' 
#' @param ylim Optional limit for y axis (only in sections, not map)
#' 
#' @param map.xlim,map.ylim Optional limits for station map; \code{map.ylim} is
#' ignored if \code{map.xlim} is provided.
#' 
#' @param clongitude,clatitude,span Optional map centre position and span (km).
#' 
#' @param projection Parameter specifying map
#' projection; see \code{\link{mapPlot}}.  If \code{projection="automatic"},
#' however, a projection is devised from the data, with \code{stereographic} if
#' the mean latitude exceeds 70N and \code{mollweide} otherwise.
#' 
#' @param xtype Type of x axis, for contour plots, either \code{"distance"} for
#' distance (in km) to the first point in the section, \code{"track"} for distance
#' along the cruise track, \code{"longitude"}, \code{"latitude"}, or
#' \code{"time"}.  Note that if the x values are not in order, they will be put in
#' order (which may make no sense) and a warning will be printed.
#' 
#' @param ytype Type of y axis for contour plots, either \code{"pressure"} for
#' pressure (in dbar, with zero at the surface) or \code{"depth"} for depth (in m
#' below the surface, calculated from pressure with \code{\link{swDepth}}).
#' 
#' @param ztype String indicating whether to use contours, an image, or points.
#' In the first two cases, the data must be gridded, with identical pressures at
#' each station.
#'     
#' @param zbreaks,zcol Breaks and colours to be used if \code{ztype="points"} or
#' \code{"image"}.  If not provided, a reasonable default is chosen.  If
#' \code{zcol} is a function, it will be invoked with an argument equal to
#' \code{1+length(zbreaks)}.  If \code{zbreaks} is not given, it defaults to a
#' vector of length 200, with values spanning the data range.
#' 
#' @param legend.loc Location of legend, as supplied to \code{\link{legend}}, or
#' set to the empty string to avoid plotting a legend.
#' 
#' @param adorn List of expressions to be executed for the panels in turn, e.g. to
#' adorn the plots.  If the number matches the number of panels, then the strings
#' are applied to the appropriate panels, as they are drawn from top-left to
#' bottom-right.   If only a single expression is provided, it is used for all
#' panels.  (See \dQuote{Examples}.)
#' 
#' @param showStations Logical indicating whether to draw station numbers on maps.
#' 
#' @param showStart Logical indicating whether to indicate the first station with
#' a different symbol than the others.
#' 
#' @param showBottom Logical indicating whether to draw the bottom, or a character
#' string indicating the method for plotting the bottom.  The allowed methods are:
#' \code{polygon}, which fills the space to the bottom, or \code{lines}, which
#' draws lines from stations to the bottom, or \code{points}, which draws points
#' at the bottom.
#' 
#' @param axes Logical value indicating whether to draw axes.
#' 
#' @param mgp 3-element numerical vector to use for \code{par(mgp)}, and also for
#' \code{par(mar)}, computed from this.  The default is tighter than the R
#' default, in order to use more space for the data and less for the axes.
#' 
#' @param mar Value to be used with \code{\link{par}("mar")}.
#' 
#' @param col Colour, as in \code{\link{par}("col")}.
#' 
#' @param cex Value to be used with \code{\link{par}("cex")}, for any use of
#' \code{\link{points}}, e.g. for \code{which="data"}.
#' 
#' @param pch Value to be used with \code{\link{par}("pch")} (see \code{cex},
#' above.)
#' 
#' @param debug A flag that turns on debugging.  Set to 1 to get a moderate amount
#' of debugging information, or to 2 to get more.
#' 
#' @param ... Optional arguments passed to the contouring function, e.g. using
#' \code{labcex=1} will increase the size of contour labels.
#' 
#' 
#' @return None.
#' 
#' @seealso The documentation for \code{\link{section-class}} explains the
#' structure of section objects, and also outlines the other functions dealing
#' with them.
#' 
#' 
#' @examples
#' library(oce)
#' data(section)
#' sg <- sectionGrid(section)
#' 
#' ## AO3 section
#' plot(sg, which="salinity", ztype="points", pch=20, cex=1.5) 
#' 
#' ## Gulf Stream
#' GS <- subset(section, 109<=stationId&stationId<=129)
#' GSg <- sectionGrid(GS, p=seq(0,2000,100))
#' plot(GSg, which=c(1,99), map.ylim=c(34,42))
#' 
#' par(mfrow=c(2,1))
#' plot(GS, which=1, ylim=c(2000, 0), ztype='points',
#' zbreaks=seq(0,30,2), pch=20, cex=3)
#' plot(GSg, which=1, ztype='image', zbreaks=seq(0,30,2))
#' 
#' ## image, with coloured dots to show if grid smoothing was OK
#' plot(GSg, which=1, ztype='image')
#' T <- GS[['temperature']]
#' col <- oce.colorsJet(100)[rescale(T, rlow=1, rhigh=100)]
#' points(GS[['distance']],GS[['depth']],pch=20,cex=3,col='white')
#' points(GS[['distance']],GS[['depth']],pch=20,cex=2.5,col=col)
#' 
#' @author Dan Kelley
#' 
#' @family functions that plot \code{oce} data
#' @family things related to \code{section} data
setMethod(f="plot",
          signature=signature("section"),
          definition=function(x,
                              which=c(1, 2, 3, 99),
                              eos=getOption("oceEOS", default="gsw"),
                              at=NULL,
                              labels=TRUE,
                              grid=FALSE,
                              contourLevels=NULL,
                              contourLabels=NULL,
                              stationIndices,
                              coastline=c("best", "coastlineWorld", "coastlineWorldMedium",
                                          "coastlineWorldFine", "none"),
                              xlim=NULL, ylim=NULL,
                              map.xlim=NULL, map.ylim=NULL,
                              clongitude, clatitude, span,
                              projection=NULL,
                              xtype=c("distance", "track", "longitude", "latitude", "time"),
                              ytype=c("depth", "pressure"),
                              ztype=c("contour", "image", "points"),
                              zbreaks=NULL, zcol=NULL,
                              legend.loc="bottomright",
                              adorn=NULL,
                              showStations=FALSE,
                              showStart=TRUE,
                              showBottom=TRUE,
                              axes=TRUE,
                              mgp=getOption("oceMgp"),
                              mar=c(mgp[1]+1, mgp[1]+1, mgp[2]+1, mgp[2]+0.5),
                              col=par("col"), cex=par("cex"), pch=par("pch"),
                              debug=getOption("oceDebug"),
                              ...)
          {
              debug <- if (debug > 4) 4 else floor(0.5 + debug)
              ##> message("section.R:434, station 1 pressure: ",
              ##>         paste(x@data$station[[1]]@data$pressure, collapse=" "))
              xtype <- match.arg(xtype)
              ytype <- match.arg(ytype)
              ztype <- match.arg(ztype)
              drawPoints <- ztype == "points"
              coastline <- match.arg(coastline)

              ## Make 'which' be numeric, to simplify following code
              ##oceDebug(debug, "which=c(", paste(which, collapse=","), ")\n")
              lw <- length(which)
              whichOriginal <- which
              which <- oce.pmatch(which,
                                  list(temperature=1, salinity=2, 
                                       sigmaTheta=3, nitrate=4, nitrite=5, oxygen=6,
                                       phosphate=7, silicate=8, 
                                       u=9, uz=10, v=11, vz=12, # lowered adcp
                                       data=20, map=99))
              oceDebug(debug, "which=c(", paste(which, collapse=","), ")\n")
              oceDebug(debug, "plot.section(, ..., which=c(",
                       paste(which, collapse=","), "), eos=\"", eos,
                       "\", ztype=\"", ztype, "\", ...) {\n", sep="", unindent=1)
              ##> message("section.R:456, station 1 pressure: ",
              ##>         paste(x@data$station[[1]]@data$pressure, collapse=" "))
              ##> message("!is.na(which[1]): ", !is.na(which[1]))
              ##> message("which!='data'", which!='data')
              ##> message("which!='map'", which!='map')
              ## Ensure data on levels, for plots requiring pressure (e.g. sections)
              if (is.na(which[1]) || which != "data" || which != 'map') {
                  p1 <- x[["station", 1]][["pressure"]]
                  np1 <- length(p1)
                  numStations <- length(x@data$station)
                  for (ix in 2:numStations) {
                      ##> message("ix: ", ix)
                      thisStation <- x@data$station[[ix]]
                      thisPressure <- thisStation[["pressure"]]
                      if ("points" != ztype && 
                          np1 != length(thisPressure) ||
                          any(p1 != x[["station", ix]][["pressure"]])) {
                          ##> message("before gridding: p: ", paste(x@data$station[[1]]@data$pressure, collapse=" "))
                          x <- sectionGrid(x, debug=debug-1)
                          ##> message("after gridding: p: ", paste(x@data$station[[1]]@data$pressure, collapse=" "))
                          ##warning("plot.section() gridded the data for plotting", call.=FALSE)
                          break
                      }
                  }
              }

              ## Trim stations that have zero good data FIXME: brittle to addition of new metadata
              haveData <- sapply(x@data$station,
                                 function(stn) 0 < length(stn[['pressure']]))
              x@data$station <- x@data$station[haveData]
              x@metadata$stationId <- x@metadata$stationId[haveData]
              x@metadata$latitude <- x@metadata$latitude[haveData]
              x@metadata$longitude <- x@metadata$longitude[haveData]
              x@metadata$time <- x@metadata$time[haveData]
              ##> message("section.R:487, station 1 pressure: ",
              ##>         paste(x@data$station[[1]]@data$pressure, collapse=" "))
              plotSubsection <- function(xx, yy, zz, which.xtype, which.ytype,
                                         variable="temperature", vtitle="T",
                                         eos=getOption("oceEOS", default="gsw"),
                                         indicate.stations=TRUE, contourLevels=NULL, contourLabels=NULL,
                                         xlim=NULL,
                                         ylim=NULL,
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
                  oceDebug(debug, "plotSubsection(variable=\"", variable, "\", eos=\"", eos, "\", ztype=\"", ztype, "\", zcol=", if (missing(zcol)) "(missing)" else "(provided)",
                           ", axes=", axes, ", ...) {\n", sep="", unindent=1)
                  ztype <- match.arg(ztype)
                  drawPoints <- "points" == ztype
                  omar <- par('mar')
                  xIsTime <- inherits(xx, "POSIXt")

                  if (variable == "map") {
                      lat <- array(NA_real_, numStations)
                      lon <- array(NA_real_, numStations)
                      for (i in 1:numStations) {
                          thisStation <- x[["station", stationIndices[i]]]
                          lon[i] <- thisStation[["longitude"]]
                          lat[i] <- thisStation[["latitude"]]
                      }
                      lon[lon<0] <- lon[lon<0] + 360
                      asp <- 1 / cos(mean(range(lat,na.rm=TRUE))*pi/180)
                      latm <- mean(lat, na.rm=TRUE)
                      lonm <- mean(lon, na.rm=TRUE)
                      if (missing(span)) {
                          lonr <- lonm + sqrt(2) * (range(lon, na.rm=TRUE) - mean(lon, na.rm=TRUE)) # expand range
                          latr <- latm + sqrt(2) * (range(lat, na.rm=TRUE) - mean(lat, na.rm=TRUE))
                      } else {
                          ## FIXME: the sqrt(2) below helps in a test case ... not sure it make sense though --DK
                          lonr <- lonm + span / 111.1 * c(-0.5, 0.5) / cos(2*pi/180*latm) / sqrt(2)
                          latr <- latm + span / 111.1 * c(-0.5, 0.5) / sqrt(2)
                          ##message("latr:");print(latr)
                      }

                      ## FIXME: this coastline code is reproduced in section.R; it should be DRY
                      haveCoastline <- FALSE
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
                          mapPlot(coastline, longitudelim=map.xlim, latitudelim=map.ylim, projection=projection, fill='gray')
                          mapPoints(x[['longitude', 'byStation']], x[['latitude', 'byStation']],
                                    col=col, pch=3, lwd=1/2)
                          if (xtype == "distance" && showStart) {
                              mapPoints(lon[1], lat[1], col=col, pch=22, cex=3*par("cex"), lwd=1/2)
                          }
                          return()
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
                              ## message("about to plot; lonr=");print(lonr)
                              ## message("about to plot; latr=");print(latr)
                              plot(lonr, latr, asp=asp, type='n',
                                   xlab=gettext("Longitude", domain="R-oce"),
                                   ylab=gettext("Latitude", domain="R-oce"))
                          }
                      }
                      if (haveCoastline) {
                          if (!is.null(coastline@metadata$fillable) && coastline@metadata$fillable) {
                              polygon(coastline[["longitude"]], coastline[["latitude"]], col="lightgray", lwd=3/4)
                              polygon(coastline[["longitude"]]+360, coastline[["latitude"]], col="lightgray", lwd=3/4)
                          } else {
                              lines(coastline[["longitude"]], coastline[["latitude"]], col="darkgray")
                              lines(coastline[["longitude"]]+360, coastline[["latitude"]], col="darkgray")
                          }
                      }
                      ## add station data
                      lines(lon, lat, col="lightgray")
                      ## replot with shifted longitude
                      col <- if("col" %in% names(list(...))) list(...)$col else "black"
                      points(lon, lat, col=col, pch=3, lwd=1/2)
                      points(lon - 360, lat, col=col, pch=3, lwd=1/2)
                      if (showStations) {
                          stationId <- x[['station ID']]
                          text(lon, lat, stationId, pos=2)
                          text(lon-360, lat, stationId, pos=2)
                      }
                      if (xtype == "distance" && showStart) {
                          points(lon[1], lat[1], col=col, pch=22, cex=3*par("cex"), lwd=1/2)
                          points(lon[1] - 360, col=col, lat[1], pch=22, cex=3*par("cex"), lwd=1/2)
                      }
                      if (indicate.stations) {
                          dy <- 5 * mean(diff(sort(x@metadata$latitude)),na.rm=TRUE)
                          dx <- 5 * mean(diff(sort(x@metadata$longitude)),na.rm=TRUE)
                          ylab <- x@metadata$latitude[1]  - dy * sign(x@metadata$latitude[2]  - x@metadata$latitude[1])
                          xlab <- x@metadata$longitude[1] - dx * sign(x@metadata$longitude[2] - x@metadata$longitude[1])
                          text(xlab, ylab, x@metadata$stationId[1])
                          xlab <- x@metadata$longitude[numStations] - dx * sign(x@metadata$longitude[numStations-1] - x@metadata$longitude[numStations])
                          ylab <- x@metadata$latitude[numStations]  - dy * sign(x@metadata$latitude[numStations-1]  - x@metadata$latitude[numStations])
                          text(xlab, ylab, x@metadata$stationId[numStations])
                      }
                  } else {                        
                      ## not a map
                      ## if (!(variable %in% names(x@data$station[[1]]@data)) && variable != "data") {
                      ##     stop("this section does not contain a variable named '", variable, "'")
                      ## }

                      if (drawPoints || ztype == "image") {
                          if (is.null(zbreaks)) {
                              zRANGE <- range(x[[variable]], na.rm=TRUE)
                              if (is.null(zcol) || is.function(zcol)) {
                                  zbreaks <- seq(zRANGE[1], zRANGE[2], length.out=200)
                              } else {
                                  zbreaks <- seq(zRANGE[1], zRANGE[2], length.out=length(zcol) + 1)
                              }
                          }
                          nbreaks <- length(zbreaks)
                          if (is.null(zcol)) 
                              zcol <- oce.colorsJet(nbreaks - 1)
                          if (is.function(zcol))
                              zcol <- zcol(nbreaks - 1)
                          zlim <- range(zbreaks)
                          drawPalette(zlim=range(zbreaks), breaks=zbreaks, col=zcol)
                      }


                      ## FIXME: contours don't get to plot edges
                      xxrange <- range(xx, na.rm=TRUE)
                      yyrange <- range(yy, na.rm=TRUE)
                      ##yyrange[1] <- -1

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
                                             gettext("Time", domain="R-oce"))
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
                              oceDebug(debug, "drawing axes\n")
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
                          ##oceDebug(debug, "filling matrix for station", i, "\n")
                          if (variable != "data") {
                              if (drawPoints) {
                                  p <- x@data$station[[stationIndices[i]]]@data$pressure
                                  if (eos == "teos" && variable == "temperature")
                                      v <- swConservativeTemperature(x@data$station[[stationIndices[i]]])
                                  else if (eos == "teos" && variable == "salinity")
                                      v <- swAbsoluteSalinity(x@data$station[[stationIndices[i]]])
                                  else
                                      v <- x@data$station[[stationIndices[i]]]@data[[variable]]
                                  points(rep(xx[i], length(p)), -p,
                                         pch=pch, cex=cex,
                                         col=zcol[rescale(v, xlow=zlim[1], xhigh=zlim[2], rlow=1, rhigh=nbreaks)])
                              } else {
                                  if (eos == "teos" && variable == "temperature") {
                                      zz[i,] <- rev(swConservativeTemperature(x@data$station[[stationIndices[i]]]))
                                  } else if (eos == "teos" && variable == "salinity") {
                                      zz[i,] <- rev(swAbsoluteSalinity(x@data$station[[stationIndices[i]]]))
                                  } else {
                                      zz[i,] <- rev(x@data$station[[stationIndices[i]]][[variable]])
                                  }
                              }
                          }
                          if (grid && !drawPoints)
                              points(rep(xx[i], length(yy)), yy, col="gray", pch=20, cex=1/3)
                          temp <- x@data$station[[stationIndices[i]]]@data$temperature
                          len <- length(temp)
                          if ("waterDepth" %in% names(x@data$station[[stationIndices[i]]]@metadata)
                              && is.finite(x@data$station[[stationIndices[i]]]@metadata$waterDepth)) {
                              wd <- x@data$station[[stationIndices[i]]]@metadata$waterDepth
                              ##oceDebug(debug, "known waterDepth", wd, "for station i=", i, "\n")
                          } else {
                              wd <- NA
                              ## 20160116 if (is.na(temp[len])) {
                              ## 20160116     wdi <- len - which(!is.na(rev(temp)))[1] + 1
                              ## 20160116     wd <- max(x@data$station[[stationIndices[i]]]@data$pressure, na.rm=TRUE)
                              ## 20160116     message("FAKE waterDepth:", wd, ", station:", i)
                              ## 20160116     ##oceDebug(debug, "inferred waterDepth", wd, "for station i=", i, "\n")
                              ## 20160116 } else {
                              ## 20160116     ##oceDebug(debug, "cannot infer waterDepth for station i=", i, "\n")
                              ## 20160116 }
                          }
                          in.land <- which(is.na(x@data$station[[stationIndices[i]]]@data$temperature[-3])) # skip first 3 points
                          waterDepth <- c(waterDepth, wd)
                          ## 20160116: it's a bad idea guessing on the water depth (e.g. argo)
                          ## if (!is.na(wd)) {
                          ##     waterDepth <- c(waterDepth, wd)
                          ## } else {
                          ##     waterDepth <- c(waterDepth, max(x@data$station[[stationIndices[i]]]@data$pressure, na.rm=TRUE))
                          ## }
                      }

                      ##oceDebug(debug, "waterDepth=c(", paste(waterDepth, collapse=","), ")\n")
                      ##waterDepth <- -waterDepth
                      if (!grid && axes)
                          Axis(side=3, at=xx, labels=FALSE, tcl=-1/3, lwd=0.5) # station locations
                      bottom.x <- c(xx[1], xx, xx[length(xx)])
                      bottom.y <- c(graph.bottom, -waterDepth, graph.bottom)
                      ##cat("bottom.x: (length", length(bottom.x),")");print(bottom.x)
                      ##cat("bottom.y: (length", length(bottom.y),")");print(bottom.y)

                      dots <- list(...) # adjust plot parameter labcex, unless user did

                      ##par(xaxs="i", yaxs="i")

                      ## Put x in order, if it's not already
                      ox <- order(xx)
                      xxOrig <- xx
                      if (any(xx[ox] != xx)) {
                          xx <- xx[ox]
                          zz <- zz[ox,] ## FIXME keep this???
                          ##warning("plot.section() reordered the stations to make x monotonic")
                          bottom.x <- c(min(xxOrig), xxOrig[ox], max(xxOrig))
                          bottom.y <- c(graph.bottom, -waterDepth[ox], graph.bottom)
                      }

                      ## cannot contour with duplicates in x or y; the former is the only problem
                      xx.unique <- c(TRUE, 0 != diff(xx))
                      yy.unique <- c(TRUE, 0 != diff(yy))
                      xx.unique <- xx.unique & !is.na(xx.unique)
                      yy.unique <- yy.unique & !is.na(yy.unique)
                      ## a problem with mbari data revealed that we need to chop NA valaues too
                      if (variable == "data") {
                          for (i in 1:numStations) {
                              thisStation <- x[["station", i]]
                              pressure <- thisStation[["pressure"]]
                              if (which.xtype == 4) {
                                  longitude <- thisStation[["longitude"]]
                                  points(rep(longitude, length(pressure)), -pressure, cex=cex, pch=pch, col=col)
                              } else {
                                  ## FIXME: shouldn't the next line work for all types??
                                  points(rep(xx[i], length(pressure)), -pressure, cex=cex, pch=pch, col=col)
                              }
                          }
                      } else if (!drawPoints) {
                          zrange <- range(zz[xx.unique,yy.unique], na.rm=TRUE)
                          if (!is.null(contourLevels) && !is.null(contourLabels)) {
                              oceDebug(debug, "user-supplied contourLevels: ", contourLevels, "\n")
                              if (!("labcex" %in% dots$labcex)) {
                                  if (ztype == 'contour') {
                                      contour(x=xx[xx.unique], y=yy[yy.unique], z=zz[xx.unique,yy.unique],
                                              axes=FALSE, add=TRUE, labcex=0.8,
                                              levels=contourLevels, labels=contourLabels,
                                              col=col,
                                              xaxs="i", yaxs="i",
                                              ...)
                                  } else if (ztype == "image") {
                                      zz[zz < min(zbreaks)] <- min(zbreaks)
                                      zz[zz > max(zbreaks)] <- max(zbreaks)
                                      if (is.function(zcol))
                                          zcol <- zcol(1+length(zbreaks))
                                      .filled.contour(x=xx[xx.unique], y=yy[yy.unique], z=zz[xx.unique,yy.unique],
                                                      levels=zbreaks, col=zcol)
                                  } else {
                                      stop("unknown ztype: \"", ztype, "\" [1]")
                                  }
                              } else {
                                  if (ztype == 'contour') {
                                  contour(x=xx[xx.unique], y=yy[yy.unique], z=zz[xx.unique,yy.unique],
                                          axes=FALSE, add=TRUE,
                                          levels=contourLevels, labels=contourLabels,
                                          col=col,
                                          xaxs="i", yaxs="i",
                                          ...)
                                  } else if (ztype == "image") {
                                      zz[zz < min(zbreaks)] <- min(zbreaks)
                                      zz[zz > max(zbreaks)] <- max(zbreaks)
                                      if (is.function(zcol))
                                          zcol <- zcol(1+length(zbreaks))
                                      .filled.contour(x=xx[xx.unique], y=yy[yy.unique], z=zz[xx.unique,yy.unique],
                                                      levels=zbreaks, col=zcol)
                                  } else {
                                      stop("unknown ztype: \"", ztype, "\" [2]")
                                  }
                              }
                          } else {
                              oceDebug(debug, "automatically-calculated contourLevels\n")
                              if (is.null(dots$labcex)) {
                                  if (ztype == 'contour') {
                                      ##> message("yy: ", paste(round(yy), collapse=" "), " (section.R near line 865); length=", length(yy))
                                      contour(x=xx[xx.unique], y=yy[yy.unique], z=zz[xx.unique,yy.unique],
                                              labcex=0.8, add=TRUE, col=col, ...)
                                  } else if (ztype == "image") {
                                      zz[zz < min(zbreaks)] <- min(zbreaks)
                                      zz[zz > max(zbreaks)] <- max(zbreaks)
                                      ## FIXME: testing here
                                      if (is.function(zcol))
                                          zcol <- zcol(1+length(zbreaks))
                                      .filled.contour(x=xx[xx.unique], y=yy[yy.unique], z=zz[xx.unique,yy.unique],
                                                      levels=zbreaks, col=zcol)
                                  } else if (ztype == "points") {
                                      ## nothing to do now
                                  } else {
                                      stop("unknown ztype: \"", ztype, "\" [3]")
                                  }
                              } else {
                                  if (ztype == 'contour') {
                                      contour(x=xx[xx.unique], y=yy[yy.unique], z=zz[xx.unique,yy.unique],
                                              axes=FALSE,
                                              add=TRUE,
                                              col=col,
                                              xaxs="i", yaxs="i",
                                              ...)
                                  } else if (ztype == "image") {
                                      zz[zz < min(zbreaks)] <- min(zbreaks)
                                      zz[zz > max(zbreaks)] <- max(zbreaks)
                                      if (is.function(zcol))
                                          zcol <- zcol(1+length(zbreaks))
                                      .filled.contour(x=xx[xx.unique], y=yy[yy.unique], z=zz[xx.unique,yy.unique],
                                                      levels=zbreaks, col=zcol)
                                  } else {
                                      stop("unknown ztype: \"", ztype, "\" [4]")
                                  }
                              }
                          }
                      }
                      if (is.character(showBottom) || showBottom) {
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
                      if (nchar(legend.loc))
                          legend(legend.loc, legend=vtitle, bg="white", x.intersp=0, y.intersp=0.5,cex=1)
                      ##lines(xx, -waterDepth[ox], col='red')

                      ## undo negation of the y coordinate, so further can can make sense
                      usr <- par('usr')
                      par('usr'=c(usr[1], usr[2], -usr[3], usr[4]))
                  }
                  par(mar=omar)
                  oceDebug(debug, "} # plotSubsection()\n", unindent=1)
              }                        # plotSubsection()
              if (!inherits(x, "section"))
                  stop("method is only for objects of class '", "section", "'")
              opar <- par(no.readonly = TRUE)
              if (length(which) > 1) on.exit(par(opar))
              which.xtype <- pmatch(xtype, c("distance", "track", "longitude", "latitude", "time"), nomatch=0)
              if (0 == which.xtype)
                  stop('xtype must be one of: "distance", "track", "longitude", "latitude" or "time"')
              xtype <- c("distance", "track", "longitude", "latitude", "time")[which.xtype]
              which.ytype <- pmatch(ytype, c("pressure", "depth"), nomatch=0)
              if (missing(stationIndices)) {
                  numStations <- length(x@data$station)
                  stationIndices <- 1:numStations
              } else {
                  numStations <- length(stationIndices)
              }
              if (numStations < 2)
                  stop("cannot plot a section containing fewer than 2 stations")

              firstStation <- x@data$station[[stationIndices[1]]]
              num.depths <- length(firstStation@data$pressure)

              ## Ensure data on levels, for plot types requiring that
              ##if (length(which) > 1 || (which != "data" && which != 'map')) { ## FIXME: why the length here?
              ##    p1 <- firstStation@data$pressure
              ##    np1 <- length(p1)
              ##    for (ix in 2:numStations) {
              ##        thisStation <- x@data$station[[stationIndices[ix]]]
              ##        thisPressure <- thisStation[["pressure"]]
              ##        if ("points" != ztype && (np1 != length(thisPressure) || any(p1 != x@data$station[[stationIndices[ix]]]@data$pressure))) {
              ##            stop("plot.section() requires stations to have identical pressure levels.\n  Please use e.g.\n\tsectionGridded <- sectionGrid(section)\n  to create a uniform grid, and then you will be able to plot the section.", call.=FALSE)
              ##        }
              ##    }
              ##}

              zz <- matrix(nrow=numStations, ncol=num.depths)
              xx <- array(NA_real_, numStations)
              yy <- array(NA_real_, num.depths)
              if (is.null(at)) {
                  lon0 <- firstStation@metadata$longitude
                  lat0 <- firstStation@metadata$latitude
                  for (ix in 1:numStations) {
                      j <- stationIndices[ix]
                      if (which.xtype == 1) {
                          xx[ix] <- geodDist(lon0, lat0,
                                             x@data$station[[j]]@metadata$longitude,
                                             x@data$station[[j]]@metadata$latitude)
                      } else if (which.xtype == 2) {
                          if (ix == 1) {
                              xx[ix] <- 0
                          } else {
                              xx[ix] <- xx[ix-1] + geodDist(x@data$station[[stationIndices[ix-1]]]@metadata$longitude,
                                                            x@data$station[[stationIndices[ix-1]]]@metadata$latitude,
                                                            x@data$station[[j]]@metadata$longitude,
                                                            x@data$station[[j]]@metadata$latitude)
                          }
                      } else if (which.xtype == 3) {
                          xx[ix] <- x@data$station[[j]]@metadata$longitude
                      } else if (which.xtype == 4) {
                          xx[ix] <- x@data$station[[j]]@metadata$latitude
                      } else if (which.xtype == 5) {
                          ## use ix as a desparate last measure, if there are no times.
                          if (is.null(x@data$station[[j]]@metadata$startTime)) {
                              xx[ix] <- ix
                              if (ix == 1)
                                  warning("In plot,section-method() :\n  section stations do not contain startTime; using integers for time axis",
                                          call.=FALSE)
                          } else {
                              xx[ix] <- as.POSIXct(x@data$station[[j]]@metadata$startTime)
                          }
                      } else {
                          stop('unknown xtype; it must be one of: "distance", "track", "longitude", "latitude", or "time"')
                      }
                  }
              } else {
                  xx <- at
              }
              ##> message("which.xtype: ", which.xtype)
              if (which.xtype==5)
                  xx <- numberAsPOSIXct(xx)
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
                      yy <- c(-max(x[["pressure"]]), 0)
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
              ##> message("CHECK(section.R:1034) yy: ", paste(round(yy), " "))
              ##> message("station 1 pressure: ", paste(x@data$station[[1]]@data$pressure, collapse=" "))
              par(mgp=mgp, mar=mar)
              if (lw > 1) {
                  if (lw > 2)
                      layout(matrix(1:4, nrow=2, byrow=TRUE))
                  else
                      layout(matrix(1:2, nrow=2, byrow=TRUE))
              }
              adorn.length <- length(adorn)
              if (adorn.length == 1) {
                  adorn <- rep(adorn, lw)
                  adorn.length <- lw
              }
              for (w in 1:lw) {
                  if (!missing(contourLevels)) {
                      contourLabels <- format(contourLevels)
                      if (is.na(which[w])) {
                          plotSubsection(xx, yy, zz, which.xtype, which.ytype,
                                         whichOriginal[w], whichOriginal[w], eos=eos, ylab="",
                                         levels=contourLevels, labels=contourLabels, xlim=xlim, ylim=ylim, ztype=ztype,
                                         axes=axes, col=col, debug=debug-1, ...)
                      } else if (which[w] == 1) {
                          plotSubsection(xx, yy, zz, which.xtype, which.ytype,
                                         "temperature", if (eos=="unesco") "T" else expression(Theta), eos=eos, ylab="",
                                         levels=contourLevels, labels=contourLabels, xlim=xlim, ylim=ylim, ztype=ztype,
                                         axes=axes, col=col, debug=debug-1, ...)
                      } else if (which[w] == 2) {
                          plotSubsection(xx, yy, zz, which.xtype, which.ytype,
                                         "salinity", if (eos=="unesco") "S" else expression(S[A]), eos=eos, ylab="",
                                         levels=contourLevels, labels=contourLabels,  xlim=xlim, ylim=ylim,
                                         axes=axes, col=col, debug=debug-1, ...)
                      } else if (which[w] == 3) {
                          plotSubsection(xx, yy, zz, which.xtype, which.ytype,
                                         "sigmaTheta", expression(sigma[theta]),
                                         levels=contourLevels, labels=contourLabels, xlim=xlim, ylim=ylim,
                                         axes=axes, col=col, debug=debug-1, ...)
                      } else if (which[w] == 4) {
                          plotSubsection(xx, yy, zz, which.xtype, which.ytype,
                                         "nitrate", "nitrate",
                                         levels=contourLevels, labels=contourLabels,  xlim=xlim, ylim=ylim,
                                         axes=axes, col=col, debug=debug-1, ...)
                      } else if (which[w] == 5) {
                          plotSubsection(xx, yy, zz, which.xtype, which.ytype,
                                         "nitrite", "nitrite",
                                         levels=contourLevels, labels=contourLabels,  xlim=xlim, ylim=ylim,
                                         axes=axes, col=col, debug=debug-1, ...)
                      } else if (which[w] == 6) {
                          plotSubsection(xx, yy, zz, which.xtype, which.ytype,
                                         "oxygen", "oxygen",
                                         levels=contourLevels, labels=contourLabels,  xlim=xlim, ylim=ylim,
                                         axes=axes, col=col, debug=debug-1, ...)
                      } else if (which[w] == 7) {
                          plotSubsection(xx, yy, zz, which.xtype, which.ytype,
                                         "phosphate", "phosphate",
                                         levels=contourLevels, labels=contourLabels, xlim=xlim, ylim=ylim,
                                         axes=axes, col=col, debug=debug-1, ...)
                      } else if (which[w] == 8) {
                          plotSubsection(xx, yy, zz, which.xtype, which.ytype,
                                         "silicate", "silicate",
                                         levels=contourLevels, labels=contourLabels,  xlim=xlim, ylim=ylim,
                                         axes=axes, col=col, debug=debug-1, ...) 
                      } else if (which[w] == 9) {
                          plotSubsection(xx, yy, zz, which.xtype, which.ytype,
                                         "u", "u",
                                         levels=contourLevels, labels=contourLabels,  xlim=xlim, ylim=ylim,
                                         axes=axes, col=col, debug=debug-1, ...) 
                      } else if (which[w] == 10) {
                          plotSubsection(xx, yy, zz, which.xtype, which.ytype,
                                         "uz", "du/dz",
                                         levels=contourLevels, labels=contourLabels,  xlim=xlim, ylim=ylim,
                                         axes=axes, col=col, debug=debug-1, ...) 
                      } else if (which[w] == 11) {
                          plotSubsection(xx, yy, zz, which.xtype, which.ytype,
                                         "v", "v",
                                         levels=contourLevels, labels=contourLabels,  xlim=xlim, ylim=ylim,
                                         axes=axes, col=col, debug=debug-1, ...) 
                      } else if (which[w] == 12) {
                          plotSubsection(xx, yy, zz, which.xtype, which.ytype,
                                         "vz", "dv/dz",
                                         levels=contourLevels, labels=contourLabels,  xlim=xlim, ylim=ylim,
                                         axes=axes, col=col, debug=debug-1, ...) 
                      }
                  } else {
                      ##> message("**CHECK(section.R:1107)**")
                      if (is.na(which[w])) {
                          ##> message("*** ", whichOriginal[w], " ***")
                          plotSubsection(xx, yy, zz, which.xtype, which.ytype,
                                         whichOriginal[w], whichOriginal[w], eos=eos,
                                         xlim=xlim, ylim=ylim, ztype=ztype,
                                         zbreaks=zbreaks, zcol=zcol,
                                         axes=axes, col=col, debug=debug-1, ...)
                      } else if (which[w] == 1) {
                          ##> message("*** temperature ***")
                          plotSubsection(xx, yy, zz, which.xtype, which.ytype,
                                         "temperature", if (eos == "unesco") "T" else expression(Theta), eos=eos,
                                         xlim=xlim, ylim=ylim, ztype=ztype,
                                         zbreaks=zbreaks, zcol=zcol,
                                         axes=axes, col=col, debug=debug-1, ...)
                      } else if (which[w] == 2) {
                          ##> message("*** salinity ***")
                          plotSubsection(xx, yy, zz, which.xtype, which.ytype,
                                         "salinity",    if (eos == "unesco") "S" else expression(S[A]), eos=eos,
                                         xlim=xlim, ylim=ylim, ztype=ztype,
                                         zbreaks=zbreaks, zcol=zcol,
                                         axes=axes, col=col, debug=debug-1, ...)
                      } else if (which[w] == 3) {
                          plotSubsection(xx, yy, zz, which.xtype, which.ytype,
                                         "sigmaTheta", expression(sigma[theta]),
                                         xlim=xlim, ylim=ylim, ztype=ztype,
                                         zbreaks=zbreaks, zcol=zcol,
                                         axes=axes, col=col, debug=debug-1, ...)
                      } else if (which[w] == 4) {
                          plotSubsection(xx, yy, zz, which.xtype, which.ytype,
                                         "nitrate",     "nitrate",
                                         xlim=xlim, ylim=ylim, ztype=ztype,
                                         zbreaks=zbreaks, zcol=zcol,
                                         axes=axes, col=col, debug=debug-1, ...)
                      } else if (which[w] == 5) {
                          plotSubsection(xx, yy, zz, which.xtype, which.ytype,
                                         "nitrite",     "nitrite",
                                         xlim=xlim, ylim=ylim, ztype=ztype,
                                         zbreaks=zbreaks, zcol=zcol,
                                         axes=axes, col=col, debug=debug-1, ...)
                      } else if (which[w] == 6) {
                          plotSubsection(xx, yy, zz, which.xtype, which.ytype,
                                         "oxygen",      "oxygen",
                                         xlim=xlim, ylim=ylim, ztype=ztype,
                                         zbreaks=zbreaks, zcol=zcol,
                                         axes=axes, col=col, debug=debug-1, ...)
                      } else if (which[w] == 7) {
                          plotSubsection(xx, yy, zz, which.xtype, which.ytype,
                                         "phosphate",   "phosphate",
                                         xlim=xlim, ylim=ylim, ztype=ztype,
                                         zbreaks=zbreaks, zcol=zcol,
                                         axes=axes, col=col, debug=debug-1, ...)
                      } else if (which[w] == 8) {
                          plotSubsection(xx, yy, zz, which.xtype, which.ytype,
                                         "silicate",    "silicate",
                                         xlim=xlim, ylim=ylim, ztype=ztype,
                                         zbreaks=zbreaks, zcol=zcol,
                                         axes=axes, col=col, debug=debug-1, ...)
                      } else if (which[w] == 9) {
                          plotSubsection(xx, yy, zz, which.xtype, which.ytype,
                                         "u", "u",
                                         xlim=xlim, ylim=ylim,
                                         axes=axes, col=col, debug=debug-1, ...) 
                      } else if (which[w] == 10) {
                          plotSubsection(xx, yy, zz, which.xtype, which.ytype,
                                         "uz", "du/dz",
                                         xlim=xlim, ylim=ylim,
                                         axes=axes, col=col, debug=debug-1, ...) 
                      } else if (which[w] == 11) {
                          plotSubsection(xx, yy, zz, which.xtype, which.ytype,
                                         "v", "v",
                                         xlim=xlim, ylim=ylim,
                                         axes=axes, col=col, debug=debug-1, ...) 
                      } else if (which[w] == 12) {
                          plotSubsection(xx, yy, zz, which.xtype, which.ytype,
                                         "vz", "dv/dz",
                                         xlim=xlim, ylim=ylim,
                                         axes=axes, col=col, debug=debug-1, ...) 
                      }
                  }
                  if (!is.na(which[w]) && which[w] == 20)
                      plotSubsection(xx, yy, zz, which.xtype, which.ytype,
                                     "data", "", xlim=xlim, ylim=ylim, col=col, debug=debug-1, legend=FALSE, ...)
                  if (!is.na(which[w]) && which[w] == 99) {
                      plotSubsection(xx, yy, zz, which.xtype, which.ytype,
                                     "map", indicate.stations=FALSE,
                                         clongitude=clongitude, clatitude=clatitude, span=span,
                                         projection=projection,
                                         debug=debug-1, ...)
                  }
                  if (w <= adorn.length) {
                      t <- try(eval(adorn[w]), silent=TRUE)
                      if (class(t) == "try-error") warning("cannot evaluate adorn[", w, "]\n")
                  }
              }
              oceDebug(debug, "} # plot.section()\n", unindent=1)
              invisible()
          })


#' @title Read a Section File
#' 
#' @description
#' Read a file that contains a series of \code{ctd} profiles that make up an
#' oceanographic section.
#' Only \emph{exchange BOT} comma-separated value format is permitted at this time,
#' but other formats may be added later.  It should also be noted that the parsing
#' scheme was developed after inspection of the A03 data set (see Examples). This
#' may cause problems if the format is not universal. For example, the header must
#' name the salinity column "\code{CTDSAL}"; if not, salinity values will not be
#' read from the file.
#' 
#' @section Disambiguating salinity:
#' WOCE datasets commonly have a column named \code{CTDSAL} for salinity inferred
#' from a CTD and \code{SALNTY} (not a typo) for salinity derived from bottle data.
#' If only one of these is present in the data file, the data will be called
#' \code{salinity} in the \code{data} slot of the return value. However, if both
#' are present, then \code{CTDSAL} is stored as \code{salinity} and \code{SALNTY}
#' is stored as \code{salinityBottle}.
#' 
#' @param file A file containing a set of CTD observations.  At present, only the
#' \emph{exchange BOT} format is accepted (see Details).
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
#' @param debug Logical. If \code{TRUE}, print some information that might be
#' helpful during debugging.
#' 
#' @param processingLog If provided, the action item to be stored in the log.  This
#' is typically only provided for internal calls; the default that it provides is
#' better for normal calls by a user.
#' 
#' @return An object of class \code{\link{section-class}}.
#' 
#' @references
#' Several repository sites provide section data. An example that is perhaps likely
#' to exist for years is \url{http://cchdo.ucsd.edu}, but a search on \code{"WOCE
#'   bottle data"} should turn up other sites, if this one ceases to exist. Only
#' the so-called \emph{exchange BOT} data format can be processed by read.section()
#' at this time.
#' 
#' @author Dan Kelley
#' 
#' @family things related to \code{section} data
read.section <- function(file, directory, sectionId="", flags,
			 ship="", scientist="", institute="",
                         missingValue=-999,
			 debug=getOption("oceDebug"), processingLog)
{
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
    ##> 	flags <- c(2)
    # Skip header
    lines <- readLines(file)
    if ("BOTTLE" != substr(lines[1], 1, 6))
	stop("only type \"BOTTLE\" understood, but got header line\n", lines[1],"\n")
    if (nchar(sectionId) < 1)
        sectionId <- substr(lines[1], 8, nchar(lines[1]))
    n <- length(lines)
    header <- lines[1]
    for (l in (2:n)) {
	oceDebug(debug, lines[l], "\n")
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
    for (l in ((header.length + 1):(n-1))) { # last line is END_DATA
	contents <- strsplit(lines[l], split=",")[[1]]
	stnSectionId[l - header.length] <- sub(" *","", contents[2])
	stnId[l - header.length] <- sub("^ *","", contents[3])
	data[l - header.length,] <- contents[col.start:nv]
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
	stn.date <- as.character(data[,which(var.names=="DATE") - col.start + 1])
    else
	stop("no column named \"DATE\"")
    if (length(which(var.names=="TIME")))
	stn.time <- as.character(data[,which(var.names=="TIME") - col.start + 1])
    else
	stop("no column named \"TIME\"")
    ## EXPOCODE,SECT_ID,STNNBR,CASTNO,SAMPNO,BTLNBR,BTLNBR_FLAG_W,DATE,TIME,LATITUDE,LONGITUDE,DEPTH,CTDPRS,CTDTMP,CTDSAL,CTDSAL_FLAG_W,SALNTY,SALNTY_FLAG_W,OXYGEN,OXYGEN_FLAG_W,SILCAT,SILCAT_FLAG_W,NITRIT,NITRIT_FLAG_W,NO2+NO3,NO2+NO3_FLAG_W,PHSPHT,PHSPHT_FLAG_W

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
    waterDepth  <- as.numeric(data[,which(var.names=="DEPTH") - col.start + 1])
    ## FIXME: we have both 'latitude' and 'lat'; this is too confusing
    longitude <- as.numeric(data[,which(var.names=="LONGITUDE") - col.start + 1])
    latitude  <- as.numeric(data[,which(var.names=="LATITUDE") - col.start + 1])
    stationId <- data[,which(var.names=="STNNBR") - col.start + 1]
    stationId <- sub(" *$","",sub("^ *","",stationId)) #remove blanks
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
    dataNamesOriginal <- var.names[!colSkip]
    dataNames <- woceNames2oceNames(var.names)[!colSkip]
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
	oceDebug(debug, "reading station", i, "... ")
	select <- which(stationId == stationList[i])
	# "199309232222"
	# "1993-09-23 22:22:00"
	time[i] <- as.numeric(strptime(paste(stn.date[select[1]], stn.time[select[1]], sep=""), "%Y%m%d%H%M", tz="UTC"))
	stn[i] <- sub("^ *", "", stationId[select[1]])
	lon[i] <- longitude[select[1]]
	lat[i] <- latitude[select[1]]
        ## ## FIXME: chop flags up
        ## flagsSelected <- flags
        ## for (name in names(flagsSelected)) {
        ##     flagsSelected[[name]] <- flags[[name]][select]
        ## }
        ## ##> message("flagsSelected:"); str(flagsSelected)
	## thisStation <- as.ctd(salinity=salinity[select],
	## 		       temperature=temperature[select],
	## 		       pressure=pressure[select],
        ##                        oxygen=if(!is.null(oxygen))oxygen[select],
        ##                        nitrate=if(!is.null(nitrate))nitrate[select],
        ##                        nitrite=if(!is.null(nitrite))nitrite[select],
        ##                        phosphate=if(!is.null(phosphate))phosphate[select],
        ##                        silicate=if(!is.null(silicate))silicate[select],
        ##                        flags=flagsSelected,
	## 		       ship=ship,
	## 		       startTime=numberAsPOSIXct(time[i]),
	## 		       scientist=scientist,
	## 		       institute=institute,
	## 		       longitude=lon[i], latitude=lat[i],
	## 		       cruise=stnSectionId[select[1]],
	## 		       station=stn[i],
	## 		       waterDepth=waterDepth[select[1]],
	## 		       src=filename)
        select <- which(stationId == stationList[i])
        thisStation <- new("ctd")
        thisStation@data <- list() # start over, then insert one by one
        ## colNames <- oceNames[!colSkip]
        DATA <- data[,!colSkip]
        isFlag <- rep(TRUE, sum(!colSkip))
        for (idata in seq_along(dataNames)) {
            ## Split flags into metadata
            isFlag[idata] <- 0 < length(grep("Flag$", dataNames[idata]))
            if (isFlag[idata]) {
                thisStation@metadata$flags[[gsub("Flag$", "", dataNames[idata])]] <- as.numeric(DATA[select, idata])
            } else {
                ## message("colNames[", idata, "]: ", colNames[idata])
                thisStation@data[[dataNames[idata]]] <- as.numeric(DATA[select, idata])
            }
        }
        thisStation@metadata$names <- dataNames[!isFlag]
        thisStation@metadata$labels <- dataNames[!isFlag]
        thisStation@metadata$dataNamesOriginal <- dataNamesOriginal[!isFlag]
        thisStation@metadata$src <- filename
        thisStation@metadata$startTime <- numberAsPOSIXct(time[i])
        thisStation@metadata$longitude <- lon[i]
        thisStation@metadata$latitude <- lat[i]
        thisStation@metadata$time[i] <- as.numeric(strptime(paste(stn.date[select[1]], stn.time[select[1]], sep=""), "%Y%m%d%H%M", tz="UTC"))
        thisStation@metadata$stn[i] <- sub("^ *", "", stationId[select[1]])
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
	if (debug) cat(length(select), "levels @ ", lat[i], "N ", lon[i], "W\n")
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

#' @title Grid a Section
#' 
#' @description
#' Grid a section, by interpolating to fixed pressure levels.  The
#' \code{"approx"}, \code{"boxcar"} and \code{"lm"} methods are described in the
#' documentation for \code{\link{ctdDecimate}}, which is used to do this
#' processing.  The default \code{"approx"} method is best for bottle data, the
#' \code{"boxcar"} is best for ctd data, and the \code{"lm"} method is probably
#' too slow to recommend for exploratory work, in which it is common to do trials
#' with a variety of \code{"p"} values.
#'
#' @template flagDeletionTemplate
#' 
#' @param section A \code{section} object containing the section to be gridded.
#' 
#' @param p Optional indication of the pressure levels to which interpolation
#' should be done.  If this is not supplied, the pressure levels will be
#' calculated based on the typical spacing in the ctd profiles stored within
#' \code{section}.  If \code{p="levitus"}, then pressures will be set to be those
#' of the Levitus atlas, given by \code{\link{standardDepths}}, trimmed to the
#' maximum pressure in \code{section}.  If \code{p} is a single numerical value,
#' it is taken as the number of subdivisions to use in a call to \code{\link{seq}}
#' that has range from 0 to the maximum pressure in \code{section}.  Finally, if a
#' vector numerical values is provided, then it is used as is.
#' 
#' @param method The method to use to decimate data within the stations; see
#' \code{\link{ctdDecimate}}, which is used for the decimation.
#' 
#' @param debug A flag that turns on debugging.  The value indicates the depth
#' within the call stack to which debugging applies.  For example,
#' \code{read.adv.nortek()} calls \code{read.header.nortek()}, so that
#' \code{read.adv.nortek(...,debug=2)} provides information about not just the
#' main body of the data file, but also the details of the header.
#' 
#' @param ... Optional arguments to be supplied to \code{\link{ctdDecimate}}.
#' 
#' 
#' @return An object of \code{\link{section-class}} that contains stations whose
#' pressure values match identically.
#' 
#' @examples
#' # Gulf Stream
#' library(oce)
#' data(section)
#' GS <- subset(section, 109<=stationId&stationId<=129)
#' GSg <- sectionGrid(GS, p=seq(0, 5000, 100))
#' plot(GSg, map.xlim=c(-80,-60))
#' 
#' @author Dan Kelley
#' 
#' @family things related to \code{section} data
sectionGrid <- function(section, p, method="approx", debug=getOption("oceDebug"), ...)
{
    oceDebug(debug, "sectionGrid(section, p, method=\"", if (is.function(method)) "(function)" else method, "\", ...) {\n", sep="", unindent=1)
    n <- length(section@data$station)
    oceDebug(debug, "have", n, "stations in this section\n")
    dp.list <- NULL
    if (missing(p)) {
	oceDebug(debug, "argument 'p' not given\n")
	p.max <- 0
	for (i in 1:n) {
	    p <- section@data$station[[i]]@data$pressure
	    dp.list <- c(dp.list, mean(diff(p)), na.rm=TRUE)
	    p.max <- max(c(p.max, p), na.rm=TRUE)
            ## message("i: ", i, ", p.max: ", p.max)
	}
	dp <- mean(dp.list, na.rm=TRUE) / 1.5 # make it a little smaller
	pt <- pretty(c(0, p.max), n=min(200, floor(p.max / dp)))
	oceDebug(debug, "p.max=", p.max, "; dp=", dp, "\n")
	oceDebug(debug, "pt=", pt, "\n")
    } else {
	if (length(p) == 1) {
	    if (p=="levitus") {
		pt <- standardDepths()
                pt <- pt[pt < max(section[["pressure"]], na.rm=TRUE)]
	    } else {
                if (!is.numeric(p))
                    stop("p must be numeric")
                pMax <- max(section[["pressure"]], na.rm=TRUE)
		pt <- seq(0, pMax, p)
	    }
	} else {
	    pt <- p
	}
    }
    ## BUG should handle all variables (but how to interpolate on a flag?)
    res <- section
    warning("Data flags are omitted from the gridded section object. Use handleFlags() first to remove bad data.")
    for (i in 1:n) {
        ##message("i: ", i, ", p before decimation: ", paste(section@data$station[[i]]@data$pressure, " "))
	suppressWarnings(res@data$station[[i]] <- ctdDecimate(section@data$station[[i]], p=pt, method=method, debug=debug-1, ...))
        res@data$station[[i]]@metadata$flags <- NULL
        ##message("i: ", i, ", p after decimation: ", paste(res@data$station[[i]]@data$pressure, " "))
    }
    res@processingLog <- processingLogAppend(res@processingLog, paste(deparse(match.call()), sep="", collapse=""))
    oceDebug(debug, "} # sectionGrid\n", unindent=1)
    res
}


#' @title Smooth a Section
#' 
#' @description
#' Smooth a section in the lateral (alpha version that may change).
#' 
#' @details
#' This function should be used with caution, as should any operation that changes
#' data.  Although smoothing may be desirable to produce aesthetically-pleasing
#' plots, it can also introduce artifacts that can lead to erroneous conclusions.
#' The prudent analyst starts by comparing plots of the raw data with plots of the
#' smoothed data.
#'     
#' For \code{method="spline"}, the section is smoothed using
#' \code{\link{smooth.spline}} on individual pressure levels, with any parameters
#' listed in \code{parameters} being passed to that function.  If \code{df} is not
#' present in \code{parameters}, then this function sets it to the number of
#' stations divided by 5.  Smoothing is done separately for temperature, salinity,
#' and sigma-theta.
#' 
#' For the (much slower) \code{method="barnes"} method, smoothing is done across
#' both horizontal and vertical coordinates, using \code{\link{interpBarnes}}.
#' Any arguments in \code{\dots} being passed to that function; see
#' \sQuote{Examples}.
#' 
#' @param section A \code{section} object containing the section to be smoothed.
#' For \code{method="spline"}, the pressure levels must match for each station in
#' the section.
#' 
#' @param method Specifies the method to use; see \sQuote{Details}.
#' 
#' @param debug A flag that turns on debugging.  Set to 1 to get a moderate amount
#' of debugging information, or to 2 to get more.
#' 
#' @param ... Optional extra arguments, passed to either
#' \code{\link{smooth.spline}} or \code{\link{interpBarnes}}.
#' 
#' @return An object of \code{\link{section-class}} that ordered in some way.
#' 
#' @examples
#' library(oce)
#' data(section)
#' gs <- subset(section, 109<=stationId&stationId<=129)
#' gsg <- sectionGrid(gs, p=seq(0, 5000, 150))
#' gss1 <- sectionSmooth(gsg, "spline", df=16)
#' plot(gss1)
#' gss2 <- sectionSmooth(gsg, "barnes", xr=24, yr=100)
#' plot(gss2)
#' 
#' @author Dan Kelley
#' 
#' @family things related to \code{section} data
sectionSmooth <- function(section, method=c("spline", "barnes"), debug=getOption("oceDebug"), ...)
{
    method <- match.arg(method)
    ## bugs: should ensure that every station has identical pressures
    ## FIXME: should have smoothing in the vertical also ... and is spline what I want??
    oceDebug(debug, "sectionSmooth(section,method=\"", method, "\", ...) {\n", sep="", unindent=1)
    if (!inherits(section, "section"))
        stop("method is only for objects of class '", "section", "'")
    nstn <- length(section@data$station)
    if (method == "spline") {
        stn1pressure <- section[["station", 1]][["pressure"]]
        npressure <- length(stn1pressure)
        for (istn in 1:nstn) {
            thisp <- section[["station", istn]][["pressure"]]
            if (length(thisp) != npressure)
                stop("pressure mismatch between station 1 and station", istn)
            if (any(thisp != stn1pressure))
                stop("pressure mismatch between station 1 and station", istn)
        }
        oceDebug(debug, "nstn=", nstn, "npressure=", npressure, "\n")
        res <- section
        ## reorder stations by distance from first (this
        ## is crucial if the files have been ordered by a
        ## directory listing, and they are not named e.g. 01
        ## to 10 etc but 1 to 10 etc.
        x <- geodDist(section)
        o <- order(x)
        res@metadata$longitude <- section@metadata$longitude[o]
        res@metadata$latitude <- section@metadata$latitude[o]
        res@metadata$stationId <- section@metadata$stationId[o]
        res@data$station <- section@data$station[o]
        x <- geodDist(res)
        temperatureMat <- array(double(), dim=c(npressure, nstn))
        salinityMat <- array(double(), dim=c(npressure, nstn))
        sigmaThetaMat <- array(double(), dim=c(npressure, nstn))
        for (s in 1:nstn) {
            thisStation <- res@data$station[[s]]
            temperatureMat[,s] <- thisStation@data$temperature
            salinityMat[,s] <- thisStation@data$salinity
            sigmaThetaMat[,s] <- thisStation[["sigmaTheta"]]
        }
        ## turn off warnings about df being too small
        o <- options('warn')
        options(warn=-1) 
        gaveDF <- "df" %in% names(list(...))
        for (p in 1:npressure) {
            ok <- !is.na(temperatureMat[p,]) ## FIXME: ok to infer missingness from temperature alone?
            nok <- sum(ok)
            ##iok <- (1:nstn)[ok]
            if (nok > 4) { ## Only fit spline if have 4 or more values; ignore bad values in fitting.
                if (gaveDF) {
                    temperatureMat[p,] <- predict(smooth.spline(x[ok], temperatureMat[p,ok], ...), x)$y
                    salinityMat[p,]    <- predict(smooth.spline(x[ok],    salinityMat[p,ok], ...), x)$y
                    sigmaThetaMat[p,]  <- predict(smooth.spline(x[ok],  sigmaThetaMat[p,ok], ...), x)$y
                    oceDebug(debug, stn1pressure[p], "dbar: smoothing with supplied df=", list(...)$df, " (have", nok, "good values)\n")
                } else {
                    usedf <- nok / 5
                    temperatureMat[p,] <- predict(smooth.spline(x[ok], temperatureMat[p,ok], df=usedf), x)$y
                    salinityMat[p,]    <- predict(smooth.spline(x[ok],    salinityMat[p,ok], df=usedf), x)$y
                    sigmaThetaMat[p,]  <- predict(smooth.spline(x[ok],  sigmaThetaMat[p,ok], df=usedf), x)$y
                    oceDebug(debug, stn1pressure[p], "dbar: smoothing with df=", usedf, " (have", nok, "good values)\n")
                }
            } else {
                oceDebug(debug, stn1pressure[p], "dbar: not smoothing, since have only", nok, "good values\n")
            }
        }
        options(warn=o$warn) 
        for (s in 1:nstn) {
            res@data$station[[s]]@data$temperature <- temperatureMat[,s]
            res@data$station[[s]]@data$salinity <- salinityMat[,s]
            res@data$station[[s]]@data$sigmaTheta <- sigmaThetaMat[,s]
        }
    } else if (method == "barnes") {
        vars <- names(section[["station", 1]]@data)
        res <- section
        x <- geodDist(section)
        X <- p <- NULL
        stn1pressure <- section[["station", 1]][["pressure"]]
        npressure <- length(stn1pressure)
        for (istn in 1:nstn) {
            stn <- section[["station", istn]]
            if (length(stn[["pressure"]]) != npressure)
                stop("pressure mismatch between station 1 and station", istn)
            if (any(stn[["pressure"]] != stn1pressure))
                stop("pressure mismatch between station 1 and station.", istn)
        }
        P <- rep(stn1pressure, nstn)
        X <- rep(x, each=npressure)
        for (var in vars) {
            if (var == "scan" || var == "time" || var == "pressure"
                || var == "depth" || var == "flag" || var == "quality")
                next
            v <- NULL
            oceDebug(debug, "smoothing", var, "\n")
            ## collect data
            if (FALSE) {
                for (istn in 1:nstn) {
                    oceDebug(debug, "station", istn, "\n")
                    stn <- section[["station", istn]]
                    v <- c(v, stn[[var]])
                }
            }
            ## grid overall, then deposit into stations (trimming for NA)
            v <- section[[var]]
            smu <- interpBarnes(X, P, v, xg=x, yg=stn1pressure, ..., debug=debug-1)
            for (istn in 1:nstn) {
                res@data$station[[istn]]@data[[var]] <- smu$zg[istn,]
                na <- is.na(section@data$station[[istn]][[var]])
                res@data$station[[istn]]@data[[var]][na] <- NA
            }
        }
    } else {
        stop("unknown method \"", method, "\"") # cannot reach here
    }

    res@processingLog <- processingLogAppend(res@processingLog, paste(deparse(match.call()), sep="", collapse=""))
    oceDebug(debug, "} # sectionSmooth()\n", unindent=1)
    res
}


#' @title Create a Section
#' 
#' @description
#' Create a section based on columnar data, or a set of \code{\link{oce-class}}
#' objects that can be coerced to CTD form with \code{\link{as.ctd}}.
#' 
#' If the first argument is a numerical vector, then it is taken to be the
#' salinity, and \code{\link{factor}} is applied to \code{station} to break the
#' data up into chunks that are assembled into \code{\link{ctd-class}} objects with
#' \code{\link{as.ctd}} and combined to make a \code{\link{section-class}} object
#' to be returned. This mode of operation is provided as a convenience for datasets
#' that are already partly processed; if original CTD data are available, the next
#' mode is preferred, because it permits the storage of much more data and metadata
#' in the CTD object.
#' 
#' If the first argument is a list containing oce objects that can be coerced into
#' CTD form with \code{\link{as.ctd}} -- or a character vector containing the names
#' of such objects that are defined in the calling environment -- then those
#' objects are combined to form the station, and all other arguments are ignored.
#' This is the best way to call \code{as.section}.
#' 
#' If the first argument is a \code{\link{argo-class}} object, then the profiles it
#' contains are turned into \code{\link{ctd-class}} object, and these are assembled
#' into a section to be returned.
#' 
#' 
#' @param salinity This may be a numerical vector, in which case it is interpreted
#' as the salinity, and the other arguments are used for the other components of
#' \code{\link{ctd-class}} objects. Alternatively, it may be one of a variety of
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
#' @return An object of \code{\link{section-class}}.
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
#' @family things related to \code{section} data
as.section <- function(salinity, temperature, pressure, longitude, latitude, station, sectionId="")
{
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
                                longitude=longitude[look][1], latitude=latitude[look][1],
                                station=stationLevels[i])
        }
    } else if (inherits(salinity, "argo")) {
        tmp <- salinity
        nstation <- length(tmp[['longitude']])
        ndepth <- dim(tmp[["salinity"]])[1]
        station <- rep(1:nstation, each=ndepth)
        longitude <- rep(tmp[['longitude']], each=ndepth)
        latitude <- rep(tmp[['latitude']], each=ndepth)
        salinity <- as.vector(tmp[['salinity']])
        temperature <- as.vector(tmp[['temperature']])
        pressure <- as.vector(tmp[['pressure']])
        stationFactor <- factor(station)
        stationLevels <- levels(stationFactor)
        nstation <- length(stationLevels)
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
            ##message("ARGO CASE. i: ", i, ", name:", stationLevels[i])
            look <- station==stationLevels[i]
            ctds[[i]] <- as.ctd(salinity[look], temperature[look], pressure[look],
                                longitude=longitude[look][1], latitude=latitude[look][1],
                                startTime=as.POSIXct(time[i]), station=paste("profile", stationLevels[i]))
            for (Ni in seq_along(N)) {
                ctds[[i]] <- ctdAddColumn(ctds[[i]], as.vector(tmp[[N[Ni]]])[look], N[Ni],
                                          unit=tmp[['units']][[N[Ni]]])
            }
        }
    } else if (inherits(salinity, "list")) {
        ##if (!inherits(salinity[[1]], "ctd")) stop("list must contain ctd objects")
        nstation <- length(salinity)
        ctds <- vector("list", nstation)
        for (i in 1:nstation) {
            ##message("CTD-LIST CASE. i: ", i, ", name:", salinity[[i]][["station"]])
            ctds[[i]] <- as.ctd(salinity[[i]])
        }
    } else if (is.character(salinity) && length(salinity) > 1) {
        ## vector of names of CTD objects
        nstation <- length(salinity)
        ctds <- vector("list", nstation)
        for (i in 1:nstation)
            ctds[[i]] <- get(salinity[i], parent.frame())
    }
    ## In each case, we now have a vector of CTD objects.
    res@metadata$sectionId<- ""
    res@metadata$stationId<- unlist(lapply(ctds, function(x) x[["station"]]))
    res@metadata$longitude<- unlist(lapply(ctds, function(x) x[["longitude"]]))
    res@metadata$latitude<- unlist(lapply(ctds, function(x) x[["latitude"]]))
    res@data <- list(station=ctds)
    res@processingLog <- processingLogAppend(res@processingLog, paste(deparse(match.call()), sep="", collapse=""))
    res
}

