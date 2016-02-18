# vim:textwidth=128:expandtab:shiftwidth=4:softtabstop=4

#' Class to hold argo data
#'
#' This class stores data from argo floats. It will be in fairly
#' active development in the early months of 2016.
#'
#' An \code{argo} object may be read with \code{\link{read.argo}} or
#' created with \code{\link{as.argo}}.  Argo data can be gridded to constant
#' pressures with \code{\link{argoGrid}}.  Plots can be made with
#' \code{\link{plot.argo}}, while \code{\link{summary.argo}} produces statistical
#' summaries and \code{show} produces overviews. The usual oce generic
#' functions are available, e.g. \code{\link{[[,argo-method}} may 
#' be used to extract data, and \code{\link{[[<-,argo-method}} may
#' be used to insert data.
#'
#' See \url{http://www.argo.ucsd.edu/Gridded_fields.html} for some
#' argo-related datasets that may be useful in a wider context.
setClass("argo", contains="oce")

#' Extract something from an argo object
#'
#' @param x An argo object, i.e. one inheriting from \code{\link{argo-class}}.
#' @param i The item to extract.
#' @param j Optional additional information on the \code{i} item.
#' @param ... Optional additional information (ignored).
setMethod(f="[[",
          signature(x="argo", i="ANY", j="ANY"),
          definition=function(x, i, j, ...) {
              callNextMethod()
          })

#' Change something within an argo object
#'
#' In addition to the usual insertion of elements by name, note
#' that e.g. \code{pitch} gets stored into \code{pitchSlow}.
#' 
#' @param x An argo object, i.e. one inheriting from \code{\link{argo-class}}.
#' @param i The item to insert
#' @param j Optional additional information on the \code{i} item.
#' @param ... Optional additional information (ignored).
#' @param value The value recoverd from \code{x}.
setMethod(f="[[<-",
          signature(x="argo", i="ANY", j="ANY"),
          definition=function(x, i, j, value) {
              callNextMethod()
          })

setMethod(f="initialize",
          signature="argo",
          definition=function(.Object,time,id,longitude,latitude,salinity,temperature,pressure,filename,dataMode) {
              if (!missing(time)) .Object@data$time <- time
              if (!missing(id)) .Object@metadata$id <- id
              if (!missing(longitude)) .Object@data$longitude <- longitude
              if (!missing(latitude)) .Object@data$latitude <- latitude
              if (!missing(salinity)) .Object@data$salinity <- salinity
              if (!missing(temperature)) .Object@data$temperature <-temperature 
              if (!missing(pressure)) .Object@data$pressure <- pressure
              .Object@metadata$filename <- if (missing(filename)) "" else filename
              .Object@metadata$dataMode <- if (missing(dataMode)) "" else dataMode
              .Object@processingLog$time <- as.POSIXct(Sys.time())
              .Object@processingLog$value <- "create 'argo' object"
              return(.Object)
          })

getData <- function(file, name) # a local function -- no need to pollute namesapce with it
{
    res <- try(ncdf4::ncvar_get(file, name), silent=TRUE)
    if (inherits(res, "try-error")) {
        cat(file$filename, " has no variable named '", name, "'\n", sep='')
        res <- NULL
    }
    res
}

#' Convert profile-data names from the Argo convention to the oce convention
#'
#' For example, \code{"PSAL"} becomes \code{"salinity"}
#' @param names vector of character strings containing names in the Argo convention.
argoDataNames <- function(names)
{
    names <- gsub("CYCLE_NUMBER", "cycle", names)
    names <- gsub("TEMP_DOXY", "temperatureOxygen", names)
    names <- gsub("DOXY", "oxygen", names)
    names <- gsub("PRES", "pressure", names)
    names <- gsub("PSAL", "salinity", names)
    names <- gsub("TEMP", "temperature", names)
    names <- gsub("_ADJUSTED", "Adjusted", names)
    names <- gsub("_QC", "Qc", names)
    names <- gsub("_ERROR", "Error", names)
    names
}


#' Subset an argo object
#'
#' Subset an argo object, either by selecting just the "adjusted" data
#' or by subsetting by pressure or other variables.
#'
#' @details
#' If \code{subset} is the string \code{"adjusted"}, then \code{subset} 
#' replaces the station variables with their adjusted counterparts. In
#' the argo notation, e.g. \code{PSAL} is replaced with \code{PSAL_ADJUSTED};
#' in the present notation, this means that \code{salinity} in the \code{data}
#' slot is replaced with \code{salinityAdjusted}, and the latter is deleted.
#' Similar replacements are also done with the flags stored in the \code{metadata}
#' slot.
#'
#' If \code{subset} is an expression, then the action is somewhat similar
#' to other \code{subset} functions, but with the restriction that
#' only one independent variable may be
#' used in in any call to the function, so that 
#' repeated calls will be necessary to subset based on more than one
#' independent variable.  Subsetting may be done by anything
#' stored in the data, e.g. \code{time},
#' \code{latitude}, \code{longitude}, \code{profile}, \code{dataMode},
#' or \code{pressure} or by \code{profile} (a made-up variable)
#' or \code{id} (from the \code{metadata} slot).
#'
#' @param x An object inheriting from \code{\link{argo-class}}.
#' @param subset An expression indicating how to subset \code{x}.
#' @param ... Ignored.
#' @return An argo object.
#' 
#' @aliases subset.argo
#'
#' @examples
#' library(oce)
#' data(argo)
#'
#' # Example 1: buset by time, longitude, and pressure
#' par(mfrow=c(2,2))
#' plot(argo)
#' plot(subset(argo, time > mean(time)))
#' plot(subset(argo, longitude > mean(longitude)))
#' plot(subset(argoGrid(argo), pressure > 500 & pressure < 1000), which=5)
#' 
#' # Example 2: restrict attention to delayed-mode profiles.
#' par(mfrow=c(1,1))
#' plot(subset(argo, dataMode == "D"))
#'
#' # Example 3: contrast corrected and uncorrected data
#' par(mfrow=c(1,2))
#' plotTS(argo)
#' plotTS(subset(argo, "adjusted"))
#'
#' @author Dan Kelley
#' @family functions that deal with argo data
setMethod(f="subset",
          signature="argo",
          definition=function(x, subset, ...) {
              if (missing(subset)) {
                  warning("subset.argo(): argument 'subset' must be given\n", call.=FALSE)
                  return(x)
              }
              if (is.character(substitute(subset))) {
                  if (subset != "adjusted") 
                      stop("if subset is a string, it must be \"adjusted\"")
                  res <- x
                  dataNames <- names(x@data)
                  ## Seek 'Adjusted' names
                  adjustedIndices <- grep(".*Adjusted$", dataNames)
                  for (i in adjustedIndices) {
                      adjusted <- dataNames[i]
                      base <- gsub("Adjusted$", "", adjusted)
                      adjustedError <- paste(adjusted, "Error", sep="")
                      ##> message("    base:          ", base)
                      ##> message("    adjusted:      ", adjusted)
                      ##> message("    adjustedError: ", adjustedError)
                      res@data[[base]] <- res@data[[adjusted]]
                      res@data[[adjusted]] <- NULL
                      res@data[[adjustedError]] <- NULL
                  }
                  flagNames <- names(x@metadata$flags)
                  adjustedIndices <- grep(".*AdjustedQc$", flagNames)
                  ##> message("FLAGS")
                  ##> message("flagNames...");print(flagNames)
                  ##> message("adjustedIndices");print(adjustedIndices)
                  for (i in adjustedIndices) {
                      adjusted <- flagNames[i]
                      base <- gsub("AdjustedQc$", "Qc", adjusted)
                      adjustedError <- paste(adjusted, "ErrorQc", sep="")
                      ##> message("    base:          ", base)
                      ##> message("    adjusted:      ", adjusted)
                      ##> message("    adjustedError: ", adjustedError)
                      res@metadata$flags[[base]] <- res@metadata$flags[[adjusted]]
                      res@metadata$flags[[adjusted]] <- NULL
                      res@metadata$flags[[adjustedError]] <- NULL
                  }
                  res@processingLog <- processingLogAppend(res@processingLog,
                                                           paste("subset.argo(x, subset=\"",
                                                                 as.character(subset), "\")", sep=""))
              } else {
                  subsetString <- paste(deparse(substitute(subset)), collapse=" ")
                  res <- x
                  if (length(grep("time", subsetString)) ||
                      length(grep("longitude", subsetString)) || length(grep("latitude", subsetString))) {
                      keep <- eval(substitute(subset), x@data, parent.frame(2))
                  } else if (length(grep("id", subsetString))) {
                      ## add id into the data, then do as usual
                      tmp <- x@data
                      tmp$id <- x@metadata$id
                      keep <- eval(substitute(subset), tmp, parent.frame(2))
                      rm(tmp)
                  } else if (length(grep("profile", subsetString))) {
                      ## add profile into the data, then do as usual
                      tmp <- x@data
                      tmp$profile <- 1:length(x@data$time)
                      keep <- eval(substitute(subset), tmp, parent.frame(2))
                      rm(tmp)
                  } else if (length(grep("pressure", subsetString))) {
                      ## check that it is a "gridded" argo
                      gridded <- ifelse(all(apply(x@data$pressure, 1, diff) == 0, na.rm=TRUE), TRUE, FALSE)
                      if (gridded) {
                          x@data$pressure <- x@data$pressure[,1] ## FIXME: have to convert pressure to vector
                          keep <- eval(substitute(subset), x@data, parent.frame(2))
                          x@data$pressure <- res@data$pressure ## FIXME: convert back to original for subsetting below
                      } else {
                          stop("cannot subset ungridded argo by pressure -- use argoGrid() first", call.=FALSE)
                      }
                  } else if (length(grep("dataMode", subsetString))) {
                      keep <- eval(substitute(subset), x@metadata, parent.frame(2))
                  } else {
                      stop("can only subset by time, longitude, latitude, pressure, dataMode, and not by combinations", call.=FALSE)
                  }
                  ## Now do the subset
                  if (length(grep("pressure", subsetString))) {
                      fieldname <- names(x@data)
                      for (field in fieldname) {
                          if (field != 'time' & field != 'longitude' & field != 'latitude') {
                              ifield <- which(field == fieldname)
                              res@data[[ifield]] <- if (is.matrix(res@data[[ifield]]))
                                  res@data[[ifield]][,keep] else res@data[[ifield]][keep]
                          }
                      }
                      fieldname <- names(x@metadata$flags)
                      for (field in fieldname) {
                          ifield <- which(field == fieldname)
                          res@metadata$flags[[ifield]] <- res@metadata$flags[[ifield]][keep,]
                      }
                      ## res@data$salinity <- x@data$salinity[keep,]
                      ## res@data$temperature <- x@data$temperature[keep,]
                      ## res@data$pressure <- x@data$pressure[keep,]
                      res@processingLog <- processingLogAppend(res@processingLog, paste("subset.argo(x, subset=", subsetString, ")", sep=""))
                  } else {
                      res@data$time <- x@data$time[keep]
                      res@data$longitude <- x@data$longitude[keep]
                      res@data$latitude <- x@data$latitude[keep]
                      res@data$profile <- x@data$profile[keep]
                      res@metadata$dataMode <- x@metadata$dataMode[keep]
                      fieldname <- names(x@data)
                      for (field in fieldname) {
                          if (field != 'time' && field != 'longitude' && field != 'latitude' && field != 'profile') {
                              ifield <- which(field == fieldname)
                              res@data[[ifield]] <- if (is.matrix(x@data[[ifield]]))
                                  x@data[[ifield]][,keep] else x@data[[ifield]][keep]
                          }
                      }
                      fieldname <- names(x@metadata$flags)
                      for (field in fieldname) {
                          ifield <- which(field == fieldname)
                          res@metadata$flags[[ifield]] <- res@metadata$flags[[ifield]][,keep]
                      }
                                        #if (sum(keep) < 1) warning("In subset.argo() :\n  removed all profiles", call.=FALSE)
                      ## res@data$salinity <- x@data$salinity[,keep]
                      ## res@data$temperature <- x@data$temperature[,keep]
                      ## res@data$pressure <- x@data$pressure[,keep]
                      res@processingLog <- processingLogAppend(res@processingLog, paste("subset.argo(x, subset=", subsetString, ")", sep=""))
                  }
              }
              res
          })



setMethod(f="summary",
          signature="argo",
          definition=function(object, ...) {
              cat("Argo Summary\n------------\n\n")
              showMetadataItem(object, "filename",                  "Source:              ", quote=TRUE)
              nid <- length(unique(object@metadata$id))
              if (1 == nid)
                   cat("* id:                  \"", object@metadata$id[1], "\"\n", sep="")
              else cat("* id list:             \"", object@metadata$id[1], "\", \"", object@metadata$id[2], "\", ...\n", sep="")
              nD <- sum(object@metadata$dataMode == "D")
              nA <- sum(object@metadata$dataMode == "A")
              nR <- sum(object@metadata$dataMode == "R")
              cat("* Profiles:            ", nD, " delayed; ", nA, " adjusted; ", nR, " realtime", "\n", sep="")
              callNextMethod()
          })

ncdfFixMatrix <- function(x)
{
    if (length(dim(x)) == 1)
        x <- as.vector(x)
    x
}

argoGrid <- function(argo, p, debug=getOption("oceDebug"), ...)
{
    oceDebug(debug, "argoGrid() {\n", sep="", unindent=1)
    dim <- dim(argo@data$pressure)
    ## ndepth <- dim[1]
    nprofile <- dim[2]
    ## FIXME: modify sal, temp, and pre.  In the end, pre constant along first index
    res <- argo
    salinity <- argo[["salinity"]]
    temperature <- argo[["temperature"]]
    pressure <- argo[["pressure"]]
    if (missing(p)) {
        pt <- apply(pressure, 1, median, na.rm=TRUE)
    } else if (length(p) == 1 && p == "levitus") {
        pt <- standardDepths()
        pt <- pt[pt < max(pressure, na.rm=TRUE)]
    } else if (is.numeric(p)) {
        if (length(p) == 1) {
            if (p < 1)
                stop("'p' must exceed 1")
            pt <- seq(0, max(pressure, na.rm=TRUE), length.out=p)
        } else {
            pt <- p
        }
    } else {
        stop("value of 'p' must be numeric, or \"levitus\"")
    }
    ##message("pt=c(", paste(round(pt), collapse=","), ")")
    npt <- length(pt)
    res@data$salinity <- matrix(0.0, ncol=nprofile, nrow=npt)
    res@data$temperature <- matrix(0.0, ncol=nprofile, nrow=npt)
    res@data$pressure <- matrix(0.0, ncol=nprofile, nrow=npt)
    for (profile in 1:nprofile) {
        ndata <- sum(!is.na(salinity[,profile]))
        if (ndata > 2 && 0 < max(abs(diff(pressure[,profile])),na.rm=TRUE)) {
            res@data$salinity[,profile] <- approx(pressure[,profile], salinity[,profile], pt, ...)$y
            res@data$temperature[,profile] <- approx(pressure[,profile], temperature[,profile], pt, ...)$y
            res@data$pressure[,profile] <- pt
        } else {
            res@data$salinity[,profile] <- rep(NA, npt)
            res@data$temperature[,profile] <- rep(NA, npt)
            res@data$pressure[,profile] <- pt
        }
    }
    res
}

argoDecodeFlags <- function(f) # local function
{
    res <- unlist(lapply(seq_along(f), function(i) strsplit(f[i], split="")))
    dim(res) <- c(length(res)/length(f), length(f))
    res
}



#' Read an Argo data file
#' 
#' \code{read.argo} is used to read an Argo file, producing an object of type
#' \code{argo}. The file must be in the ARGO-style netCDF format described at
#' in the Argo documentation [2,3].
#' 
#' @details
#'
#' Metadata items such as \code{time}, \code{longitude} and \code{latitude}
#' are inferred from the data file in a straightforward way, using
#' \code{\link[ncdf4]{ncvar_get}} and data-variable names as listed in
#' the Argo documentation [2,3]. The items listed in section 2.2.3
#' of [3] is read from the file and stored in the \code{metadata} slot, 
#' with the exception of \code{longitude} and \code{latitude},
#' which are stored in the \code{data} slot.
#'
#' String data that contain trailing blanks in the argo NetCDF
#' are trimmed using \code{\link{trimString}}.  One-dimensional
#' matrices are converted to vectors using \code{\link{as.vector}}.
#' Items listed in section 2.2.3 of [3] are meant to be present
#' in all files, but tests showed that this is not the case, and so
#' \code{read.argo} sets such items to \code{NULL} before saving
#' them in returned object.
#'
#' Items are translated from upper-case Argo names to \code{oce} names
#' as follows.
#' \itemize{
#' \item \code{PLATFORM_NUMBER} becomes \code{id}
#' \item \code{PROJECT_NAME} becomes \code{projectName}
#' \item \code{PI_NAME} becomes \code{PIName}
#' \item \code{STATION_PARAMETERS} becomes \code{stationParameters}
#' \item \code{CYCLE_NUMBER} becomes \code{cycleNumber}
#' \item \code{DIRECTION} becomes \code{direction} (either \code{A} for ascending or \code{D} for descending)
#' \item \code{DATA_CENTRE} becomes \code{dataCentre} (note the spelling)
#' \item \code{DC_REFERENCE} becomes \code{DCReference}
#' \item \code{DATA_STATE_INDICATOR} becomes \code{dataStateIndicator}
#' \item \code{DATA_MODE} becomes \code{dataMode}
#' \item \code{INST_REFERENCE} becomes \code{instReference}
#' \item \code{FIRMWARE_VERSION} becomes \code{firmwareVersion}
#' \item \code{WMO_INST_TYPE} becomes \code{WMOInstType}
#' \item \code{JULD} becomes \code{juld} (and used to compute \code{time})
#' \item \code{JULD_QC} becomes \code{juldQc}
#' \item \code{JULD_QC_LOCATION} becomes \code{juldQcLocation}
#' \item \code{LATITUDE} becomes \code{latitude}
#' \item \code{LONGITUDE} becomes \code{longitude}
#' \item \code{POSITION_QC} becomes \code{positionQC}
#' \item \code{POSITIONING_SYSTEM} becomes \code{positioningSystem}
#' \item \code{PROFILE_QC} becomes \code{} ... FIX ME
#'}
#' 
#' It is assumed that the profile data are as listed in the NetCDF variable
#' called \code{STATION_PARAMETERS}. Each item can have variants, as
#' described in Sections 2.3.4 of [3].
#' For example, if \code{"PRES"} is found in \code{STATION_PARAMETERS},
#' then \code{PRES} (pressure) data are sought in the file, along with
#' \code{PRES_QC}, \code{PRES_ADJUSTED}, \code{PRES_ADJUSTED_QC}, and
#' \code{PRES_ERROR}. The same pattern works for other profile data. The variables
#' are stored with different names within the resultant \code{\link{argo-class}}
#' object, to match with \code{oce} conventions. Thus, \code{PRES} gets renamed
#' \code{pressure}, while \code{PRES_ADJUSTED} gets renamed \code{pressureAdjusted},
#' and \code{PRES_ERROR} gets renamed \code{pressureError}; all of these are 
#' stored in the \code{data} slot. Meanwhile, the quality-control flags
#' \code{PRES_QC} and \code{PRES_ADJUSTED_QC} are stored as \code{pressureQc}
#' and \code{pressureAdjustedQc} in the \code{metadata} slot.
#' 
#' @param file a character string giving the name of the file to load.
#' 
#' @param debug a flag that turns on debugging.  Set to 1 to get a moderate amount
#' of debugging information, or to 2 to get more.
#' 
#' @param processingLog if provided, the action item to be stored in the log.
#' (Typically only provided for internal calls; the default that it provides is
#' better for normal calls by a user.)
#' 
#' @param ... additional arguments, passed to called routines.
#' 
#' @return
#' An object of \code{\link{argo-class}}.
#' 
#' @examples
#' \dontrun{
#' ## Example 1: read from a local file
#' library(oce)
#' d <- read.argo("/data/OAR/6900388_prof.nc")
#' summary(d)
#' plot(d)
#'
#' ## Example 2: construct URL for download (brittle)
#' id <- "6900388"
#' url <- "http://www.usgodae.org/ftp/outgoing/argo"
#' if (!length(list.files(pattern="argo_index.txt")))
#'     download.file(paste(url, "ar_index_global_meta.txt", sep="/"), "argo_index.txt")
#' index <- readLines("argo_index.txt")
#' line <- grep(id, index)
#' if (0 == length(line)) stop("id ", id, " not found")
#' if (1 < length(line)) stop("id ", id, " found multiple times")
#' dac <- strsplit(index[line], "/")[[1]][1]
#' profile <- paste(id, "_prof.nc", sep="")
#' float <- paste(url, "dac", dac, id, profile, sep="/")
#' download.file(float, profile)
#' argo <- read.argo(profile)
#' summary(argo)
#' }
#' 
#' 
#' @seealso
#' The documentation for \code{\link{argo-class}} explains the structure of argo
#' objects, and also outlines the other functions dealing with them.
#' 
#' @references
#' 1. \url{http://www.argo.ucsd.edu/}
#' 
#' 2. \url{http://archimer.ifremer.fr/doc/00187/29825/40575.pdf} documents the codes used in the netCDF files.
#'
#' 3. \url{http://www.argodatamgt.org/content/download/4729/34634/file/argo-dm-user-manual-version-2.3.pdf}
#' is the main document describing argo data.
#' 
#' @section Data sources:
#' Argo data are made available at several websites. A bit of detective
#' work can be required to track down the data.  
#'
#' Some servers provide  data for floats that surfaced in a given ocean
#' on a given day, the anonymous FTP server 
#' \url{ftp://usgodae.org/pub/outgoing/argo/geo/} being an example.
#'
#' Other servers provide data on a per-float basis. A complicating
#' factor is that these data tend to be categorized by "dac" (data
#' archiving centre), which makes it difficult to find a particular
#' float. For example, 
#' \url{http://www.usgodae.org/ftp/outgoing/argo/} is the top level of
#' a such a repository. If the ID of a float is known but not the
#' "dac", then a first step is to download the text file
#' \url{http://www.usgodae.org/ftp/outgoing/argo/ar_index_global_meta.txt}
#' and search for the ID. The first few lines of that file are header,
#' and after that the format is simple, with columns separated by slash
#' (\code{/}). The dac is in the first such column and the float ID in the
#' second. A simple search will reveal the dac.
#' For example \code{data(argo)} is based on float 6900388, and the line
#' containing that token is
#' \url{bodc/6900388/6900388_meta.nc,846,BO,20120225005617}, from
#' which the dac is seen to be the British Oceanographic Data Centre
#' (\code{bodc}). Armed with that information, visit
#' \url{http://www.usgodae.org/ftp/outgoing/argo/dac/bodc/6900388}
#' and see a directory called `profiles` that contains a NetCDF
#' file for each profile the float made. These can be read with
#' \code{read.argo}. It is also possible, and probably more common,
#' to read a NetCDF file containing all the profiles together and for
#' that purpose the file
#' \url{http://www.usgodae.org/ftp/outgoing/argo/dac/bodc/6900388/6900388_prof.nc}
#' should be downloaded and provided as the \code{file} argument to
#' \code{read.argo}.  This can be automated as in Example 2,
#' although readers are cautioned that URL structures tend to change
#' over time.
#'
#' Similar steps can be followed on other servers.
#'
#' @author Dan Kelley
#' @family functions that deal with argo data
read.argo <- function(file, debug=getOption("oceDebug"), processingLog, ...)
{
    if (!requireNamespace("ncdf4", quietly=TRUE))
        stop('must install.packages("ncdf4") to read argo data')
    if (missing(processingLog)) processingLog <- paste(deparse(match.call()), sep="", collapse="")
    ## ofile <- file
    filename <- ""
    ## NOTE: need to name ncdf4 package because otherwise R checks give warnings.
    if (is.character(file)) {
        filename <- fullFilename(file)
        file <- ncdf4::nc_open(file)
        on.exit(ncdf4::nc_close(file))
    } else {
        if (!inherits(file, "connection"))
            stop("argument `file' must be a character string or connection")
        if (!isOpen(file)) {
            file <- ncdf4::nc_open(file)
            on.exit(ncdf4::nc_close(file))
        }
    }
    res <- new("argo")
    if (debug > 0) {
        if (debug > 10)
            message("This netcdf file contains the following $var: ", paste(names(file$var), collapse=" "))
        columnNames <- gsub(" *$", "",
                            unique(as.vector(ncdf4::ncvar_get(file, "STATION_PARAMETERS"))))
        message("columnNames: ", paste(columnNames, collapse=" "), " (from STATION_PARAMETERS)")
        QCNames <- paste(columnNames, "_QC", sep="")
        message("QCnames: ", paste(QCNames, collapse=" "), " (inferred from above)")
        physicalNames <- ODFNames2oceNames(columnNames)
        message("Therefore need @data items: ", paste(physicalNames, collapse=" "), " (in addition to longitude etc)")
    }

    ## Grab all information listed in table 2.2.3 of [3], with exceptions as listed in the 
    ## docs, e.g. STATION_PARAMETERS is really of no use.
    ## Must check against varNames to avoid errors if files lack some items ... e.g.
    ## 6900388_prof.nc lacked FIRMWARE_VERSION, even though table 2.2.3 of [3] indicates
    ## that it should be present.
    varNames <- names(file$var)
    res@metadata$id <- if ("PLATFORM_NUMBER" %in% varNames)
        as.vector(trimString(ncdf4::ncvar_get(file, "PLATFORM_NUMBER"))) else NULL
    res@metadata$projectName <- if ("PROJECT_NAME" %in% varNames)
        as.vector(trimString(ncdf4::ncvar_get(file, "PROJECT_NAME"))) else NULL
    res@metadata$PIName <- if ("PI_NAME" %in% varNames)
        as.vector(trimString(ncdf4::ncvar_get(file, "PI_NAME"))) else NULL
    res@metadata$stationParameters <- if ("STATION_PARAMETERS" %in% varNames)
        trimString(ncdf4::ncvar_get(file, "STATION_PARAMETERS")) else NULL
    res@metadata$cycleNumber <- if ("CYCLE_NUMBER" %in% varNames) 
        as.vector(ncdf4::ncvar_get(file, "CYCLE_NUMBER")) else NULL
    res@metadata$direction <- if ("DIRECTION" %in% varNames)
        as.vector(ncdf4::ncvar_get(file, "DIRECTION")) else NULL
    res@metadata$dataCentre <- if ("DATA_CENTRE" %in% varNames)
        as.vector(ncdf4::ncvar_get(file, "DATA_CENTRE")) else NULL
    res@metadata$DCReference <- if ("DC_REFERENCE" %in% varNames)
        as.vector(trimString(ncdf4::ncvar_get(file, "DC_REFERENCE"))) else NULL
    res@metadata$dataStateIndicator <- if ("DATA_STATE_INDICATOR" %in% varNames)
        as.vector(trimString(ncdf4::ncvar_get(file, "DATA_STATE_INDICATOR"))) else NULL
    res@metadata$dataMode <- if ("DATA_MODE" %in% varNames)
        strsplit(ncdf4::ncvar_get(file, "DATA_MODE"), "")[[1]] else NULL
    res@metadata$instReference <- if ("INST_REFERENCE" %in% varNames)
        as.vector(trimString(ncdf4::ncvar_get(file, "INST_REFERENCE"))) else NULL
    res@metadata$firmwareVersion <- if ("FIRMWARE_VERSION" %in% varNames)
        as.vector(trimString(ncdf4::ncvar_get(file, "FIRMWARE_VERSION"))) else NULL
    res@metadata$WMOInstType <- if ("WMO_INST_TYPE" %in% varNames)
        as.vector(trimString(ncdf4::ncvar_get(file, "WMO_INST_TYPE"))) else NULL
    res@metadata$juld <- if ("JULD" %in% varNames)
        as.vector(ncdf4::ncvar_get(file, "JULD")) else NULL
    ## set up 'time' also
    t0s <- as.vector(ncdf4::ncvar_get(file, "REFERENCE_DATE_TIME"))
    t0 <- strptime(t0s, "%Y%m%d%M%H%S", tz="UTC")
    julianDayTime <- as.vector(ncdf4::ncvar_get(file, "JULD"))
    res@data$time <- t0 + julianDayTime * 86400
    rm(list=c("t0s", "t0", "julianDayTime")) # no longer needed

    res@metadata$juldQc <- if ("JULD_QC" %in% varNames)
        as.vector(ncdf4::ncvar_get(file, "JULD_QC")) else NULL
    res@metadata$juldLocation <- if ("JULD_LOCATION" %in% varNames)
        as.vector(ncdf4::ncvar_get(file, "JULD_LOCATION")) else NULL

    if ("LATITUDE" %in% varNames) {
        res@data$latitude <- as.vector(ncdf4::ncvar_get(file, "LATITUDE"))
        latitudeNA <- ncdf4::ncatt_get(file, "LATITUDE","_FillValue")$value
        res@data$latitude[res@data$latitude == latitudeNA] <- NA
        rm(list="latitudeNA") # no longer needed
        res@metadata$units$latitude <-
            if (1 == length(grep("north", ncdf4::ncatt_get(file, "LATITUDE", "units")$value, ignore.case=TRUE)))
                list(unit=expression(degree*N), scale="") else list(unit=expression(degree*S), scale="")
    }
    if ("LONGITUDE" %in% varNames) {
        res@data$longitude <- as.vector(ncdf4::ncvar_get(file, "LONGITUDE"))
        longitudeNA <- ncdf4::ncatt_get(file, "LONGITUDE","_FillValue")$value
        res@data$longitude[res@data$longitude == longitudeNA] <- NA
        rm(list="longitudeNA") # no longer needed
        res@metadata$units$longitude <-
            if (1 == length(grep("east", ncdf4::ncatt_get(file, "LONGITUDE", "units")$value, ignore.case=TRUE)))
                list(unit=expression(degree*E), scale="") else list(unit=expression(degree*W), scale="")
    }

    res@metadata$positionQc <- if ("POSITION_QC" %in% varNames)
        as.vector(ncdf4::ncvar_get(file, "POSITION_QC")) else NULL
    res@metadata$positioningSystem <- if ("POSITIONING_SYSTEM" %in% varNames)
        as.vector(trimString(ncdf4::ncvar_get(file, "POSITIONING_SYSTEM"))) else NULL

    stationParameters <- unique(as.vector(res@metadata$stationParameters)) # will be PRES, TEMP etc
    for (item in stationParameters) {
        n <- item
        d <- getData(file, n)
        res@data[[argoDataNames(n)]] <- if (!is.null(d)) d else NULL

        n <- paste(item, "_QC", sep="")
        d <- getData(file, n)
        if (!is.null(d)) res@metadata$flags[[argoDataNames(n)]] <- argoDecodeFlags(d)
        n <- paste(item, "_ADJUSTED", sep="")
        if (n %in% varNames) {
            d <- getData(file, n)
            res@data[[argoDataNames(n)]] <- if (!is.null(d)) d else NULL
        }
        n <- paste(item, "_ADJUSTED_QC", sep="")
        if (n %in% varNames) {
            d <- getData(file, n)
            if (!is.null(d)) res@metadata$flags[[argoDataNames(n)]] <- argoDecodeFlags(d)
        }
        n <- paste(item, "_ADJUSTED_ERROR", sep="")
        if (n %in% varNames) {
            d <- getData(file, n)
            res@data[[argoDataNames(n)]] <- if (!is.null(d)) d else NULL
        }
    }
    res@metadata$filename <- filename
    if ("TEMP" %in% varNames) {
        ## leave some code in case we get a newer scale
        if (1 == length(grep("ITS-90", ncdf4::ncatt_get(file, "TEMP", "long_name")$value, ignore.case=TRUE)))
            res@metadata$units$temperature <- list(unit=expression(degree *C), scale="ITS-90")
        else res@metadata$units$temperature <- list(unit=expression(degree *C), scale="ITS-90")
    }
    if ("TEMP_ADJUSTED" %in% varNames) {
        ## leave some code in case we get a newer scale
        if (1 == length(grep("ITS-90", ncdf4::ncatt_get(file, "TEMP_ADJUSTED", "long_name")$value, ignore.case=TRUE)))
            res@metadata$units$temperatureAdjusted <- list(unit=expression(degree *C), scale="ITS-90")
        else res@metadata$units$temperatureAdjusted <- list(unit=expression(degree *C), scale="ITS-90")
    }
    if ("TEMP_ADJUSTED_ERROR" %in% varNames) {
        ## leave some code in case we get a newer scale
        if (1 == length(grep("ITS-90", ncdf4::ncatt_get(file, "TEMP_ADJUSTED_ERROR", "long_name")$value, ignore.case=TRUE)))
            res@metadata$units$temperatureAdjustedError <- list(unit=expression(degree *C), scale="ITS-90")
        else res@metadata$units$temperatureAdjustedError <- list(unit=expression(degree *C), scale="ITS-90")
    }
    if ("PSAL" %in% varNames) {
        ## leave some code in case we get a newer scale
        if (1 == length(grep("PRACTICAL", ncdf4::ncatt_get(file, "PSAL", "long_name")$value, ignore.case=TRUE)))
            res@metadata$units$salinity <- list(unit=expression(), scale="PSS-78")
        else
            res@metadata$units$salinity <- list(unit=expression(), scale="PSS-78")
    }
    if ("PSAL_ADJUSTED" %in% varNames) {
        ## leave some code in case we get a newer scale
        if (1 == length(grep("PRACTICAL", ncdf4::ncatt_get(file, "PSAL_ADJUSTED", "long_name")$value, ignore.case=TRUE)))
            res@metadata$units$salinityAdjusted <- list(unit=expression(), scale="PSS-78")
        else
            res@metadata$units$salinityAdjusted <- list(unit=expression(), scale="PSS-78")
    }
    if ("PSAL_ADJUSTED_ERROR" %in% varNames) {
        ## leave some code in case we get a newer scale
        if (1 == length(grep("PRACTICAL", ncdf4::ncatt_get(file, "PSAL_ADJUSTED_ERROR", "long_name")$value, ignore.case=TRUE)))
            res@metadata$units$salinityAdjustedError <- list(unit=expression(), scale="PSS-78")
        else
            res@metadata$units$salinityAdjustedError <- list(unit=expression(), scale="PSS-78")
    }
    if ("PRES" %in% varNames) {
        if (1 == length(grep("decibar", ncdf4::ncatt_get(file, "PRES", "units")$value, ignore.case=TRUE)))
            res@metadata$units$pressure <- list(unit=expression(dbar), scale="")
        else
            res@metadata$units$pressure <- list(unit=expression(dbar), scale="")
    }
    if ("PRES_ADJUSTED" %in% varNames) {
        if (1 == length(grep("decibar", ncdf4::ncatt_get(file, "PRES_ADJUSTED", "units")$value, ignore.case=TRUE)))
            res@metadata$units$pressureAdjusted <- list(unit=expression(dbar), scale="")
        else
            res@metadata$units$pressureAdjusted <- list(unit=expression(dbar), scale="")
    }
    if ("PRES_ADJUSTED_ERROR" %in% varNames) {
        if (1 == length(grep("decibar", ncdf4::ncatt_get(file, "PRES_ADJUSTED_ERROR", "units")$value, ignore.case=TRUE)))
            res@metadata$units$pressureAdjustedError <- list(unit=expression(dbar), scale="")
        else
            res@metadata$units$pressureAdjustedError<- list(unit=expression(dbar), scale="")
    }
    res@processingLog <- if (is.character(file))
        processingLogAppend(res@processingLog, paste("read.argo(\"", file, "\")", sep=""))
    else processingLogAppend(res@processingLog, paste(deparse(match.call()), sep="", collapse=""))
    res
}

#' Coerce data into argo dataset
#' 
#' Coerce a dataset into an argo dataset. This is not the right way to 
#' read official argo datasets, which are provided in NetCDF format and may
#' be read with \code{\link{read.argo}}.
#'
#' @param time vector of POSIXct times.
#' @param longitude vector of longitudes.
#' @param latitude vector of latitudes.
#' @param salinity vector of salinities.
#' @param temperature vector of temperatures.
#' @param pressure vector of pressures.
#' @param units optional list containing units. If \code{NULL}, the default,
#' then \code{"degree east"} is used for \code{longitude},
#' \code{"degree north"} for \code{latitude},
#' \code{""} for \code{salinity},
#' \code{"ITS-90"} for \code{temperature}, and
#' \code{"dbar"} for \code{pressure}.
#' @param id identifier.
#' @param filename source filename.
#' @param missingValue Optional missing value, indicating data that should be
#' taken as \code{NA}.
#' 
#' @return
#' An object of \code{\link{argo-class}}.
#' 
#' @seealso
#' The documentation for \code{\link{argo-class}} explains the structure of argo
#' objects, and also outlines the other functions dealing with them.
#' 
#' @author Dan Kelley
#' @family functions that deal with argo data
as.argo <- function(time, longitude, latitude,
                       salinity, temperature, pressure, 
                       units=NULL,
                       id, filename="",
                       missingValue)
{
    if (inherits(class, "data.frame")) {
        df <- time 
        names <- names(df)
        time <- if ("time" %in% names) df$time else NULL
        salinity <- if ("salinity" %in% names) df$salinity else NULL
        temperature <- if ("temperature" %in% names) df$temperature else NULL
        pressure <- if ("pressure" %in% names) df$pressure else NULL
        longitude <- if ("longitude" %in% names) df$longitude else NULL
        latitude <- if ("latitude" %in% names) df$latitude else NULL
        id <- if ("id" %in% names) df$id else NULL
    } else {
        if (missing(time)) stop("must give time")
        if (missing(longitude)) stop("must give longitude")
        if (missing(latitude)) stop("must give latitude")
        if (missing(temperature)) stop("must give temperature")
        if (missing(salinity)) stop("must give salinity")
        if (missing(pressure)) stop("must give pressure")
        if (missing(id)) stop("must give id")
    }
    res <- new("argo", time=time, id=id,
               longitude=longitude, latitude=latitude, salinity=salinity, 
               temperature=temperature, pressure=pressure, filename=filename)
    res@metadata$units <- if (!is.null(units)) units else
        list(longitude=list(expression(degree*E), scale=""),
             latitude=list(expression(degree*N), scale=""),
             salinity=list(unit=expression(), scale="PSS-78"), # assuming a particular scale
             temperature=list(unit=expression(degree*C), scale="ITS-90"), # assuming a particular scale
             pressure=list(unit=expression(dbar), scale="")) # assuming a particular unit
    res@processingLog <- processingLogAppend(res@processingLog, paste(deparse(match.call()), sep="", collapse=""))
    res
}


#' Plot argo data
#' 
#' Plot a summary diagram for argo data.
#' 
#' @param x object inheriting from \code{\link{argo-class}}.
#' 
#' @param which list of desired plot types, one of the following.
#' \itemize{
#'     \item \code{which=1} or \code{which="trajectory"} gives a 
#'     plot of the argo trajectory, with the coastline, if one is provided.
#' 
#'     \item \code{which=2} or \code{"salinity ts"} gives a time series of
#'     salinity at the indicated level(s)
#' 
#'     \item \code{which=3} or \code{"temperature ts"} gives a time series
#'     of temperature at the indicated level(s)
#' 
#'     \item \code{which=4} or \code{"TS"} gives a TS diagram at the
#'     indicated level(s)
#' 
#'     \item \code{which=5} or \code{"salinity profile"} gives a salinity
#'     profile of all the data (with S and p trimmed to the 1 and 99
#'     percentiles)
#' 
#'     \item \code{which=6} or \code{"temperature profile"} gives a
#'     temperature profile (with T and p trimmed to the 1 and 99
#'     percentiles)
#' }
#'     
#' @param level depth pseudo-level to plot, for \code{which=2} and higher.  May be an
#' integer, in which case it refers to an index of depth (1 being the top)
#' or it may be the string "all" which means to plot all data.
#' 
#' @param coastline character string giving the coastline to be used in an Argo-location
#' map, or \code{"best"} to pick the one with highest resolution, or
#' \code{"none"} to avoid drawing the coastline.
#' 
#' @param cex size of plotting symbols to be used if \code{type='p'}.
#' 
#' @param pch type of plotting symbols to be used if \code{type='p'}.
#' 
#' @param type plot type, either \code{"l"} or \code{"p"}.
#' 
#' @param col optional list of colours for plotting.
#' 
#' @param fill Either a logical, indicating whether to fill the land with
#' light-gray, or a colour name.  Owing to problems with some projections, the
#' default is not to fill.
#' 
#' @param adorn list of expressions to be executed for the panels in turn, e.g. to
#' adorn the plots.  If the number matches the number of panels, then the strings
#' are applied to the appropriate panels, as they are drawn from top-left to
#' bottom-right.   If only a single expression is provided, it is used for all
#' panels. (See \dQuote{Examples}.)
#' 
#' 
#' @param mgp 3-element numerical vector to use for \code{par(mgp)}, and also for
#' \code{par(mar)}, computed from this.  The default is tighter than the R
#' default, in order to use more space for the data and less for the axes.
#' 
#' @param projection indication of the projection to be used
#' in trajetory maps. If this is \code{NULL}, no projection is used, although
#' the plot aspect ratio will be set to yield zero shape distortion at the 
#' mean float latitude.  If \code{projection="automatic"}, then one
#' of two projections is used: stereopolar (i.e. \code{"+proj=stere +lon_0=X"}
#' where \code{X} is the mean longitude), or Mercator (i.e. \code{"+proj=merc"})
#' otherwise.  Otherwise, \code{projection} must be a character string specifying
#' a projection in the notation used by \link[rgdal]{project} in the \CRANpkg{rgdal};
#' this will be familiar to many readers as the PROJ.4 notation;
#' see \code{\link{mapPlot}}.
#'
#' @param mar value to be used with \code{\link{par}}("mar").
#' 
#' @param tformat optional argument passed to \code{\link{oce.plot.ts}}, for plot
#' types that call that function.  (See \code{\link{strptime}} for the format
#' used.)
#' 
#' @param debug debugging flag.
#' 
#' @param ... optional arguments passed to plotting functions.
#' 
#' @return None.
#' 
#' @examples
#' library(oce)
#' data(argo)
#' plot(argo, which="trajectory")
#' 
#' 
#' @references \url{http://www.argo.ucsd.edu/}
#' 
#' @aliases plot.argo
#' @author Dan Kelley
#' @family functions that deal with argo data
setMethod(f="plot",
          signature=signature("argo"),
          definition=function (x, which = 1, level,
                               coastline=c("best", "coastlineWorld", "coastlineWorldMedium",
                                           "coastlineWorldFine", "none"),
                               cex=1, pch=1, type='p', col, fill=FALSE, 
                               adorn=NULL,
                               projection=NULL,
                               mgp=getOption("oceMgp"), mar=c(mgp[1]+1.5, mgp[1]+1.5, 1.5, 1.5),
                               tformat,
                               debug=getOption("oceDebug"),
                               ...)
          {
              if (!inherits(x, "argo"))
                  stop("method is only for objects of class '", "argo", "'")
              oceDebug(debug, "plot.argo(x, which=c(", paste(which,collapse=","), "),",
                      " mgp=c(", paste(mgp, collapse=","), "),",
                      " mar=c(", paste(mar, collapse=","), "),",
                      " ...) {\n", sep="", unindent=1)
              coastline <- match.arg(coastline)
              #opar <- par(no.readonly = TRUE)
              lw <- length(which)
              ##if (lw > 1) on.exit(par(opar))
              ##if (length(type) < lw) type <- rep(type, lw) # FIXME: recycle more sensibly
              ##if (length(pch) < lw) pch <- rep(pch, lw) # FIXME: recycle more sensibly
              ##if (length(cex) < lw) cex <- rep(cex, lw) # FIXME: recycle more sensibly
              adorn.length <- length(adorn)
              if (adorn.length == 1) {
                  adorn <- rep(adorn, lw)
                  adorn.length <- lw
              }
              ## omar <- par('mar')
              nw  <- length(which)
              if (nw > 1) {
                  par(mfcol=c(1, nw), mgp=mgp, mar=mar)
              } else {
                  par(mgp=mgp, mar=mar)
              }
              if (missing(level) || level == "all")
                  level <- seq(1L, dim(x@data$temperature)[1])
              ctd <- as.ctd(x@data$salinity, x@data$temperature, x@data$pressure,
                            units=list(temperature=list(unit=expression(degree*C), scale="ITS-90"),
                                       conductivity=list(list=expression(ratio), scale=""))) # guess on units
              which <- oce.pmatch(which,
                                  list(trajectory=1,
                                       "salinity ts"=2,
                                       "temperature ts"=3,
                                       "TS"=4,
                                       "salinity profile"=5,
                                       "temperature profile"=6))
              for (w in 1:nw) {
                  if (which[w] == 1) {
                      oceDebug(debug, "which[", w, "] ==1, so plotting a map\n")
                      ## map
                      ## FIXME: coastline selection should be DRY
                      haveCoastline <- FALSE
                      haveOcedata <- requireNamespace("ocedata", quietly=TRUE)
                      lonr <- range(x[["longitude"]], na.rm=TRUE)
                      latr <- range(x[["latitude"]], na.rm=TRUE)
                      if (coastline == "best") {
                          if (haveOcedata) {
                              bestcoastline <- coastlineBest(lonRange=lonr, latRange=latr)
                              oceDebug(debug, " 'best' coastline is: \"", bestcoastline, '\"\n', sep="")
                              if (bestcoastline == "coastlineWorld") {
                                  data(list=bestcoastline, package="oce", envir=environment())
                              } else {
                                  data(list=bestcoastline, package="ocedata", envir=environment())
                              }
                              coastline <- get(bestcoastline)
                          } else {
                              bestcoastline <- coastlineBest(lonRange=lonr, latRange=latr)
                              oceDebug(debug, " using \"coastlineWorld\" because ocedata package not installed\n")
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
                      ## if (!is.character(coastline)) stop("coastline must be a character string")

                      if (!is.null(projection)) {
                          meanlat <- mean(x[['latitude']], na.rm=TRUE)
                          meanlon <- mean(x[['longitude']], na.rm=TRUE)
                          ## id <- pmatch(projection, "automatic")
                          if (!is.na(pmatch(projection, "automatic"))) {
                              projection <- if (meanlat > 70)
                                  paste("+proj=stere +lon_0=", meanlon, sep="") else "+proj=merc"
                              oceDebug(debug, "using", projection, "projection (chosen automatically)\n")
                          } else {
                              oceDebug(debug, "using", projection, "projection (specified)\n")
                          }
                          mapPlot(x[["longitude"]], x[["latitude"]],
                                  projection=projection,
                                  type='p', cex=cex, pch=pch,
                                  col=if (missing(col)) "black" else col,
                                  debug=debug-1)
                          if (is.logical(fill) && fill) {
                              mapPolygon(coastline[['longitude']], coastline[['latitude']], col='lightgray')
                          } else {
                              if (is.character(fill)) {
                                  mapPolygon(coastline[['longitude']], coastline[['latitude']], col=fill) 
                              } else {
                                  mapPolygon(coastline[['longitude']], coastline[['latitude']])
                              }
                          }
                      } else {
                          asp <- 1 / cos(mean(range(x@data$latitude, na.rm=TRUE)) * atan2(1,1) / 45)
                          plot(x@data$longitude, x@data$latitude, asp=asp, 
                               type=type, cex=cex, pch=pch,
                               col=if (missing(col)) "black" else col,
                               xlab=resizableLabel("longitude"), ylab=resizableLabel("latitude"), ...)

                          if (haveCoastline) {
                              if (!is.null(coastline@metadata$fillable) && coastline@metadata$fillable) {
                                  polygon(coastline[["longitude"]], coastline[["latitude"]], col="lightgray", lwd=3/4)
                                  polygon(coastline[["longitude"]]+360, coastline[["latitude"]], col="lightgray", lwd=3/4)
                              } else {
                                  lines(coastline[["longitude"]], coastline[["latitude"]], col="darkgray")
                                  lines(coastline[["longitude"]]+360, coastline[["latitude"]], col="darkgray")
                              }
                          }
                          if (!missing(coastline)) {
                              polygon(coastline[["longitude"]], coastline[["latitude"]], col='lightgray')
                              if (type[w] == 'l')
                                  lines(x@data$longitude, x@data$latitude)
                              else
                                  points(x@data$longitude, x@data$latitude, cex=cex, pch=pch, col=if(!missing(col))col)
                          }
                      }
                      par(mar=mar)
                  } else if (which[w] == 2) {    # salinity timeseries
                      if (0 != sum(!is.na(x@data$salinity))) {
                          nlevels <- dim(x@data$salinity)[1]
                          t <- if (length(level) > 1)
                              numberAsPOSIXct(t(matrix(rep(x@data$time, nlevels), byrow=FALSE, ncol=nlevels)))
                          else
                              x@data$time
                          oce.plot.ts(t, as.vector(x@data$salinity[level,]),
                                      ylab=resizableLabel("S", "y"), type=type, 
                                      col=if (missing(col)) "black" else col,
                                      tformat=tformat, ...)
                      } else {
                          warning("no non-missing salinity data")
                      }
                  } else if (which[w] == 3) {    # temperature timeseries
                      if (0 != sum(!is.na(x@data$temperature))) {
                          nlevels <- dim(x@data$temperature)[1]
                          t <- if (length(level) > 1)
                              numberAsPOSIXct(t(matrix(rep(x@data$time, nlevels), byrow=FALSE, ncol=nlevels)))
                          else
                              x@data$time
                          oce.plot.ts(t, x@data$temperature[level,],
                                      ylab=resizableLabel("T", "y"), type=type,
                                      col=if (missing(col)) "black" else col,
                                      tformat=tformat, ...)
                      } else {
                          warning("no non-missing temperature data")
                      }
                  } else if (which[w] == 4) {    # TS
                      if (0 != sum(!is.na(x@data$temperature)) && 0 != sum(!is.na(x@data$salinity))) {
                          plotTS(ctd, col=if (missing(col)) "black" else col, type=type, ...)
                     } else {
                          warning("no non-missing salinity data")
                      }
                  } else if (which[w] == 5) {    # S profile
                      ## FIXME: how to handle the noise; if as below, document it
                      plotProfile(ctd, xtype="salinity",
                           Slim=quantile(x@data$salinity, c(0.01, 0.99), na.rm=TRUE),
                           ylim=quantile(x@data$pressure, c(0.99, 0.01), na.rm=TRUE),
                           col=if (missing(col)) "black" else col, type=type)
                  } else if (which[w] == 6) {    # T profile
                      ## FIXME: how to handle the noise; if as below, document it
                      plotProfile(ctd, xtype="temperature",
                           Tlim=quantile(x@data$temperature, c(0.01, 0.99), na.rm=TRUE),
                           ylim=quantile(x@data$pressure, c(0.99, 0.01), na.rm=TRUE),
                           col=if (missing(col)) "black" else col, type=type)
                  } else {
                      stop("plot.difter() given unknown value of which=", which[w], "\n", call.=FALSE)
                  }
              }
              oceDebug(debug, "} # plot.argo()\n", unindent=1)
              invisible()
          })

