## vim:textwidth=128:expandtab:shiftwidth=4:softtabstop=4

## To save storage, this new() function has arguments only for quantities that are present in almost all cases. For example, not
## all data files will have oxygen, so that's not present here. Similarly, not all files have data-quality columns, so they are
## not present either. Columnar data should be added after the object is created, using ctdAddColumn(), which updates metadata
## as needed. As for adding metadata, do that directly. Examples of these things are seen throughout this file.  Note that
## normal users should employ read.ctd() or as.ctd() to create ctd objects ... this function is intended for internal use, and
## may be changed at any moment.
setMethod(f="initialize",
          signature="ctd",
          definition=function(.Object, pressure, salinity, temperature, conductivity, 
                              units,# =list(), #=list(temperature="ITS-90", conductivity="ratio"),
                              pressureType, deploymentType) {
              ## Assign to some columns so they exist if needed later (even if they are NULL)
              .Object@data$pressure <- if (missing(pressure)) NULL else pressure
              .Object@data$temperature <- if (missing(temperature)) NULL else temperature
              .Object@data$salinity <- if (missing(salinity)) NULL else salinity
              .Object@data$conductivity <- if (missing(conductivity)) NULL else conductivity
              names <- names(.Object@data)
              .Object@metadata$names <- names
              .Object@metadata$labels <- paste(toupper(substring(names,1,1)), substring(names,2),sep="")
              ##.Object@metadata$filename <- filename
              if (missing(units)) {
                  .Object@metadata$units <- list(temperature="ITS-90", conductivity="ratio")
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


setMethod(f="summary",
          signature="ctd",
          definition=function(object, ...) {
              ##mnames <- names(object@metadata)
              cat("CTD Summary\n-----------\n\n")
              showMetadataItem(object, "type", "Instrument: ")
              showMetadataItem(object, "model", "Instrument model:  ")
              showMetadataItem(object, "serialNumber",             "Instr. serial no.:   ")
              showMetadataItem(object, "serialNumberTemperature",  "Temp. serial no.:    ")
              showMetadataItem(object, "serialNumberConductivity", "Cond. serial no.:    ")
              showMetadataItem(object, "filename",                  "File source:         ")
              showMetadataItem(object, "hexfilename",               "Original file source (hex):  ")
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

setMethod(f="[[",
          signature(x="ctd", i="ANY", j="ANY"),
          ##definition=function(x, i, j=NULL, drop=NULL) {
          definition=function(x, i, j, drop) {
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
              } else if (i == "temperature" || i == "t") { # FIXME: document "t" part
                  x@data$temperature
              } else if (i == "temperature68") {
                  T68fromT90(x@data$temperature)
              } else if (i == "pressure" || i == "p") {
                  x@data$pressure
              } else if (i == "longitude") {
                  if ("longitude" %in% names(x@data)) x@data$longitude else x@metadata$longitude
              } else if (i == "latitude") {
                  if ("latitude" %in% names(x@data)) x@data$latitude else x@metadata$latitude
              } else if (i == "N2") {
                  swN2(x)
              } else if (i == "sigmaTheta") {
                  swSigmaTheta(x)
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
              } else if (length(grep("Unit$", i))) {
                  if ("units" %in% names(x@metadata)) {
                      x@metadata$units[[gsub("Unit$", "", i)]]
                  } else {
                      x@metadata[[i]]
                  }
              } else {
                  ## I use 'as' because I could not figure out callNextMethod() etc
                  ## res <- as(x, "oce")[[i, j, drop]]
                  as(x, "oce")[[i]]
              }
          })

as.ctd <- function(salinity, temperature=NULL, pressure=NULL, conductivity=NULL,
                   SA=NULL, CT=NULL, oxygen=NULL, nitrate=NULL, nitrite=NULL, phosphate=NULL, silicate=NULL,
                   scan=NULL, time=NULL, other=NULL,
                   ## salinityFlag, temperatureFlag, pressureFlag, conductivityFlag, SAFlag, CTFlag, oxygenFlag, nitrateFlag,
                   ## nitriteFlag, phosphateFlag, silicateFlag,
                   units=NULL,
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
    if (missing(salinity)) {
        stop("must provide salinity")
        ##if (inherits(salinity, "ctd"))
        ##    return(salinity) # a convenience that lets us coerce without altering
        ## 1. coerce an oce object (with special tweaks for rsk)
    } else if (inherits(salinity, "oce")) {
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
            if (missing(units)) # this lets the user over-ride
                units <- list(temperature="ITS-90", conductivity="mS/cm")
            salinity <- swSCTp(conductivity=conductivity/42.914, temperature=temperature, pressure=pressure)
        } else {
            salinity <- d$salinity # FIXME: ok for objects (e.g. rsk) that lack salinity?
        }
        if (inherits(o, "ctd") && missing(units)) {
            if (missing(units)) # this lets the user over-ride
                units <- o@metadata$units
        }
        ##res <- new("ctd", pressure=pressure, salinity=salinity, temperature=temperature,
        ##           conductivity=conductivity,
        ##           units=units, pressureType=pressureType)
        res@metadata$units <- units
        res@metadata$pressureType <- pressureType
        res@data$pressure <- pressure
        res@data$salinity <- salinity
        res@data$temperature <- temperature
        res@data$conductivity <- conductivity
        res <- ctdAddColumn(res, swSigmaTheta(salinity, temperature, pressure),
                            name="sigmaTheta", label="Sigma Theta", unit="kg/m^3")
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
            ## res <- new("ctd",
            ##            pressure=x$pressure, salinity=x$salinity, temperature=x$temperature,
            ##            units=units, pressureType=pressureType)
            res@data$pressure <- x$pressure
            res@data$salinity <- x$salinity
            res@data$temperature <- x$temperature
            res@metadata$units <- units
            res@metadata$pressureType <- pressureType
        } else if (3 == sum(c("PSAL", "TEMP", "PRES") %in% names)) {
            ## res <- new("ctd",
            ##            pressure=x$PRES, salinity=x$PSAL, temperature=x$TEMP,
            ##            units=units, pressureType=pressureType)
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
        ## res <- new('ctd',
        ##            units=units, #if (!length(units)) list(temperature="ITS-90", conductivity="ratio") else units,
        ##            pressureType=pressureType)
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
            scan <- as.numeric(seq_along(salinity))
        data <- list(salinity=salinity,
                     temperature=temperature,
                     pressure=pressure,
                     sigmaTheta=swSigmaTheta(salinity, temperature, pressure)) # FIXME: what about gsw?
        if (!missing(scan)) data$scan <- as.vector(scan)
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
        labels <- paste(toupper(substring(names,1,1)),substring(names,2),sep="")
        if (length(longitude) != length(latitude))
            stop("lengths of longitude and latitude must match")
        if (1 < length(longitude) && length(longitude) != length(salinity))
            stop("lengths of salinity and longitude must match")
        ## FIXME: should sampleInterval be a default?
        res@metadata$names <- names
        res@metadata$labels <- labels
        if (!missing(filename)) res@metadata$filename <- filename
        if (!missing(ship)) res@metadata$ship <- ship
        if (!missing(scientist)) res@metadata$scientist <- scientist
        if (!missing(institute)) res@metadata$institute <- institute
        if (!missing(address)) res@metadata$address <- address
        if (!missing(cruise)) res@metadata$cruise <- cruise
        if (!missing(station)) res@metadata$station <- station
        if (!missing(date)) res@metadata$date <- date
        if (!missing(startTime)) res@metadata$startTime <- startTime
        if (!missing(recovery)) res@metadata$recovery <- recovery
        if (!missing(type)) res@metadata$type <- type
        if (!missing(model)) res@metadata$model <- model
        if (!missing(serialNumber)) res@metadata$serialNumber <- serialNumber
        ## if (!missing(systemUploadTime)) metadata$systemUploadTime <- systemUploadTime
        if (!missing(src)) res@metadata$src <- src
        ## If lon and lat are vectors, place in data, with averages in metadata.
        if (length(latitude) == 1) {
            res@metadata$longitude <- longitude
            res@metadata$latitude <- latitude
        } else {
            if (length(latitude) != length(temperature))
                stop("lengths of latitude and temperature must match")
            data$longitude <- longitude
            data$latitude <- latitude
        }
        res@data <- data
    }
    if (is.na(res@metadata$waterDepth) && !is.na(waterDepth))
        res@metadata$waterDepth <- waterDepth
    oceDebug(debug, "} # as.ctd()\n", sep="", unindent=1)
    res
}

ctdAddColumn <- function (x, column, name, label, unit="", debug = getOption("oceDebug"))
{
    ## FIXME: not using the unit
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
    res@processingLog <- processingLogAppend(res@processingLog, paste("ctdAddColumn(..., name=\"", name, "\", ...)", sep=""))
    oceDebug(debug, "} # ctdAddColumn()\n", sep="", unindent=1)
    res
}

##method=c("boxcar", "approx", "lm", "rr", "unesco"),
## SHOULD ADD: spline; supsmu; ...
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
                dataNew[[datumName]] <- method(pressure, x[[datumName]], pt)
            }
        }
        dataNew[["pressure"]] <- pt
    } else {
        if (method == "approx") {
            tooDeep <- pt > max(x@data[["pressure"]], na.rm=TRUE)
            for (datumName in dataNames) {
                oceDebug(debug, "decimating \"", datumName, "\"\n", sep="")
                if (!length(x[[datumName]])) {
                    dataNew[[datumName]] <- NULL
                } else {
                    if (datumName != "pressure") {
                        good <- sum(!is.na(x@data[[datumName]]))
                        if (good > 2) {
                            dataNew[[datumName]] <- approx(x@data[["pressure"]], x@data[[datumName]], pt, rule=2)$y
                            dataNew[[datumName]][tooDeep] <- NA
                        } else {
                            oceDebug(debug, " note: fewer than 2 good data in the above\n")
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
                        pred <- oce.approx(xvar, yvar, pt, method=method)
                        dataNew[[datumName]] <- pred
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
                        dataNew[[datumName]] <- binMean1D(p, x@data[[datumName]], xbreaks=pbreaks)$result
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
    res@data <- dataNew
    res@processingLog <- processingLogAppend(res@processingLog, paste(deparse(match.call()), sep="", collapse=""))
    oceDebug(debug, "} # ctdDecimate()\n", unindent=1)
    res
}


ctdFindProfiles<- function(x, cutoff=0.5, minLength=10, minHeight=0.1*diff(range(x[["pressure"]])),
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

read.ctd.odf <- function(file, columns=NULL, station=NULL, missing.value=-999, monitor=FALSE,
                         debug=getOption("oceDebug"), processingLog, ...)
{
    oceDebug(debug, "read.ctd.odf() {")
    if (!is.null(columns)) warning("'columns' is ignored by read.ctd.odf() at present")
    odf <- read.odf(file)
    res <- as.ctd(odf)
    if (!is.null(station))
        res@metadata$station <- station
    res@metadata$units <- list(temperature="ITS-90", conductivity="ratio") # FIXME just a guess for ODV
    oceDebug(debug, "} # read.ctd.odf()")
    res
}


ctdTrim <- function(x, method, removeDepthInversions=FALSE, parameters=NULL,
                   debug=getOption("oceDebug"))
{
    oceDebug(debug, "ctdTrim() {\n", unindent=1)
    methodIsFunction <- !missing(method) && is.function(method)
    if (!inherits(x, "ctd"))
        stop("method is only for objects of class '", "ctd", "'")
    if (!("scan" %in% names(x@data))) {
        x@data$scan <- seq_along(x@data[["pressure"]])
    }
    res <- x
    if (!methodIsFunction) {
        n <- length(x@data$pressure)
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
            keep <- (x@data$pressure > pmin) # 2. in water (or below start depth)
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
            max.location <- which.max(smooth(x@data$pressure, kind="3R"))
            keep[max.location:n] <- FALSE
            oceDebug(debug, "removed data at indices from ", max.location,
                     " (where pressure is ", x@data$pressure[max.location], ") to the end of the data\n", sep="")
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
                ## bilinearAold <- function(param) { # param=c(s0,p0,dpds); this uses ss and pp
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
                bilinearA <- function(s, s0, p0, dpds) { # same model as B but results treated differently
                    oceDebug(debug-1, "bilinearA s0=", s0, "p0=", p0, "dpds=", dpds, "\n")
                    ifelse(s < s0, p0, p0+dpds*(s-s0))
                }
                bilinearB <- function(s, s0, dpds) {
                    oceDebug(debug-1, "bilinearB s0=", s0, "dpds=", dpds, "\n")
                    ifelse(s < s0, 0, dpds*(s-s0))
                }
                pp <- x@data$pressure[keep]
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
    ##     message("DANNY")
    ##     res@metadata$waterDepth <- max(res@data$pressure, na.rm=TRUE)
    ##     waterDepthWarning <- TRUE
    ## }
    if (removeDepthInversions) {
        badDepths <- c(FALSE, diff(res@data$pressure) <= 0)
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

ctdUpdateHeader <- function (x, debug = FALSE)
{
    if (length(x@metadata$header) < 1)
        stop("there is no header in this CTD object")
    if (length(x@data) < 1)
        stop("there are no data in this CTD object")
    replaceHeaderElement <- function(h, match, new)
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

setMethod(f="plot",
          signature=signature("ctd"),
          definition=function(x, which,
                              col=par("fg"), fill=FALSE,
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
              if (missing(which)) {
                  oceDebug(debug, "plot.ctd(..., eos=\"", eos, "\", inset=", inset, ", ...) {\n", sep="", unindent=1)
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
                  oceDebug(debug, "plot.ctd(..., which=c(", paste(which, collapse=",", sep=""),
                           "), eos=\"", eos, "\", inset=", inset, ", ...) {\n", sep="", unindent=1)
              }
              lw <- length(which)
              dots <- list(...)
              dotsNames <- names(dots)
              ## FIXME: In the below, we could be more clever for single-panel plots
              ## but it may be better to get users out of the habit of supplying xlim
              ## etc (which will yield errors in plot.lm(), for example).
              if ("xlim" %in% dotsNames)
                  stop("in plot.ctd() : 'xlim' argument not allowed; use Slim for a salinity profile, Tlim for a temperature profile, etc", call.=FALSE)
              if ("ylim" %in% dotsNames)
                  stop("in plot.ctd() : 'ylim' argument not allowed; use plim for a profile, Tlim for a TS plot, etc", call.=FALSE)
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
              ##dec_deg <- function(x, code = "lat")
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
                          warning("plot.ctd(): unknown plot type \"", whichOrig[w], "\" requested\n", call.=FALSE)
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
                      textItem <- function(xloc, yloc, item, label, cex=0.8, d.yloc=0.8) {
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
                              ## FIXME: maybe demand say 10 coastline points in view
                              nearest <- mean(head(sort(d), 20), na.rm=TRUE) # in km
                              span <- 2 * nearest
                              oceDebug(debug, "span not given, and nearest point is", nearest,
                                       "km away, so set span=", span, "\n")
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
                                  oceDebug(debug, "fill=", fill, "\n")
                                  oceDebug(debug, "ok, about to call plot(coastline)\n")
                                  plot(coastline,
                                       clatitude=mean(latlim.c), clongitude=clon, span=span,
                                       projection=projection, parameters=parameters, orientation=orientation,
                                       fill=fill,
                                       mgp=mgp, mar=mar, inset=inset, cex.axis=cex.axis,
                                       lonlabel=lonlabel, latlabel=latlabel, sides=sides,
                                       debug=debug-1)
                                  oceDebug(debug, " ... did plot(coastline)\n")
                              } else {
                                  oceDebug(debug, "CASE 2: latlim given, lonlim missing\n")
                                  clat <- mean(latlim)
                                  plot(coastline,
                                       clatitude=clat, clongitude=clon, span=span,
                                       projection=projection, parameters=parameters, orientation=orientation,
                                       fill=fill,
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
                                       clatitude=clat, clongitude=clon, span=span,
                                       projection=projection, parameters=parameters, orientation=orientation,
                                       fill=fill,
                                       mgp=mgp, mar=mar, inset=inset, cex.axis=cex.axis,
                                       lonlabel=lonlabel, latlabel=latlabel, sides=sides,
                                       debug=debug-1)
                              } else {
                                  oceDebug(debug, "CASE 4: both latlim and lonlim given\n")
                                  clat <- mean(latlim)
                                  plot(coastline,
                                       clatitude=clat, clongitude=clon, span=span,
                                       fill=fill,
                                       projection=projection, parameters=parameters, orientation=orientation,
                                       mgp=mgp, mar=mar, inset=inset, cex.axis=cex.axis,
                                       lonlabel=lonlabel, latlabel=latlabel, sides=sides,
                                       debug=debug-1)
                              }
                          }
                          if (is.null(projection)) {
                              points(x[["longitude"]], x[["latitude"]],
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
              oceDebug(debug, "} # plot.ctd()\n", unindent=1)
              invisible()
          })

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
 

plotScan <- function(x, which=1, type='l', mgp=getOption("oceMgp"),
                     mar=c(mgp[1]+1.5,mgp[1]+1.5,mgp[1],mgp[1]), ...)
{
    if (!inherits(x, "ctd"))
        stop("method is only for objects of class '", "ctd", "'")
    nw <- length(which)
    if (nw > 1)
        par(mfrow=c(nw,1))
    par(mar=mar)
    par(mgp=mgp)
    scan <- if (("scan" %in% names(x@data))) x[["scan"]] else seq_along(x@data$pressure)
    for (w in which) {
        if (w == 1) {
            plot(scan, x@data$pressure, ylab=resizableLabel("p", "y"), xlab="Scan", yaxs='r', type=type, ...)
        } else if (w == 2) {
            dp <- diff(x@data$pressure)
            dp <- c(dp, dp[length(dp)])
            plot(scan, dp, ylab="diff(pressure)", xlab="Scan", yaxs='r', type=type, ...)
        } else if (w == 3) {
            dp <- diff(x@data$pressure)
            dp <- c(dp, dp[length(dp)])
            plot(scan, x[["temperature"]], ylab=resizableLabel("T", "y"),
                 xlab="Scan", yaxs='r', type=type, ...)
        } else if (w == 4) {
            dp <- diff(x@data$pressure)
            dp <- c(dp, dp[length(dp)])
            plot(scan, x[["salinity"]], ylab=resizableLabel("S", "y"),
                 xlab="Scan", yaxs='r', type=type, ...)
        } else {
            stop("unknown 'which'; must be in 1:4")
        }
    }
    ## mtext(x@metadata$station, side=3, adj=1, cex=par('cex'))
    ## mtext(latlonFormat(x@metadata$latitude, x@metadata$longitude, digits=5), side=3, adj=0, cex=par('cex'))
    ## if (1 <= adorn.length) {
    ##     t <- try(eval(adorn[1]), silent=TRUE)
    ##     if (class(t) == "try-error")
    ##         warning("cannot evaluate adorn[", 1, "]\n")
    ## }

    ## ##    par(mar=c(4,4,1,4)) # bot left top right
    ## Slen <- length(x@data$salinity)
    ## Tlen <- length(x@data$temperature)
    ## if (Slen != Tlen)
    ##     stop(paste("length mismatch.  'salinity' has length ", Slen, " but 'temperature' has length ", Tlen, sep=""))
    ## plot(x[[name]], x[["temperature"]], xlab="scan", ylab=resizableLabel("T", "y"),
    ##      yaxs='r', type=type)
    ## grid()
    ## if (2 <= adorn.length) {
    ##     t <- try(eval(adorn[2]), silent=TRUE)
    ##     if (class(t) == "try-error")
    ##         warning("cannot evaluate adorn[", 2, "]\n")
    ## }
    ## plot(x[[name]], x[['salinity']], xlab="scan", ylab=resizableLabel("S", "y"),
    ##      yaxs='r', type=type)
    ## grid()
}
##* Sea-Bird SBE 25 Data File:
##CTD,20060609WHPOSIODAM

read.ctd <- function(file, type=NULL, columns=NULL, station=NULL, monitor=FALSE, debug=getOption("oceDebug"), processingLog, ...)
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
                   SBE19 = read.ctd.sbe(file, columns=columns, station=station, monitor=monitor,
                                        debug=debug, processingLog=processingLog, ...),
                   WOCE  = read.ctd.woce(file, columns=columns, station=station, missing.value=-999, monitor=monitor,
                                         debug=debug, processingLog=processingLog, ...),
                   ODF = read.ctd.odf(file, columns=columns, station=station, monitor=monitor,
                                      debug=debug, processingLog=processingLog, ...),
                   ODV = read.ctd.odv(file, columns=columns, station=station, monitor=monitor,
                                      debug=debug, processingLog=processingLog, ...),
                   ITP = read.ctd.itp(file, columns=columns, station=station, monitor=monitor,
                                      debug=debug, processingLog=processingLog, ...))
    ## water depth is sometimes zero, which is a hassle in section plots, so make a guess
    #if (!"waterDepth" %in% names(res@metadata)) # may be entirely missing
    #    res@metadata$waterDepth <- max(res@data$pressure, na.rm=TRUE)
    #if (res@metadata$waterDepth < 1)   # may be silly
    #    res@metadata$waterDepth <- max(res@data$pressure, na.rm=TRUE)
    res
}

read.ctd.woce <- function(file, columns=NULL, station=NULL, missing.value=-999, monitor=FALSE,
                          debug=getOption("oceDebug"), processingLog, ...)
{
    if (length(grep("\\*", file))) {
        oceDebug(debug, "bread.ctd.woce(file=\"", file, "\") { # will read a series of files\n", unindent=1)
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
                names <- strsplit(tolower(header[i]), ",")[[1]]
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
        res@metadata$units <- list(temperature="ITS-90", conductivity="ratio")
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
        nlines <- length(lines)
        pressure <- vector("numeric", nlines)
        temperature <- vector("numeric", nlines)
        salinity <- vector("numeric", nlines)
        oxygen <- vector("numeric", nlines)
        b <- 0
        oceDebug(debug, "pcol:", pcol, ", Scol:", Scol, ", Tcol:", Tcol, ", Ocol:", Ocol, "\n")
        for (iline in 1:nlines) {
            if (0 < (length(grep("END_DATA", lines[iline]))))
                break
            items <- strsplit(lines[iline], ",")[[1]]
            pressure[iline] <- as.numeric(items[pcol])
            salinity[iline] <- as.numeric(items[Scol])
            temperature[iline] <- as.numeric(items[Tcol])
            oxygen[iline] <- as.numeric(items[Ocol])
            if (monitor) {
                cat(".")
                if (!((b+1) %% 50))
                    cat(b+1, "\n")
            }
            b <- b + 1
        }
        pressure <- pressure[1:b]
        temperature <- temperature[1:b]
        salinity <- salinity[1:b]
        oxygen <- oxygen[1:b]
        if (monitor)
            cat("\nRead", b-1, "lines of data\n")
        pressure[pressure == missing.value] <- NA
        salinity[salinity == missing.value] <- NA
        temperature[temperature == missing.value] <- NA
        sigmaTheta <- swSigmaTheta(salinity, temperature, pressure)
        data <- list(pressure=pressure, salinity=salinity, temperature=temperature, sigmaTheta=sigmaTheta)
        names <- c("pressure", "salinity", "temperature", "sigmaTheta", "oxygen")
        labels <- c("Pressure", "Salinity", "Temperature", "Sigma Theta", "Oxygen")
        if (length(oxygen) > 0) {
            oxygen[oxygen == missing.value] <- NA
            data <- list(pressure=pressure, salinity=salinity, temperature=temperature, sigmaTheta=sigmaTheta, oxygen=oxygen)
        }
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
        res@metadata$units <- list(temperature="ITS-90", conductivity="ratio")
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
    res
}

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

read.ctd.sbe <- function(file, columns=NULL, station=NULL, missing.value, monitor=FALSE, debug=getOption("oceDebug"), processingLog, ...)
{
    if (!is.null(columns)) {
        columnsNames <- names(columns)
        if (!("temperature" %in% columnsNames)) stop("'columns' must contain 'temperature'")
        if (!("pressure" %in% columnsNames)) stop("'columns' must contain 'pressure'")
        if (!("salinity" %in% columnsNames)) stop("'columns' must contain 'salinity'")
        if (3 > length(columns)) stop("'columns' must contain three or more elements")
    }

    if (length(grep("\\*", file))) {
        oceDebug(debug, "read.ctd.sbe(file=\"", file, "\") { # will read a series of files\n", unindent=1)
        files <- list.files(pattern=file)
        nfiles <- length(files)
        if (monitor)
            pb <- txtProgressBar(1, nfiles, style=3)
        res <- vector("list", nfiles)
        for (i in 1:nfiles) {
            res[[i]] <- read.ctd.sbe(files[i], debug=debug-1)
            if (monitor)
                setTxtProgressBar(pb, i)
        }
        oceDebug(debug, "} # read.ctd.sbe() {\n")
        return(res)
    }
    oceDebug(debug, "read.ctd.sbe(file=\"", file, "\") {\n", unindent=1)

    ## Read Seabird data file.  Note on headers: '*' is machine-generated,
    ## '**' is a user header, and '#' is a post-processing header.
    filename <- ""
    if (is.character(file)) {
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
    res <- new("ctd", pressureType="sea")
    ## Header
    scientist <- ship <- institute <- address <- cruise <- hexfilename <- ""
    sampleInterval <- NA
    systemUploadTime <- NULL
    latitude <- longitude <- NA
    startTime <- NULL
    waterDepth <- NA
    date <- recovery <- NA
    header <- c()
    col.names.inferred <- NULL
    found.time <- FALSE
    ##conductivity.standard <- 4.2914
    found.header.latitude <- found.header.longitude <- FALSE
    serialNumber <- serialNumberConductivity <- serialNumberTemperature <- ""
    conductivityUnit = "ratio"         # guess; other types are "mS/cm" and "S/m"
    temperatureUnit = "ITS-90"         # guess; other option is IPTS-68
    pressureType = "sea"               # guess; other option is "absolute"

    lines <- readLines(file)
    for (iline in seq_along(lines)) {
        line <- lines[iline]
        #line <- scan(file, what='char', sep="\n", n=1, quiet=TRUE)
        oceDebug(debug, "examining header line '",line,"'\n", sep="")
        header <- c(header, line)
        ##if (length(grep("\*END\*", line))) #BUG# why is this regexp no good (new with R-2.1.0)
        aline <- iconv(line, from="UTF-8", to="ASCII", sub="?")
        if (length(grep("END", aline, perl=TRUE, useBytes=TRUE))) {
            ## Sometimes SBE files have a header line after the *END* line.
            iline <- iline + 1  
            if (length(grep("[a-cf-zA-CF-Z]", lines[iline])))
                iline <- iline + 1
            break
        }
        lline <- tolower(aline)
        ## BUG: discovery of column names is brittle to format changes
        found.depth <- FALSE
        if (0 < (r <- regexpr("# name ", lline))) {
            oceDebug(debug, "lline: '",lline,"'\n",sep="")
            tokens <- strsplit(line, split=" ", useBytes=TRUE)
            oceDebug(debug, "   successfully tokenized\n")
            name <- tokens[[1]][6]
            oceDebug(debug, "  name: '",name,"'\n",sep="")
            if (0 < regexpr("scan", lline)) {
                name <- "scan"
                found.scan <- TRUE
            }
            if (0 < regexpr("pressure", lline)) {
                if (0 > regexpr("deg c", lline)) {
                    ## ignore "# name 5 = ptempC: Pressure Temperature [deg C]"
                    name <- "pressure"
                    found.pressure <- TRUE
                }
            }
            if (0 < regexpr("time", lline)) {
                name <- "time"
                found.time <- TRUE
            }
            if (0 < regexpr("salinity", lline)) {
                name <- "salinity"
                found.salinity <- TRUE
            }
            if (0 < regexpr("temperature", lline)) {
                ## ignore "# name 5 = ptempC: Pressure Temperature [deg C]"
                if (0 > regexpr("pressure", lline) && 0 > regexpr("potential", lline)) {
                    name <- "temperature"
                    found.temperature <- TRUE
                    unit <- gsub(":.*","",gsub(".*=[ ]*","", line))
                    if (length(grep("68", unit)))
                        temperatureUnit <- "IPTS-68"
                    else if (length(grep("90", unit)))
                        temperatureUnit <- "ITS-90"
                    oceDebug(debug, "temperatureUnit: ", temperatureUnit, "(inferred from '", unit, "'\n", sep="")
                }
            }
            if (0 < regexpr("conductivity", lline)) {
                if (0 < regexpr("ratio", lline)) {
                    found.conductivity.ratio <- TRUE;
                    name <- "conductivityratio"
                    conductivityUnit = "ratio"
                } else {
                    found.conductivity <- TRUE;
                    name <- "conductivity"
                    unit <- gsub(":.*","",gsub(".*=[ ]*","", line))
                    if (length(grep("S/m", unit)))
                        conductivityUnit <- "S/m"
                    else if (length(grep("mS/cm", unit)))
                        conductivityUnit <- "mS/cm"
                }
            }
            if (0 < regexpr("depth", lline) || 0 < regexpr("depSM", lline)) {
                name <- "depth"
                found.depth <- TRUE
            }
            if (0 < regexpr("fluorometer", lline))
                name <- "fluorometer"
            ## Used to have oxygen.temperature and oxygen.current here (why??)
            if (0 < regexpr("oxygen", lline))
                name <- "oxygen"
            if (0 < regexpr("flag", lline)) name <- "flag"
            if (0 < regexpr("sigma-theta", lline)) {
                name <- "sigmaTheta"
                ##foundSigmaTheta <- TRUE
            } else {
                if (0 < regexpr("sigma-t", lline)) {
                    name <- "sigmat"
                    ##foundSigmaT <- TRUE
                }
            }
            col.names.inferred <- c(col.names.inferred, name)
        }
        if (0 < regexpr(".*seacat profiler.*", lline))
            serialNumber <- gsub("[ ].*$","",gsub(".*sn[ ]*","",lline))
        if (length(grep("^\\* temperature sn", lline)))
            serialNumberTemperature <- gsub("^.*=\\s", "", lline)
        if (length(grep("^\\* conductivity sn", lline)))
            serialNumberConductivity <- gsub("^.*=\\s", "", lline)
        if (0 < (r<-regexpr("date:", lline))) {
            d <- sub("(.*)date:([ ])*", "", lline)
            date <- decodeTime(d, "%Y%m%d") # e.g. 20130701 Canada Day
        }
        ##* NMEA UTC (Time) = Jul 28 2011  04:17:53 
        ##* system upload time = jan 26 2010 13:02:57
        if (length(grep("^\\* .*time.*=.*$", lline))) {
            if (0 == length(grep("real-time sample interval", lline))) {
                d <- sub(".*=", "", lline)
                d <- sub("^ *", "", d)
                d <- sub(" *$", "", d)
                date <- decodeTime(d)
            }
        }
        if (0 < (r<-regexpr("filename", lline)))
            hexfilename <- sub("(.*)FileName =([ ])*", "", ignore.case=TRUE, lline)
        if (0 < (r<-regexpr("system upload time", lline))) {
            d <- sub("([^=]*)[ ]*=[ ]*", "", ignore.case=TRUE, lline)
            systemUploadTime <- decodeTime(d)
            oceDebug(debug, " systemUploadTime ", format(systemUploadTime), " inferred from substring '", d, "'\n", sep="")
        }
        ## Styles:
        ## * NMEA Latitude = 47 54.760 N
        ## ** Latitude:      47 53.27 N
        if (!found.header.latitude && (0 < (r<-regexpr("latitude*[0-8]*", lline, ignore.case=TRUE)))) {
            latitude <- parseLatLon(lline, debug=debug-1)
            found.header.latitude <- TRUE
        }
        if (!found.header.longitude && (0 < (r<-regexpr("longitude*[0-8]*", lline, ignore.case=TRUE)))) {
            longitude <- parseLatLon(lline, debug=debug-1)
            found.header.longitude <- TRUE
        }
        if (0 < (r<-regexpr("start_time =", lline))) {
            d <- sub("#[ ]*start_time[ ]*=[ ]*", "", lline)
            startTime <- decodeTime(d)
            oceDebug(debug, " startTime ", format(startTime), "' inferred from substring '", d, "'\n", sep="")
        }
        if (0 < (r<-regexpr("ship:", lline))) {
            ship <- sub("(.*)ship:([ \t])*", "", ignore.case=TRUE, line) # note: using full string
            ship <- sub("[ \t]*$", "", ship)
        }
        if (0 < (r<-regexpr("scientist:", lline)))
            scientist <- sub("(.*)scientist:([ ])*", "", ignore.case=TRUE, line) # full string
        if (0 < (r<-regexpr("chef", lline)))
            scientist <- sub("(.*):([ ])*", "", ignore.case=TRUE, line) # full string
        if (0 < (r<-regexpr("institute:", lline)))
            institute <- sub("(.*)institute:([ ])*", "", ignore.case=TRUE, line) # full string
        if (0 < (r<-regexpr("address:", lline)))
            address <- sub("(.*)address:([ ])*", "", ignore.case=TRUE, line) # full string
        if (0 < (r<-regexpr("cruise:", lline))) {
            cruise <- sub("(.*)cruise:([ ])*", "", ignore.case=TRUE, line) # full string
            cruise <- sub("[ ]*$", "", ignore.case=TRUE, cruise) # full string
        }
        if (is.null(station)) {
            if (0 < (r<-regexpr("station:", lline)))
                station <- sub("[ ]*$", "", sub("(.*)station:([ ])*", "", ignore.case=TRUE, line)) # full string
        }
        if (0 < (r<-regexpr("recovery:", lline)))
            recovery <- sub("(.*)recovery:([ ])*", "", lline)
        if (0 < (r<-regexpr("depth", lline))) { # "** Depth (m): 3447 "
            look <- sub("[a-z:()]*", "", lline, ignore.case=TRUE)
            look <- gsub("^[*a-zA-Z\\(\\) :]*", "", lline, ignore.case=TRUE)
            look <- gsub("[ ]*", "", look, ignore.case=TRUE)
            oceDebug(debug, " trying to get water depth from '", lline, "', reduced to '", look, "'\n", sep="")
            if (!length(grep('[a-zA-Z]', look))) {
                waterDepth<- as.numeric(look)
                oceDebug(debug, "got waterDepth: ", waterDepth, "\n")
            }
        }
        if (0 < (r<-regexpr("water depth:", lline))
            || 0 < (r<-regexpr(pattern="profondeur", text=lline))) {
            ## Examples from files in use by author:
            ##** Profondeur: 76
            ##** Water Depth:   40 m
            look <- sub("[ ]*$", "", sub(".*:[ ]*", "", lline))
            linesplit <- strsplit(look," ")[[1]]
            nitems <- length(linesplit)
            if (nitems == 1) {
                waterDepth <- as.numeric(linesplit[1])
            } else if (nitems == 2) {
                unit <- linesplit[2]
                if (unit == "m") {
                    waterDepth <- as.numeric(linesplit[1])
                } else if (unit == "km") {
                    waterDepth <- 1000 * as.numeric(linesplit[1])
                } else {
                    warning("ignoring unit on water depth '", look, "'")
                }
            } else {
                stop("cannot interpret water depth from '", lline, "'")
            }
        }
        if (0 < (r<-regexpr("^. sample rate =", lline))) {
            ## * sample rate = 1 scan every 5.0 seconds
            rtmp <- lline;
            rtmp <- sub("(.*) sample rate = ", "", rtmp)
            rtmp <- sub("scan every ", "", rtmp)
            rtmp <- strsplit(rtmp, " ")
            ##      if (length(rtmp[[1]]) != 3)
            ##        warning("cannot parse sample-rate string in `",line,"'")
            sampleInterval <- as.double(rtmp[[1]][2]) / as.double(rtmp[[1]][1])
            if (rtmp[[1]][3] == "seconds") {
                ;
            } else {
                if (rtmp[[1]][3] == "minutes") {
                    sampleInterval <- sampleInterval / 60;
                } else {
                    if (rtmp[[1]][3] == "hours") {
                        sampleInterval <- sampleInterval / 3600;
                    } else {
                        warning("cannot understand `",rtmp[[1]][2],"' as a unit of time for sampleInterval")
                    }
                }
            }
        }
    }
    oceDebug(debug, "Finished reading header\n")
    if (debug > 0) {
        if (is.nan(sampleInterval))
            warning("'* sample rate =' not found in header")
        if (is.nan(latitude))
            warning("'** Latitude:' not found in header")
        if (is.nan(longitude))
            warning("'** Longitude:' not found in header")
        if (is.null(date))
            warning("'** Date:' not found in header")
        if (is.null(recovery))
            warning("'** Recovery' not found in header")
    }
    ## Require p,S,T data at least
    if (!found.temperature)
        stop("cannot find 'temperature' in this file")
    if (!found.pressure && !found.depth)
        stop("no column named 'pressure', 'depth' or 'depSM'")

    res@metadata$header <- header
    res@metadata$type <- "SBE"
    res@metadata$hexfilename <- hexfilename # from instrument
    res@metadata$serialNumber <- serialNumber
    res@metadata$serialNumberConductivity <- serialNumberConductivity
    res@metadata$pressureType <- pressureType
    res@metadata$units <- list(conductivity=conductivityUnit, temperature=temperatureUnit)
    res@metadata$systemUploadTime <- systemUploadTime
    res@metadata$ship <- ship
    res@metadata$scientist <- scientist
    res@metadata$institute <- institute
    res@metadata$address <- address
    res@metadata$cruise <- cruise
    res@metadata$station <- station
    res@metadata$deploymentType <- "unknown"
    res@metadata$date <- date
    res@metadata$startTime <- startTime
    res@metadata$latitude <- latitude
    res@metadata$longitude <- longitude
    res@metadata$recovery <- recovery
    res@metadata$waterDepth <- waterDepth # if NA, will update later
    res@metadata$sampleInterval <- sampleInterval
    res@metadata$names <- col.names.inferred
    res@metadata$labels <- col.names.inferred
    res@metadata$filename <- filename
    ## Read the data as a table.
    ## FIXME: should we match to standardized names?
    ##col.names.forced <- c("scan","pressure","temperature","conductivity","descent","salinity","sigmaThetaUnused","depth","flag")

    ## Handle similar names by tacking numbers on the end, e.g. the first column that
    ## is automatically inferred to hold temperature is called "temperature", while the
    ## next one is called "temperature2", and a third would be called "temperature3".
    col.names.inferred <- tolower(col.names.inferred)
    for (uname in unique(col.names.inferred)) {
        w <- which(uname == col.names.inferred)
        lw <- length(w)
        ##message("uname:", uname, ", lw: ", lw)
        if (1 != lw) {
            col.names.inferred[w[-1]] <- paste(uname, seq.int(2, lw), sep="")
        }
    }
    if (is.null(columns)) {
        oceDebug(debug, "About to read these names:", col.names.inferred,"\n")
        data <- as.list(read.table(text=lines[seq.int(iline, length(lines))],
                                   header=FALSE, col.names=col.names.inferred))
        ndata <- length(data[[1]])
        if (0 < ndata) {
            haveData <- TRUE
            names <- names(data)
            ##labels <- names
            if (!found.scan) {
                data[['scan']] <- 1:ndata
            }
        } else {
            haveData <- FALSE
            warning("no data in CTD file \"", filename, "\"\n")
            data <- list(scan=NULL, salinity=NULL, temperature=NULL, pressure=NULL)
        }
    } else {
        dataAll <- read.table(text=lines[seq.int(iline, length(lines))],
                              header=FALSE, col.names=col.names.inferred)
        if ("scan" %in% names(columns)) {
            data <- dataAll[, as.numeric(columns)]
            names(data) <- names(columns)
        } else {
            data <- cbind(seq.int(1, dim(dataAll)[1]), dataAll[, as.numeric(columns)])
            names(data) <- c("scan", names(columns))
        }
        data <- as.list(data)
        ndata <- length(data[[1]])
        haveData <- ndata > 0
    }
    if (missing(processingLog))
        processingLog <- paste(deparse(match.call()), sep="", collapse="")
    ##hitem <- processingLogItem(processingLog)
    res@data <- data
    ## Add standard things, if missing
    if (haveData) {
        if (!found.salinity) {
            if (found.conductivity.ratio) {
                warning("cannot find 'salinity' in this file; calculating from T, conductivity ratio, and p")
                C <- data$conductivityratio
                cmax <- max(C, na.rm=TRUE)
                if (cmax > 5) {
                    warning("max(conductivity) > 5, so dividing by 42.914 before computing S. However, the original data are left in the object.")
                    C <- C / 42.914
                } else if (cmax > 1) {
                    warning("max(conductivity) between 1 and 5, so dividing by 4.2914 before computing S. However, the original data are left in the object.")
                    C <- C / 4.2914
                }
                S <- swSCTp(C, data$temperature, data$pressure)
            } else if (found.conductivity) {
                warning("cannot find 'salinity' in this file; calculating from T, conductivity, and p")
                C <- data$conductivity
                cmax <- max(C, na.rm=TRUE)
                if (cmax > 5) {
                    warning("max(conductivity) > 5, so dividing by 42.914 before computing S. However, the original data are left in the object.")
                    C <- C / 42.914
                } else if (cmax > 1) {
                    warning("max(conductivity) between 1 and 5, so dividing by 4.2914 before computing S. However, the original data are left in the object.")
                    C <- C / 4.2914
                }
                S <- swSCTp(C, data$temperature, data$pressure)
            } else {
                stop("cannot find salinity in this file, nor conductivity or conductivity ratio")
            }
            res <- ctdAddColumn(res, S, name="salinity", label="Salinity", unit="PSU", debug=debug-1)
        }
        if (found.depth && !found.pressure) { # BUG: this is a poor, nonrobust approximation of pressure
            g <- if (found.header.latitude) gravity(latitude) else 9.8
            rho0 <- 1000 + swSigmaTheta(median(res@data$salinity), median(res@data$temperature), 0)
            res <- ctdAddColumn(res, res@data$depth * g * rho0 / 1e4, name="pressure", label="Pressure", unit="dbar", debug=debug-1)
            warning("created a pressure column from the depth column\n")
        }
        res <- ctdAddColumn(res, swSigmaTheta(res@data$salinity, res@data$temperature, res@data$pressure),
                        name="sigmaTheta", label="Sigma Theta", unit="kg/m^3", debug=debug-1)
    }
    ## waterDepthWarning <- FALSE
    ## if (is.na(res@metadata$waterDepth)) {
    ##     res@metadata$waterDepth <- max(abs(res@data$pressure), na.rm=TRUE)
    ##     waterDepthWarning <- TRUE
    ## }
    res@processingLog <- processingLogAppend(res@processingLog, paste(deparse(match.call()), sep="", collapse=""))
    ## update to temperature IPTS-90, if have an older version
    if ("IPTS-68" == res@metadata$units$temperature) {
        res@data$temperature68 <- res@data$temperature
        res@data$temperature <- T90fromT68(res@data$temperature68)
        res@metadata$units$temperature <- "ITS-90"
        warning("converted temperature from IPTS-68 to ITS-90")
        res@processingLog <- processingLogAppend(res@processingLog, "converted temperature from IPTS-68 to ITS-90")
    }
    oceDebug(debug, "} # read.ctd.sbe()\n")
    ## if (waterDepthWarning)
    ##     res@processingLog <- processingLogAppend(res@processingLog, "inferred water depth from maximum pressure")
    res
}

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
        oceDebug(debug, "plotJustProfile(type=\"", if (is.vector(type)) "(a vector)" else type, "\", col[1:3]=\"", col[1:3], "\", ...) {\n", sep="", unindent=1)
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
                       sigmaTheta=rev(range(x@data$sigmaTheta, na.rm=TRUE)))
    examineIndices <- switch(ytype,
                       pressure = (min(ylim) <= x@data$pressure & x@data$pressure <= max(ylim)),
                       z = (min(ylim) <= swZ(x@data$pressure) & swZ(x@data$pressure) <= max(ylim)),
                       depth = (min(ylim) <= swDepth(x@data$pressure) & swDepth(x@data$pressure) <= max(ylim)),
                       sigmaTheta  = (min(ylim) <= x@data$sigmaTheta & x@data$sigmaTheta <= max(ylim)))
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
            plot(xtype, y, xlab="", ylab=yname, type=type, xaxs=xaxs, yaxs=yaxs, ylim=ylim, col=col, lty=lty, ...)
            if (list(...)$axes) {
                axis(3)
                mtext(xlab, side = 3, line = axis.name.loc, cex=par("cex"))
                axis(2)
            }
            box()
        } else {
            plot(xtype, y, xlab="", ylab=yname, type=type, axes=FALSE, xaxs=xaxs, yaxs=yaxs, ylim=ylim, col=col, lty=lty, ...)
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
            plot(xtype, y, xlab="", ylab=yname, type=type, axes=FALSE, xaxs=xaxs, yaxs=yaxs, ylim=ylim, lty=lty, ...)
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
        plot(index, x@data$pressure, ylim=ylim, col=col, lty=lty, xlab = "index", ylab = yname, type=type, xaxs=xaxs, yaxs=yaxs)
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
        plot(st[look], y[look], xlim=densitylim, ylim=ylim,
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
        plot(time, y, xlim=timelim, ylim=ylim, type=type, xlab="", ylab=yname, axes=FALSE, lwd=lwd, col=col.time, xaxs=xaxs, yaxs=yaxs, lty=lty)
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
             xlim=densitylim, ylim=ylim, col=col.rho, lty=lty,
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
        plot(dpdt.sm$y, dpdt.sm$x, xlim=dpdtlim, ylim=ylim, type=type, xlab="", ylab=yname, axes=FALSE, lwd=lwd, col=col.dpdt,
             xaxs=xaxs, yaxs=yaxs, lty=lty, ...)
        axis(1, col=col.dpdt, col.axis=col.dpdt, col.lab=col.dpdt)
        ## lines(dpdt.sm$y, dpdt.sm$x, lwd=lwd, col=col.dpdt)
        if (getOption("oceUnitBracket") == '[') {
            mtext(expression(paste(dp/dt, if (know.time.unit) " [ dbar/s ]" else " [ dbar/(time-unit)]")),
                  side = 1, line = axis.name.loc, cex=par("cex"), col=col.dpdt)

        } else {
            mtext(expression(paste(dp/dt, if (know.time.unit) " ( dbar/s )" else " ( dbar/(time-unit))")),
                  side = 1, line = axis.name.loc, cex=par("cex"), col=col.dpdt)
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
                     xlim=Slim, ylim=ylim, lty=lty,
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
                    if (unit == "ratio") {
                        mtext(resizableLabel("C", "x"), side=3, line=axis.name.loc, cex=par("cex"))
                    } else if (unit == "mS/cm") {
                        mtext(resizableLabel("conductivity mS/cm", "x"), side=3, line=axis.name.loc, cex=par("cex"))
                    } else if (unit == "S/m") {
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
                     xlim=Clim, ylim=ylim, lty=lty,
                     type = "n", xlab = "", ylab = yname, axes = FALSE, xaxs=xaxs, yaxs=yaxs, ...)
                if (is.null(xlab)) {
                    ## Look up conductivity unit (issue 731)
                    unit <- x[["conductivityUnit"]]
                    if (is.null(unit)) {
                        mtext(resizableLabel("C", "x"), side=3, line=axis.name.loc, cex=par("cex"))
                    } else {
                        if (unit == "ratio") {
                            mtext(resizableLabel("C", "x"), side=3, line=axis.name.loc, cex=par("cex"))
                        } else if (unit == "mS/cm") {
                            mtext(resizableLabel("conductivity mS/cm", "x"), side=3, line=axis.name.loc, cex=par("cex"))
                        } else if (unit == "S/m") {
                            mtext(resizableLabel("conductivity S/m", "x"), side=3, line=axis.name.loc, cex=par("cex"))
                        } else {
                            stop("unknown conductivity unit ", unit, "; should be 'ratio', 'mS/cm' or 'S/m'")
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
                     xlim=if (!missing(Rrholim)) Rrholim, ylim=ylim,
                     type = "n", xlab = "", ylab = yname, axes = FALSE, xaxs=xaxs, yaxs=yaxs, ...)
            } else {
                plot(Rrho, y[look], lty=lty,
                     xlim=if (!missing(Rrholim)) Rrholim, ylim=rev(range(y[look])),
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
                     xlim=Tlim, ylim=ylim,
                     type = "n", xlab = "", ylab = "", axes = FALSE, xaxs=xaxs, yaxs=yaxs, ...)
                if (eos == "gsw")
                    mtext(resizableLabel("conservative temperature", "x"), side = 3, line = axis.name.loc, cex=par("cex"))
                else
                    mtext(resizableLabel("T", "x"), side = 3, line = axis.name.loc, cex=par("cex"))
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
                     xlim=Tlim, ylim=ylim,
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
    } else if (xtype == "density") {
        st <- swSigmaTheta(x@data$salinity, x@data$temperature, x@data$pressure) # FIXME: why not use existing column?
        look <- if (keepNA) 1:length(y) else !is.na(st) & !is.na(y)
        ## FIXME: if this works, extend to other x types
        look <- look & (min(ylim) <= y & y <= max(ylim))
        if (!add) {
            if (densitylimGiven) {
                plot(st[look], y[look], xlim=densitylim, ylim=ylim, type = "n", xlab = "", ylab = yname, axes = FALSE, xaxs=xaxs, yaxs=yaxs, lty=lty, ...)
            } else {
                plot(st[look], y[look], xlim=range(st[look], na.rm=TRUE), ylim=ylim, type = "n", xlab = "", ylab = yname, axes = FALSE, xaxs=xaxs, yaxs=yaxs, lty=lty, ...)
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
             xlim=densitylim, ylim=ylim,
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
            points(st, y, col = col.rho, pch=pch)
        } else {
            points(st, y, col = col.rho, pch=pch)
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
             xlim=N2lim, ylim=ylim,
             type = "n", xlab = "", ylab = "", axes = FALSE, lwd=lwd, xaxs=xaxs, yaxs=yaxs)
        axis(1, col = col.N2, col.axis = col.N2, col.lab = col.N2)

        if (type == 'l') {
            lines(N2, y, col = col.N2, lwd=lwd, lty=lty) 
        } else if (type == 'p') {
            points(N2, y, col = col.N2, pch=pch)
        } else {
            points(N2, y, col = col.N2, pch=pch)
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
                 xlim=N2lim, ylim=ylim,
                 type = "n", xlab = "", ylab = yname, axes = FALSE)
            if (getOption("oceUnitBracket") == '[') {
                mtext(expression(paste(N^2, " [ ", s^-2, " ]")), side = 3, line = axis.name.loc, col = col, cex=par("cex"), xaxs=xaxs, yaxs=yaxs)
            } else {
                mtext(expression(paste(N^2, " ( ", s^-2, " )")), side = 3, line = axis.name.loc, col = col, cex=par("cex"), xaxs=xaxs, yaxs=yaxs)
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
                 ylim=ylim,
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
             xlim=Tlim, ylim=ylim, col = col.temperature, lty=lty,
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
             xlim=Slim, ylim=ylim, col = col.salinity, lty=lty,
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
                 ylim=ylim, lty=lty,
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
            points(x@data[[w]], y, lwd=lwd, pch=pch, col=col, lty=lty)
        } else if (type == "b" || type == "o") {
            lines(x@data[[w]], y, lwd=lwd, col=col)
            points(x@data[[w]], y, lwd=lwd, pch=pch, col=col, lty=lty)
        } else {
            points(x@data[[w]], y, lwd=lwd, pch=pch, col=col, lty=lty)
        }
    }
    oceDebug(debug, "} # plotProfile()\n", unindent=1)
}

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


