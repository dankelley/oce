## vim:textwidth=128:expandtab:shiftwidth=4:softtabstop=4

setMethod(f="initialize",
          signature="ctd",
          definition=function(.Object,pressure,salinity,temperature,filename="") {
              if (!missing(pressure)) .Object@data$pressure <- pressure
              if (!missing(temperature)) .Object@data$temperature <-temperature 
              if (!missing(salinity)) .Object@data$salinity <- salinity
              .Object@metadata$filename <- filename
              .Object@processingLog$time <- as.POSIXct(Sys.time())
              .Object@processingLog$value <- "create 'ctd' object"
              return(.Object)
          })

setMethod(f="[[",
          signature="ctd",
          definition=function(x, i, j, drop) {
              if (i == "N2") {
                  swN2(x)
              } else if (i == "theta" || i == "potential temperature") {
                  swTheta(x)
              } else if (i == "Rrho") {
                  swRrho(x)
              } else if (i == "spice") {
                  swSpice(x)
              } else if (i == "absoluteSalinity" || i == "SA") {
                  Sp <- x@data$salinity
                  t <- x@data$temperature
                  p <- x@data$pressure
                  n <- length(Sp)
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
                      lat <- rep(0, n)
                      haveLatLon <- FALSE
                  }
                  if (!haveLatLon)
                      warning("TEOS-10 calculation assuming lat=0 lon=300, because location is unknown")
                  Sp[is.nan(Sp)] <- NA
                  p[is.nan(p)] <- NA
                  lat[is.nan(lat)] <- NA
                  lon[is.nan(lon)] <- NA
                  teos("gsw_sa_from_sp", Sp, p, lon, lat)
              } else if (i == "conservativeTemperature" || i == "CT") {
                  Sp <- x@data$salinity
                  t <- x@data$temperature
                  p <- x@data$pressure
                  teos("gsw_ct_from_t", Sp, t, p)
              } else {
                  ## I use 'as' because I could not figure out callNextMethod() etc
                  as(x, "oce")[[i, j, drop]]
              }
          })

as.ctd <- function(salinity, temperature, pressure,
                   SA, CT,
                   oxygen, nitrate, nitrite, phosphate, silicate,
                   scan, other,
                   missingValue,
                   quality,
                   filename="", type="", model="", serialNumber="",
                   ship="", scientist="", institute="", address="", cruise="", station="",
                   date="", startTime="", recovery="",
                   latitude=NA, longitude=NA,
                   waterDepth=NA, sampleInterval=NA, src="")
{
    if (!missing(salinity) && class(salinity) == "data.frame") {
        d <- salinity
        names <- names(d)
        if ("temperature" %in% names && "salinity" %in% names && "pressure" %in% names) {
            ## FIXME: extract SA and CT if they exist
            salinity <- d$salinity
            temperature <- d$temperature
            pressure <- d$pressure
            ## FIXME: extract nitrate etc
        } else stop("data frame must contain columns 'temperature', 'salinity', and 'pressure'")
    } else {
        if (missing(temperature) && missing(CT))
            stop("must give temperature or CT")
        if (missing(pressure))
            stop("must give pressure")
    }
    res <- new('ctd')
    salinity <- as.vector(salinity)
    temperature <- as.vector(temperature)
    pressure <- as.vector(pressure)
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
        salinity <- teos("gsw_sp_from_sa", SA, pressure, longitude, latitude)
        temperature <- teos("gsw_t_from_ct", SA, CT, pressure)
    }
    depths <- max(length(salinity), length(temperature), length(pressure))
    if (length(pressure) < depths)
        pressure <- rep(pressure[1], depths)
    if (length(salinity) < depths)
        salinity <- rep(salinity[1], depths)
    if (length(temperature) < depths)
        temperature <- rep(temperature[1], depths)
    if (missing(quality))
        quality <- rep(2, depths)
    if (missing(scan))
        scan <- 1:depths
    else if (length(scan) < depths)
        scan <- rep(scan[1], depths)
    salinity <- as.vector(salinity)
    temperature <- as.vector(temperature)
    scan <- as.vector(scan)
    nSalinity <- length(salinity)
    data <- list(salinity=salinity,
                 temperature=temperature,
                 pressure=pressure,
                 scan=scan,
                 oxygen=   if (!missing(oxygen)    && !is.null(oxygen))    oxygen    else rep(NA, nSalinity),
                 nitrate=  if (!missing(nitrate)   && !is.null(nitrate))   nitrate   else rep(NA, nSalinity),
                 nitrite=  if (!missing(nitrite)   && !is.null(nitrite))   nitrite   else rep(NA, nSalinity),
                 phosphate=if (!missing(phosphate) && !is.null(phosphate)) phosphate else rep(NA, nSalinity),
                 silicate= if (!missing(silicate)  && !is.null(silicate))  silicate  else rep(NA, nSalinity),
                 quality=quality,
                 sigmaTheta=swSigmaTheta(salinity, temperature, pressure))
    if (!missing(other)) {
        names <- names(other)
        n <- length(names)
        for (i in 1:n) {
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
    metadata <- list(header=NULL,
                     type=type, model=model, filename=filename, serialNumber=serialNumber,
                     filename.orig=filename,
                     systemUploadTime=NULL,
                     ship=ship,scientist=scientist,institute=institute,address=address,cruise=cruise,station=station,
                     date=date, startTime=startTime, recovery=recovery,
                     latitude=latitude, longitude=longitude,
                     waterDepth=if (is.na(waterDepth)) max(abs(data$pressure), na.rm=TRUE) else waterDepth,
                     sampleInterval=sampleInterval,
                     names=c("salinity", "temperature", "pressure", "sigmaTheta"), # FIXME: incorrect names and labels
                     labels=c("Salinity", "Temperature", "Pressure", expression(sigma[theta])),
                     src=src)
    res@metadata <- metadata
    res@data <- data
    res@processingLog <- processingLog(res@processingLog, paste(deparse(match.call()), sep="", collapse=""))
    res
}

ctdAddColumn <- function (x, column, name, label, unit, debug = getOption("oceDebug"))
{
    ## FIXME: not using the unit
    oceDebug(debug, "\bctdAddColumn(x, column, name=\"", name, "\", label=\"", label, "\", debug) {\n", sep="")
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
    r <- range(column)
    res@data[[name]] <- column
    if (!replace) {
        res@metadata$names <- c(res@metadata$names, name)
        res@metadata$labels <- c(res@metadata$labels, label)
    }
    res@processingLog <- processingLog(res@processingLog, paste(deparse(match.call()), sep="", collapse=""))
    oceDebug(debug, "\b} # ctdAddColumn()\n", sep="")
    res
}

ctdDecimate <- function(x, p, method=c("approx", "boxcar","lm","reiniger-ross"), e=1.5, debug=getOption("oceDebug"))
    ## SHOULD ADD: spline; supsmu; ...
{
    oceDebug(debug, "\bctdDecimate(x, p, method=\"", method, "\", ...) {\n", sep="")
    if (!inherits(x, "ctd"))
        stop("method is only for ctd objects")
    res <- x
    n <- length(x@data$pressure)
    if (n < 2) {
        warning("too few data to trim.decimate()")
        return(res)
    }
    ## Figure out pressure targets, pt
    if (missing(p)) { # autoscale
        dp.exact <- median(abs(diff(x@data$pressure)))
        dp <- pretty(3 * dp.exact)[2] # try for 3 data at least
        pt <- seq(0, dp * floor(max(x@data$pressure, na.rm=TRUE) / dp), dp)
    } else {
        if (length(p) == 1) {
            pt <- seq(0, p * floor(max(x@data$pressure, na.rm=TRUE) / p), p)
        } else {
            pt <- p
        }
    }
    npt <- length(pt)
    data.names <- names(x@data)         # Step through each variable.
    data.new <- vector("list", length(data.names)) # as.data.frame(array(NA, dim=c(npt, length(data.names))))
    names(data.new) <- data.names
    method <- match.arg(method)
    if (method == "approx") {
        too.deep <- pt > max(x@data[["pressure"]], na.rm=TRUE)
        for (datum.name in data.names) {
            oceDebug(debug, "decimating \"", datum.name, "\"\n", sep="")
            if (datum.name != "pressure") {
                good <- sum(!is.na(x@data[[datum.name]]))
                if (good > 2) {
                    data.new[[datum.name]] <- approx(x@data[["pressure"]], x@data[[datum.name]], pt, rule=2)$y
                    data.new[[datum.name]][too.deep] <- NA
                } else {
                    oceDebug(debug, " note: fewer than 2 good data in the above\n")
                }
            }
        }
        data.new[["pressure"]] <- pt
    } else if ("reiniger-ross" == method) {
        oceDebug(debug, "Reiniger-Ross method\n")
        xvar <- x@data[["pressure"]]
        for (datum.name in data.names) {
            if (datum.name != "pressure") {
                yvar <- x@data[[datum.name]]
                pred <- oceApprox(xvar, yvar, pt)
                data.new[[datum.name]] <- pred
            }
        }
        data.new[["pressure"]] <- pt
    } else if ("boxcar" == method) {
        pcut <- cut(x@data$pressure, c(pt, tail(pt, 1)+diff(pt[1:2])))
        for (name in data.names) {
            ## FIXME: we should probably not e averaging scan, flag, etc
            oceDebug(debug, "decimating", name)
            data.new[[name]] <- as.vector(sapply(split(x@data[[name]], pcut), mean))
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
            ##cat("i=",i,"pt[i]=",pt[i],"; datum.name=", datum.name, "\n")
            if (sum(focus, na.rm=TRUE) > 0) {
                if ("boxcar" == method) {
                    for (datum.name in data.names) {
                        if (datum.name != "pressure") {
                            ##cat("i=",i,"datum=",datum.name,"avg=",mean(x@data[[datum.name]][focus]),"\n")
                            data.new[[datum.name]][i] <- mean(x@data[[datum.name]][focus],na.rm=TRUE)
                        }
                    }
                } else if ("lm" == method) { # FIXME: this is far too slow
                    xvar <- x@data[["pressure"]][focus]
                    for (datum.name in data.names) {
                        if (datum.name != "pressure") {
                            yvar <- x@data[[datum.name]][focus]
                            m <- lm(yvar ~ xvar)
                            data.new[[datum.name]][i] <- predict(m, newdata=list(xvar=pt[i]))
                        }
                    }
                } else {
                    stop("impossible to get here -- developer error")
                }
            } else {                    # No data in the focus region
                for (datum.name in data.names) {
                    ##cat("i=",i,"NO DATA IN focus =\n")
                    if (datum.name != "pressure") {
                        data.new[[datum.name]][i] <- NA
                    }
                }
            }
        }
        data.new[["pressure"]] <- pt
    }
    res@data <- data.new
    res@processingLog <- processingLog(res@processingLog, paste(deparse(match.call()), sep="", collapse=""))
    oceDebug(debug, "\b\b} # ctdDecimate()\n")
    res
}


ctdFindProfiles<- function(x, cutoff=0.5, minLength=10, minHeight=0.1*diff(range(x[["pressure"]])),
                           direction=c("descending", "ascending"),
                           arr.ind=FALSE, 
                           debug=getOption("oceDebug"), ...)
{
    oceDebug(debug, "\b\bctdFindProfiles(x, cutoff=", cutoff, 
             ", minLength=", minLength,
             ", minHeight=", minHeight,
             ", direction=\"", direction, "\"",
             ", arr.ind=", arr.ind, ", debug=", debug, ") {\n", sep="")
    if (!inherits(x, "ctd"))
        stop("method is only for ctd objects")
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
        oceDebug(debug, "\b\b} # ctdFindProfiles()\n", sep="")
        return(indices)
    } else {
        ncasts <- length(indices$start)
        casts <- vector("list", ncasts)
        for (i in 1:ncasts) {
            oceDebug(debug, "profile #", i, "of", ncasts, "\n")
            ii <- seq.int(indices$start[i], indices$end[i])
            cast <- ctdTrim(x, "index", parameters=ii)
            cast@processingLog <- processingLog(cast@processingLog,
                                                paste(paste(deparse(match.call()), sep="", collapse=""),
                                                " # profile ", i, " of ", ncasts))
            casts[[i]] <- cast
        }
        oceDebug(debug, "\b\b} # ctdFindProfiles()\n", sep="")
        return(casts)
    }
}


ctdTrim <- function(x, method=c("downcast", "index", "range"),
                    inferWaterDepth=TRUE, removeDepthInversions=FALSE, 
                    parameters, debug=getOption("oceDebug"))
{
    oceDebug(debug, "\b\bctdTrim() {\n")
    if (!inherits(x, "ctd"))
        stop("method is only for ctd objects")
    res <- x
    n <- length(x@data$pressure)
    if (n < 2) {
        warning("too few data to ctdTrim()")
        return(res)
    } else {
        which.method <- pmatch(method, c("index", "downcast"), nomatch=0)
        method <- match.arg(method)
        oceDebug(debug, paste("ctdTrim() using method \"", method,"\"\n", sep=""))
        keep <- rep(TRUE, n)
        if (method == "index") {
            if (is.logical(parameters)) {
                if (length(parameters) != n)
                    stop("for method=\"index\", need length(parameters) to match number of pressure values")
                keep <- parameters
            } else {
                if (min(parameters) < 1)
                    stop("Cannot select indices < 1")
                if (max(parameters) > n)
                    stop(paste("Cannot select past end of array, i.e. past ", n))
                keep <- rep(FALSE, n)
                keep[parameters] <- TRUE
            }
        } else if (method == "downcast") {
            ## 1. despike to remove (rare) instrumental problems
            x@data$pressure <- smooth(x@data$pressure, kind="3R")
            ascending <- 0 > mean(diff(x@data$pressure))
            oceDebug(debug, "ascending=", ascending, "\n")
            if (ascending) {
                for (name in names(x@data)) {
                    x@data[[name]] <- rev(x@data[[name]])
                }
            }
            pmin <- 0
            if (!missing(parameters)) {
                if ("pmin" %in% names(parameters)) pmin <- parameters$pmin else stop("parameter not understood for this method")
            }
            oceDebug(debug, 'pmin=', pmin, '\n')
            keep <- (x@data$pressure > pmin) # 2. in water (or below start depth)
            delta.p <- diff(x@data$pressure)  # descending
            delta.p <- c(delta.p[1], delta.p) # to get right length
            keep <- keep & (delta.p > 0)
            ## 3. trim the upcast and anything thereafter (ignore beginning and end)
            trim.top <- as.integer(0.1*n)
            trim.bottom <- as.integer(0.9*n)
            max.spot <- which.max(smooth(x@data$pressure[trim.top:trim.bottom],kind="3R"))
            max.location <- trim.top + max.spot
            keep[max.location:n] <- FALSE
            oceDebug(debug, "pressure maximum of", x@data$pressure[max.spot], "dbar, at index=", max.spot, "\n")
            if (FALSE) {
                ## deleted method: slowly-falling data
                delta.p.sorted <- sort(delta.p)
                if (!is.null(parameters)) {
                    dp.cutoff <- t.test(delta.p[keep], conf.level=0.5)$conf.int[1]
                    print(t.test(delta.p[keep], conf.level=0.05))#$conf.int[1]
                } else {
                    dp.cutoff <- delta.p.sorted[0.1*n]
                }
                keep[delta.p < dp.cutoff] <- FALSE
            }
            ## 4. remove equilibration phase
            if (FALSE) {                # old method, prior to Feb 2008
                pp <- x@data$pressure[keep]
                ss <- x@data$scan[keep]
                equilibration <- (predict(m <- lm(pp ~ ss), newdata=list(ss=x@data$scan)) < 0)
                keep[equilibration] <- FALSE
            }
            if (TRUE) {                 # new method, after Feb 2008
                bilinear1 <- function(s, s0, dpds) {
                    ifelse(s < s0, 0, dpds*(s-s0))
                }
                pp <- x@data$pressure[keep]
                ss <- x@data$scan[keep]
                p0 <- 0
                s0 <- ss[0.25*length(ss)]
                p0 <- pp[1]
                p1 <- max(pp) #pp[0.9*length(pp)]
                if (length(ss) > 2)
                    dpds0 <-  diff(range(pp, na.rm=TRUE)) / diff(range(ss, na.rm=TRUE))
                else
                   dpds0 <- 0 
                t <- try(m <- nls(pp ~ bilinear1(ss, s0, dpds),
                                  start=list(s0=s0, dpds=dpds0)),
                         silent=TRUE)
                if (class(t) != "try-error") {
                    if (m$convInfo$isConv) {
                        s0 <- floor(coef(m)[[1]])
                        oceDebug(debug, "trimming scan numbers below", s0, "\n")
                        keep <- keep & (x@data$scan > (coef(m)[[1]]))
                    }
                    ##} else {
                    ##warning("unable to complete step 5 of the trim operation (removal of initial equilibrium phase)")
                }
            }
            if (ascending) {
                for (name in names(x@data)) {
                    x@data[[name]] <- rev(x@data[[name]])
                }
            }
        } else if (method == "range") {
            if (!("item" %in% names(parameters)))
                stop("'parameters' must be a list containing 'item'")
            oceDebug(debug, "method='range'; parameters= ", parameters, "\n")
            item <- parameters$item
            if (!(item %in% names(x@data)))
                stop("x@data has no item named '", item, "'")
            keep <- rep(TRUE, n)
            if ("from" %in% names(parameters))
                keep <- keep & (x@data[[item]] >= parameters$from)
            if ("to" %in% names(parameters))
                keep <- keep & (x@data[[item]] <= parameters$to)
        } else {
            stop("'method' not recognized; must be 'index', 'downcast', or 'range'")
        }
    }
    if (is.data.frame(res@data)) {
        res@data <- res@data[keep,]
    } else {
        for (i in seq_along(res@data)) {
            res@data[[i]] <- res@data[[i]][keep]
        }
    }
    if (inferWaterDepth && !is.finite(res@metadata$waterDepth)) {
        res@metadata$waterDepth <- max(res@data$pressure, na.rm=TRUE)
        oceDebug(debug, "inferred water depth of", res@metadata$waterDepth, "from pressure\n")
    }
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
    res@metadata$waterDepth <- max(abs(res@data$pressure)) # the bad data sometimes have high p
    res@processingLog <- processingLog(res@processingLog, paste(deparse(match.call()), sep="", collapse=""))
    oceDebug(debug, "\b\b} # ctdTrim()\n")
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
    nquan <- length(x@data)
    rval <- x
    h <- x@metadata$header
    for (i in seq_along(x@data)) {
        r <- range(x@data[[i]])
        prefix <- sprintf("^#[\t ]*span[\t ]*%d[\t ]*=", i)
        span <- sprintf("# span %d = %g, %g", i, r[1], r[2])
        h <- replaceHeaderElement(h, prefix, span)
    }
    rval@metadata$header <- h
    rval
}

write.ctd <- function(object, file=stop("'file' must be specified"))
{
    if (!inherits(object, "ctd"))
        stop("method is only for ctd objects")
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
          definition=function(x, which = c(1, 2, 3, 5),
                              eos=getOption("eos", default='unesco'),
                              ref.lat = NaN, ref.lon = NaN,
                              grid = TRUE, col.grid="lightgray", lty.grid="dotted",
                              coastline="best",
                              Slim, Tlim, plim, densitylim, N2lim,
                              dpdtlim, timelim,
                              lonlim, latlim, span,
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
              eos <- match.arg(eos, c("unesco", "teos"))
              oceDebug(debug, "\b\bplot.ctd(..., which=c(", paste(which, collapse=",", sep=""),
                       "), eos=\"", eos, "\", inset=", inset, ", ...) {\n", sep="")
              dots <- list(...)
              opar <- par(no.readonly = TRUE)
              if (add && length(which) > 1) {
                  warning("ignoring add=TRUE because length(which) > 1")
                  add <- FALSE
              }
              lw <- length(which)
              if (lw > 1) on.exit(par(opar))
              if (length(type) < lw)
                  type <- rep(type, lw) # FIXME: recycle more sensibly
              if (length(pch) < lw)
                  pch <- rep(pch, lw) # FIXME: recycle more sensibly
              if (length(cex) < lw)
                  cex <- rep(cex, lw) # FIXME: recycle more sensibly
              dec_deg <- function(x, code = "lat")
              {
                  if (code == "lat") {
                      if (x < 0) {
                          x <- -x
                          sprintf("%.0f %.2fS", floor(x), 60 * (x - floor(x)))
                      }
                      else {
                          sprintf("%.0f %.2fN", floor(x), 60 * (x - floor(x)))
                      }
                  } else {
                      if (x < 0) {
                          x <- -x
                          sprintf("% %.2fW", floor(x), 60 * (x - floor(x)))
                      }
                      else {
                          sprintf("% %.2fE", floor(x), 60 * (x - floor(x)))
                      }
                  }
              }
              adorn.length <- length(adorn)
              if (adorn.length == 1) {
                  adorn <- rep(adorn, lw)
                  adorn.length <- lw
              }
              if (!inset)
                  par(mar=mar)
              par(mgp=mgp)

              if (lw > 1) {
                  oldpar <- par(no.readonly = TRUE)
                  if (lw > 2)
                      lay <- layout(matrix(1:4, nrow=2, byrow=TRUE))
                  else
                      lay <- layout(matrix(1:2, nrow=2, byrow=TRUE))
                  ##layout.show(lay)
                  ##stop()
              }
              ## Ignore any bottom region consisting of NA for temperature and salinity, e.g.
              ## as created by makeSection().
              if (0 == length(x@data$salinity)) {
                  warning("no data to plot in this object")
                  return(invisible())
              }
              last.good <- which(rev(is.na(x@data$salinity))==FALSE)[1]
              if (length(last.good) > 0) {
                  last.good <- length(x@data$temperature) - last.good + 1
                  for (nc in seq_along(x@data)) {
                      if (!is.null(x@data[[nc]])) {
                          x@data[[nc]] <- x@data[[nc]][1:last.good]
                      }
                  }
              }

              oceDebug(debug, "which:", which, "(before matching character strings)\n")
              which <- ocePmatch(which,
                                 list("temperature+salinity"=1,
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
                                      RrhoSF=16))
              oceDebug(debug, "which:", which, "(after matching character strings)\n")

              for (w in 1:length(which)) {
                  if (is.na(which[w])) {
                      warning("plot.ctd(): unknown plot type requested\n", call.=FALSE)
                      next
                  }
                  oceDebug(debug, "this which:", which[w], "\n")
                  if (which[w] == 1) {
                      plotProfile(x, xtype="salinity+temperature", Slim=Slim, Tlim=Tlim, ylim=plim,
                                  eos=eos,
                                  useSmoothScatter=useSmoothScatter,
                                  grid=grid, col.grid=col.grid, lty.grid=lty.grid,
                                  cex=cex[w], pch=pch[w], type=type[w], keepNA=keepNA, inset=inset, add=add,
                                  debug=debug-1,
                                  ...)
                  } else if (which[w] == 2) {
                      plotProfile(x, xtype="density+N2",
                                  ylim=plim,
                                  eos=eos,
                                  useSmoothScatter=useSmoothScatter,
                                  df=df,
                                  grid=grid, col.grid=col.grid, lty.grid=lty.grid,
                                  cex=cex[w], pch=pch[w], type=type[w], keepNA=keepNA, inset=inset, add=add,
                                  debug=debug-1,
                                  ...)
                  } else if (which[w] == 6) {
                      plotProfile(x, xtype="density+dpdt",
                                  ylim=plim, densitylim=densitylim, dpdtlim=dpdtlim,
                                  eos=eos,
                                  useSmoothScatter=useSmoothScatter,
                                  grid=grid, col.grid=col.grid, lty.grid=lty.grid,
                                  cex=cex[w], pch=pch[w], type=type[w], keepNA=keepNA, inset=inset, add=add,
                                  debug=debug-1,
                                  ...)
                  } else if (which[w] == 7) {
                      plotProfile(x, xtype="density+time",
                                  ylim=plim, densitylim=densitylim, timelim=timelim,
                                  useSmoothScatter=useSmoothScatter,
                                  grid=grid, col.grid=col.grid, lty.grid=lty.grid,
                                  cex=cex[w], pch=pch[w], type=type[w], keepNA=keepNA, inset=inset, add=add,
                                  debug=debug-1,
                                  ...)
                  } else if (which[w] == 8) {
                      plotProfile(x, xtype="index",
                                  ylim=plim,
                                  eos=eos,
                                  useSmoothScatter=useSmoothScatter,
                                  grid=grid, col.grid=col.grid, lty.grid=lty.grid,
                                  cex=cex[w], pch=pch[w], type=type[w], keepNA=keepNA, inset=inset, add=add,
                                  debug=debug-1,
                                  ...)
                  } else if (which[w] == 9) {
                      plotProfile(x, xtype="salinity",
                                  ylim=plim,
                                  Slim=Slim,
                                  eos=eos,
                                  useSmoothScatter=useSmoothScatter,
                                  grid=grid, col.grid=col.grid, lty.grid=lty.grid,
                                  cex=cex[w], pch=pch[w], type=type[w], keepNA=keepNA, inset=inset, add=add,
                                  debug=debug-1,
                                  ...)
                  } else if (which[w] == 10) {
                      plotProfile(x, xtype="temperature",
                                  ylim=plim,
                                  Tlim=Tlim,
                                  eos=eos,
                                  useSmoothScatter=useSmoothScatter,
                                  grid=grid, col.grid=col.grid, lty.grid=lty.grid,
                                  cex=cex[w], pch=pch[w], type=type[w], keepNA=keepNA, inset=inset, add=add,
                                  debug=debug-1,
                                  ...)
                   } else if (which[w] == 11) {
                      plotProfile(x, xtype="density",
                                  ylim=plim,
                                  densitylim=densitylim,
                                  grid=grid,
                                  eos=eos,
                                  useSmoothScatter=useSmoothScatter,
                                  col.grid=col.grid, lty.grid=lty.grid,
                                  cex=cex[w], pch=pch[w], type=type[w], keepNA=keepNA, inset=inset, add=add,
                                  debug=debug-1,
                                  ...)
                  } else if (which[w] == 12) {
                      plotProfile(x, xtype="N2",
                                  ylim=plim,
                                  N2lim=N2lim,
                                  grid=grid,
                                  eos=eos,
                                  df=df,
                                  useSmoothScatter=useSmoothScatter,
                                  col.grid=col.grid, lty.grid=lty.grid,
                                  cex=cex[w], pch=pch[w], type=type[w], keepNA=keepNA, inset=inset, add=add,
                                  debug=debug-1,
                                  ...)
                  } else if (which[w] == 13) {
                      plotProfile(x, xtype="spice",
                                  ylim=plim,
                                  eos=eos,
                                  useSmoothScatter=useSmoothScatter,
                                  grid=grid, col.grid=col.grid, lty.grid=lty.grid,
                                  cex=cex[w], pch=pch[w], type=type[w], keepNA=keepNA, inset=inset, add=add,
                                  debug=debug-1,
                                  ...)
                  } else if (which[w] == 14) {
                      plotProfile(x, xtype="tritium",
                                  ylim=plim,
                                  eos=eos,
                                  useSmoothScatter=useSmoothScatter,
                                  grid=grid, col.grid=col.grid, lty.grid=lty.grid,
                                  cex=cex[w], pch=pch[w], type=type[w], keepNA=keepNA, inset=inset, add=add,
                                  debug=debug-1,
                                  ...)
                  } else if (which[w] == 15) {
                      plotProfile(x, xtype="Rrho",
                                  ylim=plim,
                                  eos=eos,
                                  useSmoothScatter=useSmoothScatter,
                                  grid=grid, col.grid=col.grid, lty.grid=lty.grid,
                                  cex=cex[w], pch=pch[w], type=type[w], keepNA=keepNA, inset=inset, add=add,
                                  debug=debug-1,
                                  ...)
                  } else if (which[w] == 16) {
                      plotProfile(x, xtype="RrhoSF",
                                  ylim=plim,
                                  eos=eos,
                                  useSmoothScatter=useSmoothScatter,
                                  grid=grid, col.grid=col.grid, lty.grid=lty.grid,
                                  cex=cex[w], pch=pch[w], type=type[w], keepNA=keepNA, inset=inset, add=add,
                                  debug=debug-1,
                                  ...)
                  } else if (which[w] == 3) {
                      ##par(mar=c(3.5,3,2,2))
                      lwd.rho <- if ("lwd.rho" %in% names(dots)) dots$lwd.rho else par('lwd')
                      lty.rho <- if ("lty.rho" %in% names(dots)) dots$lty.rho else par('lty')
                      plotTS(x, Slim=Slim, Tlim=Tlim,
                             grid=grid, col.grid=col.grid, lty.grid=lty.grid,
                             eos=eos,
                             lwd.rho=lwd.rho, lty.rho=lty.rho,
                             useSmoothScatter=useSmoothScatter, pch=pch, cex=cex, 
                             inset=inset,
                             add=add,
                             debug=debug-1, ...) # FIXME use inset here
                  } else if (which[w] == 4) {
                      text.item <- function(item, label, cex=0.8) {
                          if (!is.null(item) && !is.na(item)) {
                              text(xloc, yloc, paste(label, item), adj = c(0, 0), cex=cex)
                          }
                      }
                      par(mar=c(0,0,0,0))
                      plot.new()
                      plot.window(c(0,10), c(0,10))
                      xloc <- 0
                      yloc <- 8
                      d.yloc <- 0.8
                      cex <- 3/4
                      text(xloc, yloc, paste("CTD Station"), adj = c(0, 0), cex=cex)
                      yloc <- yloc - d.yloc
                      xm <- x@metadata
                      if (!is.null(xm$filename) && nchar(xm$filename) > 0) {
                          text.item(xm$filename,    " File:     ", cex=cex)
                          yloc <- yloc - d.yloc
                      }
                      if (!is.null(xm$scientist))	{
                          text.item(xm$scientist,   " Scientist:", cex=cex)
                          yloc <- yloc - d.yloc
                      }
                      if (!is.null(xm$institute))	{
                          text.item(xm$institute,   " Institute:", cex=cex)
                          yloc <- yloc - d.yloc
                      }
                      if (!is.null(xm$date)) {
                          text.item(xm$date,        " Date:     ", cex=cex)
                          yloc <- yloc - d.yloc
                      }
                      if (!is.null(xm$ship)) {
                          text.item(xm$ship,        " Ship:     ", cex=cex)
                          yloc <- yloc - d.yloc
                      }
                      if (!is.null(xm$cruise)) {
                          text.item(xm$cruise,      " Cruise:   ", cex=cex)
                          yloc <- yloc - d.yloc
                      }
                      if (!is.null(xm$station)) {
                          text.item(xm$station,     " Station:  ", cex=cex)
                          yloc <- yloc - d.yloc
                      }
                      if (!is.null(xm$waterDepth)) {
                          text.item(xm$waterDepth, " Depth:    ", cex=cex)
                          yloc <- yloc - d.yloc
                      }
                      if (!is.na(xm$longitude) && !is.na(xm$latitude)) {
                          text.item(latlonFormat(xm$latitude, xm$longitude),   " Location: ", cex=cex)
                          yloc <- yloc - d.yloc
                      }
                      if (!is.na(ref.lat) && !is.na(ref.lon)) {
                          dist <- geodDist(xm$latitude, xm$longitude, ref.lat, ref.lon)
                          kms <- sprintf("%.2f km", dist/1000)
                          rlat <- text(xloc, yloc, paste(" Distance to (", dec_deg(ref.lon),
                                                         ",", dec_deg(ref.lat), ") = ", kms), adj = c(0, 0), cex=cex)
                          yloc <- yloc - d.yloc
                      }
                  } else if (which[w] == 5) {
                      if (is.finite(x[["latitude"]]) && is.finite(x[["longitude"]])) {
                          oceDebug(debug, "draw(ctd, ...) of type MAP\n")
                          ## FIXME: use waterdepth to guess a reasonable span, if not supplied
                          if ("waterDepth" %in% names(x@metadata) && !is.na(x@metadata$waterDepth))
                              waterDepth <- x[["waterDepth"]]
                          else 
                              waterDepth <- max(x[["pressure"]], na.rm=TRUE)
                          if (missing(span)) {
                              if (waterDepth < 50)
                                  span <- 100
                              else if (waterDepth < 200)
                                  span <- 500
                              else if (waterDepth < 2000)
                                  span <- 1000
                              else
                                  span <- 10000
                          }
                          oceDebug(debug, "span=", span, "km\n")
                          if (is.character(coastline)) {
                              if (coastline == "best") {
                                  coastline <- coastlineBest(x[["longitude"]]+c(-1,1)*span/111,
                                                             x[["latitude"]]+c(-1,1)*span/111,
                                                             debug=debug-1)
                              } else if (coastline == "coastlineWorld") {
                                  data(coastlineWorld, envir=environment())
                                  coastline <- coastlineWorld
                              } else if (coastline == "coastlineMaritimes") {
                                  data(coastlineMaritimes, envir=environment())
                                  coastline <- coastlineMaritimes
                              } else if (coastline == "coastlineHalifax") {
                                  data(coastlineHalifax, envir=environment())
                                  coastline <- coastlineHalifax
                              } else if (coastline == "coastlineSLE") {
                                  data(coastlineSLE, envir=environment())
                                  coastline <- coastlineSLE
                              } else if (coastline == "none") {
                              } else {
                                  stop("there is no built-in coastline file of name \"", coastline, "\"")
                              }
                          }
                          if (missing(lonlim)) {
                              lonlim.c <- x@metadata$longitude + c(-1, 1) * min(abs(range(coastline[["longitude"]], na.rm=TRUE) - x@metadata$longitude))
                              clon <- mean(lonlim.c)
                              if (missing(latlim)) {
                                  oceDebug(debug, "CASE 1: both latlim and lonlim missing\n")
                                  latlim.c <- x@metadata$latitude + c(-1, 1) * min(abs(range(coastline[["latitude"]],na.rm=TRUE) - x@metadata$latitude))
                                  plot(coastline, clatitude=mean(latlim.c), clongitude=clon,
                                       span=span, mgp=mgp, mar=mar, inset=inset, cex.axis=cex.axis, debug=debug-1)
                              } else {
                                  oceDebug(debug, "CASE 2: latlim given, lonlim missing\n")
                                  clat <- mean(latlim)
                                  plot(coastline, clatitude=clat, clongitude=clon,
                                       span=span, mgp=mgp, mar=mar, inset=inset, cex.axis=cex.axis, debug=debug-1)
                              }
                              if (is.numeric(which[w]) && round(which[w],1) == 5.1) # HIDDEN FEATURE
                                  mtext(gsub(".*/", "", x@metadata$filename), side=3, line=0.1, cex=0.7*cex)
                          } else {
                              oceDebug(debug, "lonlim was provided\n")
                              clon <- mean(lonlim)
                              if (missing(latlim)) {
                                  oceDebug(debug, "CASE 3: lonlim given, latlim missing\n")
                                  latlim.c <- x@metadata$latitude + c(-1, 1) * min(abs(range(coastline[["latitude"]],na.rm=TRUE) - x@metadata$latitude))
                                  clat <- mean(latlim.c)
                                  plot(coastline, clatitude=clat, clongitude=clon,
                                       span=span, mgp=mgp, mar=mar, inset=inset, cex.axis=cex.axis, debug=debug-1)
                              } else {
                                  oceDebug(debug, "CASE 4: both latlim and lonlim given\n")
                                  clat <- mean(latlim)
                                  plot(coastline, clatitude=clat, clongitude=clon,
                                       span=span, mgp=mgp, mar=mar, inset=inset, cex.axis=cex.axis, debug=debug-1)
                              }
                          }
                          oceDebug(debug, "about to add a station point[s] to map; mai=", par('mai'), '\n')
                          points(x@metadata$longitude, x@metadata$latitude, cex=latlon.cex, col=latlon.col, pch=latlon.pch)
                          if (!is.null(x@metadata$station) && !is.na(x@metadata$station))
                              mtext(paste("Station", x@metadata$station), side=3, adj=0, cex=0.8*par("cex"), line=0.5)
                          if (!is.null(x@metadata$startTime))
                              mtext(format(x@metadata$startTime), side=3, adj=1, cex=0.8*par("cex"), line=0.5)
                          ##if (!is.null(x@metadata$scientist))
                          ##    mtext(paste(" ", x@metadata$scientist, sep=""), side=3, line=-1, adj=0, cex=0.8*par("cex"))
                      }
                  } else {
                      stop("unknown value of which, ", which[w])
                  }
                  if (w <= adorn.length && nchar(adorn[w]) > 0) {
                      t <- try(eval(adorn[w]), silent=TRUE)
                      if (class(t) == "try-error")
                          warning("cannot evaluate adorn[", w, "]\n")
                  }
              }
              oceDebug(debug, "\b\b} # plot.ctd()\n")
              invisible()
          })

plotScan <- function(x,
                     name = "scan",
                     adorn=NULL,
                     mgp=getOption("oceMgp"),
                     type='l',
                     ...)
{
    if (!inherits(x, "ctd"))
        stop("method is only for ctd objects")
    if (3 == length(mgp)) par(mgp=mgp)
    par(mar=c(mgp[1], mgp[1]+1, 1, mgp[1]+2))

    adorn.length <- length(adorn)
    if (adorn.length == 1) {
        adorn <- rep(adorn, 2)
        adorn.length <- 2
    }
##    layout(matrix(1:2, nrow=2))
    par(mfrow=c(3,1))
    xx <- x@data[[name]];
    xxlen <- length(xx)
    ##if (xxlen < 1)
    ##   stop(paste("this ctd has no data column named '", name, "'",sep=""))
    if (xxlen < 1) {
        xxlen <- length(x@data$pressure)
        xx <- seq(1, xxlen)             # fake a scan number
    }
    if (xxlen != length(x@data$pressure))
        stop(paste("length mismatch.  '", name, "' has length ", xxlen, " but pressure has length ", length(x@data$pressure),sep=""))
    if (!("scan" %in% names(x@data))) {
        x@data[["scan"]] <- 1:length(x@data$pressure)
    }
    plot(x[[name]], x@data$pressure,
         xlab=name, ylab=resizableLabel("p", "y"),
         yaxs='r',
         type=type)
    mtext(paste("Station", x@metadata$station), side=3, adj=1, cex=par('cex'))
    mtext(latlonFormat(x@metadata$latitude, x@metadata$longitude, digits=5), side=3, adj=0, cex=par('cex'))
    if (1 <= adorn.length) {
        t <- try(eval(adorn[1]), silent=TRUE)
        if (class(t) == "try-error")
            warning("cannot evaluate adorn[", 1, "]\n")
    }

    ##    par(mar=c(4,4,1,4)) # bot left top right
    Slen <- length(x@data$salinity)
    Tlen <- length(x@data$temperature)
    if (Slen != Tlen)
        stop(paste("length mismatch.  'salinity' has length ", Slen, " but 'temperature' has length ", Tlen, sep=""))
    plot(x[[name]], x[["temperature"]], xlab="scan", ylab=resizableLabel("T", "y"),
         yaxs='r', type=type)
    grid()
    if (2 <= adorn.length) {
        t <- try(eval(adorn[2]), silent=TRUE)
        if (class(t) == "try-error")
            warning("cannot evaluate adorn[", 2, "]\n")
    }
    plot(x[[name]], x[['salinity']], xlab="scan", ylab=resizableLabel("S", "y"),
         yaxs='r', type=type)
    grid()
    if (2 <= adorn.length) {
        t <- try(eval(adorn[2]), silent=TRUE)
        if (class(t) == "try-error")
            warning("cannot evaluate adorn[", 2, "]\n")
    }
    invisible(x)
}
##* Sea-Bird SBE 25 Data File:
##CTD,20060609WHPOSIODAM

read.ctd <- function(file, type=NULL, columns=NULL, station=NULL, monitor=FALSE, debug=getOption("oceDebug"), processingLog, ...)
{
    if (missing(processingLog)) processingLog <- paste(deparse(match.call()), sep="", collapse="")
    ofile <- file
    filename <- NULL
    if (is.null(type)) {
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
        line <- scan(file, what='char', sep="\n", n=1, quiet=TRUE) # slow, but just one line
        pushBack(line, file)
        if ("CTD" == substr(line, 1, 3))              type <- "WOCE"
        else if ("* Sea-Bird" == substr(line, 1, 10)) type <- "SBE19"
        else stop("Cannot discover type in line '", line, "'\n")
    } else {
        if (!is.na(pmatch(type, "SBE19")))            type <- "SBE19"
        else if (!is.na(pmatch(type, "WOCE")))        type <- "WOCE"
        else stop("type must be SBE19, WOCE or ODF, not ", type)
    }                                   # FIXME: should just use oceMagic() here
    rval <- switch(type,
                   SBE19 = read.ctd.sbe(file, columns=columns, station=station, monitor=monitor,
                                        debug=debug, processingLog=processingLog, ...),
                   WOCE  = read.ctd.woce(file, columns=columns, station=station, missing.value=-999, monitor=monitor,
                                         debug=debug, processingLog=processingLog, ...),
                   ODF = read.ctd.odf(file, columns=columns, station=station, monitor=monitor,
                                      debug=debug, processingLog=processingLog, ...)
                   )
    ## water depth is sometimes zero, which is a hassle in section plots, so make a guess
    if (!"waterDepth" %in% names(ctd@metadata)) # may be entirely missing
        rval@metadata$waterDepth <- max(rval@data$pressure, na.rm=TRUE)
    if (ctd@metadata$waterDepth < 1)   # may be silly
        rval@metadata$waterDepth <- max(rval@data$pressure, na.rm=TRUE)
    rval
}

read.ctd.woce <- function(file, columns=NULL, station=NULL, missing.value=-999, monitor=FALSE,
                          debug=getOption("oceDebug"), processingLog, ...)
{
    ## FIXME: should have an argument that selects CTDSAL or SALNTY
    oceDebug(debug, "\b\bread.ctd.woce() {\n")
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
    res <- new("ctd")
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
    col.names.inferred <- NULL
    found.scan <- FALSE
    found.temperature <- found.salinity <- found.pressure <- found.depth <- FALSE
    foundSigmaTheta <- foundSigmaT <- foundSigma <- FALSE
    found.conductivity <- found.conductivity.ratio <- FALSE
    conductivity.standard <- 4.2914
    ## http://www.nodc.noaa.gov/woce_V2/disk02/exchange/exchange_format_desc.htm
    ## First line
    line <- scan(file, what='char', sep="\n", n=1, quiet=TRUE)
    oceDebug(debug, paste("examining header line '",line,"'\n", sep=""))
    header <- line
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
            cruiseRow <- grep("CRUISE NAME", header[i])
            if (length(cruiseRow)) cruise<- sub("CRUISE[ ]*NAME[ ]*=[ ]*", "", header[i])
            shipRow <- grep("SHIP", header[i])
            if (length(shipRow)) ship <- sub("SHIP[ ]*=[ ]*", "", header[i])
            stationRow <- grep("CASTNO", header[i])
            if (length(stationRow)) station <- sub("CASTNO[ ]*=[ ]*", "", header[i])
            latitudeRow <- grep("LATITUDE", header[i])
            if (length(latitudeRow)) latitude <- as.numeric(sub("LATITUDE.*=[ ]*", "", header[i]))
            longitudeRow <- grep("LONGITUDE", header[i])
            if (length(longitudeRow)) longitude <- as.numeric(sub("LONGITUDE.*=[ ]*", "", header[i]))
            dateRow <- grep("DATE", header[i])
            if (length(dateRow)) {
                d <- sub("[ ]*DATE[ ]*=[ ]*", "", header[i])
                date <- as.POSIXct(d, format="%d-%b-%Y", tz="UTC")
            }
            namesRow <- grep("^[ ]*Pressure,", header[i])
            if (length(namesRow)) {
                names <- strsplit(tolower(header[i]), ",")[[1]]
            }
        }
        dataLines <- lines[seq.int(headerEnd+1, length(lines)-1)]
        metadata <- list(header=header,
                         filename=filename, # provided to this routine
                         filename.orig=filename.orig, # from instrument
                         systemUploadTime=systemUploadTime,
                         ship=ship,
                         scientist=scientist,
                         institute=institute,
                         address=address,
                         cruise=NULL,
                         station=station,
                         date=date,
                         startTime=startTime,
                         latitude=latitude,
                         longitude=longitude,
                         recovery=recovery,
                         waterDepth=NA, #if (is.na(waterDepth)) max(abs(data$pressure)) else waterDepth,
                         sampleInterval=sampleInterval,
                         names=names,
                         labels=labels,
                         src=filename)
        data <- read.table(textConnection(dataLines), header=FALSE, sep=",", col.names=names)
        metadata$waterDepth <- max(abs(data$pressure), na.rm=TRUE)
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
                        oceDebug(debug, line)
                        if ((0 < (r<-regexpr("LATITUDE",  line))))
                            latitude  <- as.numeric(sub("[a-zA-Z =]*","", line))
                        if ((0 < (r<-regexpr("LONGITUDE", line))))
                            longitude <- as.numeric(sub("(.*) =","", line))
                        if ((0 < (r<-regexpr("DATE", line)))) {
                            d <- sub("[ ]*DATE[ ]*=[ ]*", "", line)
                            date <- as.POSIXct(d, format="%Y%m%d", tz="UTC")
                        }
                        if ((0 < (r<-regexpr(pattern="DEPTH", text=line, ignore.case=TRUE))))
                            waterDepth <- as.numeric(sub("[a-zA-Z =:]*","", line))
                        if ((0 < (r<-regexpr(pattern="Profondeur", text=line, ignore.case=TRUE))))
                            waterDepth <- as.numeric(sub("[a-zA-Z =]*","", line))
                        if ((0 < (r<-regexpr(pattern="STNNBR", text=line, ignore.case=TRUE))))
                            station <- as.numeric(sub("[a-zA-Z =]*","", line))
                        if ((0 < (r<-regexpr(pattern="Station", text=line, ignore.case=TRUE))))
                            station <- as.numeric(sub("[a-zA-Z =]*","", line))
                        if ((0 < (r<-regexpr(pattern="Mission", text=line, ignore.case=TRUE)))) {
                            scientist <- sub(".*:", "", line)
                        }
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
        var.names <- strsplit(line, split=",")[[1]]
        oceDebug(debug, "var.names=", paste(var.names, collapse=" "), "\n")
        line <- scan(file, what='char', sep="\n", n=1, quiet=TRUE) # skip the units line
        var.units <- strsplit(line, split=",")[[1]]
        pcol <- pmatch("CTDPRS", var.names)
        if (is.na(pcol)) {
            pcol <- pmatch("DB", var.names)
            if (is.na(pcol))
                stop("cannot find pressure column in list c(\"", paste(var.names, '","'), "\"); need 'DB' or 'CTDPRS'")
        }
        Scol <- pmatch("CTDSAL", var.names)
        if (is.na(Scol)) {
            Scol <- pmatch("SALNTY", var.names)
            if (is.na(Scol))
                stop("cannot find salinity column in list c(\"", paste(var.names, '","'), "\"); need 'CTDSAL' or 'SALNTY'")
        }
        Sflagcol <- pmatch("CTDSAL_FLAG_W", var.names)
        if (is.na(Sflagcol)) {
            Sflagcol <- pmatch("SALNTY_FLAG_W", var.names)
            if (is.na(Sflagcol))
                stop("cannot find salinity-flag column ('CTDSAL_FLAG_W' or 'SALNTY_FLAG_W') in list", paste(var.names,","))
        }
        Tcol <- pmatch("CTDTMP", var.names)
        if (is.na(Tcol))
            stop("cannot find temperature column in list", paste(var.names,","))
        Ocol <- pmatch("CTDOXY", var.names)
        oceDebug(debug, "pcol=", pcol, "Scol=", Scol, "Tcol=", Tcol, "Ocol=", Ocol, "\n")
        ##var.names <- strsplit(line, split=",")[[1]]
        ##oceDebug(debug, "var.names=", paste(var.names, collapse=" "), "[line737]\n")
        line <- scan(file, what='char', sep="\n", n=1, quiet=TRUE)
        var.units <- strsplit(line, split=",")[[1]]
        lines <- readLines(file)
        nlines <- length(lines)
        pressure <- vector("numeric", nlines)
        temperature <- vector("numeric", nlines)
        salinity <- vector("numeric", nlines)
        oxygen <- vector("numeric", nlines)
        b <- 0
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
        ## catch e.g. -999 sometimes used for water depth's missing value
        if (is.finite(waterDepth) && waterDepth <= 0)
            waterDepth <- NA
        metadata <- list(header=header,
                         filename=filename, # provided to this routine
                         filename.orig=filename.orig, # from instrument
                         systemUploadTime=systemUploadTime,
                         ship=ship,
                         scientist=scientist,
                         institute=institute,
                         address=address,
                         cruise=NULL,
                         station=station,
                         date=date,
                         startTime=startTime,
                         latitude=latitude,
                         longitude=longitude,
                         recovery=recovery,
                         waterDepth=if (is.na(waterDepth)) max(abs(data$pressure)) else waterDepth,
                         sampleInterval=sampleInterval,
                         names=names,
                         labels=labels,
                         src=filename)
    }
    res@metadata <- metadata
    res@data <- data
    if (missing(processingLog))
        processingLog <- paste(deparse(match.call()), sep="", collapse="")
    res@processingLog <- processingLog(res@processingLog, processingLog)
    oceDebug(debug, "\b\b} # read.ctd.woce()\n") # FIXME: use S4 for ctd / woce
    res
}


parseLatLon <- function(line, debug=getOption("oceDebug"))
{
    ## The following formats are understood (for, e.g. latitude)
    ## * NMEA Latitude = 47 54.760 N
    ## ** Latitude:      47 53.27 N
    x <- line
    positive <- TRUE
    oceDebug(debug, paste("parseLatLon() processing stages\n0. [", x, "]\n", sep=""))
    x <- sub("(.*)latitude", "", ignore.case=TRUE, x)
    x <- sub("(.*)longitude", "", ignore.case=TRUE, x)
    x <- sub("[:=]", "", ignore.case=TRUE, x)
    oceDebug(debug, paste("1. [", x, "]\n", sep=""))
    if (0 < (r <- regexpr("[NnEe]", x)))
        x <- sub("[NnEe]", "", ignore.case=TRUE, x)
    oceDebug(debug, paste("2. [", x, "]\n", sep=""))
    if (0 < (r <- regexpr("[SsWw]", x))) {
        positive <- FALSE
        x <- sub("[SsWw]", "", ignore.case=TRUE, x)
    }
    oceDebug(debug, paste("3. [", x, "]\n", sep=""))
    x <- sub("^[ \t]*", "", ignore.case=TRUE, x)
    oceDebug(debug, paste("4. [", x, "]\n", sep=""))
    x <- sub("[ \t]*$", "", ignore.case=TRUE, x)
    oceDebug(debug, paste("5. [", x, "]\n", sep=""))
    if (0 == nchar(x)) {
        x <- NA
    } else {
        x <- strsplit(x, " ")
        if (length(x[[1]]) == 2) {
            x <- as.double(x[[1]][1]) + as.double(x[[1]][2]) / 60
            if (!positive)
                x <- (-x)
        } else {
            if (debug > 0)
                warning("cannot parse latitude or longitude in header since need 2 items but got ", length(x[[1]]), " items in '", line, "'\n")
        }
    }
    oceDebug(debug, sprintf("6. x = %f\n", x))
    x
}

time.formats <- c("%b %d %Y %H:%M:%s", "%Y%m%d")

read.ctd.sbe <- function(file, columns=NULL, station=NULL, missing.value, monitor=FALSE, debug=getOption("oceDebug"), processingLog, ...)
{
    oceDebug(debug, "\b\bread.ctd.sbe() {\n")
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
    res <- new("ctd")
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
    found.temperature <- found.salinity <- found.pressure <- found.depth <- found.scan <-
        found.time <- foundSigmaTheta <- foundSigmaT <- found.sigma <-
            found.conductivity <- found.conductivity.ratio <- FALSE
    conductivity.standard <- 4.2914
    found.header.latitude <- found.header.longitude <- FALSE
    serialNumber <- ""
    while (TRUE) {
        line <- scan(file, what='char', sep="\n", n=1, quiet=TRUE)
        oceDebug(debug, paste("examining header line '",line,"'\n"))
        header <- c(header, line)
        ##if (length(grep("\*END\*", line))) #BUG# why is this regexp no good (new with R-2.1.0)
        aline <- iconv(line, from="UTF-8", to="ASCII", sub="?")
        if (length(grep("END", aline, perl=TRUE, useBytes=TRUE))) break;
        lline <- tolower(aline)
        ## BUG: discovery of column names is brittle to format changes
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
                name <- "pressure"
                found.pressure <- TRUE
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
                name <- "temperature"
                found.temperature <- TRUE
            }
            if (0 < regexpr("conductivity", lline)) {
                if (0 < regexpr("ratio", lline)) {
                    found.conductivity.ratio <- TRUE;
                    name <- "conductivityratio";
                } else {
                    found.conductivity <- TRUE;
                    name <- "conductivity";
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
                foundSigmaTheta <- TRUE
            } else {
                if (0 < regexpr("sigma-t", lline)) {
                    name <- "sigmat"
                    foundSigmaT <- TRUE
                }
            }
            col.names.inferred <- c(col.names.inferred, name)
        }
        if (0 < regexpr(".*seacat profiler.*", lline))
            serialNumber <- gsub("[ ].*$","",gsub(".*sn[ ]*","",lline))
        if (0 < (r<-regexpr("date:", lline))) {
            d <- sub("(.*)date:([ ])*", "", lline)
            date <- as.POSIXct(d, format="%Y%m%d", tz="UTC")
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
            latitude <- parseLatLon(lline)
            found.header.latitude <- TRUE
        }
        if (!found.header.longitude && (0 < (r<-regexpr("longitude*[0-8]*", lline, ignore.case=TRUE)))) {
            longitude <- parseLatLon(lline)
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
        if (0 < (r<-regexpr("cruise:", lline)))
            cruise <- sub("(.*)cruise:([ ])*", "", ignore.case=TRUE, line) # full string
        if (is.null(station)) {
            if (0 < (r<-regexpr("station:", lline)))
                station <- sub("(.*)station:([ ])*", "", ignore.case=TRUE, line) # full string
        }
        if (0 < (r<-regexpr("recovery:", lline)))
            recovery <- sub("(.*)recovery:([ ])*", "", lline)
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

    ## If no water depth found, guess it from the maximum depth
    ##
    ##    if (is.na(water.depth)) {
    ##        water.depth <- max(data$pressure, na.rm=TRUE)
    ##        print(data$pressure)
    ##        oceDebug(debug, "file header has no water depth, so inferring", water.depth, "from the pressure")
    ##    }
    ##
    metadata <- list(header=header,
                     type="SBE",
                     hexfilename=hexfilename, # from instrument
                     serialNumber=serialNumber,
                     systemUploadTime=systemUploadTime,
                     ship=ship,
                     scientist=scientist,
                     institute=institute,
                     address=address,
                     cruise=cruise,
                     station=station,
                     date=date,
                     startTime=startTime,
                     latitude=latitude,
                     longitude=longitude,
                     recovery=recovery,
                     waterDepth=waterDepth,
                     sampleInterval=sampleInterval,
                     names=col.names.inferred,
                     labels=col.names.inferred,
                     filename=filename)

    ## Read the data as a table.
    ## FIXME: should we match to standardized names?
    ##col.names.forced <- c("scan","pressure","temperature","conductivity","descent","salinity","sigmaThetaUnused","depth","flag")
    col.names.inferred <- tolower(col.names.inferred)
    oceDebug(debug, "About to read these names:", col.names.inferred,"\n")
    data <- as.list(read.table(file, col.names=col.names.inferred, colClasses="numeric"))
    ndata <- length(data[[1]])
    if (0 < ndata) {
        haveData <- TRUE
        names <- names(data)
        labels <- names
        if (!found.scan) {
            data[['scan']] <- 1:ndata
        }
    } else {
        haveData <- FALSE
        warning("no data in CTD file \"", filename, "\"\n")
        data <- list(scan=NULL, salinity=NULL, temperature=NULL, pressure=NULL)
    }
    if (missing(processingLog))
        processingLog <- paste(deparse(match.call()), sep="", collapse="")
    hitem <- processingLogItem(processingLog)
    res@metadata <- metadata
    res@data <- data
    ## Add standard things, if missing
    if (haveData) {
        if (!found.salinity) {
            if (found.conductivity.ratio) {
                warning("cannot find 'salinity' in this file; calculating from T, C, and p")
                S <- swSCTp(data$conductivityratio, data$temperature, data$pressure)
            } else if (found.conductivity) {
                warning("cannot find 'salinity' in this file; calculating from T, C-ratio, and p")
                S <- swSCTp(data$conductivity/conductivity.standard, data$temperature, data$pressure)
            } else {
                stop("cannot find salinity in this file, nor conductivity or conductivity ratio")
            }
            res <- ctdAddColumn(res, S, name="salinity", label="Salinity", unit="PSU", debug=debug-1)
        }
        if (found.depth && !found.pressure) { # BUG: this is a poor, nonrobust approximation of pressure
            g <- if (found.header.latitude) gravity(latitude) else 9.8
            rho0 <- 1000 + swSigmaTheta(median(res@data$salinity), median(res@data$temperature), rep(0, length(res@data$salinity)))
            res <- ctdAddColumn(res, res@data$depth * g * rho0 / 1e4, name="pressure", label="Pressure", unit="dbar", debug=debug-1)
        }
        res <- ctdAddColumn(res, swSigmaTheta(res@data$salinity, res@data$temperature, res@data$pressure),
                        name="sigmaTheta", label="Sigma Theta", unit="kg/m^3", debug=debug-1)
    }
    if (is.na(res@metadata$waterDepth)) 
        res@metadata$waterDepth <- max(abs(data$pressure))
    oceDebug(debug, "} # read.ctd.sbe()\n")
    res@processingLog <- processingLog(res@processingLog, paste(deparse(match.call()), sep="", collapse=""))
    res
}

read.ctd.odf <- function(file, columns=NULL, station=NULL, missing.value=-999, monitor=FALSE,
                         debug=getOption("oceDebug"), processingLog, ...)
{
    fromHeader <- function(key)
    {
        i <- grep(key, lines)
        if (length(i) < 1)
            ""
        else
            gsub("\\s*$", "", gsub("^\\s*", "", gsub("'","", gsub(",","",strsplit(lines[i[1]], "=")[[1]][2]))))
    }
    oceDebug(debug, "\b\bread.ctd.odf() {\n")
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
    dataStart <- grep("-- DATA --", lines)
    if (!length(dataStart))
        stop("cannot locate a line containing '-- DATA --'")
    parameterStart <- grep("PARAMETER_HEADER", lines)
    if (!length(parameterStart))
        stop("cannot locate any lines containing 'PARAMETER_HEADER'")
    namesWithin <- parameterStart[1]:dataStart[1]
    ## extract column codes in a step-by-step way, to make it easier to adjust if the format changes
    nullValue <- as.numeric(fromHeader("NULL_VALUE")[1]) # FIXME: should do this for columns separately
    names <- lines[grep("^\\s*CODE\\s*=", lines)]
    names <- gsub("\\s*$", "", gsub("^\\s*", "", names)) # trim start/end whitespace
    names <- gsub(",", "", names) # trim commas
    names <- gsub("'", "", names) # trim single quotes
    names <- gsub(",\\s*$", "", gsub("^\\s*","", names)) # "  CODE=PRES_01," -> "CODE=PRES_01"
    names <- gsub("^CODE\\s*=\\s*", "", names) # "CODE=PRES_01" -> "PRES_01"
    names <- gsub("\\s*$", "", gsub("^\\s*", "", names)) # trim remnant start/end spaces
    scientist <- fromHeader("CHIEF_SCIENTIST")
    ship <- fromHeader("PLATFORM") # maybe should rename, e.g. for helicopter
    institute <- fromHeader("ORGANIZATION") # maybe should rename, e.g. for helicopter
    latitude <- as.numeric(fromHeader("INITIAL_LATITUDE"))
    longitude <- as.numeric(fromHeader("INITIAL_LONGITUDE"))
    cruise <- fromHeader("CRUISE_NAME")
    countryInstituteCode <- fromHeader("COUNTRY_INSTITUTE_CODE")
    cruiseNumber <- fromHeader("CRUISE_NUMBER")
    date <- strptime(fromHeader("START_DATE"), "%b %d/%y")
    startTime <- strptime(tolower(fromHeader("START_DATE_TIME")), "%d-%b-%Y %H:%M:%S", tz="UTC")
    endTime <- strptime(tolower(fromHeader("END_DATE_TIME")), "%d-%b-%Y %H:%M:%S", tz="UTC")
    waterDepth <- as.numeric(fromHeader("SOUNDING"))
    station <- fromHeader("EVENT_NUMBER")
    if (!is.na(waterDepth) && waterDepth < 0)                # catch -999
        waterDepth <- NA
    type <- fromHeader("INST_TYPE")
    if (length(grep("sea", type, ignore.case=TRUE)))
        type <- "SBE"
    serialNumber <- fromHeader("SERIAL_NUMBER")
    model <- fromHeader("MODEL")
    metadata <- list(header=NULL, # FIXME
                     type=type,        # only odt
                     model=model,      # only odt
                     serialNumber=serialNumber,
                     ship=ship,
                     scientist=scientist,
                     institute=institute,
                     address=NULL,
                     cruise=cruise,
                     station=station,
                     countryInstituteCode=countryInstituteCode, # ODF only
                     cruiseNumber=cruiseNumber, # ODF only
                     date=startTime,
                     startTime=startTime,
                     latitude=latitude,
                     longitude=longitude,
                     recovery=NULL,
                     waterDepth=if (is.na(waterDepth)) max(abs(data$pressure)) else waterDepth,
                     sampleInterval=NA,
                     filename=filename)
    fff <- textConnection(lines)
    data <- read.table(fff, skip=dataStart)
    close(fff)
    if (length(data) != length(names))
        stop("mismatch between length of data names (", length(names), ") and number of columns in data matrix (", length(data), ")")
    if (debug) cat("Initially, column names are:", paste(names, collapse="|"), "\n\n")
    ## Infer standardized names for columsn, partly based on documentation (e.g. PSAL for salinity), but
    ## mainly from reverse engineering of some files from BIO and DFO.  The reverse engineering
    ## really is a kludge, and if things break (e.g. if data won't plot because of missing temperatures,
    ## or whatever), this is a place to look.  That's why the debugging flag displays a before-and-after
    ## view of names.
    ## Step 1: trim numbers at end (which occur for BIO files)
    ## Step 2: recognize some official names
    names[grep("CNTR_*.*", names)[1]] <- "scan"
    names[grep("CRAT_*.*", names)[1]] <- "conductivity"
    names[grep("OCUR_*.*", names)[1]] <- "oxygen_by_mole"
    names[grep("OTMP_*.*", names)[1]] <- "oxygen_temperature"
    names[grep("PSAL_*.*", names)[1]] <- "salinity"
    names[grep("PSAR_*.*", names)[1]] <- "par"
    names[grep("DOXY_*.*", names)[1]] <- "oxygen_by_volume"
    names[grep("TEMP_*.*", names)[1]] <- "temperature"
    names[grep("TE90_*.*", names)[1]] <- "temperature"
    names[grep("PRES_*.*", names)[1]] <- "pressure"
    names[grep("DEPH_*.*", names)[1]] <- "pressure" # FIXME possibly this actually *is* depth, but I doubt it
    names[grep("SIGP_*.*", names)[1]] <- "sigmaTheta"
    names[grep("FLOR_*.*", names)[1]] <- "fluorometer"
    names[grep("FFFF_*.*", names)[1]] <- "flag"
    ## Step 3: recognize something from moving-vessel CTDs
    names[which(names=="FWETLABS")[1]] <- "fwetlabs" # FIXME: what is this?
    if (debug) cat("Finally, column names are:", paste(names, collapse="|"), "\n\n")
    names(data) <- names
    if (!is.na(nullValue)) {
        data[data==nullValue] <- NA
    }
    if (!("salinity" %in% names)) warning("missing data$salinity")
    if (!("pressure" %in% names)) warning("missing data$pressure")
    if (!("temperature" %in% names)) warning("missing data$temperature")
    metadata$names <- names
    metadata$labels <- labels 
    if (missing(processingLog))
        processingLog <- paste(deparse(match.call()), sep="", collapse="")
    hitem <- processingLogItem(processingLog)
    res <- new("ctd")
    res@data <- data
    res@metadata <- metadata
    res@processingLog <- hitem
    res <- ctdAddColumn(res, swSigmaTheta(res@data$salinity, res@data$temperature, res@data$pressure),
                        name="sigmaTheta", label="Sigma Theta", unit="kg/m^3", debug=debug-1)
    oceDebug(debug, "} # read.ctd.odf()\n")
    res
}

summary.ctd <- function(object, ...)
{
    cat("CTD Summary\n-----------\n\n")
    showMetadataItem(object, "type", "Instrument: ")
    showMetadataItem(object, "model", "Instrument model:  ")
    showMetadataItem(object, "serialNumber", "Instrument serial number:  ")
    showMetadataItem(object, "filename", "File source:         ")
    showMetadataItem(object, "hexfilename", "Original file source (hex):  ")
    showMetadataItem(object, "institute", "Institute:      ")
    showMetadataItem(object, "scientist", "Chief scientist:      ")
    showMetadataItem(object, "date", "Date:      ", isdate=TRUE)
    showMetadataItem(object, "startTime", "Start time:          ", isdate=TRUE)
    showMetadataItem(object, "systemUploadTime", "System upload time:  ", isdate=TRUE)
    showMetadataItem(object, "cruise",  "Cruise:              ")
    showMetadataItem(object, "ship",    "Vessel:              ")
    showMetadataItem(object, "station", "Station:             ")
    cat("* Location:           ",       latlonFormat(object@metadata$latitude,
                                                     object@metadata$longitude,
                                                     digits=5), "\n")
    showMetadataItem(object, "waterDepth", "Water depth: ")
    showMetadataItem(object, "levels", "Number of levels: ")
    cat("* Statistics of subsample::\n")
    ndata <- length(object@data)
    threes <- matrix(nrow=ndata, ncol=3)
    for (i in 1:ndata)
        threes[i,] <- threenum(object@data[[i]])
    rownames(threes) <- paste("   ", names(object@data))
    colnames(threes) <- c("Min.", "Mean", "Max.")
    print(threes, indent='  ')
    processingLogShow(object)
    invisible()
} 


plotTS <- function (x,
                    inSitu=FALSE,
                    type='p',
                    referencePressure=0,
                    rhoLevels=6,
                    grid=TRUE,
                    col.grid="lightgray",
                    lty.grid="dotted",
                    rho1000=FALSE,
                    eos=getOption("eos", default='unesco'),
                    cex=par("cex"), col = par("col"), pch=par("pch"), bg,
                    col.rho="darkgray",
                    cex.rho=3/4*par("cex"),
                    rotateRhoLabels=FALSE,
                    useSmoothScatter=FALSE,
                    xlab, ylab,
                    Slim, Tlim,
                    mgp=getOption("oceMgp"),
                    mar=c(mgp[1]+1.5,mgp[1]+1.5,mgp[1],mgp[1]),
                    lwd.rho=par("lwd"), lty.rho=par("lty"),
                    add=FALSE, inset=FALSE,
                    debug=getOption("oceDebug"),
                    ...)
{
    oceDebug(debug, "\bplotTS(..., lwd.rho=", lwd.rho, ", lty.rho=", lty.rho,
             "eos=\"", eos, "\", ",
             "mgp=c(", paste(mgp, collapse=","), "), ", 
             "mar=c(", paste(mar, collapse=","), "), ", 
             "...) {\n", sep="")
    eos <- match.arg(eos, c("unesco", "teos"))
    if (!inherits(x, "ctd")) {
        if (inherits(x, "section")) { 
            salinity <- salinity(x) # FIXME: new accessors?
            temperature <- temperature(x)
            x <- as.ctd(salinity, temperature, 0) # FIXME: what if we want theta?
        } else {
            names <- names(x)
            if ("temperature" %in% names && "salinity" %in% names) {
                x <- as.ctd(x$salinity, x$temperature, 0) # FIXME: what if we want theta?
            } else {
                names <- names(x@data)
                if ("temperature" %in% names && "salinity" %in% names) {
                    x <- as.ctd(x@data$salinity, x@data$temperature, 0) # FIXME: what if we want theta?
                } else {
                    stop("cannot find salinity and temperature in 'x'")
                }
            }
        }
    }
    if (eos == "teos") {
        salinity <- x[["SA"]]
        y <- x[["CT"]]
    } else {
        y <- if (inSitu) x[["temperature"]] else swTheta(x, referencePressure=referencePressure)
        salinity <- x[["salinity"]]
    }
    if (missing(Slim)) Slim <- range(salinity, na.rm=TRUE)
    if (missing(Tlim)) Tlim <- range(y, na.rm=TRUE)
    if (!add) {
        omar <- par("mar")
        omgp <- par("mgp")
        opar <- par(no.readonly = TRUE)
        if (!inset) {
            ## on.exit(par(mar=omar, mgp=omgp))
            if (3 == length(mgp)) par(mgp=mgp)
            if (!is.null(mar)) {
                if (4 == length(mar)) par(mar=mar)
            }
        }
    }
    axis.name.loc <- mgp[1]
    if (missing(xlab)) {
        if (eos == "teos")
            xlab <- resizableLabel("absolute salinity", "x")
        else
            xlab <- resizableLabel("S","x")
    }
    if (missing(ylab)) {
        if (eos == "teos")
            ylab <- resizableLabel("conservative temperature", "y")
        else
            ylab <- if (inSitu) resizableLabel("T","y") else resizableLabel("theta", "y")
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
            if (type == 'p')
                points(salinity, y, cex=cex, pch=pch, col=col)
            else if (type == 'l')
                lines(salinity, y, col=col, ...)
            else if (type == 'o') {
                points(salinity, y, cex=cex, pch=pch, col=col)
                lines(salinity, y, col=col, ...)
            } else 
                points(salinity, y, cex=cex, pch=pch, col=col)
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
            if (type == 'p')
                points(salinity, y, cex=cex, pch=pch, col=col, ...)
            else if (type == 'l')
                lines(salinity, y, col=col, ...)
            else if (type == 'o') {
                points(salinity, y, cex=cex, pch=pch, col=col, ...)
                lines(salinity, y, col=col, ...)
            } else
                points(salinity, y, cex=cex, pch=pch, col=col, ...)

        }
    }
    ## grid, isopycnals, then freezing-point line
    if (grid)
        grid(col=col.grid, lty=lty.grid)
    drawIsopycnals(rhoLevels=rhoLevels, rotateRhoLabels=rotateRhoLabels, rho1000=rho1000,
                   eos=eos, cex=cex.rho, col=col.rho, lwd=lwd.rho, lty=lty.rho)
    usr <- par("usr")
    Sr <- c(max(0, usr[1]), usr[2])
    lines(Sr, swTFreeze(salinity=Sr, pressure=0), col="darkblue")
    box()                              # redraw box (otherwise overdrawn with isopycnals)
    oceDebug(debug, "\b} # plotTS(...)\n", sep="")
}

drawIsopycnals <- function(rhoLevels=6, rotateRhoLabels=TRUE, rho1000=FALSE,
                           eos=getOption("eos", default='unesco'),
                           cex=1, col="darkgray", lwd=par("lwd"), lty=par("lty"))
{
    eos <- match.arg(eos, c("unesco","teos"))
    usr <- par("usr")
    SAxisMin <- max(0.1, usr[1])       # avoid NaN, which UNESCO density gives for freshwater
    SAxisMax <- usr[2]
    TAxisMin <- usr[3]
    TAxisMax <- usr[4]
    if (eos == "teos") {
        rhoCorners <- teos("gsw_rho",
                           c(SAxisMin, SAxisMax, SAxisMin, SAxisMax),
                           c(TAxisMin, TAxisMin, TAxisMax, TAxisMax),
                           rep(0, 4)) - 1000
    } else {
        rhoCorners <- swSigma(c(SAxisMin, SAxisMax, SAxisMin, SAxisMax),
                              c(TAxisMin, TAxisMin, TAxisMax, TAxisMax),
                              rep(0, 4))
    }
    rhoMin <- min(rhoCorners, na.rm=TRUE)
    rhoMax <- max(rhoCorners, na.rm=TRUE)
    if (length(rhoLevels) == 1) {
        rhoList <- pretty(c(rhoMin, rhoMax), n=rhoLevels)
        ## Trim first and last values, since not in box
        rhoList <- rhoList[-1]
        rhoList <- rhoList[-length(rhoList)]
    } else {
        rhoList <- rhoLevels
    }
    Tn <- 200
    Tline <- seq(TAxisMin, TAxisMax, length.out=Tn)
    cex.par <- par("cex")               # need to scale text() differently than mtext()
    for (rho in rhoList) {
        rhoLabel <- if (rho1000) 1000+rho else rho
        Sline <- swSTrho(Tline, rep(rho, Tn), rep(0, Tn), eos=eos)
        ok <- !is.na(Sline) # crazy T can give crazy S
        if (sum(ok) > 2) {
            Sok <- Sline[ok]
            Tok <- Tline[ok]
            lines(Sok, Tok, col = col, lwd=lwd, lty=lty)
            if (cex > 0) {
                if (Sok[length(Sok)] > SAxisMax) { # to right of box
                    i <- match(TRUE, Sok > SAxisMax)
                    if (rotateRhoLabels)
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
                         ytype=c("pressure", "z", "sigmaTheta"),
                         eos=getOption("eos", default='unesco'),
                         xlab=NULL, ylab=NULL,
                         col='black',
                         col.salinity = "darkgreen",
                         col.temperature = "red",
                         col.rho = "blue",
                         col.N2 = "brown",
                         col.dpdt = "darkgreen",
                         col.time = "darkgreen",
                         grid = TRUE,
                         col.grid = "lightgray",
                         lty.grid = "dotted",
                         Slim, Tlim, densitylim, N2lim, dpdtlim, timelim, ylim,
                         lwd=par("lwd"),
                         xaxs="r",
                         yaxs="r",
                         cex=1, pch=1,
                         useSmoothScatter=FALSE,
                         df,
                         keepNA=FALSE,
                         type='l',
                         mgp=getOption("oceMgp"),
                         mar=c(1 + if (length(grep('\\+', xtype))) mgp[1] else 0, mgp[1]+2, mgp[1] + 2, 2),
                         add=FALSE,
                         inset=FALSE,
                         debug=getOption("oceDebug"),
                         ...)
{
    oceDebug(debug, "\bplotProfile(x, xtype=\"", if(is.vector(xtype)) "(a vector)" else xtype,
             "\", ...) {\n", sep="")
    eos <- match.arg(eos, c("unesco", "teos"))
    plotJustProfile <- function(x, y, col="black", type="l", lwd=par("lwd"), cex=1, pch=1, df=df, keepNA=FALSE,
                                debug=getOption("oceDebug"))
    {
        oceDebug(debug, "\b    plotJustProfile(type=\"", if (is.vector(type)) "(a vector)" else type, "\", col[1:3]=\"", col[1:3], "\", ...) {\n", sep="")
        if (!keepNA) {
            keep <- !is.na(x) & !is.na(y)
            x <- x[keep]
            y <- y[keep]
        }
        if (type == 'l') {
            lines(x, y, col = col, lwd=lwd)
        } else if (type == 's') {
            lines(x, y, col = col, lwd=lwd, type='s')
        } else if (type == 'p') {
            points(x, y, col = col, cex=cex, pch=pch)
        } else if (type == 'o') {
            lines(x, y, col = col, lwd=lwd)
            points(x, y, col = col, cex=cex, pch=pch)
        } else if (type == 'b') {
            lines(x, y, col = col, lwd=lwd)
            points(x, y, col = col, cex=cex, pch=pch)
        } else if (type == 'n') {
            ; # skip it
        } else {
            lines(x, y, col = col, lwd=lwd)
        }
        oceDebug(debug, "} # plotJustProfile\n")
    }
    ##if (!inherits(x, "ctd")) stop("method is only for ctd objects")
    ylimGiven <- !missing(ylim)
    densitylimGiven <- !missing(densitylim)
    dots <- list(...)
    ytype <- match.arg(ytype)
    yname <- switch(ytype,
                    pressure=resizableLabel("p", "y"),
                    z=resizableLabel("z", "y"),
                    sigmaTheta=resizableLabel("sigmaTheta", "y"))
##    par(mgp=mgp, mar=mar)
    if (missing(ylim))
        ylim <- switch(ytype,
                       pressure = rev(range(x@data$pressure, na.rm=TRUE)),
                       z = range(swZ(x), na.rm=TRUE),
                       sigmaTheta = rev(range(x@data$sigmaTheta, na.rm=TRUE)))
    axis.name.loc <- mgp[1]
    know.time.unit <- FALSE
    if ("time" %in% names(x@data)) {
        know.time.unit <- TRUE
        time <- x@data$time
    } else {
        time <- 0:(length(x@data$pressure) - 1)
        if (!is.na(x@metadata$sampleInterval)) {
            know.time.unit <- TRUE
            time <- time * x@metadata$sampleInterval
        }
    }
    if (ytype == "pressure")
        y <- x@data$pressure
    else if (ytype == "z")
        y <- swZ(x@data$pressure)
    else if (ytype == "sigmaTheta")
        y <- x@data$sigmaTheta # FIXME: are we sure this exists?

    if (!add)
        par(mar=mar, mgp=mgp)

    if (is.vector(xtype) && length(xtype) == length(y)) {
        if ('axes' %in% names(list(...))) {
            plot(xtype, y, xlab="", ylab=yname, type=type, xaxs=xaxs, yaxs=yaxs, ylim=ylim, col=col, ...)
            if (list(...)$axes) {
                axis(3)
                mtext(xlab, side = 3, line = axis.name.loc, cex=par("cex"))
                axis(2)
            }
            box()
        } else {
            plot(xtype, y, xlab="", ylab=yname, type=type, axes=FALSE, xaxs=xaxs, yaxs=yaxs, ylim=ylim, col=col, ...)
            axis(3)
            mtext(xlab, side = 3, line = axis.name.loc, cex=par("cex"))
            axis(2)
            box()
        }
    } else if (is.numeric(xtype)) {
        if (length(xtype) != length(y))
            stop("length(xtype) must match number of levels in the CTD object")
        if (add) {
            lines(xtype, y, type=type, ...)
        } else {
            plot(xtype, y, xlab="", ylab=yname, type=type, axes=FALSE, xaxs=xaxs, yaxs=yaxs, ylim=ylim, ...)
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
        plot(index, x@data$pressure, ylim=ylim, xlab = "index", ylab = yname, type='l', xaxs=xaxs, yaxs=yaxs)
        if (grid) {
            at <- par("yaxp")
            abline(h=seq(at[1], at[2], length.out=at[3]+1), col=col.grid, lty=lty.grid)
        }
    } else if (xtype == "density+time") {
        if (add)
            warning("argument 'add' is ignored for xtype=\"density+time\"")
        st <- swSigmaTheta(x@data$salinity, x@data$temperature, x@data$pressure) # why recalculate?
        if (missing(densitylim))
            densitylim <- range(x@data$sigmaTheta, na.rm=TRUE)
        look <- if (keepNA) 1:length(y) else !is.na(st) & !is.na(y)
        plot(st[look], y[look], xlim=densitylim, ylim=ylim,
             type = "n", xlab = "", ylab = yname, axes = FALSE, xaxs=xaxs, yaxs=yaxs, ...)
        lines(st[look], y[look])
        axis(3, col = col.rho, col.axis = col.rho, col.lab = col.rho)
        mtext(expression(paste(sigma[theta], " [ ", kg/m^3, " ]")), side = 3, line = axis.name.loc, col = col.rho, cex=par("cex"))
        axis(2)
        box()
        par(new = TRUE)                ## FIXME: this probably won't work if add=TRUE
        if (missing(timelim))
            timelim <- range(time, na.rm=TRUE)
        plot(time, y, xlim=timelim, ylim=ylim, type='n', xlab="", ylab=yname, axes=FALSE, lwd=lwd, col=col.time, xaxs=xaxs, yaxs=yaxs)
        axis(1, col=col.dpdt, col.axis=col.dpdt, col.lab=col.time)
        lines(time, y, lwd=lwd, col=col.time)
        if (know.time.unit)
            mtext(expression(paste(Delta*t, " [ s ]")), side = 1, line = axis.name.loc, cex=par("cex"), col=col.time)
        else
            mtext(expression(paste(Delta*t, " [ unknown unit ]")), side = 1, line = axis.name.loc, cex=par("cex"), col=col.time)
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
             xlim=densitylim, ylim=ylim,
             type = "n", xlab = "", ylab = yname, axes = FALSE, xaxs=xaxs, yaxs=yaxs, ...)
        axis(3, col = col.rho, col.axis = col.rho, col.lab = col.rho)
        mtext(expression(paste(sigma[theta], " [ ", kg/m^3, " ]")), side = 3, line = axis.name.loc, col = col.rho, cex=par("cex"))
        axis(2)
        box()
        lines(st, y, col = col.rho, lwd=lwd)
        par(new = TRUE)
        dpdt <- diff(x@data$pressure) / diff(time)
        dpdt <- c(dpdt[1], dpdt)        # fake first point
        df <- min(max(x@data$pressure, na.rm=TRUE) / 5, length(x@data$pressure) / 10) # FIXME: adjust params
        dpdt.sm <- smooth.spline(x@data$pressure, dpdt, df=df)
        if (missing(dpdtlim))
            dpdtlim <- range(dpdt.sm$y)
        plot(dpdt.sm$y, dpdt.sm$x, xlim=dpdtlim, ylim=ylim, type='n', xlab="", ylab=yname, axes=FALSE, lwd=lwd, col=col.dpdt,
             xaxs=xaxs, yaxs=yaxs, ...)
        axis(1, col=col.dpdt, col.axis=col.dpdt, col.lab=col.dpdt)
        lines(dpdt.sm$y, dpdt.sm$x, lwd=lwd, col=col.dpdt)
        mtext(expression(paste(dp/dt, if (know.time.unit) " [ dbar/s ]" else " [ dbar/(time-unit)]")),
              side = 1, line = axis.name.loc, cex=par("cex"), col=col.dpdt)
        box()
        if (grid) {
            at <- par("yaxp")
            abline(h=seq(at[1], at[2], length.out=at[3]+1), col=col.grid, lty=lty.grid)
            at <- par("xaxp")
            abline(v=seq(at[1], at[2], length.out=at[3]+1), col=col.grid, lty=lty.grid)
        }
    } else if (xtype == "S" || xtype == "salinity") {
        salinity <- if (eos == "teos") swAbsoluteSalinity(x) else x@data$salinity
        if (missing(Slim)) {
            if ("xlim" %in% names(dots)) Slim <- dots$xlim else Slim <- range(salinity, na.rm=TRUE)
        }
        if (useSmoothScatter) {
            smoothScatter(salinity, y, xlim=Slim, ylim=ylim, xlab="", ylab=yname, axes=FALSE, ...)
            axis(2)
            axis(3)
            box()
            if (eos == "teos")
                mtext(resizableLabel("absolute salinity", "x"), side = 3, line = axis.name.loc, cex=par("cex"))
            else
                mtext(resizableLabel("S", "x"), side = 3, line = axis.name.loc, cex=par("cex"))
        } else {
            look <- if (keepNA) 1:length(y) else !is.na(salinity) & !is.na(y)
            if (!add) {
                plot(salinity[look], y[look],
                     xlim=Slim, ylim=ylim,
                     type = "n", xlab = "", ylab = yname, axes = FALSE, xaxs=xaxs, yaxs=yaxs, ...)
                if (eos == "teos")
                    mtext(resizableLabel("absolute salinity", "x"), side = 3, line = axis.name.loc, cex=par("cex"))
                else
                    mtext(resizableLabel("S", "x"), side = 3, line = axis.name.loc, cex=par("cex"))
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
            plotJustProfile(salinity, y, type=type, lwd=lwd, cex=cex, pch=pch, keepNA=keepNA, debug=debug-1)
        }
    } else if (xtype %in% c("oxygen", "nitrate", "nitrite", "phosphate", "silicate", "tritium")) {
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
                         ylim=ylim,
                         type = "n", xlab = "", ylab = yname, axes = FALSE, xaxs=xaxs, yaxs=yaxs, ...)
                } else {
                    plot(x@data[[xtype]][look], y[look],
                         ylim=rev(range(y[look])),
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
            plotJustProfile(x@data[[xtype]][look], y[look], type=type, lwd=lwd, cex=cex, pch=pch, keepNA=keepNA, debug=debug-1)
        }
    } else if (xtype == "Rrho" || xtype == "RrhoSF") {
        Rrho <- swRrho(x, sense=if (xtype=="Rrho") "diffusive" else "finger", ...)
        look <- if (keepNA) 1:length(y) else !is.na(Rrho) & !is.na(y)
        if (!add) {
            if (ylimGiven) {
                plot(Rrho, y[look],
                     ylim=ylim,
                     type = "n", xlab = "", ylab = yname, axes = FALSE, xaxs=xaxs, yaxs=yaxs, ...)
            } else {
                plot(Rrho, y[look],
                     ylim=rev(range(y[look])),
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
        plotJustProfile(Rrho, y[look], type=type, lwd=lwd, cex=cex, pch=pch, keepNA=keepNA, debug=debug-1)
    } else if (xtype == "T" || xtype == "temperature") {
        temperature <- if (eos == "teos") swConservativeTemperature(x) else x@data$temperature
        if (missing(Tlim)) {
            if ("xlim" %in% names(dots)) Tlim <- dots$xlim else Tlim <- range(temperature, na.rm=TRUE)
        }
        if (useSmoothScatter) {
            smoothScatter(temperature, y, xlim=Tlim, ylim=ylim, xlab="", ylab=yname, axes=FALSE, ...)
            axis(2)
            axis(3)
            box()
            if (eos == "teos")
                mtext(resizableLabel("conservative temperature", "x"), side = 3, line = axis.name.loc, cex=par("cex"))
            else
                mtext(resizableLabel("T", "x"), side = 3, line = axis.name.loc, cex=par("cex"))
        } else {
            look <- if (keepNA) 1:length(y) else !is.na(x@data$temperature) & !is.na(y)
            if (!add) {
                plot(temperature[look], y[look],
                     xlim=Tlim, ylim=ylim,
                     type = "n", xlab = "", ylab = "", axes = FALSE, xaxs=xaxs, yaxs=yaxs, ...)
                if (eos == "teos")
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
            plotJustProfile(temperature, y, type=type, lwd=lwd, cex=cex, pch=pch, keepNA=keepNA, debug=debug-1)
        }
    } else if (xtype == "theta" || xtype == "potential temperature") {
        theta <- swTheta(x, method=eos)
        if (missing(Tlim)) {
            if ("xlim" %in% names(dots)) Tlim <- dots$xlim else Tlim <- range(theta, na.rm=TRUE)
        }
        if (useSmoothScatter) {
            smoothScatter(theta, y, xlim=Tlim, ylim=ylim, xlab="", ylab=yname, axes=FALSE, ...)
            axis(2)
            axis(3)
            box()
            if (eos == "teos")
                mtext(resizableLabel("Conservative temperature", "x"), side = 3, line = axis.name.loc, cex=par("cex"))
            else
                mtext(resizableLabel(theta, "x"), side = 3, line = axis.name.loc, cex=par("cex"))
        } else {
            look <- if (keepNA) 1:length(y) else !is.na(theta) & !is.na(y)
            if (!add) {
                plot(theta[look], y[look],
                     xlim=Tlim, ylim=ylim,
                     type = "n", xlab = "", ylab = "", axes = FALSE, xaxs=xaxs, yaxs=yaxs, ...)
                if (eos == "teos")
                    mtext(resizableLabel("Conservative temperature", "x"), side = 3, line = axis.name.loc, cex=par("cex"))
                else
                    mtext(resizableLabel("theta", "x"), side = 3, line = axis.name.loc, cex=par("cex"))
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
            plotJustProfile(theta, y, type=type, lwd=lwd, cex=cex, pch=pch, keepNA=keepNA, debug=debug-1)
        }
    } else if (xtype == "density") {
        st <- swSigmaTheta(x@data$salinity, x@data$temperature, x@data$pressure) # FIXME: why not use existing column?
        look <- if (keepNA) 1:length(y) else !is.na(st) & !is.na(y)
        ## FIXME: if this works, extend to other x types
        look <- look & (min(ylim) <= y & y <= max(ylim))
        if (!add) {
            if (densitylimGiven) {
                plot(st[look], y[look], xlim=densitylim, ylim=ylim, type = "n", xlab = "", ylab = yname, axes = FALSE, xaxs=xaxs, yaxs=yaxs, ...)
            } else {
                plot(st[look], y[look], xlim=range(st[look], na.rm=TRUE), ylim=ylim, type = "n", xlab = "", ylab = yname, axes = FALSE, xaxs=xaxs, yaxs=yaxs, ...)
            }
            mtext(expression(paste(sigma[theta], " [ ", kg/m^3, " ]")), side = 3, line = axis.name.loc, col = col.rho, cex=par("cex"))
            axis(2)
            axis(3, col = col.rho, col.axis = col.rho, col.lab = col.rho)
            box()
            if (grid) {
                at <- par("yaxp")
                abline(h=seq(at[1], at[2], length.out=at[3]+1), col=col.grid, lty=lty.grid)
                at <- par("xaxp")
                abline(v=seq(at[1], at[2], length.out=at[3]+1), col=col.grid, lty=lty.grid)
            }
        }
        plotJustProfile(st, y, col = col.rho, type=type, lwd=lwd, cex=cex, pch=pch, keepNA=keepNA, debug=debug-1)
    } else if (xtype == "density+N2") {
        if (add)
            warning("argument 'add' is ignored for xtype=\"density+dpdt\"")
        st <- swSigmaTheta(x@data$salinity, x@data$temperature, x@data$pressure)
        look <- if (keepNA) 1:length(y) else !is.na(st) & !is.na(y)
        if (missing(densitylim))
            densitylim <- range(st, na.rm=TRUE)
        plot(st[look], y[look],
             xlim=densitylim, ylim=ylim,
             type = "n", xlab = "", ylab = yname, axes = FALSE, xaxs=xaxs, yaxs=yaxs, ...)
        axis(3, col = col.rho, col.axis = col.rho, col.lab = col.rho)
        mtext(expression(paste(sigma[theta], " [ ", kg/m^3, " ]")), side = 3, line = axis.name.loc, col = col.rho, cex=par("cex"))
        axis(2)
        box()
        if (type == 'l') lines(st, y, col = col.rho, lwd=lwd) else points(st, y, col = col.rho, pch=pch)
        par(new = TRUE)
        N2 <- swN2(x@data$pressure, st, df=df)
        if (missing(N2lim)) N2lim <- range(N2, na.rm=TRUE)
        look <- if (keepNA) 1:length(y) else !is.na(N2) & !is.na(y)
        plot(N2[look], y[look],
             xlim=N2lim, ylim=ylim,
             type = "n", xlab = "", ylab = "", axes = FALSE, lwd=lwd, xaxs=xaxs, yaxs=yaxs)
        axis(1, col = col.N2, col.axis = col.N2, col.lab = col.N2)
        if (type == 'l')
            lines(N2, y, col = col.N2, lwd=lwd) else points(N2, y, col = col.N2, pch=pch)
        mtext(expression(paste(N^2, " [ ", s^-2, " ]")), side = 1, line = axis.name.loc, col = col.N2, cex=par("cex"))
        box()
        if (grid) {
            at <- par("yaxp")
            abline(h=seq(at[1], at[2], length.out=at[3]+1), col=col.grid, lty=lty.grid)
        }
    } else if (xtype == "N2") {
        N2 <- swN2(x@data$pressure, x@data$sigmaTheta, df=df)
        if (missing(N2lim))
            N2lim <- range(N2, na.rm=TRUE)
        look <- if (keepNA) 1:length(y) else !is.na(N2) & !is.na(y)
        if (!add) {
            plot(N2[look], y[look],
                 xlim=N2lim, ylim=ylim,
                 type = "n", xlab = "", ylab = yname, axes = FALSE)
            mtext(expression(paste(N^2, " [ ", s^-2, " ]")), side = 3, line = axis.name.loc, col = col.N2, cex=par("cex"), xaxs=xaxs, yaxs=yaxs)
            axis(2)
            axis(3, col = col.N2, col.axis = col.N2, col.lab = col.N2)
            box()
            if (grid) {
                at <- par("yaxp")
                abline(h=seq(at[1], at[2], length.out=at[3]+1), col=col.grid, lty=lty.grid)
                at <- par("xaxp")
                abline(v=seq(at[1], at[2], length.out=at[3]+1), col=col.grid, lty=lty.grid)
            }
        }
        plotJustProfile(x=N2, y=y, col=col.N2, type=type, lwd=lwd, cex=cex, pch=pch, keepNA=keepNA, debug=debug-1)
    } else if (xtype == "spice") {
        spice <-swSpice(x)
        look <- if (keepNA) 1:length(y) else !is.na(spice) & !is.na(y)
        if (!add) {
            plot(spice[look], y[look],
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
        plotJustProfile(x=spice, y=y, type=type, lwd=lwd, cex=cex, pch=pch, keepNA=keepNA, debug=debug-1)
    } else if (xtype == "salinity+temperature") {
        if (add)
            warning("argument 'add' is ignored for xtype=\"density+dpdt\"")
        salinity <- if (eos == "teos") swAbsoluteSalinity(x) else x@data$salinity
        temperature <- if (eos == "teos") swConservativeTemperature(x) else x@data$temperature
        if (missing(Slim)) Slim <- range(salinity, na.rm=TRUE)
        if (missing(Tlim)) Tlim <- range(temperature, na.rm=TRUE)
        look <- if (keepNA) 1:length(y) else !is.na(temperature) & !is.na(y)
        plot(temperature[look], y[look],
             xlim=Tlim, ylim=ylim,
             type = "n", xlab = "", ylab = yname, axes = FALSE, xaxs=xaxs, yaxs=yaxs)
        axis(3, col = col.temperature, col.axis = col.temperature, col.lab = col.temperature)
        if (eos == "teos")
            mtext(resizableLabel("conservative temperature", "x"), side = 3, line=axis.name.loc, col=col.temperature, cex=par("cex"))
        else
            mtext(resizableLabel("T", "x"), side=3, line=axis.name.loc, col=col.temperature, cex=par("cex"))
        axis(2)
        box()
        lines(temperature, y, col = col.temperature, lwd=lwd)
        par(new = TRUE)
        look <- if (keepNA) 1:length(y) else !is.na(x@data$salinity) & !is.na(y)
        plot(salinity[look], y[look],
             xlim=Slim, ylim=ylim,
             type = "n", xlab = "", ylab = "", axes = FALSE, xaxs=xaxs, yaxs=yaxs)
        axis(1, col = col.salinity, col.axis = col.salinity, col.lab = col.salinity)
        if (eos == "teos")
            mtext(resizableLabel("absolute salinity", "x"), side=1, line=axis.name.loc, col=col.salinity, cex=par("cex"))
        else
            mtext(resizableLabel("S", "x"), side=1, line=axis.name.loc, col=col.salinity, cex=par("cex"))
        box()
        if (grid) {
            at <- par("yaxp")
            abline(h=seq(at[1], at[2], length.out=at[3]+1), col=col.grid, lty=lty.grid)
        }
        lines(salinity, y, col = col.salinity, lwd=if (length(lwd)>1)lwd[2] else lwd[1])
    } else {
        w <- which(names(x@data) == xtype)
        if (length(w) < 1)
            stop("unknown which value (\"", xtype, "\")")
        look <- if (keepNA) 1:length(y) else !is.na(x@data[[xtype]]) & !is.na(y)
        if (!add) {
            plot(x@data[[xtype]][look], y[look],
                 ylim=ylim,
                 type = "n", xlab="", ylab="",axes = FALSE, xaxs=xaxs, yaxs=yaxs)
            axis(3)
            mtext(resizableLabel("p"), side = 2, line = axis.name.loc, cex=par("cex"))
            label <- xtype
            if (label == "sigmaTheta")
                label <- resizableLabel("sigmaTheta", "x")
            mtext(label, side=3, line=axis.name.loc, cex=par("cex"))
            axis(2)
            box()
        }
        lines(x@data[[w]], y, lwd=lwd)
    }
}

read.ctd <- function(file, type=NULL, columns=NULL, station=NULL, monitor=FALSE, debug=getOption("oceDebug"), processingLog, ...)
{
    if (missing(processingLog)) processingLog <- paste(deparse(match.call()), sep="", collapse="")
    ofile <- file
    filename <- NULL
    if (is.null(type)) {
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
        line <- scan(file, what='char', sep="\n", n=1, quiet=TRUE) # slow, but just one line
        pushBack(line, file)
        if ("CTD" == substr(line, 1, 3))              type <- "WOCE"
        else if ("* Sea-Bird" == substr(line, 1, 10)) type <- "SBE19"
        else stop("Cannot discover type in line '", line, "'\n")
    } else {
        if (!is.na(pmatch(type, "SBE19")))            type <- "SBE19"
        else if (!is.na(pmatch(type, "WOCE")))        type <- "WOCE"
        else stop("type must be SBE19 or WOCE, not ", type)
    }                                   # FIXME: should just use oceMagic() here
    switch(type,
           SBE19 = read.ctd.sbe(file, columns=columns, station=station, monitor=monitor,
                                debug=debug, processingLog=processingLog, ...),
           WOCE  = read.ctd.woce(file, columns=columns, station=station, missing.value=-999, monitor=monitor,
                                 debug=debug, processingLog=processingLog, ...),
           ODF = read.ctd.odf(file, columns=columns, station=station, monitor=monitor,
                              debug=debug, processingLog=processingLog, ...))
}

