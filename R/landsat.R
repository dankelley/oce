## vim:textwidth=128:expandtab:shiftwidth=4:softtabstop=4

bandnames <-c("aerosol", "blue", "green", "red",
              "nir", "swir1", "swir2",
              "panchromatic",
              "cirrus",
              "tirs1", "tirs2")

setMethod(f="initialize",
          signature="landsat",
          definition=function(.Object,filename="") {
              .Object@metadata$filename <- filename
              .Object@processingLog$time <- as.POSIXct(Sys.time())
              .Object@processingLog$value <- "create 'landsat' object"
              return(.Object)
          })

setMethod(f="summary",
          signature="landsat",
          definition=function(object, ...) {
              cat("Landsat Summary\n---------------\n\n")
              showMetadataItem(object, "filename",   "Data file:           ")
              showMetadataItem(object, "time",       "Time:                ")
              showMetadataItem(object, "spacecraft", "Spacecraft:          ")
              cat(sprintf("* Header file:         %s\n", object@metadata$headerfilename))
              datadim <- dim(object@data[[1]])
              cat(sprintf("* Data:\n"))
              datanames <- names(object@data)
              for (b in seq_along(object@data)) {
                  d <- object@data[[b]]
                  if (is.list(d)) {
                      dim <- dim(d$msb)
                      cat(sprintf("*     band %s has dim=c(%d,%d)\n",
                                  datanames[b], dim[1], dim[2]))
                  } else {
                  }
              }

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
              ## do not show the data stats: calculating them is very slow
              processingLogShow(object)
          })


setMethod(f="[[", # FIXME: ensure working on all the many possibilities, including user-created (not broken by byte??)
          signature="landsat",
          definition=function(x, i, j, drop) {
              if (missing(i))
                  stop("Must name a landsat item to retrieve, e.g. '[[\"panchromatic\"]]'", call.=FALSE)
              i <- i[1]                # drop extras if more than one given
              if (is.numeric(i)) {
                  if (is.list(x@data[[i]])) {
                      rval <- (256L*as.integer(x@data[[i]]$msb) + as.integer(x@data[[i]]$lsb))
                      dim(rval) <- dim(x@data[[i]]$msb)
                      return(rval)
                  } else {
                      return(x@data[[i]])
                  }
              }
              datanames <- names(x@data) # user may have added items
              if (!is.na(ii <- pmatch(i, datanames))) {
                  b <- x@data[[datanames[ii]]]
                  rval <- 256L*as.integer(b$msb) + as.integer(b$lsb)
                  dim(rval) <- dim(b$msb)
                  return(rval)
              }
              if (i == "band") {
                  if (missing(j))
                      stop("Must give a landsat band number or name", call.=FALSE)
                  if (is.character(j)) {
                      ## FIXME: can later add e.g. "temperature" etc
                      jj <- pmatch(j, datanames)
                      if (is.na(jj)) {
                          stop("Landsat band \"", j, "\" is not in this object; try one of: ",
                               paste(datanames, collapse=", "), "\n", call.=FALSE)
                      }
                      j <- round(jj)
                  } else {
                      ## Numeric only works with satellite-supplied bands (not bands added by user)
                      j <- round(as.numeric(j))
                      if (1 <= j && j <= length(bandnames))
                          warning("Hint: access landsat bands as e.g. [[\"", bandnames[j], "\"]]", call.=FALSE)
                  }
                  if (j < 1 || j > length(bandnames))
                      stop("Landsat band must be between 1 and ", length(bandnames), ", not ", j, " as given", call.=FALSE)
                  rval <- x@data[[datanames[j]]]
                  if (is.null(rval))
                      stop("No landsat band \"", datanames[j], "\" in this object", call.=FALSE)
                  return(rval)
              } else {
                  if (is.numeric(i)) {
                      nband <- floor(length(x@data) / 2)
                      if (i > nband)
                          stop("no landsat band numbered ", i, "; maximum allowed number is ", nband, call.=FALSE)
                      ii <- floor(i / 2 + 1)
                      rval <- (256L*as.integer(x@data[[ii]]) + as.integer(x@data[[ii+1]]))
                      dim(rval) <- dim(x@data[[ii]])
                      return(rval)
                  }
                  if (!is.na(ii <- pmatch(i, bandnames))) {
                      theband <- bandnames[ii[1]]
                      if (!(theband %in% datanames))
                          stop("This landsat object does not contain the band named \"", bandnames[ii[1]],
                               "\"; the available data are named: ", paste(datanames, collapse=", "), call.=FALSE)
                      dim <- dim(x@data[[theband]]$msb)
                      rval <- 256L * as.integer(x@data[[theband]]$msb) + as.integer(x@data[[theband]]$lsb)
                      dim(rval) <- dim
                      return(rval)
                  } else if (!is.na(ii <- pmatch(i, datanames))) {
                      rval <- x@data[[datanames[ii[1]]]]
                      if (is.null(rval))
                          stop("No landsat band \"", datanames[ii[1]], "\" in this object", call.=FALSE)
                      return(rval)
                  } else if (i %in% names(x@metadata)) {
                      return(x@metadata[[i]])
                  } else {
                      stop("No item named \"", i, "\" in this landsat object", call.=FALSE)
                  }
                  rval <- x@data[[j]]
                  if (is.null(rval))
                      stop("No data for requested Landsat band", call.=FALSE)
                  return(x@data[[j]])
              }
              stop("Landsat indexing is only for bands (e.g. x[[\"band\", 8]]) or metadata (e.g. x[[\"time\"]]\n", call.=FALSE)
          })

setMethod(f="plot",
          signature=signature("landsat"),
          definition=function(x, which=1, band, decimate, zlim, utm=FALSE,
                              col=oceColorsPalette, debug=getOption("oceDebug"), ...)
          {
              oceDebug(debug, "plot.landsat(..., which=c(", which,
                       ", decimate=", if (missing(decimate)) "(missing)" else decimate,
                       ", zlim=", if(missing(zlim)) "(missing)" else zlim,
                       ", ...) {\n", sep="", unindent=1)
              if (missing(band)) {
                  if ("panchromatic" %in% names(x@data)) {
                      oceDebug(debug, "using panchromatic\n")
                      d <- x[["panchromatic"]]
                      band <- "panchromatic"
                  }  else {
                      oceDebug(debug, "using band", x@metadata$bands[1], "\n")
                      d <- x[[1]]
                      band <- x@metadata$bands[1] # FIXME: would prefer to get band name from names()
                  }
              } else {
                  if (length(band) > 1) warning("only plotting first requested band\n")
                  d <- x[[band[1]]]
              }
              d[d == 0] <- NA
              if (which == 1) {
                  dim <- dim(d)
                  if (missing(decimate)) {
                      if (prod(dim) > 1000000L) {
                          max <- max(dim)
                          decimate <- as.integer(floor(max / 800))
                          warning("Auto-decimating landsat image with decimate=", decimate, ", since it has > 1e6 pixels",
                                  call.=FALSE)
                      } else {
                          decimate <- 1
                      }
                  }
                  if (decimate > 1) {
                      d <- d[seq(1, dim[1], by=decimate), seq(1, dim[2], by=decimate)]
                      dim <- dim(d)
                  }
                  lon <- x@metadata$lllon + seq(0, 1, length.out=dim[1]) * (x@metadata$urlon - x@metadata$lllon)
                  lat <- x@metadata$lllat + seq(0, 1, length.out=dim[2]) * (x@metadata$urlat - x@metadata$lllat)
                  asp <- 1 / cos(0.5 * (x@metadata$lllat + x@metadata$urlat) * pi / 180)
                  if (missing(zlim))
                      zlim <- quantile(d, c(0.01, 0.99), na.rm=TRUE)
                  if (utm) {
                      if (!("llUTM" %in% names(x@metadata))) {
                          x@metadata$llUTM <- lonlat2utm(x@metadata$lllon, x@metadata$lllat)
                          x@metadata$urUTM <- lonlat2utm(x@metadata$urlon, x@metadata$urlat, zone=x@metadata$llUTM$zone)
                      }
                      imagep(x=0.001*seq(x@metadata$llUTM$easting, x@metadata$urUTM$easting, length.out=dim[1]),
                             y=0.001*seq(x@metadata$llUTM$northing, x@metadata$urUTM$northing, length.out=dim[2]),
                             z=d, asp=1, zlim=zlim, col=col, ...)
                  } else {
                      imagep(x=lon, y=lat, z=d, asp=asp, zlim=zlim, col=col, ...)
                  }
                  mtext(band, side=3, adj=1, line=0, cex=1)
              } else if (which == 2) {
                  hist(d, xlab="Image value", main="", ...)
              } else {
                  stop("unknown value of 'which'")
              }
              oceDebug(debug, "} # plot.landsat()\n", unindent=1)
          })


read.landsatmeta <- function(file, debug=getOption("oceDebug"))
{
    getItem <- function(name, numeric=TRUE)
    {
        line <- grep(paste("^[ ]*", name, "[ ]*=[ ]*", sep=""), info)
        rval <- NULL
        if (length(line)) {
            rval <- strsplit(info[line[1]], "=")[[1]][2]
            rval <- gsub("^[ ]+", "", rval)
            rval <- gsub("[ ]+$", "", rval)
        }
        rval <- if (numeric) as.numeric(rval) else gsub("\"", "", rval)
        ##oceDebug(debug, "read item", name, "\n")
        rval
    }
    info <- readLines(file)
    date <- getItem("DATE_ACQUIRED", numeric=FALSE)
    centerTime <- getItem("SCENE_CENTER_TIME", numeric=FALSE)
    time <- as.POSIXct(paste(date, centerTime), tz="UTC")
    spacecraft <- getItem("SPACECRAFT_ID", numeric=FALSE)
    ## Bounding region (not a latlon box!)
    ullat <- getItem("CORNER_UL_LAT_PRODUCT")
    ullon <- getItem("CORNER_UL_LON_PRODUCT")
    urlat <- getItem("CORNER_UR_LAT_PRODUCT")
    urlon <- getItem("CORNER_UR_LON_PRODUCT")
    lllat <- getItem("CORNER_LL_LAT_PRODUCT")
    lllon <- getItem("CORNER_LL_LON_PRODUCT")
    lrlat <- getItem("CORNER_LR_LAT_PRODUCT")
    lrlon <- getItem("CORNER_LR_LON_PRODUCT")
    zoneUTM <- getItem("UTM_ZONE")
    llUTM <- list(easting=getItem("CORNER_LL_PROJECTION_X_PRODUCT"),
                  northing=getItem("CORNER_LL_PROJECTION_Y_PRODUCT"),
                  zone=zoneUTM)
    urUTM <- list(easting=getItem("CORNER_UR_PROJECTION_X_PRODUCT"),
                  northing=getItem("CORNER_UR_PROJECTION_Y_PRODUCT"),
                  zone=zoneUTM)
    ## Cell sizes
    gridCellSizePanchromatic <- getItem("GRID_CELL_SIZE_PANCHROMATIC")
    gridCellSizeReflective <- getItem("GRID_CELL_SIZE_REFLECTIVE")
    gridCellSizeThermal <- getItem("GRID_CELL_SIZE_THERMAL")                            
    ## ## Image dimensions
    ## l <- getItem("PANCHROMATIC_LINES")
    ## s <- getItem("PANCHROMATIC_SAMPLES")
    ## dimPanchromatic <- c(l, s)         # or reverse?
    ## l <- getItem("REFLECTIVE_LINES")
    ## s <- getItem("REFLECTIVE_SAMPLES")
    ## dimReflective <- c(l, s)
    ## l <- getItem("THERMAL_LINES")
    ## s <- getItem("THERMAL_SAMPLES")
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
    list(header=header,
         time=time, spacecraft=spacecraft,
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

read.landsat <- function(file, band=1:11, debug=getOption("oceDebug"))
{
    oceDebug(debug, "read.landsat(file=\"", file, "\", band=c(",
             paste(band, collapse=","), "), debug=", debug, ") {\n", sep="", unindent=1)
    ## convert to numerical bands (checks also that named bands are OK)
    band2 <- rep(NA, length(band))
    for (b in seq_along(band)) {
        if (is.character(band[b])) {
            m <- pmatch(band[b], bandnames, nomatch=0)
            if (0 == m)
                stop('band "', band[b], '" unknown; must be one of: ', paste(bandnames, collapse=", "))
            else
                band2[b] <- m
        } else {
            band2[b] <- band[b]
        }
    }
    band <- band2
    if (!require("tiff"))
        stop('must install.packages("tiff") to read landsat data')
    rval <- new("landsat")
    actualfilename <- gsub(".*/", "", file)
    headerfilename <- paste(file, "/", actualfilename, "_MTL.txt", sep="")
    header <- read.landsatmeta(headerfilename, debug=debug-1)
    rval@metadata <- header
    rval@metadata[["headerfilename"]] <- headerfilename
    rval@metadata[["bands"]] <- bandnames[band]
    actualfilename <- gsub(".*/", "", file)
    rval@metadata[["bandfiles"]] <- paste(file,"/",actualfilename,"_B",band,".TIF",sep="")
    options <- options('warn') # avoid readTIFF() warnings about geo tags
    options(warn=-1) 
    for (b in seq_along(band)) {
        bandfilename <- paste(file, "/", actualfilename, "_B", band[b], ".TIF", sep="")
        ##rval@metadata[["filename"]] <- bandfilename 
        oceDebug(debug, "reading \"", bandnames[band[b]], "\" band in \"", bandfilename, "\"\n", sep="")
        ## FIXME: should also handle JPG data (i.e. previews)
        d <- tiff::readTIFF(bandfilename)
        ## if (FALSE && !is.null(getOption("testLandsat1"))) { # FIXME: disable
        bandname <- bandnames[band[b]]
        d <- .Call("landsat_numeric_to_bytes", d) # reuse 'd' to try to save storage
        rval@data[[bandname]] <- list(msb=.Call("landsat_transpose_flip", d$msb),
                                      lsb=.Call("landsat_transpose_flip", d$lsb))
        ## 2014-07-10  d <- t(d)
        ## 2014-07-10  d <- d[, seq.int(dim(d)[2], 1, -1)]
        ## 2014-07-10  d[d==0] <- NA                  # FIXME: move to transpose_flip *if* it's right
        ## 2014-07-10  rval@data[[bandname]] <- d
    }
    options(warn=options$warn) 
    rval@processingLog <- processingLog(rval@processingLog,
                                        paste(deparse(match.call()), sep="", collapse=""))
    oceDebug(debug, "} # read.landsat()\n", unindent=1)
    rval
}

landsatTrim <- function(x, ll, ur, debug=getOption("oceDebug"))
{
    if (!inherits(x, "landsat"))
        stop("method is only for landsat objects")
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
    utm <- TRUE                        # FIXME: make this an arg
    if (!("llUTM" %in% names(x@metadata))) {
        x@metadata$llUTM <- lonlat2utm(x@metadata$lllon, x@metadata$lllat, zone=x@metadata$zoneUTM)
        x@metadata$urUTM <- lonlat2utm(x@metadata$urlon, x@metadata$urlat, zone=x@metadata$zoneUTM)
    }
    llTrimUTM <- lonlat2utm(ll, zone=x@metadata$llUTM$zone)
    urTrimUTM <- lonlat2utm(ur, zone=x@metadata$llUTM$zone)
    oldEastingRange <- c(x@metadata$llUTM$easting, x@metadata$urUTM$easting) 
    oldNorthingRange <- c(x@metadata$llUTM$northing, x@metadata$urUTM$northing) 
    trimmedEastingRange <- c(llTrimUTM$easting, urTrimUTM$easting)
    trimmedNorthingRange <- c(llTrimUTM$northing, urTrimUTM$northing)
    eStart <- (trimmedEastingRange[1] - oldEastingRange[1])/(diff(oldEastingRange))
    eEnd <- (trimmedEastingRange[2] - oldEastingRange[1])/(diff(oldEastingRange))
    eStart <- min(max(eStart, 0), 1)
    eEnd <- min(max(eEnd, 0), 1)
    #if (eStart < 0 || eStart > 1) stop("internal error trimming (eStart)")
    #if (eEnd < 0 || eEnd > 1) stop("internal error trimming (eEnd)")
    nStart <- (trimmedNorthingRange[1] - oldNorthingRange[1])/(diff(oldNorthingRange))
    nEnd <- (trimmedNorthingRange[2] - oldNorthingRange[1])/(diff(oldNorthingRange))
    nStart <- min(max(nStart, 0), 1)
    nEnd <- min(max(nEnd, 0), 1)
    #if (nStart < 0 || nStart > 1) stop("internal error trimming (nStart)")
    #if (nEnd < 0 || nEnd > 1) stop("internal error trimming (nEnd)")

    oceDebug(debug, "Easting trim range:", eStart, "to", eEnd, "\n")
    oceDebug(debug, "Northing trim range:", nStart, "to", nEnd, "\n")


    ## Convert lat-lon limits to i-j indices

    for (b in seq_along(x@data)) {
        oceDebug(debug, "Trimming band", x@metadata$bands[b], "\n")
        isList <- is.list(x@data[[b]])
        dim <- if (isList) dim(x@data[[b]]$msb) else dim(x@data[[b]])
        ilim <- round(c(1+(dim[1]-1)/(x@metadata$urlon-x@metadata$lllon)*(ll$longitude-x@metadata$lllon),
                        1+(dim[1]-1)/(x@metadata$urlon-x@metadata$lllon)*(ur$longitude-x@metadata$lllon)))
        ilim[1] <- max(1, ilim[1])
        ilim[2] <- min(ilim[2], dim[1])
        oceDebug(debug, "ilim:", ilim[1], "to", ilim[2], "\n")
        ilimUTM <- 1 + round((dim[1] - 1) * c(eStart, eEnd))
        ilim <- ilimUTM # FIXME: clean up this code
        oceDebug(debug, "ilimUTM:", ilimUTM[1], "to", ilimUTM[2], "\n")
        jlim <- round(c(1+(dim[2]-1)/(x@metadata$urlat-x@metadata$lllat)*(ll$latitude-x@metadata$lllat),
                        1+(dim[2]-1)/(x@metadata$urlat-x@metadata$lllat)*(ur$latitude-x@metadata$lllat)))
        jlim[1] <- max(1, jlim[1])
        jlim[2] <- min(jlim[2], dim[2])
        oceDebug(debug, "jlim:", jlim[1], "to", jlim[2], "\n")
        jlimUTM <- 1 + round((dim[2] - 1) * c(nStart, nEnd))
        oceDebug(debug, "jlimUTM:", jlimUTM[1], "to", jlimUTM[2], "\n")
        jlim <- jlimUTM # FIXME: clean up this code
        if (jlim[2] <= jlim[1] || ilim[2] <= ilim[1])
            stop("no intersection between landsat image and trimming box")
        oceDebug(debug, "  trimming i to range ", ilim[1], ":", ilim[2], ", percent range ",
                 ilim[1]/dim[1], " to ", ilim[2]/dim[1], sep="", "\n")
        oceDebug(debug, "  trimming j to range ", jlim[1], ":", jlim[2], ", percent range ",
                 jlim[1]/dim[2], " to ", jlim[2]/dim[2], sep="", "\n")
        if (isList) {
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
            "urlon=", x@metadata$urlon,
            "lllat=", x@metadata$lllat,
            "lrlat=", x@metadata$lrlat,
            "ullat=", x@metadata$ullat,
            "urlat=", x@metadata$urlat, "\n")
    x@metadata$llUTM <- llTrimUTM
    x@metadata$urUTM <- urTrimUTM
    llE <- llTrimUTM$easting
    llN <- llTrimUTM$northing
    urE <- urTrimUTM$easting
    urN <- urTrimUTM$northing
    zone <- llTrimUTM$zone
    hemisphere <- llTrimUTM$hemisphere
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
            "urlon=", x@metadata$urlon,
            "lllat=", x@metadata$lllat,
            "lrlat=", x@metadata$lrlat,
            "ullat=", x@metadata$ullat,
            "urlat=", x@metadata$urlat, "\n")
 
    x@processingLog <- processingLog(x@processingLog,
                                     sprintf("landsatTrim(x, ll=list(longitude=%f, latitude=%f), ur=list(longitude=%f, latitude=%f))",
                                             ll$longitude, ll$latitude, ur$longitude, ur$latitude))
    x
}

