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
              for (b in seq_along(object@data)) {
                  dim <- dim(object@data[[b]])
                  cat(sprintf("*     band %s has dim=c(%d,%d)\n",
                              object@metadata$bands[b], dim[1], dim[2]))
              }
              cat(sprintf("* Lower left:          %fE %fN\n", object@metadata$lllon, object@metadata$lllat)) 
              cat(sprintf("* Lower right:         %fE %fN\n", object@metadata$lrlon, object@metadata$lrlat)) 
              cat(sprintf("* Upper right:         %fE %fN\n", object@metadata$urlon, object@metadata$urlat)) 
              cat(sprintf("* Upper left:          %fE %fN\n", object@metadata$ullon, object@metadata$ullat)) 
              ## do not show the data stats: calculating them is very slow
              processingLogShow(object)
          })

setMethod(f="[[",
          signature="landsat",
          definition=function(x, i, j, drop) {
              if (missing(i))
                  stop("need to give 'i', perhaps 'band'")
              if (i == "band") {
                  if (missing(j))
                      stop("need to give 'j', a band number")
                  if (is.character(j)) {
                      ## FIXME: can later add e.g. "natural" etc
                      jj <- pmatch(j, bandnames)
                      if (is.na(jj))
                          stop("band \"", j, "\" unknown; try one of: ",
                               paste(bandnames, collapse=", "), "\n")
                      j <- round(jj)
                  } else {
                      j <- round(as.numeric(j))
                  }
                  if (j < 1 || j > 11)
                      stop("band must be between 1 and 11, not ", j, " as given")
                  return(x@data[[j]])
              } else if (i %in% names(x@metadata)) {
                  return(x@metadata[[i]])
              }
              stop("can only index for bands (e.g. x[[\"band\", 8]]) or metadata (e.g. x[[\"time\"]]\n")
          })

setMethod(f="plot",
          signature=signature("landsat"),
          definition=function(x, which=1, band, decimate=1, zlim, col=oceColorsPalette,
                              debug=getOption("oceDebug"), ...)
          {
              oceDebug(debug, "plot.landsat(..., which=c(", which,
                       ", decimate=", decimate,
                       ", zlim=", if(missing(zlim)) "(missing)" else zlim,
                       ", ...) {\n", sep="", unindent=1)
              if (missing(band)) {
                  if ("panchromatic" %in% names(x@data)) {
                      oceDebug(debug, "using panchromatic\n")
                      d <- x@data$panchromatic
                      band <- "panchromatic"
                  }  else {
                      oceDebug(debug, "using band", x@metadata$bands[1], "\n")
                      d <- x@data[[1]]
                      band <- x@metadata$bands[1]
                  }
              } else {
                  if (length(band) > 1)
                      warning("only plotting first requested band\n")
                  band <- band[1]
                  if (is.character(band)) {
                      oceDebug(debug, "using band named", band, "\n")
                      d <- x[["band", band]]
                  } else {
                      oceDebug(debug, "using band", band, "\n")
                      if (length(which(x@metadata$bands == band)))
                          d <- x@data[[which(x@metadata$bands == band)]]
                      else
                          stop("[[\"band\", ", band, "]] not available; try one of: ",
                               paste(x@metadata$bands, collapse=", "), "\n",
                               call.=FALSE)
                  }
              }
              if (which == 1) {
                  dim <- dim(d)
                  if (decimate > 1) {
                      d <- d[seq(1, dim[1], by=decimate), seq(1, dim[2], by=decimate)]
                      dim <- dim(d)
                  }
                  lon <- x@metadata$lllon + seq(0, 1, length.out=dim[1]) * (x@metadata$urlon - x@metadata$lllon)
                  lat <- x@metadata$lllat + seq(0, 1, length.out=dim[2]) * (x@metadata$urlat - x@metadata$lllat)
                  asp <- 1 / cos(0.5 * (x@metadata$lllat + x@metadata$urlat) * pi / 180)
                  if (missing(zlim))
                      zlim <- quantile(d, c(0.01, 0.99), na.rm=TRUE)
                  imagep(x=lon, y=lat, z=d, asp=asp, zlim=zlim, col=col, ...)
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
    list(info=info,
         time=time, spacecraft=spacecraft,
         ullat=ullat, ullon=ullon, urlat=urlat, urlon=urlon,
         lllat=lllat, lllon=lllon, lrlat=lrlat, lrlon=lrlon,
         gridCellSizePanchromatic=gridCellSizePanchromatic,
         gridCellSizeReflective=gridCellSizeReflective,
         gridCellSizeThermal=gridCellSizeThermal)
         ##dimPanchromatic=dimPanchromatic,
         ##dimReflective=dimReflective,
         ##dimThermal=dimThermal)
}

read.landsat <- function(file, band=1:11, debug=getOption("oceDebug"))
{
    oceDebug(1+debug, "read.landsat(file=\"", file, "\", band=c(",
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
            band2[b] <- b
        }
    }
    band <- band2
    if (!require("tiff"))
        stop('must install.packages("tiff") to read landsat data')
    rval <- new("landsat")
    headerfilename <- paste(file, "/", file, "_MTL.txt", sep="")
    header <- read.landsatmeta(headerfilename, debug=debug-1)
    rval@metadata <- header
    rval@metadata[["headerfilename"]] <- headerfilename
    rval@metadata[["bands"]] <- bandnames[band]
    rval@metadata[["bandfiles"]] <- paste(file,"/",file,"_B",band,".TIF",sep="")
    for (b in seq_along(band)) {
        bandfilename <- paste(file, "/", file, "_B", b, ".TIF", sep="")
        ##rval@metadata[["filename"]] <- bandfilename 
        oceDebug(debug, "reading ", bandnames[band[b]], " in ", bandfilename, "\n", sep="")
        ## FIXME: should also handle JPG data (i.e. previews)
        d <- tiff::readTIFF(bandfilename)
        d <- t(d)
        d <- d[, seq.int(dim(d)[2], 1, -1)]
        d[d==0] <- NA
        bandname <- bandnames[band[b]]
        rval@data[[bandname]] <- d
    }
    rval@processingLog <- processingLog(rval@processingLog,
                                        paste(deparse(match.call()), sep="", collapse=""))
    oceDebug(debug, "} # read.landsat()\n")
    rval
}

landsatTrim <- function(x, ll, ur, debug=getOption("oceDebug"))
{
    if (!inherits(x, "landsat"))
        stop("method is only for landsat objects")
    oceDebug(debug, "ll:", ll$logitude, "E, ", ll$latitude, "N\n", sep="")
    oceDebug(debug, "ur:", ur$logitude, "E, ", ur$latitude, "N\n", sep="")
    if (2 != sum(c("longitude", "latitude") %in% names(ll)))
        stop("'ll' must have named items 'longitude' and 'latitude'")
    if (2 != sum(c("longitude", "latitude") %in% names(ur)))
        stop("'ur' must have named items 'longitude' and 'latitude'")
    ## Trim to box
    ll$longitude <- max(ll$longitude, x@metadata$lllon)
    ur$longitude <- min(ur$longitude, x@metadata$urlon)
    ll$latitude <- max(ll$latitude, x@metadata$lllat)
    ur$latitude <- min(ur$latitude, x@metadata$urlat)
    ## Convert lat-lon limits to i-j indices
    for (b in seq_along(x@data)) {
        oceDebug(debug, "trimming band", x@metadata$bands[b], "\n")
        dim <- dim(x@data[[b]])
        ilim <- round(c(1+(dim[1]-1)/(x@metadata$urlon-x@metadata$lllon)*(ll$longitude-x@metadata$lllon),
                        1+(dim[1]-1)/(x@metadata$urlon-x@metadata$lllon)*(ur$longitude-x@metadata$lllon)))
        ilim[1] <- max(1, ilim[1])
        ilim[2] <- min(ilim[2], dim[1])
        jlim <- round(c(1+(dim[2]-1)/(x@metadata$urlat-x@metadata$lllat)*(ll$latitude-x@metadata$lllat),
                        1+(dim[2]-1)/(x@metadata$urlat-x@metadata$lllat)*(ur$latitude-x@metadata$lllat)))
        jlim[1] <- max(1, jlim[1])
        jlim[2] <- min(jlim[2], dim[2])
        if (jlim[2] <= jlim[1] || ilim[2] <= ilim[1])
            stop("no intersection between landsat image and trimming box")
        oceDebug(debug, "  trimming i to range ", ilim[1], ":", ilim[2], ", percent range ",
                 ilim[1]/dim[1], "to", ilim[2]/dim[1], sep="", "\n")
        oceDebug(debug, "  trimming j to range ", jlim[1], ":", jlim[2], ", percent range ",
                 jlim[1]/dim[2], "to", jlim[2]/dim[2], sep="", "\n")
        x@data[[b]] <- x@data[[b]][seq.int(ilim[1], ilim[2]), seq.int(jlim[1], jlim[2])]
    }
    ## Update bounding box but FIXME: this is not quite right, owing to projections
    x@metadata$lllon <- ll$longitude
    x@metadata$ullon <- ll$longitude
    x@metadata$lrlon <- ur$longitude
    x@metadata$urlon <- ur$longitude
    x@metadata$lllat <- ll$latitude
    x@metadata$lrlat <- ll$latitude
    x@metadata$urlat <- ur$latitude
    x@metadata$ullat <- ur$latitude
    x@processingLog <- processingLog(x@processingLog,
                                     sprintf("landsatTrim(x, ll=list(longitude=%f, latitude=%f), ur=list(longitude=%f, latitude=%f))",
                                             ll$longitude, ll$latitude, ur$longitude, ur$latitude))
    x
}
#lt<-landsatTrim(landsat, list(longitude=-71, latitude=48), list(longitude=-72, latitude=48.3));dim(lt@data$band8)
