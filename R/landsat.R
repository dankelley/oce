## vim:textwidth=128:expandtab:shiftwidth=4:softtabstop=4

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
              cat(sprintf("* Data:                %s, which has dim=c(%d,%d)\n",
                          names(object@data), datadim[1], datadim[2]))
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
              error("no indexing yet\n")
          })

setMethod(f="plot",
          signature=signature("landsat"),
          definition=function(x, which=1, decimate=1, zlim, col=oceColorsJet,
                              debug=getOption("oceDebug"), ...)
          {
              if (which == 1) {
                  hist(x@data[[1]], xlab="Image value", main="", ...)
              } else if (which == 2) {
                  d <- x@data[[1]]
                  dim <- dim(d)
                  if (decimate > 1) {
                      d <- d[seq(1, dim[1], by=decimate), seq(1, dim[2], by=decimate)]
                      dim <- dim(d)
                  }
                  lon <- x@metadata$lllon + seq(0, 1, length.out=dim[1]) * (x@metadata$urlon - x@metadata$lllon)
                  lat <- x@metadata$lllat + seq(0, 1, length.out=dim[2]) * (x@metadata$urlat - x@metadata$lllat)
                  asp <- 1 / cos(0.5 * (x@metadata$lllat + x@metadata$urlat) * pi / 180)
                  if (!missing(zlim) && zlim == "histogram") {
                      ## Equalize the histogram, with result in matrix dd
                      dd <- as.vector(d)
                      n <- length(dd)
                      h <- hist(dd, breaks=100, plot=FALSE)   # the 100 could be altered...
                      x <- h$mids
                      y <- cumsum(h$counts)/n
                      dd <- approx(x, y, dd, rule=2)$y
                      dim(dd) <- dim
                      labels <- round(approx(y, x, seq(0, 1, 0.1), rule=2)$y, 3)
                      if (is.function(col))
                          col <- col(200)
                      if (!missing(col)) {
                          drawPalette(zlim=c(0, 1), col=col, labels=labels, at=seq(0, 1, 0.1))
                          image(lon, lat, dd, asp=asp, zlim=c(0,1), col=col, ...)
                      } else {
                          drawPalette(zlim=c(0, 1), labels=labels, at=seq(0, 1, 0.1))
                          image(lon, lat, dd, asp=asp, zlim=c(0,1), ...)
                      }
                  } else {
                      imagep(x=lon, y=lat, z=d, asp=asp, zlim=zlim, col=col, ...)
                  }
              } else {
                  stop("unknown value of 'which'")
              }
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
        oceDebug(debug, "read item", name, "\n")
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

## OLD read.landsatdata <- function(file, type=c("tiff", "jpeg"))
## OLD {
## OLD     type <- match.arg(type)
## OLD     if (type == "jpg") {
## OLD         stop("no support for jpg type")
## OLD         ##if (!require(jpeg))
## OLD         ##    stop("Need the 'jpeg' package")
## OLD         ##d <- readJPEG(file)
## OLD         ##d <- t(d)
## OLD         ##d <- d[, seq.int(dim(d)[2], 1, -1)]
## OLD     } else if (type == "tiff") {
## OLD         if (!require(tiff))
## OLD             stop("Need the 'tiff' package")
## OLD         d <- readTIFF(file)
## OLD         d <- t(d)
## OLD         d <- d[, seq.int(dim(d)[2], 1, -1)]
## OLD     } else {
## OLD         stop("internal error with filetype")
## OLD     }
## OLD     d[d==0] <- NA
## OLD     d
## OLD }

read.landsat <- function(file, band=8, debug=getOption("oceDebug"))
{
    oceDebug(debug, "read.landsat(file=\"", file, "\", band=", band, ", ...) {", sep="")
    rval <- new("landsat")
    headerfilename <- paste(file, "/", file, "_MTL.txt", sep="")
    header <- read.landsatmeta(headerfilename, debug=debug-1)
    rval@metadata <- header
    bandfilename <- paste(file, "/", file, "_B", band, ".TIF", sep="")
    rval@metadata[["headerfilename"]] <- headerfilename
    rval@metadata[["filename"]] <- bandfilename 
    oceDebug(debug, "about to read landsat data from ", bandfilename, ", which may take several moments.\n")
    ## FIXME: should also handle JPG data (i.e. previews)
    if (!require(tiff))
        stop("Need the 'tiff' package")
    d <- readTIFF(bandfilename)
    d <- t(d)
    d <- d[, seq.int(dim(d)[2], 1, -1)]
    d[d==0] <- NA
    oceDebug(debug, " done reading landsat data\n")
    bandname <- paste("band", band, sep="")
    rval@data[[bandname]] <- d
    rm(d)
    oceDebug(debug, "} # read.landsat()\n")
    rval@processingLog <- processingLog(rval@processingLog,
                                        paste(deparse(match.call()), sep="", collapse=""))
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
    dim <- dim(x@data[[1]])

    1+(dim[1]-1)/(x@metadata$urlon-x@metadata$lllon)*(ll$longitude-x@metadata$lllon)


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
    oceDebug(debug, "Trimming i to range", ilim[1], "to", ilim[2], "inclusive, or percent range",
             ilim[1]/dim[1], "to", ilim[2]/dim[1], "inclusive\n")
    oceDebug(debug, "Trimming j to range", jlim[1], "to", jlim[2], "inclusive, or percent range",
             jlim[1]/dim[2], "to", jlim[2]/dim[2], "inclusive\n")
    x@data[[1]] <- x@data[[1]][seq.int(ilim[1], ilim[2]), seq.int(jlim[1], jlim[2])]
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
