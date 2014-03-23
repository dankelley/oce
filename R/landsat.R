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
              showMetadataItem(object, "filename",   "File source:         ")
              showMetadataItem(object, "time",       "Time:                ")
              showMetadataItem(object, "spacecraft", "Spacecraft:          ")
              ## do not show the data stats: calculating them is very slow
              processingLogShow(object)
          })

setMethod(f="[[",
          signature="landsat",
          definition=function(x, i, j, drop) {
              error("no indexing yet\n")
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
    ## Image dimensions
    l <- getItem("PANCHROMATIC_LINES")
    s <- getItem("PANCHROMATIC_SAMPLES")
    dimPanchromatic <- c(l, s)         # or reverse?
    l <- getItem("REFLECTIVE_LINES")
    s <- getItem("REFLECTIVE_SAMPLES")
    dimReflective <- c(l, s)
    l <- getItem("THERMAL_LINES")
    s <- getItem("THERMAL_SAMPLES")
    dimThermal <- c(l, s)

    list(info=info,
         time=time, spacecraft=spacecraft,
         ullat=ullat, ullon=ullon, urlat=urlat, urlon=urlon,
         lllat=lllat, lllon=lllon, lrlat=lrlat, lrlon=lrlon,
         gridCellSizePanchromatic=gridCellSizePanchromatic,
         gridCellSizeReflective=gridCellSizeReflective,
         gridCellSizeThermal=gridCellSizeThermal,
         dimPanchromatic=dimPanchromatic,
         dimReflective=dimReflective,
         dimThermal=dimThermal)
}


read.landsatdata <- function(file, type=c("tiff", "jpeg"))
{
    type <- match.arg(type)
    if (type == "jpg") {
        stop("no support for jpg type")
        ##if (!require(jpeg))
        ##    stop("Need the 'jpeg' package")
        ##d <- readJPEG(file)
        ##d <- t(d)
        ##d <- d[, seq.int(dim(d)[2], 1, -1)]
    } else if (type == "tiff") {
        if (!require(tiff))
            stop("Need the 'tiff' package")
        d <- readTIFF(file)
        d <- t(d)
        d <- d[, seq.int(dim(d)[2], 1, -1)]
    } else {
        stop("internal error with filetype")
    }
    d[d==0] <- NA
    d
}

read.landsat <- function(file, band=8, debug=getOption("oceDebug"))
{
    oceDebug(debug, "read.landsat(file=\"", file, "\", band=", band, ", ...) {", sep="")
    rval <- new("landsat")
    rval@metadata <- read.landsatmeta(paste(file, "/", file, "_MTL.txt", sep=""),
                                      debug=debug-1)
    oceDebug(debug, "about to read landsat data (may take a while!)\n")
    bandfile <- paste(file, "/", file, "_B", band, ".TIF", sep="")
    ##band <- read.landsatdata(paste(file, "/", file, "_B", band, ".TIF", sep=""))

    ## possibly calling out to a function slows things by copying
    if (!require(tiff))
        stop("Need the 'tiff' package")
    d <- readTIFF(bandfile)
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

