## vim:textwidth=128:expandtab:shiftwidth=4:softtabstop=4

setMethod(f="show",
          signature="landsat",
          definition=function(object) {
              cat("Landsat object, ID", object@metadata$header$landsat_scene_id, "\n")
              cat("Data (bands or calculated):\n")
              dataNames <- names(object@data)
              for (b in seq_along(dataNames)) {
                  dim <- if (is.list(object@data[[b]])) dim(object@data[[b]]$lsb) else dim(object@data[[b]])
                  cat("  \"", dataNames[b], "\" has dimension c(", dim[1], ",", dim[2], ")\n", sep='')
              }
          })

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
                      dim <- dim(d$lsb)
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


setMethod(f="[[",
          signature(x="landsat", i="ANY", j="ANY"),
          definition=function(x, i, j, drop) {
              debug <- getOption("oceDebug")
              oceDebug(debug, "landsat [[ {\n", unindent=1)
              if (missing(i))
                  stop("Must name a landsat item to retrieve, e.g. '[[\"panchromatic\"]]'", call.=FALSE)
              i <- i[1]                # drop extras if more than one given
              if (!is.character(i))
                  stop("landsat item must be specified by name", call.=FALSE)
              ## Handle cases one by one, starting with simplest.
              if (!(is.na(pmatch(i, "longitude")))) { # FIXME: ignoring decimation (may be best, anyway)
                  b1 <- x@data[[1]]
                  dim <- if (is.list(b1)) dim(b1$lsb) else dim(b1)
                  oceDebug(debug, "} # landsat [[\n", unindent=1)
                  return(x@metadata$lllon + seq(0, 1, length.out=dim[1]) * (x@metadata$urlon - x@metadata$lllon))
              }
              if (!(is.na(pmatch(i, "latitude")))) { # FIXME: ignoring decimation (may be best, anyway)
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
                      if (is.logical(j) && j) {
                          decimate <- max(as.integer(round(maxdim / 800)), 1)
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
                      d <- as.integer(x@data$tirs1$lsb[ilook,jlook])
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
                          rval <- if (is.null(dim(msb))) as.integer(lsb) else
                              256L*as.integer(msb[ilook,jlook]) + as.integer(lsb)
                          dim(rval) <- dim(lsb)
                          oceDebug(getOption("oceDebug"), "} # \"[[\"\n", unindent=1)
                          return(rval)
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
                          rval <- 256L*as.integer(msb) + as.integer(lsb)
                          dim(rval) <- dim(lsb)
                          oceDebug(debug, "} # landsat [[\n", unindent=1)
                          return(rval)
                      } else {
                          d <- d[ilook, jlook]
                          oceDebug(debug, "} # landsat [[\n", unindent=1)
                          return(d)
                      }
                  }
              }
              ## OK, no decimation is requested, so just return the desired value.
              if (isList) {
                  rval <- 256L*as.integer(msb) + as.integer(lsb)
                  dim(rval) <- dim(lsb)
                  oceDebug(debug, "} # landsat [[\n", unindent=1)
                  return(rval)
              } else {
                  oceDebug(debug, "} # landsat [[\n", unindent=1)
                  return(d)
              }
          })

setMethod(f="plot",
          signature=signature("landsat"),
          definition=function(x, band, which=1, decimate=TRUE, zlim, utm=FALSE,
                              col=oce.colorsPalette,
                              showBandName=TRUE,
                              alpha.f=1, red.f=2, green.f=2, blue.f=4,
                              offset=c(0, 0, 0, 0),
                              transform=diag(c(red.f, green.f, blue.f, alpha.f)),
                              debug=getOption("oceDebug"), ...)
          {
              oceDebug(debug, "plot.landsat(..., which=c(", paste(which, collapse=","),
                       "), decimate=", decimate,
                       ", zlim=", if(missing(zlim)) "(missing)" else zlim,
                       ", ...) {\n", sep="", unindent=1)
              terralook <- FALSE
              datanames <- names(x@data)
              spacecraft <- if (is.null(x@metadata$spacecraft)) "LANDSAT_8" else x@metadata$spacecraft
              d <- NULL
              if (which == 1) {
                  if (!missing(band) && is.character(band) && !is.na(pmatch(band, "terralook"))) {
                      terralook <- TRUE
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
                      na <- r==0 && g23==0 && nir3==0
                      ## http://terralook.cr.usgs.gov/what_is_terralook.php
                      b <- g23 - nir3
                      g <- g23 + nir3
                      g[g<0] <- 0
                      b[b<0] <- 0
                      if (spacecraft == "LANDSAT_8") {
                          oceDebug(debug, "colours for landsat 8 (range 0 to 2^16-1)\n")
                          colors <- rgb(r, g, b, maxColorValue=2^16-1)
                      } else {
                          oceDebug(debug, "colours for landsat 7 (range 0 to 2^8-1)\n")
                          colors <- rgb(r, g, b, maxColorValue=2^8-1)
                      }
                      rm(list=c("r", "g", "b")) # memory is likely tight
                      col <- unique(colors)
                      d <- array(match(colors, col), dim=dim) # method of Clark Richards
                      oceDebug(debug, "colour compaction: ",floor(prod(dim)/length(col)), '\n')
                      ## Do not NA out because then image chopped excessively;
                      ## Just leave black which is easier on the eye (although
                      ## deceptive).
                      if (FALSE) {
                          oceDebug(debug, "NA-ing out\n")
                          d[na] <- NA
                      }
                      oceDebug(debug, "adjusting colors: orig=", paste(head(col), collapse=" "), "\n")
                      col <- adjustcolor(col, alpha.f=alpha.f,
                                         red.f=red.f, green.f=green.f, blue.f=blue.f,
                                         offset=offset,
                                         transform=transform)
                      oceDebug(debug, "adjusting colors: new=", paste(head(col), collapse=" "), "\n")
                      oceDebug(debug, "finished constucting image\n")
                      ## end of band="terralook"; plot below
                  } else {
                      ## not band="terralook"
                      if (missing(band)) {
                          if ("tirs1" %in% names(x@data)) { # different meanings landsat-8 and previous
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
                              if (band == "temperature") {
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
                  asp <- 1 / cos(0.5 * (x@metadata$lllat + x@metadata$urlat) * pi / 180)
                  if (missing(zlim) && !terralook)
                      zlim <- quantile(d, c(0.01, 0.99), na.rm=TRUE)
                  if (utm) {
                      if (!("llUTM" %in% names(x@metadata))) {
                          x@metadata$llUTM <- lonlat2utm(x@metadata$lllon, x@metadata$lllat)
                          x@metadata$urUTM <- lonlat2utm(x@metadata$urlon, x@metadata$urlat, zone=x@metadata$llUTM$zone)
                      }
                      imagep(x=0.001*seq(x@metadata$llUTM$easting, x@metadata$urUTM$easting, length.out=dim[1]),
                             y=0.001*seq(x@metadata$llUTM$northing, x@metadata$urUTM$northing, length.out=dim[2]),
                             z=d, asp=1, zlim=zlim, col=col, decimate=FALSE, 
                             drawPalette=!terralook, debug=debug-1, ...)
                  } else {
                      if ("breaks" %in% names(list(...))) {
                          imagep(x=lon, y=lat, z=d, asp=asp, col=col, decimate=FALSE, 
                                 drawPalette=!terralook, debug=debug-1, ...)
                      } else {
                          imagep(x=lon, y=lat, z=d, asp=asp, zlim=zlim, col=col, decimate=FALSE, 
                                 drawPalette=!terralook, debug=debug-1, ...)
                      }
                  }
                  if (showBandName && !terralook)
                      mtext(band, side=3, adj=1, line=0, cex=1)
              } else if (which == 2) {
                  if (missing(band)) {
                      if ("tirs1" %in% names(x@data)) { # different meanings landsat-8 and previous
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
    info <- readLines(file, warn=FALSE)
    date <- getItem("DATE_ACQUIRED", numeric=FALSE)
    centerTime <- getItem("SCENE_CENTER_TIME", numeric=FALSE)
    time <- as.POSIXct(paste(date, centerTime), tz="UTC")
    spacecraft <- getItem("SPACECRAFT_ID", numeric=FALSE)
    id <- getItem("LANDSAT_SCENE_ID", numeric=FALSE)
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

read.landsat <- function(file, band="all", emissivity=0.984, debug=getOption("oceDebug"))
{
    oceDebug(debug, "read.landsat(file=\"", file, "\",",
             if (length(band) > 1) paste("band=c(\"", paste(band, collapse="\",\""), "\")", sep="") else
                 paste("band=\"", band, "\"", sep=""),
                 ", debug=", debug, ") {\n", sep="", unindent=1)
    if (!requireNamespace("tiff", quietly=TRUE))
        stop('must install.packages("tiff") to read landsat data')
    rval <- new("landsat")
    file <- gsub("/$", "", file)
    actualfilename <- gsub("/$", "", file) # permit e.g. "LE71910202005194ASN00/"
    actualfilename <- gsub(".*/", "", actualfilename)
    headerfilename <- paste(file, "/", actualfilename, "_MTL.txt", sep="")
    header <- read.landsatmeta(headerfilename, debug=debug-1)
    oceDebug(debug, "file type: ", header$spacecraft, "\n")
    ## convert to numerical bands (checks also that named bands are OK)
    bandOrig <- band
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
    rval@metadata <- header
    rval@metadata[["spacecraft"]] <- header$spacecraft
    rval@metadata[["id"]] <- header$id
    rval@metadata[["emissivity"]] <- emissivity
    rval@metadata[["filename"]] <- file
    rval@metadata[["headerfilename"]] <- headerfilename
    ## Bandnames differ by satellite.
    rval@metadata[["bands"]] <- band # FIXME: still ok?
    actualfilename <- gsub(".*/", "", file)
##    rval@metadata[["bandfiles"]] <- paste(file,"/",actualfilename,"_B",band,".TIF",sep="")
    options <- options('warn') # avoid readTIFF() warnings about geo tags
    options(warn=-1) 
    ## print(header$bandsuffices)
    for (b in seq_along(band)) {       # 'band' is numeric
        ## message("b:", b, " band: ", header$bandnames[b], " suffix: ", header$filesuffices[b])
        ##bandfilename <- paste(file, "/", actualfilename, "_B", band[b], ".TIF", sep="")
        bandfilename <- paste(file, "/", actualfilename, "_", header$filesuffices[band[b]], sep="") # FIXME: 1 more layer of indexing?
        ## message(bandfilename)
        ##rval@metadata[["filename"]] <- bandfilename 
        oceDebug(debug, "reading \"", header$bandnames[band[b]], "\" band in \"", bandfilename, "\"\n", sep="")
        ## FIXME: should also handle JPG data (i.e. previews)
        d <- tiff::readTIFF(bandfilename)
        ## if (FALSE && !is.null(getOption("testLandsat1"))) { # FIXME: disable
        bandname <- header$bandnames[band[b]] # FIXME: 1 more layer of indexing?
        #if (is.null(x@metadata$spacecraft) || x@metadata$spacecraft == "LANDSAT_7") {
        if ("LANDSAT_8" == header$spacecraft) {
            d <- .Call("landsat_numeric_to_bytes", d, 16) # reuse 'd' to try to save storage
            rval@data[[header$bandnames[band[b]]]] <- list(msb=.Call("landsat_transpose_flip", d$msb),
                                                           lsb=.Call("landsat_transpose_flip", d$lsb))
        } else {
            ## FIXME: assume all others are 1-byte, like LANDSAT_7
            d <- .Call("landsat_numeric_to_bytes", d, 8) # reuse 'd' to try to save storage
            rval@data[[header$bandnames[band[b]]]] <- list(msb=0,
                                                           lsb=.Call("landsat_transpose_flip", d$lsb))
        }
    }
    options(warn=options$warn) 
    rval@processingLog <- processingLogAppend(rval@processingLog,
                                        paste(deparse(match.call()), sep="", collapse=""))
    oceDebug(debug, "} # read.landsat()\n", unindent=1)
    rval
}

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
    rval <- x
    rval@data[[name]] <- data
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
        oceDebug(debug, "adding llUTM and urUTM to metadata\n")
        x@metadata$llUTM <- lonlat2utm(x@metadata$lllon, x@metadata$lllat, zone=x@metadata$zoneUTM)
        x@metadata$urUTM <- lonlat2utm(x@metadata$urlon, x@metadata$urlat, zone=x@metadata$zoneUTM)
    }
    llTrimUTM <- lonlat2utm(ll, zone=x@metadata$llUTM$zone)
    urTrimUTM <- lonlat2utm(ur, zone=x@metadata$llUTM$zone)
    oldEastingRange <- c(x@metadata$llUTM$easting, x@metadata$urUTM$easting) 
    trimmedEastingRange <- c(llTrimUTM$easting, urTrimUTM$easting)
    oldNorthingRange <- c(x@metadata$llUTM$northing, x@metadata$urUTM$northing) 
    trimmedNorthingRange <- c(llTrimUTM$northing, urTrimUTM$northing)
    eStart <- (trimmedEastingRange[1] - oldEastingRange[1])/(diff(oldEastingRange))
    eEnd <- (trimmedEastingRange[2] - oldEastingRange[1])/(diff(oldEastingRange))
    eStart <- min(max(eStart, 0), 1)
    eEnd <- min(max(eEnd, 0), 1)
    nStart <- (trimmedNorthingRange[1] - oldNorthingRange[1])/(diff(oldNorthingRange))
    nEnd <- (trimmedNorthingRange[2] - oldNorthingRange[1])/(diff(oldNorthingRange))
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
        ilim <- round(c(1+(dim[1]-1)/(x@metadata$urlon-x@metadata$lllon)*(ll$longitude-x@metadata$lllon),
                        1+(dim[1]-1)/(x@metadata$urlon-x@metadata$lllon)*(ur$longitude-x@metadata$lllon)))
        ilim[1] <- max(1, ilim[1])
        ilim[2] <- min(ilim[2], dim[1])
        oceDebug(debug, "ilim:", ilim[1], "to", ilim[2], "\n")
        ##? ilimUTM <- 1 + round((dim[1] - 1) * c(eStart, eEnd))
        ##? ilim <- ilimUTM # FIXME: clean up this code
        ##? oceDebug(debug, "ilimUTM:", ilimUTM[1], "to", ilimUTM[2], "\n")
        jlim <- round(c(1+(dim[2]-1)/(x@metadata$urlat-x@metadata$lllat)*(ll$latitude-x@metadata$lllat),
                        1+(dim[2]-1)/(x@metadata$urlat-x@metadata$lllat)*(ur$latitude-x@metadata$lllat)))
        jlim[1] <- max(1, jlim[1])
        jlim[2] <- min(jlim[2], dim[2])
        oceDebug(debug, "jlim:", jlim[1], "to", jlim[2], "\n")

       #browser()

        ##? jlimUTM <- 1 + round((dim[2] - 1) * c(nStart, nEnd))
        ##? jlim <- jlimUTM # FIXME: clean up this code
        ##? oceDebug(debug, "jlimUTM:", jlimUTM[1], "to", jlimUTM[2], "\n")
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
    ##? x@metadata$lllon <- ll$longitude
    ##? x@metadata$ullon <- ll$longitude
    ##? x@metadata$lrlon <- ur$longitude
    ##? x@metadata$urlon <- ur$longitude
    ##? x@metadata$lllat <- ll$latitude
    ##? x@metadata$lrlat <- ll$latitude
    ##? x@metadata$urlat <- ur$latitude
    ##? x@metadata$ullat <- ur$latitude

    oceDebug(debug, "OLD:",
            "lllon=", x@metadata$lllon,
            "lrlon=", x@metadata$lrlon,
            "ullon=", x@metadata$ullon,
            "urlon=", x@metadata$urlon, "\n        ",
            "lllat=", x@metadata$lllat,
            "lrlat=", x@metadata$lrlat,
            "ullat=", x@metadata$ullat,
            "urlat=", x@metadata$urlat, "\n")
    ## a regression saves writing messy formulae that will be hard to debug
    xx <- c(1, dim[1])
    XX <- c(x@metadata$lllon, x@metadata$urlon)
    mx <- lm(XX ~ xx)
    ppx <- predict(mx, new=data.frame(xx=ilim))
    newlllon <- newullon <- ppx[1]
    newurlon <- newlrlon <- ppx[2]
    yy <- c(1, dim[2])
    YY <- c(x@metadata$lllat, x@metadata$urlat)
    my <- lm(YY ~ yy)
    ppy <- predict(my, new=data.frame(yy=jlim))
    newlllat <- newullat <- ppy[1]
    newurlat <- newlrlat <- ppy[2]


    x@metadata$lllon <- x@metadata$ullon <- ppx[1]
    x@metadata$lrlon <- x@metadata$urlon <- ppx[2]
    x@metadata$lllat <- x@metadata$lrlat <- ppy[1]
    x@metadata$ullat <- x@metadata$urlat <- ppy[2]

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

