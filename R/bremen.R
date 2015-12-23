## vim:textwidth=128:expandtab:shiftwidth=4:softtabstop=4

setMethod(f="initialize",
          signature="bremen",
          definition=function(.Object,filename="") {
              ## Assign to some columns so they exist if needed later (even if they are NULL)
              .Object@metadata$filename <- filename
              .Object@processingLog$time <- as.POSIXct(Sys.time())
              .Object@processingLog$value <- "create 'bremen' object"
              return(.Object)
          })

setMethod(f="plot",
          signature=signature("bremen"),
          definition=function(x, type, ...) {
              names <- names(x@data)
              n <- length(names)
              if (missing(type)) {
                  if ("salinity" %in% names) plot(as.ctd(x), ...)
                  else plot(as.ladp(x), ...)
              } else {
                  if (!is.na(pmatch(type, "ctd"))) plot(as.ctd(x), ...)
                  else if (!is.na(pmatch(type, "ladp"))) plot(as.ladp(x), ...)
              }
          })

setMethod(f="summary",
          signature="bremen",
          definition=function(object, ...) {
              cat("Bremen Summary\n--------------\n\n")
              #showMetadataItem(object, "type", "Instrument: ")
              showMetadataItem(object, "model", "Instrument model:    ")
              #showMetadataItem(object, "serialNumber", "Instrument serial number:  ")
              #showMetadataItem(object, "serialNumberTemperature", "Temperature serial number:  ")
              #showMetadataItem(object, "serialNumberConductivity", "Conductivity serial number:  ")
              showMetadataItem(object, "filename", "File source:         ")
              showMetadataItem(object, "hexfilename", "Original file source (hex):  ")
              showMetadataItem(object, "institute", "Institute:           ")
              showMetadataItem(object, "scientist", "Chief scientist:     ")
              showMetadataItem(object, "date", "Date:      ", isdate=TRUE)
              showMetadataItem(object, "startTime", "Start time:          ", isdate=TRUE)
              showMetadataItem(object, "systemUploadTime", "System upload time:  ", isdate=TRUE)
              showMetadataItem(object, "cruise",  "Cruise:              ")
              showMetadataItem(object, "ship",    "Vessel:              ")
              showMetadataItem(object, "station", "Station:             ")
              showMetadataItem(object, "profile", "Profile:             ")
              cat("* Location:           ",       latlonFormat(object@metadata$latitude,
                                                               object@metadata$longitude,
                                                               digits=5), "\n")
              showMetadataItem(object, "waterDepth", "Water depth: ")
              showMetadataItem(object, "levels", "Number of levels: ")
              names <- names(object@data)
              ndata <- length(names)
              isTime <- names == "time"
              if (any(isTime))
                  cat("* Time ranges from", format(object@data$time[1]), "to", format(tail(object@data$time, 1)), "\n")
              threes <- matrix(nrow=sum(!isTime), ncol=3)
              ii <- 1
              for (i in 1:ndata) {
                  if (isTime[i])
                      next
                  threes[ii,] <- threenum(object@data[[i]])
                  ii <- ii + 1
              }
              rownames(threes) <- paste("   ", names[!isTime])
              colnames(threes) <- c("Min.", "Mean", "Max.")
              cat("* Statistics of data::\n")
              print(threes, indent='  ')
              processingLogShow(object)
          })


findInHeaderBremen <- function(key, lines)
{
    i <- grep(paste("^", key, sep=""), lines)[1] # only take first -- may be problematic
    if (length(i) < 1) "" else gsub("^.*=[ ]*", "", lines[i])
}


read.bremen <- function(file)
{
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
    res <- new("bremen")
    lines <- readLines(file)
    ## Discover header as lines starting with a letter
    headerLength <- max(grep("^[a-zA-Z]", lines))
    h <- lines[1:headerLength]
    res@metadata$filename <- filename
    res@metadata$header <- h
    lat <- strsplit(findInHeaderBremen("Latitude", h), " ")[[1]]
    latitude <- as.numeric(lat[1]) + as.numeric(lat[2]) / 60 # assume N hemi
    if (lat[3] == "S")
        latitude <- -latitude
    res@metadata$latitude <- latitude
    lon <- strsplit(findInHeaderBremen("Longitude", h), " ")[[1]]
    longitude <- as.numeric(lon[1]) + as.numeric(lon[2]) / 60 # assume N hemi
    if (lon[3] == "W")
        longitude <- -longitude
    res@metadata$longitude <- longitude
    date <- findInHeaderBremen("Date", h)
    time <- findInHeaderBremen("Time", h)
    datetime <- paste(date, " ", time, ":00", sep="")
    res@metadata$time <- as.POSIXct(datetime, tz="UTC")
    res@metadata$station <- findInHeaderBremen("Station", h)
    res@metadata$profile <- findInHeaderBremen("Profile", h)
    res@metadata$ship <- findInHeaderBremen("Shipname", h)
    res@metadata$cruise <- findInHeaderBremen("Cruise", h)
    res@metadata$scientist<- findInHeaderBremen("CruisePI", h)
    res@metadata$institute <- findInHeaderBremen("Affiliation", h)
    res@metadata$model <- findInHeaderBremen("CTD_Model", h)
    res@metadata$waterDepth <- as.numeric(findInHeaderBremen("WaterDepth", h))
    res@metadata$maxPress <- as.numeric(findInHeaderBremen("MaxPress", h))
    ## Columns have nicknames
    nicknames <- strsplit(gsub(" ", "", strsplit(h[grep("^(Columns)|(Fields)", h)], "=")[[1]][2]), ":")[[1]]
    names <- nicknames
    names[nicknames=="p"] <- "pressure"
    names[nicknames=="t"] <- "temperature"
    names[nicknames=="pt"] <- "theta"
    names[nicknames=="sth"] <- "sigmaTheta"
    names[nicknames=="s"] <- "salinity"
    names[nicknames=="o"] <- "oxygen"
    names[nicknames=="z"] <- "pressure" # NOTE: bremen files have positive z values
    ## infer column names from last line of header (guessing a bit)
    data <- read.table(text=lines[-seq.int(1, headerLength)], header=FALSE, col.names=names)
    for (name in names(data)) {
        ## FIXME: I have no idea what "uz" is, so I cannot guess the unit
        if (name == "u" || name == "v" || name == "uz" || name == "vz") {
            res@data[name] <- data[name] / 100 # velocities in cm/s
        } else if (name == "salinity" || name == "temperature") {
            #res@data[name] <- ifelse(data[[name]] == -9, NA, data[[name]])
            res@data[name] <- as.data.frame(ifelse(data[[name]] == -9, NA, data[[name]]))
           
        } else {
            res@data[name] <- data[name]
        }
    }
    res 
}

