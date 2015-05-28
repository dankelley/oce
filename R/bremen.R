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
          definition=function(x) {
              names <- names(x@data)
              n <- length(names)
              if ("salinity" %in% names) {
                  ## CTD
                  plot(as.ctd(x[["salinity"]], x[["temperature"]], x[["pressure"]]))
              } else {
                  ## assume lowered adcp
                  par(mfrow=c(1, n-1), mar=c(3, 3, 1, 1), mgp=c(2, 0.7, 0))
                  Depth <- x[["depth"]]
                  for (i in 1:n) {
                      if (names[i] != "depth") {
                          plot(x[[names[i]]], Depth, ylim=rev(range(Depth)), type='l', xlab=names[i])
                          grid()
                      }
                  }
              }
          })

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
    rval <- new("bremen")
    lines <- readLines(file)
    ## Discover header as lines starting with a letter
    headerLength <- max(grep("^[a-zA-Z]", lines))
    h <- lines[1:headerLength]
    rval@metadata$header <- h
    lat <- strsplit(strsplit(h[grep("Latitude", h)], " = ")[[1]][2], " ")[[1]]
    latitude <- as.numeric(lat[1]) + as.numeric(lat[2]) / 60 # assume N hemi
    if (lat[3] == "S")
        latitude <- -latitude
    rval@metadata$latitude <- latitude
    lon <- strsplit(strsplit(h[grep("Longitude", h)], " = ")[[1]][2], " ")[[1]]
    longitude <- as.numeric(lon[1]) + as.numeric(lon[2]) / 60 # assume N hemi
    if (lon[3] == "W")
        longitude <- -longitude
    rval@metadata$longitude <- longitude
    date <- strsplit(h[grep("^Date", h)], "=[ ]*")[[1]][2]
    time <- strsplit(h[grep("^Time", h)], "=[ ]*")[[1]][2]
    datetime <- paste(date, " ", time, ":00", sep="")
    rval@metadata$time <- as.POSIXct(datetime, tz="UTC")
    rval@metadata$station <- strsplit(h[grep("^Station", h)], "=[ ]*")[[1]][2]
    rval@metadata$profile <- strsplit(h[grep("^Profile", h)], "=[ ]*")[[1]][2]

    ## Columns have nicknames
    nicknames <- strsplit(gsub(" ", "", strsplit(h[grep("^(Columns)|(Fields)", h)], "=")[[1]][2]), ":")[[1]]
    names <- nicknames
    names[nicknames=="p"] <- "pressure"
    names[nicknames=="t"] <- "temperature"
    names[nicknames=="pt"] <- "theta"
    names[nicknames=="sth"] <- "sigmaTheta"
    names[nicknames=="s"] <- "salinity"
    names[nicknames=="o"] <- "oxygen"
    names[nicknames=="z"] <- "depth"
    ## infer column names from last line of header (guessing a bit)
    data <- read.table(text=lines[-seq.int(1, headerLength)], header=FALSE, col.names=names)
    for (name in names(data)) {
        ## FIXME: I have no idea what "uz" is, so I cannot guess the unit
        if (name == "u" || name == "v" || name == "uz" || name == "vz") {
            rval@data[name] <- data[name] / 100 # velocities in cm/s
        } else if (name == "salinity" || name == "temperature") {
            #rval@data[name] <- ifelse(data[[name]] == -9, NA, data[[name]])
            rval@data[name] <- as.data.frame(ifelse(data[[name]] == -9, NA, data[[name]]))
           
        } else {
            rval@data[name] <- data[name]
        }
    }
    rval 
}

