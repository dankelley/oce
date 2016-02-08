## vim: tw=120 shiftwidth=4 softtabstop=4 expandtab:

read.observatory <- function(file, 
                    type=c("ctd"),
                    debug=getOption("oceDebug"), processingLog, ...)
{
    if (debug > 2) debug <- 2
    if (debug < 0) debug  <- 0
    oceDebug(debug, "read.observatory(file=\"",file, "\", ...) {\n", sep="", unindent=1)
    type <- match.arg(type)
    if (type == "ctd") {
        read.observatory.ctd(file=file, 
                   debug=debug-1, processingLog=processingLog, ...)
    } else {
        stop("unknown type of observatory data")
    }
}

read.observatory.ctd <- function(file, 
                                 debug=getOption("oceDebug"), processingLog, ...)
{
    if (debug > 1)
        debug <- 1
    oceDebug(debug, "read.observatory.ctd(file=\"",file,
              "\", ...) {\n", sep="", unindent=1)
    if (is.character(file)) {
        filename <- fullFilename(file)
        file <- file(file, "r")
        on.exit(close(file))
    }
    if (!inherits(file, "connection"))
        stop("argument `file' must be a character string or connection")
    if (!isOpen(file)) {
        filename <- "(connection)"
        open(file, "rb")
        on.exit(close(file))
    }
    lines <- readLines(file)
    location <- lines[grep("Latitude", lines)]
    latitude <- as.numeric(gsub(" .*$", "", gsub("^.*Latitude\\(decdeg N\\): ", "", location)))
    longitude <- -as.numeric(gsub(" .*$", "", gsub("^.*Longitude\\(decdeg W\\): ", "", location)))
    waterDepth <- as.numeric(gsub("^.*Depth\\(m\\): ", "", location)) ## FIXME: is this actually on that line?
    check <- min(100, length(lines))
    headerStartEnd <- grep('^%', lines[1:check])
    d <- read.csv(text=lines[-(1:headerStartEnd[2])])
    colNames <- strsplit(lines[headerStartEnd[2]+1], "[ ]*,[ ]*")[[1]] 
    time <- strptime(d[,1], "%Y-%m-%dT%H:%M:%S",tz="UTC")
    Scol <- grep("^Practical Salinity \\(psu\\)", colNames)
    Tcol <- grep("^Temperature \\(C\\)", colNames)
    pcol <- grep("^Pressure \\(decibar\\)", colNames)
    salinity <- d[,Scol]
    temperature <- d[,Tcol]
    pressure  <- d[,pcol]
    res <- as.ctd(salinity=salinity, temperature=temperature, pressure=pressure,
                  latitude=latitude, longitude=longitude, waterDepth=waterDepth,
                  other=list(time=time))
    res@metadata$filename <- filename
    oceDebug(debug, "} # read.observatory.ctd()\n", unindent=1)
    res
}

