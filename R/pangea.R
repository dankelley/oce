read.pangea <- function(file, debug=getOption("oceDebug"))
{
    oceDebug(debug, "read.pangea() {\n", unindent=1)
    filename <- NULL
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
    res <- new("oce")
    lines <- readLines(file)
    headerEnd <- grep("^\\*/$", lines)
    if (!length(headerEnd))
        stop("cannot find header")
    if (length(headerEnd) > 1)
        stop("cannot decode header")
    headerEnd <- headerEnd[1]
    oceDebug(debug, "header has", headerEnd, "lines")
    header <- lines[seq.int(1L, headerEnd)]
    names <- strsplit(lines[headerEnd+1], '\t')[[1]]
    print(names)
    res@metadata$dataNamesOriginal <- list()
    res@metadata$units <- list()

    if (length(n <- grep("^Date/Time*$", names))) {
        names[n[1]] <- "time"
        res@metadata$dataNamesOriginal$time <- "Date/Time"
        res@metadata$units$time <- list(unit=expression(), scale="")
    }
    if (length(n <- grep("^Latitude*$", names))) {
        names[n[1]] <- "latitude"
        res@metadata$dataNamesOriginal$latitude <- "Latitude"
        res@metadata$units$latitude <- list(unit=expression(degree*N), scale="")
        warning("guessing that latitude is in degree*N")
    }
    if (length(n <- grep("^Longitude*$", names))) {
        names[n[1]] <- "longitude"
        res@metadata$dataNamesOriginal$longitude <- "Longitude"
        res@metadata$units$longitude <- list(unit=expression(degree*E), scale="")
        warning("guessing that longitude is in degree*E")
    }
    if (length(n <- grep("^O2 \\[\xc2\xb5mol/l\\]*$", names, useBytes=TRUE))) { ## FIXME: what 'mu' and locale()?
        ##if (length(n <- grep("^O2 \\[µmol/l\\]*$", names))) { ## FIXME: what 'mu' and locale()?
        ## O2 [µmol/l]
        ##   > a<-"µ"
        ##   > Encoding(a)<-"bytes"
        ##   > a
        ##   [1] "\\xc2\\xb5"
        names[n[1]] <- "oxygen"
        res@metadata$dataNamesOriginal$oxygen <- "O2"
        res@metadata$units$oxygen <- list(unit=expression(mu*mol/l), scale="")
    }
    if (length(n <- grep("^Press \\[dbar\\]\\s*$", names))) {
        names[n[1]] <- "pressure"
        res@metadata$dataNamesOriginal$pressure <- "Press"
        res@metadata$units$pressure <- list(unit=expression(dbar), scale="")
    }
    if (length(n <- grep("^Sal.*$", names))) {
        names[n[1]] <- "salinity"
        res@metadata$dataNamesOriginal$salinity <- "Sal"
        res@metadata$units$salinity <- list(unit=expression(), scale="PSS-78")
        warning("guessing that salinity is PSS-78")
    }
    if (length(n <- grep("^Temp.*$", names))) {
        names[n[1]] <- "temperature"
        res@metadata$dataNamesOriginal$temperature <- "Temp"
        res@metadata$units$temperature <- list(unit=expression(degree*C), scale="ITS-90")
        warning("guessing that temperature is in ITS-90")
    }
    pushBack(lines, file)
    data <- read.delim(file, skip=headerEnd+1, sep="\t", header=FALSE, col.names=names)
    if ("time" %in% names)
        data$time <- as.POSIXct(data$time, tz="UTC")
    res@data <- data
    res@metadata$header <- header
    res@metadata$filename <- filename
    res@processingLog <- processingLogAppend(res@processingLog, paste(deparse(match.call()), sep="", collapse=""))
    warning("FIXME: interpret column names and units")
    oceDebug(debug, "}\n", unindent=1)
    res
}

