#' Read a tab-separated file from the PANGEA website [1]
#'
#' This is a preliminary version of this function, and everything
#' may change through the Autumn of 2016. Indeed, the function could
#' be deleted.
#'
#' @param file a connection or a character string giving the name of the file
#' to load.
#' @template debugTemplate
#' @author Dan Kelley
#' @references
#' 1. PANGAEA website \url{https://www.pangaea.de}
read.pangaea <- function(file, debug=getOption("oceDebug"))
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
    ## Set up original names and units, the latter blank until we know more.
    res@metadata$dataNamesOriginal <- as.list(names)
    names(res@metadata$dataNamesOriginal) <- names
    res@metadata$units <- list()
    ## Special-case items. This could be done in another function, but
    ## there doesn't seem to be much advantage to that. It might make
    ## more sense to get the original name and the unit by searching
    ## in the string, but I don't have any documentation on the format
    ## and would not be surprised to find it is variable and I prefer
    ## to start with things that work with the sample files I have on
    ## hand.
    if (length(n <- grep("^Date/Time\\s*$", names))) {
        names[n[1]] <- "time"
        res@metadata$dataNamesOriginal$time <- "Date/Time"
        res@metadata$units$time <- list(unit=expression(), scale="")
    }
    if (length(n <- grep("^Depth water \\[m\\]\\s*$", names))) {
        names[n[1]] <- "depth"
        res@metadata$dataNamesOriginal$depth <- "Depth water"
        res@metadata$units$depth <- list(unit=expression(m), scale="")
    }
    if (length(n <- grep("^Elevation \\[m\\]\\s*$", names))) {
        names[n[1]] <- "elevation" # FIXME: what is this?
        res@metadata$dataNamesOriginal$elevation <- "Elevation"
        res@metadata$units$elevation <- list(unit=expression(m), scale="")
    }
    if (length(n <- grep("^Latitude*\\s*$", names))) {
        names[n[1]] <- "latitude"
        res@metadata$dataNamesOriginal$latitude <- "Latitude"
        res@metadata$units$latitude <- list(unit=expression(degree*N), scale="")
    }
    if (length(n <- grep("^Longitude\\s*$", names))) {
        names[n[1]] <- "longitude"
        res@metadata$dataNamesOriginal$longitude <- "Longitude"
        res@metadata$units$longitude <- list(unit=expression(degree*E), scale="")
    }
    if (length(n <- grep("^O2 \\[\xc2\xb5mol/l\\]\\s*$", names, useBytes=TRUE))) { ## FIXME: what 'mu' and locale()?
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
    if (length(n <- grep("^Sal\\s*$", names))) {
        names[n[1]] <- "salinity"
        res@metadata$dataNamesOriginal$salinity <- "Sal"
        res@metadata$units$salinity <- list(unit=expression(), scale="PSS-78")
    }
    if (length(n <- grep("^Sal \\(CTD\\)\\s*$", names))) {
        names[n[1]] <- "salinity"
        res@metadata$dataNamesOriginal$salinity <- "Sal (CTD)"
        res@metadata$units$salinity <- list(unit=expression(), scale="PSS-78")
    }
    if (length(n <- grep("^Sal \\(Salinometer, inductive\\)\\s*$", names))) {
        ## Sal (Salinometer, inductive)
        names[n[1]] <- "salinitySalinometer"
        res@metadata$dataNamesOriginal$salinitySalinometer <- "Sal (Salinometer, inductive)"
        res@metadata$units$salinitySalinometer <- list(unit=expression(), scale="PSS-78")
    }
    if (length(n <- grep("^Sigma-theta \\[kg/m\\*\\*3\\]\\s*$", names))) {
        ## Sigma-theta [kg/m**3]
        names[n[1]] <- "sigmaTheta"
        res@metadata$dataNamesOriginal$sigmaTheta <- "Sigma-theta"
        res@metadata$units$sigmaTheta <- list(unit=expression(kg/m^3), scale="")
    }
    if (length(n <- grep("^Temp \\[\\xc2\\xb0C\\]\\s*$", names))) {
        ## > a<-"°"
        ## > Encoding(a)<-"bytes"
        ## > a
        ## [1] "\\xc2\\xb0"
        names[n[1]] <- "temperature"
        res@metadata$dataNamesOriginal$temperature <- "Temp"
        res@metadata$units$temperature <- list(unit=expression(degree*C), scale="ITS-90")
    }
    if (length(n <- grep("^Tpot \\[\\xc2\\xb0C\\]\\s*$", names))) {
        names[n[1]] <- "theta"
        res@metadata$dataNamesOriginal$theta <- "Tpot"
        res@metadata$units$theta <- list(unit=expression(degree*C), scale="ITS-90")
    }
    pushBack(lines, file)
    data <- read.delim(file, skip=headerEnd+1, sep="\t", header=FALSE, col.names=names)
    if ("time" %in% names)
        data$time <- as.POSIXct(data$time, tz="UTC")
    res@data <- data
    res@metadata$header <- header
    res@metadata$filename <- filename
    res@processingLog <- processingLogAppend(res@processingLog, paste("read.pangea(\"", filename, "\", ...)",sep=""))
    oceDebug(debug, "}\n", unindent=1)
    res
}

