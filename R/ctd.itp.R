#' Read an ITP-type CTD File
#' @template readCtdTemplate
#'
#' @details
#' \code{read.ctd.itp()} reads files stored in ice-tethered profile format.
#'
#' @references
#' Information about ice-tethered profile data is provided at
#' \url{http://www.whoi.edu/page.do?pid=23096}, which also provides a link for
#' downloading data.  Note that the present version only handles data in
#' profiler-mode, not fixed-depth mode.
read.ctd.itp <- function(file, columns=NULL, station=NULL, missingValue, monitor=FALSE,
                         debug=getOption("oceDebug"), processingLog, ...)
{
    oceDebug(debug, "read.ctd.itp() {\n", unindent=1)
    if (is.character(file)) {
        filename <- fullFilename(file)
        file <- file(file, "r")
        on.exit(close(file))
    } else {
        filename <- ""
    }
    if (!inherits(file, "connection"))
        stop("argument `file' must be a character string or connection")
    if (!isOpen(file)) {
        open(file, "r")
        on.exit(close(file))
    }
    lines <- readLines(file, encoding="UTF-8")
    nlines <- length(lines)
    if ("%endofdat" == substr(lines[nlines], 1, 9)) {
        lines <- lines[1:(nlines-1)]
        nlines <- nlines - 1
    }
    if (nlines < 2)
        stop("file is too short; must have more than 2 lines")
    isProfile <- '%' != substr(lines[2], 1, 1)
    ## see e.g. http://www.whoi.edu/page.do?pid=125516
    if (isProfile) {
        ## %ITP 59, profile 2: year day longitude(E+) latitude(N+) ndepths
        ## 2013  247.25002   156.2163  80.3189  371
        ## %year day pressure(dbar) temperature(C) salinity oxygen(umol/kg)
        ## 2013  247.25036   18   -1.6548   30.5816  366.5573
        ## 2013  247.25043   20   -1.6523   30.7274  365.4786
        ## 2013  247.25052   22   -1.6537   31.1021  362.6732
        station <- gsub(":.*", "", gsub(".*profile[ ]*", "", lines[1]))
        d <- scan(text=lines[2], quiet=TRUE)
        year <- d[1]
        yearday <- d[2]
        longitude <- d[3]
        if (longitude < 0)
            longitude <- 360 + longitude
        latitude <- d[4]
        namesLine <- grep("^%year day", lines[1:10])
        if (1 == length(namesLine)) {
            hline <- gsub("%", "", lines[namesLine])
            tokens <- strsplit(hline, " ")[[1]]
            names <- gsub("\\(.*\\)", "", tokens)
            ## unitGiven <- grep("\\(", tokens)
            units <- list()
            for (i in seq_along(names)) {
                if (names[i] == "temperature") {
                    units[[names[i]]] <- list(unit=expression(degree*C), scale="ITS-90")
                } else if (names[i] == "salinity") {
                    units[[names[i]]] <- list(unit=expression(), scale="PSS-78")
                } else if (names[i] == "pressure") {
                    units[[names[i]]] <- list(unit=expression(dbar), scale="")
                } else if (names[i] != "year" && names[i] != "day") {
                    unit <- gsub("(.*)\\((.*)\\)", "\\2", tokens[i])
                    units[[names[i]]] <- list(unit=as.expression(unit), scale="")
                }
            }
            d <- read.table(text=lines[-seq.int(1, namesLine)], col.names=names)
            ## print(head(d), 2)
            ## print(str(units))
            pressure <- d$pressure
            salinity <- d$salinity
            temperature <- d$temperature
            oxygen <- d$oxygen
            time <- as.POSIXct(paste(d$year, "-01-01 00:00:00", sep=""), tz="UTC") + d$day * 86400
        } else {
            d <- read.table(text=lines[4:nlines])
            items <- scan(text=lines[3], what="character", quiet=TRUE)
            pcol <- grep("pressure", items)[1]
            Scol <- grep("salinity", items)[1]
            Tcol <- grep("temperature", items)[1]
            Ocol <- grep("oxygen", items)[1]
            pressure <- d[, pcol]
            temperature <- d[, Tcol]
            salinity <- d[, Scol]
            oxygen <- d[, Ocol]
            time <- NULL
        }
        ## replace any missingValue with NA
        if (!missing(missingValue) && !is.null(missingValue)) {
            pressure <- ifelse(pressure==missingValue, NA, pressure)
            temperature <- ifelse(temperature==missingValue, NA, temperature)
            salinity <- ifelse(salinity==missingValue, NA, salinity)
            oxygen <- ifelse(oxygen==missingValue, NA, oxygen)
        }
        res <- as.ctd(salinity, temperature, pressure,
                      longitude=longitude, latitude=latitude,
                      startTime=ISOdate(year, 1, 1) + yearday * 3600 * 24,
                      station=station)
        res <- oceSetData(res, name="oxygen", value=oxygen,
                          unit=expression(unit=expression(), scale=""))
        res <- oceSetMetadata(res, "filename", filename)
        res <- oceSetMetadata(res, "dataNamesOriginal",
                              list(temperature="temperature", salinity="salinity", oxygen="oxygen", pressure="pressure"))
        if (!is.null(time))
            res <- oceSetData(res, "time", time) 
    } else {
        stop("can only handle 'profile' data type, not (presumably) SAMI type")
    }
    oceDebug(debug, "} # read.ctd.itp()\n", unindent=1)
    res
}

