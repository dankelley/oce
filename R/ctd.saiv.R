# vim:textwidth=80:expandtab:shiftwidth=4:softtabstop=4

#' Read a ctd File in SAIV Format
#'
#' [read.ctd.saiv()] reads files that hold data acquired with a
#' SAIV model SD204 CTD profiler (reference 1). The function was
#' written based on examination of a particular data file, and
#' so its results ought to be treated with some caution (see
#' \dQuote{Details}).
#'
#' any user has access to a manual describing the data format, the
#' author would appreciate getting a copy.)
#'
#' The sample data provided to the author had some confusing points, including
#' the following.  If any user has access to a manual describing the file
#' format, the author would appreciate seeing a copy!
#'
#' 1. The header line that names the data columns ends with a tab,
#' indicating the presence of 12 columns (the last unnamed), but the
#' data contain only 11 columns.  Therefore, the last tab character is
#' ignored by [read.ctd.saiv()].
#'
#' 2. The test file lacked longitude and latitude information, which
#' users ought to insert later using [oceSetMetadata()], if they wish
#' to employ the TEOS-10 equation of state in their analysis.
#'
#' 3. Further to the previous point, it is not possible to compute pressure
#' accurately from depth (which is what the header suggests the file contains) unless
#' the latitude is known.  Here, it is assumed that latitude is 45 degrees
#' north, which is the default used by [swPressure()].
#'
#' 4. The author does not know what the data columns named `"Ser"`
#' and `"Meas"` represent, so they are simply added to the CTD object
#' as data variables, leaving interpretation up to the user.
#'
#' @param file a connection or a character string giving the name of
#' the file to load.
#'
#' @template encodingTemplate
#'
#' @template debugTemplate
#'
#' @param processingLog ignored.
#'
#' @param ... ignored.
#'
#' @return [read.ctd.aml()] returns a [ctd-class] object.
#'
#' @author Dan Kelley
#'
#' @references
#'
#' 1. `https://saiv.no/sd204-ctd-profiler`
#'
#' @family things related to ctd data
#' @family functions that read ctd data
read.ctd.saiv <- function(file, encoding="latin1", debug=getOption("oceDebug"), processingLog, ...)
{
    if (missing(file))
        stop("must supply 'file'")
    if (is.character(file)) {
    }
    debug <- max(0L, as.integer(debug))
    oceDebug(debug, "read.ctd.saiv(file=\"", file, "\", ...) {\n", unindent=1, style="bold")
    filename <- ""
    if (is.character(file)) {
        if (!file.exists(file))
            stop("cannot find file '", file, "'")
        if (0L == file.info(file)$size)
            stop("empty file '", file, "'")
        filename <- fullFilename(file)
        file <- file(file, "r", encoding=encoding)
        on.exit(close(file))
    }
    if (!inherits(file, "connection"))
        stop("argument `file' must be a character string or connection")
    if (!isOpen(file)) {
        open(file, "r", encoding=encoding)
        on.exit(close(file))
    }
    # From file: Tr1_all_stations	Instrument no.:	595
    # Ser	Interval (sec)	Integration	Air pressure	Salinity	Chart Datum (dbar)	
    # 4	1		1019.84	
    # Ser	Meas	Sal.	Temp	F (µg/l)	T (FTU)	Density	S. vel.	Depth(u)	Date	Time		
    # 4	584	0.02	8.221	0.09	0.56	-0.147	1440.08	0.00	10/06/2023	09:46:22
    header <- readLines(file, n=4)
    dataNamesOriginal <- strsplit(header[4], "\t")[[1]]
    # drop an empty name
    dataNamesOriginal <- dataNamesOriginal[nchar(dataNamesOriginal) > 0L]
    # compute oce-style data names
    oceDebug(debug, "Original data names: c(\"", paste(dataNamesOriginal, collapse="\", \""), "\")\n")
    dataNames <- dataNamesOriginal
    dataNames[dataNames == "Sal."] <- "salinity"
    dataNames[dataNames == "Temp"] <- "temperature"
    dataNames[dataNames == "Depth(u)"] <- "depth"
    dataNames[dataNames == "Density"] <- "density"
    dataNames[dataNames == "S. vel."] <- "soundVelocity"
    dataNames[dataNames == "T (FTU)"] <- "turbidity"
    dataNames[grep("^F [(]{1}", dataNames)] <- "fluorescence"
    oceDebug(debug, "data names: c(\"", paste(dataNames, collapse="\", \""), "\")\n")
    data <- read.delim(file, sep="\t", col.names=dataNames)
    # NOTE: pressure needs latitude for accuracy
    res <- as.ctd(salinity=data$salinity, temperature=data$temperature, pressure=swPressure(data$depth), debug=debug-1L)
    res <- oceSetMetadata(res, "header", header)
    res <- oceSetMetadata(res, "filename", filename)
    dno <- list()
    dno[dataNames] <- dataNamesOriginal
    res <- oceSetMetadata(res, "dataNamesOriginal", dno)
    res <- oceSetData(res, "fluorescence", data$fluorescence, unit=list(unit=expression(mu*g/L), scale=""))
    res <- oceSetData(res, "turbidity", data$turbidity, unit=list(unit=expression(FTU), scale=""))
    res <- oceSetData(res, "Meas", data$Meas)
    res <- oceSetData(res, "Ser", data$Ser)
    return(res)
}
