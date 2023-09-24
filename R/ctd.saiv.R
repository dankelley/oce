# vim:textwidth=80:expandtab:shiftwidth=4:softtabstop=4

#' Read a ctd File in SAIV Format
#'
#' [read.ctd.saiv()] reads files that hold data acquired with a
#' SAIV model SD204 CTD profiler (reference 1). Since no
#' documentation on the format was available to the author,
#' this function was written based on examination of a particular
#' data file.  This almost certainly will yield limitations
#' for other files, in particular for those with data names
#' that differ from those in the sample file (see
#' \dQuote{Details} for this and other limitations).
#'
#' Some quantities are renamed to match the oce convention, e.g.
#' `"Sal."` is renamed to `"salinity"`, `"Temp"` is renamed to
#' `"temperature"`, etc.  Note that the sample file upon which
#' the code was based did not list units for several quantities,
#' This problem is addressed in a risky way:
#' [read.ctd.saiv()] simply assumes that common units are
#' used in the file.  This is not the only problem with
#' the sample file, and thus with this version of the function.
#' Some other issues encountered with the sample file are as
#' follows.
#'
#' 1. The header line that names the data columns ends with a tab,
#' indicating the presence of 12 columns (the last unnamed), but the
#' data contain only 11 columns.  Therefore, the last tab character is
#' ignored by [read.ctd.saiv()].
#'
#' 2. The test file lacked longitude and latitude information.  This
#' means that modern quantities like Absolute Salinity and Conservative
#' Temperature cannot be computed.  Users who know the location information
#' ought to insert values into the object returned by [read.ctd.saiv()]
#' using [oceSetMetadata()].
#'
#' 3. Further to the previous point, it is not possible to compute pressure
#' accurately from depth (which is what the header suggests the file contains) unless
#' the latitude is known. In [read.ctd.saiv()], latitude is assumed to be 45 degrees
#' north, which is the default used by [swPressure()].
#'
#' 4. The data columns named `"Ser"` and `"Meas"` are inserted into
#' the return value without renaming.  This is true of other quantities
#' as well.  Since `[[` can access by *original* name and not just by
#' oce name, this may not be too much of a problem.
#'
#' @param file a character string naming the file to be read.
#'
#' @template encodingTemplate
#'
#' @template debugTemplate
#'
#' @param processingLog ignored.
#'
#' @param ... ignored.
#'
#' @return [read.ctd.saiv()] returns a [ctd-class] object.
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
    debug <- max(0L, as.integer(debug))
    oceDebug(debug, "read.ctd.saiv(file=\"", file, "\", ...) {\n", unindent=1, style="bold")
    if (!is.character(file))
        stop("'file' must be a character value")
    filename <- ""
    # From file: Tr1_all_stations	Instrument no.:	595
    # Ser	Interval (sec)	Integration	Air pressure	Salinity	Chart Datum (dbar)
    # 4	1		1019.84
    # Ser	Meas	Sal.	Temp	F (µg/l)	T (FTU)	Density	S. vel.	Depth(u)	Date	Time
    # 4	584	0.02	8.221	0.09	0.56	-0.147	1440.08	0.00	10/06/2023	09:46:22
    header <- readLines(file, n=4L, encoding=encoding)
    if (debug > 0L) {
        cat("header is:\n")
        print(header)
    }
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
    data <- read.delim(file, skip=4L, sep="\t", col.names=dataNames, encoding=encoding)
    # FIXME it doesn't find first line, but if I skip 3 lines, then
    # it sees a problem in number of columns.  Maybe it is
    # counting lines wrong because of the "mu" in line 4.
    if (debug > 0) {
        cat("First 3 lines of data:\n")
        print(head(data, 3L))
        cat("Last 3 lines of data:\n")
        print(tail(data, 3L))
    }
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
