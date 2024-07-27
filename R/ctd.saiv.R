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
#' Some variable names are change to the oce convention, e.g.
#' `"Sal."` becomes `"salinity"`, `"Temp"` becomes
#' `"temperature"`, etc.  In the first version of the code,
#' this renaming was done based on examination of a single file.
#' This list was expanded after a user kindly supplied a one-page
#' document that explains the variable names and units. As with
#' other functions for reading [oce-class] data, [read.ctd.saiv()]
#' resolves duplicate variable names by appending 2 to the second
#' instance, 3 to the third, etc.
#'
#' As with other [ctd-class] objects, the `[[` operator handles
#' both the original name from the file, and the converted oce
#' name.
#'
#' It is worth noting the following oddities that were present
#' in the sample file upon which [read.ctd.saiv()] was based.
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
#' accurately from depth (which is what the header suggests the file
#' contains) unless the latitude is known. In [read.ctd.saiv()],
#' latitude is assumed to be 45 degrees north, which is the default
#' used by [swPressure()].
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
#' @author Dan Kelley, with help from the github member with
#' the handle 'Rdescoteaux', who kindly supplied a sample file
#' and a document listing SAIV variable names.
#'
#' @references
#'
#' 1. `https://saiv.no/sd204-ctd-profiler`
#'
#' @family things related to ctd data
#' @family functions that read ctd data
read.ctd.saiv <- function(file, encoding = "latin1", debug = getOption("oceDebug"), processingLog, ...) {
    if (missing(file)) {
        stop("must supply 'file'")
    }
    debug <- max(0L, as.integer(debug))
    oceDebug(debug, "read.ctd.saiv(file=\"", file, "\", ...) START\n", unindent = 1)
    if (!is.character(file)) {
        stop("'file' must be a character value")
    }
    filename <- ""
    # From file: Tr1_all_stations	Instrument no.:	595
    # Ser	Interval (sec)	Integration	Air pressure	Salinity	Chart Datum (dbar)
    # 4	1		1019.84
    # Ser	Meas	Sal.	Temp	F (ETC)
    # 4	584	0.02	8.221	0.09	0.56	-0.147	1440.08	0.00	10/06/2023	09:46:22
    header <- readLines(file, n = 4L, encoding = encoding)
    if (debug > 0L) {
        cat("header is:\n")
        print(header)
    }
    dataNamesOriginal <- strsplit(header[4], "\t")[[1]]
    # drop an empty name
    dataNamesOriginal <- dataNamesOriginal[nchar(dataNamesOriginal) > 0L]
    # compute oce-style data names
    oceDebug(debug, "Original data names: c(\"", paste(dataNamesOriginal, collapse = "\", \""), "\")\n")
    units <- list()
    dataNames <- dataNamesOriginal
    if ("Ser" %in% dataNames) {
        dataNames[dataNames == "Ser"] <- "series"
        units$series <- list(unit = expression(), scale = "")
    }
    if ("Meas" %in% dataNames) {
        dataNames[dataNames == "Meas"] <- "measurement"
        units$measurement <- list(unit = expression(), scale = "")
    }
    if ("Sal." %in% dataNames) {
        dataNames[dataNames == "Sal."] <- "salinity"
        units$salinity <- list(unit = expression(), scale = "PSS-78") # FIXME: this is a guess
    }
    if ("Cond" %in% dataNames) {
        dataNames[dataNames == "Cond"] <- "conductivity"
        units$conductivity <- list(unit = expression(mS / cm), scale = "")
    }
    if ("Temp" %in% dataNames) {
        dataNames[dataNames == "Temp"] <- "temperature"
        units$temperature <- list(unit = expression(degree * C), scale = "")
    }
    if ("Ox %" %in% dataNames) {
        dataNames[dataNames == "Ox %"] <- "oxygen"
        units$oxygen <- list(unit = expression("%"), scale = "SAIV 205")
    }
    if ("OpOx %" %in% dataNames) {
        dataNames[dataNames == "OpOx %"] <- "oxygen"
        units$oxygen <- list(unit = expression("%"), scale = "Aanderaa optode")
    }
    if ("OSOx %" %in% dataNames) {
        dataNames[dataNames == "OSOx %"] <- "oxygen"
        units$oxygen <- list(unit = expression("%"), scale = "Rinko III")
    }
    if ("mg/l" %in% dataNames) {
        dataNames[dataNames == "mg/l"] <- "oxygen"
        units$oxygen <- list(unit = expression(mg / l), scale = "")
    }
    if ("ml/l" %in% dataNames) {
        dataNames[dataNames == "ml/l"] <- "oxygen"
        units$oxygen <- list(unit = expression(ml / l), scale = "")
    }
    # Use grep because if comparing to a string requires writing that
    # string in a way that does not get flagged as an encoding problem.
    # Testing for encoding problems has proved a challenge in the past,
    # with e.g. all CRAN machines but one declaring code problem-free,
    # and that one machine causing a rejection. That's a bumpy road
    # I prefer not to revisit.
    tmp <- grep("^.*mol/l", dataNames)
    if (length(tmp) == 1L) {
        dataNames[tmp] <- "oxygen"
        units$oxygen <- list(unit = expression(umol / l), scale = "")
    }
    tmp <- grep("^.*mol/kg", dataNames)
    if (length(tmp) == 1L) {
        dataNames[tmp] <- "oxygen"
        units$oxygen <- list(unit = expression(umol / kg), scale = "")
    }
    if ("T (FTU)" %in% dataNames) {
        dataNames[dataNames == "T (FTU)"] <- "turbidity"
        units$turbidity <- list(unit = expression(FTU), scale = "")
    }
    tmp <- grep("^F [(]{1}", dataNames)
    if (length(tmp) == 1L) {
        dataNames[tmp] <- "fluorescence"
        units$fluorescence <- list(unit = expression(ug / l), scale = "")
    }
    # SAIV documents call this density, but the numbers in the
    # test file indicate that it's clearly sigma, or sigma-theta,
    # or sigma-t, or something.  Let's make a guess. I don't see
    # this as much of an issue, because analysts can compute density
    # in any desired form from salinity, etc., with the proviso that
    # TEOS-10 values require longitude and latitude to be inserted
    # into thectd-class object by the user.
    if ("Density" %in% dataNames) {
        dataNames[dataNames == "Density"] <- "sigma"
        units$sigma <- list(unit = expression(kg / m^3), scale = "")
    }
    if ("S. vel." %in% dataNames) {
        dataNames[dataNames == "S. vel."] <- "soundVelocity"
        units$soundVelocity <- list(unit = expression(m / s), scale = "")
    }
    if ("Pres" %in% dataNames) {
        dataNames[dataNames == "Pres"] <- "pressure"
        units$pressure <- list(unit = expression(dbar), scale = "")
    }
    # As with sigma above, I don't see any point in worrying a lot
    # about e.g. what density was used for the hydrostatic case,
    # because we have pressure, and can compute depth using oce
    # functions.
    if ("Depth(u)" %in% dataNames) {
        dataNames[dataNames == "Depth(u)"] <- "depth"
        units$depth <- list(unit = expression(m), scale = "unesco")
    }
    if ("Depth(d)" %in% dataNames) {
        dataNames[dataNames == "Depth(d)"] <- "depth"
        units$depth <- list(unit = expression(m), scale = "hydrostatic")
    }
    dataNames <- unduplicateNames(dataNames)
    oceDebug(debug, "data names: c(\"", paste(dataNames, collapse = "\", \""), "\")\n")
    data <- read.delim(file, skip = 4L, sep = "\t", col.names = dataNames, encoding = encoding)
    res <- new("ctd")
    res@data <- data
    res@metadata$header <- header
    res@metadata$filename <- filename
    dno <- list()
    dno[dataNames] <- dataNamesOriginal
    res@metadata$dataNamesOriginal <- dno
    res@metadata$units <- units
    oceDebug(debug, "END read.ctd.saiv()\n", sep="", unindent=1)
    return(res)
}
