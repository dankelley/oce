# vim:textwidth=80:expandtab:shiftwidth=4:softtabstop=4

# A helpful function.  I seem to do this kind of thing quite a lot, and so I may
# document this function later, and add it to the NAMESPACE.
makeNumeric <- function(x)
{
    if (is.numeric(x))
        return(x)
    if (is.vector(x))
        return(as.numeric(x))
    if (is.array(x)) {
        dim <- dim(x)
        rval <- as.numeric(x)
        dim(rval) <- dim
        return(rval)
    }
    stop("'x' must be a vector or an array")
}

#' Trim an AD2CP File
#'
#' Create an AD2CP file by copying the first `n` data chunks (regions starting
#' with 0xa5, etc) of another such file. This can be useful in supplying
#' small sample files for bug reports.
#'
#' @param infile name of an AD2CP source file.
#'
#' @param n integer indicating the number of data chunks to keep. The default is
#' to keep 100 chunks, a common good choice for sample files.
#'
#' @param outfile optional name of the new AD2CP to be created. If this is not
#' supplied, a default is used, by adding `_trimmed` to the base filename, e.g.
#' if `infile` is `"a.ad2cp"` then `outfile` will be `a_trimmed.ad2cp`.
#'
#' @param debugTerminal an integer value indicating the level of debugging. If
#' this is 1L, then a brief indication is given of the processing steps. If it
#' is > 1L, then information is given about each data chunk, which can yield
#' very extensive output.
#'
#' @return `ad2cpFileTrim()` returns the name of the output file, `outfile`, as
#' provided or constructed.
#'
#' @family things related to adp data
#' @family things related to ad2cp data
#' @family functions that trim data files
#' @examples
#'\dontrun{
#' # Can only be run by the developer, since it uses a private file.
#' f  <- "/Users/kelley/Dropbox/oce_secret_data/ad2cp/byg_trimmed.ad2cp"
#' if (file.exists(f)) {
#'     ad2cpFileTrim(f, 100L) # this file is already trimmed to 200 chunks
#' }
#'}
#' @author Dan Kelley
ad2cpFileTrim <- function(infile, n=100L, outfile, debug=getOption("oceDebug"))
{
    oceDebug(debug, "ad2cpFileTrim(infile=\"", infile, "\", n=", n, ", debug=", debug, ") { #\n", unindent=1)
    debug <- ifelse(debug < 1, 0L, ifelse(debug < 2, 1, 2))
    if (missing(infile))
        stop("must provide 'infile'")
    n <- as.integer(n)
    if (n < 1L)
        stop("'n' must be a positive number, but it is ", n)
    if (missing(outfile)) {
        outfile <- gsub("(.*).ad2cp", "\\1_trimmed.ad2cp", infile)
        oceDebug(debug, "created outfile value \"", outfile, "\"")
    }
    r <- read.oce(infile, which="??")
    nmax <- length(r$start)
    if (n >= nmax)
        stop("maximum allowed 'n' for this file is ", nmax)
    # add 1 to profile count; go back 1 char before that
    last <- r$start[n+1L] - 1L
    buf <- readBin(infile, "raw", n=last)
    writeBin(buf, outfile, useBytes=TRUE)
    oceDebug(debug, "} # ad2cpFileTrim\n", unindent=1)
    outfile
}

# private function
ad2cpDefaultDataItem <- function(x, j=NULL, order=c("burst", "average",
        "bottomTrack", "interleavedBurst", "burstAltimeterRaw",
        "DVLBottomTrack", "echosounder", "DVLWaterTrack", "altimeter",
        "averageAltimeter"))
{
    if (!is.ad2cp(x))
        stop("x is not an AD2CP object")
    dataNames <- names(x@data)
    if (is.null(j) || nchar(j) == 0) {
        i <- which(order %in% dataNames)
        if (length(i)) order[i[1]] else stop("ad2cp object does not contain any of '", paste(order, collapse="', '"), "'")
    } else {
        if (j %in% dataNames) j else stop("ad2cp object does not contain data item '", j, "'")
    }
}


#' Decode an item from a Nortek AD2CP file header (an internal function)
#'
#' @param x an [adp-class] object that holds AD2CP data.
#'
#' @param key Character value that identifies a particular line in the file
#' header.
#'
#' @param item Character value indicating the name of the item sought.
#'
#' @param numeric Logical value indicating whether to convert the return value
#' from a string to a numerical value.
#'
#' @param default Optional value to be used if the item is not found in the
#' header, or if the header is `NULL` (as in the case of a split-up file
#' that lacks the initial header information)
#'
#' @return String or number interpreted from the `x[["text"]]`, or `NULL`,
#' if the desired item is not found there, or if `x` is not of the required
#' class and variety.
#'
#' @examples
#'\dontrun{
#' if (file.exists("a.ad2cp")) {
#'     d <- read.oce("a.ad2cp")
#'     # The examples start with the line in x[["text"]][[1]]; note that in the second
#'     # example, it would be insuficient to use a key of "BEAMCFGLIST", because that will
#'     # yield 4 lines, and the function is not designed to handle that.
#'
#'     # ID,STR=\"Signature1000\",SN=123456
#'     type <- ad2cpHeaderValue(d, "ID", "STR", numeric=FALSE)
#'     serialNumber <- ad2cpHeaderValue(d, "ID", "SN")
#'
#'     # BEAMCFGLIST,BEAM=1,THETA=25.00,PHI=0.00,FREQ=1000,BW=25,BRD=1,HWBEAM=1,ZNOM=60.00
#'     beam1Angle <- ad2cpHeaderValue(d, "BEAMCFGLIST,BEAM=1", "THETA")
#'     frequency <- ad2cpHeaderValue(d, "BEAMCFGLIST,BEAM=1", "FREQ", default=NA)
#' }
#'}
#'
#' @family things related to adp data
#' @family things related to ad2cp data
#'
#' @author Dan Kelley
ad2cpHeaderValue <- function(x, key, item, numeric=TRUE, default)
{
    if (missing(x))
        stop("must provide x")
    if (is.character(x)) {
        header <- x
    } else if (is.ad2cp(x)) {
        header <- x[["header"]]
    } else {
        stop("x must be either a character value or an AD2CP object")
    }
    if (missing(key))
        stop("must provide key")
    if (missing(item))
        stop("must provide item")
    if (is.null(header))
        return(if (missing(default)) NULL else default)
    key2 <- paste("^", key, ",", sep="")
    # message("key2='", key2, "'")
    hline <- header[grep(key2, header)]
    # message("hline='",hline,"'")
    if (length(hline) > 1)
        stop("header line is not distinct; try using a comma at the end of key")
    if (0 == length(hline))
        return(if (missing(default)) NULL else default)
    if (0 == length(grep(item, hline))) {
        return(if (missing(default)) NULL else default)
    }
    res <- gsub(paste("^.*", item, "=([^,]*).*$", sep=""), "\\1", hline)
    if (nchar(res)) {
        res <- if (numeric) as.numeric(res) else gsub('"', '', res)
    } else {
        res <- if (missing(default)) NULL else default
    }
    res
}

#' Test whether object is an AD2CP type
#'
#' @param x an [oce] object.
#'
#' @return Logical value indicating whether `x` is an [adp-class] object,
#' with `fileType` in its `metadata` slot equal to `"AD2CP"`.
#'
#' @family things related to adp data
#' @family things related to ad2cp data
#'
#' @author Dan Kelley
is.ad2cp <- function(x)
{
    if (!inherits(x, "adp")) {
        FALSE
    } else {
        fileType <- x@metadata$fileType
        (!is.null(fileType)) && fileType == "AD2CP"
    }
}

#' Map AD2CP ID Code to oce Name
#'
#' As explained in Nortek (2022, section 6.1, page 80), AD2CP files use a
#' hexadecimal (in R, "raw") code to indicate the nature of each data chunk, and
#' [read.adp.ad2cp()] uses the present function as it analyses AD2CP files.
#'
#' The mapping from code (hex or decimal) to oce name is as follows.
#'
#' | code (raw) | code (integer) |          oce name |
#' |      ----: |          ----: |             ----: |
#' | ---------- | -------------- | ----------------- |
#' |      0x15  |             21 |             burst |
#' |      0x16  |             22 |           average |
#' |      0x17  |             23 |       bottomTrack |
#' |      0x18  |             24 |  interleavedBurst |
#' |      0x1a  |             26 | burstAltimeterRaw |
#' |      0x1b  |             27 |    DVLBottomTrack |
#' |      0x1c  |             28 |       echosounder |
#' |      0x1d  |             29 |     DVLWaterTrack |
#' |      0x1e  |             30 |         altimeter |
#' |      0x1f  |             31 |  averageAltimeter |
#' |      0xa0  |            160 |              text |
#'
#' @param code a [raw] (or corresponding integer) vector indicating the IDs of
#' interest, or NULL to get a summary of possible values.
#'
#' @return An indication of the mapping.  If `code` is NULL, this is a data
#' frame.  Otherwise, it is a character vector with the relevant mappings,
#' with the raw form of the code linked with the name, as in the example.
#'
#' @examples
#' stopifnot(ad2cpCodeToName(0x15) == "0x15=burst")
#'
#' @references
#' Nortek AS. \dQuote{Signature Integration 55|250|500|1000kHz.} Nortek AS,
#' March 31, 2022.
#'
#' @family things related to adp data
#' @family things related to ad2cp data
#'
#' @author Dan Kelley
ad2cpCodeToName <- function(code=NULL)
{
    table <- c(burst=as.raw(0x15),
        average=as.raw(0x16),
        bottomTrack=as.raw(0x17),
        interleavedBurst=as.raw(0x18),
        burstAltimeterRaw=as.raw(0x1a),
        DVLBottomTrack=as.raw(0x1b),
        echosounder=as.raw(0x1c),
        DVLWaterTrack=as.raw(0x1d),
        altimeter=as.raw(0x1e),
        averageAltimeter=as.raw(0x1f),
        text=as.raw(0xa0))
    if (is.null(code)) {
        rval <- data.frame(
            "code"=paste0("0x", as.raw(table), " (=", as.integer(table), ")"),
            "name"=names(table))
    } else {
        code <- as.raw(code)
        rval <- rep("", length(code))
        for (i in seq_along(code)) {
            m <- match(code[i], table)
            rval[i] <- paste0("0x", as.raw(code[i]), "=", if (is.na(m)) "?" else names(table)[m])
        }
    }
    rval
}



#' Read a Nortek AD2CP File
#'
#' This function reads Nortek AD2CP files, storing data elements in lists within
#' the `data` slot.  Those elements are named for the ID type in question.  For
#' example, data with ID code `0x16` are stored in `data$average`; see
#' [ad2cpCodeToName()] for the code mapping.
#'
#' By default, [read.adp.ad2cp()] reads all ID codes that are in the file.
#' This can yield very large objects, so if only certain IDs are of interest,
#' try setting the `which` document accordingly.
#'
#' It is important to realize that [read.adp.ad2cp()] is incomplete, and has not
#' been well tested.  The data format is not documented thoroughly in the
#' available Nortek manuals, and contradictions between the manuals require an
#' uncomfortable degree of guesswork; see \dQuote{Cautionary Notes}.
#'
#' @section Cautionary Notes:
#'
#' Early in the year 2022, support was added for 12-byte headers.  These are not
#' described in any Nortek document in the possession of the author of
#' [read.adp.ad2cp()], although some personal communications made via
#' https://github.com/dankelley/oce/issues have exposed some clues that have led
#' to provisional, but largely untested, code here.
#'
#' The \dQuote{References} section lists some manuals that were consulted during
#' the coding of `read.adp.ad2cp()].  Since instruments evolve over time, one
#' might think that Nortek (2022) would be the best place to start, in coding to
#' read AD2CP files. That would be a mistake, and a big one, at that. There
#' are two reasons for this.
#'
#' First, Nortek (2022) is not as clear in its description of the data format as
#' Nortek (2017) and Nortek (2018), as exemplified by a few examples.
#'
#' 1. Nortek (2022) does not explain how to compute checksums.  Without this,
#' it's impossible to write code to read the files, because the chance of random
#' byte sequences would match chunk-start codes is too high, relative to file
#' size.  After all, the Nortek engineers have used checksums in their products
#' for a long time, and for a very good reason.
#'
#' 2. Nortek (2022) does not lay out the data formats in sufficient detail to be
#' of much guidance. The new leading-underscore format (Nortek 2022, page 79) it
#' results in information being split into chunks that are spread throughout the
#' document.  Given a particular field (say "burst") just where is one to look in
#' the document?  And what do "Extends" and "Used By" (e.g. Nortek 2022, p85)
#' mean? The older document laid things out more clearly, e.g. the average/burst
#' format is laid out in detail, *in one place* on pages 57 to 64 of Nortek, with
#' the optional fields being clearly labelled in the rightmost column of Table
#' 6.1.3.
#'
#' 3. Nortek (2022) often lists units incorrectly.  For example, on page 82,
#' Pressure is said to have "Unit \[dBar\]" in green text, but the black text
#' above states "Raw data given as 0.001 dBar". If the stated storage class
#' (uint32) is to be believed, then it seems clear that the unit must be
#' 0.001 dBar, so the green text should be ignored.  The same can be said
#' of items throughout the data-format tables. In coding `read.adp.ad2cp()],
#' the green "Unit" text was ignored in basically every case.
#'
#' Second, Nortek (2022) contains significant errors, e.g. the following.
#'
#' 1. Nortek (2022 page 89) states the storage class for "Altimeter
#' data. Altimeter distance" (called `AltimeterDistance` by the present function)
#' to be `int32`, but Nortek (2017, 2018) both state it to be `float32`. Tests
#' with actual datasets make it clear that the format is `float32`, since wild
#' result are inferred by following the Nortek (2022) guidance.
#'
#' 2. As above, but for "AST data.AST distance" (called `ASTDistance` by the
#' present function).
#'
#' @param file a connection or a character string giving the name of the file to
#' load.
#'
#' @param from an integer indicating the index number of the first record to
#' read. This must equal 1, for this version of `read.adp.ad2cp`.  (If not
#' provided, `from` defaults to 1.)
#'
#' @param by ignored.
#'
#' @param to an integer indicating the final record to read. If `to` is 0L,
#' which is the default, then the value is changed internally to 1e9, and
#' reading stops at the end of the file.
#'
#' @param which a character value indicating the data type(s) to be read, and
#' stored in the `data` slot of the returned value.  The default, `which="all"`,
#' means to read all the types.  In many cases, though, the user does not want
#' to read everything at once, either as a way to speed processing or to avoid
#' running out of memory.  For this reason, a common first step is instead to
#' use `which="?"`, which gives a table of data types in the file or
#' `which="??"`, which gives a data frame overviewing the data 'chunks'; after
#' doing those things, the next step is usually to extract all the data, or an
#' individual type of interest is extracted.  The choices of individual type are
#' as follows:
#'
#' `"burst"` for ID code 0x15,
#' `"average"` for ID code 0x16,
#' `"bottomTrack"` for ID code 0x17,
#' `"interleavedBurst"` for ID code 0x18,
#' `"burstAltimeterRaw"` for ID code 0x1a,
#' `"DVLBottomTrack"` for ID code 0x1b,
#' `"echosounder"` for ID code 0x1c,
#' `"DVLWaterTrack"` for ID code 0x1d,
#' `"altimeter"` for ID code 0x1e,
#' and
#' `"averageAltimeter"` for ID code 0x1f.
#'
#' @param tz a character value indicating time zone. This is used in
#' interpreting times stored in the file.
#'
#' @param ignoreChecksums a logical value indicating whether to ignore
#' checksums.  This is FALSE by default, meaning that any data chunk with an
#' improper checksum is ignored. It may be necessary to set this to TRUE to
#' parse some problematic files.
#'
#' @param longitude,latitude numerical values indicating the observation
#' location.
#'
#' @param orientation ignored by [read.adp.ad2cp()], and provided only for
#' similarity to other `read.adp.*` functions.
#'
#' @param distance ignored by [read.adp.ad2cp()], and provided only for similarity
#' to other `read.adp.*` functions.
#'
#' @param plan optional integer specifying which 'plan' to focus on (see
#' reference 1 for the meaning of 'plan').  If this is not given, it defaults to
#' the most common plan in the requested subset of the data.
#'
#' @param type optional character value indicating the type of Nortek
#' instrument.  If this is not provided, an attempt is made to infer it from the
#' file header (if there is one), and `"Signature1000"` is used, otherwise. The
#' importance of knowing the type is for inferring the slantwise beam angle,
#' which is usd in the conversion from beam coordinates to xyz coordinates. If
#' `type` is provided, it must be one of `"Signature250"`, `"Signature500"`, or
#' `"Signature1000"`; the first of these has a 20 degree slant-beam angle, while
#' the others each have 20 degrees (see reference 2, section 2 on page 6). Note
#' that [oceSetMetadata()] can be used to alter the slantwise beam angle of an
#' existing object, and this will alter any later conversion from beam to xyz
#' coordinates.
#'
#' @param monitor a boolean value indicating whether to indicate the progress of
#' reading the file, by using [txtProgressBar()] or otherwise.  The value of
#' `monitor` is changed to `FALSE` automatically, for non-interactive sessions.
#'
#' @param despike ignored by [read.adp.ad2cp()], and provided only for similarity
#' to other `read.adp.*` functions.
#'
#' @param processingLog a character value that, if provided, is saved within the
#' `processingLog` slot of the returned value.
#'
#' @param debug an integer value indicating the level of debugging.  Set to 1 to
#' get a moderate amount of debugging information, from the R code only, to 2 to
#' get some debugging information from the C++ code that is used to parse the
#' data chunks, or to 3 for intensive debugging at both levels.
#'
#' @param \dots ignored.
#'
## @examples
##\dontrun{
## d <- read.adp.ad2cp("~/test.ad2cp", to=100) # or read.oce()
##}
#'
#' @return An [adp-class] object with `metadata$fileType` equal to `"AD2CP"`, a
#' table (if `which="?"`), a data frame (if `which="??"`), or a vector of
#' character (if `which="text"`).
#'
#' @author Dan Kelley
#'
#' @references
#'
#' Nortek AS. \dQuote{Signature Integration 55|250|500|1000kHz.} Nortek AS,
#' 2017.
#'
#' Nortek AS. \dQuote{Signature Integration 55|250|500|1000kHz.} Nortek AS,
#' 2018. (This revision includes new information about instrument orientation.)
#'
#' Nortek AS. \dQuote{Signature Integration 55|250|500|1000kHz.} Nortek AS,
#' March 31, 2022.  (This version is incomplete and quite confusing,
#' so the 2017 and 2018 versions are preferable, albeit perhaps out-of-date.)
#'
## Nortek AS. \dQuote{Operations Manual - Signature 250, 500 and 1000.} Nortek AS,
## September 21, 2018.
#'
#' @family things related to adp data
#' @family things related to ad2cp data
#'
#' @examples
#' # You can run this within the oce directory, if you clone from github.
#' file <- "tests/testthat/local_data/ad2cp/S102791A002_Barrow_v2.ad2cp"
#' if (file.exists(file)) {
#'     library(oce)
#'     d <- read.oce(file)
#' }
#'
#' @family functions that read adp data
#'
#' @author Dan Kelley
read.adp.ad2cp <- function(file, from=1, to=0, by=1, which="all",
    tz=getOption("oceTz"),
    ignoreChecksums=FALSE,
    longitude=NA, latitude=NA,
    orientation, distance, plan, type,
    monitor=FALSE, despike=FALSE, processingLog,
    debug=getOption("oceDebug"), ...)
{
    # setup
    i0v <- 0L                          # global variable, which some functions alter using <<-
    i <- 0L                            # global variable, which some functions alter using <<-
    # Interpret 'which'
    if (any(grepl("\\?", which))) {
        if (length(which) != 1L)
            stop("If which is \"?\" or \"??\", no other values are permitted")
        if (which != "?" && which != "??")
            stop("did you mean which=\"?\" or \"??\" for your value of which?")
    } else {
        whichChoices <- c("burst", "average", "bottomTrack", "interleavedBurst",
            "burstAltimeterRaw", "DVLBottomTrack", "echosounder", "DVLWaterTrack",
            "altimeter", "averageAltimeter")
        if (1L == length(which) && which == "all")
            which <- whichChoices
        unknownWhich <- !(which %in% whichChoices)
        if (any(unknownWhich))
            stop("unknown 'which' value(s): \"",
                paste(which[unknownWhich], collapse="\", \""), "\"")
        which <- unique(which)
    }
    # Interpret other parameters
    if (missing(file))
        stop("must supply 'file'")
    if (is.character(file)) {
        if (!file.exists(file))
            stop("cannot find file '", file, "'")
        if (0L == file.info(file)$size)
            stop("empty file '", file, "'")
    }
    debug <- min(3L, max(0L, as.integer(debug)))
    if (!interactive())
        monitor <- FALSE
    if (!missing(orientation))
        warning("ignoring 'orientation' (see documentation)")
    if (!missing(distance))
        warning("ignoring 'distance' (see documentation)")
    if (!missing(despike))
        warning("ignoring 'despike' (see documentation)")
    if (!interactive())
        monitor <- FALSE
    fromGiven <- !missing(from)
    toGiven <- !missing(to)
    byGiven <- !missing(by)
    planGiven <- !missing(plan)
    typeGiven <- !missing(type)

    oceDebug(debug, "read.adp.ad2cp(...,\n    ",
        "which=c(\"", paste0(which, sep="", collapse="\", \""), "\"),\n",
        "    from=", if (fromGiven) format(from) else "(missing)",
        ", to=", if (toGiven) to else "(missing)",
        ", by=", if (byGiven) by else "(missing)\n",
        "    plan=", if (planGiven) plan else "(missing)",
        ", type=\"", if (typeGiven) type else "(missing)",
        ", ignoreChecksums=", ignoreChecksums,
        ", ...)\n", sep="", unindent=1, style="bold")
    if (debug == 1L)
        oceDebug(debug, "HINT: set debug=2 for more output, or 3 for the maximum output\n")
    else if (debug == 2L)
        oceDebug(debug, "HINT: set debug=3 for even more output\n")

    if (typeGiven) {
        typeAllowed <- c("Signature1000", "Signature500", "Signature250")
        typei <- pmatch(type, typeAllowed)
        if (is.na(typei))
            stop("type must be \"Signature1000\", \"Signature500\", or \"Signature250\", but it is \"", type, "\"")
        type <- typeAllowed[typei]
    }
    if (!fromGiven)
        from <- 1
    #. if (!byGiven)
    #.     by <- 1
    if (!toGiven)
        to <- 0
    #. if (by != 1)
    #.     stop("must have by=1, since skipping makes no sense in complex ad2cp files")
    if (to < 0)
        stop("cannot have to<0")
    if (from != 1)
        stop("must have from=1")
    if (to == 0) {
        to <- 1e9                      # this should be enough to read any file
        oceDebug(debug, "'to' not given; defaulting to ", to, " so we will likely get to the end of the file\n")
    }
    if (is.character(file)) {
        filename <- fullFilename(file)
        file <- file(file, "rb")
        on.exit(close(file))
    } else {
        filename <- "(connection)"
    }
    if (!inherits(file, "connection"))
        stop("`file` must be a character string or connection")
    if (!isOpen(file)) {
        filename <- "(connection)"
        open(file, "rb")
        on.exit(close(file))
    }
    # Determine file size
    seek(file, 0, "start")
    seek(file, where=0, origin="end")
    fileSize <- seek(file, where=0)
    seek(file, 0, "start")
    oceDebug(debug, "fileSize:", fileSize, "\n")
    buf <- readBin(file, what="raw", n=fileSize, size=1)
    oceDebug(debug, 'first 10 bytes in file: ',
        paste(paste("0x", buf[1+0:9], sep=""), collapse=" "), "\n", sep="")
    headerSize <- as.integer(buf[2])
    oceDebug(debug, "headerSize:", headerSize, "\n")
    ID <- buf[3]
    oceDebug(debug, "ID: 0x", ID, " (NB: 0x15=burst data record; 0x16=avg data record; 0x17=bottom track record; 0x18=interleaved data record; 0xa0=string data record, e.g. GPS NMEA, comment from the FWRITE command)\n", sep="")
    dataSize <- readBin(buf[5:6], what="integer", n=1, size=2, endian="little", signed=FALSE)
    oceDebug(debug, "dataSize:", dataSize, "\n")
    oceDebug(debug, "buf[1+headerSize+dataSize=", 1+headerSize+dataSize, "]=0x", buf[1+headerSize+dataSize], " (expect 0xa5)\n", sep="")
    nav <- do_ldc_ad2cp_in_file(filename, from, to, by,
        if (ignoreChecksums) 1L else 0L,
        debug-1L)
    # Return overviews (whole file)
    if (which[1] == "?") {
        t <- table(nav$id)
        names(t) <- ad2cpCodeToName(names(t))
        o <- order(names(t))
        return(t[o])
    } else if (which[1] == "??") {
        bytesInHeader <- ifelse(nav$twelve_byte_header, 12L, 10L)
        return(data.frame(
                ID=ad2cpCodeToName(nav$id),
                start=nav$index-bytesInHeader+1))
    } else if (which[1] == "???") {
        bytesInHeader <- ifelse(nav$twelve_byte_header, 12L, 10L)
        #gi <- gappyIndex(nav$index-bytesInHeader, 4L, 2L)
        return(data.frame(
                ID=ad2cpCodeToName(nav$id),
                start=nav$index-bytesInHeader+1,
                offsetOfData=as.integer(buf[nav$index+2L])))
    }
    #DANnav<<-nav;message("FIXME: exported nav as DANnav")
    #DAN<<-nav;save(DAN,file="DAN.rda")
    if (nav$twelve_byte_header == 1L)
        warning("file has 12-byte headers (an undocumented format), so be on the lookout for spurious results")
    d <- list(buf=buf, index=nav$index, length=nav$length, id=nav$id)
    if (0x10 != d$buf[d$index[1]+1]) # must be 0x10 for an AD2CP (p38 integrators guide)
        stop("expecting byte value 0x10 index ", d$index[1]+1, ", but got 0x", d$buf[d$index[1]+1])
    oceDebug(debug, "length(d$index)=", length(d$index), "\n", sep="")
    Nmax <- length(d$index)
    if (to > Nmax) {
        warning("using to=", Nmax, " based on file contents")
        to <- Nmax
    }
    focusIndex <- seq(from, to, by=by)
    N <- length(focusIndex)
    if (N <= 0)
        stop("must have to > from")


    # Set up object, with key metadata to allow other functions to work.
    res <- new("adp")
    firstData <- which(d$id != 0xa0)[1] # first non-text chunk
    serialNumber <- readBin(d$buf[d$index[firstData]+5:8], "integer", size=4, endian="little")

    # Create pointers for accessing 1-byte, 2-byte, and 4-byte chunks
    oceDebug(debug, "focussing on ", length(d$index), " data records\n")
    #.pointer2 <- as.vector(t(cbind(pointer1, 1 + pointer1))) # rbind() would be fine, too.
    #.pointer4 <- as.vector(t(cbind(pointer1, 1 + pointer1, 2 + pointer1, 3 + pointer1)))
    # Below indicates that gappyIndex() is about 6 times faster on a small file
    # (1477 chunks) This won't matter for such a small file, but it will for
    # larger files.  My tests with 1e6 points suggests factor >100 speedup; for
    # more, see https://github.com/dankelley/oce/wiki/fast-indexing
    #> browser()
    #> microbenchmark(
    #>     as.vector(t(cbind(pointer1, 1 + pointer1, 2 + pointer1, 3 + pointer1))),
    #>     pointer4NEW <- gappyIndex(pointer1, 0, 3)
    #>     )
    #.pointer2NEW <- gappyIndex(pointer1, 0, 2)
    #.pointer4NEW <- gappyIndex(pointer1, 0, 4)
    #.if (!all.equal(pointer2, pointer2NEW))
    #.    warning("DEVELOPER NOTE: pointer2NEW != pointer2 at spot 1")
    #.if (!all.equal(pointer4, pointer4NEW))
    #.    warning("DEVELOPER NOTE: pointer4NEW != pointer4 at spot 1")
    pointer1 <- d$index
    pointer2 <- gappyIndex(d$index, 0, 2)
    pointer4 <- gappyIndex(d$index, 0, 4)

    # {{{
    # Construct an array to store the bits within the 'status' vector. The nortek
    # docs refer to the first bit as 0, which becomes [1,] in this array. Note
    # that we may drop some elements (if they are not in the current 'plan')
    # later, depending on 'keep'.
    status <- intToBits(readBin(d$buf[pointer4 + 69L], "integer", size=4L, n=N, endian="little"))
    dim(status) <- c(32L, N)
    # Interpret status, but note that items will be subsetted later (see 'keep')
    # Bit 1 in the Nortek (2022 table 6.3 page 87) zero-based notation is index 2 in R.
    blankingDistanceInCm <- as.integer(status[2L,])
    # Nortek docs say bit 16 indicates the active configuration, but they
    # count from 0, so it is bit 17 here.
    activeConfiguration <- as.integer(status[17L, ])
    # Decode the orientation from bits 25-27 in 0-offset notation, i.e. 26-28 here.
    # Note that reference [1 table 1 page 55] only has some of the
    # permitted codes, but the updated manual [3 table 1 page 64], has more,
    # including the value 7 that is used in the three sample files available
    # to the author as of late December, 2018.
    orientation1 <- as.integer(status[26L, ])
    orientation2 <- as.integer(status[27L, ])
    orientation3 <- as.integer(status[28L, ])
    O <- orientation1 + 2L*orientation2 + 4L*orientation3
    orientation <- c("xup", "xdown", "yup", "ydown", "zup", "zdown", "-", "AHRS")[O+1]
    # TEST below is a test of whether I have the bits wrong. Comparison
    # TEST with the header in 4 files suggests I am doing this right.
    # TEST message(vectorShow(O))
    # TEST message(vectorShow(orientation))
    # TEST O2 <- 4*orientation1 + 2*orientation2 + orientation3
    # TEST message(vectorShow(O2))
    # TEST orientation2 <- c("xup", "xdown", "yup", "ydown", "zup", "zdown", "-", "AHRS")[O2+1]
    # TEST message(vectorShow(orientation2))
    # TEST message("I think O1/orientation is correct")

    # Plan (active configuration)
    # TEST message("table(activeConfiguration):")
    # TEST print(table(activeConfiguration))
    # If the 'plan' argument is missing, we select the most common one in the data subset.
    if (!planGiven) {
        u <- unique(activeConfiguration)
        nu <- length(u)
        if (nu == 1) {
            plan <- activeConfiguration[1]
            warning("'plan' defaulting to ", plan, ", the only value in the file")
        } else {
            plan <- u[which.max(unlist(lapply(u,function(x)sum(activeConfiguration==x))))]
            acTable <- table(activeConfiguration)
            warning("'plan' defaulting to ", plan,
                ", most common value in file (",
                paste(names(acTable)," occurs ",unname(acTable)," time[s]", sep="",collapse="; "), ")")
        }
    }
    # Try to find a header, as the first record-type that has id=0xa0.
    header <- NULL
    idHeader <- which(d$id == 0xa0)[1] # first text chunk
    if (length(idHeader)) {
        oceDebug(debug, "this file has a header at id=", idHeader, "\n", sep="")
        chars <- rawToChar(d$buf[seq.int(2L+d$index[idHeader], by=1L, length.out=-1L+d$length[idHeader])])
        header <- strsplit(chars, "\r\n")[[1]]
        if (!typeGiven) {
            type <- gsub('.*STR="([^"]*)".*$', '\\1', header[grep("^ID,", header)])
            typeGiven <- TRUE
        }
    }
    #if (which == "text") {
    #    w <- which(d$id == 0xa0)
    #    rval <- vector("list", length(w))
    #    for (i in seq_along(w)) {
    #        chars <- rawToChar(d$buf[seq.int(2L+d$index[w[i]], by=1, length.out=-1+d$length[w[i]])])
    #        rval[i] <- strsplit(chars, "\r\n")[1]
    #    }
    #    return(rval)
    #}
    keep <- activeConfiguration == plan
    if (sum(keep) == 0L) {
        stop("there are no data for plan=", plan, "; try one of the following values instead: ", paste(unique(activeConfiguration), collapse=" "))
    }
    if (sum(keep) < length(keep)) {
        oceDebug(debug, "this plan has ", sum(keep), " data records, out of a total of ", length(keep), " in the file subset\n")
        N <- sum(keep)                 # need to update this or all else will fail!
        d$index <- d$index[keep]
        d$length <- d$length[keep]
        d$id <- d$id[keep]
        status <- status[, keep, drop=FALSE]
        #message(vectorShow(blankingDistanceInCm))
        blankingDistanceInCm <- blankingDistanceInCm[keep]
        #message(vectorShow(blankingDistanceInCm))
        activeConfiguration <- activeConfiguration[keep]
        orientation1 <- orientation1[keep]
        orientation2 <- orientation2[keep]
        orientation3 <- orientation3[keep]
        orientation <- orientation[keep]
        oceDebug(debug, "N=", N, "\n")
        pointer1 <- d$index
        pointer2 <- as.vector(t(cbind(pointer1, 1 + pointer1))) # rbind() would be fine, too.
        pointer4 <- as.vector(t(cbind(pointer1, 1 + pointer1, 2 + pointer1, 3 + pointer1)))
        pointer2NEW <- gappyIndex(pointer1, 0, 2)
        pointer4NEW <- gappyIndex(pointer1, 0, 4)
        if (!all.equal(pointer2, pointer2NEW))
            warning("DEVELOPER NOTE: pointer2NEW != pointer2 at spot 2")
        if (!all.equal(pointer4, pointer4NEW))
            warning("DEVELOPER NOTE: pointer4NEW != pointer4 at spot 2")
        oceDebug(debug, "focussing on ", length(pointer1), " data records (after subsetting for plan=", plan, ")\n", sep="")
    }
    if (debug > 0) {
        oceDebug(debug, "below is table() of the 'plan' values in this subset of the file:\n")
        print(table(activeConfiguration))
    }

    # }}}

    # commonData (Nortek 2022 Table 6.2 Page 81)
    commonData <- list()

    # "Version" in Nortek (2022 table 6.2 page 81)
    # NB. this can vary across IDs, e.g. in private test file f2, the text chunk
    # (i.e. the header) has version 16, while the other records had version 3.
    commonData$version <- as.integer(d$buf[pointer1+1L])
    commonData$offsetOfData <- as.integer(d$buf[pointer1+2L])
    #message("DAN holds commonData");DAN<<-commonData
    commonData$configuration <- local({
        tmp <- rawToBits(d$buf[pointer2+3L]) == 0x01
        dim(tmp) <- c(16, N)
        t(tmp)
    })
    commonData$serialNumber <- readBin(d$buf[pointer4+5L], "integer", n=N, size=4L)
    # The vectorization scheme used in this function assumes that configurations
    # match within a given ID type.  This seems like a reasonable assumption,
    # and one backed up by the impression of a Nortek representative, but I do
    # not see definititive statement of the requirement in any documentation
    # I've studied. Since we *need* this to be true in order to read the data in
    # vectorized way, we *insist* on it here, rather than trying to catch
    # problems later. Use local() to avoid polluting namespace.
    local({
        for (id in as.raw(unique(d$id))) {
            config <- commonData$configuration[d$id==id,]
            if (is.matrix(config)) {
                ok <- TRUE
                for (col in seq(2L, ncol(config)))
                    ok <- ok && all(config[,col] == config[1,col])
                if (!ok) {
                    oceDebug(debug, "commonData$configuration DIFFERS from the first value, in ", sum(!ok), " instances for chunk key 0x", as.raw(id), " (", ad2cpCodeToName(id), ")\n")
                    stop("Variable \"", ad2cpCodeToName(id), "\" configuration detected, so expect erroneous results. Please submit a bug report.")
                }
            }
        }
    })

    # Extract columns as simply-named flags, for convenience. The variable
    # names to which the assignments are made apply to average/burst data.
    #UNUSED pressureValid <- configuration[, 1]
    #UNUSED temperatureValid <- configuration[, 2]
    #UNUSED compassValid <- configuration[, 3]
    #UNUSED tiltValid <- configuration[, 4]
    # configuration[, 5] -

    # BOOKMARK 1 define *Included, used later in reading
    configuration <- commonData$configuration # FIXME: remove this later
    velocityIncluded <- configuration[, 6]
    amplitudeIncluded <- configuration[, 7]
    correlationIncluded <- configuration[, 8]
    altimeterIncluded <- configuration[, 9]
    altimeterRawIncluded <- configuration[,10]
    ASTIncluded <- configuration[,11]
    echosounderIncluded <- configuration[,12]
    AHRSIncluded <- configuration[,13]
    percentGoodIncluded<- configuration[,14]
    stdDevIncluded <- configuration[,15]
    # We skip bit 16, which  is called 'unused' in Nortek AS. \dQuote{Signature
    # Integration 55|250|500|1000kHz.} Nortek AS, 2017.
    # configuration[, 16] "Unused" in 2017 Signature
    oceDebug(debug, vectorShow(velocityIncluded))
    oceDebug(debug, vectorShow(amplitudeIncluded))
    oceDebug(debug, vectorShow(correlationIncluded))
    oceDebug(debug, vectorShow(altimeterIncluded))
    oceDebug(debug, vectorShow(altimeterRawIncluded))
    oceDebug(debug, vectorShow(ASTIncluded))
    oceDebug(debug, vectorShow(echosounderIncluded))
    oceDebug(debug, vectorShow(AHRSIncluded))
    oceDebug(debug, vectorShow(percentGoodIncluded))
    oceDebug(debug, vectorShow(stdDevIncluded))


    # Now, start decoding actual data.

    # Decode time. Note that the 100usec part sometimes exceeds 1s, when
    # multiplied by 1e4.  But we get the same result as nortek-supplied matlab
    # code in a test file, so I won't worry about this, assuming instead that
    # this is a quirk of the nortek setup.
    time <- ISOdatetime(year=1900+ as.integer(d$buf[pointer1 + 9]),
        month=1+as.integer(d$buf[pointer1 + 10]),
        day=as.integer(d$buf[pointer1 + 11]),
        hour=as.integer(d$buf[pointer1 + 12]),
        min=as.integer(d$buf[pointer1 + 13]),
        sec=as.integer(d$buf[pointer1 + 14]) +
        1e-4 * readBin(d$buf[pointer2 + 15],
            "integer", size=2, n=N, signed=FALSE, endian="little"),
        tz="UTC")
    soundSpeed <- 0.1 * readBin(d$buf[pointer2 + 17], "integer", size=2, n=N, signed=FALSE, endian="little")
    temperature <- 0.01 * readBin(d$buf[pointer2 + 19], "integer", size=2, n=N, signed=FALSE, endian="little")
    # FIXME: docs say pressure is uint32, but R does not handle unsigned 32-bit chunks
    #TEST<-list(buf=d$buf, pointer4=pointer4);save(TEST,file="TEST.rda")
    pressure <- 0.001 * readBin(d$buf[pointer4 + 21L], "integer", size=4L, n=N, endian="little")
    heading <- 0.01 * readBin(d$buf[pointer2 + 25L], "integer", size=2L, n=N, signed=FALSE, endian="little")
    pitch <- 0.01 * readBin(d$buf[pointer2 + 27L], "integer", size=2L, n=N, endian="little")
    roll <- 0.01 * readBin(d$buf[pointer2 + 29L], "integer", size=2L, n=N, endian="little")
    # See Nortek (2022) section 6.5, page 88 for bit-packing scheme used for BCC.
    # BCC (beam, coordinate system, and cell) uses packed bits to hold info on
    # the number of beams, coordinate-system, and the number cells. There are
    # two cases [1 page 49]:
    # case 1: Standard bit 9-0 ncell; bit 11-10 coord (00=enu, 01=xyz, 10=beam, 11=NA); bit 15-12 nbeams
    # case 2: bit 15-0 number of echo sounder cells
    # As for 'configuration' above, we set this up as a matrix of 0s and 1s,
    # with rows corresponding to times, for easy transformation into integers.
    # BCC case 1
    BCC <- ifelse(0x01 == rawToBits(d$buf[pointer2 + 31]), 1L, 0L)
    dim(BCC) <- c(16, N)
    BCC <- t(BCC)
    # Use Horner's rule for clarity (for lispers, anyway!)
    ncells <- BCC[,1]+2*(BCC[,2]+2*(BCC[,3]+2*(BCC[,4]+2*(BCC[,5]+2*(BCC[,6]+2*(BCC[,7]+2*(BCC[,8]+2*(BCC[,9]+2*BCC[,10]))))))))
    nbeams <- BCC[,13]+2L*(BCC[,14L]+2L*(BCC[,15L]+2L*BCC[,16L]))
    #DAN<<-nbeams
    # b00=enu, b01=xyz, b10=beam, b11=- [1 page 49]
    coordinateSystem <- c("enu", "xyz", "beam", "?")[1 + BCC[,11] + 2*BCC[,12]]
    # BCC case 2
    ncellsEchosounderWholeFile <- readBin(d$buf[pointer2 + 31], "integer", size=2, n=N, signed=FALSE, endian="little")

    # cell size is recorded in mm [1, table 6.1.2, page 49]
    cellSize <- 0.001 * readBin(d$buf[pointer2 + 33], "integer", size=2, n=N, signed=FALSE, endian="little")
    # FIXME: resolve question about unit of blankingDistance.
    # Nortek (2017 table 6.1.2 page 49) indicates that blanking distance is
    # recorded in cm but Nortek (2022 page 85) says it is in either cm or mm,
    # depending on an element in status (bit 1 in notation of Nortek 2022 page
    # 87). Perhaps the ability to store in mm was added later?  However, in
    # tests/testthat/test_ad2cp_2.R, the value inferred from assuming that
    # Nortek (2022) is correct does not match with the header (header says 2.000
    # for echosounder but the method below yields 2).
    tmp <- readBin(d$buf[pointer2 + 35L], "integer", size=2, n=N, signed=FALSE, endian="little")
    blankingDistanceFactor <- ifelse(blankingDistanceInCm==1, 1e-2, 1e-3)
    blankingDistance <- blankingDistanceFactor * tmp

    nominalCorrelation <- readBin(d$buf[pointer1 + 37], "integer", size=1, n=N, signed=FALSE, endian="little")
    # Magnetometer (Table 6.2, page 82, ref 1b)
    magnetometerx <- readBin(d$buf[pointer2 + 41], "integer", size=2, n=N, signed=TRUE, endian="little")
    magnetometery <- readBin(d$buf[pointer2 + 43], "integer", size=2, n=N, signed=TRUE, endian="little")
    magnetometerz <- readBin(d$buf[pointer2 + 45], "integer", size=2, n=N, signed=TRUE, endian="little")
    # Accelerometer (Table 6.2, page 82, ref 1b)
    # IMOS https://github.com/aodn/imos-toolbox/blob/e19c8c604cd062a7212cdedafe11436209336ba5/Parser/readAD2CPBinary.m#L555
    #  AccRawX starts at idx+46
    #  IMOS_pointer = oce_pointer - 1
    accelerometerx <- 1.0/16384.0 * readBin(d$buf[pointer2 + 47], "integer", size=2, n=N, signed=TRUE, endian="little")
    accelerometery <- 1.0/16384.0 * readBin(d$buf[pointer2 + 49], "integer", size=2, n=N, signed=TRUE, endian="little")
    accelerometerz <- 1.0/16384.0 * readBin(d$buf[pointer2 + 51], "integer", size=2, n=N, signed=TRUE, endian="little")
    # NOTE: all things below this are true only for current-profiler data; see
    # page 82 of Nortek (2022) for the vexing issue of ambiguityVelocity being
    # 2 bytes for current-profiler data but 4 bytes for bottom-track data.
    datasetDescription <- readBin(d$buf[pointer2 + 55], "integer", size=2, n=N, signed=FALSE, endian="little")
    transmitEnergy <- readBin(d$buf[pointer2 + 57], "integer", size=2, n=N, signed=FALSE, endian="little")
    # FIXME: next, using offset 59, is true only for currents ('average' or 'burst').
    # Nortek (2022) page 82.
    velocityFactor <- 10^readBin(d$buf[pointer1 + 59], "integer", size=1, n=N, signed=TRUE, endian="little")
    oceDebug(debug, "velocityFactor=", velocityFactor[1], " (for current-profiler data ONLY)")
    # 0.001 for 'average' in private file ~/Dropbox/oce_secret_data/ad2cp_secret_1.ad2cp
    powerLevel <- readBin(d$buf[pointer1 + 60], "integer", size=1, n=N, signed=TRUE, endian="little")
    temperatureMagnetometer <- 0.001 * readBin(d$buf[pointer2 + 61], "integer", size=2, n=N, signed=TRUE, endian="little")
    # See https://github.com/dankelley/oce/issues/1957 for a discussion of the
    # unit of temperatureRTC.  Nortek (2022) says it is in degC, but a
    # previous manual says it is in 0.01C; the latter produces values that make
    # sense (e.g. approx 20C for an in-air test) so that's used here.
    temperatureRTC <- 0.01 * readBin(d$buf[pointer2 + 63], "integer", size=2, n=N, signed=TRUE, endian="little")
    #UNUSED error <- readBin(d$buf[pointer2 + 65], "integer", size=4, n=N, endian="little") # FIXME: UNUSED

    # status0, byte 67:68, skipped
    # status,  byte 69:71, already read above so we could infer activeConfiguration

    oceDebug(debug, vectorShow(status[2,]))
    ensemble <- readBin(d$buf[pointer4+73], "integer", size=4, n=N, endian="little")

    # Limitations
    nconfiguration <- length(unique(activeConfiguration))
    if (1 < nconfiguration) {
        cat("developer-aimed information:\n")
        print(unique(activeConfiguration))
        print(table(activeConfiguration))
        stop("This file has ", nconfiguration, " active configurations, but read.adp.ad2cp() can only handle one. Please contact the oce developers if you need to work with this file.")
    }

    # Record-type keys and phrases in documentation [1, sec 6.1, page 47]
    # 0x15 - Burst Data Record.
    # 0x16 - Average Data Record.
    # 0x17 - Bottom Track Data Record (nb. IMOS does this wrong)
    # 0x18 - Interleaved Burst Data Record (beam 5).
    # 0x1A - Burst Altimeter Raw Record.
    # 0x1B - DVL Bottom Track Record.
    # 0x1C - Echo Sounder Record. (cf. IMOS does not handle)
    # 0x1D - DVL Water Track Record. (cf. IMOS does not handle)
    # 0x1E - Altimeter Record.
    # 0x1F - Avg Altimeter Raw Record.
    # 0xA0 - String Data Record, eg. GPS NMEA data, comment from the FWRITE command.
    # Set up pointers to records matching these keys.
    p <- list(burst=which(d$id==0x15),
        average=which(d$id==0x16),
        bottomTrack=which(d$id==0x17),
        interleavedBurst=which(d$id==0x18),
        burstAltimeterRaw=which(d$id==0x1a),
        DVLBottomTrack=which(d$id==0x1b),
        echosounder=which(d$id==0x1c),
        DVLWaterTrack=which(d$id==0x1d),
        altimeter=which(d$id==0x1e),
        averageAltimeter=which(d$id==0x1f))

    #x Try to retrieved a named item from the data buffer.
    #x
    #x This checks external variable `haveItem` to see whether the data item
    #x indicated by `name` is available in external raw vector `buf`.  If so, the
    #x item is retrieved from the starting at position indicated by external
    #x variable `i0v`, and `i0v` is then increased to prepare for the next call to
    #x this function.
    #x
    #x @param object a list containing what is known so far. A possibly-modified
    #x value of this is returned.
    #x
    #x @param name character value naming the item.
    #x
    #x @param i integer vector that points to starting spots of relevant chunks.
    #x
    #x @param type character value naming the type of chunk. This is used to
    #x look scaling factors for velocity.
    #x
    #x @return a list defined by adding the named item to `object`, if it
    #x is present in the dataset.
    #x
    #x @references
    #x
    #x Nortek AS. \dQuote{Signature Integration 55|250|500|1000kHz.} Nortek AS,
    #x 2017.
    #x
    #x @author Dan Kelley
    getItemFromBuf <- function(object, name, i, type, debug=getOption("oceDebug"))
    {
        NB <- object$numberOfBeams
        NC <- object$numberOfCells
        NP <- length(object$time)      # already defined
        #oceDebug(debug, "  ... NP=",NP,", NB=", NB, ", NC=", NC, "\n")
        NBC <- NB * NC
        oceDebug(debug, "getItemFromBuf: NB=", NB, ", NC=", NC, ", NBC=", NBC, ", NP=", NP, "\n")
        oceDebug(debug, "   ", vectorShow(i))
        if (name == "v") {
            oceDebug(debug, "   v starts at i0v=", i0v, "\n")
            velocityFactor <- velocityFactor[p[[type]][1]]
            oceDebug(debug, "   velocityFactor=", velocityFactor, " for type=", type, "\n")
            if (NBC > 0L) {
                iv <- gappyIndex(i, i0v, 2L*NBC)
                oceDebug(debug, vectorShow(i))
                oceDebug(debug, vectorShow(i0v))
                # FIXME: use proper velocity factor
                v <- velocityFactor*readBin(d$buf[iv], "integer", size=2L, n=NP*NBC, endian="little")
                object$v <- array(double(), dim=c(NP, NC, NB))
                for (ip in 1:NP) {
                    look <- seq(1L+(ip-1L)*NBC, length.out=NBC)
                    #if (ip < 5L) # FIXME: remove this
                    #    cat("ip=",ip,": ",vectorShow(look))
                    object$v[ip,,] <- matrix(v[look], ncol=NB, byrow=FALSE)
                }
                i0v <<- i0v + 2L * NBC
            }
        } else if (name == "a") {
            oceDebug(debug, "   a starts at i0v=", i0v, "\n")
            if (NBC > 0L) {
                iv <- gappyIndex(i, i0v, NBC)
                a <- readBin(d$buf[iv], "raw", size=1L, n=NP*NBC, endian="little")
                object$a <- array(raw(), dim=c(NP, NC, NB))
                for (ip in 1:NP) {
                    look <- seq(1L+(ip-1L)*NBC, length.out=NBC)
                    object$a[ip,,] <- matrix(a[look], ncol=NB, byrow=FALSE)
                }
                i0v <<- i0v + NBC
            }
        } else if (name == "q") {
            oceDebug(debug, "   q starts at i0v=", i0v, "\n")
            if (NBC > 0L) {
                iv <- gappyIndex(i, i0v, NBC)
                q <- readBin(d$buf[iv], "raw", size=1L, n=NP*NBC, endian="little")
                object$q <- array(q, dim=c(NP, NB, NC))
                q <- readBin(d$buf[iv], "raw", size=1L, n=NP*NBC, endian="little")
                object$q <- array(raw(), dim=c(NP, NC, NB))
                for (ip in 1:NP) {
                    look <- seq(1L+(ip-1L)*NBC, length.out=NBC)
                    object$q[ip,,] <- matrix(q[look], ncol=NB, byrow=FALSE)
                }
                i0v <<- i0v + NBC
            }
        } else if (name == "altimeter") {
            # Nortek 2022 p89 (top) states altimeterDistance is int32, but the
            # values obtained with that setting ar crazy (e.g. 1109925788 m),
            # and Nortek 2017 p51 bottom states that it is a float value.
            oceDebug(debug, "   altimeter starts at i0v=", i0v, "\n")
            iv <- gappyIndex(i, i0v, 4L)
            object$altimeter <- list()
            object$altimeter$distance <- readBin(buf[iv], "numeric", size=4L, n=NP, endian="little", signed=TRUE)
            #message(vectorShow(object$altimeterDistance))
            i0v <<- i0v + 4L
            iv <- gappyIndex(i, i0v, 2L)
            object$altimeter$quality <- readBin(buf[iv], "integer", size=2L, n=NP, endian="little", signed=FALSE)
            i0v <<- i0v + 2L
            iv <- gappyIndex(i, i0v, 2L)
            object$altimeter$status <- readBin(buf[iv], "integer", size=2L, n=NP, endian="little", signed=FALSE)
            i0v <<- i0v + 2L
        } else if (name == "AST") {
            oceDebug(debug, "   AST starts at i0v=", i0v, "\n")
            iv <- gappyIndex(i, i0v, 4L)
            object$AST <- list()
            object$AST$distance <- readBin(buf[iv], "numeric", size=4L, n=NP, endian="little")
            #message(vectorShow(object$ASTDistance))
            i0v <<- i0v + 4L
            iv <- gappyIndex(i, i0v, 2L)
            object$AST$quality <- readBin(buf[iv], "integer", size=2L, n=NP, endian="little", signed=FALSE)
            i0v <<- i0v + 2L
            iv <- gappyIndex(i, i0v, 2L)
            object$AST$offset <- readBin(buf[iv], "integer", size=2L, n=NP, endian="little", signed=TRUE)
            i0v <<- i0v + 2L
            iv <- gappyIndex(i, i0v, 4L)
            object$AST$pressure <- readBin(buf[iv], "numeric", size=4L, n=NP, endian="little")
            #message(vectorShow(object$ASTPressure))
            i0v <<- i0v + 4L
            # The 2017 manual states there are 8 more bytes, named 'spare', and
            # the 2022 manual agrees that there is an 8-byte spacer, by stating
            # that the next occurs at ALTIRAW+8).
            i0v <<- i0v + 8L
        } else if (name == "altimeterRaw") {
            oceDebug(debug, "   altimeterRaw starts at i0v=", i0v, "\n")
            # sampling characteristics
            object$altimeterRaw <- list()
            iv <- gappyIndex(i, i0v, 4L)
            NS <- readBin(buf[iv], "integer", size=4L, n=NP, endian="little") # no. samples (tmp var)
            dNS <- diff(range(NS))
            if (0 != dNS)
                stop("altimeterRawNumberOfSamples not all equal.  Range is ", dNS[1], " to ", dNS[2])
            NS <- NS[1]
            object$altimeterRaw$numberOfSamples <- NS
            object$altimeterRaw$blankingDistance <- object$blankingDistance
            i0v <<- i0v + 4L # skip the 4 bytes we just read
            iv <- gappyIndex(i, i0v, 2L)
            object$altimeterRaw$sampleDistance <-
                1e-4 * readBin(buf[iv], "integer", size=2L, n=1, endian="little", signed=FALSE)
            # data
            object$altimeterRaw$time <- object$time
            i0v <<- i0v + 2L
            iv <- gappyIndex(i, i0v, 2L*NS)
            tmp <- readBin(buf[iv], "integer", size=2L, endian="little", n=NP*NS)
            object$altimeterRaw$samples <- matrix(tmp, nrow=NP, ncol=NS, byrow=FALSE)
            i0v <<- i0v + 2L*NS
            # Constructed vector of altimeterRaw sample distances.
            object$altimeterRaw$distance <-
                object$blankingDistance + object$altimeterRaw$sampleDistance * seq_len(object$altimeterRaw$numberOfSamples)
        } else if (name == "echosounder") {
            # Nortek (2017 p52): each profile has NC*16 bits
            oceDebug(debug, "   echosounder starts at i0v=", i0v, " (NC=", NC, ", NP=", NP, ")\n")
            iv <- gappyIndex(i, i0v, 2L*NC)
            #iv <- gappyIndex(i, i0v, 2L*NC)
            tmp <- readBin(buf[iv], "integer", size=2L, endian="little", n=NP*NC)
            object$echosounder <- matrix(tmp, nrow=NP, byrow=FALSE)
            i0v <<- i0v + 2L*NC
        } else if (name == "AHRS") {
            oceDebug(debug, "   AHRS starts at i0v=", i0v, "\n")
            # AHRSRotationMatrix
            object$AHRS <- list(rotationMatrix=NULL,
                quaternions=list(w=NULL, x=NULL, y=NULL, z=NULL),
                gyro=list(x=NULL, y=NULL, z=NULL))
            object$AHRS$rotationMatrix <- array(double(), dim=c(NP, 3L, 3L))
            iv <- gappyIndex(i, i0v, 9L*4L)
            tmp <- readBin(buf[iv], "numeric", size=4L, endian="little", n=NP*9L)
            for (ip in 1:NP) {
                look <- seq(1L+(ip-1L)*9L, length.out=9L)
                # read by row, given docs say M11, then M12, then M13, etc.
                object$AHRS$rotationMatrix[ip,,] <- matrix(tmp[look], ncol=3, byrow=TRUE) # note byrow
            }
            i0v <<- i0v + 9L*4L
            # AHSR$quaternions$w, $x, $y and $z
            iv <- gappyIndex(i, i0v, 4L)
            object$AHRS$quaternions$w <- readBin(buf[iv], "numeric", size=4L, endian="little", n=NP)
            i0v <<- i0v + 4L
            iv <- gappyIndex(i, i0v, 4L)
            object$AHRS$quaternions$x <- readBin(buf[iv], "numeric", size=4L, endian="little", n=NP)
            i0v <<- i0v + 4L
            iv <- gappyIndex(i, i0v, 4L)
            object$AHRS$quaternions$y <- readBin(buf[iv], "numeric", size=4L, endian="little", n=NP)
            i0v <<- i0v + 4L
            iv <- gappyIndex(i, i0v, 4L)
            object$AHRS$quaternions$z <- readBin(buf[iv], "numeric", size=4L, endian="little", n=NP)
            i0v <<- i0v + 4L
            # AHSR$gyro$x, $y, $z
            iv <- gappyIndex(i, i0v, 4L)
            object$AHRS$gyro$x <- readBin(buf[iv], "numeric", size=4L, endian="little", n=NP)
            i0v <<- i0v + 4L
            iv <- gappyIndex(i, i0v, 4L)
            object$AHRS$gyro$y <- readBin(buf[iv], "numeric", size=4L, endian="little", n=NP)
            i0v <<- i0v + 4L
            iv <- gappyIndex(i, i0v, 4L)
            object$AHRS$gyro$z <- readBin(buf[iv], "numeric", size=4L, endian="little", n=NP)
            i0v <<- i0v + 4L
        } else if (name == "percentgood") {
            # Nortek (2017) page 53: 8 bits, unsigned, appears after AHRS gyro z
            oceDebug(debug, "   percentgood starts at i0v=", i0v, "; see Nortek (2017) p53\n")
            iv <- gappyIndex(i, i0v, 4L)
            object$percentgood <- as.integer(buf[iv])
            i0v <<- i0v + 4L
        } else if (name == "stdDev") {
            # Nortek (2017) page 53-54: appears after percentgood
            oceDebug(debug, "    tdDev starts at i0v=", i0v, "; see Nortek (2017) p53-54\n")
            iv <- gappyIndex(i, i0v, 2L)
            object$stdDevPitch <- 0.01*readBin(buf[iv], "numeric", size=2L, endian="little", n=NP)
            i0v <<- i0v + 2L           # advance for next subitem
            iv <- gappyIndex(i, i0v, 2L)
            object$stdDevRoll <- 0.01*readBin(buf[iv], "numeric", size=2L, endian="little", n=NP)
            i0v <<- i0v + 2L           # advance for next subitem
            iv <- gappyIndex(i, i0v, 2L)
            object$stdDevHeading <- 0.01*readBin(buf[iv], "numeric", size=2L, endian="little", n=NP)
            i0v <<- i0v + 2L           # advance for next subitem
            iv <- gappyIndex(i, i0v, 2L)
            object$stdDevPressure <- 0.001*readBin(buf[iv], "numeric", size=2L, endian="little", n=NP)
            i0v <<- i0v + 2L           # advance for next subitem
            iv <- gappyIndex(i, i0v, 2L)
            object$stdDev <- 0.01*readBin(buf[iv], "numeric", size=2L, endian="little", n=NP)
            i0v <<- i0v + 2L           # advance for next subitem
            i0v <<- i0v + 24           # skip over "dummy" (last item listed in N2017 p54)
        } else {
            stop("unknown item, name=\"", name, "\"")
        }
        #oceDebug(debug, "    after handling \"", name, "\", i0v=", i0v, "\n")
        object
    }

    readBurstAltimeterRaw <- function(id, debug=getOption("oceDebug")) # uses global 'd' and 'configuration'
    {
        look <- which(d$id == id)
        oceDebug(debug, vectorShow(look))
        configuration0 <- configuration[look[1],]
        rval <- list(
            configuration=configuration0,
            numberOfBeams=nbeams[look[1]],
            numberOfCells=ncells[look[1]],
            originalCoordinate=coordinateSystem[look[1]],
            oceCoordinate=coordinateSystem[look[1]],
            cellSize=cellSize[look[1]],
            nominalCorrelation=nominalCorrelation[look],
            blankingDistance=blankingDistance[look[1]],
            ensemble=ensemble[look],
            time=time[look],
            orientation=orientation[look],
            soundSpeed=soundSpeed[look],
            temperature=temperature[look], # "temperature pressure sensor"
            pressure=pressure[look],
            heading=heading[look], pitch=pitch[look], roll=roll[look],
            magnetometer=list(
                x=magnetometerx[look],
                y=magnetometery[look],
                z=magnetometerz[look]),
            accelerometer=list(
                x=accelerometerx[look],
                y=accelerometery[look],
                z=accelerometerz[look]),
            datasetDescription=datasetDescription[look],
            temperatureMagnetometer=temperatureMagnetometer[look],
            temperatureRTC=temperatureRTC[look],
            transmitEnergy=transmitEnergy[look],
            powerLevel=powerLevel[look])
        oceDebug(debug, "configuration0=", paste(ifelse(configuration0,"T","F"), collapse=", "), "\n")
        oceDebug(debug, "vector-read 'burstAltimeterRaw' records (0x1a) {\n")
        # See CR's snapshot at
        # https://github.com/dankelley/oce/issues/1959#issuecomment-1141409542
        # which is p89 of Nortek AS. “Signature Integration
        # 55|250|500|1000kHz.” Nortek AS, March 31, 2022)
        i <<- d$index[look]            # pointers to "burstAltimeterRaw" chunks in buf
        oceDebug(debug, vectorShow(i, n=4))
        i0v <<- 77                     # pointer to data (incremented by getItemFromBuf() later).
        NP <- length(i)                # number of profiles of this type
        NC <- rval$numberOfCells       # number of cells for v,a,q
        NB <- rval$numberOfBeams       # number of beams for v,a,q
        p1 <- p$burstAltimeterRaw[1]
        # Nortek (2022 page 89) "Altimeter raw data.NumRawSamples at ALLTIRAWSTART + 8
        #print(configuration0)
        if (configuration0[6])          # read velocity, if included
            rval <- getItemFromBuf(rval, "v", i=i, type=type, debug=debug)
        if (configuration0[7])          # read amplitude, if included
            rval <- getItemFromBuf(rval, "a", i=i, type=type, debug=debug)
        if (configuration0[8])          # read correlation, if included
            rval <- getItemFromBuf(rval, "q", i=i, type=type, debug=debug)
        if (configuration0[9])          # read altimeter, if included
            rval <- getItemFromBuf(rval, "altimeter", i=i, type=type, debug=debug)
        if (configuration0[11])         # read AST, if included
            rval <- getItemFromBuf(rval, "AST", i=i, type=type, debug=debug)
        if (configuration0[10])         # read altimeterRaw, if included
            rval <- getItemFromBuf(rval, "altimeterRaw", i=i, type=type, debug=debug)
        if (configuration0[12])         # read echosounder, if included
            rval <- getItemFromBuf(rval, "echosounder", i=i, type=type, debug=debug)
        if (configuration0[13])         # read AHRS, if included
            rval <- getItemFromBuf(rval, "AHRS", i=i, type=type, debug=debug)
        if (configuration0[14])         # read percentGood, if included
            rval <- getItemFromBuf(rval, "percentgood", i=i, type=type, debug=debug)
        if (configuration0[15])         # read stdDev, if included
            rval <- getItemFromBuf(rval, "stdDev", i=i, type=type, debug=debug)
        oceDebug(debug, "} # vector-read for type=", type, "\n")
        rval
    }                                  # readBurstAltimeterRaw

    # This is intended to handle burst, average, altimeter, ... records:
    # anything but bottom-track.
    readProfile <- function(id, debug=getOption("oceDebug")) # uses global 'd' and 'configuration'
    {
        type <- gsub(".*=","", ad2cpCodeToName(id))
        oceDebug(debug, "readProfile(id=0x", id, ") # i.e. type=", type, "\n")
        # str(d)
        #    List of 4
        #    $ buf   : raw [1:305988694] a5 0a a0 10 ...
        #    $ index : int [1:99] 5530 6704 9254 10428 11602 12776 13950 15124 16298 17472 ...
        #    $ length: int [1:99] 1164 2540 1164 1164 1164 1164 1164 1164 1164 1164 ...
        #    $ id    : int [1:99] 21 22 21 21 21 21 21 21 21 21 ...
        look <- which(d$id == id)
        lookIndex <- d$index[look]
        #message("burstOrAverage: offsetOfData=", vectorShow(as.integer(d$buf[d$index[look[1]] + 2L])))

        oceDebug(debug, vectorShow(look))
        configuration0 <- configuration[look[1],]
        velocityIncluded <- configuration0[6]
        amplitudeIncluded <- configuration0[7]
        correlationIncluded <- configuration0[8]
        altimeterIncluded <- configuration0[9]
        altimeterRawIncluded <- configuration0[10]
        ASTIncluded <- configuration0[11]
        echosounderIncluded <- configuration0[12]
        AHRSIncluded <- configuration0[13]
        percentGoodIncluded<- configuration0[14]
        stdDevIncluded <- configuration0[15]
        oceDebug(debug, vectorShow(velocityIncluded))
        oceDebug(debug, vectorShow(amplitudeIncluded))
        oceDebug(debug, vectorShow(correlationIncluded))
        oceDebug(debug, vectorShow(altimeterIncluded))
        oceDebug(debug, vectorShow(ASTIncluded))
        oceDebug(debug, vectorShow(echosounderIncluded))
        oceDebug(debug, vectorShow(AHRSIncluded))
        oceDebug(debug, vectorShow(percentGoodIncluded))
        oceDebug(debug, vectorShow(stdDevIncluded))

        rval <- list(
            configuration=configuration0,
            numberOfBeams=nbeams[look[1]],
            numberOfCells=ncells[look[1]],
            originalCoordinate=coordinateSystem[look[1]],
            oceCoordinate=coordinateSystem[look[1]],
            cellSize=cellSize[look[1]],
            nominalCorrelation=nominalCorrelation[look],
            blankingDistance=blankingDistance[look[1]],
            ensemble=ensemble[look],
            time=time[look],
            orientation=orientation[look],
            soundSpeed=soundSpeed[look],
            temperature=temperature[look], # "temperature pressure sensor"
            pressure=pressure[look],
            heading=heading[look], pitch=pitch[look], roll=roll[look],
            magnetometer=list(
                x=magnetometerx[look],
                y=magnetometery[look],
                z=magnetometerz[look]),
            accelerometer=list(
                x=accelerometerx[look],
                y=accelerometery[look],
                z=accelerometerz[look]),
            datasetDescription=datasetDescription[look],
            temperatureMagnetometer=temperatureMagnetometer[look],
            temperatureRTC=temperatureRTC[look],
            transmitEnergy=transmitEnergy[look],
            powerLevel=powerLevel[look])
        i <<- d$index[look]            # pointers to "average" chunks in buf
        i0v <<- 77                     # pointer to data (incremented by getItemFromBuf() later).
        NP <- length(i)                # number of profiles of this type
        NC <- rval$numberOfCells       # number of cells for v,a,q
        NB <- rval$numberOfBeams       # number of beams for v,a,q
        rval$distance <- rval$blankingDistance + rval$cellSize * seq_len(rval$numberOfCells)
        oceDebug(debug, "  NP=", NP, ", NB=", NB, ", NC=", NC, "\n", sep="")
        oceDebug(debug, "configuration0=", paste(ifelse(configuration0,"T","F"), collapse=", "), "\n")
        if (configuration0[6])          # read velocity, if included
            rval <- getItemFromBuf(rval, "v", i=i, type=type, debug=debug)
        if (configuration0[7])          # read amplitude, if included
            rval <- getItemFromBuf(rval, "a", i=i, type=type, debug=debug)
        if (configuration0[8])          # read correlation, if included
            rval <- getItemFromBuf(rval, "q", i=i, type=type, debug=debug)
        if (configuration0[9])          # read altimeter, if included
            rval <- getItemFromBuf(rval, "altimeter", i=i, type=type, debug=debug)
        if (configuration0[11])         # read AST, if included
            rval <- getItemFromBuf(rval, "AST", i=i, type=type, debug=debug)
        if (configuration0[10])         # read altimeterRaw, if included
            rval <- getItemFromBuf(rval, "altimeterRaw", i=i, type=type, debug=debug)
        if (configuration0[12])         # read echosounder, if included
            rval <- getItemFromBuf(rval, "echosounder", i=i, type=type, debug=debug)
        if (configuration0[13])         # read AHRS, if included
            rval <- getItemFromBuf(rval, "AHRS", i=i, type=type, debug=debug)
        if (configuration0[14])         # read percentGood, if included
            rval <- getItemFromBuf(rval, "percentgood", i=i, type=type, debug=debug)
        if (configuration0[15])         # read stdDev, if included
            rval <- getItemFromBuf(rval, "stdDev", i=i, type=type, debug=debug)
        oceDebug(debug, "} # vector-read for type=", type, "\n")
        rval
    }                                  # readProfile

    # Nortek (2022 page 93 ) "6.7 _DF20BottomTrack"
    readTrack <- function(id, debug=getOption("oceDebug")) # uses global 'd' and 'configuration'
    {
        # id will be 0x17 for bottomTrack
        type <- gsub(".*=","", ad2cpCodeToName(id))
        oceDebug(debug, "readTrack(id=0x", id, ") # i.e. type=", type, "\n")
        look <- which(d$id == id)
        lookIndex <- d$index[look]
        offsetOfData <- as.integer(d$buf[d$index[look[1]] + 2L])
        oceDebug(debug, "bottom-track (is this 79+1?)", vectorShow(offsetOfData))
        oceDebug(debug, vectorShow(lookIndex))
        configuration0 <- configuration[look[1],]
        velocityIncluded <- configuration0[6]
        amplitudeIncluded <- configuration0[7]
        correlationIncluded <- configuration0[8]
        altimeterIncluded <- configuration0[9]
        altimeterRawIncluded <- configuration0[10]
        ASTIncluded <- configuration0[11]
        echosounderIncluded <- configuration0[12]
        AHRSIncluded <- configuration0[13]
        percentGoodIncluded<- configuration0[14]
        stdDevIncluded <- configuration0[15]
        oceDebug(debug, vectorShow(velocityIncluded))
        oceDebug(debug, vectorShow(amplitudeIncluded))
        oceDebug(debug, vectorShow(correlationIncluded))
        oceDebug(debug, vectorShow(altimeterIncluded))
        oceDebug(debug, vectorShow(ASTIncluded))
        oceDebug(debug, vectorShow(echosounderIncluded))
        oceDebug(debug, vectorShow(AHRSIncluded))
        oceDebug(debug, vectorShow(percentGoodIncluded))
        oceDebug(debug, vectorShow(stdDevIncluded))
        rval <- list(
            configuration=configuration0,
            numberOfBeams=nbeams[look[1]],
            numberOfCells=ncells[look[1]],
            originalCoordinate=coordinateSystem[look[1]],
            oceCoordinate=coordinateSystem[look[1]],
            cellSize=cellSize[look[1]],
            nominalCorrelation=nominalCorrelation[look],
            blankingDistance=blankingDistance[look[1]],
            ensemble=ensemble[look],
            time=time[look],
            orientation=orientation[look],
            soundSpeed=soundSpeed[look],
            temperature=temperature[look], # "temperature pressure sensor"
            pressure=pressure[look],
            heading=heading[look], pitch=pitch[look], roll=roll[look],
            magnetometer=list(
                x=magnetometerx[look],
                y=magnetometery[look],
                z=magnetometerz[look]),
            accelerometer=list(
                x=accelerometerx[look],
                y=accelerometery[look],
                z=accelerometerz[look]),
            datasetDescription=datasetDescription[look],
            temperatureMagnetometer=temperatureMagnetometer[look],
            temperatureRTC=temperatureRTC[look],
            transmitEnergy=transmitEnergy[look],
            powerLevel=powerLevel[look])
        i <- d$index[look]             # pointers to "average" chunks in buf
        oceDebug(debug, "in readTrack: ", vectorShow(i))
        #message(vectorShow(commonData$offsetOfData))
        # IMOS https://github.com/aodn/imos-toolbox/blob/e19c8c604cd062a7212cdedafe11436209336ba5/Parser/readAD2CPBinary.m#L561
        #  IMOS_pointer = oce_pointer - 3
        #  Q: is IMOS taking ambiguity-velocity to
        #  be 2 bytes, as for currents?  My reading
        #  of Nortek (2022 page 80) is that for
        #  _DF20BottomTrack, ambiguity-velocity is 4 bytes, whereas it is 2
        #  bytes for _currentProfileData.  See
        # https://github.com/dankelley/oce/issues/1980#issuecomment-1188992788
        # for more context on this.
        rval$velocityFactor <- 10^readBin(d$buf[lookIndex[1] + 61L], "integer", size=1, n=N, signed=TRUE, endian="little")
        oceDebug(debug, vectorShow(rval$velocityFactor))
        #message(vectorShow(rval$velocityFactor))
        # Nortek (2022 page 94, 52 in zero-indexed notation)
        # IMOS uses idx+52 for ambiguityVelocity
        #   https://github.com/aodn/imos-toolbox/blob/e19c8c604cd062a7212cdedafe11436209336ba5/Parser/readAD2CPBinary.m#L558
        #   IMOS_pointer = oce_pointer - 1
        rval$ambiguityVelocity <- rval$velocityFactor*readBin(d$buf[lookIndex[1] + 53:56], "integer", size=4L, n=1)
        oceDebug(debug, vectorShow(rval$ambiguityVelocity))
        #message(vectorShow(rval$ambiguityVelocity))
        # NOTE: pointer is 2 bytes past pointer for e.g. burst/average
        NP <- length(i)                # number of profiles of this type
        NB <- rval$numberOfBeams       # number of beams for v,a,q
        oceDebug(debug, "  NP=", NP, ", NB=", NB, "\n", sep="")
        oceDebug(debug, "configuration0=", paste(ifelse(configuration0,"T","F"), collapse=", "), "\n")
        # NOTE: imos uses idx+72 for ensembleCounter
        # https://github.com/aodn/imos-toolbox/blob/e19c8c604cd062a7212cdedafe11436209336ba5/Parser/readAD2CPBinary.m#L567
        # oce_pointer = imos_pointer - 3
        i0v <<- 75L
        # ensemble counter Nortek (2017) p62
        iv <- gappyIndex(i, i0v, 4L)
        rval$ensemble <- readBin(d$buf[iv], "integer", size=4L, n=NP, endian="little")
        #message(vectorShow(rval$ensemble))
        #<> #message(vectorShow(commonData$offsetOfData[look]))
        #<> offsetOfData <- commonData$offsetOfData[look]
        #<> #message(vectorShow(offsetOfData))
        #<> if (any(offsetOfData != offsetOfData[1])) {
        #<>     print(offsetOfData)
        #<>     stop("offsetOfData for bottom-track (printed above) are non-uniform")
        #<> }
        i0v <<- i0v + 4L
        # velocity [Nortek 2017 p60 table 6.1.3]
        if (configuration0[6]) {
            #message("reading v with i0v=", i0v, " (NB=", NB, ")")
            #message("FIXME: only read velo if flag is set")
            #message("about to read velo with i[1]=", i[1], ", i0v=",i0v,", NB=", NB)
            #message("configuration0: ", paste(configuration0, collapse=" "))
            #! i0v <- i0v - 2L # test (gives v ~ -14,000 m/s)
            #! i0v <- i0v + 2L # test (gives v ~ -1,500 and -15,000 m/s)
            oceDebug(debug, "reading bottom-track v with velocityFactor=", rval$velocityFactor, ":", vectorShow(i0v))
            iv <- gappyIndex(i, i0v, 4L*NB)
            tmp <- readBin(d$buf[iv], "integer", size=4L, n=NB*NP, endian="little")
            rval$v <- rval$velocityFactor * matrix(tmp, ncol=NB, byrow=FALSE)
            i0v <<- i0v + 4L*NB
        }
        # distance.  See configuration information at Nortek (2017, Table 6.1.3,
        # p60-62) and Nortek (2022, Table 6.7, p93-94).
        if (configuration0[8]) {
            #message("read distance with i0v=", i0v)
            iv <- gappyIndex(i, i0v, 4L*NB)
            oceDebug(debug, "reading bottom-track distance: ", vectorShow(i0v))
            tmp <- readBin(d$buf[iv], "integer", size=4L, n=NB*NP, endian="little")
            rval$distance <- 1e-3 * matrix(tmp, ncol=NB, byrow=FALSE)
            i0v <<- i0v + 4L*NB
        }
        # figure-of-merit [Nortek 2017, Table 6.1.3, pages 60 and 62]
        if (configuration0[9]) {
            #message("read figure-of-merit with i0v=", i0v)
            iv <- gappyIndex(i, i0v, 2L*NB)
            oceDebug(debug, "reading bottom-track figureOfMerit: ", vectorShow(i0v))
            tmp <- readBin(d$buf[iv], "integer", size=2L, n=NB*NP, endian="little", signed=FALSE)
            rval$figureOfMerit <- matrix(tmp, ncol=NB, byrow=FALSE)
            i0v <<- i0v + 2L*NB
        }
        rval
    }                                  # readTrack

    readInterleavedBurst <- function(id, debug=getOption("oceDebug")) # uses global 'd' and 'configuration'
    {
        type <- gsub(".*=","", ad2cpCodeToName(id))
        oceDebug(debug, "readInterleavedBurst(id=0x", id, ") # i.e. type=", type, "\n")
        look <- which(d$id == id)
        lookIndex <- d$index[look]
        oceDebug(debug, vectorShow(lookIndex))
        configuration0 <- configuration[look[1],]
        velocityIncluded <- configuration0[6]
        amplitudeIncluded <- configuration0[7]
        correlationIncluded <- configuration0[8]
        altimeterIncluded <- configuration0[9]
        altimeterRawIncluded <- configuration0[10]
        ASTIncluded <- configuration0[11]
        echosounderIncluded <- configuration0[12]
        AHRSIncluded <- configuration0[13]
        percentGoodIncluded<- configuration0[14]
        stdDevIncluded <- configuration0[15]
        oceDebug(debug, vectorShow(velocityIncluded))
        oceDebug(debug, vectorShow(amplitudeIncluded))
        oceDebug(debug, vectorShow(correlationIncluded))
        oceDebug(debug, vectorShow(altimeterIncluded))
        oceDebug(debug, vectorShow(ASTIncluded))
        oceDebug(debug, vectorShow(echosounderIncluded))
        oceDebug(debug, vectorShow(AHRSIncluded))
        oceDebug(debug, vectorShow(percentGoodIncluded))
        oceDebug(debug, vectorShow(stdDevIncluded))
        rval <- list(
            configuration=configuration0,
            numberOfBeams=nbeams[look[1]],
            numberOfCells=ncells[look[1]],
            originalCoordinate=coordinateSystem[look[1]],
            oceCoordinate=coordinateSystem[look[1]],
            cellSize=cellSize[look[1]],
            nominalCorrelation=nominalCorrelation[look],
            blankingDistance=blankingDistance[look[1]],
            ensemble=ensemble[look],
            time=time[look],
            orientation=orientation[look],
            soundSpeed=soundSpeed[look],
            temperature=temperature[look], # "temperature pressure sensor"
            pressure=pressure[look],
            heading=heading[look], pitch=pitch[look], roll=roll[look],
            magnetometer=list(
                x=magnetometerx[look],
                y=magnetometery[look],
                z=magnetometerz[look]),
            accelerometer=list(
                x=accelerometerx[look],
                y=accelerometery[look],
                z=accelerometerz[look]),
            datasetDescription=datasetDescription[look],
            temperatureMagnetometer=temperatureMagnetometer[look],
            temperatureRTC=temperatureRTC[look],
            transmitEnergy=transmitEnergy[look],
            powerLevel=powerLevel[look])
        i <- d$index[look]             # pointers to "average" chunks in buf
        oceDebug(debug, "in readInterleavedBurst: ", vectorShow(i))
        #message(vectorShow(commonData$offsetOfData))
        # IMOS https://github.com/aodn/imos-toolbox/blob/e19c8c604cd062a7212cdedafe11436209336ba5/Parser/readAD2CPBinary.m#L561
        #  IMOS_pointer = oce_pointer - 3
        #  Q: is IMOS taking ambiguity-velocity to
        #  be 2 bytes, as for currents?  My reading
        #  of Nortek (2022 page 80) is that for
        #  _DF20BottomTrack, ambiguity-velocity is 4 bytes, whereas it is 2
        #  bytes for _currentProfileData.  See
        # https://github.com/dankelley/oce/issues/1980#issuecomment-1188992788
        # for more context on this.
        rval$velocityFactor <- 10^readBin(d$buf[lookIndex[1] + 61L], "integer", size=1, n=N, signed=TRUE, endian="little")
        #message(vectorShow(rval$velocityFactor))
        # Nortek (2022 page 94, 52 in zero-indexed notation)
        # IMOS uses idx+52 for ambiguityVelocity
        #   https://github.com/aodn/imos-toolbox/blob/e19c8c604cd062a7212cdedafe11436209336ba5/Parser/readAD2CPBinary.m#L558
        #   IMOS_pointer = oce_pointer - 1
        rval$ambiguityVelocity <- rval$velocityFactor*readBin(d$buf[lookIndex[1] + 53:56], "integer", size=4L, n=1)
        #message(vectorShow(rval$ambiguityVelocity))
        i0v <<- 77                      # pointer to data (incremented by getItemFromBuf() later).
        NP <- length(i)                # number of profiles of this type
        NB <- rval$numberOfBeams       # number of beams for v,a,q
        oceDebug(debug, "  NP=", NP, ", NB=", NB, "\n", sep="")
        oceDebug(debug, "configuration0=", paste(ifelse(configuration0,"T","F"), collapse=", "), "\n")
        # NOTE: imos uses idx+72 for ensembleCounter
        # https://github.com/aodn/imos-toolbox/blob/e19c8c604cd062a7212cdedafe11436209336ba5/Parser/readAD2CPBinary.m#L567
        # oce_pointer = imos_pointer - 3
        i0v <<- 75L                    # FIXME: don't these start at 77???
        # ensemble counter Nortek (2017) p62
        iv <- gappyIndex(i, i0v, 4L)
        rval$ensemble <- readBin(d$buf[iv], "integer", size=4L, n=NP, endian="little")
        #message(vectorShow(rval$ensemble))
        #message(vectorShow(commonData$offsetOfData[look]))
        offsetOfData <- commonData$offsetOfData[look]
        #message(vectorShow(offsetOfData))
        if (any(offsetOfData != offsetOfData[1])) {
            print(offsetOfData)
            stop("offsetOfData for bottom-track (printed above) are non-uniform")
        }
        i0v <<- 1L + offsetOfData[1]
        # velocity [Nortek 2017 p60 table 6.1.3]
        if (configuration0[6]) {
            #message("FIXME: only read velo if flag is set")
            #message("about to read velo with i[1]=", i[1], ", i0v=",i0v,", NB=", NB)
            #message("configuration0: ", paste(configuration0, collapse=" "))
            iv <- gappyIndex(i, i0v, 4L*NB)
            tmp <- readBin(d$buf[iv], "integer", size=4L, n=NB*NP, endian="little")
            rval$v <- rval$velocityFactor * matrix(tmp, ncol=NB, byrow=FALSE)
            i0v <<- i0v + 4L*NB
        } else {
            #message("no velo data")
        }
        # distance [Nortek 2017, Table 6.1.3, pages 60 and 62]
        if (configuration0[8]) {
            #message("read distance with i0v=", i0v)
            iv <- gappyIndex(i, i0v, 4L*NB)
            tmp <- readBin(d$buf[iv], "integer", size=4L, n=NB*NP, endian="little")
            rval$distance <- 1e-3 * matrix(tmp, ncol=NB, byrow=FALSE)
            i0v <<- i0v + 4L*NB
        }
        # figure-of-merit [Nortek 2017, Table 6.1.3, pages 60 and 62]
        if (configuration0[9]) {
            #message("read figure-of-merit with i0v=", i0v)
            iv <- gappyIndex(i, i0v, 2L*NB)
            tmp <- readBin(d$buf[iv], "integer", size=2L, n=NB*NP, endian="little", signed=FALSE)
            rval$figureOfMerit <- matrix(tmp, ncol=NB, byrow=FALSE)
            i0v <<- i0v + 2L*NB
        }
        rval
    }                                  # readInterleavedBurst

    readEchosounder  <- function(id, debug=getOption("oceDebug")) # uses global 'd' and 'configuration'
    {
        # Nortek (2022 page 87) "Section 6.4 EchosounderDataV3"
        type <- gsub(".*=","", ad2cpCodeToName(id))
        oceDebug(debug, "readEchosounder(id=0x", id, ") # i.e. type=", type, "\n")
        # str(d)
        #    List of 4
        #    $ buf   : raw [1:305988694] a5 0a a0 10 ...
        #    $ index : int [1:99] 5530 6704 9254 10428 11602 12776 13950 15124 16298 17472 ...
        #    $ length: int [1:99] 1164 2540 1164 1164 1164 1164 1164 1164 1164 1164 ...
        #    $ id    : int [1:99] 21 22 21 21 21 21 21 21 21 21 ...
        look <- which(d$id == id)
        oceDebug(debug, vectorShow(look))
        lookIndex <- d$index[look]
        oceDebug(debug, vectorShow(lookIndex))
        # According to Nortek (2022, Section 6.4, page 88), the only
        # configuration flag is whether we have echosounder data, which seems a
        # bit odd because the whole point of an echosounder ID must be that we
        # have echosounder data.  Anyway, we will use that.
        configuration0 <- configuration[look[1],]
        echosounderIncluded0 <- configuration0[12]
        oceDebug(debug, vectorShow(echosounderIncluded0))

        rval <- list(
            configuration=configuration,
            #numberOfBeams=nbeams[look[1]],
            numberOfCells=readBin(d$buf[lookIndex[1] + 31:32],
                "integer", size=2L, n=1, endian="little", signed=FALSE),
            #originalCoordinate=coordinateSystem[look[1]],
            #oceCoordinate=coordinateSystem[look[1]],
            # Nortek (2022 Table 6.4 page 87) does not state a factor on
            # frequency, but a sample file states 500 in the header lines, and
            # the number I read with the next line is 5000, so I assume this a
            # guess worth making.
            frequency=0.1*readBin(d$buf[lookIndex[1]+53:54],
                "integer", size=2L, n=1L, endian="little", signed=FALSE),
            cellSize=cellSize[look[1]],
            nominalCorrelation=nominalCorrelation[look],
            blankingDistance=blankingDistance[look[1]],
            ensemble=ensemble[look],
            time=time[look],
            orientation=orientation[look],
            soundSpeed=soundSpeed[look],
            temperature=temperature[look], # "temperature pressure sensor"
            pressure=pressure[look],
            heading=heading[look], pitch=pitch[look], roll=roll[look],
            magnetometer=list(
                x=magnetometerx[look],
                y=magnetometery[look],
                z=magnetometerz[look]),
            accelerometer=list(
                x=accelerometerx[look],
                y=accelerometery[look],
                z=accelerometerz[look]),
            datasetDescription=datasetDescription[look],
            temperatureMagnetometer=temperatureMagnetometer[look],
            temperatureRTC=temperatureRTC[look],
            transmitEnergy=transmitEnergy[look],
            powerLevel=powerLevel[look])
        rval$distance <- rval$blankingDistance + seq(1, by=rval$cellSize, length.out=rval$numberOfCells)
        i <- d$index[look]            # pointers to "average" chunks in buf
        oceDebug(debug, "in readEchosounder: ", vectorShow(i))

        i0v <<- 77                     # pointer to data (incremented by getItemFromBuf() later).
        NP <- length(i)                # number of profiles of this type
                                       #NC <- rval$numberOfCells       # number of cells for v,a,q
                                       #NB <- rval$numberOfBeams       # number of beams for v,a,q
        #oceDebug(debug, "  NP=", NP, ", NB=", NB, ", NC=", NC, "\n", sep="")
        oceDebug(debug, "configuration0=", paste(ifelse(configuration0,"T","F"), collapse=", "), "\n")
        if (configuration0[12])         # read echosounder, if included
            rval <- getItemFromBuf(rval, "echosounder", i=i, type=type, debug=debug)
        #message(vectorShow(d$index))
        #message(vectorShow(commonData$offsetOfData))

        i <- d$index[which(d$id==id)]
        i0v <<- commonData$offsetOfData[1]
        oceDebug(debug, "set gappyIndex(c(", paste(head(i),collapse=","), "...), ", i0v, ", ", rval$numberOfCells, ") to read n=", NP*rval$numberOfCells, "=NP*numberOfCells uint16 values for echosounder\n")
        iv <- gappyIndex(i, i0v, 2L*rval$numberOfCells)
        E <- readBin(d$buf[iv], "integer", size=2L, n=NP*rval$numberOfCells, endian="little", signed=FALSE)
        # m<-t(matrix(E,nrow=e$numberOfCells,byrow=FALSE));imagep(log10(m))
        rval$echosounder <- t(matrix(E, nrow=rval$numberOfCells, byrow=FALSE))
        oceDebug(debug, "} # vector-read for type=", type, "\n")
        rval
    }                                  # readEchosounder

    data  <- list()

    # The following conditional blocks handle the vectorized reading of various
    # data ID classes.  Although the data format is described in many manuals,
    # this code started with Nortek (2017), DK's copy of which is highly
    # marked-up, and so most citations are in that context (referring to certain
    # page numbers).
    #
    # Note that the second index of configuration, tested below, does not match
    # the order of the data.  In particular notice that the data have order:
    # altimeter/AST/altimeterRaw but these are columns 9/11/10 in the
    # configuration matrix. Careful reading of Nortek (2017) Table 6.1.2 should
    # come before changing the code!
    #
    # BOOKMARK A: vectorized reading
    #
    # Nortek (2017 p 48) "6.1.2 Burst/Average Data Record Definition (DF3)"
    if ("burst" %in% which && length(p$burst) > 0L) # 0x15
        data$burst <- readProfile(id=as.raw(0x15), debug=debug)
    # Nortek (2017 p 48) "6.1.2 Burst/Average Data Record Definition (DF3)"
    if ("average" %in% which && length(p$average) > 0L) # 0x16
        data$average <- readProfile(id=as.raw(0x16), debug=debug)
    # Nortek (2017 p60) "6.1.3 Bottom Track Data Record Definition (DF20)"
    if ("bottomTrack" %in% which && length(p$bottomTrack) > 0) # 0x17
        data$bottomTrack <- readTrack(id=as.raw(0x17), debug=debug)
    if ("interleavedBurst" %in% which && length(p$interleavedBurst) > 0) # 0x18
        data$interleavedBurst <- readProfile(id=as.raw(0x18), debug=debug)
    if ("burstAltimeterRaw" %in% which && length(p$burstAltimeterRaw) > 0L) # 0x1a
        data$burstAltimeterRaw <- readBurstAltimeterRaw(id=as.raw(0x1a), debug=debug-1L)
    if ("DVLBottomTrack" %in% which && length(p$DVLBottomTrack) > 0) # 0x1b
        data$DVLBottomTrack <- readTrack(id=as.raw(0x1b), debug=debug-1L)
    if ("echosounder" %in% which && length(p$echosounder) > 0) # 0x1c
        data$echosounder <- readEchosounder(id=as.raw(0x1c), debug=debug)
    if ("altimeter" %in% which && length(p$altimeter) > 0) # 0x1e
        data$altimeter <- readProfile(id=as.raw(0x1e), debug=debug)
    if ("DVLWaterTrack" %in% which && length(p$DVLWaterTrack) > 0) # 0x1d
        data$DVLWaterTrack <- readTrack(id=as.raw(0x1d), debug=debug)
    if ("averageAltimeter" %in% which && length(p$averageAltimeter) > 0) # 0x1f
        data$averageAltimeter <- readProfile(id=as.raw(0x1f), debug=debug)

    # Insert metadata
    #res@metadata$id <- id
    res@metadata$filename <- filename
    res@metadata$powerLevel <- powerLevel
    res@metadata$status <- status
    res@metadata$activeConfiguration <- activeConfiguration
    res@metadata$orientation <- orientation
    res@metadata$manufacturer <- "nortek"
    res@metadata$fileType <- "AD2CP"
    res@metadata$serialNumber <- serialNumber
    res@metadata$header <- header
    res@metadata$orientation <- orientation

    # Warn if we had to guess the type
    if (!typeGiven) {
        type <- "Signature1000"
        warning("defaulting 'type' to '", type, "', since no header was found in the file, and the 'type' argument was not provided")
    }
    res@metadata$type <- type
    res@metadata$declination <- ad2cpHeaderValue(x=header, key="GETUSER", item="DECL", default=NA)
    res@metadata$frequency <- ad2cpHeaderValue(x=header, key="BEAMCFGLIST,BEAM=1", item="FREQ", default=NA)
    res@metadata$beamAngle <- switch(type, "Signature1000"=25, "Signature500"=25, "Signature250"=20)
    # Note: metadata$transformationMatrix is not defined; we make "[[" compute
    # that, because the user may realize that x@metadata$beamAngle is wrong,
    # and want to correct it.  This makes ad2cp different from other adp
    # types.  Also, we must remove the overall coordinate (created by
    # initializer) since it has no meaning here.
    res@metadata$oceCoordinate <- NULL
    # Insert data
    res@data <- data
    # Insert processingLog
    if (missing(processingLog))
        processingLog <- paste("read.adp.ad2cp(file=\"", filename, "\", from=", from, ", to=", to, ", by=", by, ")", sep="")
    res@processingLog <- processingLogItem(processingLog)
    oceDebug(debug, "} # read.adp.ad2cp()\n", unindent=1, style="bold")
    res
}

#' Plot an AD2CP Object
#'
#' Used by \code{\link{plot,adp-method}} or called directly, this function
#' plots some aspects of AD2CP data. The `which` parameter
#' has an entirely different meaning to that of
#' \code{\link{plot,adp-method}}, because AD2CP objects
#' are laid out differently from other [adp] objects.  As an aide,
#' `which` can be supply prompts that will work with the particular
#' object at hand, e.g. using `plotAD2CP(x,which="?")` will print a message
#' indicating the names of items in the `data` slot that can be plotted.
#' If, say, one of these is `"average"`, then using `which="average/?"` will
#' display a message indicating the items within the `"average"` records that
#' can be plotted.  Some of those items (e.g. `"magnetometer"`) can be
#' explored further, using `which="average/magnetometer/?"`; see
#' Example 3.
#'
#' @param x an AD2CP object, as created with [read.adp.ad2cp()] or by
#' [read.oce()] on a file of the AD2CP type.
#'
#' @param which a character value indicating what to plot.  Use NULL to see a
#' listing of the possibilities for this particular object.  See
#' \dQuote{Details} and \dQuote{Examples}, and note that some understanding
#' of the object layout is required to devise `which` properly.  If `which`
#' is inappropriate for this particular `x`, then hints are printed to help
#' guide the user to something that will work.
#'
#' @param col indication of colour, passed to [imagep()] or to [oce.plot.ts()],
#' depending on whether the plot is an image or a time-series graph. This
#' defaults to [oceColorsVelocity] for velocity images, [oceColorsViridis]
#' for amplitude and quality images, and to black for time-series plots.
#'
#' @param type plot type, used only for time-series
#' graphs.
#'
#' @param lwd linewidth, used only for time-series graphs.
#'
#' @param cex character expansion factor
#'
#' @param pch character code
#'
#' @param ... optional other arguments, passed to the lower-level plotting
#' commands.
#'
#' @examples
#' library(oce)
#' # This example will only work for the author, because it uses a
#' # private file.  The file contains 'burst' and 'average' data.
#' f <- "/Users/kelley/Dropbox/oce_secret_data/ad2cp/secret1_trimmed.ad2cp"
#' if (file.exists(f)) {
#'     library(oce)
#'     d <- read.oce(f)
#'     # Example 1: time-distance variation of "average" velocity (beams 1 through 4)
#'     plot(d, which="average/v", col=oceColorsVelocity)
#'     # Example 2: time variation of "average" amplitude (beam 1)
#'     plot(d, which="average/a/1")
#'     # Example 3: time variation of "burst" magnetometer (x component)
#'     plot(d, which="burst/magnetometer/x")
#'     # Example 4: time variation of "burst" AHRS/gyro
#'     plot(d, which="burst/AHRS/gyro")
#' }
#'
#' @author Dan Kelley
plotAD2CP <- function(x, which=NULL, cex, col, pch, lwd, type, ...)
{
    if (!is.ad2cp(x))
        stop("'x' must be an AD2CP object, e.g. as created with read.adp.ad2cp()")
    names1 <- sort(names(x@data))
    if (is.null(which))
        stop("which must be supplied; try one of: \"", paste(names1, collapse="\", \""), "\"")
    if (!is.character(which[1]))
        stop("'which' must be a character value")
    if (length(which) != 1L)
        stop("'which' must be of length 1")
    if (which == "?") {
        message("try setting 'which' to one of: \"", paste(names1, collapse="\", \""), "\"")
        return(invisible(NULL))
    }
    w <- strsplit(which, "/")[[1]]
    nw <- length(w)
    #message(vectorShow(w))
    if (nw > 4L)
        stop("'which' must contain zero to four \"/\" characters, but it has ", nw)
    if (!w[1] %in% names1)
        stop("unknown which, \"", w[1], "\"; try one of: \"", paste(names1, collapse="\", \""), "\"")
    # Handle some top-level defaults
    #?if (identical(w, "echosounder") && "echosounder" %in% names1) {
    #?    with(x@data$echosounder,
    #?        {
    #?            imagep(time, distance, echosounder, xlab="", ylab=resizableLabel("distance"))
    #?        })
    #?    return(invisible(NULL))
    #?}
    if (nw < 2)
        stop("insufficient detail in 'which'; try e.g. which=\"", names1[1], "/?\" to see possibilities for \"", names1[1], "\" data")
    d <- x@data[[w[1]]]
    time <- d[["time"]]
    ntime <- length(time)
    distance <- d[["distance"]]
    ndistance <- length(distance)
    # Find relevant subitem names, which are for things that can be shown in a
    # time-distance image, or in a variable-time linegraph.
    names2 <- sort(names(d))
    names2keep <- sapply(names2,
        function(x)
        {
            I <- d[[x]]
            if (x == "configuration" || x == "datasetDescription") FALSE
            else if (is.list(I)) TRUE
            else if (is.vector(I) && length(I) == ntime) TRUE
            else if (is.array(I) && dim(I)[1] == ntime && dim(I)[2] == ndistance) TRUE
            else FALSE
        })
    names2 <- names2[names2keep]
    if (nw > 1L && w[2] == "?") {
        message("try setting 'which' to one of: \"", paste(paste0(w[1],"/",names2), collapse="\", \""), "\"")
        return(invisible(NULL))
    }
    if (nw > 2L && w[3] == "?") {
        if (w[2] %in% c("accelerometer", "magnetometer")) {
            message("try setting 'which' to one of: \"", paste0(paste0(w[1],"/",w[2]), "/", c("x","y","z"), collapse="\", \""), "\"")
        } else if (w[2] == "AHRS") {
            message("try setting 'which' to one of: \"", paste0(paste0(w[1],"/",w[2]), "/", c("quaternions","gyro"), collapse="\", \""), "\"")
        } else {
            message("sorry, no hints are available for which=\"", w[1], "/", w[2], "/?")
        }
        return(invisible(NULL))
    }
    if (nw > 3L && w[4] == "?" && w[2] == "AHRS") {
        if (w[3] == "quaternions") {
            message("try setting 'which' to one of: \"", paste0(paste0(w[1],"/",w[2]), "/", w[3], "/", c("x","y","z"), collapse="\", \""), "\"")
        } else if (w[3] == "gyro") {
            message("try setting 'which' to one of: \"", paste0(paste0(w[1],"/",w[2]), "/", w[3], "/", c("x","y","z"), collapse="\", \""), "\"")
        } else {
            stop("the 3th element of 'which' must be \"quaternions\" or \"gyro\", not \"", w[3], "\"")
        }
        return(invisible(NULL))
    }

    #message("next is names(d):");print(names(d),file=stderr())
    opar <- par(no.readonly=TRUE)      # retain so we can reset afterwards, as CRAN requires
    # Note that we are not conditioning on w[1] ... but we might, if that helps
    if (nw == 1L) {
        w[2] <- "v"
        nw <- 2L
    }
    # Ensure that user is asking for a plottable item.
    if (!w[2] %in% names2)
        stop("item \"", w[2], "\" is not available; try one of \"", paste(names2, collapse="\", \""), "\"")
    # Handle by case
    if (w[2] %in% c("a", "q", "v")) {
        D <- makeNumeric(d[[w[2]]])
        nbeam <- dim(D)[3]
        #message(vectorShow(nbeam))
        dots <- list(...)
        dotsNames <- names(dots)
        #cat(str(dots))
        beams <- if (nw < 3L) seq_len(nbeam) else w[3]
        if (length(beams) > 1L)
            par(mfrow=c(length(beams), 1))
        if (missing(col))
            col <- if (w[2] == "v") oceColorsVelocity else oceColorsViridis
        for (ibeam in as.integer(beams)) {
            #message(vectorShow(ibeam))
            if (w[2] == "v" && !"zlim" %in% dotsNames) {
                zlim <- c(-1,1)*max(abs(D[,,ibeam]), na.rm=TRUE)
                imagep(time, distance, D[,,ibeam], zlim=zlim, ylab="Distance [m]", col=col, ...)
            } else {
                imagep(time, distance, D[,,ibeam], ylab="Distance [m]", col=col, ...)
            }
            mtext(paste0(w[2], "[,,",ibeam,"]"), side=3, line=0, adj=1)
        }
        if (length(beams) > 1L)
            par(opar)
    } else if (w[2] == c("echosounder")) {
        D <- x@data$echosounder
        if (missing(col))
            imagep(D$time, D$distance, D$echosounder, ylab=resizableLabel("distance"), ...)
        else
            imagep(D$time, D$distance, D$echosounder, col=col, ylab=resizableLabel("distance"), ...)
    } else if (w[2] == "altimeter") {
        stop("FIXME: add altimeter plot here")
    } else if (w[2] == "altimeterRaw") {
        stop("FIXME: add altimeterRaw plot here")
    } else if (w[2] == "AHRS") {
        if (nw == 2) {
            message("'which' needs more detail; use which=\"", w[1], "/", w[2], "/?\" for hints")
            return(invisible(NULL))
        } else if (nw == 3) {
            if (missing(col))
                col <- 1
            if (w[3] == "gyro") {
                par(mfrow=c(3, 1))
                oce.plot.ts(time, x@data[[w[1]]][[w[2]]][[w[3]]][["x"]], ylab=paste(w[3], "x"),
                    cex=if (missing(cex)) 1 else cex, col=if (missing(col)) 1 else col,
                    pch=if (missing(pch)) 1 else pch, lwd=if (missing(lwd)) 1 else lwd,
                    type=if (missing(type)) 1 else type)
                oce.plot.ts(time, x@data[[w[1]]][[w[2]]][[w[3]]][["y"]], ylab=paste(w[3], "y"),
                    cex=if (missing(cex)) 1 else cex, col=if (missing(col)) 1 else col,
                    pch=if (missing(pch)) 1 else pch, lwd=if (missing(lwd)) 1 else lwd,
                    type=if (missing(type)) 1 else type)
                oce.plot.ts(time, x@data[[w[1]]][[w[2]]][[w[3]]][["z"]], ylab=paste(w[3], "z"),
                    cex=if (missing(cex)) 1 else cex, col=if (missing(col)) 1 else col,
                    pch=if (missing(pch)) 1 else pch, lwd=if (missing(lwd)) 1 else lwd,
                    type=if (missing(type)) 1 else type)
                par(opar)
            } else if (w[3] == "quaternions") {
                par(mfrow=c(4, 1))
                oce.plot.ts(time, x@data[[w[1]]][[w[2]]][[w[3]]][["x"]], ylab=paste(w[3], "x"),
                    cex=if (missing(cex)) 1 else cex, col=if (missing(col)) 1 else col,
                    pch=if (missing(pch)) 1 else pch, lwd=if (missing(lwd)) 1 else lwd,
                    type=if (missing(type)) 1 else type)
                oce.plot.ts(time, x@data[[w[1]]][[w[2]]][[w[3]]][["y"]], ylab=paste(w[3], "y"),
                    cex=if (missing(cex)) 1 else cex, col=if (missing(col)) 1 else col,
                    pch=if (missing(pch)) 1 else pch, lwd=if (missing(lwd)) 1 else lwd,
                    type=if (missing(type)) 1 else type)
                oce.plot.ts(time, x@data[[w[1]]][[w[2]]][[w[3]]][["z"]], ylab=paste(w[3], "z"),
                    cex=if (missing(cex)) 1 else cex, col=if (missing(col)) 1 else col,
                    pch=if (missing(pch)) 1 else pch, lwd=if (missing(lwd)) 1 else lwd,
                    type=if (missing(type)) 1 else type)
                oce.plot.ts(time, x@data[[w[1]]][[w[2]]][[w[3]]][["w"]], ylab=paste(w[3], "w"),
                    cex=if (missing(cex)) 1 else cex, col=if (missing(col)) 1 else col,
                    pch=if (missing(pch)) 1 else pch, lwd=if (missing(lwd)) 1 else lwd,
                    type=if (missing(type)) 1 else type)
                par(opar)
            } else {
                stop("third 'which' entry must be \"gyro\" or \"quaternions\", not \"", w[3], "\"")
            }
        } else if (nw == 4) {
            if (w[3] == "gyro") {
                if (w[4] %in% c("x", "y", "z")) {
                    oce.plot.ts(time, x@data[[w[1]]][[w[2]]][[w[3]]][[w[4]]], ylab=paste(w[3], "x"),
                        cex=if (missing(cex)) 1 else cex, col=if (missing(col)) 1 else col,
                        pch=if (missing(pch)) 1 else pch, lwd=if (missing(lwd)) 1 else lwd,
                        type=if (missing(type)) 1 else type)
                } else {
                    stop("fourth word in which must be \"x\", \"y\" or \"z\", not \"", w[4], "\"")
                }
            } else if (w[3] == "quaternions") {
                if (w[4] %in% c("x", "y", "z", "w")) {
                    oce.plot.ts(time, x@data[[w[1]]][[w[2]]][[w[3]]][[w[4]]], ylab=paste(w[3], "x"),
                        cex=if (missing(cex)) 1 else cex, col=if (missing(col)) 1 else col,
                        pch=if (missing(pch)) 1 else pch, lwd=if (missing(lwd)) 1 else lwd,
                        type=if (missing(type)) 1 else type)
                } else {
                    stop("fourth word in which must be \"x\", \"y\", \"z\" or \"w\", not \"", w[4], "\"")
                }
            } else {
                stop("third 'which' entry must be \"gyro\" or \"quaternions\", not \"", w[3], "\"")
            }
            return(invisible(NULL))
        }
        return(invisible(NULL))
    } else if (w[2] %in% c("accelerometer", "magnetometer")) {
        D <- d[[w[2]]]                 # has components $x, $y and $z
        if (nw == 2) { # plot 3 panels
            par(mfrow=c(3, 1))
            oce.plot.ts(time, D[["x"]], ylab=paste(w[2], "x"),
                cex=if (missing(cex)) 1 else cex, col=if (missing(col)) 1 else col,
                pch=if (missing(pch)) 1 else pch, lwd=if (missing(lwd)) 1 else lwd,
                type=if (missing(type)) 1 else type)
            oce.plot.ts(time, D[["y"]], ylab=paste(w[2], "y"),
                cex=if (missing(cex)) 1 else cex, col=if (missing(col)) 1 else col,
                pch=if (missing(pch)) 1 else pch, lwd=if (missing(lwd)) 1 else lwd,
                type=if (missing(type)) 1 else type)
            oce.plot.ts(time, D[["y"]], ylab=paste(w[2], "z"),
                cex=if (missing(cex)) 1 else cex, col=if (missing(col)) 1 else col,
                pch=if (missing(pch)) 1 else pch, lwd=if (missing(lwd)) 1 else lwd,
                type=if (missing(type)) 1 else type)
            par(opar)
        } else {
            if (!w[3] %in% names(D))
                stop(w[1], "$", w[2], " does not contain \"", w[3], "\"; try one of \"", paste(sort(names(D)), collapse="\" \""), "\"")
            oce.plot.ts(time, D[[w[3]]], ylab=paste(w[2], w[3]),
                cex=if (missing(cex)) 1 else cex, col=if (missing(col)) 1 else col,
                pch=if (missing(pch)) 1 else pch, lwd=if (missing(lwd)) 1 else lwd,
                type=if (missing(type)) 1 else type)
        }
    } else if (length(d[[w[2]]]) == ntime) {
        # time-series graph of some vector element
        oce.plot.ts(time, d[[w[2]]], ylab=paste(w[1], w[2]),
            cex=if (missing(cex)) 1 else cex, col=if (missing(col)) 1 else col,
            pch=if (missing(pch)) 1 else pch, lwd=if (missing(lwd)) 1 else lwd,
            type=if (missing(type)) 1 else type)
    } else {
        stop("although subitem \"", w[2], "\" is present in \"", w[1], "\", it is not handled yet")
    }
}

