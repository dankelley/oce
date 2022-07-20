# vim:textwidth=80:expandtab:shiftwidth=4:softtabstop=4

# private function
ad2cpDefaultDataItem <- function(x, j=NULL,
    order=c("average", "burst", "interleavedBurst",
        "bottomTrack", "burstAltimeter", "DVLBottomTrack"))
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
#' d <- read.oce("a.ad2cp")
#' # The examples start with the line in x[["text"]][[1]]; note that in the second
#' # example, it would be insuficient to use a key of "BEAMCFGLIST", because that will
#' # yield 4 lines, and the function is not designed to handle that.
#'
#' # ID,STR=\"Signature1000\",SN=123456
#' type <- ad2cpHeaderValue(d, "ID", "STR", numeric=FALSE)
#' serialNumber <- ad2cpHeaderValue(d, "ID", "SN")
#'
#' # BEAMCFGLIST,BEAM=1,THETA=25.00,PHI=0.00,FREQ=1000,BW=25,BRD=1,HWBEAM=1,ZNOM=60.00
#' beam1Angle <- ad2cpHeaderValue(d, "BEAMCFGLIST,BEAM=1", "THETA")
#' frequency <- ad2cpHeaderValue(d, "BEAMCFGLIST,BEAM=1", "FREQ", default=NA)
#'}
#'
#' @family things related to adp data
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
#' @param x character value naming an item.
#'
#' @return Logical value indicating whether `x` is an [adp-class] object,
#' with `fileType` in its `metadata` slot equal to `"AD2CP"`.
#'
#' @family things related to adp data
is.ad2cp <- function(x)
{
    if (!inherits(x, "adp")) {
        FALSE
    } else {
        fileType <- x@metadata$fileType
        (!is.null(fileType)) && fileType == "AD2CP"
    }
}

ad2cpCodeToName <- function(code)
{
    #             burst          average bottomTrack interleavedBurst
    #           0x15=21          0x16=22     0x17=23          0x18=24
    # burstAltimeterRaw   DVLBottomTrack echosounder    DVLWaterTrack
    #           0x1a=26          0x1b=27     0x1c=28          0x1d=29
    #         altimeter averageAltimeter        text
    #           0x1e=30          0x1f=31    0xa0=160
    code <- as.integer(code)
    table <- c(burst=0x15, average=0x16, bottomTrack=0x17,
        interleavedBurst=0x18, burstAltimeterRaw=0x1a, DVLBottomTrack=0x1b,
        echosounder=0x1c, DVLWaterTrack=0x1d, altimeter=0x1e,
        averageAltimeter=0x1f, text=0xa0)
    rval <- rep("", length(code))
    for (i in seq_along(code)) {
        m <- match(code[i], table)
        rval[i] <- paste0("0x", as.raw(code[i]), "=", if (is.na(m)) "?" else names(table)[m])
    }
    rval
}

#' Read a Nortek AD2CP File
#'
#' This function reads Nortek AD2CP files, storing data elements in lists within
#' the `data` slot.  So, for example, the following might be a way to read and
#' then access burst altimeter raw data.
#'```
#' d <- read.adp.ad2cp("file.ad2cp", which="burstAltimeterRaw")
#' bar <- d[["burstAltimeterRaw"]]
#'```
#'
#' `read.adp.ad2cp` is incomplete in some important ways, partly because the
#' Nortek manuals are somewhat incomplete and contradictory, and partly because
#' no other software is available for checking results.  The code has been
#' tested with a small number of files that are available to the author, but
#' these do not cover some cases that users might require. Given all of this, it
#' makes sense to use this function with caution; see \dQuote{Cautionary Notes}.
#'
#' Some of the standard `read.adp.*` arguments are handled differently with this
#' function, e.g. `by` must equal 1, because skipping records makes little sense
#' with blended multiple streams. Plus, this function has an extra argument, not
#' provided by other `read.adp.*` functions: `which` may be used to focus on
#' just a particular data stream.
#'
#' @section Cautionary Notes:
#'
#' This function is in active development, and it may produce incorrect results,
#' partly owing to the need to make guesses about the data format, given lack of
#' clarity in various Nortek documents describing the file format, and also
#' contradictions between these documents (see below).
#'
#' In spring of 2022, support was added for 12-byte headers.  These are not
#' described in any Nortek document in the possession of the author of
#' `read.adp.ad2cp(), although some personal communications made via
#' https://github.com/dankelley/oce/issues have exposed some clues that have led
#' to provisional, but largely untested, code here.
#'
#' The \dQuote{References} section lists some manuals that were consulted during
#' the coding of `read.adp.ad2cp`.  Since instruments evolve over time, one
#' might think that Nortek (2022) would be the best place to start, in coding to
#' read AD2CP files. That would be a mistake, and a big one, at that. There
#' are two reasons for this.
#'
#' First, Nortek (2022) is less clear about the data format than Nortek (2017)
#' and Nortek (2018), as exemplified by a few examples.
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
#' of items throughout the data-format tables. In coding `read.adp.ad2cp`,
#' the green "Unit" text was ignored in basically every case.
#'
#' Second, Nortek (2022) contains some very significant errors,
#' e.g. the following.
#'
#' 1. Nortek (2022 page 89) states the storage class for "Altimeter
#' data.Altimeter distance" (called `AltimeterDistance` by the present function)
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
#' @param to an integer indicating the final record to read.
#' (If not provided, `to` defaults to 1e9, and reading stops at the
#' end of the file.)
#'
#' @param which a character value indicating the data type(s) to be read, and
#' stored in the `data` slot of the returned value.  The default, `which="all"`,
#' means to read all the types.  In many cases, though, the user does not want
#' to read everything at once, either as a way to speed processing or to avoid
#' running out of memory.  For this reason, a common first step is instead to
#' use `which="?"`, which gives a table of data types in the file, after which
#' an individual type of interest is extracted.  The choices for that individual
#' type are as follows:
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
#' `"averageAltimeter"` (*not coded yet*) for ID code 0x1f.
#' with each of those vectors holding lines inferred by splitting the string
#' at occurrences of carriage-return/line-feed pairs).
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
#' @param orientation ignored by `read.adp.ad2cp`, and provided only for
#' similarity to other `read.adp.*` functions.
#'
#' @param distance ignored by `read.adp.ad2cp`, and provided only for similarity
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
#' @param despike ignored by `read.adp.ad2cp`, and provided only for similarity
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
#' table (if `which="?"`), or a vector of character (if `which="text"`).
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
#' March 31, 2022.  (This confusing revision may be updated sometime in 2022.)
#'
## Nortek AS. \dQuote{Operations Manual - Signature 250, 500 and 1000.} Nortek AS,
## September 21, 2018.
#'
#' @family things related to adp data
read.adp.ad2cp <- function(file, from=1, to=0, by=1, which="all",
    tz=getOption("oceTz"),
    ignoreChecksums=FALSE,
    longitude=NA, latitude=NA,
    orientation, distance, plan, type,
    monitor=FALSE, despike=FALSE, processingLog,
    debug=getOption("oceDebug"), ...)
{
    if (missing(file))
        stop("must supply 'file'")
    if (is.character(file)) {
        if (!file.exists(file))
            stop("cannot find file '", file, "'")
        if (0L == file.info(file)$size)
            stop("empty file '", file, "'")
    }
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

    oceDebug(debug, "read.adp.ad2cp(...",
        ", from=", if (fromGiven) format(from) else "(missing)",
        ", to=", if (toGiven) to else "(missing)",
        ", by=", if (byGiven) by else "(missing)",
        ", plan=", if (planGiven) plan else "(missing)",
        ", type=\"", if (typeGiven) type else "(missing)",
        ", ignoreChecksums=", ignoreChecksums,
        "\",...)\n", sep="", unindent=1)
    oceDebug(debug, "HINT: set debug=2 or 3 to track more (or even more) processing steps\n")

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
    #DANnav<<-nav;message("FIXME: exported nav as DANnav")
    # Return table of names, in alphabetical order
    if (which == "?") {
        t <- table(nav$id)
        names(t) <- ad2cpCodeToName(names(t))
        o <- order(names(t))
        return(t[o])
    }
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
    # Handle multiple plans (FIXME: this is limited to a single plan, at present)
    status <- intToBits(readBin(d$buf[pointer4 + 69L], "integer", size=4L, n=N, endian="little"))
    # Construct an array to store the bits within th 'status' vector. The nortek
    # docs refer to the first bit as 0, which becomes [1,] in this array.
    dim(status) <- c(32L, N)
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
    if (which == "text") {
        w <- which(d$id == 0xa0)
        rval <- vector("list", length(w))
        for (i in seq_along(w)) {
            chars <- rawToChar(d$buf[seq.int(2L+d$index[w[i]], by=1, length.out=-1+d$length[w[i]])])
            rval[i] <- strsplit(chars, "\r\n")[1]
        }
        return(rval)
    }
    keep <- activeConfiguration == plan
    if (sum(keep) == 0L) {
        stop("there are no data for plan=", plan, "; try one of the following values instead: ", paste(unique(activeConfiguration), collapse=" "))
    }
    #message("exported nav as DANnav");DANnav<<-nav
    #message("exported d as DANd");DANd<<-d
    #message("exported keep as DANkeep");DANkeep<<-keep
    if (sum(keep) < length(keep)) {
        oceDebug(debug, "this plan has ", sum(keep), " data records, out of a total of ", length(keep), " in the file subset\n")
        d$index <- d$index[keep]
        d$length <- d$length[keep]
        d$id <- d$id[keep]
        status <- status[, keep, drop=FALSE]
        activeConfiguration <- activeConfiguration[keep]
        orientation <- orientation[keep]
        N <- sum(keep)
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
    # BCC (beam, coordinate system, and cell) uses packed bits to hold info on
    # the number of beams, coordinate-system, and the number cells. There are
    # two cases [1 page 49]:
    # case 1: Standard bit 9-0 ncell; bit 11-10 coord (00=enu, 01=xyz, 10=beam, 11=NA); bit 15-12 nbeams
    # case 2: bit 15-0 number of echo sounder cells
    # As for 'configuration' above, we set this up as a matrix of 0s and 1s,
    # with rows corresponding to times, for easy transformation into integers.
    # BCC case 1
    BCC <- ifelse(0x01 == rawToBits(d$buf[pointer2 + 31]), 1, 0)
    dim(BCC) <- c(16, N)
    BCC <- t(BCC)
    # Use Horner's rule for clarity (for lispers, anyway!)
    ncells <- BCC[,1]+2*(BCC[,2]+2*(BCC[,3]+2*(BCC[,4]+2*(BCC[,5]+2*(BCC[,6]+2*(BCC[,7]+2*(BCC[,8]+2*(BCC[,9]+2*BCC[,10]))))))))
    nbeams <- BCC[,13]+2*(BCC[,14]+2*(BCC[,15]+2*BCC[,16]))
    # b00=enu, b01=xyz, b10=beam, b11=- [1 page 49]
    coordinateSystem <- c("enu", "xyz", "beam", "?")[1 + BCC[,11] + 2*BCC[,12]]
    # BCC case 2
    ncellsEchosounderWholeFile <- readBin(d$buf[pointer2 + 31], "integer", size=2, n=N, signed=FALSE, endian="little")

    # cell size is recorded in mm [1, table 6.1.2, page 49]
    cellSize <- 0.001 * readBin(d$buf[pointer2 + 33], "integer", size=2, n=N, signed=FALSE, endian="little")
    # blanking distance is recorded in cm [1, table 6.1.2, page 49]
    # NB. blanking may be altered later, if status[2]==0x01
    blankingDistance <- 0.01 * readBin(d$buf[pointer2 + 35], "integer", size=2, n=N, signed=FALSE, endian="little")
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
    # unit of temperatureRTC.  The manual as of 2022 says it is in degC, but a
    # previous manual says it is in 0.01C; the latter produces values that make
    # sense (e.g. approx 20C for an in-air test) so that is the factor we will
    # use here.
    temperatureRTC <- 0.01 * readBin(d$buf[pointer2 + 63], "integer", size=2, n=N, signed=TRUE, endian="little")
    #UNUSED error <- readBin(d$buf[pointer2 + 65], "integer", size=4, n=N, endian="little") # FIXME: UNUSED

    # status0, byte 67:68, skipped
    # status,  byte 69:71, already read above so we could infer activeConfiguration

    # Nortek docs [2 p51] say bit 1 (in 0-offset notation) in 'status' indicates blankingDistance
    # unit, either 0 for m or 1 for cm. (Above, it was read and converted to m, assuming cm.)
    oceDebug(debug, vectorShow(status[2,]))
    oceDebug(debug, vectorShow(blankingDistance))
    blankingDistance <- blankingDistance * ifelse(status[2, ] == 0x01, 1, 0.1)
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
    # 0x17 - Bottom Track Data Record.
    # 0x18 - Interleaved Burst Data Record (beam 5).
    # 0x1A - Burst Altimeter Raw Record.
    # 0x1B - DVL Bottom Track Record.
    # 0x1C - Echo Sounder Record. (cf. IMOS does not handle)
    # 0x1D - DVL Water Track Record. (cf. IMOS does not handle)
    # 0x1E - Altimeter Record.
    # 0x1F - Avg Altimeter Raw Record.
    # 0xA0 - String Data Record, eg. GPS NMEA data, comment from the FWRITE command.
    # Set up pointers to records matching these keys.
    p <- list(burst=which(d$id==0x15), # coded and checked against matlab and .cfg file
        average=which(d$id==0x16),     # coded and checked against matlab and .cfg file
        bottomTrack=which(d$id==0x17), # coded, but no sample-data test and no plot()
        interleavedBurst=which(d$id==0x18), # coded, with no errors reading sample files
        burstAltimeterRaw=which(d$id==0x1a), # https://github.com/dankelley/oce/issues/1959
        DVLBottomTrack=which(d$id==0x1b), # coded, but no sample-data test and no plot()
        echosounder=which(d$id==0x1c), # coded, but no sample-data test and no plot()
        DVLWaterTrack=which(d$id==0x1d), # coded, but no sample-data test and no plot()
        altimeter=which(d$id==0x1e),   # coded, but no sample-data test and no plot()
        averageAltimeter=which(d$id==0x1f)) # coded, but no sample-data test and no plot()

    #x Try to retrieved a named item from the data buffer.
    #x
    #x This checks external variable `haveItem` to see whether the data item
    #x indicated by `name` is available in external raw vector `buf`.  If so, the
    #x item is retrieved from the starting at position indicated by external
    #x variable `i0`, and `i0` is then increased to prepare for the next call to
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
        #oceDebug(debug, "    ", vectorShow(i0v))
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
            object$altimeterDistance <- readBin(buf[iv], "numeric", size=4L, n=NP, endian="little", signed=TRUE)
            #message(vectorShow(object$altimeterDistance))
            i0v <<- i0v + 4L
            iv <- gappyIndex(i, i0v, 2L)
            object$altimeterQuality <- readBin(buf[iv], "integer", size=2L, n=NP, endian="little", signed=FALSE)
            i0v <<- i0v + 2L
            iv <- gappyIndex(i, i0v, 2L)
            object$altimeterStatus <- readBin(buf[iv], "integer", size=2L, n=NP, endian="little", signed=FALSE)
            i0v <<- i0v + 2L
        } else if (name == "AST") {
            oceDebug(debug, "   AST starts at i0v=", i0v, "\n")
            iv <- gappyIndex(i, i0v, 4L)
            object$ASTDistance <- readBin(buf[iv], "numeric", size=4L, n=NP, endian="little")
            #message(vectorShow(object$ASTDistance))
            i0v <<- i0v + 4L
            iv <- gappyIndex(i, i0v, 2L)
            object$ASTQuality <- readBin(buf[iv], "integer", size=2L, n=NP, endian="little", signed=FALSE)
            i0v <<- i0v + 2L
            iv <- gappyIndex(i, i0v, 2L)
            object$ASTOffset <- readBin(buf[iv], "integer", size=2L, n=NP, endian="little", signed=TRUE)
            i0v <<- i0v + 2L
            iv <- gappyIndex(i, i0v, 4L)
            object$ASTPressure <- readBin(buf[iv], "numeric", size=4L, n=NP, endian="little")
            #message(vectorShow(object$ASTPressure))
            i0v <<- i0v + 4L
            # The 2017 manual states there are 8 more bytes, named 'spare', and
            # the 2022 manual agrees that there is an 8-byte spacer, by stating
            # that the next occurs at ALTIRAW+8).
            i0v <<- i0v + 8L
        } else if (name == "altimeterRaw") {
            oceDebug(debug, "   altimeterRaw starts at i0v=", i0v, "\n")
            iv <- gappyIndex(i, i0v, 4L)
            NS <- readBin(buf[iv], "integer", size=4L, n=NP, endian="little") # no. samples (tmp var)
            dNS <- diff(range(NS))
            if (0 != dNS)
                stop("altimeterRawNumberOfSamples not all equal.  Range is ", dNS[1], " to ", dNS[2])
            NS <- NS[1]
            object$altimeterRawNumberOfSamples <- NS
            i0v <<- i0v + 4L # skip the 4 bytes we just read
            # FIXME: OK to assume all altimeterRawSampleDistance values are equal?
            iv <- gappyIndex(i, i0v, 2L)
            object$altimeterRawSampleDistance <-
                1e-4 * readBin(buf[iv], "integer", size=2L, n=1, endian="little", signed=FALSE)
            i0v <<- i0v + 2L
            iv <- gappyIndex(i, i0v, 2L*NS)
            tmp <- readBin(buf[iv], "integer", size=2L, endian="little", n=NP*NS)
            #object$altimeterRawSamples <- matrix(tmp, nrow=NP, ncol=NS, byrow=TRUE) # FIXME: is byrow ok???
            object$altimeterRawSamples <- t(matrix(tmp, nrow=NP, ncol=NS, byrow=FALSE))
            i0v <<- i0v + 2L*NS
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
                quaternions=list(W=NULL, X=NULL, Y=NULL, Z=NULL),
                gyro=list(X=NULL, Y=NULL, Z=NULL))
            object$AHRS$rotationMatrix <- array(double(), dim=c(NP, 3L, 3L))
            iv <- gappyIndex(i, i0v, 9L*4L)
            tmp <- readBin(buf[iv], "numeric", size=4L, endian="little", n=NP*9L)
            for (ip in 1:NP) {
                look <- seq(1L+(ip-1L)*9L, length.out=9L)
                # read by row, given docs say M11, then M12, then M13, etc.
                object$AHRS$rotationMatrix[ip,,] <- matrix(tmp[look], ncol=3, byrow=TRUE) # note byrow
            }
            i0v <<- i0v + 9L*4L
            # AHSR$quaternions$W, $X, $Y and $Z
            iv <- gappyIndex(i, i0v, 4L)
            object$AHRS$quaternions$W <- readBin(buf[iv], "numeric", size=4L, endian="little", n=NP)
            i0v <<- i0v + 4L
            iv <- gappyIndex(i, i0v, 4L)
            object$AHRS$quaternions$X <- readBin(buf[iv], "numeric", size=4L, endian="little", n=NP)
            i0v <<- i0v + 4L
            iv <- gappyIndex(i, i0v, 4L)
            object$AHRS$quaternions$Y <- readBin(buf[iv], "numeric", size=4L, endian="little", n=NP)
            i0v <<- i0v + 4L
            iv <- gappyIndex(i, i0v, 4L)
            object$AHRS$quaternions$Z <- readBin(buf[iv], "numeric", size=4L, endian="little", n=NP)
            i0v <<- i0v + 4L
            # AHSR$gyro$X, $Y, $Z
            iv <- gappyIndex(i, i0v, 4L)
            object$AHRS$gyro$X <- readBin(buf[iv], "numeric", size=4L, endian="little", n=NP)
            i0v <<- i0v + 4L
            iv <- gappyIndex(i, i0v, 4L)
            object$AHRS$gyro$Y <- readBin(buf[iv], "numeric", size=4L, endian="little", n=NP)
            i0v <<- i0v + 4L
            iv <- gappyIndex(i, i0v, 4L)
            object$AHRS$gyro$Z <- readBin(buf[iv], "numeric", size=4L, endian="little", n=NP)
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

    readBurstOrAverage <- function(id, debug=getOption("oceDebug")) # uses global 'd' and 'configuration'
    {
        type <- gsub(".*=","", ad2cpCodeToName(id))
        oceDebug(debug+1L, "readBurstOrAverage(id=0x", id, ") # i.e. type=", type, "\n")
        # str(d)
        #    List of 4
        #    $ buf   : raw [1:305988694] a5 0a a0 10 ...
        #    $ index : int [1:99] 5530 6704 9254 10428 11602 12776 13950 15124 16298 17472 ...
        #    $ length: int [1:99] 1164 2540 1164 1164 1164 1164 1164 1164 1164 1164 ...
        #    $ id    : int [1:99] 21 22 21 21 21 21 21 21 21 21 ...
        look <- which(d$id == id)
        oceDebug(debug+1L, vectorShow(look))
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
        oceDebug(debug+1L, "in readBottomTrack: ", vectorShow(i))
        i0v <<- 77                     # pointer to data (incremented by getItemFromBuf() later).
        NP <- length(i)                # number of profiles of this type
        NC <- rval$numberOfCells       # number of cells for v,a,q
        NB <- rval$numberOfBeams       # number of beams for v,a,q
        oceDebug(debug+1L, "  NP=", NP, ", NB=", NB, ", NC=", NC, "\n", sep="")
        oceDebug(debug, "configuration0=", paste(ifelse(configuration0,"T","F"), collapse=", "), "\n")
        if (configuration0[6])          # read velocity, if included
            rval <- getItemFromBuf(rval, "v", i=i, type=type, debug=debug+1L)
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
        oceDebug(debug+1, "} # vector-read for type=", type, "\n")
        rval
    }

    readBottomTrack <- function(id, debug=getOption("oceDebug")) # uses global 'd' and 'configuration'
    {
        type <- gsub(".*=","", ad2cpCodeToName(id))
        oceDebug(debug+1L, "readBottomTrack(id=0x", id, ") # i.e. type=", type, "\n")
        look <- which(d$id == id)
        lookIndex <- d$index[look]
        oceDebug(debug+1L, vectorShow(lookIndex))
        # blanking
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
        oceDebug(debug+1L, "in readBottomTrack: ", vectorShow(i))
        message(vectorShow(commonData$offsetOfData))
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
        message(vectorShow(rval$velocityFactor))
        # Nortek (2022 page 94, 52 in zero-indexed notation)
        # IMOS uses idx+52 for ambiguityVelocity
        #   https://github.com/aodn/imos-toolbox/blob/e19c8c604cd062a7212cdedafe11436209336ba5/Parser/readAD2CPBinary.m#L558
        #   IMOS_pointer = oce_pointer - 1
        rval$ambiguityVelocity <- rval$velocityFactor*readBin(d$buf[lookIndex[1] + 53:56], "integer", size=4L, n=1)
        message(vectorShow(rval$ambiguityVelocity))
        i0v <- 77                      # pointer to data (incremented by getItemFromBuf() later).
        NP <- length(i)                # number of profiles of this type
        NB <- rval$numberOfBeams       # number of beams for v,a,q
        oceDebug(debug+1L, "  NP=", NP, ", NB=", NB, "\n", sep="")
        oceDebug(debug, "configuration0=", paste(ifelse(configuration0,"T","F"), collapse=", "), "\n")
        # NOTE: imos uses idx+72 for ensembleCounter
        # https://github.com/aodn/imos-toolbox/blob/e19c8c604cd062a7212cdedafe11436209336ba5/Parser/readAD2CPBinary.m#L567
        # oce_pointer = imos_pointer - 3
        i0v <- 75L
        # ensemble counter Nortek (2017) p62
        iv <- gappyIndex(i, i0v, 4L)
        rval$ensemble <- readBin(d$buf[iv], "integer", size=4L, n=NP, endian="little")
        message(vectorShow(rval$ensemble))
        message(vectorShow(commonData$offsetOfData[look]))
        offsetOfData <- commonData$offsetOfData[look]
        message(vectorShow(offsetOfData))
        if (any(offsetOfData != offsetOfData[1])) {
            print(offsetOfData)
            stop("offsetOfData for bottom-track (printed above) are non-uniform")
        }
        i0v <- 1L + offsetOfData[1]
        # velocity [Nortek 2017 p60 table 6.1.3]
        if (configuration0[6]) {
            message("FIXME: only read velo if flag is set")
            message("about to read velo with i[1]=", i[1], ", i0v=",i0v,", NB=", NB)
            message("configuration0: ", paste(configuration0, collapse=" "))
            iv <- gappyIndex(i, i0v, 4L*NB)
            tmp <- readBin(d$buf[iv], "integer", size=4L, n=NB*NP, endian="little")
            rval$v <- rval$velocityFactor * matrix(tmp, ncol=NB, byrow=FALSE)
            i0v <- i0v + 4L*NB
        } else {
            message("no velo data")
        }
        # distance [Nortek 2017, Table 6.1.3, pages 60 and 62]
        if (configuration0[8]) {
            message("read distance with i0v=", i0v)
            iv <- gappyIndex(i, i0v, 4L*NB)
            tmp <- readBin(d$buf[iv], "integer", size=4L, n=NB*NP, endian="little")
            rval$distance <- 1e-3 * matrix(tmp, ncol=NB, byrow=FALSE)
            i0v <- i0v + 4L*NB
        }
        # figure-of-merit [Nortek 2017, Table 6.1.3, pages 60 and 62]
        if (configuration0[9]) {
            message("read figure-of-merit with i0v=", i0v)
            iv <- gappyIndex(i, i0v, 2L*NB)
            tmp <- readBin(d$buf[iv], "integer", size=2L, n=NB*NP, endian="little", signed=FALSE)
            rval$figureOfMerit <- matrix(tmp, ncol=NB, byrow=FALSE)
            i0v <- i0v + 2L*NB
        }
        rval
    }




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
    # Nortek (2017 p 48) "6.1.2 Burst/Average Data Record Definition (DF3)"
    if (length(p$burst) > 0L) {        # vector-read 'burst'=0x15 BOOKMARK=A
        burst <- readBurstOrAverage(id=as.raw(0x15), debug=debug-1L)
        #<> nbeamsBurst <- nbeams[p$burst[1]]
        #<> ncellsBurst <- ncells[p$burst[1]]
        #<> oceDebug(debug, "burst data records: nbeams:", nbeamsBurst, ", ncells:", ncellsBurst, "\n", sep="")
        #<> if (any(nbeams[p$burst] != nbeamsBurst))
        #<>     stop("the 'burst' data records do not all have the same number of beams")
        #<> if (any(ncells[p$burst] != ncellsBurst))
        #<>     stop("the 'burst' data records do not all have the same number of cells")
        #<> burst <- list(i=1,
        #<>     NEWburst=NEWburst,
        #<>     configuration=configuration[p$burst[1],],
        #<>     numberOfBeams=nbeamsBurst,
        #<>     numberOfCells=ncellsBurst,
        #<>     originalCoordinate=coordinateSystem[p$burst[1]],
        #<>     oceCoordinate=coordinateSystem[p$burst[1]],
        #<>     cellSize=cellSize[p$burst[1]],
        #<>     blankingDistance=blankingDistance[p$burst[1]],
        #<>     ensemble=ensemble[p$burst],
        #<>     time=time[p$burst],
        #<>     orientation=orientation[p$burst],
        #<>     soundSpeed=soundSpeed[p$burst],
        #<>     nominalCorrelation=nominalCorrelation[p$burst],
        #<>     temperature=temperature[p$burst], # "temperature pressure sensor"
        #<>     pressure=pressure[p$burst],
        #<>     heading=heading[p$burst], pitch=pitch[p$burst], roll=roll[p$burst],
        #<>     magnetometer=list(x=magnetometerx[p$burst],
        #<>         y=magnetometery[p$burst],
        #<>         z=magnetometerz[p$burst]),
        #<>     accelerometer=list(x=accelerometerx[p$burst],
        #<>         y=accelerometery[p$burst],
        #<>         z=accelerometerz[p$burst]),
        #<>     datasetDescription=datasetDescription[p$burst],
        #<>     transmitEnergy=transmitEnergy[p$burst],
        #<>     powerLevel=powerLevel[p$burst],
        #<>     temperatureMagnetometer=temperatureMagnetometer[p$burst],
        #<>     temperatureRTC=temperatureRTC[p$burst])
        #<> oceDebug(debug, "vector-read 'burst' records (0x15) {\n")
        #<> i <- d$index[which(d$id==0x15)] # pointers to "burst" chunks in buf
        #<> i0v <- 77                      # pointer to data (incremented by getItemFromBuf() later).
        #<> NP <- length(i)                # number of profiles of this type
        #<> oceDebug(debug, "  ", vectorShow(i, n=3))
        #<> NC <- burst$numberOfCells      # number of cells for v,a,q
        #<> NB <- burst$numberOfBeams      # number of beams for v,a,q
        #<> oceDebug(debug, "  NP=", NP, ", NB=", NB, ", NC=", NC, "\n", sep="")
        #<> p1 <- p$burst[1]
        #<> if (configuration[p1, 6])      # read velocity, if included
        #<>     burst <- getItemFromBuf(burst, "v", i=i, type="burst", debug=debug)
        #<> if (configuration[p1, 7])      # read amplitude, if included
        #<>     burst <- getItemFromBuf(burst, "a", i=i, type="burst", debug=debug)
        #<> if (configuration[p1, 8])      # read correlation, if included
        #<>     burst <- getItemFromBuf(burst, "q", i=i, type="burst", debug=debug)
        #<> if (configuration[p1, 9])      # read altimeter, if included
        #<>     burst <- getItemFromBuf(burst, "altimeter", i=i, type="burst", debug=debug)
        #<> if (configuration[p1, 11])      # read AST, if included
        #<>     burst <- getItemFromBuf(burst, "AST", i=i, type="burst", debug=debug)
        #<> if (configuration[p1, 10])      # read altimeterRaw, if included
        #<>     burst <- getItemFromBuf(burst, "altimeterRaw", i=i, type="burst", debug=debug)
        #<> if (configuration[p1, 12])      # read echosounder, if included
        #<>     burst <- getItemFromBuf(burst, "echosounder", i=i, type="burst", debug=debug)
        #<> if (configuration[p1, 13])      # read AHRS, if included
        #<>     burst <- getItemFromBuf(burst, "AHRS", i=i, type="burst", debug=debug)
        #<> if (configuration[p1, 14])      # read percentGood, if included
        #<>     burst <- getItemFromBuf(burst, "percentgood", i=i, type="burst", debug=debug)
        #<> if (configuration[p1, 15])      # read stdDev, if included
        #<>     burst <- getItemFromBuf(burst, "stdDev", i=i, type="burst", debug=debug)
        #<> ch <- p$burst[1] # FiXME: what is this for?
        oceDebug(debug, "} # vector-read 'burst'\n") # 0x15
    } else {
        burst <- NULL
    }
    # Nortek (2017 p 48) "6.1.2 Burst/Average Data Record Definition (DF3)"
    # Nortek (2020 p )
    if (length(p$average) > 0L) {      # vector-read 'average'=0x16
        average <- readBurstOrAverage(id=as.raw(0x16), debug=debug-1L)
        #<> message("FIXME: DAN, is average$NEWaverage ok?")
        #<> average <- list(i=1,
        #<>     NEWaverage=NEWaverage,
        #<>     configuration=configuration[p$average[1],],
        #<>     numberOfBeams=nbeams[p$average[1]],
        #<>     numberOfCells=ncells[p$average[1]],
        #<>     originalCoordinate=coordinateSystem[p$average[1]],
        #<>     oceCoordinate=coordinateSystem[p$average[1]],
        #<>     cellSize=cellSize[p$average[1]],
        #<>     blankingDistance=blankingDistance[p$average[1]],
        #<>     ensemble=ensemble[p$average],
        #<>     time=time[p$average],
        #<>     orientation=orientation[p$average],
        #<>     soundSpeed=soundSpeed[p$average],
        #<>     temperature=temperature[p$average], # "temperature pressure sensor"
        #<>     pressure=pressure[p$average],
        #<>     heading=heading[p$average], pitch=pitch[p$average], roll=roll[p$average],
        #<>     magnetometer=list(x=magnetometerx[p$average],
        #<>         y=magnetometery[p$average],
        #<>         z=magnetometerz[p$average]),
        #<>     accelerometer=list(x=accelerometerx[p$average],
        #<>         y=accelerometery[p$average],
        #<>         z=accelerometerz[p$average]),
        #<>     datasetDescription=datasetDescription[p$average],
        #<>     temperatureMagnetometer=temperatureMagnetometer[p$average],
        #<>     temperatureRTC=temperatureRTC[p$average],
        #<>     transmitEnergy=transmitEnergy[p$average],
        #<>     powerLevel=powerLevel[p$average])
        #<> oceDebug(debug, "vector-read 'average' records (0x16) {\n")
        #<> i <- d$index[which(d$id==0x16)] # pointers to "average" chunks in buf
        #<> oceDebug(debug+1L, "global: ", vectorShow(i))
        #<> i0v <- 77                      # pointer to data (incremented by getItemFromBuf() later).
        #<> NP <- length(i)                # number of profiles of this type
        #<> oceDebug(debug, "  ", vectorShow(i, n=3))
        #<> NC <- average$numberOfCells    # number of cells for v,a,q
        #<> NB <- average$numberOfBeams    # number of beams for v,a,q
        #<> oceDebug(debug+1L, "  NP=", NP, ", NB=", NB, ", NC=", NC, "\n", sep="")
        #<> p1 <- p$average[1]
        #<> if (configuration[p1, 6])      # read velocity, if included
        #<>     average <- getItemFromBuf(average, "v", i=i, type="average", debug=debug+1L)
        #<> if (configuration[p1, 7])      # read amplitude, if included
        #<>     average <- getItemFromBuf(average, "a", i=i, type="average", debug=debug)
        #<> if (configuration[p1, 8])      # read correlation, if included
        #<>     average <- getItemFromBuf(average, "q", i=i, type="average", debug=debug)
        #<> if (configuration[p1, 9])      # read altimeter, if included
        #<>     average <- getItemFromBuf(average, "altimeter", i=i, type="average", debug=debug)
        #<> if (configuration[p1, 11])      # read AST, if included
        #<>     average <- getItemFromBuf(average, "AST", i=i, type="average", debug=debug)
        #<> if (configuration[p1, 10])      # read altimeterRaw, if included
        #<>     average <- getItemFromBuf(average, "altimeterRaw", i=i, type="average", debug=debug)
        #<> if (configuration[p1, 12])      # read echosounder, if included
        #<>     average <- getItemFromBuf(average, "echosounder", i=i, type="average", debug=debug)
        #<> if (configuration[p1, 13])      # read AHRS, if included
        #<>     average <- getItemFromBuf(average, "AHRS", i=i, type="average", debug=debug)
        #<> if (configuration[p1, 14])      # read percentGood, if included
        #<>     average <- getItemFromBuf(average, "percentgood", i=i, type="average", debug=debug)
        #<> if (configuration[p1, 15])      # read stdDev, if included
        #<>     average <- getItemFromBuf(average, "stdDev", i=i, type="average", debug=debug)
        #<> ch <- p$average[1] # FiXME: what is this for?
        #<> oceDebug(debug, "} # vector-read 'average'\n") # 0x15
        #<> ch <- p$average[1] # FiXME: what is this for?
    } else {
        average <- NULL
    }

    # Nortek (2017 p60) "6.1.3 Bottom Track Data Record Definition (DF20)"
    #message(vectorShow(p$bottomTrack))
    if (length(p$bottomTrack) > 0) {   # vector-read bottomTrack==0x17 BOOKMARK=B
        bottomTrack <- readBottomTrack(id=as.raw(0x17), debug=debug-1L)
        #<> NEWbottomTrack <- readBottomTrack(id=as.raw(0x17), debug=debug-1L)
        #<> nbeamsBottomTrack <- nbeams[p$bottomTrack[1]]
        #<> ncellsBottomTrack <- ncells[p$bottomTrack[1]]
        #<> oceDebug(debug, "1+bottomTrack data records: nbeams:", nbeamsBottomTrack, ", ncells:", ncellsBottomTrack, "\n")
        #<> if (any(ncells[p$bottomTrack] != ncellsBottomTrack))
        #<>     stop("the 'bottomTrack' data records do not all have the same number of cells")
        #<> if (any(nbeams[p$bottomTrack] != nbeamsBottomTrack))
        #<>     stop("the 'bottomTrack' data records do not all have the same number of beams")
        #<> # FIXME: read other fields to the following list.
        #<> bottomTrack <- list(i=1,
        #<>     NEWbottomTrack=NEWbottomTrack,
        #<>     configuration=configuration[p$bottomTrack[1]],
        #<>     numberOfCells=ncellsBottomTrack,
        #<>     numberOfBeams=nbeamsBottomTrack,
        #<>     originalCoordinate=coordinateSystem[p$bottomTrack[1]],
        #<>     oceCoordinate=coordinateSystem[p$bottomTrack[1]],
        #<>     cellSize=cellSize[p$bottomTrack[1]],
        #<>     blankingDistance=blankingDistance[p$bottomTrack[1]],
        #<>     ensemble=ensemble[p$bottomTrack],
        #<>     time=time[p$bottomTrack],
        #<>     orientation=orientation[p$bottomTrack],
        #<>     soundSpeed=soundSpeed[p$bottomTrack],
        #<>     #??? datasetDescription=datasetDescription[p$bottomTrack],
        #<>     temperature=temperature[p$bottomTrack],
        #<>     pressure=pressure[p$bottomTrack],
        #<>     heading=heading[p$bottomTrack], pitch=pitch[p$bottomTrack], roll=roll[p$bottomTrack],
        #<>     # nominalCorrelation is not present for bottomTrack
        #<>     magnetometer=list(
        #<>         x=magnetometerx[p$bottomTrack],
        #<>         y=magnetometery[p$bottomTrack], # FIXME: some of these are wrong,
        #<>         z=magnetometerz[p$bottomTrack]), # owing to differences from burst/average
        #<>     accelerometer=list(
        #<>         x=accelerometerx[p$bottomTrack],
        #<>         y=accelerometery[p$bottomTrack],
        #<>         z=accelerometerz[p$bottomTrack])
        #<>     #? temperatureMagnetometer=temperatureMagnetometer[p$bottomTrack],
        #<>     #? temperatureRTC=temperatureRTC[p$bottomTrack],
        #<>     #? transmitEnergy=transmitEnergy[p$bottomTrack],
        #<>     #? powerLevel=powerLevel[p$bottomTrack])
        #<>     )
        #<> # FIXME:vectorize this
        #<> #message("FIXME: working here (need to vectorize bottomTrack reading)")
        #<> if (any(velocityIncluded[p$bottomTrack])) { # FIXME: do allocation later (MARK A)
        #<>     if (1 < length(unique(velocityIncluded[p$bottomTrack])))
        #<>         stop("velocityIncluded values non-unique across 'bottomTrack' data records")
        #<>     bottomTrack$v <- array(double(), dim=c(length(p$bottomTrack), nbeamsBottomTrack))
        #<> }
        #<> if (any(altimeterIncluded[p$bottomTrack])) { # note name-shift from average/burst data
        #<>     if (1 < length(unique(altimeterIncluded[p$bottomTrack])))
        #<>         stop("altimeterIncluded values non-unique across 'bottomTrack' data records")
        #<>     bottomTrack$altimeterDistance <- array(double(), dim=c(length(p$bottomTrack), nbeamsBottomTrack))
        #<> }
        #<> if (any(altimeterRawIncluded[p$bottomTrack])) { # note name-shift from average/burst data
        #<>     bottomTrack$altimeterFigureOfMerit <- array(double(), dim=c(length(p$bottomTrack), nbeamsBottomTrack))
        #<> }
    } else {
        bottomTrack <- NULL
    }

    if (length(p$interleavedBurst) > 0) { # key=0x18
        #if (any(version[p$interleavedBurst] != 3))
        #    stop("can only decode 'interleavedBurst' data records that are in 'version 3' format")
        nbeamsInterleavedBurst <- nbeams[p$interleavedBurst[1]]
        ncellsInterleavedBurst <- ncells[p$interleavedBurst[1]]
        oceDebug(debug, "interleavedBurst data records: nbeams:", nbeamsInterleavedBurst, ", ncells:", ncellsInterleavedBurst, "\n")
        if (any(ncells[p$interleavedBurst] != ncellsInterleavedBurst))
            stop("the 'interleavedBurst' data records do not all have the same number of cells")
        if (any(nbeams[p$interleavedBurst] != nbeamsInterleavedBurst))
            stop("the 'interleavedBurst' data records do not all have the same number of beams")
        # FIXME: read other fields to the following list.
        interleavedBurst <- list(i=1,
            numberOfCells=ncellsInterleavedBurst,
            numberOfBeams=nbeamsInterleavedBurst,
            originalCoordinate=coordinateSystem[p$interleavedBurst[1]],
            oceCoordinate=coordinateSystem[p$interleavedBurst[1]],
            cellSize=cellSize[p$interleavedBurst[1]],
            blankingDistance=blankingDistance[p$interleavedBurst[1]],
            ensemble=ensemble[p$interleavedBurst],
            time=time[p$interleavedBurst],
            orientation=orientation[p$interleavedBurst],
            heading=heading[p$interleavedBurst],
            pitch=pitch[p$interleavedBurst],
            roll=roll[p$interleavedBurst],
            pressure=pressure[p$interleavedBurst],
            temperature=temperature[p$interleavedBurst],
            temperatureMagnetometer=temperatureMagnetometer[p$interleavedBurst],
            temperatureRTC=temperatureRTC[p$interleavedBurst],
            soundSpeed=soundSpeed[p$interleavedBurst],
            magnetometerx=magnetometerx[p$interleavedBurst],
            magnetometery=magnetometery[p$interleavedBurst],
            magnetometerz=magnetometerz[p$interleavedBurst],
            accelerometerx=accelerometerx[p$interleavedBurst],
            accelerometery=accelerometery[p$interleavedBurst],
            accelerometerz=accelerometerz[p$interleavedBurst],
            nominalCorrelation=nominalCorrelation[p$interleavedBurst],
            datasetDescription=datasetDescription[p$interleavedBurst],
            transmitEnergy=transmitEnergy[p$interleavedBurst],
            powerLevel=powerLevel[p$interleavedBurst])
        if (any(velocityIncluded[p$interleavedBurst])) {
            if (1 < length(unique(velocityIncluded[p$interleavedBurst])))
                stop("velocityIncluded values non-unique across 'interleavedBurst' data records")
            interleavedBurst$v <- array(double(), dim=c(length(p$interleavedBurst), ncellsInterleavedBurst, nbeamsInterleavedBurst))
        }
        if (any(amplitudeIncluded[p$interleavedBurst])) {
            if (1 < length(unique(amplitudeIncluded[p$interleavedBurst])))
                stop("amplitudeIncluded values non-unique across 'interleavedBurst' data records")
            interleavedBurst$a <- array(raw(), dim=c(length(p$interleavedBurst), ncellsInterleavedBurst, nbeamsInterleavedBurst))
        }
        if (any(correlationIncluded[p$interleavedBurst])) {
            if (1 < length(unique(correlationIncluded[p$interleavedBurst])))
                stop("correlationIncluded values non-unique across 'interleavedBurst' data records")
            interleavedBurst$q <- array(raw(), dim=c(length(p$interleavedBurst), ncellsInterleavedBurst, nbeamsInterleavedBurst))
        }
        if (any(altimeterIncluded[p$interleavedBurst])) {
            if (1 < length(unique(altimeterIncluded[p$burst])))
                stop("altimeterIncluded values non-unique across 'interleavedBurst' data records")
            interleavedBurst$altimeterDistance <- vector("numeric", length(p$interleavedBurst))
        }
        if (any(ASTIncluded[p$interleavedBurst])) {
            interleavedBurst$ASTDistance <- vector("numeric", length(p$interleavedBurst))
            interleavedBurst$ASTPressure <- vector("numeric", length(p$interleavedBurst))
        }
        if (any(echosounderIncluded[p$interleavedBurst])) {
            interleavedBurst$echosounder <- matrix(double(), ncol=length(p$interleavedBurst), nrow=ncellsInterleavedBurst)
        }
        if (any(AHRSIncluded[p$interleavedBurst])) {
            interleavedBurst$AHRS <- matrix(numeric(), nrow=length(p$interleavedBurst), ncol=9)
        }
    } else {
        interleavedBurst <- NULL
    }

    if (length(p$burstAltimeterRaw) > 0L) { # key=0x1a (burst Altimeter raw record)
        oceDebug(debug, "length(p$burstAltimeterRaw)=", length(p$burstAltimeterRaw), "\n", sep="")
        #if (any(version[p$burstAltimeter] != 3))
        #    stop("can only decode 'burstAltimeter' data records that are in 'version 3' format")
        message("FIXME L1093")
        nbeamsBurstAltimeterRaw <- nbeams[p$burstAltimeterRaw[1]]
        ncellsBurstAltimeterRaw <- ncells[p$burstAltimeterRaw[1]]
        oceDebug(debug+1, "burstAltimeterRaw data records: nbeams:", nbeamsBurstAltimeterRaw, ", ncells:", ncellsBurstAltimeterRaw, "\n")
        if (any(ncells[p$burstAltimeterRaw] != ncellsBurstAltimeterRaw))
            stop("the 'burstAltimeterRaw' data records do not all have the same number of cells")
        if (any(nbeams[p$burstAltimeterRaw] != nbeamsBurstAltimeterRaw))
            stop("the 'burstAltimeterRaw' data records do not all have the same number of beams")
        # FIXME: read other fields to the following list.
        burstAltimeterRaw <- list(i=1,
            numberOfCells=ncellsBurstAltimeterRaw,
            numberOfBeams=nbeamsBurstAltimeterRaw,
            numberOfProfiles=length(p$burstAltimeterRaw),
            originalCoordinate=coordinateSystem[p$burstAltimeterRaw[1]],
            oceCoordinate=coordinateSystem[p$burstAltimeterRaw[1]],
            cellSize=cellSize[p$burstAltimeterRaw[1]],
            blankingDistance=blankingDistance[p$burstAltimeterRaw[1]],
            ensemble=ensemble[p$burstAltimeterRaw],
            time=time[p$burstAltimeterRaw],
            orientation=orientation[p$burstAltimeterRaw],
            heading=heading[p$burstAltimeterRaw],
            pitch=pitch[p$burstAltimeterRaw],
            roll=roll[p$burstAltimeterRaw],
            pressure=pressure[p$burstAltimeterRaw],
            temperature=temperature[p$burstAltimeterRaw],
            temperatureMagnetometer=temperatureMagnetometer[p$burstAltimeterRaw],
            temperatureRTC=temperatureRTC[p$burstAltimeterRaw],
            soundSpeed=soundSpeed[p$burstAltimeterRaw],
            magnetometerx=magnetometerx[p$burstAltimeterRaw],
            magnetometery=magnetometery[p$burstAltimeterRaw],
            magnetometerz=magnetometerz[p$burstAltimeterRaw],
            accelerometerx=accelerometerx[p$burstAltimeterRaw],
            accelerometery=accelerometery[p$burstAltimeterRaw],
            accelerometerz=accelerometerz[p$burstAltimeterRaw],
            nominalCorrelation=nominalCorrelation[p$burstAltimeterRaw],
            datasetDescription=datasetDescription[p$burstAltimeterRaw],
            transmitEnergy=transmitEnergy[p$burstAltimeterRaw],
            powerLevel=powerLevel[p$burstAltimeterRaw])
        # burstAltimeterRaw: vectorized
        oceDebug(debug, "vector-read 'burstAltimeterRaw' records (0x1a) {\n")
            # See CR's snapshot at
            # https://github.com/dankelley/oce/issues/1959#issuecomment-1141409542
            # which is p89 of Nortek AS. “Signature Integration
            # 55|250|500|1000kHz.” Nortek AS, March 31, 2022)
            i <- d$index[which(d$id==0x1a)] # pointers to "burstAltimeterRaw" chunks in buf
            i0v <- 77                      # pointer to data (incremented by getItemFromBuf() later).
            NP <- length(i)            # number of profiles of this type
            oceDebug(debug, vectorShow(i, n=4))
            NC <- burstAltimeterRaw$numberOfCells # number of cells for v,a,q
            NB <- burstAltimeterRaw$numberOfBeams # number of beams for v,a,q
            p1 <- p$burstAltimeterRaw[1]
            if (velocityIncluded[p1])
                burstAltimeterRaw <- getItemFromBuf(burstAltimeterRaw, "v", i=i, type="burstAltimeterRaw", debug=debug)
            if (amplitudeIncluded[p1])
                burstAltimeterRaw <- getItemFromBuf(burstAltimeterRaw, "a", i=i, type="burstAltimeterRaw", debug=debug)
            if (correlationIncluded[p1])
                burstAltimeterRaw <- getItemFromBuf(burstAltimeterRaw, "q", i=i, type="burstAltimeterRaw", debug=debug)
            if (altimeterIncluded[p1])
                burstAltimeterRaw <- getItemFromBuf(burstAltimeterRaw, "altimeter", i=i, type="burstAltimeterRaw", debug=debug)
            if (ASTIncluded[p1])
                burstAltimeterRaw <- getItemFromBuf(burstAltimeterRaw, "AST", i=i, type="burstAltimeterRaw", debug=debug)
            if (altimeterRawIncluded[p1])
                burstAltimeterRaw <- getItemFromBuf(burstAltimeterRaw, "altimeterRaw", i=i, type="burstAltimeterRaw", debug=debug)
            if (echosounderIncluded[p1])
                burstAltimeterRaw <- getItemFromBuf(burstAltimeterRaw, "echosounder", i=i, type="burstAltimeterRaw", debug=debug)
            if (AHRSIncluded[p1])
                burstAltimeterRaw <- getItemFromBuf(burstAltimeterRaw, "AHRS", i=i, type="burstAltimeterRaw", debug=debug)
            if (percentGoodIncluded[p1])
                burstAltimeterRaw <- getItemFromBuf(burstAltimeterRaw, "percentgood", i=i, type="burstAltimeterRaw", debug=debug)
            if (stdDevIncluded[p1])
                burstAltimeterRaw <- getItemFromBuf(burstAltimeterRaw, "stdDev", i=i, type="burstAltimeterRaw", debug=debug)
            ch <- p$burstAltimeterRaw[1] # FiXME: what is this for?
            #cat(file=stderr(), vectorShow(p$burstAltimeterRaw))
            oceDebug(debug, "} # vector-read burstAltimeterRaw\n")
    } else {
        ## FIXME DAN DAN DAN DAN
        burstAltimeterRaw <- NULL
    }
    #save(burstAltimeterRaw, file="dan.rda")
    #message("see burstAltimeterRaw in dan.rda")
    #return(burstAltimeterRaw)

    if (length(p$DVLBottomTrack) > 0) { # key=0x1b
        #if (any(version[p$DVLBottomTrack] != 3))
        #    stop("can only decode 'DVLBottomTrack' data records that are in 'version 3' format")
        nbeamsDVLBottomTrack <- nbeams[p$DVLBottomTrack[1]]
        ncellsDVLBottomTrack <- ncells[p$DVLBottomTrack[1]]
        oceDebug(debug, "DVLBottomTrack data records: nbeams:", nbeamsDVLBottomTrack, ", ncells:", ncellsDVLBottomTrack, "\n")
        if (any(ncells[p$DVLBottomTrack] != ncellsDVLBottomTrack))
            stop("the 'DVLBottomTrack' data records do not all have the same number of cells")
        if (any(nbeams[p$DVLBottomTrack] != nbeamsDVLBottomTrack))
            stop("the 'DVLBottomTrack' data records do not all have the same number of beams")
        # FIXME: read other fields to the following list.
        DVLBottomTrack <- list(i=1,
            numberOfCells=ncellsDVLBottomTrack,
            numberOfBeams=nbeamsDVLBottomTrack,
            originalCoordinate=coordinateSystem[p$DVLBottomTrack[1]],
            oceCoordinate=coordinateSystem[p$DVLBottomTrack[1]],
            cellSize=cellSize[p$DVLBottomTrack[1]],
            blankingDistance=blankingDistance[p$DVLBottomTrack[1]],
            ensemble=ensemble[p$DVLBottomTrack],
            time=time[p$DVLBottomTrack],
            orientation=orientation[p$DVLBottomTrack],
            heading=heading[p$DVLBottomTrack],
            pitch=pitch[p$DVLBottomTrack],
            roll=roll[p$DVLBottomTrack],
            pressure=pressure[p$DVLBottomTrack],
            temperature=temperature[p$DVLBottomTrack],
            temperatureMagnetometer=temperatureMagnetometer[p$DVLBottomTrack],
            temperatureRTC=temperatureRTC[p$DVLBottomTrack],
            soundSpeed=soundSpeed[p$DVLBottomTrack],
            magnetometerx=magnetometerx[p$DVLBottomTrack],
            magnetometery=magnetometery[p$DVLBottomTrack],
            magnetometerz=magnetometerz[p$DVLBottomTrack],
            accelerometerx=accelerometerx[p$DVLBottomTrack],
            accelerometery=accelerometery[p$DVLBottomTrack],
            accelerometerz=accelerometerz[p$DVLBottomTrack],
            nominalCorrelation=nominalCorrelation[p$DVLBottomTrack],
            datasetDescription=datasetDescription[p$DVLBottomTrack],
            transmitEnergy=transmitEnergy[p$DVLBottomTrack],
            powerLevel=powerLevel[p$DVLBottomTrack])
        if (any(velocityIncluded[p$DVLBottomTrack])) {
            if (1 < length(unique(velocityIncluded[p$DVLBottomTrack])))
                stop("velocityIncluded values non-unique across 'DVLBottomTrack' data records")
            DVLBottomTrack$v <- array(double(), dim=c(length(p$DVLBottomTrack), ncellsDVLBottomTrack, nbeamsDVLBottomTrack))
        }
        if (any(amplitudeIncluded[p$DVLBottomTrack])) {
            if (1 < length(unique(amplitudeIncluded[p$DVLBottomTrack])))
                stop("amplitudeIncluded values non-unique across 'DVLBottomTrack' data records")
            DVLBottomTrack$a <- array(raw(), dim=c(length(p$DVLBottomTrack), ncellsDVLBottomTrack, nbeamsDVLBottomTrack))
        }
        if (any(correlationIncluded[p$DVLBottomTrack])) {
            if (1 < length(unique(correlationIncluded[p$DVLBottomTrack])))
                stop("correlationIncluded values non-unique across 'DVLBottomTrack' data records")
            DVLBottomTrack$q <- array(raw(), dim=c(length(p$DVLBottomTrack), ncellsDVLBottomTrack, nbeamsDVLBottomTrack))
        }
        if (any(altimeterIncluded[p$DVLBottomTrack])) {
            if (1 < length(unique(altimeterIncluded[p$DVLBottomTrack])))
                stop("altimeterIncluded values non-unique across 'DVLBottomTrack' data records")
            DVLBottomTrack$altimeterDistance <- vector("numeric", length(p$DVLBottomTrack))
        }
        if (any(ASTIncluded[p$DVLBottomTrack])) {
            DVLBottomTrack$ASTDistance <- vector("numeric", length(p$DVLBottomTrack))
            DVLBottomTrack$ASTPressure <- vector("numeric", length(p$DVLBottomTrack))
        }
        if (any(echosounderIncluded[p$DVLBottomTrack])) {
            DVLBottomTrack$echosounder <- matrix(double(), ncol=length(p$DVLBottomTrack), nrow=ncellsDVLBottomTrack)
        }
        if (any(AHRSIncluded[p$DVLBottomTrack])) {
            DVLBottomTrack$AHRS <- matrix(numeric(), nrow=length(p$DVLBottomTrack), ncol=9)
        }
    } else {
        DVLBottomTrack <- NULL
    }

    if (length(p$echosounder) > 0) {   # vector-read 'echosounder'=0x1c BOOKMARK=E (see also J)
        # FIXME: once this is coded, comment-out near bookmark J
        #nbeamsEchosounder <- nbeams[p$echosounder[1]]
        #ncellsEchosounder <- ncellsEchosounderWholeFile[p$echosounder][1]
        #message(vectorShow(nbeamsEchosounder))
        #message("next is ncellsEchosounder:")
        #print(ncellsEchosounder)
        #message("next is ncellsEchosounder[p$echosounder]:")
        #print(ncellsEchosounder[p$echosounder])
        oceDebug(debug, "preparing to vector-read echosounder data\n")
        echosounder <- list(i=1,
            numberOfCells=ncellsEchosounderWholeFile[p$echosounder[1]],
            numberOfBeams=1, # FIXME: is this right?
            originalCoordinate=coordinateSystem[p$echosounder[1]],
            oceCoordinate=coordinateSystem[p$echosounder[1]],
            cellSize=cellSize[p$echosounder[1]],
            blankingDistance=blankingDistance[p$echosounder[1]],
            ensemble=ensemble[p$echosounder],
            time=time[p$echosounder],
            orientation=orientation[p$echosounder],
            heading=heading[p$echosounder],
            pitch=pitch[p$echosounder],
            roll=roll[p$echosounder],
            pressure=pressure[p$echosounder],
            temperature=temperature[p$echosounder],
            temperatureMagnetometer=temperatureMagnetometer[p$echosounder],
            temperatureRTC=temperatureRTC[p$echosounder],
            soundSpeed=soundSpeed[p$echosounder],
            magnetometer=list(x=magnetometerx[p$echosounder],
                y=magnetometery[p$echosounder],
                z=magnetometerz[p$echosounder]),
            accelerometer=list(x=accelerometerx[p$echosounder],
                y=accelerometery[p$echosounder],
                z=accelerometerz[p$echosounder]),
            nominalCorrelation=nominalCorrelation[p$echosounder],
            datasetDescription=datasetDescription[p$echosounder],
            transmitEnergy=transmitEnergy[p$echosounder],
            powerLevel=powerLevel[p$echosounder])
        oceDebug(debug, "vector-read 'echosounder' records (0x1c) {\n")
        i <- d$index[which(d$id==0x1c)] # pointers to "echosounder" chunks in buf
        i0v <- 77                      # pointer to data (incremented by getItemFromBuf() later).
        NP <- length(i)                # number of profiles of this type
        oceDebug(debug, "  ", vectorShow(i, n=3))
        NC <- echosounder$numberOfCells    # number of cells for v,a,q
        NB <- echosounder$numberOfBeams    # number of beams for v,a,q
        oceDebug(debug, "  NP=", NP, ", NB=", NB, ", NC=", NC, "\n", sep="")

        p1 <- p$echosounder[1]
        if (velocityIncluded[p1])
            echosounder <- getItemFromBuf(echosounder, "v", i=i, type="echosounder", debug=debug)
        if (amplitudeIncluded[p1])
            echosounder <- getItemFromBuf(echosounder, "a", i=i, type="echosounder", debug=debug)
        if (correlationIncluded[p1])
            echosounder <- getItemFromBuf(echosounder, "q", i=i, type="echosounder", debug=debug)
        if (altimeterIncluded[p1])
            echosounder <- getItemFromBuf(echosounder, "altimeter", i=i, type="echosounder", debug=debug)
        if (ASTIncluded[p1])
            echosounder <- getItemFromBuf(echosounder, "AST", i=i, type="echosounder", debug=debug)
        if (altimeterRawIncluded[p1])
            echosounder <- getItemFromBuf(echosounder, "altimeterRaw", i=i, type="echosounder", debug=debug)
        if (echosounderIncluded[p1])
            echosounder <- getItemFromBuf(echosounder, "echosounder", i=i, type="echosounder", debug=debug)
        if (AHRSIncluded[p1])
            echosounder <- getItemFromBuf(echosounder, "AHRS", i=i, type="echosounder", debug=debug)
        if (percentGoodIncluded[p1])
            echosounder <- getItemFromBuf(echosounder, "percentgood", i=i, type="echosounder", debug=debug)
        if (stdDevIncluded[p1])
            echosounder <- getItemFromBuf(echosounder, "stdDev", i=i, type="echosounder", debug=debug)
        ch <- p$echosounder[1] # FiXME: what is this for?
        oceDebug(debug, "} # vector-read 'echosounder'\n")
    } else {
        echosounder <- NULL
    }

    if (length(p$DVLWaterTrack) > 0) {    # key=0x1d
        #if (any(version[p$DVLWaterTrack] != 3))
        #    stop("can only decode 'DVLWaterTrack' data records that are in 'version 3' format")
        nbeamsDVLWaterTrack <- nbeams[p$DVLWaterTrack[1]]
        ncellsDVLWaterTrack <- ncells[p$DVLWaterTrack[1]]
        oceDebug(debug, "DVLWaterTrack data records: nbeams:", nbeamsDVLWaterTrack, ", ncells:", ncellsDVLWaterTrack, "\n")
        if (any(ncells[p$DVLWaterTrack] != ncellsDVLWaterTrack))
            stop("the 'DVLWaterTrack' data records do not all have the same number of cells")
        if (any(nbeams[p$DVLWaterTrack] != nbeamsDVLWaterTrack))
            stop("the 'DVLWaterTrack' data records do not all have the same number of beams")
        # FIXME: read other fields to the following list.
        DVLWaterTrack <- list(i=1,
            numberOfCells=ncellsDVLWaterTrack,
            numberOfBeams=nbeamsDVLWaterTrack,
            originalCoordinate=coordinateSystem[p$DVLWaterTrack[1]],
            oceCoordinate=coordinateSystem[p$DVLWaterTrack[1]],
            cellSize=cellSize[p$DVLWaterTrack[1]],
            blankingDistance=blankingDistance[p$DVLWaterTrack[1]],
            ensemble=ensemble[p$DVLWaterTrack],
            time=time[p$DVLWaterTrack],
            orientation=orientation[p$DVLWaterTrack],
            heading=heading[p$DVLWaterTrack],
            pitch=pitch[p$DVLWaterTrack],
            roll=roll[p$DVLWaterTrack],
            pressure=pressure[p$DVLWaterTrack],
            temperature=temperature[p$DVLWaterTrack],
            temperatureMagnetometer=temperatureMagnetometer[p$DVLWaterTrack],
            temperatureRTC=temperatureRTC[p$DVLWaterTrack],
            soundSpeed=soundSpeed[p$DVLWaterTrack],
            magnetometerx=magnetometerx[p$DVLWaterTrack],
            magnetometery=magnetometery[p$DVLWaterTrack],
            magnetometerz=magnetometerz[p$DVLWaterTrack],
            accelerometerx=accelerometerx[p$DVLWaterTrack],
            accelerometery=accelerometery[p$DVLWaterTrack],
            accelerometerz=accelerometerz[p$DVLWaterTrack],
            nominalCorrelation=nominalCorrelation[p$DVLWaterTrack],
            datasetDescription=datasetDescription[p$DVLWaterTrack],
            transmitEnergy=transmitEnergy[p$DVLWaterTrack],
            powerLevel=powerLevel[p$DVLWaterTrack])
        if (any(velocityIncluded[p$DVLWaterTrack])) {
            if (1 < length(unique(velocityIncluded[p$DVLWaterTrack])))
                stop("velocityIncluded values non-unique across 'DVLWaterTrack' data records")
            DVLWaterTrack$v <- array(double(), dim=c(length(p$DVLWaterTrack), nbeamsDVLWaterTrack))
        }
        if (any(altimeterIncluded[p$DVLWaterTrack])) { # note name-shift from average/burst data
            if (1 < length(unique(altimeterIncluded[p$DVLWaterTrack])))
                stop("altimeterIncluded values non-unique across 'DVLWaterTrack' data records")
            DVLWaterTrack$altimeterDistance <- array(double(), dim=c(length(p$DVLWaterTrack), nbeamsDVLWaterTrack))
        }
        if (any(altimeterRawIncluded[p$DVLWaterTrack])) { # note name-shift from average/burst data
            DVLWaterTrack$altimeterFigureOfMerit <- array(double(), dim=c(length(p$DVLWaterTrack), nbeamsDVLWaterTrack))
        }
    } else {
        DVLWaterTrack <- NULL
    }

    if (length(p$altimeter) > 0) {     # key=0x1e
        #if (any(version[p$altimeter] != 3))
        #    stop("can only decode 'altimeter' data records that are in 'version 3' format")
        nbeamsAltimeter <- nbeams[p$altimeter[1]]
        ncellsAltimeter <- ncells[p$altimeter[1]]
        oceDebug(debug, "altimeter data records: nbeams:", nbeamsAltimeter, ", ncells:", ncellsAltimeter, "\n")
        if (any(ncells[p$altimeter] != ncellsAltimeter))
            stop("the 'altimeter' data records do not all have the same number of cells")
        if (any(nbeams[p$altimeter] != nbeamsAltimeter))
            stop("the 'altimeter' data records do not all have the same number of beams")
        # FIXME: read other fields to the following list.
        altimeter <- list(i=1,
            numberOfCells=ncellsAltimeter,
            numberOfBeams=nbeamsAltimeter,
            originalCoordinate=coordinateSystem[p$altimeter[1]],
            oceCoordinate=coordinateSystem[p$altimeter[1]],
            cellSize=cellSize[p$altimeter[1]],
            blankingDistance=blankingDistance[p$altimeter[1]],
            ensemble=ensemble[p$altimeter],
            time=time[p$altimeter],
            orientation=orientation[p$altimeter],
            heading=heading[p$altimeter],
            pitch=pitch[p$altimeter],
            roll=roll[p$altimeter],
            pressure=pressure[p$altimeter],
            temperature=temperature[p$altimeter],
            temperatureMagnetometer=temperatureMagnetometer[p$altimeter],
            temperatureRTC=temperatureRTC[p$altimeter],
            soundSpeed=soundSpeed[p$altimeter],
            magnetometerx=magnetometerx[p$altimeter],
            magnetometery=magnetometery[p$altimeter],
            magnetometerz=magnetometerz[p$altimeter],
            accelerometerx=accelerometerx[p$altimeter],
            accelerometery=accelerometery[p$altimeter],
            accelerometerz=accelerometerz[p$altimeter],
            nominalCorrelation=nominalCorrelation[p$altimeter],
            datasetDescription=datasetDescription[p$altimeter],
            transmitEnergy=transmitEnergy[p$altimeter],
            powerLevel=powerLevel[p$altimeter])
        if (any(velocityIncluded[p$altimeter])) {
            if (1 < length(unique(velocityIncluded[p$altimeter])))
                stop("velocityIncluded values non-unique across 'altimeter' data records")
            altimeter$v <- array(double(), dim=c(length(p$altimeter), nbeamsAltimeter))
        }
        if (any(altimeterIncluded[p$altimeter])) { # note name-shift from average/burst data
            if (1 < length(unique(altimeterIncluded[p$altimeter])))
                stop("altimeterIncluded values non-unique across 'altimeter' data records")
            altimeter$altimeterDistance <- array(double(), dim=c(length(p$altimeter), nbeamsAltimeter))
        }
        if (any(altimeterRawIncluded[p$altimeter])) { # note name-shift from average/burst data
            altimeter$altimeterFigureOfMerit <- array(double(), dim=c(length(p$altimeter), nbeamsAltimeter))
        }
    } else {
        altimeter <- NULL
    }

    if (length(p$averageAltimeter) > 0) {   # key=0x1f
        #if (any(version[p$averageAltimeter] != 3))
        #    stop("can only decode 'averageAltimeter' data records that are in 'version 3' format")
        nbeamsAverageAltimeter <- nbeams[p$averageAltimeter[1]]
        ncellsAverageAltimeter <- ncells[p$averageAltimeter[1]]
        oceDebug(debug, "averageAltimeter data records: nbeams:", nbeamsAverageAltimeter, ", ncells:", ncellsAverageAltimeter, "\n")
        if (any(ncells[p$averageAltimeter] != ncellsAverageAltimeter))
            stop("the 'averageAltimeter' data records do not all have the same number of cells")
        if (any(nbeams[p$averageAltimeter] != nbeamsAverageAltimeter))
            stop("the 'averageAltimeter' data records do not all have the same number of beams")
        # FIXME: read other fields to the following list.
        averageAltimeter <- list(i=1,
            numberOfCells=ncellsAverageAltimeter,
            numberOfBeams=nbeamsAverageAltimeter,
            originalCoordinate=coordinateSystem[p$averageAltimeter[1]],
            oceCoordinate=coordinateSystem[p$averageAltimeter[1]],
            cellSize=cellSize[p$averageAltimeter[1]],
            blankingDistance=blankingDistance[p$averageAltimeter[1]],
            ensemble=ensemble[p$averageAltimeter],
            time=time[p$averageAltimeter],
            orientation=orientation[p$averageAltimeter],
            heading=heading[p$averageAltimeter],
            pitch=pitch[p$averageAltimeter],
            roll=roll[p$averageAltimeter],
            pressure=pressure[p$averageAltimeter],
            temperature=temperature[p$averageAltimeter],
            temperatureMagnetometer=temperatureMagnetometer[p$averageAltimeter],
            temperatureRTC=temperatureRTC[p$averageAltimeter],
            soundSpeed=soundSpeed[p$averageAltimeter],
            magnetometerx=magnetometerx[p$averageAltimeter],
            magnetometery=magnetometery[p$averageAltimeter],
            magnetometerz=magnetometerz[p$averageAltimeter],
            accelerometerx=accelerometerx[p$averageAltimeter],
            accelerometery=accelerometery[p$averageAltimeter],
            accelerometerz=accelerometerz[p$averageAltimeter],
            nominalCorrelation=nominalCorrelation[p$averageAltimeter],
            datasetDescription=datasetDescription[p$averageAltimeter],
            transmitEnergy=transmitEnergy[p$averageAltimeter],
            powerLevel=powerLevel[p$averageAltimeter])
        if (any(velocityIncluded[p$averageAltimeter])) {
            if (1 < length(unique(velocityIncluded[p$averageAltimeter])))
                stop("velocityIncluded values non-unique across 'averageAltimeter' data records")
            averageAltimeter$v <- array(double(), dim=c(length(p$averageAltimeter), nbeamsAverageAltimeter))
        }
        if (any(altimeterIncluded[p$averageAltimeter])) { # note name-shift from average/burst data
            if (1 < length(unique(altimeterIncluded[p$averageAltimeter])))
                stop("altimeterIncluded values non-unique across 'averageAltimeter' data records")
            averageAltimeter$altimeterDistance <- array(double(), dim=c(length(p$averageAltimeter), nbeamsAverageAltimeter))
        }
        if (any(altimeterRawIncluded[p$averageAltimeter])) { # note name-shift from average/burst data
            averageAltimeter$altimeterFigureOfMerit <- array(double(), dim=c(length(p$averageAltimeter), nbeamsAverageAltimeter))
        }
    } else {
        averageAltimeter <- NULL
    }

    #if (length(p$text) > 0) {          # key=0xa0
    #    message("L1636: yes, we have p$text")
    #} else {
    #    p$text <- NULL                 # erase the empty list
    #}
    #?print(str(p,1))
    p$text <- NULL

    # Fill up the arrays in a loop (FIXME: remove when all is vectorized)
    id <- d$id
    if (monitor)
        progressBar <- txtProgressBar(max=N, style=3, title="Reading profiles")
    unknownKeys <- list()
    oceDebug(debug, "processing N=", N, " data chunks (check: is this ", length(d$id), "?)\n")
    #print(table(d$id))
    #DAN<<-d$id
    for (ch in 1:N) {                  # BOOKMARK J
        # oceDebug(debug>3, "d$id[", ch, "]=", d$id[[ch]], "\n", sep="")
        key <- d$id[ch]
        i <- d$index[ch]
        #oceDebug(debug > 0, sprintf("chunk ch=%d of %d, starting at buf[%d] has key=0x%02x (%s)\n",
        #        ch, N, i, key, ad2cpCodeToName(key)), unindent=1)

        if (FALSE && key == 0x15) { # burst (DISABLED unvectorized)
            #ncol <- burst$numberOfBeams
            #nrow <- burst$numberOfCells
            #n <- ncol * nrow
            #n2 <- 2 * n
            #i0 <- 77
            #if (velocityIncluded[ch]) {
            #    v <- velocityFactor[ch]*readBin(d$buf[i+i0+seq(0,n2-1)], "integer",size=2,n=n,endian="little")
            #    burst$vOLD[burst$i, , ] <- matrix(v, ncol=ncol, nrow=nrow, byrow=FALSE)
            #    i0 <- i0 + n2
            #}
            #if (amplitudeIncluded[ch]) {
            #    a <- d$buf[i + i0 + seq(0,n-1)]
            #    burst$aOLD[burst$i, ,] <- matrix(a, ncol=ncol, nrow=nrow, byrow=FALSE)
            #    i0 <- i0 + n
            #}
            #if (correlationIncluded[ch]) {
            #    q <- d$buf[i + i0 + seq(0,n-1)]
            #    burst$qOLD[burst$i, ,] <- matrix(q, ncol=ncol, nrow=nrow, byrow=FALSE)
            #    i0 <- i0 + n
            #}
            #if (altimeterIncluded[ch]) { # burst
            #    burst$altimeterDistanceOLD[burst$i] <- readBin(d$buf[i + i0 + 0:3], "numeric", size=4, n=1, endian="little")
            #    # FIXME: perhaps save altimeterQuality from next 2 bytes
            #    # FIXME: perhaps save altimeterStatus from next 2 bytes
            #    i0 <- i0 + 8
            #}
            #if (ASTIncluded[ch]) { # burst
            #    # bytes: 4(distance)+2(quality)+2(offset)+4(pressure)+8(spare)
            #    burst$ASTDistanceOLD <- readBin(d$buf[i + i0 + 0:3], "numeric", size=4, n=1, endian="little")
            #    i0 <- i0 + 8 # advance past distance (4 bytes), then skip skip quality (2 bytes) and offset (2 bytes)
            #    burst$ASTPressureOLD <- readBin(d$buf[i + i0 + 0:3], "numeric", size=4, n=1, endian="little")
            #    i0 <- i0 + 12 # skip spare (8 bytes)
            #}
            #if (altimeterRawIncluded[ch]) { # burst
            #    burst$altimeterRawNumberOfSamplesOLD <- readBin(d$buf[i+i0+0:3],"integer",size=4,n=1,endian="little")
            #    message(vectorShow(burst$altimeterRawNumberOfSamples))
            #    i0 <- i0 + 4
            #    burst$altimeterRawSampleDistanceOLD <- 0.1e-3*readBin(d$buf[i+i0+0:1],"integer",size=2,n=1,endian="little",signed=FALSE)
            #    message(vectorShow(burst$altimeterRawSampleDistance))
            #    i0 <- i0 + 2
            #    # ERROR: not increasing i0 properly. Never caught this because sample
            #    # dataset did not have following data (echosounder etc).
            #    burst$altimeterRawSamplesOLD <- readBin(d$buf[i+i0+0:1],"integer",size=2,n=1,endian="little") # 'signed frac' in docs FIXME(DEK) what does that mean??
            #    message(vectorShow(burst$altimeterRawSamplesOLD))
            #    i0 <- i0 + 2
            #    # See the Nortek manual snippet below, for more on format.
            #    # https://github.com/dankelley/oce/issues/1959#issuecomment-1141411327
            #    #DAN1<<-list(echosounderIncluded=echosounderIncluded,ch=ch)
            #    #stop("test stop at DAN1 with ch=", ch)
            #}
            #if (echosounderIncluded[ch]) {
            #    message("echosounderIncluded[", ch, "] about to read ", nrow, "two-byte values (stored as DAN)\n")
            #    burst$echosounderOLD[burst$i, ] <- readBin(d$buf[i + i0 + seq(0,nrow-1)], size=2, n=nrow, endian="little")
            #    #DAN2<<-burst
            #    i0 <- i0 + 2 * nrow

            #}
            #if (AHRSIncluded[ch]) {
            #    burst$AHRSOLD[burst$i,] <- readBin(d$buf[i + i0 + 0:35], "numeric", size=4, n=9, endian="little")
            #}
            #burst$i <- burst$i + 1
        } else if (FALSE && key == 0x16) { # average (DISABLED unvectorized)
            #ncol <- average$numberOfBeams
            #nrow <- average$numberOfCells
            #n <- ncol * nrow
            #n2 <- 2 * n
            #i0 <- 77
            #if (velocityIncluded[ch]) {
            #    v <- velocityFactor[ch]*readBin(d$buf[i+i0+seq(0,n2-1)],"integer",size=2,n=n,endian="little")
            #    average$v[average$i, , ] <- matrix(v, ncol=ncol, nrow=nrow, byrow=FALSE)
            #    i0 <- i0 + n2
            #}
            #if (amplitudeIncluded[ch]) {
            #    a <- d$buf[i + i0 + seq(0,n-1)]
            #    average$a[average$i, ,] <- matrix(a, ncol=ncol, nrow=nrow, byrow=FALSE)
            #    i0 <- i0 + n
            #}
            #if (correlationIncluded[ch]) {
            #    q <- d$buf[i + i0 + seq(0,n-1)]
            #    average$q[average$i, ,] <- matrix(q, ncol=ncol, nrow=nrow, byrow=FALSE)
            #    i0 <- i0 + n
            #}
            #if (altimeterIncluded[ch]) { # average
            #    average$altimeterDistance[average$i] <- readBin(d$buf[i + i0 + 0:3],"numeric", size=4,n=1,endian="little")
            #    # FIXME: perhaps save altimeterQuality from next 2 bytes
            #    # FIXME: perhaps save altimeterStatus from next 2 bytes
            #    i0 <- i0 + 8
            #}
            #if (ASTIncluded[ch]) { # average
            #    # bytes: 4(distance)+2(quality)+2(offset)+4(pressure)+8(spare)
            #    average$ASTDistance <- readBin(d$buf[i + i0 + 0:3], "numeric", size=4, n=1, endian="little")
            #    i0 <- i0 + 8 # advance past distance (4 bytes), then skip skip quality (2 bytes) and offset (2 bytes)
            #    average$ASTPressure <- readBin(d$buf[i + i0 + 0:3], "numeric", size=4, n=1, endian="little")
            #    i0 <- i0 + 12 # skip spare (8 bytes)
            #}
            #if (altimeterRawIncluded[ch]) { # average
            #    average$altimeterRawNumberOfSamples <- readBin(d$buf[i+i0+0:3],"integer",size=4,n=1,endian="little")
            #    i0 <- i0 + 4
            #    average$altimeterRawSampleDistance <- readBin(d$buf[i+i0+0:1],"integer",size=2,n=1,endian="little",signed=FALSE)
            #    i0 <- i0 + 2
            #    average$altimeterRawSamples <- readBin(d$buf[i+i0+0:1],"integer",size=2,n=1,endian="little") # 'singed frac' in docs
            #    i0 <- i0 + 2
            #}
            #if (echosounderIncluded[ch]) {
            #    average$echosounder[average$i, ] <- readBin(d$buf[i + i0 + seq(0,nrow-1)], size=2, n=nrow, endian="little")
            #    i0 <- i0 + 2 * nrow
            #}
            #if (AHRSIncluded[ch]) {
            #    average$AHRS[average$i,] <- readBin(d$buf[i + i0 + 0:35],"numeric", size=4, n=9, endian="little")
            #}
            #average$i <- average$i + 1

        } else if (FALSE && key == 0x17) { # bottomTrack BOOKMARK B

            #<> #warning("FIXME: vectorize bottomTrack")
            #<> ncol <- bottomTrack$numberOfBeams
            #<> nrow <- bottomTrack$numberOfCells
            #<> # distance: uses variable name that makes sense for average/burst data
            #<> i0 <- 77 # FIXME: where is this documented?
            #<> if (velocityIncluded[ch]) { # configuration[,9]=bit8 [1 pages 60 and 62]
            #<>     oceDebug(debug>1, "saving bottomTrack$v[", bottomTrack$i, ",]\n")
            #<>     bottomTrack$v[bottomTrack$i, ] <-
            #<>         0.001*readBin(buf[i + i0 + seq(0,4*ncol-1)], "numeric", size=4, n=ncol, endian="little")
            #<>     i0 <- i0 + 4*ncol
            #<>     #message(" ... done")
            #<> }
            #<> if (altimeterIncluded[ch]) { # bottomTrack
            #<>     # configuration[,9]=bit8 [1 pages 60 and 62]
            #<>     oceDebug(debug>1, "saving bottomTrack$altimeterDistance[", bottomTrack$i, ",]\n")
            #<>     bottomTrack$altimeterDistance[bottomTrack$i, ] <-
            #<>         readBin(buf[i + i0 + seq(0,4*ncol-1)], "numeric", size=4, n=ncol, endian="little")
            #<>     i0 <- i0 + 4*ncol
            #<>     #message(" ... done")
            #<> }
            #<> # figureOfMerit: uses variable name that makes sense for average/burst data
            #<> if (altimeterRawIncluded[ch]) { # bottomTrack
            #<>     # configuration[,10]=bit9 [1 pages 60 and 62]
            #<>     oceDebug(debug>1, "saving bottomTrack$altimeterFigureOfMerit[", bottomTrack$i, ",]\n")
            #<>     # FIXME: is this integer or numeric?  R won't let me read 2-byte
            #<>     # numerics, so -- for now -- I'm assuming integer. If it is
            #<>     # actually numeric, I'll need to construct it byte by byte.
            #<>     bottomTrack$altimeterFigureOfMerit[bottomTrack$i, ] <-
            #<>         readBin(buf[i + i0 + seq(0,2*ncol-1)], "integer", size=2, n=ncol, endian="little")
            #<>     i0 <- i0 + 2*ncol
            #<>     #message(" ... done")
            #<> }
            #<> bottomTrack$i <- bottomTrack$i + 1

        } else if (key == 0x18) { # interleavedBurst
            oceDebug(debug>1, "handling ", ad2cpCodeToName(key), "\n", unindent=2)
            ncol <- interleavedBurst$numberOfBeams
            nrow <- interleavedBurst$numberOfCells
            n <- ncol * nrow
            n2 <- 2 * n
            i0 <- 77
            if (velocityIncluded[ch]) {
                v <- velocityFactor[ch]*readBin(d$buf[i+i0+seq(0,n2-1)],"integer",size=2,n=n,endian="little")
                interleavedBurst$v[interleavedBurst$i, , ] <- matrix(v, ncol=ncol, nrow=nrow, byrow=FALSE)
                i0 <- i0 + n2
            }
            if (amplitudeIncluded[ch]) {
                a <- d$buf[i + i0 + seq(0,n-1)]
                interleavedBurst$a[interleavedBurst$i, ,] <- matrix(a, ncol=ncol, nrow=nrow, byrow=FALSE)
                i0 <- i0 + n
            }
            if (correlationIncluded[ch]) {
                q <- d$buf[i + i0 + seq(0,n-1)]
                interleavedBurst$q[interleavedBurst$i, ,] <- matrix(q, ncol=ncol, nrow=nrow, byrow=FALSE)
                i0 <- i0 + n
            }
            if (altimeterIncluded[ch]) { # interleavedBurst
                interleavedBurst$altimeterDistance[interleavedBurst$i] <- readBin(d$buf[i + i0 + 0:3],"numeric", size=4,n=1,endian="little")
                # FIXME: perhaps save altimeterQuality from next 2 bytes
                # FIXME: perhaps save altimeterStatus from next 2 bytes
                i0 <- i0 + 8
            }
            if (ASTIncluded[ch]) { # interleavedBurst
                # bytes: 4(distance)+2(quality)+2(offset)+4(pressure)+8(spare)
                interleavedBurst$ASTDistance <- readBin(d$buf[i + i0 + 0:3], "numeric", size=4, n=1, endian="little")
                i0 <- i0 + 8 # advance past distance (4 bytes), then skip skip quality (2 bytes) and offset (2 bytes)
                interleavedBurst$ASTPressure <- readBin(d$buf[i + i0 + 0:3], "numeric", size=4, n=1, endian="little")
                i0 <- i0 + 12 # skip spare (8 bytes)
            }
            if (altimeterRawIncluded[ch]) { # interleavedBurst
                interleavedBurst$altimeterRawNumberOfSamples <- readBin(d$buf[i+i0+0:3],"integer",size=4,n=1,endian="little")
                i0 <- i0 + 4
                interleavedBurst$altimeterRawSampleDistance <- readBin(d$buf[i+i0+0:1],"integer",size=2,n=1,endian="little",signed=FALSE)
                i0 <- i0 + 2
                interleavedBurst$altimeterRawSamples <- readBin(d$buf[i+i0+0:1],"integer",size=2,n=1,endian="little") # 'singed frac' in docs
                i0 <- i0 + 2
            }
            if (echosounderIncluded[ch]) {
                interleavedBurst$echosounder[interleavedBurst$i, ] <- readBin(d$buf[i + i0 + seq(0,nrow-1)], size=2, n=nrow, endian="little")
                i0 <- i0 + 2 * nrow
            }
            if (AHRSIncluded[ch]) {
                interleavedBurst$AHRS[interleavedBurst$i,] <- readBin(d$buf[i + i0 + 0:35], "numeric", size=4, n=9, endian="little")
            }
            interleavedBurst$i <- interleavedBurst$i + 1

        } else if (FALSE && key == 0x1a) { # burstAltimeterRaw (DISABLED unvectorized)
            #ncol <- burstAltimeterRaw$numberOfBeams # for v only
            #nrow <- burstAltimeterRaw$numberOfCells # for v only
            #n <- ncol * nrow           # for v only
            #n2 <- 2 * n
            #i0 <- 77
            #oceDebug(debug>1, "velocityIncluded[", ch, "]=", velocityIncluded[ch],
            #    ", amplitudeIncluded[", ch, "]=", amplitudeIncluded[ch],
            #    ", correlationIncluded[", ch, "]=", correlationIncluded[ch],
            #    ", altimeterIncluded[", ch, "]=", altimeterIncluded[ch],
            #    ", ASTIncluded[", ch, "]=", ASTIncluded[ch],
            #    ", altimeterRawIncluded[", ch, "]=", altimeterRawIncluded[ch],
            #    ", echosounderIncluded[", ch, "]=", echosounderIncluded[ch],
            #    ", AHRSIncluded[", ch, "]=", AHRSIncluded[ch], "\n")
            #if (velocityIncluded[ch]) {
            #    # Create space, or check that dimensionality agrees with existing space.
            #    if ("v" %in% names(burstAltimeterRaw)) {
            #        nOld <- dim(burstAltimeterRaw$v)[2]
            #        if (n != nOld)
            #            stop("burstAltimeterRaw$v holds ", nOld, " samples, but chunk ", ch, " has ", n, " samples")
            #    } else {
            #        oceDebug(debug>1, "creating burstAltimeterRaw$v (", sum(d$id==0x1a), "X", n, ")\n")
            #        burstAltimeterRaw$v <- array(double(), dim=c(sum(d$id==0x1a), n))
            #    }
            #    v <- velocityFactor[ch]*readBin(d$buf[i+i0+seq(0L,2L*n-1L)], "integer", size=2L, n=n, endian="little")
            #    burstAltimeterRaw$v[burstAltimeterRaw$i, , ] <- matrix(v, ncol=ncol, nrow=nrow, byrow=FALSE)
            #    oceDebug(debug>1, "saving burstAltimeterRaw$v[", burstAltimeterRaw$i, ",,] at i0=", i0, "\n")
            #    i0 <- i0 + n2
            #}
            #if (amplitudeIncluded[ch]) {
            #    # Create space for 'a', or check that dimensionality agrees with existing space.
            #    if ("a" %in% names(burstAltimeterRaw)) {
            #        nOld <- dim(burstAltimeterRaw$a)[2]
            #        if (n != nOld)
            #            stop("burstAltimeterRaw$a holds ", nOld, " samples, but chunk ", ch, " has ", n, " samples")
            #    } else {
            #        oceDebug(debug>1, "creating burstAltimeterRaw$a (", sum(d$id==0x1a), "X", n, ")\n")
            #        burstAltimeterRaw$a <- array(raw(), dim=c(sum(d$id==0x1a), n))
            #    }
            #    a <- d$buf[i + i0 + seq(0L, n-1L)]
            #    burstAltimeterRaw$a[burstAltimeterRaw$i, ,] <- matrix(a, ncol=ncol, nrow=nrow, byrow=FALSE)
            #    i0 <- i0 + n
            #    oceDebug(debug>1, "saving burstAltimeterRaw$a[", burstAltimeterRaw$i, ",] at i0=", i0, "\n")
            #}
            #if (correlationIncluded[ch]) {
            #    message("FIXME: comment this out!!")
            #    # Create space for 'q', or check that dimensionality agrees with existing space.
            #    if ("q" %in% names(burstAltimeterRaw)) {
            #        nOld <- dim(burstAltimeterRaw$q)[2]
            #        if (n != nOld)
            #            stop("burstAltimeterRaw$q holds ", nOld, " samples, but chunk ", ch, " has ", n, " samples")
            #    } else {
            #        oceDebug(debug>1, "creating burstAltimeterRaw$q (", sum(d$id==0x1a), "X", n, ")\n")
            #        burstAltimeterRaw$q <- array(raw(), dim=c(sum(d$id==0x1a), n))
            #    }
            #    q <- d$buf[i + i0 + seq(0L, n-1L)]
            #    burstAltimeterRaw$q[burstAltimeterRaw$i, ,] <- matrix(q, ncol=ncol, nrow=nrow, byrow=FALSE)
            #    oceDebug(debug>1, "saving burstAltimeterRaw$q[", burstAltimeterRaw$i, ",,] at i0=", i0, "\n")
            #    i0 <- i0 + n
            #}
            #if (altimeterIncluded[ch]) { # burstAltimeterRaw (DELETED)
            ##<>     message("BREADCRUMB 1a")
            ##<>     # Create space
            ##<>     if (!("altimeterDistance" %in% names(burstAltimeterRaw))) {
            ##<>         message("BREADCRUMB 1b: blanking out altimeterDistance (should remove this)")
            ##<>         oceDebug(debug>1, "creating burstAltimeterRaw$altimeterDistance (", sum(d$id==0x1a), ")\n")
            ##<>         burstAltimeterRaw$altimeterDistance <- rep(NA_real_, sum(d$id==0x1a))
            ##<>     }
            ##<>     burstAltimeterRaw$altimeterDistance[burstAltimeterRaw$i] <- readBin(d$buf[i + i0 + 0:3],"numeric", size=4,n=1,endian="little")
            ##<>     oceDebug(debug>1, "saving altimeterDistance[", burstAltimeterRaw$i,  "]=",
            ##<>         burstAltimeterRaw$altimeterDistance[burstAltimeterRaw$i], " at i0=", i0, "\n")

            ##<>     # FIXME: perhaps save altimeterQuality from next 2 bytes
            ##<>     # FIXME: perhaps save altimeterStatus from next 2 bytes
            #    i0 <- i0 + 8 # FIXME: change this, if read altimeterQuality and/or altimeterStatus
            #}
            ##<> if (ASTIncluded[ch]) { # AST
            ##<>     # Create space
            ##<>     if (!"ASTDistance" %in% names(burstAltimeterRaw)) {
            ##<>         oceDebug(debug>1, "creating burstAltimeterRaw$ASTDistance (", sum(d$id==0x1a), ")\n")
            ##<>         burstAltimeterRaw$ASTDistance <- rep(NA_real_, sum(d$id==0x1a))
            ##<>     }
            ##<>     if (!"ASTPressure" %in% names(burstAltimeterRaw)) {
            ##<>         oceDebug(debug>1, "creating burstAltimeterRaw$ASTpressure (", sum(d$id==0x1a), ")\n")
            ##<>         burstAltimeterRaw$ASTPressure <- rep(NA_real_, sum(d$id==0x1a))
            ##<>     }
            ##<>     # bytes: 4(distance)+2(quality)+2(offset)+4(pressure)+8(spare)
            ##<>     burstAltimeterRaw$ASTDistance[burstAltimeterRaw$i] <- readBin(d$buf[i + i0 + 0:3], "numeric", size=4, n=1, endian="little")
            ##<>     oceDebug(debug>1, "saving ASTDistance[", burstAltimeterRaw$i,  "]=",
            ##<>         burstAltimeterRaw$ASTDistance[burstAltimeterRaw$i], " at i0=", i0, "\n")
            ##<>     i0 <- i0 + 8 # advance past distance (4 bytes), then skip quality (2 bytes) and offset (2 bytes)
            ##<>     burstAltimeterRaw$ASTPressure[burstAltimeterRaw$i] <- readBin(d$buf[i + i0 + 0:3], "numeric", size=4, n=1, endian="little")
            ##<>     oceDebug(debug>1, "saving ASTPressure[", burstAltimeterRaw$i,  "]=",
            ##<>         burstAltimeterRaw$ASTPressure[burstAltimeterRaw$i], " at i0=", i0, "\n")
            ##<>     i0 <- i0 + 12 # skip spare (8 bytes)
            ##<> }
            ##<> if (altimeterRawIncluded[ch]) { # burstAltimeterRaw
            ##<>     message("BREADCRUMB 2: blanking out altimeterDistance (should remove this)")
            ##<>     i0 <- i0 + 6 + 2*burstAltimeterRaw$altimeterRawNumberOfSamples
            ##<> }
            #if (echosounderIncluded[ch]) {
            #    # Create space for data, or check that dimensionality agrees with existing space.
            #    if ("echosounder" %in% names(burstAltimeterRaw)) {
            #        nrow2Old <- nrow(burstAltimeterRaw$echosounder)[2]
            #        if (nrow != nrow2Old)
            #            stop("burstAltimeterRaw$echosounder was set up to hold ", nrow2Old, " samples, but data chunk ", ch, " has ", nrow, " samples")
            #    } else {
            #        oceDebug(debug>1, "creating burstAltimeterRaw$echosounder (",
            #            sum(d$id==0x1a), "X", nrow, ")\n")
            #        burstAltimeterRaw$echosounder <- array(double(), dim=c(sum(d$id==0x1a), nrow))
            #    }
            #    burstAltimeterRaw$echosounder[burstAltimeterRaw$i, ] <- readBin(d$buf[i + i0 + seq(0,nrow-1)], size=2, n=nrow, endian="little")
            #    oceDebug(debug>1, "saving burstAltimeterRaw$echosounder[", burstAltimeterRaw$i, ",] at i0=", i0, "\n")
            #    i0 <- i0 + 2 * nrow
            #}
            #if (AHRSIncluded[ch]) {
            #    # Create space for data
            #    if (!("AHRS" %in% names(burstAltimeterRaw))) {
            #        oceDebug(debug>1, "creating burstAltimeterRaw$AHRS (",
            #            sum(d$id==0x1a), "X", 9L, ")\n")
            #        burstAltimeterRaw$AHRS <- array(double(), dim=c(sum(d$id==0x1a), 9L))
            #    }
            #    burstAltimeterRaw$AHRS[burstAltimeterRaw$i,] <- readBin(d$buf[i + i0 + 0:35], "numeric", size=4, n=9, endian="little")
            #    oceDebug(debug>1, "saving burstAltimeterRaw$AHRS[", burstAltimeterRaw$i, ",] at i0=", i0, "\n")
            #}
            ## increment burstAltimeterRaw counter.
            #burstAltimeterRaw$i <- 1L + burstAltimeterRaw$i

        } else if (key == 0x1b) { # DVLBottomTrack

            ncol <- DVLBottomTrack$numberOfBeams
            nrow <- DVLBottomTrack$numberOfCells
            n <- ncol * nrow
            n2 <- 2 * n
            i0 <- 77
            if (velocityIncluded[ch]) {
                v <- velocityFactor[ch]*readBin(d$buf[i+i0+seq(0,n2-1)],"integer",size=2,n=n,endian="little")
                DVLBottomTrack$v[DVLBottomTrack$i, , ] <- matrix(v, ncol=ncol, nrow=nrow, byrow=FALSE)
                i0 <- i0 + n2
            }
            if (amplitudeIncluded[ch]) {
                a <- d$buf[i + i0 + seq(0,n-1)]
                DVLBottomTrack$a[DVLBottomTrack$i, ,] <- matrix(a, ncol=ncol, nrow=nrow, byrow=FALSE)
                i0 <- i0 + n
            }
            if (correlationIncluded[ch]) {
                q <- d$buf[i + i0 + seq(0,n-1)]
                DVLBottomTrack$q[DVLBottomTrack$i, ,] <- matrix(q, ncol=ncol, nrow=nrow, byrow=FALSE)
                i0 <- i0 + n
            }
            if (altimeterIncluded[ch]) { # DVLBottomTrack
                DVLBottomTrack$altimeterDistance[DVLBottomTrack$i] <- readBin(d$buf[i + i0 + 0:3],"numeric", size=4,n=1,endian="little")
                # FIXME: perhaps save altimeterQuality from next 2 bytes
                # FIXME: perhaps save altimeterStatus from next 2 bytes
                i0 <- i0 + 8
            }
            if (ASTIncluded[ch]) {
                # bytes: 4(distance)+2(quality)+2(offset)+4(pressure)+8(spare)
                DVLBottomTrack$ASTDistance <- readBin(d$buf[i + i0 + 0:3], "numeric", size=4, n=1, endian="little")
                i0 <- i0 + 8 # advance past distance (4 bytes), then skip skip quality (2 bytes) and offset (2 bytes)
                DVLBottomTrack$ASTPressure <- readBin(d$buf[i + i0 + 0:3], "numeric", size=4, n=1, endian="little")
                i0 <- i0 + 12 # skip spare (8 bytes)
            }
            if (altimeterRawIncluded[ch]) { # DVLBottomTrack
                DVLBottomTrack$altimeterRawNumberOfSamples <- readBin(d$buf[i+i0+0:3],"integer",size=4,n=1,endian="little")
                i0 <- i0 + 4
                DVLBottomTrack$altimeterRawSampleDistance <- readBin(d$buf[i+i0+0:1],"integer",size=2,n=1,endian="little",signed=FALSE)
                i0 <- i0 + 2
                DVLBottomTrack$altimeterRawSamples <- readBin(d$buf[i+i0+0:1],"integer",size=2,n=1,endian="little") # 'singed frac' in docs
                i0 <- i0 + 2
            }
            if (echosounderIncluded[ch]) {
                DVLBottomTrack$echosounder[DVLBottomTrack$i, ] <- readBin(d$buf[i + i0 + seq(0,nrow-1)], size=2, n=nrow, endian="little")
                i0 <- i0 + 2 * nrow
            }
            if (AHRSIncluded[ch]) {
                DVLBottomTrack$AHRS[DVLBottomTrack$i,] <- readBin(d$buf[i + i0 + 0:35], "numeric", size=4, n=9, endian="little")
            }
            DVLBottomTrack$i <- DVLBottomTrack$i + 1

        } else if (FALSE && key == 0x1c) { # echosounder
            #oceDebug(debug, "*** WE GOT AN 0x1c record ***\n")

            # FIXME: determine echosounder records have other types of data intermixed.
            # The docs are not clear on whether echosounder data ever have v, etc., so they are commented-out
            # here. The main reason I think they *cannot* have such things is that the 2-byte sequence
            # that holds nbeam/ncell for other data types holds just ncells, for echosounders.
            #? ncol <- echosounder$numberOfBeams
            #? nrow <- echosounder$numberOfCells
            #? n <- ncol * nrow
            #? n2 <- 2 * n
            #? i0 <- 77
            #? if (velocityIncluded[ch]) {
            #?     v <- velocityFactor[ch]*readBin(d$buf[i+i0+seq(0,n2-1)],"integer",size=2,n=n,endian="little")
            #?     echosounder$v[echosounder$i, , ] <- matrix(v, ncol=ncol, nrow=nrow, byrow=FALSE)
            #?     i0 <- i0 + n2
            #? }
            #? if (amplitudeIncluded[ch]) {
            #?     a <- d$buf[i + i0 + seq(0,n-1)]
            #?     echosounder$a[echosounder$i, ,] <- matrix(a, ncol=ncol, nrow=nrow, byrow=FALSE)
            #?     i0 <- i0 + n
            #? }
            #? if (correlationIncluded[ch]) {
            #?     q <- d$buf[i + i0 + seq(0,n-1)]
            #?     echosounder$q[echosounder$i, ,] <- matrix(q, ncol=ncol, nrow=nrow, byrow=FALSE)
            #?     i0 <- i0 + n
            #? }
            #? if (altimeterIncluded[ch]) {
            #?     echosounder$altimeterDistance[echosounder$i] <- readBin(d$buf[i + i0 + 0:3], "numeric", size=4,
            #?                                                             n=ncellsEchosounder[p$echosounder][1],
            #?                                                             endian="little")
            #?     # FIXME: perhaps save altimeterQuality from next 2 bytes
            #?     # FIXME: perhaps save altimeterStatus from next 2 bytes
            #?     i0 <- i0 + 8
            #? }
            #? if (ASTIncluded[ch]) {
            #?     # bytes: 4(distance)+2(quality)+2(offset)+4(pressure)+8(spare)
            #?     echosounder$ASTDistance <- readBin(d$buf[i + i0 + 0:3], "numeric", size=4, n=1, endian="little")
            #?     i0 <- i0 + 8 # advance past distance (4 bytes), then skip skip quality (2 bytes) and offset (2 bytes)
            #?     echosounder$ASTPressure <- readBin(d$buf[i + i0 + 0:3], "numeric", size=4, n=1, endian="little")
            #?     i0 <- i0 + 12 # skip spare (8 bytes)
            #? }
            #? if (altimeterRawIncluded[ch]) { # echosounder
            #?     echosounder$altimeterRawNumberOfSamples <- readBin(d$buf[i+i0+0:3],"integer",size=4,n=1,endian="little")
            #?     i0 <- i0 + 4
            #?     echosounder$altimeterRawSampleDistance <- readBin(d$buf[i+i0+0:1],"integer",size=2,n=1,endian="little",signed=FALSE)
            #?     i0 <- i0 + 2
            #?     echosounder$altimeterRawSamples <- readBin(d$buf[i+i0+0:1],"integer",size=2,n=1,endian="little") # 'singed frac' in docs
            #?     i0 <- i0 + 2
            #? }
            #? if (echosounderIncluded[ch]) {
            #?     nToRead <- dim(echosounder$echosounder)[1]
            #?     # cat("before trying to store to echosounder at",
            #?     #     ", echosounder$i=", echosounder$i,
            #?     #     ", i=", i,
            #?     #     ", i0=", i0,
            #?     #     ", nToRead=", nToRead,
            #?     #     ", dim()=", paste(dim(echosounder$echosounder),collapse="x"), "\n", sep="")
            #?     echosounder$echosounder[, echosounder$i] <- readBin(d$buf[i + i0 + seq(0,2*nToRead)], "integer", size=2, n=nToRead, endian="little")
            #?     i0 <- i0 + 2 * nToRead
            #? }
            #? if (AHRSIncluded[ch]) {
            #?     echosounder$AHRS[echosounder$i,] <- readBin(d$buf[i + i0 + 0:35], "numeric", size=4, n=9, endian="little")
            #? }
            #?echosounder$i <- echosounder$i + 1

        } else if (key == 0x1d) { # DVLWaterTrack

            ncol <- DVLWaterTrack$numberOfBeams
            nrow <- DVLWaterTrack$numberOfCells
            n <- ncol * nrow
            n2 <- 2 * n
            i0 <- 77
            if (velocityIncluded[ch]) {
                v <- velocityFactor[ch]*readBin(d$buf[i+i0+seq(0,n2-1)], "integer",size=2,n=n,endian="little")
                DVLWaterTrack$v[DVLWaterTrack$i, , ] <- matrix(v, ncol=ncol, nrow=nrow, byrow=FALSE)
                i0 <- i0 + n2
            }
            if (amplitudeIncluded[ch]) {
                a <- d$buf[i + i0 + seq(0,n-1)]
                DVLWaterTrack$a[DVLWaterTrack$i, ,] <- matrix(a, ncol=ncol, nrow=nrow, byrow=FALSE)
                i0 <- i0 + n
            }
            if (correlationIncluded[ch]) {
                q <- d$buf[i + i0 + seq(0,n-1)]
                DVLWaterTrack$q[DVLWaterTrack$i, ,] <- matrix(q, ncol=ncol, nrow=nrow, byrow=FALSE)
                i0 <- i0 + n
            }
            if (altimeterIncluded[ch]) { # DVLWaterTrack
                DVLWaterTrack$altimeterDistance[DVLWaterTrack$i] <- readBin(d$buf[i + i0 + 0:3], "numeric", size=4, n=1, endian="little")
                # FIXME: perhaps save altimeterQuality from next 2 bytes
                # FIXME: perhaps save altimeterStatus from next 2 bytes
                i0 <- i0 + 8
            }
            if (ASTIncluded[ch]) {
                # bytes: 4(distance)+2(quality)+2(offset)+4(pressure)+8(spare)
                DVLWaterTrack$ASTDistance <- readBin(d$buf[i + i0 + 0:3], "numeric", size=4, n=1, endian="little")
                i0 <- i0 + 8 # advance past distance (4 bytes), then skip skip quality (2 bytes) and offset (2 bytes)
                DVLWaterTrack$ASTPressure <- readBin(d$buf[i + i0 + 0:3], "numeric", size=4, n=1, endian="little")
                i0 <- i0 + 12 # skip spare (8 bytes)
            }
            if (altimeterRawIncluded[ch]) { # DVLWaterTrack
                DVLWaterTrack$altimeterRawNumberOfSamples <- readBin(d$buf[i + i0 + 0:3], "integer", size=4, n=1, endian="little")
                i0 <- i0 + 4
                DVLWaterTrack$altimeterRawSampleDistance <- readBin(d$buf[i + i0 + 0:1], "integer", size=2, n=1, endian="little", signed=FALSE)
                i0 <- i0 + 2
                DVLWaterTrack$altimeterRawSamples <- readBin(d$buf[i + i0 + 0:1], "integer", size=2, n=1, endian="little") # 'singed frac' in docs
                i0 <- i0 + 2
            }
            if (echosounderIncluded[ch]) {
                DVLWaterTrack$echosounder[DVLWaterTrack$i, ] <- readBin(d$buf[i + i0 + seq(0,nrow-1)], size=2, n=nrow, endian="little")
                i0 <- i0 + 2 * nrow
            }
            if (AHRSIncluded[ch]) {
                DVLWaterTrack$AHRS[DVLWaterTrack$i,] <- readBin(d$buf[i + i0 + 0:35], "numeric", size=4, n=9, endian="little")
            }
            DVLWaterTrack$i <- DVLWaterTrack$i + 1


        } else if (key == 0x1e) { # altimeter

            ncol <- altimeter$numberOfBeams
            nrow <- altimeter$numberOfCells
            n <- ncol * nrow
            n2 <- 2 * n
            i0 <- 77
            if (velocityIncluded[ch]) {
                v <- velocityFactor[ch]*readBin(d$buf[i+i0+seq(0,n2-1)], "integer",size=2,n=n,endian="little")
                altimeter$v[altimeter$i, , ] <- matrix(v, ncol=ncol, nrow=nrow, byrow=FALSE)
                i0 <- i0 + n2
            }
            if (amplitudeIncluded[ch]) {
                a <- d$buf[i + i0 + seq(0,n-1)]
                altimeter$a[altimeter$i, ,] <- matrix(a, ncol=ncol, nrow=nrow, byrow=FALSE)
                i0 <- i0 + n
            }
            if (correlationIncluded[ch]) {
                q <- d$buf[i + i0 + seq(0,n-1)]
                altimeter$q[altimeter$i, ,] <- matrix(q, ncol=ncol, nrow=nrow, byrow=FALSE)
                i0 <- i0 + n
            }
            if (altimeterIncluded[ch]) { # altimeter
                altimeter$altimeterDistance[altimeter$i] <- readBin(d$buf[i + i0 + 0:3], "numeric", size=4, n=1, endian="little")
                # FIXME: perhaps save altimeterQuality from next 2 bytes
                # FIXME: perhaps save altimeterStatus from next 2 bytes
                i0 <- i0 + 8
            }
            if (ASTIncluded[ch]) {
                # bytes: 4(distance)+2(quality)+2(offset)+4(pressure)+8(spare)
                altimeter$ASTDistance <- readBin(d$buf[i + i0 + 0:3], "numeric", size=4, n=1, endian="little")
                i0 <- i0 + 8 # advance past distance (4 bytes), then skip skip quality (2 bytes) and offset (2 bytes)
                altimeter$ASTPressure <- readBin(d$buf[i + i0 + 0:3], "numeric", size=4, n=1, endian="little")
                i0 <- i0 + 12 # skip spare (8 bytes)
            }
            if (altimeterRawIncluded[ch]) { # altimeter
                altimeter$altimeterRawNumberOfSamples <- readBin(d$buf[i + i0 + 0:3], "integer", size=4, n=1, endian="little")
                i0 <- i0 + 4
                altimeter$altimeterRawSampleDistance <- readBin(d$buf[i + i0 + 0:1], "integer", size=2, n=1, endian="little", signed=FALSE)
                i0 <- i0 + 2
                altimeter$altimeterRawSamples <- readBin(d$buf[i + i0 + 0:1], "integer", size=2, n=1, endian="little") # 'singed frac' in docs
                i0 <- i0 + 2
            }
            if (echosounderIncluded[ch]) {
                altimeter$echosounder[altimeter$i, ] <- readBin(d$buf[i + i0 + seq(0,nrow-1)], size=2, n=nrow, endian="little")
                i0 <- i0 + 2 * nrow
            }
            if (AHRSIncluded[ch]) {
                altimeter$AHRS[altimeter$i,] <- readBin(d$buf[i + i0 + 0:35], "numeric", size=4, n=9, endian="little")
            }
            altimeter$i <- altimeter$i + 1

        } else if (key == 0x1f) { # averageAltimeter

            ncol <- averageAltimeter$numberOfBeams
            nrow <- averageAltimeter$numberOfCells
            n <- ncol * nrow
            n2 <- 2 * n
            i0 <- 77
            if (velocityIncluded[ch]) {
                v <- velocityFactor[ch]*readBin(d$buf[i+i0+seq(0,n2-1)], "integer",size=2,n=n,endian="little")
                averageAltimeter$v[averageAltimeter$i, , ] <- matrix(v, ncol=ncol, nrow=nrow, byrow=FALSE)
                i0 <- i0 + n2
            }
            if (amplitudeIncluded[ch]) {
                a <- d$buf[i + i0 + seq(0,n-1)]
                averageAltimeter$a[averageAltimeter$i, ,] <- matrix(a, ncol=ncol, nrow=nrow, byrow=FALSE)
                i0 <- i0 + n
            }
            if (correlationIncluded[ch]) {
                q <- d$buf[i + i0 + seq(0,n-1)]
                averageAltimeter$q[averageAltimeter$i, ,] <- matrix(q, ncol=ncol, nrow=nrow, byrow=FALSE)
                i0 <- i0 + n
            }
            if (altimeterIncluded[ch]) { # averageAltimeter
                averageAltimeter$altimeterDistance[averageAltimeter$i] <- readBin(d$buf[i + i0 + 0:3], "numeric", size=4, n=1, endian="little")
                # FIXME: perhaps save altimeterQuality from next 2 bytes
                # FIXME: perhaps save altimeterStatus from next 2 bytes
                i0 <- i0 + 8
            }
            if (ASTIncluded[ch]) {
                # bytes: 4(distance)+2(quality)+2(offset)+4(pressure)+8(spare)
                averageAltimeter$ASTDistance <- readBin(d$buf[i + i0 + 0:3], "numeric", size=4, n=1, endian="little")
                i0 <- i0 + 8 # advance past distance (4 bytes), then skip skip quality (2 bytes) and offset (2 bytes)
                averageAltimeter$ASTPressure <- readBin(d$buf[i + i0 + 0:3], "numeric", size=4, n=1, endian="little")
                i0 <- i0 + 12 # skip spare (8 bytes)
            }
            if (altimeterRawIncluded[ch]) { # averageAltimeter
                averageAltimeter$altimeterRawNumberOfSamples <- readBin(d$buf[i + i0 + 0:3], "integer", size=4, n=1, endian="little")
                i0 <- i0 + 4
                averageAltimeter$altimeterRawSampleDistance <- readBin(d$buf[i + i0 + 0:1], "integer", size=2, n=1, endian="little", signed=FALSE)
                i0 <- i0 + 2
                averageAltimeter$altimeterRawSamples <- readBin(d$buf[i + i0 + 0:1], "integer", size=2, n=1, endian="little") # 'singed frac' in docs
                i0 <- i0 + 2
            }
            if (echosounderIncluded[ch]) {
                averageAltimeter$echosounder[averageAltimeter$i, ] <- readBin(d$buf[i + i0 + seq(0,nrow-1)], size=2, n=nrow, endian="little")
                i0 <- i0 + 2 * nrow
            }
            if (AHRSIncluded[ch]) {
                averageAltimeter$AHRS[averageAltimeter$i,] <- readBin(d$buf[i + i0 + 0:35], "numeric", size=4, n=9, endian="little")
            }
            averageAltimeter$i <- averageAltimeter$i + 1

        } else if (key == 0xa0) { # text FIXME: remove once vectorized
            #v message("HERE DAN HERE DAN")
            #v chars <- rawToChar(d$buf[seq.int(2+d$index[ch], by=1, length.out=-1+d$length[ch])])
            #v t <- strsplit(chars, "\r\n")[[1]]
            #v if (!typeGiven) {
            #v     type <- gsub('.*STR="([^"]*)".*$', '\\1', t[grep("^ID,",t)])
            #v     message("inferred type as '", type, "' from a text record")
            #v     typeGiven <- TRUE
            #v }
            #v text$text[[text$i]] <- t
            #v text$i <- text$i + 1
            #oceDebug(debug, "added to text; now, text$i=", text$i, "\n")
        } else {
            # stop("unknown key 0x", as.raw(key), "; only 0x15 through 0x1f, plus 0xa0, are permitted", sep="")
            #cat("unknown key=", key, "\n", sep="")
            keyname <- paste0("0x", as.character(as.raw(key)))
            if (keyname %in% names(unknownKeys)) {
                unknownKeys[[keyname]] <- unknownKeys[[keyname]] + 1
            } else {
                unknownKeys[[keyname]] <- 1
            }
        }
        if (monitor)
            setTxtProgressBar(progressBar, ch)
    }
    if (monitor)
        close(progressBar)
    if (length(unknownKeys)) {
        msg <- ""
        for (kn in names(unknownKeys))
            msg <- paste0(msg, "   key=", kn, " found ", unknownKeys[[kn]], " times\n")
        #warning("data records with 'id' that is not yet handled:\n", msg)
    }

    # Prepare data
    data <- list(powerLevel=powerLevel, # FIXME: put in individual items?
        status=status,
        activeConfiguration=activeConfiguration,
        orientation=orientation)
    if (!is.null(burst)) {             # 0x15
        burst$i <- NULL
        data$burst <- burst
    }
    if (!is.null(average)) {           # 0x16
        average$i <- NULL
        data$average <- average
    }
    if (!is.null(bottomTrack)) {       # 0x17
        bottomTrack$i <- NULL
        data$bottomTrack <- bottomTrack
    }
    if (!is.null(interleavedBurst)) {  # 0x18
        interleavedBurst$i <- NULL
        data$interleavedBurst <- interleavedBurst
    }
    if (!is.null(burstAltimeterRaw)) {    # 0x1a
        oceDebug(debug, "storing data$burstAltimeterRaw, for later inclusion into returned value\n")
        burstAltimeterRaw$i <- NULL
        data$burstAltimeterRaw <- burstAltimeterRaw
    }
    if (!is.null(DVLBottomTrack)) {    # 0x1b
        DVLBottomTrack$i <- NULL
        data$DVLBottomTrack <- DVLBottomTrack
    }
    if (!is.null(echosounder)) {       # 0x1c
        echosounder$i <- NULL
        data$echosounder <- echosounder
    }
    if (!is.null(DVLWaterTrack)) {     # 0x1d
        DVLWaterTrack$i <- NULL
        data$DVLWaterTrack <- DVLWaterTrack
    }
    if (!is.null(altimeter)) {         # 0x1e
        altimeter$i <- NULL
        data$altimeter <- altimeter
    }
    if (!is.null(averageAltimeter)) {  # 0x1f
        averageAltimeter$i <- NULL
        data$averageAltimeter <- averageAltimeter
    }
    #if (!is.null(header)) {            # 0xa0 (first instance only, also in metadata$header)
    #    data$text <- list()
    #    data$text[[1]] <- header
    #}

    # Insert metadata
    res@metadata$id <- id
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
    oceDebug(debug, "} # read.adp.ad2cp()\n", unindent=1)
    res
}

