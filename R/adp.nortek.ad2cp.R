# vim:textwidth=80:expandtab:shiftwidth=4:softtabstop=4

#' Trim an AD2CP File
#'
#' Create an AD2CP file by copying the first `n` data chunks (regions starting
#' with `0xa5`, etc) of another such file. This can be useful in supplying
#' small sample files for bug reports.
#'
#' @param infile name of an AD2CP file.
#'
#' @param n integer indicating the number of data chunks to keep. The default is
#' to keep 100 chunks, a common choice for sample files.
#'
#' @param outfile optional name of the new AD2CP file to be created. If this is not
#' supplied, a default is used, by adding `_trimmed` to the base filename, e.g.
#' if `infile` is `"a.ad2cp"` then `outfile` will be `a_trimmed.ad2cp`.
#'
#' @param debug an integer value indicating the level of debugging. If
#' this is 1L, then a brief indication is given of the processing steps. If it
#' is > 1L, then information is given about each data chunk, which can yield
#' very extensive output.
#'
#' @return `adpAd2cpFileTrim()` returns the name of the output file, `outfile`, as
#' provided or constructed.
#'
#' @family things related to adp data
#' @family things related to ad2cp data
#' @family functions that trim data files
#' @examples
#'\dontrun{
#' # Can only be run by the developer, since it uses a private file.
#' f  <- "/Users/kelley/Dropbox/oce_secret_data/ad2cp/byg_trimmed.ad2cp"
#' if (file.exists(f))
#'     adpAd2cpFileTrim(f, 100L) # this file is already trimmed to 200 chunks
#'}
#' @author Dan Kelley
adpAd2cpFileTrim <- function(infile, n=100L, outfile, debug=getOption("oceDebug"))
{
    oceDebug(debug, "adpAd2cpFileTrim(infile=\"", infile, "\", n=", n, ", debug=", debug, ") { #\n", unindent=1)
    debug <- ifelse(debug < 1, 0L, ifelse(debug < 2, 1, 2))
    if (missing(infile))
        stop("must provide 'infile'")
    n <- as.integer(n)
    if (n < 1L)
        stop("'n' must be a positive number, but it is ", n)
    if (missing(outfile)) {
        #outfile <- gsub("(.*).ad2cp", "\\1_trimmed.ad2cp", infile)
        outfile <- gsub("^(.*)\\.([^.]*)$", "\\1_trimmed.\\2", infile)
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
    oceDebug(debug, "} # adpAd2cpFileTrim\n", unindent=1)
    outfile
}

# private function
ad2cpDefaultDataItem <- function(x, j=NULL, order=c("burst", "average",
        "bottomTrack", "interleavedBurst", "burstAltimeterRaw",
        "DVLBottomTrack", "DVLWaterTrack", "echosounder", "echosounderRaw",
        "altimeter", "averageAltimeter"))
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
    if (is.null(x))
        return(NULL)
    if (is.character(x)) {
        header <- x
    } else if (is.ad2cp(x)) {
        header <- x[["header"]]
        if (is.null(header))
            return(NULL)
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
    if (0 == length(grep(item, hline)))
        return(if (missing(default)) NULL else default)
    res <- gsub(paste("^.*", item, "=([^,]*).*$", sep=""), "\\1", hline)
    if (nchar(res)) {
        res <- if (numeric) as.numeric(res) else gsub("\"", "", res)
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
#' | code (raw) | code (integer) |            oce name |
#' |      ----: |          ----: |               ----: |
#' | ---------- | -------------- |   ----------------- |
#' |     `0x15` |             21 |             `burst` |
#' |     `0x16` |             22 |           `average` |
#' |     `0x17` |             23 |       `bottomTrack` |
#' |     `0x18` |             24 |  `interleavedBurst` |
#' |     `0x1a` |             26 | `burstAltimeterRaw` |
#' |     `0x1b` |             27 |    `DVLBottomTrack` |
#' |     `0x1c` |             28 |       `echosounder` |
#' |     `0x1d` |             29 |     `DVLWaterTrack` |
#' |     `0x1e` |             30 |         `altimeter` |
#' |     `0x1f` |             31 |  `averageAltimeter` |
#' |     `0x23` |             35 |    `echosounderRaw` |
#' |     `0xa0` |            160 |              `text` |
#'
#' @param code a [raw] (or corresponding integer) vector indicating the IDs of
#' interest, or NULL to get a summary of possible values.
#'
#' @param prefix logical value indicating whether to show the raw value as a
#' prefix (e.g. `"0x1c=echosounder"` as opposed to `"echosounder"`).
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
ad2cpCodeToName <- function(code=NULL, prefix=TRUE)
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
        echosounderRaw=as.raw(0x23),
        text=as.raw(0xa0))
    if (is.null(code)) {
        rval <- data.frame(
            "code"=paste0("0x", as.raw(table), " (=", as.integer(table), ")"),
            "name"=names(table))
    } else {
        code <- as.raw(code)
        rval <- rep("", length(code))
        tnames <- names(table)
        for (i in seq_along(code)) {
            m <- match(code[i], table)
            rval[i] <- if (prefix) {
                paste0("0x", as.raw(code[i]), "=", if (is.na(m)) "?" else tnames[m])
            } else {
                tnames[m]
            }
        }
    }
    rval
}

#' Read a Nortek AD2CP File
#'
#' This function is under active development and may change without notice.  In
#' contrast with other `oce` reading functions, [read.adp.ad2cp()] focusses just
#' on one data type within the source file.  Another difference is that it can
#' either return an object holding the data or just a data frame holding a
#' description of the data types in the file; indeed, the latter is the default.
#' See \dQuote{Details} for more on the reasons for these departures from the
#' usual `oce` pattern.
#'
#' Why does [read.adp.ad2cp()] focus only on parts of the data file? The answer
#' lies in the AD2CP format itself, which may combine data subsets of such
#' differing natures as to break with the `oce` system of pairing a `metadata`
#' slot with a `data` slot.  For example, in a conventional ADP dataset, the
#' `metadata` slot has items for the sampling times, the number of beams, the
#' blanking distance, the cell size, the number of cells, etc.  Such items have
#' a natural pairing with elements of the `data` slot, and `oce` uses this
#' pairing in constructing plots and other items. However, an AD2CP file might
#' combine such data with echosounder measurements, and these will have
#' different values for number of beams and so forth.  This poses a challenge
#' in naming conventions within the `oce` object, with ripple effects for
#' plotting and data access.  Those ripple effects would extend beyond `oce`
#' itself to user code.  To avoid such problems, [read.adp.ad2cp()]
#' is designed to focus on one data type at a time, relying on users to
#' keep track of the resultant object, perhaps to combine it with other objects
#' from within the AD2CP file or other files, in the normal R manner.
#'
#' The permitted values for `dataType` are shown in the table below;
#' the `dataType` argument of [read.adp.ad2cp()] may be chosen from any
#' of the three columns in this table.
#'
#' | code (raw) | code (integer) |            oce name |
#' |      ----: |          ----: |               ----: |
#' | ---------- | -------------- |   ----------------- |
#' |     `0x15` |             21 |             `burst` |
#' |     `0x16` |             22 |           `average` |
#' |     `0x17` |             23 |       `bottomTrack` |
#' |     `0x18` |             24 |  `interleavedBurst` |
#' |     `0x1a` |             26 | `burstAltimeterRaw` |
#' |     `0x1b` |             27 |    `DVLBottomTrack` |
#' |     `0x1c` |             28 |       `echosounder` |
#' |     `0x1d` |             29 |     `DVLWaterTrack` |
#' |     `0x1e` |             30 |         `altimeter` |
#' |     `0x1f` |             31 |  `averageAltimeter` |
#' |     `0x23` |             35 |    `echosounderRaw` |
#' |     `0xa0` |            160 |              `text` |
#'
# The coding is based mainly on descriptions in various versions of a Nortek
# manual (see \dQuote{References}). However, there are some gaps and
# contradictions in these manuals, owing partly to evolution of the data
# format. These things posed a challenge in the writing of [read.adp.ad2cp()].
# Thankfully, personnel in Nortek technical support team were able to supply
# the help that was needed.
#
# Comments in the code, along with some warnings and messages that may be
# issued during processing, are used to highlight some areas that may need
# attention in revisions to this function.
#
# Early in the year 2022, support was added for 12-byte headers. Until
# August 2022, this support was provisional and the results were unlikely
# to be correct. However, personal contacts with Nortek experts shed
# a great deal of light on the format, and so the present results are
# thought to be correct.  At about the same time, support was added for
# what oce calls `echosounderRaw` format, the details of which were
# kindly communicated by Nortek personnel, in lieu of official documentation
# that had not yet been finalized.
#
# The \dQuote{References} section lists some manuals that were consulted during
# the coding of `read.adp.ad2cp()].  Since instruments evolve over time, one
# might think that Nortek (2022) would be the best place to start, in coding to
# read AD2CP files. That would be a mistake, because that manual (as of August
# 2022) employs a new presentation style that is less straightforward than the
# older manuals, with some significant gaps (e.g. no discussion of the checksum
# computation method) and errors (e.g. in stating storage classes, whether
# floating-point or integer).  A new manual is expected soon, however, and this
# is expected to lead the way revision of this function and its documentation.
#
# 2. The Nortek (2022) explanation of the data format differs from the older
# explanations and is arguably more difficult to understand.  With the new
# leading-underscore format (see Nortek 2022, page 79), information is spread
# throughout the document, making it challenging to understand data fields in
# isolation.  The older documents laid things out more clearly, e.g. the
# average/burst format is laid out in detail, *in one place* on pages 57 to 64
# of Nortek, with the optional fields being clearly labelled in the rightmost
# column of Table 6.1.3.
#
# 3. Nortek (2022) does not always specify units correctly.  For example, on
# page 82, Pressure is said to have "Unit \[dBar\]" in green text, but the
# black text above states "Raw data given as 0.001 dBar". If the stated storage
# class (uint32) is to be believed, then it seems clear that the unit must be
# 0.001 dBar, so the green text should be ignored.  The same can be said of
# items throughout the data-format tables. In coding `read.adp.ad2cp()], the
# green "Unit" text was ignored in basically every case.
#
# Second, Nortek (2022) contains significant errors, e.g. the following.
#
# 1. Nortek (2022 page 89) states the storage class for "Altimeter
# data. Altimeter distance" (called `AltimeterDistance` by the present function)
# to be `int32`, but Nortek (2017, 2018) both state it to be `float32`. Tests
# with actual datasets make it clear that the format is `float32`, since wild
# result are inferred by following the Nortek (2022) guidance.
#
# 2. As above, but for "AST data.AST distance" (called `ASTDistance` by the
# present function).
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
#' @param dataType an indication of the data type to be extracted.  If this is
#' NULL (the default) then `read.adp.ad2cp()` returns a data frame indicating
#' the data type occurrence rate in the file.  Otherwise, `dataType` must be
#' either a numeric or character value (see \dQuote{Details}).  In the numeric
#' case, which includes both base-10 numbers and `raw` values, `dataType` is
#' converted to an integer that is taken to indicate the data type via ID. The
#' permitted values follow the Nortek convention, a summary of which is shown
#' the table at the start of the \dQuote{Details} section.  In the character
#' case, it must be a string taken from that same table.
#'
#' @param dataSet a positive integer that indicates which of the possibly
#' several data sets stored within a file is to be focussed upon.  By
#' default, the first data set is chosen. Note that data sets are found
#' by trying to match each text data chunk against the regular expression
#' `"^GETCLOCKSTR,TIME="`.
#'
#' @param tz a character value indicating time zone. This is used in
#' interpreting times stored in the file.
#'
# @param ignoreChecksums a logical value indicating whether to ignore
# checksums.  This is FALSE by default, meaning that any data chunk with an
# improper checksum is ignored.  It may be necessary to set this to TRUE to
# parse some problematic files, but users are asked to report issues in
# such cases.  (This parameter may be removed without notice.)
#'
#' @param longitude,latitude numerical values indicating the observation
#' location.
#'
#' @param plan optional integer specifying which 'plan' to focus on (see
# reference 1 for the meaning of 'plan').  If this is not given, it defaults to
# the most common plan in the requested subset of the data.
#'
#- @param type optional character value indicating the type of Nortek
#- instrument.  Normally, this is inferred from the file contents, but
#- if an error is reported that no header is found, the user may
#- find it useful to set the `type` argument. The permitted choices are
#- `"Signature100"`, `"Signature250"`, `"Signature500"`, and
#- `"Signature1000"`.
#'
#' @param TOC a logical value.  If this is FALSE (the default) then
#' the other parameters of the function are used to select data from
#' the indicated `filename`, and an [adp-class] object is returned.
#' However, if `TOC` is TRUE, then the number of datasets held within
#' the file is returned.
#'
#' @param debug an integer value indicating the level of debugging.  Set to 1 to
#' get a moderate amount of debugging information, from the R code only, to 2 to
#' get some debugging information from the C++ code that is used to parse the
#' data chunks, or to 3 for intensive debugging at both levels.
#'
#' @param orientation,distance,monitor,despike ignored, provided only for
#' calling compatibility with other functions that read [adp-class]
#' files.  A warning is issued if any of these is supplied in a call
#' to `read.adp.ad2cp()`.
#'
#' @param \dots ignored parameters that might be passed to `read.adp.ad2cp()`
#' by [read.oce()].
#'
# @examples
#\dontrun{
# d <- read.adp.ad2cp("~/test.ad2cp", to=100) # or read.oce()
#}
#'
#' @return `read.adp.ad2cp()` returns either an [adp-class] object or
#' the number of data sets within the file, according to the value
#' of `TOC`.
#'
#' @author Dan Kelley
#'
#' @references
#'
#' Nortek AS. \dQuote{Signature Integration 55|250|500|1000kHz.} Nortek AS,
#' 2017.
#'
#' Nortek AS. \dQuote{Signature Integration 55|250|500|1000kHz.} Nortek AS,
#' 2018.
#'
#' Nortek AS. \dQuote{Signature Integration 55|250|500|1000kHz.} Nortek AS,
#' March 31, 2022.
#'
# Nortek AS. \dQuote{Operations Manual - Signature 250, 500 and 1000.} Nortek AS,
# September 21, 2018.
#'
#' @family things related to adp data
#' @family things related to ad2cp data
#'
#' @examples
#' library(oce)
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
read.adp.ad2cp <- function(file,
    from=1L, to=0L, by=1L, dataType=NULL, dataSet=1L,
    tz=getOption("oceTz"), longitude=NA, latitude=NA, plan, TOC=FALSE,
    debug=getOption("oceDebug"),
    orientation, distance, monitor, despike, # ignored; warning issued if provided
    ...)
{
    if (!is.logical(TOC))
        stop("TOC must be a logical value, but it is ", TOC)
    dataSet <- as.integer(dataSet)
    if (dataSet < 1L)
        stop("dataSet=", dataSet, " is not permitted; please supply a positive integer")
    originalParameters <- list(from=from, to=to, by=by, dataSet=dataSet, dataType=dataType,
        tz=tz, longitude=longitude, latitude=latitude)
    i0v <- 0L                          # global variable that some functions alter using <<-
    i <- 0L                            # global variable that some functions alter using <<-
    dataTypeChoices <- list("burst"=0x15,
        "average"=0x16,
        "bottomTrack"=0x17,
        "interleavedBurst"=0x18,
        "burstAltimeterRaw"=0x1a,
        "DVLBottomTrack"=0x1b,
        "echosounder"=0x1c,
        "DVLWaterTrack"=0x1d,
        "altimeter"=0x1e,
        "averageAltimeter"=0x1f,
        "echosounderRaw"=0x23)
    dataTypeOrig <- dataType
    if (!is.null(dataType)) {
        #oceDebug(debug, "original dataType=\"", dataType, "\n")
        if (length(dataType) > 1L)
            stop("length of dataType (", length(dataType), ") must not exceed 1")
        if (is.character(dataType)) {
            if (!identical(dataType, "TOC")) {
                if (dataType %in% names(dataTypeChoices)) {
                    dataType <- dataTypeChoices[[dataType]]
                } else {
                    stop("dataType=\"", dataType, "\" not understood; try one of: \"",
                        paste(names(dataTypeChoices), collapse="\", \""), "\"")
                }
            }
        } else if (is.numeric(dataType)) {
            dataType <- as.integer(dataType)
            if (!(dataType %in% as.integer(dataTypeChoices))) {
                stop("dataType=", dataType, " not understood; try one of: ",
                    paste(as.integer(dataTypeChoices), collapse=", "))
            }
        } else {
            stop("dataType must be character or numeric")
        }
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
    if (is.null(dataType) && !TOC)
        stop("must supply 'dataType'; hint: use read.adp.ad2cp(filename,TOC=TRUE) to discover file contents")
    if (!missing(orientation))
        warning("ignoring 'orientation' (see documentation)")
    if (!missing(distance))
        warning("ignoring 'distance' (see documentation)")
    if (!missing(monitor))
        warning("ignoring 'monitor' (see documentation)")
    if (!missing(despike))
        warning("ignoring 'despike' (see documentation)")
    fromGiven <- !missing(from)
    toGiven <- !missing(to)
    byGiven <- !missing(by)
    planGiven <- !missing(plan)
    if ((fromGiven && toGiven) && !(from < to) && to != 0L)
        stop("require from<to but got from=", from, " and to=", to)

    oceDebug(debug, "read.adp.ad2cp(filename",
        ", dataType=0x", as.raw(dataType), " (original was ", dataTypeOrig, ")",
        ", from=", if (fromGiven) format(from) else "(missing)",
        ", to=", if (toGiven) to else "(missing)",
        ", by=", if (byGiven) by else "(missing)",
        "  plan=", if (planGiven) plan else "(missing)",
        ", debug=", debug,
        #", ignoreChecksums=", ignoreChecksums,
        ", ...)\n", sep="", unindent=1, style="bold")
    #- if (typeGiven) {
    #-     typeAllowed <- c("Signature100", "Signature250", "Signature500", "Signature1000")
    #-     typei <- pmatch(type, typeAllowed)
    #-     if (is.na(typei))
    #-         stop("type must be \"Signature100\", \"Signature250\", \"Signature500\", or \"Signature1000\", but it is \"", type, "\"")
    #-     type <- typeAllowed[typei]
    #- }
    # Check from, to, and by.
    if (!fromGiven)
        from <- 1L
    from <- as.integer(from)
    if (from < 1L)
        stop("from=", from, " is not allowed; use a value of 1 or more")
    if (!toGiven)
        to <- 0L
    to <- as.integer(to)
    if (to < 0L)
        stop("to=", to, " is not allowed; use a value of 0 (for all data) or more (for specified limit)")
    if (to == 0)
        to <- 1e9                      # this should be enough to read any file
    if (!byGiven)
        by <- 1L
    by <- as.integer(by)
    if (by < 1L)
        stop("by=", by, " is not allowed; use a value of 1 or more")
    oceDebug(debug, "after some analysis, have from=", from, ", to=", to, " and by=", by, "\n", sep="")
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
    # Read whole file into a buffer
    seek(file, 0, "start")
    seek(file, where=0, origin="end")
    fileSize <- seek(file, where=0)
    seek(file, 0, "start")
    oceDebug(debug, vectorShow(fileSize))
    buf <- readBin(file, what="raw", n=fileSize, size=1)
    oceDebug(debug, "first 10 bytes in file: ",
        paste(paste("0x", head(buf, 10), sep=""), collapse=" "), "\n", sep="")
    headerSize <- as.integer(buf[2])
    oceDebug(debug, "headerSize:", headerSize, "\n")
    ID <- buf[3]
    oceDebug(debug, "ID: 0x", ID, " (NB: 0x15=burst data record; 0x16=avg data record; 0x17=bottom ",
        "track record; 0x18=interleaved data record; 0xa0=string data record, e.g. GPS NMEA, ",
        "comment from the FWRITE command)\n", sep="")
    dataSize <- readBin(buf[5:6], what="integer", n=1, size=2, endian="little", signed=FALSE)
    oceDebug(debug, "dataSize:", dataSize, "\n")
    oceDebug(debug, "buf[1+headerSize+dataSize=", 1+headerSize+dataSize, "]=0x", buf[1+headerSize+dataSize], " (expect 0xa5)\n", sep="")
    # Note that we read the *whole* file; from, to and by are used later, for
    # the particular plan,dataSet,dataType value that is the focus here.  We use
    # from, to and by in a few lines, when focusIndex is defined.
    nav <- do_ldc_ad2cp_in_file(filename, from=1L, to=1e9, by=1L, DEBUG=debug-1L)
    d <- list(buf=buf, index=nav$index, headerLength=nav$headerLength, dataLength=nav$dataLength, id=nav$id)
    oceDebug(debug, vectorShow(length(d$index)))
    N <- length(d$index)
    #-message("L635 N=",N,", to=", to)
    # Set up object, with key metadata to allow other functions to work.
    res <- new("adp")
    # FIXME: THIS IS WRONG: we should be focussing on d focussed by focusIndex.
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
    # docs refer to the first bit as 0, which becomes [1, ] in this array. Note
    # that we may drop some elements (if they are not in the current 'plan')
    # later, depending on 'keep'.
    status <- intToBits(readBin(d$buf[pointer4 + 69L], "integer", size=4L, n=N, endian="little"))
    #-message("L665 ", vectorShow(status, showNewline=FALSE))
    #-message("L666 ", vectorShow(N, showNewline=FALSE))
    dim(status) <- c(32L, N)
    # Interpret status, but note that items will be subsetted later (see 'keep')
    #
    # Read blankingDistanceInCm.  This is bit 1 in the Nortek (2022 table 6.3
    # page 85) zero-based notation is index 2 in R.
    #
    # NOTE: 2022-08-29 Nortek informs me that the blankingDistance is *always*
    # in mm, for echosounder=0x1c data, so we alter that here. Nortek considers
    # this a bug, and has plans to alter how this works, but we issue a warning
    # here to let the user know that their dataSet is (in this one set of bits)
    # faulty.
    blankingDistanceInCm <- as.integer(status[2L, ])
    echosounderChunks <- which(d$id == 0x1c)
    if (length(echosounderChunks)) {
        if (any(blankingDistanceInCm[echosounderChunks])) {
            warning("In read.adp.ad2cp() : setting blankingDistanceInCm to FALSE for echosounder to ",
                "handle an error noted by Nortek on 2022-08-29", call.=FALSE)
            if (debug > 0) {
                df <- data.frame(id=paste0("0x", as.raw(d$id)),
                    blankingDistanceInCmOriginal=blankingDistanceInCm)
            }
            blankingDistanceInCm[echosounderChunks] <- FALSE
            if (debug > 0) {
                df$blankingDistanceInCmUpdated <- blankingDistanceInCm
                print(df)
            }
        }
    }
    # Bit 16 in Nortek (zero-based) notation is bit 17 here.
    # count from 0, so it is bit 17 here.
    activeConfiguration <- as.integer(status[17L, ])
    #-message("L698 ", vectorShow(activeConfiguration, showNewline=FALSE))
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
    # If the 'plan' argument is missing, we select the most common one in the data subset.
    # See https://github.com/dankelley/oce/issues/2050 which shows that, as of
    # 2023-03, it is quite unclear what to do with plan.
    #
    # TEST message("table(activeConfiguration):")
    # TEST print(table(activeConfiguration))
    #message("FIXME saving activeConfiguration to activeConfiguration.rda");save(activeConfiguration,file="activeConfiguration.rda")
    if (!planGiven) {
        u <- unique(activeConfiguration)
        nu <- length(u)
        if (nu == 1) {
            plan <- activeConfiguration[1]
            message("In read.adp.ad2cp() : setting plan=", plan, ", the only value in the file")
        } else {
            plan <- u[which.max(unlist(lapply(u, function(x) sum(activeConfiguration==x))))]
            acTable <- table(activeConfiguration)
            message("In read.adp.ad2cp() : setting plan=", plan,
                ", the most common value in this file; ",
                paste(names(acTable), " occurs ", unname(acTable), " time[s]", sep="", collapse="; "))
            #message("A table of plan values is as follows")
            #print(acTable, file=stderr())
        }
    }
    # Find text blocks, some of which are configuration headers. This code has
    # altered substantially over time, as I've come to learn more about the data
    # format, and studied some sample files.  Be on the lookout for old variable
    # names that don't quite make sense anymore.
    #- header <- NULL
    idText <- which(d$id == 0xa0) # text chunk
    oceDebug(debug, vectorShow(idText))
    textBlocks <- lapply(idText, function(i) {
        chars <- rawToChar(d$buf[seq.int(2L+d$index[i], by=1L, length.out=-1L+d$dataLength[i])])
        strsplit(chars, "\r\n")[[1]]
    })
    # Find configuration (AKA header) blocks, as opposed to other strings.
    textBlockIsConfig <- sapply(textBlocks, function(b) grepl("^GETCLOCKSTR,TIME=", b[[1]]))
    configText <- textBlocks[textBlockIsConfig]
    numberOfDataSets <- length(configText)
    oceDebug(debug, "This file has ", pluralize(numberOfDataSets, "data set"), "\n")
    # nolint start object_usage_linter
    commentText <- textBlocks[!textBlockIsConfig]
    # nolint end object_usage_linter
    if (dataSet > numberOfDataSets)
        stop("Cannot access dataSet ", dataSet, " because the file contains only ", pluralize(numberOfDataSets, "dataset"))
    dataSetTime <- as.POSIXct(sapply(configText,
            function(h) gsub(".*=\"(.*)\"", "\\1", h[1])), tz="UTC")
    dataSetStart <- idText[textBlockIsConfig]
    dataSetEnd <- c(diff(dataSetStart), length(d$id))
    oceDebug(debug, vectorShow(dataSetStart))
    oceDebug(debug, vectorShow(dataSetEnd))
    if (TOC) {
        rval <- list()
        for (ds in seq_len(numberOfDataSets)) {
            #<2053>#See https://github.com/dankelley/oce/issues/2053
            #<2053>cat("dataSet ", ds, ", starting at ", format(dataSetTime[ds]), " ", tz, ", contains\n", sep="")
            look <- seq(dataSetStart[ds], dataSetEnd[ds])
            t <- table(d$id[look])
            names <- names(t)
            tv <- as.vector(t)
            #<2043>for (i in seq_along(names)) {
            #<2043>    cat("    ", pluralize(tv[i], "record"), " with dataType ", names[i],
            #<2043>        " (\"", ad2cpCodeToName(names[i], prefix=FALSE), "\")\n", sep="")
            #<2043>}
            rval[[paste("dataSet", ds, "at", format(dataSetTime[ds]))]] <-
                data.frame(`ID hex`=paste0("0x", as.raw(as.integer(names))),
                    `ID dec`=as.integer(names),
                    `dataType`=ad2cpCodeToName(names, prefix=FALSE),
                    Count=tv)
        }
        return(rval)
    }
    # We have handled the TOC case.
    oceDebug(debug, "this is not a TOC call\n")
    if (dataSet > numberOfDataSets)
        stop("cannot access dataSet=", dataSet, " because this file contains only ",
            pluralize(numberOfDataSets, "dataset"))
    # Extract the header, and the 'type' named therein.
    header <- configText[[dataSet]]
    type <- gsub(".*STR=\"([^\"]*)\".*$", "\\1", header[grep("^ID,", header)])
    # Build up an index sieve.
    dataSetKeep <- rep(FALSE, length(d$index))
    dataSetKeep[seq.int(dataSetStart[dataSet], dataSetEnd[dataSet])] <- TRUE
    planKeep <- activeConfiguration == plan
    dataTypeKeep <- d$id == dataType
    # Is the user asking for a plan that is not in the file?
    if (sum(planKeep) == 0L)
        stop("there are no data for plan=", plan, "; try one of the following values instead: ",
            paste(unique(activeConfiguration), collapse=" "))
    # Internal checks -- should never fail.
    if (length(planKeep) != length(dataSetKeep))
        stop("length(planKeep)=", length(planKeep), " != length(dataSetKeep)=",
            length(dataSetKeep), "; please report error to developers")
    if (length(planKeep) != length(dataTypeKeep))
        stop("length(planKeep)=", length(planKeep), " != length(datatypeKeep)=",
            length(dataTypeKeep), "; please report error to developers")
    keep <- planKeep & dataSetKeep & dataTypeKeep
    #-print(table(keep))
    #-message("DANNY");browser()
    if (sum(keep) < length(keep)) {
        oceDebug(debug, "retaining ", sum(keep), " records, or ", round(100*sum(keep)/length(keep), 4), "% of file\n")
        keep2 <- which(keep)
        keep3 <- keep2[seq(from=from, to=min(to, length(keep2)), by=by)]
        N <- length(keep3)
        d$index <- d$index[keep3]
        d$headerLength <- d$headerLength[keep3]
        d$dataLength <- d$dataLength[keep3]
        d$id <- d$id[keep3]
        status <- status[, keep3, drop=FALSE]
        #message(vectorShow(blankingDistanceInCm))
        blankingDistanceInCm <- blankingDistanceInCm[keep3]
        #message(vectorShow(blankingDistanceInCm))
        activeConfiguration <- activeConfiguration[keep3]
        orientation1 <- orientation1[keep3]
        orientation2 <- orientation2[keep3]
        orientation3 <- orientation3[keep3]
        orientation <- orientation[keep3]
        pointer1 <- d$index
        pointer2 <- as.vector(t(cbind(pointer1, 1 + pointer1))) # rbind() would be fine, too.
        pointer4 <- as.vector(t(cbind(pointer1, 1 + pointer1, 2 + pointer1, 3 + pointer1)))
        pointer2NEW <- gappyIndex(pointer1, 0, 2)
        pointer4NEW <- gappyIndex(pointer1, 0, 4)
        if (!all.equal(pointer2, pointer2NEW))
            warning("DEVELOPER NOTE: pointer2NEW != pointer2 at spot 2")
        if (!all.equal(pointer4, pointer4NEW))
            warning("DEVELOPER NOTE: pointer4NEW != pointer4 at spot 2")
        oceDebug(debug, "focussing on ", length(pointer1), " records (after subsetting for plan=", plan,
            ", dataSet=", dataSet, ", and dataType=", dataType, ")\n", sep="")
    }
    #if (debug > 0) {
    #    oceDebug(debug, "below is table() of the 'plan' values in this subset of the file:\n")
    #    print(table(activeConfiguration))
    #}

    # commonData (Nortek 2022 Table 6.2 Page 81)
    commonData <- list()

    # "Version" in Nortek (2022 table 6.2 page 81)
    # NB. this can vary across IDs, e.g. in private test file f2, the text chunk
    # (i.e. the header) has version 16, while the other records had version 3.
    commonData$version <- as.integer(d$buf[pointer1+1L])
    commonData$offsetOfData <- as.integer(d$buf[pointer1+2L])
    commonData$configuration <- local(
        {
            tmp <- rawToBits(d$buf[pointer2+3L]) == 0x01
            dim(tmp) <- c(16, N)
            t(tmp)
        }
    )
    # The above block can be done as below, if R is new enough to offer |>.
    #> commonData$configuration <- (rawToBits(d$buf[pointer2+3L]) == 0x01) |> matrix(byrow=TRUE, ncol=16L)

    commonData$serialNumber <- readBin(d$buf[pointer4+5L], "integer", n=N, size=4L)
    # The vectorization scheme used in this function assumes that configurations
    # match within a given ID type.  This seems like a reasonable assumption,
    # and one backed up by the impression of a Nortek representative, but I do
    # not see definitive statement of the requirement in any documentation
    # I've studied. Since we *need* this to be true in order to read the data in
    # vectorized way, we *insist* on it here, rather than trying to catch
    # problems later. Use local() to avoid polluting namespace.
    local(
        {
            oceDebug(debug, "Checking commonData$configuration (a ",
                paste(dim(commonData$configuration), collapse="x"),
                " matrix) consistency within columns {\n")
            badID <- 0L
            for (id in as.raw(unique(d$id))) {
                config <- commonData$configuration[d$id==id, ]
                if (is.matrix(config)) {
                    oceDebug(debug, "    checking id ", ad2cpCodeToName(id), "\n")
                    badColumn <- 0L
                    for (column in seq(2L, ncol(config))) {
                        numberDifferent <- sum(config[, column] != config[1, column])
                        if (numberDifferent > 0L) {
                            oceDebug(debug, "        column ", column, " has inconsistencies in ", numberDifferent,
                                " of the ", nrow(config), " rows\n", sep="")
                            warning("In read.adp.adp2cp() : id ", ad2cpCodeToName(id), " column ", column,
                                " has inconsistencies in ", numberDifferent, " of the ", nrow(config), " rows\n",
                                sep="", call.=FALSE)
                        }
                        if (numberDifferent > 0L) {
                            badColumn <- badColumn + 1L
                        }
                    }
                    if (badColumn == 0L) {
                        oceDebug(debug, "        no inconsistencies in any column\n")
                    } else {
                        badID <- badID + 1L
                        warning("In read.adp.ad2cp() : id ", ad2cpCodeToName(id),
                            " has non-uniform commonData$configuration within ", badColumn, " columns\n",
                            sep="", call.=FALSE)
                    }
                }
            }
            if (badID > 0L) {
                oceDebug(debug, "    summary: commonData$configuration inconsistencies for ", badID, " ID type(s)\n")
                warning("In read.adp.adp2cp() : found commonData$configuration inconsistencies for ", badID, " ID type(s)\n",
                    sep="", call.=FALSE)
            } else {
                oceDebug(debug, "no ID types had inconsistencies\n")
            }
            oceDebug(debug, "} finished checking commonData$configuration consistency\n")
        }
    )
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
    altimeterRawIncluded <- configuration[, 10]
    ASTIncluded <- configuration[, 11]
    echosounderIncluded <- configuration[, 12]
    AHRSIncluded <- configuration[, 13]
    percentGoodIncluded<- configuration[, 14]
    stdDevIncluded <- configuration[, 15]
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
    #
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
        1e-4 * readBin(d$buf[pointer2 + 15], "integer", size=2, n=N, signed=FALSE, endian="little"),
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
    # nolint start commas_linter
    ncells <- BCC[,1]+2*(BCC[,2]+2*(BCC[,3]+2*(BCC[,4]+2*(BCC[,5]+2*(BCC[,6]+2*(BCC[,7]+2*(BCC[,8]+2*(BCC[,9]+2*BCC[,10]))))))))
    nbeams <- BCC[,13]+2L*(BCC[,14L]+2L*(BCC[,15L]+2L*BCC[,16L]))
    # nolint end commas_linter
    # b00=enu, b01=xyz, b10=beam, b11=- [1 page 49]
    coordinateSystem <- c("enu", "xyz", "beam", "?")[1 + BCC[, 11] + 2*BCC[, 12]]
    # BCC case 2
    # nolint start object_useage_linter
    ncellsEchosounderWholeFile <- readBin(d$buf[pointer2 + 31], "integer", size=2, n=N, signed=FALSE, endian="little")
    # nolint end object_useage_linter
    # cell size is recorded in mm [1, table 6.1.2, page 49]
    cellSize <- 0.001 * readBin(d$buf[pointer2 + 33], "integer", size=2, n=N, signed=FALSE, endian="little")
    # BOOKMARK-blankingDistance-1 (see also BOOKMARK-blankingDistance-2 and -3, below)
    #
    # Update 2022-08-29 Nortek informs me that the factor is always 1e-3
    # for echosounder data, and that the factor stored here ought to be
    # ignored until they fix the bug. Since the code in this block is not yet
    # cognizant of the data type, we just read it, and change it to 1e-3 later
    # on (BOOKMARK-blankingDistance-3 below) later, for echosounderRaw data
    # only.
    #
    # Nortek (2017 table 6.1.2 page 49) indicates that blanking distance is
    # recorded in cm but Nortek (2022 section 6.4 page 85) says it is in either
    # cm or mm, depending on an element in status (bit 1 in notation of Nortek
    # 2022 page 87). Perhaps the ability to store in mm was added later?
    # However, in tests/testthat/test_ad2cp_2.R, the value inferred from
    # assuming that Nortek (2022) is correct does not match with the header
    # (header says 2.000 for echosounder but the method below yields 20).
    # Given this confusion, it seems sensible to define blankingDistance
    # here, *but* to change it later, if the file has a header and if that
    # header indicates a different value (at BOOKMARK-blankingDistance-2).
    tmp <- readBin(d$buf[pointer2 + 35L], "integer", size=2, n=N, signed=FALSE, endian="little")
    blankingDistanceFactor <- ifelse(blankingDistanceInCm==1, 1e-2, 1e-3)
    blankingDistance <- blankingDistanceFactor * tmp
    oceDebug(debug, "Steps in the computation of blanking distance\n")
    oceDebug(debug, "    ", vectorShow(tmp, n=10))
    oceDebug(debug, "    ", vectorShow(blankingDistanceInCm, n=10))
    oceDebug(debug, "    ", vectorShow(blankingDistanceFactor, n=10))
    oceDebug(debug, "    ", vectorShow(blankingDistance, n=10))
    nominalCorrelation <- readBin(d$buf[pointer1 + 37], "integer", size=1, n=N, signed=FALSE, endian="little")
    # Magnetometer (Table 6.2, page 82, ref 1b)
    magnetometer <- matrix(0.0, nrow=N, ncol=3)
    magnetometer[, 1] <- readBin(d$buf[pointer2 + 41], "integer", size=2, n=N, signed=TRUE, endian="little")
    magnetometer[, 2] <- readBin(d$buf[pointer2 + 43], "integer", size=2, n=N, signed=TRUE, endian="little")
    magnetometer[, 3] <- readBin(d$buf[pointer2 + 45], "integer", size=2, n=N, signed=TRUE, endian="little")
    # Accelerometer (Table 6.2, page 82, ref 1b)
    # IMOS https://github.com/aodn/imos-toolbox/blob/e19c8c604cd062a7212cdedafe11436209336ba5/Parser/readAD2CPBinary.m#L555
    #  AccRawX starts at idx+46
    #  IMOS_pointer = oce_pointer - 1
    accelerometer <- matrix(0.0, nrow=N, ncol=3)
    accelerometer[, 1] <- 1.0/16384.0 * readBin(d$buf[pointer2 + 47], "integer", size=2, n=N, signed=TRUE, endian="little")
    accelerometer[, 2] <- 1.0/16384.0 * readBin(d$buf[pointer2 + 49], "integer", size=2, n=N, signed=TRUE, endian="little")
    accelerometer[, 3] <- 1.0/16384.0 * readBin(d$buf[pointer2 + 51], "integer", size=2, n=N, signed=TRUE, endian="little")
    # NOTE: all things below this are true only for current-profiler data; see
    # page 82 of Nortek (2022) for the vexing issue of ambiguityVelocity being
    # 2 bytes for current-profiler data but 4 bytes for bottom-track data.
    datasetDescription <- readBin(d$buf[pointer2 + 55], "integer", size=2, n=N, signed=FALSE, endian="little")
    transmitEnergy <- readBin(d$buf[pointer2 + 57], "integer", size=2, n=N, signed=FALSE, endian="little")
    # FIXME: next, using offset 59, is true only for currents ('average' or 'burst').
    # Nortek (2022) page 82.
    velocityFactor <- 10^readBin(d$buf[pointer1 + 59], "integer", size=1, n=N, signed=TRUE, endian="little")
    oceDebug(debug, "velocityFactor=", velocityFactor[1], " (for current-profiler data ONLY)\n")
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

    oceDebug(debug, vectorShow(status[2, ]))
    ensemble <- readBin(d$buf[pointer4+73], "integer", size=4, n=N, endian="little")

    # Limitations
    nconfiguration <- length(unique(activeConfiguration))
    if (1 < nconfiguration) {
        cat("developer-aimed information:\n")
        print(unique(activeConfiguration))
        print(table(activeConfiguration))
        stop("This file has ", nconfiguration, " active configurations, but ",
            "read.adp.ad2cp() can only handle one. ",
            "Please contact the oce developers if you need to work with this file.")
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
    # 0x23 - echosounder-raw (undocumented)
    # 0xA0 - String Data Record, eg. GPS NMEA data, comment from the FWRITE command.
    # Set up pointers to records matching these keys.
    #-message("DAN 1");browser()
    p <- list(burst=which(d$id==0x15),
        average=which(d$id==0x16),
        bottomTrack=which(d$id==0x17),
        interleavedBurst=which(d$id==0x18),
        burstAltimeterRaw=which(d$id==0x1a),
        DVLBottomTrack=which(d$id==0x1b),
        echosounder=which(d$id==0x1c),
        DVLWaterTrack=which(d$id==0x1d),
        altimeter=which(d$id==0x1e),
        echosounderRaw=which(d$id==0x23),
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
                v <- velocityFactor*readBin(d$buf[iv], "integer", size=2L, n=NP*NBC, endian="little")
                object$v <- array(double(), dim=c(NP, NC, NB))
                for (ip in 1:NP) {
                    look <- seq(1L + (ip-1L)*NBC, length.out=NBC)
                    #if (ip < 5L) # FIXME: remove this
                    #    cat("ip=",ip,": ",vectorShow(look))
                    object$v[ip, , ] <- matrix(v[look], ncol=NB, byrow=FALSE)
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
                    look <- seq(1L + (ip-1L)*NBC, length.out=NBC)
                    object$a[ip, , ] <- matrix(a[look], ncol=NB, byrow=FALSE)
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
                    look <- seq(1L + (ip-1L)*NBC, length.out=NBC)
                    object$q[ip, , ] <- matrix(q[look], ncol=NB, byrow=FALSE)
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
            object$altimeterRaw$sampleDistance <- 1e-4 * readBin(buf[iv], "integer", size=2L, n=1, endian="little", signed=FALSE)
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
            oceDebug(debug, "   echosounder starts at i[1]=", i[1], ", i0v=", i0v, " (NC=", NC, ", NP=", NP, ")\n")
            iv <- gappyIndex(i, i0v, 2L*NC)
            #iv <- gappyIndex(i, i0v, 2L*NC)
            tmp <- readBin(buf[iv], "integer", size=2L, endian="little", signed=FALSE, n=NP*NC)
            dim(tmp) <-  c(NC, NP)
            object$echosounder <- t(tmp)
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
                look <- seq(1L + (ip-1L)*9L, length.out=9L)
                # read by row, given docs say M11, then M12, then M13, etc.
                object$AHRS$rotationMatrix[ip, , ] <- matrix(tmp[look], ncol=3, byrow=TRUE) # note byrow
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
            oceDebug(debug, "    stdDev starts at i0v=", i0v, "; see Nortek (2017) p53-54\n")
            iv <- gappyIndex(i, i0v, 2L)
            object$stdDevPitch <- 0.01*readBin(buf[iv], "integer", size=2L, endian="little", n=NP)
            i0v <<- i0v + 2L           # advance for next subitem
            iv <- gappyIndex(i, i0v, 2L)
            object$stdDevRoll <- 0.01*readBin(buf[iv], "integer", size=2L, endian="little", n=NP)
            i0v <<- i0v + 2L           # advance for next subitem
            iv <- gappyIndex(i, i0v, 2L)
            object$stdDevHeading <- 0.01*readBin(buf[iv], "integer", size=2L, endian="little", n=NP)
            i0v <<- i0v + 2L           # advance for next subitem
            iv <- gappyIndex(i, i0v, 2L)
            object$stdDevPressure <- 0.001*readBin(buf[iv], "integer", size=2L, endian="little", n=NP)
            i0v <<- i0v + 2L           # advance for next subitem
            iv <- gappyIndex(i, i0v, 2L)
            object$stdDev <- 0.01*readBin(buf[iv], "integer", size=2L, endian="little", n=NP)
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
        configuration0 <- configuration[look[1], ]
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
            magnetometer=magnetometer[look, ],
            accelerometer=accelerometer[look, ],
            datasetDescription=datasetDescription[look],
            temperatureMagnetometer=temperatureMagnetometer[look],
            temperatureRTC=temperatureRTC[look],
            transmitEnergy=transmitEnergy[look],
            powerLevel=powerLevel[look])
        oceDebug(debug, "configuration0=", paste(ifelse(configuration0, "T", "F"), collapse=", "), "\n")
        oceDebug(debug, "vector-read 'burstAltimeterRaw' records (0x1a) {\n")
        # See CR's snapshot at
        # https://github.com/dankelley/oce/issues/1959#issuecomment-1141409542
        # which is p89 of Nortek AS. “Signature Integration
        # 55|250|500|1000kHz.” Nortek AS, March 31, 2022)
        i <<- d$index[look]            # pointers to "burstAltimeterRaw" chunks in buf
        oceDebug(debug, vectorShow(i, n=4))
        # nolint start object_usage_linter
        i0v <<- 77                     # pointer to data (incremented by getItemFromBuf() later).
        NP <- length(i)                # number of profiles of this type
        NC <- rval$numberOfCells       # number of cells for v,a,q
        NB <- rval$numberOfBeams       # number of beams for v,a,q
        p1 <- p$burstAltimeterRaw[1]
        # nolint end object_usage_linter
        # Nortek (2022 page 89) "Altimeter raw data.NumRawSamples at ALLTIRAWSTART + 8
        #print(configuration0)
        if (configuration0[6])
            rval <- getItemFromBuf(rval, "v", i=i, type=type, debug=debug)
        if (configuration0[7])
            rval <- getItemFromBuf(rval, "a", i=i, type=type, debug=debug)
        if (configuration0[8])
            rval <- getItemFromBuf(rval, "q", i=i, type=type, debug=debug)
        if (configuration0[9])
            rval <- getItemFromBuf(rval, "altimeter", i=i, type=type, debug=debug)
        if (configuration0[11])
            rval <- getItemFromBuf(rval, "AST", i=i, type=type, debug=debug)
        if (configuration0[10])
            rval <- getItemFromBuf(rval, "altimeterRaw", i=i, type=type, debug=debug)
        if (configuration0[12])
            rval <- getItemFromBuf(rval, "echosounder", i=i, type=type, debug=debug)
        if (configuration0[13])
            rval <- getItemFromBuf(rval, "AHRS", i=i, type=type, debug=debug)
        if (configuration0[14])
            rval <- getItemFromBuf(rval, "percentgood", i=i, type=type, debug=debug)
        if (configuration0[15])
            rval <- getItemFromBuf(rval, "stdDev", i=i, type=type, debug=debug)
        oceDebug(debug, "} # vector-read for type=", type, "\n")
        rval
    }                                  # readBurstAltimeterRaw

    # See Nortek 2022 (draft preview mid-Aug 2022) section 2.4
    readEchosounderRaw <- function(id, debug=getOption("oceDebug")) # uses global 'd'
    {
        type <- gsub(".*=", "", ad2cpCodeToName(id))
        oceDebug(debug, "readEchosounderRaw(id=0x", id, ") # i.e. type=", type, "\n")
        look <- which(d$id == id)
        oceDebug(debug, vectorShow(look))
        lookIndex <- d$index[look]
        oceDebug(debug, vectorShow(lookIndex))

        # FIXME: need nav here, unless we store headerLength in d.  And I
        # *really* think we want to store that, because it keeps coming
        # up.  I think first nav is the text, though.  I think
        # below works...
        #> Browse[2]> d$buf[nav$start[which(d$id==id)[1]+1]+seq(1,11)]
        #> [1] a5 0c 23 10 a0 3e 00 00 92 24 86
        #
        # nolint start object_useage_linter
        rval <- list()
        # nolint end object_useage_linter
        offsetOfData <- as.integer(d$buf[d$index[look[1]] + 2L])
        #oceDebug(debug, vectorShow(offsetOfData, showNewline=FALSE),
        #    " (expect 240 for local_data/ad2cp/ad2cp_01.ad2cp)\n")
        # nolint start object_useage_linter
        serialNumber <- readBin(d$buf[17+0:3+lookIndex[1]], "integer", size=4L)
        # nolint end object_useage_linter
        #oceDebug(debug, vectorShow(serialNumber, showNewline=FALSE),
        #    " (expect 101135 for local_data/ad2cp/ad2cp_01.ad2cp)\n")
        numberOfSamples <- readBin(buf[21+0:3+lookIndex[1]], "integer", size=4L)
        #oceDebug(debug, vectorShow(numberOfSamples, showNewline=FALSE),
        #    " (expect 1974 for local_data/ad2cp/ad2cp_01.ad2cp)\n")
        # startSampleIndex is the echosounderRaw index at which
        # distance from sensor equals blanking distance.
        startSampleIndex <- readBin(buf[25+0:3+lookIndex[1]], "integer", size=4L)
        samplingRate <- readBin(buf[29+0:3+lookIndex[1]], "numeric", size=4L, endian="little")
        iv <- gappyIndex(lookIndex, offsetOfData+1L, 2L*4L*numberOfSamples)
        NP <- length(lookIndex)
        # Extract in simple steps to enable checking.  The format is inferred
        # from an email thread in and around 2022-08-23, in lieu of up-to-date
        # Nortek documents at that time.
        #. message(vectorShow(NP))
        #. message(vectorShow(iv))
        #. tmp <- readBin(d$buf[iv], "numeric", size=4L, endian="little",
        #.    n=2L*NP*numberOfSamples)
        tmp <- readBin(d$buf[iv], "integer", size=4L, endian="little", n=2L*NP*numberOfSamples)
        #. message(vectorShow(tmp))
        tmp <- as.numeric(tmp) / 2^31
        #. message(vectorShow(tmp))
        ntmp <- length(tmp)
        odd <- seq(1L, ntmp, by=2)
        even <- seq(2L, ntmp, by=2)
        real <- tmp[odd]
        imaginary <- tmp[even]
        samples <- t(matrix(complex(real=real, imaginary=imaginary), byrow=FALSE, ncol=NP))
        # FIXME: add distance,time as for echosounder
        # The below shows that we *cannot* use the cellSize (it is zero for a
        # test file).
        #. Browse[1]> cellSize
        #.  [1] 10.00 10.00 10.00 10.00  0.00  0.75 10.00 10.00 10.00
        #. [10]  0.00  0.75
        #. Browse[1]> filename
        #. [1] "/Users/kelley/git/oce/tests/testthat/local_data/ad2cp/ad2cp_01.ad2cp"
        #. message("  ", vectorShow(look[1]))
        #. message("  ", vectorShow(blankingDistance[look[1]]))
        #. message("  ", vectorShow(cellSize[look[1]]))
        iv <- gappyIndex(lookIndex, 3, 1)
        year <- as.integer(buf[iv]) + 1900
        month <- as.integer(buf[iv+1]) + 1
        day <- as.integer(buf[iv+2])
        hour <- as.integer(buf[iv+3])
        min <- as.integer(buf[iv+4])
        sec <- as.integer(buf[iv+5])
        hsec <- as.integer(buf[iv+6])
        time <- ISOdatetime(year, month, day, hour, min, sec+0.01*hsec, tz="UTC")
        list(time=time,
            numberOfSamples=numberOfSamples,
            samplingRate=samplingRate,
            startSampleIndex=startSampleIndex,
            samples=samples)
    }                                  # readEchosounderRaw

    # This is intended to handle burst, average, altimeter, ... records:
    # anything but bottom-track.
    readProfile <- function(id, debug=getOption("oceDebug")) # uses global 'd' and 'configuration'
    {
        type <- gsub(".*=", "", ad2cpCodeToName(id))
        oceDebug(debug, "readProfile(id=0x", id, ") # i.e. type=", type, "\n")
        # str(d)
        #    List of 4
        #    $ buf   : raw [1:305988694] a5 0a a0 10 ...
        #    $ index : int [1:99] 5530 6704 9254 10428 11602 12776 13950 15124 16298 17472 ...
        #    $ length: int [1:99] 1164 2540 1164 1164 1164 1164 1164 1164 1164 1164 ...
        #    $ id    : int [1:99] 21 22 21 21 21 21 21 21 21 21 ...
        look <- which(d$id == id)
        # nolint start object_useage_linter
        lookIndex <- d$index[look]
        # nolint end object_useage_linter
        #message("burstOrAverage: offsetOfData=", vectorShow(as.integer(d$buf[d$index[look[1]] + 2L])))
        oceDebug(debug, vectorShow(look))
        configuration0 <- configuration[look[1], ]
        velocityIncluded <- configuration0[6]
        amplitudeIncluded <- configuration0[7]
        correlationIncluded <- configuration0[8]
        altimeterIncluded <- configuration0[9]
        # nolint start object_useage_linter
        altimeterRawIncluded <- configuration0[10]
        # nolint end object_useage_linter
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
            magnetometer=magnetometer[look, ],
            accelerometer=accelerometer[look, ],
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
        oceDebug(debug, "configuration0=", paste(ifelse(configuration0, "T", "F"), collapse=", "), "\n")
        if (configuration0[6])
            rval <- getItemFromBuf(rval, "v", i=i, type=type, debug=debug)
        if (configuration0[7])
            rval <- getItemFromBuf(rval, "a", i=i, type=type, debug=debug)
        if (configuration0[8])
            rval <- getItemFromBuf(rval, "q", i=i, type=type, debug=debug)
        if (configuration0[9])
            rval <- getItemFromBuf(rval, "altimeter", i=i, type=type, debug=debug)
        if (configuration0[11])
            rval <- getItemFromBuf(rval, "AST", i=i, type=type, debug=debug)
        if (configuration0[10])
            rval <- getItemFromBuf(rval, "altimeterRaw", i=i, type=type, debug=debug)
        if (configuration0[12])
            rval <- getItemFromBuf(rval, "echosounder", i=i, type=type, debug=debug)
        if (configuration0[13])
            rval <- getItemFromBuf(rval, "AHRS", i=i, type=type, debug=debug)
        if (configuration0[14])
            rval <- getItemFromBuf(rval, "percentgood", i=i, type=type, debug=debug)
        if (configuration0[15])
            rval <- getItemFromBuf(rval, "stdDev", i=i, type=type, debug=debug)
        oceDebug(debug, "} # vector-read for type=", type, "\n")
        rval
    }                                  # readProfile

    # Nortek (2022 page 93 ) "6.7 _DF20BottomTrack"
    readTrack <- function(id, debug=getOption("oceDebug")) # uses global 'd' and 'configuration'
    {
        # id will be 0x17 for bottomTrack
        type <- gsub(".*=", "", ad2cpCodeToName(id))
        oceDebug(debug, "readTrack(id=0x", id, ") # i.e. type=", type, "\n")
        look <- which(d$id == id)
        lookIndex <- d$index[look]
        offsetOfData <- as.integer(d$buf[d$index[look[1]] + 2L])
        oceDebug(debug, "bottom-track (is this 79+1?)", vectorShow(offsetOfData))
        oceDebug(debug, vectorShow(lookIndex))
        configuration0 <- configuration[look[1], ]
        velocityIncluded <- configuration0[6]
        amplitudeIncluded <- configuration0[7]
        correlationIncluded <- configuration0[8]
        altimeterIncluded <- configuration0[9]
        # nolint start object_useage_linter
        altimeterRawIncluded <- configuration0[10]
        # nolint end object_useage_linter
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
            magnetometer=magnetometer[look, ],
            accelerometer=accelerometer[look, ],
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
        oceDebug(debug, "configuration0=", paste(ifelse(configuration0, "T", "F"), collapse=", "), "\n")
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

    # nolint start object_useage_linter
    readInterleavedBurst <- function(id, debug=getOption("oceDebug")) # uses global 'd' and 'configuration'
    # nolint end object_useage_linter
    {
        type <- gsub(".*=", "", ad2cpCodeToName(id))
        oceDebug(debug, "readInterleavedBurst(id=0x", id, ") # i.e. type=", type, "\n")
        look <- which(d$id == id)
        lookIndex <- d$index[look]
        oceDebug(debug, vectorShow(lookIndex))
        configuration0 <- configuration[look[1], ]
        velocityIncluded <- configuration0[6]
        amplitudeIncluded <- configuration0[7]
        correlationIncluded <- configuration0[8]
        altimeterIncluded <- configuration0[9]
        # nolint start object_useage_linter
        altimeterRawIncluded <- configuration0[10]
        # nolint end object_useage_linter
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
            magnetometer=magnetometer[look, ],
            accelerometer=accelerometer[look, ],
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
        oceDebug(debug, "configuration0=", paste(ifelse(configuration0, "T", "F"), collapse=", "), "\n")
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
        type <- gsub(".*=", "", ad2cpCodeToName(id))
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
        offsetOfData <- commonData$offsetOfData[look]
        oceDebug(debug, vectorShow(offsetOfData))
        # According to Nortek (2022, Section 6.4, page 88), the only
        # configuration flag is whether we have echosounder data, which seems a
        # bit odd because the whole point of an echosounder ID must be that we
        # have echosounder data.  Anyway, we will use that.
        configuration0 <- configuration[look[1], ]
        echosounderIncluded0 <- configuration0[12]
        oceDebug(debug, vectorShow(echosounderIncluded0))

        rval <- list(
            configuration=configuration,
            #numberOfBeams=nbeams[look[1]],
            numberOfCells=readBin(d$buf[lookIndex[1] + 31:32], "integer", size=2L, n=1, endian="little", signed=FALSE),
            #originalCoordinate=coordinateSystem[look[1]],
            #oceCoordinate=coordinateSystem[look[1]],
            # Nortek (2022 Table 6.4 page 87) does not state a factor on
            # frequency, but a sample file states 500 in the header lines, and
            # the number I read with the next line is 5000, so I assume this a
            # guess worth making.
            frequency=0.1*readBin(d$buf[lookIndex[1]+53:54], "integer", size=2L, n=1L, endian="little", signed=FALSE),
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
            magnetometer=magnetometer[look, ],
            accelerometer=accelerometer[look, ],
            datasetDescription=datasetDescription[look],
            temperatureMagnetometer=temperatureMagnetometer[look],
            temperatureRTC=temperatureRTC[look],
            transmitEnergy=transmitEnergy[look],
            powerLevel=powerLevel[look])
        rval$distance <- rval$blankingDistance + seq(0, by=rval$cellSize, length.out=rval$numberOfCells)
        i <- d$index[look]             # pointers to "echosounder" chunks in buf
        oceDebug(debug, "in readEchosounder: ", vectorShow(i))
        i0v <<- 1L + offsetOfData[1]   # pointer to data (incremented by getItemFromBuf() later).
        oceDebug(debug, "in readEchosounder: ", vectorShow(i0v))
        NP <- length(i)                # number of profiles of this type
        oceDebug(debug, "in readEchosounder: ", vectorShow(NP))
        oceDebug(debug, "configuration0=", paste(ifelse(configuration0, "T", "F"), collapse=", "), "\n")
        if (configuration0[12])         # read echosounder, if included
            rval <- getItemFromBuf(rval, "echosounder", i=i, type=type, debug=debug)
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
    if (0x15 == dataType) {            # 0x15=burst
        if (length(p$burst) < 1L)
            stop("no dataType=", dataTypeOrig, " (burst) in file")
        data <- readProfile(id=as.raw(dataType), debug=debug)
        oceDebug(debug, "dataType=", dataType, "(burst): move some things from data to metadata\n")
        for (name in c("blankingDistance", "cellSize", "configuration", "datasetDescription",
                "frequency", "numberOfBeams", "numberOfCells", "oceCoordinate",
                "orientation", "originalCoordinate")) {
            if (name %in% names(data)) {
                oceDebug(debug, "  transferring ", name, " from data to metadata\n")
                res@metadata[name] <- data[name]
            } else {
                oceDebug(debug, "  deleting ", name, " from data, without moving to metadata\n")
            }
            data[name] <- NULL
        }
    }
    # Nortek (2017 p 48) "6.1.2 Burst/Average Data Record Definition (DF3)"
    if (0x16 == dataType) {            # 0x16=average
        if (length(p$average) < 1L)
            stop("no dataType=", dataTypeOrig, " (average) in file")
        data <- readProfile(id=as.raw(dataType), debug=debug)
        oceDebug(debug, "dataType=", dataType, "(average): move some things from data to metadata\n")
        for (name in c("blankingDistance", "cellSize", "configuration", "datasetDescription",
                "frequency", "numberOfBeams", "numberOfCells", "oceCoordinate",
                "orientation", "originalCoordinate")) {
            if (name %in% names(data)) {
                oceDebug(debug, "  transferring ", name, " from data to metadata\n")
                res@metadata[name] <- data[name]
            } else {
                oceDebug(debug, "  deleting ", name, " from data, without moving to metadata\n")
            }
            data[name] <- NULL
        }
    }
    #<FIXME> # Nortek (2017 p60) "6.1.3 Bottom Track Data Record Definition (DF20)"
    #<FIXME> if ("bottomTrack" %in% dataType && length(p$bottomTrack) > 0) # 0x17
    #<FIXME>     data$bottomTrack <- readTrack(id=as.raw(0x17), debug=debug)
    if (0x17 == dataType) {            # 0x17=bottomTrack
        if (length(p$bottomTrack) < 1L)
            stop("no dataType=", as.raw(dataTypeOrig), " (bottomTrack) in file")
        data <- readTrack(id=dataType, debug=debug)
        oceDebug(debug, "dataType=", as.raw(dataType), "(bottomTrack): move some things from data to metadata\n")
        for (name in c("blankingDistance", "cellSize", "configuration", "datasetDescription",
                "frequency", "numberOfBeams", "numberOfCells", "oceCoordinate",
                "orientation", "originalCoordinate")) {
            if (name %in% names(data)) {
                oceDebug(debug, "  transferring ", name, " from data to metadata\n")
                res@metadata[name] <- data[name]
            } else {
                oceDebug(debug, "  deleting ", name, " from data, without moving to metadata\n")
            }
            data[name] <- NULL
        }
    }
    #<FIXME> if ("interleavedBurst" %in% which && length(p$interleavedBurst) > 0) # 0x18
    #<FIXME>     data$interleavedBurst <- readProfile(id=as.raw(0x18), debug=debug)
    if (0x18 == dataType) {            # 0x18=interleavedBurst
        if (length(p$interleavedBurst) < 1L)
            stop("no dataType=", dataTypeOrig, " (interleavedBurst) in file")
        data <- readProfile(id=as.raw(dataType), debug=debug)
        oceDebug(debug, "dataType=", dataType, "(interleavedBurst): move some things from data to metadata\n")
        for (name in c("blankingDistance", "cellSize", "configuration", "datasetDescription",
                "frequency", "numberOfBeams", "numberOfCells", "oceCoordinate",
                "orientation", "originalCoordinate")) {
            if (name %in% names(data)) {
                oceDebug(debug, "  transferring ", name, " from data to metadata\n")
                res@metadata[name] <- data[name]
            } else {
                oceDebug(debug, "  deleting ", name, " from data, without moving to metadata\n")
            }
            data[name] <- NULL
        }
    }
    #<FIXME> if ("burstAltimeterRaw" %in% which && length(p$burstAltimeterRaw) > 0L) # 0x1a
    #<FIXME>     data$burstAltimeterRaw <- readBurstAltimeterRaw(id=as.raw(0x1a), debug=debug-1L)
    if (0x1a == dataType) {            # 0x1a=burstAltimeterRaw
        if (length(p$burstAltimeterRaw) < 1L)
            stop("no dataType=", as.raw(dataTypeOrig), " (burstAltimeterRaw) in file")
        data <- readBurstAltimeterRaw(id=dataType, debug=debug)
        oceDebug(debug, "dataType=", as.raw(dataType), "(burstAltimeterRaw): move some things from data to metadata\n")
        for (name in c("blankingDistance", "cellSize", "configuration", "datasetDescription",
                "frequency", "numberOfBeams", "numberOfCells", "oceCoordinate",
                "orientation", "originalCoordinate")) {
            if (name %in% names(data)) {
                oceDebug(debug, "  transferring ", name, " from data to metadata\n")
                res@metadata[name] <- data[name]
            } else {
                oceDebug(debug, "  deleting ", name, " from data, without moving to metadata\n")
            }
            data[name] <- NULL
        }
    }
    #<FIXME> if ("DVLBottomTrack" %in% which && length(p$DVLBottomTrack) > 0) # 0x1b
    #<FIXME>     data$DVLBottomTrack <- readTrack(id=as.raw(0x1b), debug=debug-1L)
    if (0x1b == dataType) {            # 0x1b=DVLBottomTrack
        if (length(p$DVLBottomTrack) < 1L)
            stop("no dataType=", as.raw(dataTypeOrig), " (DVLBottomTrack) in file")
        data <- readTrack(id=dataType, debug=debug)
        oceDebug(debug, "dataType=", as.raw(dataType), "(DVLBottomTrack): move some things from data to metadata\n")
        for (name in c("blankingDistance", "cellSize", "configuration", "datasetDescription",
                "frequency", "numberOfBeams", "numberOfCells", "oceCoordinate",
                "orientation", "originalCoordinate")) {
            if (name %in% names(data)) {
                oceDebug(debug, "  transferring ", name, " from data to metadata\n")
                res@metadata[name] <- data[name]
            } else {
                oceDebug(debug, "  deleting ", name, " from data, without moving to metadata\n")
            }
            data[name] <- NULL
        }
    }
    #<FIXME> if ("echosounder" %in% which && length(p$echosounder) > 0) # 0x1c
    #<FIXME>     data$echosounder <- readEchosounder(id=as.raw(0x1c), debug=debug)
    if (0x1c == dataType) {            # 0x1c=echosounder
        if (length(p$echosounder) < 1L)
            stop("no dataType=", dataTypeOrig, " (echosounder) in file")
        data <- readEchosounder(id=dataType, debug=debug)
        oceDebug(debug, "FIXME: move some (echosounder) things from data to metadata\n")
        for (name in c("blankingDistance", "cellSize", "configuration", "datasetDescription",
                "frequency", "numberOfBeams", "numberOfCells", "orientation")) { # not same as above
            if (name %in% names(data)) {
                oceDebug(debug, "moving ", name, " from data to metadata\n")
                res@metadata[name] <- data[name]
            } else {
                oceDebug(debug, "  deleting ", name, " from data, without moving to metadata\n")
            }
            data[name] <- NULL
        }
    }
    #<FIXME> if ("DVLWaterTrack" %in% which && length(p$DVLWaterTrack) > 0) # 0x1d
    #<FIXME>     data$DVLWaterTrack <- readTrack(id=as.raw(0x1d), debug=debug)
    if (0x1d == dataType) {            # 0x1d=DVLWaterTrack
        if (length(p$echosounder) < 1L)
            stop("no dataType=", dataTypeOrig, " (DVLWaterTrack) in file")
        data <- readProfile(id=dataType, debug=debug)
        message("FIXME: move some (DVLWaterTrack) things from data to metadata")
        for (name in c("blankingDistance", "cellSize", "configuration", "datasetDescription",
                "frequency", "numberOfBeams", "numberOfCells", "orientation")) { # not same as above
            if (name %in% names(data)) {
                oceDebug(debug, "moving ", name, " from data to metadata\n")
                res@metadata[name] <- data[name]
            } else {
                oceDebug(debug, "  deleting ", name, " from data, without moving to metadata\n")
            }
            data[name] <- NULL
        }
    }
    #<FIXME> if ("altimeter" %in% which && length(p$altimeter) > 0) # 0x1e
    #<FIXME>     data$altimeter <- readProfile(id=as.raw(0x1e), debug=debug)
    if (0x1e == dataType) {            # 0x1e=altimeter
        if (length(p$echosounder) < 1L)
            stop("no dataType=", dataTypeOrig, " (altimeter) in file")
        data <- readProfile(id=dataType, debug=debug)
        message("FIXME: move some (altimeter) things from data to metadata")
        for (name in c("blankingDistance", "cellSize", "configuration", "datasetDescription",
                "frequency", "numberOfBeams", "numberOfCells", "orientation")) { # not same as above
            if (name %in% names(data)) {
                oceDebug(debug, "moving ", name, " from data to metadata\n")
                res@metadata[name] <- data[name]
            } else {
                oceDebug(debug, "  deleting ", name, " from data, without moving to metadata\n")
            }
            data[name] <- NULL
        }
    }
    #<FIXME> if ("averageAltimeter" %in% which && length(p$averageAltimeter) > 0) # 0x1f
    #<FIXME>     data$averageAltimeter <- readProfile(id=as.raw(0x1f), debug=debug)
    if (0x1f == dataType) {            # 0x1f=averageAltimeter
        if (length(p$echosounder) < 1L)
            stop("no dataType=", dataTypeOrig, " (averageAltimeter) in file")
        data <- readProfile(id=dataType, debug=debug)
        message("FIXME: move some (averageAltimeter) things from data to metadata")
        for (name in c("blankingDistance", "cellSize", "configuration", "datasetDescription",
                "frequency", "numberOfBeams", "numberOfCells", "orientation")) { # not same as above
            if (name %in% names(data)) {
                oceDebug(debug, "moving ", name, " from data to metadata\n")
                res@metadata[name] <- data[name]
            } else {
                oceDebug(debug, "  deleting ", name, " from data, without moving to metadata\n")
            }
            data[name] <- NULL
        }
    }
    #<FIXME> if ("echosounderRaw" %in% which && length(p$echosounderRaw) > 0) # 0x23
    #<FIXME>     data$echosounderRaw <- readEchosounderRaw(id=as.raw(0x23), debug=debug)
    if (0x23 == dataType) {            # 0x23=echosounderRaw
        if (length(p$echosounder) < 1L)
            stop("no dataType=", dataTypeOrig, " (echosounderRaw) in file")
        data <- readEchosounderRaw(id=dataType, debug=debug)
        # 2022-08-26: I asked Nortek how to compute distance for echosounderRaw, and
        # the answer involves the blankingDistance.  But, in my sample file at
        # tests/testthat/local_data/ad2cp/ad2cp_01.ad2cp, the blankingDistance for
        # echosounderRaw is 0, and so I'm guessing (pending more information from
        # Nortek) that the idea is to use the blankingDistance in the (now possibly updated)
        # ... honestly, this is a mess and I am not 100% sure what to do, lacking
        # confidence until Nortek updates their documentation.  One thing, though:
        # the code below is based on the old model for ad2cp object structure, in
        # which we stored both 'echosounder' and 'echosounderRaw', but in the new
        # model we do not do that.  I am simply skipping this for now 2022-10-08
        # but printing a message.
        #
        # Compute cellSize using a formula inferred from an email by
        # Nortek's Ragnar Ekker on 2022-09-01.
        #
        # 1. Should we use the integer `startSampleIndex` that is in the
        # file, or should we compute it using the formula provided by
        # Ragnar?  The former is an integer value that is 16 in a sample
        # file, and if that's typical then rounding might be expected to
        # give about 3% error in the results for `cellSize` and thus
        # `distance`.

        # 2. What `soundSpeed` should be used?  It varies from profile to
        # profile. But, perhaps we should use a constant value, if that's
        # what was used in some computations that led to the data creation.
        # The graph above uses the integer value. If the calculated
        # `startSampleIndex` were used instead, the peak would shift from
        # 282m to 270m=
        # `r round(with(d@data$echosounderRaw,cellSize2*282/cellSize))` m.
        XMIT1 <- 1e-3*ad2cpHeaderValue(header, "GETECHO", "XMIT1")
        BD <- ad2cpHeaderValue(header, "GETECHO", "BD")
        if (is.null(XMIT1) || is.null(BD)) {
            warning("cannot infer distance for echosounderRaw record; set to 1, 2, which is almost certainly very wrong")
            data$distance <- seq_len(data$numberOfSamples)
        } else {
            L <- 0.5 * XMIT1 * soundSpeed[1] + BD
            samplingRate <- data$samplingRate
            startSampleIndex <- (XMIT1 + 2*BD/soundSpeed[1]) * samplingRate
            # FIXME: which cellSize to use?  I think Ragnar suggested computing
            # it, rather than using the rounded value in the dataset.
            data$cellSize <- L / startSampleIndex
            # data$cellSize <- L / data$startSampleIndex
            data$distance <- seq(0, by=data$cellSize, length.out=data$numberOfSamples)
            oceDebug(debug, "read.adp.ad2cp() : computing echosounderRaw$distance based ",
                " on my interpretation of an email sent by RE/Nortek on 2022-09-01")
            # the above contradicts an email sent by EB/Nortek on 2022-08-28 but
            # I am told that this earlier one was erroneous.
        }
        oceDebug(debug, "move some (echosounderRaw) things from data to metadata\n")
        for (name in c("blankingDistance", "cellSize", "configuration", "datasetDescription",
                "frequency", "numberOfBeams", "numberOfCells", "numberOfSamples", "orientation",
                "samplingRate", "startSampleIndex")) { # not same as above
            if (name %in% names(data)) {
                oceDebug(debug, "moving ", name, " from data to metadata\n")
                res@metadata[name] <- data[name]
            } else {
                oceDebug(debug, "  deleting ", name, " from data, without moving to metadata\n")
            }
            data[name] <- NULL
        }
    }

    # Use header as the final word, if it contradicts what we inferred above.
    if (!is.null(header)) {
        if (0x1c == dataType) {        # 0x1c=echosounder
            # BOOKMARK-blankingDistance-2 (see also BOOKMARK-blankingDistance-1, above)
            BD <- ad2cpHeaderValue(header, "GETECHO", "BD")
            if (res@metadata$blankingDistance != BD) {
                warning("In read.adp.ad2cp() : inferred echosounder$blankingDistance (", res@metadata$blankingDistance,
                    "m) does not match the header GETECHO value (", BD,
                    "m); the latter value was used\n", call.=FALSE)
                res@metadata$blankingDistance <- BD
            }
        }
    }
    # FIXME: I bet some other types should not be getting distance defined.
    if (!(dataType %in% c("bottomTrack")))
        data$distance <- res@metadata$blankingDistance + seq(1, by=res@metadata$cellSize, length.out=res@metadata$numberOfCells)
    # 2022-08-29 BOOKMARK-blankingDistance-03
    # I am informed by Nortek that the blankingDistance is always 1e-3 for
    # echosounderRaw data.  So, we set that (and issue a warning so that users
    # will know about this alteration) and then compute a distance vector for
    # possible later use.
    #| if (2L == sum(c("echosounder", "echosounderRaw") %in% names(data))) {
    #|     if ("blankingDistance" %in% names(data$echosounder) && "startSampleIndex" %in% names(data$echosounderRaw)) {
    #|         message("computing echosounderRaw$distance using formula from private communication with Nortek dated 2022-08-28")
    #|         data$echosounderRaw$cellsize <- data$echosounder$blankingDistance / data$echosounderRaw$startSampleIndex
    #|         data$echosounderRaw$distance <- seq(0, by=
data#| $echosounderRaw$cellsize, length.out=data$echosounderRaw$numberOfSamples)
    #|     }
    #| }

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
    #- Warn if we had to guess the type
    #-if (!typeGiven) {
    #-    type <- "Signature1000"
    #-    warning("defaulting 'type' to '", type, "', since no header was found in the file, and the 'type' argument was not provided")
    #-}
    res@metadata$type <- type
    res@metadata$declination <- ad2cpHeaderValue(x=header, key="GETUSER", item="DECL", default=NA)
    res@metadata$frequency <- ad2cpHeaderValue(x=header, key="BEAMCFGLIST,BEAM=1", item="FREQ", default=NA)
    res@metadata$beamAngle <- switch(type, "Signature1000"=25, "Signature500"=25, "Signature250"=20)
    # Note: metadata$transformationMatrix is not defined; we make "[[" compute
    # that, because the user may realize that x@metadata$beamAngle is wrong,
    # and want to correct it.  This makes ad2cp different from other adp
    # types.  Also, we must remove the overall coordinate (created by
    # initializer) since it has no meaning here.
    # res@metadata$oceCoordinate <- NULL
    # Remove some metadata that make don't sense for the dataType
    if (dataType %in% c(0x1c, 0x1e, 0x23)) {
        # 0x1c=echosounder 0x1e=altimeter 0x23=echosounderRaw
        res@metadata$units$v <- NULL
        res@metadata$oceCoordinate <- NULL
        res@metadata$orientation <- NULL
    }
    res@metadata$dataType <- dataType
    # Insert data
    res@data <- data
    # Insert processingLog
    processingLog <- with(originalParameters,
        paste("read.adp.ad2cp(file=\"", filename, "\"",
            ", from=", from, ", to=", to, ", by=", by,
            ", dataSet=", dataSet, ", dataType=", dataType, ", tz=", tz,
            ", longitude=", longitude, ", latitude=", latitude, ")", sep=""))
    res@processingLog <- processingLogItem(processingLog)
    oceDebug(debug, "} # read.adp.ad2cp()\n", unindent=1, style="bold")
    res
}
