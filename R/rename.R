# vim:textwidth=80:expandtab:shiftwidth=4:softtabstop=4

# An internal function, used by rename(); not exported.
# @author Dan Kelley
renameInternal <- function(names, dictionary = "ioos.csv", debug = 0) {
    debug <- min(3L, max(debug, 0L))
    if (is.character(dictionary) && grepl(".csv(.gz){0,1}$", dictionary)) {
        vocab <- read.csv(dictionary,
            header = FALSE,
            col.names = c("original", "oce", "units", "scale")
        )
        oceDebug(debug, "  \"", dictionary, "\" has ", nrow(vocab), " entries\n", sep = "")
    } else if (is.data.frame(dictionary)) {
        vocab <- dictionary
        names(vocab) <- c("originalName", "oceName", "units", "scale")
        oceDebug(debug, "data-frame dictionary has ", nrow(vocab), "entries\n")
    }
    # The system is that # can represent a digit
    vocab$pattern <- paste0("^", gsub("#", "[0-9]", vocab$original), "$")
    if (debug > 1) {
        cat("next is vocabulary, after adding regexp pattern\n")
        print(vocab)
    }
    rval <- data.frame(originalName = NULL, oceName = NULL, unit = NULL, scale = NULL)
    for (i in seq_along(names)) {
        name <- names[i]
        # Look up
        # oceDebug(debug, "name=\"", name, "\"\n")
        w <- which(name == vocab$original)
        wlen <- length(w)
        if (wlen == 1) {
            oceDebug(debug, "  * \"", name, "\" is an exact match for vocabulary at index ", w, "\n")
        }
        if (length(w) == 0L) {
            # oceDebug(debug, "  no exact match, so searching patterns for a match\n")
            w <- which(sapply(vocab$pattern, \(pattern) grepl(pattern, name)))
            # oceDebug(debug, vectorShow(w))
            if (length(w) > 1L) {
                warning("multiple name match on \"", name, "\"; using first", sep = "")
                w <- w[1]
            }
            if (length(w) == 1L) {
                oceDebug(debug, "  * \"", name, "\" is a pattern match to vocabulary at index ", w, "\n")
            }
        }
        if (length(w) == 0L) {
            oceDebug(debug, "  * \"", name, "\" is not in vocabulary; so not renamed\n", sep = "")
            rval <- rbind(rval, c(originalName = name, oceName = name, unit = "", scale = ""))
        } else {
            if (length(w) == 1L) {
                oceDebug(debug, "  \"", name, "\" matches vocabulary at index ", w, "\n", sep = "")
            } else {
                oceDebug(debug, "  \"", name, "\" matches vocabulary at indices ",
                         paste(w, collapse = ", "), "; using the first of these\n", sep = "")
                warning("  \"", name, "\" matches vocabulary at indices ",
                         paste(w, collapse = ", "), "; using the first of these\n")
                w <- w[1]
            }
            oceDebug(debug, vectorShow(vocab[w, ]))
            newrow <- c(
                originalName = name,
                oceName = vocab$oce[w], unit = vocab$unit[w], scale = vocab$scale[w]
            )
            newrow <- c(name, vocab$oce[w], vocab$unit[w], vocab$scale[w])
            rval <- rbind(rval, newrow)
        }
    }
    colnames(rval) <- c("originalName", "oceName", "units", "scale")
    # rename variables so x,x becomes x,x2 etc, but skip over flags
    isFlag <- grepl("Flag$", rval$oceName)
    oceDebug(debug, "handling duplicated names\n")
    rval$oceName[!isFlag] <- unduplicateNames(rval$oceName[!isFlag])
    rval
}

#' Rename variables according to a specified dictionary
#'
#' There are many conventions for naming oceanographic variables, and this
#' function provides a way to map names in data files to names to be used in an
#' object created from those files.
#'
#' The dictionary format, whether read from a built-in CSV file, or from a
#' user-supplied CSV file, or as a data frame, contains four character-valued
#' columns, as follows.
#'
#' 1. The original name of a variable in the `data` slot of `x`. This is used in
#'    matching such names against targets. Matches may be in the form of
#'    equality, or [regexp] match. In the latter case, a `#` character may be
#'    used as an abbreviation for a digit.  Note that `^` is inserted at the
#'    start of the value, and `$` at the end, before searching for a match with
#'    [grep()].
#'
#' 2. The desired oce-convention name to be used for a match. Many files will
#'    yield duplicates, e.g. for multiple temperature sensors, so
#'    [unduplicateNames()] is called after all names are processed, to avoid
#'    problems.
#'
#' 3. The unit for the column, typically in a format handled by [expression()].
#'    Note that this value is ignored if the object already holds stated units
#'    for the quantity in question.
#'
#' 4. The scale for the column (again, only used if the object does not already
#'    hold a scale).
#'
#' For examples, see the built-in dictionaries, which are stored in
#' files with locations that are revealed with the following.
#'
#'```
#' readLines(system.file("extdata", "dictionary_sbe.csv", package = "oce"))
#' readLines(system.file("extdata", "dictionary_ioos.csv", package = "oce"))
#'```
#'
#' For example, the entry
#'
#' ```
#' PSALST##,salinity,,PSS-78
#' ```
#'
#' specifies that a variable named `"PSALT"` followed by 2 digits
#' is to be renamed as `"salinity"`, that the unit (if not
#' already defined within `x`) is to be blank, and that
#' the scale (again, if not already defined) is to be `"PSS-78"`.
#'
#' @param x an [oce-class] object.
#'
#' @param dictionary either a string or a data frame.  If a string, then it is
#' either the name of a built-in vocabulary (e.g. `ioos` or `sbe`; see
#' \sQuote{Details}) or the name of a CSV file that defines a dictionary. If it
#' is a data frame, the it must have four unnamed columns containing information as
#' described in \sQuote{Details}.
#'
#' @template debugTemplate
#'
#' @examples
#' library(oce)
#' # Example 1: made-up data
#' d <- new("oce")
#' d <- oceSetData(d, "S", c(30, 31))
#' d <- oceSetData(d, "T", c(10, 11))
#' dictText <- "S,salinity,,
#' T,temperature,degree*C,ITS-90"
#' dictionary <- read.csv(text = dictText, header = FALSE)
#' rename(d, dictionary)
#' #
#' # Example 2: a CIOOS NetCDF file. Note that this file
#' # is downloaded and removed at the end; in practice,
#' # it is likely that the file might be retained locally.
#' if (requireNamespace("curl")) {
#'     file <- tempfile(fileext = ".nc") # removed later
#'     server <- "https://cioosatlantic.ca/erddap/files"
#'     program <- "bio_atlantic_zone_monitoring_program_ctd"
#'     subprogram <- "Bedford%20Basin%20Monitoring%20Program"
#'     year <- 2023
#'     cast <- 1
#'     url <- sprintf(
#'         "%s/%s/%s/%s/CTD_BCD%s667_%03d_1_DN.ODF.nc",
#'         server, program, subprogram, year, year, cast
#'     )
#'     curl::curl_download(url, file)
#'     d <- read.netcdf(file)
#'     summary(d)
#'     dd <- rename(d, "ioos")
#'     summary(dd)
#'     unlink(file)
#' }
#'
#' @section History and Plans:
#' This function was written in late September, 2024. It is likely
#' to evolve through the remaining months of 2024, after real-world
#' testing by the developers.
#'
#' @author Dan Kelley
rename <- function(x, dictionary = "ioos.csv", debug = 0) {
    oceDebug(debug, "rename(..., dictionary=",
        if (is.data.frame(dictionary)) {
            "[data frame]"
        } else {
            paste0("\"", dictionary, "\"")
        },
        "\") START\n",
        sep = "", unindent = 1
    )
    if (!inherits(x, "oce")) stop("x is not an oce-class object")
    # if (!is.character(dictionary)) {
    #    stop("FIXME: code to allow dictionary to be a data frame")
    # }
    if (is.character(dictionary) && !grepl(".csv$", dictionary)) {
        dictionary <- system.file("extdata", paste0("dictionary_", dictionary, ".csv"), package = "oce")
    }
    rval <- x
    originalNames <- names(x[["data"]])
    oceDebug(debug, "finding vocabulary translations\n")
    R <- renameInternal(originalNames, dictionary = dictionary, debug = debug)
    oceDebug(debug, "setting up @metadata$dataNamesOriginal\n")
    # set up original names
    # print(sort(names(rval@metadata)))
    # cat(str(R))
    if (is.null(rval@metadata$dataNamesOriginal)) {
        rval@metadata$dataNamesOriginal <- as.list(R$originalName)
        # print(rval@metadata$dataNamesOriginal)
    }
    # print(R$oceName)
    names(rval@metadata$dataNamesOriginal) <- R$oceName
    # print(rval@metadata$dataNamesOriginal)
    # message("DAN 2")
    # rename data
    names(rval@data) <- R$oceName
    # message("DAN 3")
    oceDebug(debug, "setting up @metadata$units\n")
    if (0L == length(x@metadata$units)) {
        # message("DAN units 1")
        rval@metadata$units <- as.list(R$units)
        # print(rval@metadata$units)
        # print(R)
        names(rval@metadata$units) <- R$oceName
    } else if (length(rval@metadata$units) == nrow(R)) {
        # message("DAN units 2")
        names(rval@metadata$units) <- sapply(names(rval@metadata$units), \(n) R$oceName[which(n == R$originalName)])
    } else {
        oceDebug(debug, "renaming units one by one\n")
        # message("DAN 1")
        w <- sapply(names(rval@metadata$units), \(u) which(u == R$originalName))
        # print(R$oceName[w])
        # message("DAN 2")
        names(rval@metadata$units) <- R$oceName[w]
        # message("DAN 3")
        # warning("cannot set up units (length mismatch) -- can this happen?") # FIXME
    }
    # message("DAN next is x@metadata$units")
    # print(x@metadata$units)
    # message("DAN next is R")
    # print(R)
    # names(rval@metadata$units) <- R$oceName
    # message("DAN next is rval@metadata$units")
    # print(rval@metadata$units)
    # Move calibrations to metadata
    oceDebug(debug, "move unit-length items in data slot to metadata slot\n")
    dataNames <- names(rval@data)
    for (name in dataNames) {
        item <- rval@data[[name]]
        if (is.character(item) && 1L == length(item)) {
            rval@metadata[[name]] <- item
            rval@data[[name]] <- NULL
        }
    }
    # Move flags to metadata
    oceDebug(debug, "handling flags\n")
    dataNames <- names(rval@data)
    if (!is.null(rval@metadata$flags)) {
        oldFlagNames <- names(rval@metadata$flags)
        oceDebug(debug, "  ", vectorShow(oldFlagNames, n = 100))
        newFlagNames <- unname(sapply(
            oldFlagNames,
            \(o) {
                w <- which(R$originalName == o)
                if (length(w) == 1) R$oceName[w] else o
            }
        ))
        oceDebug(debug, "  ", vectorShow(newFlagNames, n = 100))
        # print(R)
        names(rval@metadata$flags) <- newFlagNames
    }
    # message("next is x@metadata$flags")
    # print(x@metadata$flags)
    # print(R)
    for (name in dataNames) {
        item <- rval@data[[name]]
        if (grepl("Flag$", name)) {
            rval@metadata$flags[[gsub("Flag$", "", name)]] <- item
            rval@data[[name]] <- NULL
        }
    }
    rval@processingLog <- processingLogAppend(
        rval@processingLog,
        paste0("rename(..., ",
            if (is.data.frame(dictionary)) {
                " [data frame]"
            } else {
                paste0("\"", dictionary, "\"")
            }, ")",
            sep = "", collapse = ""
        )
    )
    oceDebug(debug, "END rename()\n", sep = "", unindent = 1)
    rval
}
