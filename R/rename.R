# vim:textwidth=80:expandtab:shiftwidth=4:softtabstop=4

renameVocabulary <- function(dictionary = "ioos.csv", debug = 0) {
    if (is.character(dictionary)) {
        if (!grepl(".csv(.gz){0,1}$", dictionary)) {
            dictionary <- system.file("extdata", paste0("dictionary_", dictionary, ".csv"), package = "oce")
        }
        vocab <- read.csv(dictionary,
            header = FALSE,
            col.names = c("originalName", "oceName", "units", "scale")
        )
    } else if (is.data.frame(dictionary)) {
        vocab <- dictionary
        names(vocab) <- c("originalName", "oceName", "units", "scale")
    }
    # Paste regexp anchors, and substitute for single-digit search
    oceDebug(debug, "dictionary ", if (is.character(dictionary)) paste0("\"", dictionary, "\" "), "has ", nrow(vocab), " entries\n", sep = "")
    vocab$pattern <- paste0("^", gsub("#", "[0-9]", vocab$original), "$")
    vocab
}

# An internal function, used by rename(); not exported.
# @author Dan Kelley
renameInternal <- function(names, dictionary = "ioos.csv", debug = 0) {
    debug <- min(3L, max(debug, 0L))
    if (debug > 1) {
        if (is.character(dictionary) && grepl(".csv(.gz){0,1}$", dictionary)) {
            vocab <- read.csv(dictionary,
                header = FALSE,
                col.names = c("originalName", "oceName", "units", "scale")
            )
            oceDebug(debug, "  \"", dictionary, "\" has ", nrow(vocab), " entries\n", sep = "")
        } else if (is.data.frame(dictionary)) {
            vocab <- dictionary
            names(vocab) <- c("originalName", "oceName", "units", "scale")
            oceDebug(debug, "data-frame dictionary has ", nrow(vocab), " entries\n")
        }
        # The system is that # can represent a digit
        vocab$pattern <- paste0("^", gsub("#", "[0-9]", vocab$original), "$")
        if (debug > 1) {
            cat("next is vocabulary, after adding regexp pattern\n")
            print(vocab)
        }
        vocabNew <- renameVocabulary(dictionary, debug)
        if (!identical(vocab, vocabNew)) {
            message("vocab (ORIGINAL)")
            print(vocab)
            message("vocab (NEW)")
            print(vocab)
            warning("internal error with renamer(): old- and new-style codes disagree (please tell author)")
        }
        vocab <- vocabNew
    }
    vocab <- renameVocabulary(dictionary, debug)
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
                    paste(w, collapse = ", "), "; using the first of these\n",
                    sep = ""
                )
                warning(
                    "  \"", name, "\" matches vocabulary at indices ",
                    paste(w, collapse = ", "), "; using the first of these\n"
                )
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
#' The built-in dictionaries are stored in locations
#'
#' ```
#' system.file("extdata", "dictionary_codas.csv", package = "oce")
#' system.file("extdata", "dictionary_ioos.csv", package = "oce")
#' system.file("extdata", "dictionary_sbe.csv", package = "oce")
#' ```
#'
#' The data for these come from References 1, 2 and 3, respectively.  The format
#' is simple, consisting of 4 columns, with no header. The column entries
#' are as follows.
#'
#' \enumerate{
#'
#' \item The first column holds a specialized regular expression for the variable
#' name as stored in the datafile.  This is conventional, except that `#` is a
#' stand-in for the regular expression `[0-9]` (that is, a single digit).
#' Formulating these expressions requires a bit of care, so it can make sense to
#' look at the `dictionary_sbe.csv` file to get some hints.
#'
#' \item The second column holds the oce name.
#'
#' \item The third column is the unit.
#'
#' \item The fourth column is the scale.
#' }
#'
#' In many cases, the third and fourth columns are empty, and even if values are
#' provided, they will be superceded by values within the data file.
#'
#' As an example, the entry
#'
#' ```
#' PSALST##,salinity,,PSS-78
#' ```
#'
#' indicates that a variable named `"PSALT"` followed by 2 digits is to be
#' renamed as `"salinity"`, that the unit (if not already defined within `x`) is
#' to be blank, and that the scale (again, if not already defined within `x`) is
#' to be `"PSS-78"`.
#'
#' @param x either an [oce-class] object, the elements of which will
#' be renamed, or NULL. In the latter case, the dictionary is returned
#' as a data frame, which can be useful for users who want to use [rbind()]
#' to append dictionary elements of their own, thus customizing the
#' action of `rename()`.
#'
#' @param dictionary either a string or a data frame.  If a string, then it is
#' either the name of a built-in vocabulary (either `"ioos"` or `"sbe"`)
#' or the name of a CSV file that defines a dictionary in a four-column
#' format as described in \sQuote{Details}. If it is a data frame, then
#' it must hold four columns that follow the same pattern as in the CSV
#' style.
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
#'     t <- try(curl::curl_download(url, file), silent = TRUE)
#'     if (!inherits(t, "try-error")) {
#'         d <- read.netcdf(file)
#'         summary(d)
#'         dd <- rename(d, "ioos")
#'         summary(dd)
#'     } else {
#'         message("Cannot connect to ", url)
#'     }
#'     unlink(file)
#' }
#'
#' @section History and Plans:
#' This function was written in late September, 2024. It is likely
#' to evolve through the remaining months of 2024, after real-world
#' testing by the developers.
#'
#' @references
#'
#' 1. CODAS naming convention
#' <https://currents.soest.hawaii.edu/docs/adcp_doc/UHDAS_OPERATIONS/UHDAS_atsea/adcp_access/read_netCDF.html>
#'
#' 2. IOOS naming convention
#'    <https://cfconventions.org/Data/cf-standard-names/78/build/cf-standard-name-table.html>
#'
#' 3. The SBE names come from a processing manual that was once at
#'    `http://www.seabird.com/document/sbe-data-processing-manual`, but as of
#'    summer 2018, this no longer seems to be provided by SeaBird. A web search
#'    will turn up copies of the manual that have been put online by various
#'    research groups and data-archiving agencies.  On 2018-07-05, the latest
#'    version was named `SBEDataProcessing_7.26.4.pdf` and had release date
#'    12/08/2017; this was the reference version used in coding `oce`.
#'
#' @author Dan Kelley
rename <- function(x, dictionary = "ioos", debug = 0) {
    oceDebug(debug, "rename(..., dictionary=",
        if (is.data.frame(dictionary)) {
            "[data frame]"
        } else {
            paste0("\"", dictionary, "\"")
        },
        ") START\n",
        sep = "", unindent = 1
    )
    if (is.null(x)) {
        return(renameVocabulary(dictionary, debug = debug))
    }
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
    if (is.null(rval@metadata$dataNamesOriginal)) {
        rval@metadata$dataNamesOriginal <- as.list(R$originalName)
        # print(rval@metadata$dataNamesOriginal)
    }
    # print(R$oceName)
    # cat(vectorShow(names(rval@metadata$dataNamesOriginal), n = 30))
    # cat(vectorShow(R$oceName, n = 30))
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
        if (length(newFlagNames) > 0L) {
            names(rval@metadata$flags) <- newFlagNames
        }
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
