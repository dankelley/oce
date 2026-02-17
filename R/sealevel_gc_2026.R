#' Read a sealevel File in Government of Canada format 2026
#'
#' This is somewhat provisional code, to handle what seems to be
#' a new format as of the year 2026.  In this format, files come
#' in pairs, and so this function takes just the station identifier
#' and seeks files with names related to that, according to
#' a pattern observed on 2026-02-17. Whether this pattern will hold
#' is unclear.
#'
#' The two input files are constructed from the value of the `stn` parameter.
#' For example, with `stn="00491", the constructed filenames will be
#' `"00491_metadata.csv"` and `"00491_data.csv"`. Note that the first of these
#' is read, with contents inserted into the `metadata` of the returned object,
#' but the items contained therein are not otherwise examined by this function.
#' Instead, the function determines station location from a header contained
#' within the "data" file.
#'
#' @param file string vector of length 2, holding the names of the metadata file
#' and the data file.
#'
#' @template debugTemplate
#'
#' @return A [sealevel-class] object.
#'
#' @author Dan Kelley and Chantelle Layton
#'
#' @family things related to sealevel data
read.sealevel.gc2026 <- function(file, debug = 0) {
    if (2 != length(file)) stop("'file' should be of length 2")
    if (grepl("metadata", file[1])) {
        mf <- file[1]
        df <- file[2]
    } else {
        mf <- file[2]
        df <- file[1]
    }
    lines <- readLines(mf)
    commaLines <- lines[grepl(",", lines)]
    keys <- NULL
    values <- NULL
    for (line in commaLines) {
        kv <- strsplit(line, ",")[[1]]
        keys <- c(keys, kv[1])
        values <- c(values, kv[2])
    }
    metadata <- as.list(values)
    names(metadata) <- keys
    d <- readLines(df)
    headerLines <- grep(" - ", d)
    header <- d[headerLines]
    metadata$header <- header
    metadata$filename <- paste0(mf, ";", df)

    data <- read.csv(df, skip = max(headerLines), header = FALSE, col.names = c("time", "elevation"))
    data$time <- as.POSIXct(data$time, "%Y/%m/%d %H:%M", tz = "UTC")
    rval <- new("sealevel")
    rval@metadata <- metadata
    rval@data <- data
    # get latitude and longitude from the 'data' file, not
    # from the 'metadata' file
    look <- grep("^Latitude", header)
    if (length(look) == 1) {
        rval@metadata$latitude <- as.numeric(strsplit(header[look], ",")[[1]][2])
    }
    look <- grep("^Longitude", header)
    if (length(look) == 1) {
        rval@metadata$longitude <- as.numeric(strsplit(header[look], ",")[[1]][2])
    }
    # deltat is needed for summary plots.  I think cph is the unit
    rval@metadata$deltat <- (as.numeric(data$time[2]) - as.numeric(data$time[1])) / 3600.0
    rval
}
