#' Read a NOAA ocean index file
#' 
#' Read an ocean index file, in the format used by NOAA.
#' 
#' Reads a text-format index file, in a format used by NOAA.  The first line
#' holds two numbers, for start year and end year.  Then there are several
#' lines containing 13 numbers, the first being the year, and the others being
#' the data.  Then there is a line with a single number, the missing value.
#' Then there are some headers lines (which are ignored in the present version
#' of \code{read.index}.
#' 
#' @param file a connection or a character string giving the name of the file
#' to load.  May be a URL.
#' @param tz character string indicating time zone to be assumed in the data.
#' @param debug a flag that turns on debugging, ignored in the present version
#' of the function.
#' @return A data frame containing \code{t}, a POSIX time, and \code{index},
#' the numerical index.  The times are set to the 15th day of each month, which
#' is a guess that may need to be changed if so indicated by documentation (yet
#' to be located).
#' @author Dan Kelley
#' @references See \url{http://www.esrl.noaa.gov/psd/data/climateindices/list/}
#' for a list of indices.
#' @keywords misc
#' @examples
#' 
#' \dontrun{
#' library(oce)
#' # Arctic oscillation
#' ao <- read.index("http://www.esrl.noaa.gov/psd/data/correlation/ao.data")
#' recent <- subset(ao, t > as.POSIXct("2000-01-01"))
#' oce.plot.ts(recent$t, recent$index)
#' }
read.index <- function(file, tz=getOption("oceTz"), debug=getOption("oceDebug"))
{
    if (is.character(file)) {
        ##filename <- fullFilename(file)
        file <- file(file, "r")
        on.exit(close(file))
    }
    if (!inherits(file, "connection"))
        stop("argument `file' must be a character string or connection")
    if (!isOpen(file)) {
        open(file, "r")
        on.exit(close(file))
    }
    lines <- readLines(file)
    lines <- lines[-1] # drop first header line
    ## the missing value is on a line by itself, and after that
    ## is footer that we will ignore for now.
    n <- unlist(lapply(seq_along(lines),
                       function(l) length(scan(text=lines[l], what="numeric", quiet=TRUE))))
    onetoken <- which(n==1)[1]
    if (is.na(onetoken))
        stop("cannot find missing-value token")
    missingValue <- as.numeric(lines[onetoken])
    lines <- lines[seq.int(1L, onetoken-1)]
    d <- as.matrix(read.table(text=lines, header=FALSE))
    year <- d[,1]
    t <- seq(ISOdatetime(year[1], 1, 15, 0, 0, 0, tz="UTC"), by="month", length.out=12*length(year))
    data <- as.vector(t(d[,-1]))
    data[data == missingValue] <- NA
    data.frame(t=t, index=data)
}

