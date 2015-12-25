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

