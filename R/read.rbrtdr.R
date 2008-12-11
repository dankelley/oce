read.rbrtdr <- function(file, debug=FALSE, log.action)
{
    filename <- file
    if (is.character(file)) {
        file <- file(file, "r")
        on.exit(close(file))
    }
    if (!inherits(file, "connection")) {
        stop("argument `file' must be a character string or connection")
    }
    if (!isOpen(file)) {
        open(file, "r")
        on.exit(close(file))
    }
    host.time <- 0
    logger.time <- 0
    logging.start <- 0
    logging.end <- 0
    sample.period <- 0
    number.channels <- 0
    ## Q: what ends the header? a blank line?  Line 21?
    ## calibration 1
    ## calibration 2
    ## correction.to.conductivity
    ## memory type
    ## Timestamp
    ## columns t, Temperature, p

##    header <- scan(file, what='char', sep="\n", n=19, quiet=TRUE)
    header <- c()
    logging.start <- sample.period <- NULL
    while (TRUE) {
        line <- scan(file, what='char', sep="\n", n=1, quiet=TRUE)
        if (0 < (r<-regexpr("Temp[ \t]*Pres", line))) break
        header <- c(header, line)
        if (0 < (r<-regexpr("Logging[ \t]*start", line))) {
            l <- sub("[ ]*Logging[ \t]*start[ ]*", "", line)
            logging.start <- strptime(l,"%y/%m/%d %H:%M:%S")
        }
        if (0 < (r<-regexpr("Sample[ \t]*period", line))) {
            l <- sub("[ ]*Sample[ \t]*period[ ]*", "", line)
            sp <- as.numeric(strsplit(l, ":")[[1]])
            sample.period <- (sp[3] + 60*(sp[2] + 60*sp[1]))
        }
    }

    if (debug) {
        cat("logging.start:");print(logging.start)
        cat("sample.period:");print(sample.period)
    }

    col.names <- strsplit(gsub("[ ]+"," ", gsub("[ ]*$","",gsub("^[ ]+","",line))), " ")[[1]]

    ## Read a line to determine if there is a pair of columns for time
    line <- scan(file, what='char', sep="\n", n=1, quiet=TRUE)
    pushBack(line, file)
    line <- gsub("[ ]+$", "", gsub("^[ ]+","", line))
    nvar <- length(strsplit(line, "[ ]+")[[1]])

    if (debug) cat("Data line '", line, "' reveals ", nvar, " data per line\n", sep="")

    d <- scan(file, character(), quiet=TRUE)
    n <- length(d) / nvar
    if (nvar == 4) {
        if (debug) cat("4 elements per data line\n")
        t <- as.POSIXlt(paste(d[seq(1,4*n,4)], d[seq(2,4*n,4)]))
        temperature <- as.numeric(d[seq(3,4*n,4)])
        pressure <- as.numeric(d[seq(4,4*n,4)])
    } else if (nvar == 2) {
        if (debug) cat("2 elements per data line\n")
        t <- logging.start + seq(1:n) * sample.period
        temperature <- as.numeric(d[seq(1,2*n,2)])
        pressure <- as.numeric(d[seq(2,2*n,2)])
    } else stop("wrong number of variables.  Expect 2 or 4")

    data <- data.frame(t=t, temperature=temperature, pressure=pressure)
    metadata <- list(header=header, logging.start=logging.start, sample.period=sample.period)
    if (missing(log.action)) log.action <- deparse(match.call())
    log.item <- processing.log.item(log.action)
    rval <- list(data=data, metadata=metadata, processing.log=log.item)
    class(rval) <- c("rbrtdr", "oce")
    rval
}
