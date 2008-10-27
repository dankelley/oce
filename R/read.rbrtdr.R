read.rbrtdr <- function(file, debug=FALSE)
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
    header <- scan(file, what='char', sep="\n", n=19, quiet=TRUE)
    scan(file, what="char", sep="\n", n=1, quiet=TRUE) # column names
    d <- scan(file, character(), quiet=TRUE)
    n <- length(d) / 4
    ## check if integer
    t <- as.POSIXlt(paste(d[seq(1,0+4*n,4)], d[seq(2,1+4*n,4)]))
    temperature <- as.numeric(d[seq(3,2+4*n,4)])
    pressure <- as.numeric(d[seq(4,3+4*n,4)])
    data <- data.frame(t=t, temperature=temperature, pressure=pressure)
    metadata <- list(
                     header=header)
    log.item <- list(time=c(Sys.time()), action=c(paste("created by read.rbrtdr(\"",filename,"\")",sep="")))
    rval <- list(data=data, metadata=metadata, processing.log=log.item)
    class(rval) <- c("rbrtdr", "oce")
    rval
}
