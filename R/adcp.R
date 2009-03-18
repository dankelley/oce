read.adcp <- function(file, type ="RDI", debug=FALSE, log.action)
{
   if (is.character(file)) {
       filename <- file
       file <- file(file, "rb")
       on.exit(close(file))
   }
   if (!inherits(file, "connection")) {
       stop("argument `file' must be a character string or connection")
   }
   if (!isOpen(file)) {
       filename <- "(connection)"
       open(file, "r")
       on.exit(close(file))
   }
   if (missing(log.action)) log.action <- paste(deparse(match.call()), sep="", collapse="")
   cat("reading an adcp file\n")
   data <- list(da=1)

   cat("DEBUG: first 3 bytes are:\n")
   b <- readBin(file, "raw", n=1, size=1)
   if (b != 0x7f) stop("first byte in file must be 0x7f")
   b <- readBin(file, "raw", n=1, size=1)
   if (b != 0x7f) stop("second byte in file must be 0x7f")
   nb <<- readBin(file, "raw", n=2, size=1)
   n <- as.integer(nb[2]) * 256 + as.integer(nb[1])
   cat("n=", n, " (not sure this is right -- check vs MATLAB)\n", sep="")
   metadata <- list(filename=filename, n=n)
   if (missing(log.action)) log.action <- paste(deparse(match.call()), sep="", collapse="")
   log.item <- processing.log.item(log.action)
   res <- list(data=data, metadata=metadata, processing.log=log.item)
   class(res) <- c("adcp", "oce")
   res
}

summary.adcp <- function(object, ...)
{
    if (!inherits(object, "adcp")) stop("method is only for adcp objects")
##    dim <- dim(object$data)
##    fives <- matrix(nrow=dim[2], ncol=5)
    res <- list(filename="?",
##                fives=fives,
                processing.log=processing.log.summary(object))
##    if (!is.null(object$metadata$filename.orig))      res$filename <- object$metadata$filename.orig
##    res$levels <- dim[1]
##    for (v in 1:dim[2])
##        fives[v,] <- fivenum(object$data[,v], na.rm=TRUE)
##    rownames(fives) <- names(object$data)
##    colnames(fives) <- c("Min.", "1st Qu.", "Median", "3rd Qu.", "Max.")
##    res$fives <- fives
    class(res) <- "summary.adcp"
    res
}

print.summary.adcp <- function(x, digits=max(6, getOption("digits") - 1), ...)
{
    cat("ADCP record\n")
#    cat("  Raw file:           \"",     x$filename, "\"\n",sep="")
#    cat(paste("  System upload time: ", x$system.upload.time, "\n"))
#    cat(paste("  Date:               ", x$date, "\n"))
#    cat("  Institute:          ",       x$institute, "\n")
#    cat("  Scientist:          ",       x$scientist, "\n")
#    cat("  Ship:               ",       x$ship, "\n")
#    cat("  Cruise:             ",       x$cruise, "\n")
#    cat("  Location:           ",       latlon.format(x$latitude, x$longitude, digits=digits), "\n")
#    cat("  Station:            ",       x$station, "\n")
#    cat(paste("  Start time:         ", as.POSIXct(x$start.time), "\n"))
#    cat(paste("  Deployed:           ", x$date, "\n"))
#    cat(paste("  Recovered:          ", x$recovery, "\n"))
#    cat("  Water depth:        ",       x$water.depth, "\n")
#    cat("  No. of levels:      ",       x$levels,  "\n")
#    print(x$fives, digits=digits)
    print(x$processing.log)
    invisible(x)
}
