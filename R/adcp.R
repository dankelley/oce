read.adcp <- function(file, type ="RDI", debug=FALSE, log.action)
{
    show.bytes <- function(file, n) {
        cat("next", n, "bytes of file:\n")
        for (i in 1:n) {
            b <- readBin(file, "raw", n=1, size=1)
            cat("[", b, "] ")
            if (!(i %% 10)) cat("\n")
        }
    }
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
    data <- list(da=1)
    headerID <- readBin(file, "raw", n=1, size=1)
    if (headerID != 0x7f) stop("first byte in file (headID) must be 0x7f, but it was", headerID)
    dataID <- readBin(file, "raw", n=1, size=1)
    if (dataID != 0x7f) stop("second byte in file (dataID) must be 0x7f but it was", dataID)
    offset.to.cksum <- readBin(file, "raw", n=2, size=1) # not used
    spare <- readBin(file, "raw", n=1, size=1)
    if (spare != 0x00) stop("byte 5 of file must be 0x00, but it was", spare)
    num.data.types <- readBin(file, "integer", n=1, size=1)
    if (debug) cat("number of data types=", num.data.types, "\n")
    if (num.data.types < 1) stop("cannot have 0 or fewer data types")
    data.offset <- vector("integer", length=num.data.types)
    for (i in 1:num.data.types)
        data.offset[i] <- readBin(file, "integer", n=1, size=2, endian="little")
    # end of header; start of fixed leader data
    b <- readBin(file, "raw", n=1, size=1)
    if (b != 0x00) stop("first byte of fixed leader header must be 0x00 but it was ", b)
    b <- readBin(file, "raw", n=1, size=1)
    if (b != 0x00) stop("second byte of fixed leader header must be 0x00 but it was ", b)
    fv <- readBin(file, "integer", n=1, size=1)
    fr <- readBin(file, "integer", n=1, size=1)
    program.version <- paste(fv, fr, sep=".") # don't want to rely on number of digits
    system.configuration <- readBin(file, "integer", n=1, size=2, endian="little")
    b <- readBin(file, "raw", n=1, size=1); if (b != 0x00) warning("expecting byte 0x00 but got ", b)
    b <- readBin(file, "raw", n=1, size=1); if (b != 0x00) warning("expecting byte 0x00 but got ", b)
    num.beams <- readBin(file, "integer", n=1, size=1)
    num.cells <- readBin(file, "integer", n=1, size=1) # WN
    b <- readBin(file, "raw", n=1, size=1); if (b != 0x01) warning("expecting byte 0x01 but got ", b)
    b <- readBin(file, "raw", n=1, size=1); if (b != 0x00) warning("expecting byte 0x00 but got ", b)
    depth.cell.length <- readBin(file, "integer", n=1, size=2, endian="little") # WS
    WF <- readBin(file, "integer", n=2, size=1, endian="little")
    profiling.mode <- readBin(file, "integer", n=1, size=1, endian="little") # WM
    b <- readBin(file, "raw", n=1, size=1); if (b != 0x00) warning("expecting byte 0x00 but got ", b)
    num.cr <- readBin(file, "raw", n=1, size=1)
    b <- readBin(file, "raw", n=1, size=1); if (b != 0x00) warning("expecting byte 0x00 but got ", b)
    if (debug) print(list(program.version=program.version, # ok
                          num.cells=num.cells, # ok
                          num.data.types=num.data.types,
                          profiling.mode=profiling.mode,
                          num.beams=num.beams,
                          # ok (checks with matlab) above this comment
                          depth.cell.length=depth.cell.length,
                          data.offset=data.offset,
                          fv=fv,
                          fr=fr,
                          system.configuration=system.configuration,
                          #WN=WN,
                          #WS=WS,
                          WF=WF,
                          #WM=WM,
                          num.cr=num.cr))


    #show.bytes(file, 50)
    metadata <- list(filename=filename, num.data.types=num.data.types)
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
    cat("\n\nMetadata:\n")
    print(x$metadata)
    print(x$processing.log)
    invisible(x)
}
