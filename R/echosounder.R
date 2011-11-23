## vim:textwidth=128:expandtab:shiftwidth=4:softtabstop=4

setMethod(f="initialize",
          signature="echosounder",
          definition=function(.Object, filename="") {
              .Object@metadata$filename <- filename
              .Object@processingLog$time <- as.POSIXct(Sys.time())
              .Object@processingLog$value <- "create 'echosounder' object"
              return(.Object)
          })

setMethod(f="[[",
          signature="echosounder",
          definition=function(x, i, j, drop) {
              as(x, "oce")[[i, j, drop]]
          })

as.echosounder <- function(time, depth, data, src="") # just guessing on args
{
    warning("not doing much yet!")
    res <- new('echosounder')
    data <- data.frame(time=time,
                       depth=depth,
                       data=data)
    metadata <- list(src=src)
    res@metadata <- metadata
    res@data <- data
    res@processingLog <- processingLog(res@processingLog, paste(deparse(match.call()), sep="", collapse=""))
    res
}

setMethod(f="plot",
           signature=signature("echosounder"),
           definition=function(x, which = 1, # 1=z-t section 2=dist-t section 3=map
                               adorn=NULL,
                               mgp=getOption("oceMgp"),
                               mar=c(mgp[1]+1,mgp[1]+1,mgp[1]+1,mgp[1]+1),
                               debug=getOption("oceDebug"),
                               ...)
           {
               oceDebug(debug, "\b\bplot.echosounder() {\n")
               opar <- par(no.readonly = TRUE)
               lw <- length(which)
               adorn.length <- length(adorn)
               if (adorn.length == 1) {
                   adorn <- rep(adorn, lw)
                   adorn.length <- lw
               }
               par(mgp=mgp, mar=mar)
               if (lw > 1) {
                   on.exit(par=opar)
                   if (lw > 2)
                       lay <- layout(matrix(1:4, nrow=2, byrow=TRUE))
                   else
                       lay <- layout(matrix(1:2, nrow=2, byrow=TRUE))
               }
 
               warning("not actually plotting anything")

              for (w in 1:length(which)) {
                   oceDebug(debug, "this which:", w, "\n")
                   if (which[w] == 1) {
                       cat("should plot z-t graph\n")
                   } else if (which[w] == 2) {
                       cat("should plot z-distance graph\n")
                   } else if (which[w] == 3) {
                       cat("should plot map\n")
                   } else {
                       stop("unknown value of which=", which, " (must be 1, 2, or 3)")
                   }
                   if (w <= adorn.length && nchar(adorn[w]) > 0) {
                       t <- try(eval(adorn[w]), silent=TRUE)
                       if (class(t) == "try-error")
                           warning("cannot evaluate adorn[", w, "]\n")
                   }
               }
               oceDebug(debug, "\b\b} # plot.echosounder()\n")
               invisible()
           })

read.echosounder <- function(file, debug=getOption("oceDebug"))
{
    ofile <- file
    filename <- NULL
    if (is.character(file)) {
        filename <- fullFilename(file)
        file <- file(file, "rb")
        on.exit(close(file))
    }
    if (!inherits(file, "connection"))
        stop("argument `file' must be a character string or connection")
    if (!isOpen(file)) {
        open(file, "rb")
        on.exit(close(file))
    }
    res <- new("echosounder", filename=filename)
    seek(file, 0, "start")
    seek(file, where=0, origin="end")
    fileSize <- seek(file, where=0)
    oceDebug(debug, "fileSize=", fileSize, "\n")
    buf <- readBin(file, what="raw", n=fileSize, endian="little")

    ## Section 3.3 of the Biosonics doc (see ?echosounder-class) describes the
    ## file format.  Note that the files are little endian.
    ##
    ## Data are organized as a sequence of tuples, in the following format:
    ##   N = 2-byte unsigned int that indicates the size of the tuple.
    ##   code = 2-byte code (see table below)
    ##   data = N bytes (depends on code)
    ##   N6 = 2 bytes that must equal N+6, or the data are corrupted
    ##
    ## The codes, from the table in section 3.5 of Biosonics doc (see ?echosounder-class)
    ## are as follows.  The first tuple in a file must have code 0xFFFF, and the
    ## second must have code 001E, 0018, or 0001.
    ##
    ##   0xFFFF Signature (for start of file)
    ##   0x001E V3 File Header
    ##   0x0018 ￼V2 File Header
    ##   0x0001 ￼V1 File Header
    ##   0x0012 Channel Descriptor
    ##   0x0036 Extended Channel Descriptor
    ##   0x0015 ￼Single-Beam Ping
    ##   0x001C Dual-Beam Ping
    ##   0x001D Split-Beam Ping
    ##   0x000F or 0x0020 ￼ Time
    ##   0x000E ￼Position
    ##   0x0011 ￼Navigation String
    ##   0x0030 Timestamped Navigation String
    ##   0x0031 Transducer Orientation
    ##   0x0032 Bottom Pick
    ##   0x0033 ￼Single Echoes
    ##   0x0034 ￼Comment
    ##   ￼0xFFFE ￼End of File
    tuple <- 1
    offset <- 0
    while (offset < fileSize) {
        cat("tuple=", tuple, " offset=", offset, "\n")
        N <- readBin(buf[offset+1:2],"integer", size=2, n=1, endian="small", signed=FALSE)
        cat("  N=", N, "\n")
        code <- readBin(buf[offset+3:4],"integer", size=2, n=1, endian="small", signed=FALSE)
        cat("  code=", code, "\n")
        cat("  data= ... not read yet (FIXME)\n")
        N6 <- readBin(buf[offset+N+4+1:2],"integer", size=2, n=1, endian="small", signed=FALSE)
        cat("  N6=", N6, "should equal N+6 ... and we should check that (FIXME)\n")
        if (N6 != N + 6)
            stop("error reading tuple number ", tuple, " (mismatch in redundant header-length flags)")
        offset <- offset + N + 6
        tuple <- tuple + 1
    }
    warning("should work until end of file!  should read the data!  FIXME")
    res@data <- list(test=3)
    res@processingLog <- processingLog(res@processingLog,
                                       paste("read.echosounder(\"", filename, ", debug=", debug, ")"))
    ## FIXME insert data, pl etc
    res
}
## test (until I can get a dataset)
##   writeBin(as.raw(c(0x00, 0x00, 0xFF, 0xFF, 0x00, 0x06)), file("test", "wb"))
##   source('~/src/R-kelley/oce/R/echosounder.R'); read.echosounder('test')

summary.echosounder <- function(object, ...)
{
    cat("Echosounder Summary\n-------------------\n\n")
    showMetadataItem(object, "filename", "File source:        ")
    warning("FIXME: should summarize now")
}
