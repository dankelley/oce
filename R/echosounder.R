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
        file <- file(file, "r")
        on.exit(close(file))
    }
    if (!inherits(file, "connection"))
        stop("argument `file' must be a character string or connection")
    if (!isOpen(file)) {
        open(file, "r")
        on.exit(close(file))
    }
    warning("should read echosounder data now")
    res <- new("echosounder")
    processingLog <- paste(deparse(match.call()), sep="", collapse="")
    ## FIXME insert data, pl etc
    res
}


summary.echosounder <- function(object, ...)
{
    cat("Echosounder Summary\n-------------------\n\n")
    showMetadataItem(object, "filename", "File source:        ")
    warning("FIXME: should summarize now")
}
