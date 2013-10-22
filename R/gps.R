## vim:textwidth=128:expandtab:shiftwidth=4:softtabstop=4

setMethod(f="initialize",
          signature="gps",
          definition=function(.Object, longitude, latitude, filename="") {
              if (!missing(longitude)) .Object@data$longitude <- as.numeric(longitude)
              if (!missing(latitude)) .Object@data$latitude <- as.numeric(latitude)
              .Object@metadata$filename <- filename
              .Object@processingLog$time <- as.POSIXct(Sys.time())
              .Object@processingLog$value <- "create 'gps' object"
              return(.Object)
          })

setMethod(f="[[",
          signature="gps",
          definition=function(x, i, j, drop) {
              ## I use 'as' because I could not figure out callNextMethod() etc
              as(x, "oce")[[i, j, drop]]
          })

as.gps <- function(longitude, latitude, filename="")
{
    names <- names(longitude)
    if ("longitude" %in% names && "latitude" %in% names) {
        latitude <- longitude[["latitude"]]
        longitude <- longitude[["longitude"]]
    }
    rval <- new('gps', longitude=longitude, latitude=latitude, filename=filename)
}

summary.gps <- function(object, ...)
{
    if (!inherits(object, "gps"))
        stop("method is only for gps objects")
    threes <- matrix(nrow=2, ncol=3)
    threes[1,] <- threenum(object@data$latitude)
    threes[2,] <- threenum(object@data$longitude)
    colnames(threes) <- c("Min.", "Mean", "Max.")
    rownames(threes) <- c("Latitude", "Longitude")
    cat("GPX Summary\n-----------------\n\n")
    cat("* Number of points:", length(object@data$latitude), ", of which", 
        sum(is.na(object@data$latitude)), "are NA.\n")
    cat("\n",...)
    cat("* Statistics of subsample::\n\n", ...)
    print(threes)
    cat("\n")
    processingLogShow(object)
}

read.gps <- function(file,
                     type=c("gps"),
                     debug=getOption("oceDebug"),
                     monitor=FALSE,
                     processingLog)
{
    stop("not coded yet")
}

