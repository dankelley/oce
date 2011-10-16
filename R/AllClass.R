setClass("noce",
         representation(metadata="list",
                        data="list",
                        processingLog="list"),
         ## some advise NOT creating e.g. metadata, because we want daughters to do that
         prototype=list(metadata=list(),
                        data=list(),
                        processingLog=list(time = Sys.time(), value = "create base 'oce' object")))

#setGeneric("processingLog",
#           function(object) {
#               standardGeneric("processingLog")
#           })

#setMethod("processingLog",
#          "noce",
#          function(object) {
#              cat("* Processing Log::\n")
#              pl <- object@processingLog
#              n <- length(pl$value)
#              for (i in seq_along(pl$value)) {
#                  cat("  * ", format(pl$time[i]), " UTC: ``", pl$value[i], "``\n", sep="")
#              }
#          })
#
setClass("nctd", contains="noce")
#
setMethod(f="initialize",
          signature="nctd",
          definition=function(.Object,pressure,salinity,temperature,filename) {
              .Object@data$pressure <- pressure
              .Object@data$temperature <-temperature 
              .Object@data$salinity <- salinity
              .Object@metadata$filename <- if (missing(filename)) "" else filename
              .Object@processingLog$time=c(.Object@processingLog$time, Sys.time())
              .Object@processingLog$value=c(.Object@processingLog$value, "create ctd object")
              return(.Object)
          })
#
<<<<<<< HEAD
setMethod(f="plot",
          signature=signature("nctd"),
          definition=function(x, which=1:4) {
              warning("** dummy plot() for initial tests **")
              plot(x@data$temperature, x@data$pressure)
          })

=======
###setMethod(f="plot",
###          signature=signature("nctd"),
###          definition=function(x, which=1:4) {
###              plot(x@data$temperature, x@data$pressure)
###          })
###
>>>>>>> 0b1ca4628bfeadd8b6c4c23ca57143a2ec6421c0
#setMethod(f="[",
#          signature="nctd",
#          definition=function(x, i, j, drop) {
#              ## 'j' can be for times, as in OCE
#              ##if (!missing(j)) cat("j=", j, "*****\n")
#              i <- match.arg(i, c("temperature", "salinity", "pressure"))
#              if (i == "temperature") x@data$temperature
#              else if (i == "salinity") x@data$salinity
#              else if (i == "pressure") x@data$pressure
#              else NULL
#          })
#
#setReplaceMethod(f="[",
#          signature="nctd",
#          definition=function(x, i, j, value) {
#              i<- match.arg(i, c("temperature", "salinity", "pressure"))
#              if (i == "temperature") x@data$temperature <- value
#              else if (i == "salinity") x@data$salinity <- value
#              else if (i == "pressure") x@data$pressure <- value
#              else NULL
#              validObject(x)
#              return(x)
#          })
#
#setMethod(f="summary", "nctd", 
#          function(object){
#              cat("CTD object\n")
#              if (object@metadata$filename != "")
#                  cat("  Source: ``", object@metadata$filename, "''\n", sep="")
#              ncol <- length(object@data)
#              threes <- matrix(nrow=ncol, ncol=3)
#              for (v in 1:ncol)
#                  threes[v,] <- oce::threenum(object@data[[v]])
#              rownames(threes) <- names(object@data)
#              colnames(threes) <- c("Min.", "Mean", "Max.")
#              print(threes)
#              processingLog(object)
#          })
#
###setMethod(f="show",
###          signature="nctd",
###          definition=function(object) {
###              cat("CTD from '", object@metadata$filename, "'\n", sep="")
###          })
#
#
###ctd <- new("ctd", pressure=1:10, salinity=35+1:10, temperature=10+1:10, filename="test")
###str(ctd)
###plot(ctd)
###show(ctd)
###summary(ctd)
###ctd["te"]
###ctd["te"] <- -ctd["te"]
###summary(ctd)
