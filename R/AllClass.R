setClass("noce",
         representation(metadata="list",
                        data="list",
                        processingLog="list"),
         ## some advise NOT creating e.g. metadata, because we want daughters to do that
         prototype=list(metadata=list(),
                        data=list(),
                        processingLog=list(time = Sys.time(), value = "create base 'oce' object")))

setClass("ctd", contains="noce")
setClass("sealevel", contains="noce")


#

##
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
