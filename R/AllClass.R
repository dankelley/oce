setClass("oce",
         representation(metadata="list",
                        data="list",
                        processingLog="list"),
         ## some advise NOT creating e.g. metadata, because we want daughters to do that
         prototype=list(metadata=list(),
                        data=list(),
                        processingLog=list(time = Sys.time(), value = "create base 'oce' object")))

setClass("adv", contains="oce")
setClass("adp", contains="oce")
setClass("cm", contains="oce")
setClass("coastline", contains="oce")
setClass("ctd", contains="oce")
setClass("drifter", contains="oce")
setClass("lobo", contains="oce")
setClass("pt", contains="oce")
setClass("sealevel", contains="oce")
setClass("section", contains="oce")
setClass("tidem", contains="oce")
setClass("topo", contains="oce")
setClass("windrose", contains="oce")

###setMethod(f="show",
###          signature="nctd",
###          definition=function(object) {
###              cat("CTD from '", object@metadata$filename, "'\n", sep="")
###          })
#
