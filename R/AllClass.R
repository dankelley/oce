setClass("noce",
         representation(metadata="list",
                        data="list",
                        processingLog="list"),
         ## some advise NOT creating e.g. metadata, because we want daughters to do that
         prototype=list(metadata=list(),
                        data=list(),
                        processingLog=list(time = Sys.time(), value = "create base 'oce' object")))

setClass("drifter", contains="noce")
setClass("ctd", contains="noce")
setClass("coastline", contains="noce")
setClass("sealevel", contains="noce")
setClass("section", contains="noce")
## adv
## adp
## lobo
## pt
## tidem
## windrose

###setMethod(f="show",
###          signature="nctd",
###          definition=function(object) {
###              cat("CTD from '", object@metadata$filename, "'\n", sep="")
###          })
#
