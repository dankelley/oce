setClass("oce",
         representation(metadata="list",
                        data="list",
                        processingLog="list"),
         ## some advise NOT creating e.g. metadata, because we want daughters to do that
         prototype=list(metadata=list(),
                        data=list(),
                        processingLog=list(time = Sys.time(), value = "create base 'oce' object")))

## coding progress (notes in class definitions):
##   V = needs validity checker
##   a = needs [[<- for assignment
##   s = needs show method
setClass("adv", contains="oce")        # V a s
setClass("adp", contains="oce")        # V a s
setClass("cm", contains="oce")         # V a s
setClass("coastline", contains="oce")  # V a s
setClass("ctd", contains="oce")        #     s
setClass("drifter", contains="oce")    # V a s
setClass("lobo", contains="oce")       # V a s 
setClass("pt", contains="oce")         # V a s 
setClass("sealevel", contains="oce")   # V a s 
setClass("section", contains="oce")    # V a s 
setClass("tidem", contains="oce")      # V a s 
setClass("topo", contains="oce")       # V a s 
setClass("windrose", contains="oce")   # V a s 


###setMethod(f="show",
###          signature="nctd",
###          definition=function(object) {
###              cat("CTD from '", object@metadata$filename, "'\n", sep="")
###          })
#
