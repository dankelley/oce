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

setMethod(f="[[",
          signature="oce",
          definition=function(x, i, j, drop) {
              if (i %in% names(x@metadata)) return(x@metadata[[i]])
              else if (i %in% names(x@data)) return(x@data[[i]])
              else stop("there is no item named \"", i, "\" in this ", class(x), " object")
          })

setMethod(f="[[<-",
          signature="oce",
          definition=function(x, i, j, value) { # FIXME: use j for e.g. times
              if (i %in% names(x@metadata)) x@metadata[[i]] <- value
              else if (i %in% names(x@data)) x@data[[i]] <- value
              else stop("there is no item named \"", i, "\" in this ", class(x), " object")
              validObject(x)
              invisible(x)
          })

setValidity("oce",
            function(object) {
                ndata <- length(object@data)
                lengths <- vector("numeric", ndata)
                for (i in 1:ndata)
                    lengths[i] <- length(object@data[[i]])
                if (var(lengths) != 0) {
                    cat("lengths of data elements are unequal\n")
                    return(FALSE)
                } else
                    return(TRUE)
            })

setMethod(f="show",
          signature="oce",
          definition=function(object) {
              class <- class(object)[1]
              names <- names(object@data)
              ncol <- length(names)
              if (ncol > 0) {
                  if ("filename" %in% names(object@metadata) && object[["filename"]] != "")
                      cat(class, " object from file \"", object[["filename"]], "\" has column data\n", sep="")
                  else
                      cat(class, " object has column data\n", sep="")
                  for (i in 1:ncol) {
                      cat(vectorShow(object@data[[i]], paste("  ", names[i])))
                  }
              } else {
                  cat(class, " object has nothing in its \"data\" slot\n")
              }
          })

