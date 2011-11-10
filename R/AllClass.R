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
                slotNames <- slotNames(object)
                if (length(slotNames) != 3) {
                    cat("should be 3 slots, but there are", length(slotNames), "\n")
                    return(FALSE)
                }
                for (name in c("metadata", "data", "processingLog")) {
                    if (!(name %in% slotNames)) {
                        cat("object should have a slot named \"", name, "\"\n", sep="")
                        return(FALSE)
                    }
                }
                return(TRUE)
            })

setMethod(f="show",
          signature="oce",
          definition=function(object) {
              filename <- object[["filename"]]
              if (is.null(filename) || filename == "")
                  cat(class(object)[1], " object has data as follows.\n", sep="")
              else
                  cat(class(object)[1], " object, from file '", object[["filename"]], "', has data as follows.\n", sep="")
              names <- names(object@data)
              ncol <- length(names)
              for (i in 1:ncol) {
                  d <- object@data[[i]]
                  if (inherits(d, "POSIXt")) {
                      cat(vectorShow(d, paste("  ", names[i])))
                  } else if (is.vector(d)) {
                      cat(vectorShow(d, paste("  ", names[i])))
                  } else if (is.array(d)) {
                      dim <- dim(object@data[[i]])
                      if (length(dim) == 1) {
                          cat(vectorShow(d, paste("  ", names[i])))
                      } else if (length(dim) == 2) {
                          cat("   ", names[i], ", a ", dim[1], "x", dim[2], " array with value ", d[1,1], " at [1,1] position\n", sep="")
                      } else if (length(dim) == 3) {
                          cat("   ", names[i], ", a ", dim[1], "x", dim[2], "x", dim[3], " array with value ", d[1,1,1],
                              " at [1,1,1] position\n", sep="")
                      } else {
                          cat("   ", names[i], ", an array of more than 3 dimensions\n")
                      }
                  }
              }
          })

