setClass("oce",
         representation(metadata="list",
                        data="list",
                        processingLog="list"),
         prototype=list(metadata=list(),
                        data=list(),
                        processingLog=list()))

setClass("adv", contains="oce")
setClass("adp", contains="oce")
setClass("bremen", contains="oce") # 20150528 may be called "geomar" or something later
setClass("cm", contains="oce")
setClass("coastline", contains="oce")
setClass("ctd", contains="oce")
setClass("drifter", contains="oce")
setClass("echosounder", contains="oce")
setClass("gps", contains="oce")
setClass("ladp", contains="oce")
setClass("landsat", contains="oce")
setClass("lisst", contains="oce")
setClass("lobo", contains="oce")
setClass("met", contains="oce")
setClass("odf", contains="oce")
setClass("rsk", contains="oce")
setClass("sealevel", contains="oce")
setClass("section", contains="oce")
setClass("tidem", contains="oce")
setClass("topo", contains="oce")
setClass("windrose", contains="oce")


setMethod(f="subset",
          signature="oce",
          definition=function(x, subset, ...) {
              if (missing(subset))
                  stop("must give 'subset'")
              keep <- eval(substitute(subset), x@data, parent.frame())
              rval <- x
              for (i in seq_along(x@data))
                  rval@data[[i]] <- rval@data[[i]][keep]
              rval@processingLog <- processingLogAppend(rval@processingLog,
                                                        paste(deparse(match.call(call=sys.call(sys.parent(1)))),
                                                              sep="", collapse=""))
              rval
          })

setMethod(f="[[",
          signature(x="oce", i="ANY", j="ANY"),
          definition=function(x, i, j, drop) {
              if (i == "metadata") {
                  return(x@metadata)
              } else if (i == "data") {
                  return(x@data)
              } else if (i == "processingLog") {
                  return(x@processingLog)
              } else {
                  ## metadata must match exactly but data can be partially matched
                  if (i %in% names(x@metadata))
                      return(x@metadata[[i]])
                  index <- pmatch(i, names(x@data))
                  if (!is.na(index[1]))
                      return(x@data[[index]])
                  else
                      return(NULL)
                  ## if (missing(j) || j != "nowarn")
                  ##     warning("there is no item named \"", i, "\" in this ", class(x), " object", call.=FALSE)
              }
          })

setMethod(f="[[<-",
          signature(x="oce", i="ANY", j="ANY"),
          function(x, i, j, ..., value) { # FIXME: use j for e.g. times
              ## metadata must match exactly but data can be partially matched
              if (i %in% names(x@metadata)) {
                  x@metadata[[i]] <- value
              } else {
                  index <- pmatch(i, names(x@data))
                  if (!is.na(index[1])) {
                      x@data[[index]] <- value
                  } else if (i == "processingLog") {
                      if (0 == length(x@processingLog)) {
                          x@processingLog <- list(time=as.POSIXct(Sys.time(), tz="UTC"), value=value)
                      } else {
                          x@processingLog$time <- c(x@processingLog$time, as.POSIXct(Sys.time(), tz="UTC"))
                          x@processingLog$value <- c(x@processingLog$value, value)
                      }
                  } else {
                      warning("there is no item named \"", i, "\" in this ", class(x), " object", call.=FALSE)
                  }
              }
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
              if ("filename" %in% names(object@metadata))
                  filename <- object[["filename"]]
              else
                  filename <- "(no filename known)"
              if (is.null(filename) || filename == "")
                  cat(class(object)[1], " object has data as follows.\n", sep="")
              else
                  cat(class(object)[1], " object, from file '", filename, "', has data as follows.\n", sep="")
              names <- names(object@data)
              ncol <- length(names)
              for (i in 1:ncol) {
                  d <- object@data[[i]]
                  if (0 == length(d)) {
                      cat("  ", names[i], ": empty\n")
                  } else {
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
              }
          })

