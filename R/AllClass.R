setClass("oce",
         representation(metadata="list",
                        data="list",
                        processingLog="list"),
         prototype=list(metadata=list(filename="", units=list(), flags=list()),
                        data=list(),
                        processingLog=list()))

setMethod(f="summary",
          signature="oce",
          definition=function(object, ...) {
              names <- names(object@data)
              isTime <- grepl("^time", names, ignore.case=TRUE)
              if (any(isTime)) {
                  time <- object@data[[which(isTime)[1]]]
                  if (inherits(time, "POSIXt")) {
                      from <- min(time, na.rm=TRUE)
                      to <- max(time, na.rm=TRUE)
                      deltat <- mean(diff(as.numeric(time)), na.rm=TRUE)
                      cat("* Time ranges from", format(from), "to", format(to), "with mean increment", deltat, "s\n")
                  }
              }
              ndata <- length(object@data)
              if (ndata > 0) {
                  threes <- matrix(nrow=sum(!isTime), ncol=3)
                  ii <- 1
                  for (i in 1:ndata) {
                      ##message("i: ", i, ", name: ", names(object@data)[i])
                      if (isTime[i])
                          next
                      threes[ii,] <- threenum(object@data[[i]])
                      ii <- ii + 1
                  }
                  ##rownames(threes) <- paste("   ", names[!isTime])
                  units <- if ("units" %in% names(object@metadata)) object@metadata$units else NULL
                  rownames(threes) <- paste("    ", dataLabel(names[!isTime], units))
                  colnames(threes) <- c("Min.", "Mean", "Max.")
                  cat("* Statistics of data\n")
                  print(threes, indent='    ')
              }
              processingLogShow(object)
              invisible(threes)
          })


setClass("adv", contains="oce")
setClass("adp", contains="oce")
setClass("argo", contains="oce")
setClass("bremen", contains="oce") # 20150528 may be called "geomar" or something later
setClass("cm", contains="oce")
setClass("coastline", contains="oce")
setClass("ctd", contains="oce")
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
              ##message("percent keep ", round(sum(keep)/length(keep)*100, 2), "%")
              res <- x
              for (i in seq_along(x@data))
                  res@data[[i]] <- res@data[[i]][keep]
              res@processingLog <- processingLogAppend(res@processingLog,
                                                        paste(deparse(match.call(call=sys.call(sys.parent(1)))),
                                                              sep="", collapse=""))
              res
          })

setMethod(f="[[",
          signature(x="oce", i="ANY", j="ANY"),
          definition=function(x, i, j, ...) {
              if (i == "metadata") {
                  return(x@metadata)
              } else if (length(grep(" unit$", i))) {
                  return(if ("units" %in% names(x@metadata)) x@metadata$units[[gsub(" unit$","",i)]] else "")
                  ## Permit two ways of storing units, the second archaic and kept to handle old objects
              } else if (length(grep("Unit$", i))) {
                  ## Permit two ways of storing units, the second archaic and kept to handle old objects
                  return(if ("units" %in% names(x@metadata)) x@metadata$units[[gsub("Unit$","",i)]] else x@metadata[[i]])
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
                  } else if (length(grep("Unit$", i))) {
                      if ("units" %in% names(x@metadata))
                          x@metadata$units[[gsub("Unit$", "", i)]] <- value
                      else
                          x@metadata[[i]] <- value
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
                  filename <- "(filename unknown)"
              names <- names(object@data)
              ncol <- length(names)
              if (is.null(filename) || filename == "" || is.na(filename) || filename=="(filename unknown)") {
                  if (ncol > 0) {
                      cat(class(object)[1], " object has data as follows.\n", sep="")
                  } else {
                      cat(class(object)[1], " object has nothing in its data slot.\n", sep="")
                  }
              } else {
                  if (ncol > 0) {
                      cat(class(object)[1], " object, from file '", filename, "', has data as follows.\n", sep="")
                  } else {
                      cat(class(object)[1], " object, from file '", filename, "', has nothing in its data slot.\n", sep="")
                  }
              }
              for (i in seq_along(names)) {
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


