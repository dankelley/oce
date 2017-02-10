#' Base Class for oce Objects
#'
#' This is mainly used within oce to create sub-classes, although
#' users can use \code{new("oce")} to create a blank \code{oce}
#' object, if desired.
#'
#' @slot metadata A list containing information about the data. The
#' contents vary across sub-classes, e.g. an \code{\link{adp-class}}
#' object has information about beam patterns, which obviously would
#' not make sense for a \code{\link{ctd-class}} object. In addition,
#' all classes have items named \code{units} and \code{flags}, used
#' to store information on the units of the data, and the data quality.
#' @slot data A list containing the data.
#' @slot processingLog A list containing time-stamped processing steps,
#' typically stored in the object by oce functions.
#'
#' @examples
#' str(new("oce"))
#'
#' @family classes provided by \code{oce}
setClass("oce",
         representation(metadata="list",
                        data="list",
                        processingLog="list"),
         prototype=list(metadata=list(units=list(),
                                      flags=list()),
                        data=list(),
                        processingLog=list(time=as.POSIXct(Sys.time()),
                                           value="Create oce object.")
                        )
         )

#' Summarize an oce Object
#'
#' Provide a textual summary of some pertinent aspects of the object, including
#' selected components of its \code{metadata} slot, statistical and
#' dimensional information on the entries in the \code{data} slot,
#' and a listing of the contents of its \code{processingLog} slot.
#' The details depend on the class of the object, especially for
#' the \code{metadata} slot, so it can help to consult the specialized
#' documentation, e.g. \code{\link{summary,ctd-method}} for CTD objects
#' (i.e. objects inheriting from \code{\link{ctd-class}}.)
#' It is important to note that this is not
#' a good way to learn the details of the object contents. Instead,
#' for an object named \code{object}, say, one might use \code{\link{str}(object)}
#' to learn about all the contents, or \code{\link{str}(object[["metadata"]])}
#' to learn about the \code{metadata}, etc.
#'
#' @param object The object to be summarized.
#' @param ... Extra arguments (ignored)
#'
#' @examples
#' o <- new("oce")
#' summary(o)
setMethod(f="summary",
          signature="oce",
          definition=function(object, ...) {
              names <- names(object@data)
              isTime <- grepl("^time", names, ignore.case=TRUE) # pass timestampIMU
              if (any(isTime)) {
                  time <- object@data[[which(isTime)[1]]]
                  ## Times are always in POSIXct, so the length() does something useful
                  if (inherits(time, "POSIXt") && length(time) > 0) {
                      from <- min(time, na.rm=TRUE)
                      to <- max(time, na.rm=TRUE)
                      nt <- length(time)
                      deltat <- mean(diff(as.numeric(time)), na.rm=TRUE)
                      if (is.na(deltat)) {
                          cat("* Time:               ", format(from), "\n")
                      } else {
                          if (deltat < 60) {
                              cat("* Time ranges from", format(from), "to", format(to), "with", nt, "samples and mean increment", deltat, "s\n")
                          } else if (deltat < 3600) {
                              cat("* Time ranges from", format(from), "to", format(to), "with", nt, "samples and mean increment", deltat/60, "min\n")
                          } else if (deltat < 24*3600) {
                              cat("* Time ranges from", format(from), "to", format(to), "with", nt, "samples and mean increment", deltat/3600, "hour\n")
                          } else {
                              cat("* Time ranges from", format(from), "to", format(to), "with", nt, "samples and mean increment", deltat/3600/24, "day\n")
                          }
                      }
                  }
              }
              ndata <- length(object@data)
              threes <- NULL
              if (ndata > 0) {
                  threes <- matrix(nrow=ndata, ncol=4)
                  for (i in 1:ndata) {
                      threes[i, ] <- threenum(object@data[[i]])
                  }
                  ##rownames(threes) <- paste("   ", names[!isTime])
                  units <- if ("units" %in% names(object@metadata)) object@metadata$units else NULL
                  ## paste the scale after the unit
                  unitsNames <- names(object@metadata$units)
                  units <- unlist(lapply(seq_along(object@metadata$units),
                                         function(i) {
                                             u <- object@metadata$units[[i]]
                                             ##> message("AllClass.R:48  u: '", u, "'")
                                             ##> message("AllClass.R:48  name: '", names(object@metadata$units)[i], "'")
                                             ##> message("length(u[1][[1]]): ", length(u[1][[1]]))
                                             if (0 == length(u[1][[1]])) {
                                                 if (2 == length(u)) return(u[2]) else return("")
                                             }
                                             if (length(u) == 1) {
                                                 res <- if (is.expression(u)) as.character(u) else u
                                             } else if (length(u) == 2) {
                                                 res <- if (nchar(u[2])) paste(u[[1]], u[[2]], sep=", ") else u[[1]]
                                             } else {
                                                 res <- ""
                                             }
                                             res <- as.character(res)[1] # the [1] is in case the unit is mixed up
                                             ##> message("1. res: '", res, "'")
                                             ## Clean up notation, by stages. (The order may matter.)
                                             if (nchar(res)) res <- gsub("degree[ ]+[*][ ]+C", "\u00B0C", res)
                                             if (nchar(res)) res <- gsub("degree[ ]+[*][ ]+F", "\u00B0F", res)
                                             if (nchar(res)) res <- gsub("degree[ ]+[*][ ]+E", "\u00B0E", res)
                                             if (nchar(res)) res <- gsub("degree[ ]+[*][ ]+W", "\u00B0W", res)
                                             if (nchar(res)) res <- gsub("degree[ ]+[*][ ]+N", "\u00B0N", res)
                                             if (nchar(res)) res <- gsub("degree[ ]+[*][ ]+S", "\u00B0S", res)
                                             if (nchar(res)) res <- gsub("percent", "%", res)
                                             ##> message("res: '", res, "'")
                                             if (nchar(res)) res <- gsub("degree", "\u00B0", res)
                                             ##> message("res: '", res, "'")
                                             ##> message("2. res: '", res, "'")
                                             if (nchar(res)) res <- gsub("^,[ ]*", "", res)
                                             ##> message("3. res: '", res, "'")
                                             if (nchar(res)) res <- gsub("mu . ", "\u03BC", res)
                                             ##> message("4. res: '", res, "'")
                                             if (nchar(res)) res <- gsub("per . mil", "\u2030", res)
                                             if (nchar(res)) res <- gsub("10\\^\\(-8\\)[ ]*\\*", "10\u207B\u2078", res)
                                             ##> message("5. res: '", res, "'")
                                             if (nchar(res)) res <- gsub("\\^2", "\u00B2", res)
                                             ##> message("6. res: '", res, "'")
                                             if (nchar(res)) res <- gsub("\\^3", "\u00B3", res)
                                             ##> message("7. res: '", res, "'")
                                             ##> message("res: '", res, "'")
                                             res
                                         }))
                  names(units) <- unitsNames
                  ##> message("units:");str(units)
                  if (!is.null(threes)) {
                      rownames(threes) <- paste(dataLabel(names, units))
                      colnames(threes) <- c("Min.", "Mean", "Max.", "Dim.")
                      cat("* Statistics of data\n\n")
                      if ("dataNamesOriginal" %in% names(object@metadata)) {
                          if (is.list(object@metadata$dataNamesOriginal)) {
                              OriginalName <- unlist(lapply(names, function(n)
                                                            if (n %in% names(object@metadata$dataNamesOriginal))
                                                                object@metadata$dataNamesOriginal[[n]] else "-"))
                          } else {
                              OriginalName <- object@metadata$dataNamesOriginal
                          }
                      } else {
                          OriginalName <- NULL
                      }
                      ##print(OriginalName)
                      ## I'm not sure the following will ever happen, if we always remember
                      ## to use ctdAddColumn(), but I don't want names getting recycled, so
                      ## the next if-block prevents that.
                      if (length(OriginalName) < length(names))
                          OriginalName <- c(OriginalName, rep("-", length(names)-length(OriginalName)))
                      ##print(OriginalName)
                      OriginalName[0==nchar(OriginalName, "bytes")] <- "-"
                      if (!is.null(OriginalName)) {
                          threes <- cbind(threes, OriginalName)
                      }
                      if ("time" %in% names)
                          threes <- threes[-which("time"==names),]
                      owidth <- options('width')
                      options(width=150) # make wide to avoid line breaks
                      print(threes, quote=FALSE)
                      options(width=owidth$width)
                      cat("\n")
                  }
              }
              processingLogShow(object)
              invisible(threes)
          })



setClass("satellite", contains="oce") # both amsr and landsat stem from this


#' Plot an oce Object
#'
#' @description
#' This creates a \code{\link{pairs}} plot of the elements in the \code{data}
#' slot, if there are more than 2 elements there, or a simple xy plot if 2
#' elements, or a histogram if 1 element.
#'
#' @param x A basic \code{oce} object, i.e. one inheriting from \code{\link{oce-class}},
#' but not from any subclass of that (because these subclasses go to the subclass
#' plot methods, e.g. a \code{\link{ctd-class}} object would go to
#' \code{\link{plot,ctd-method}}.
#' @param y Ignored; only present here because S4 object for generic \code{plot}
#' need to have a second parameter before the \code{...} parameter.
#' @param ... Passed to \code{\link{hist}}, \code{\link{plot}}, or to
#" \code{\link{pairs}}, according to whichever does the plotting.
#' @examples
#' library(oce)
#' o <- new("oce")
#' o <- oceSetData(o, 'x', rnorm(10))
#' o <- oceSetData(o, 'y', rnorm(10))
#' o <- oceSetData(o, 'z', rnorm(10))
#' plot(o)
setMethod(f="plot",
          signature="oce",
          definition=function(x, y, ...) {
              n <- length(x@data)
              if (n == 1)
                  hist(x@data[[1]])
              else if (n == 2)
                  plot(x@data[[1]], x@data[[2]])
              else if (n > 2)
                  pairs(x@data, ...)
              else
                  warning("no data to plot")
          })

#' Subset an oce Object
#'
#' @description
#' This is a basic class for general oce objects.  It has specialised
#' versions for most sub-classes, e.g. \code{\link{subset,ctd-method}}
#' for \code{ctd} objects.
#'
#' @param x An oce object.
#' @param subset A logical expression indicating how to take the subset; the form depends on the sub-class.
#' @param ... Ignored.
#' @return An oce object.
#' @examples
#' library(oce)
#' data(ctd)
#' # Select just the top 10 metres (pressure less than 10 dbar)
#' top10 <- subset(ctd, pressure < 10)
#' par(mfrow=c(1, 2))
#' plotProfile(ctd)
#' plotProfile(top10)
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

#' Extract Something From an oce Object
#'
#' @description
#' The named item is sought first in
#' \code{metadata}, where an exact match to the name is required. If
#' it is not present in the \code{metadata} slot, then a partial-name
#' match is sought in the \code{data} slot. Failing both
#' tests, an exact-name match is sought in a field named
#' \code{dataNamesOriginal} in the object's \code{metadata}
#' slot, if that field exists. Failing that, \code{NULL} is returned.
#'
#' To get information on the specialized variants of this function,
#' type e.g. \code{?"[[,adv-method"} for information on extracting
#' data from an object of \code{\link{adv-class}}.
#'
#' @param x An oce object
#' @param i The item to extract.
#' @param j Optional additional information on the \code{i} item.
#' @param ... Optional additional information (ignored).
#'
#' @examples
#' data(ctd)
#' ctd[["longitude"]] # in metadata
#' head(ctd[["salinity"]]) # in data
#' data(section)
#' summary(section[["station", 1]])
setMethod(f="[[",
          signature(x="oce", i="ANY", j="ANY"),
          definition=function(x, i, j, ...) {
              if (i == "metadata") {
                  return(x@metadata)
              } else if (i == "data") {
                  return(x@data)
              } else if (i == "processingLog") {
                  return(x@processingLog)
               } else if (length(grep("Unit$", i))) {
                   ## returns a list
                   return(if ("units" %in% names(x@metadata)) x@metadata$units[[gsub("Unit$", "", i)]] else x@metadata[[i]])
              } else if (length(grep(" unit$", i))) {
                  ## returns just the unit, an expression
                  return(if ("units" %in% names(x@metadata)) x@metadata$units[[gsub(" unit$", "", i)]][[1]] else "")
              } else if (length(grep(" scale$", i))) {
                  ## returns just the scale, a character string
                  return(if ("units" %in% names(x@metadata)) as.character(x@metadata$units[[gsub(" scale$", "", i)]][[2]]) else "")
              } else if (length(grep("Flag$", i))) {
                  ## returns a list
                  return(if ("flags" %in% names(x@metadata)) x@metadata$flags[[gsub("Flag$", "", i)]] else NULL)
              } else {
                  ## Check metadata
                  if (i %in% names(x@metadata))
                      return(x@metadata[[i]])
                  index <- pmatch(i, names(x@data))
                  if (!is.na(index[1])) {
                      return(x@data[[index]])
                  } else {
                      ## some special cases
                      if (i == "sigmaTheta") {
                          return(swSigmaTheta(x))
                      } else {
                          ## Check original data names
                          if (i %in% x@metadata$dataNamesOriginal)
                              return(x@data[[which(i==x@metadata$dataNamesOriginal)[1]]])
                          ## Give up
                          return(NULL)
                      }
                  }
                  ## if (missing(j) || j != "nowarn")
                  ##     warning("there is no item named \"", i, "\" in this ", class(x), " object", call.=FALSE)
              }
          })

#' @title Replace Parts of an Oce Object
#' @param x An \code{oce} object, i.e. inheriting from \code{\link{oce-class}}.
#' @template sub_subsetTemplate
setMethod(f="[[<-",
          signature(x="oce", i="ANY", j="ANY"),
          function(x, i, j, ..., value) {
              ## FIXME: use j for e.g. times
              ## message("in base [[<-")
              ## message("i: ", as.character(i))
              ## message("value: ", paste(value, collapse=" "))
              ## metadata must match exactly but data can be partially matched
              if (i == "metadata") {
                  x@metadata <- value
              } else if (i %in% names(x@metadata)) {
                  x@metadata[[i]] <- value
              } else {
                  if (length(grep("Unit$", i))) {
                      if (!("units" %in% names(x@metadata)))
                          x@metadata$units <- list()
                      x@metadata$units[[gsub("Unit$", "", i)]] <- value
                  } else if (length(grep("Flag$", i))) {
                      if (!("flags" %in% names(x@metadata)))
                          x@metadata$flags <- list()
                      x@metadata$flags[[gsub("Flag$", "", i)]] <- value
                  } else {
                      x@data[[i]] <- value
                      ##1162 index <- pmatch(i, names(x@data))
                      ##1162 if (!is.na(index[1])) {
                      ##1162     x@data[[index]] <- value
                      ##1162 } else {
                      ##1162     warning("there is no item named \"", i, "\" in this ", class(x), " object", call.=FALSE)
                      ##1162 }
                  }
              }
              validObject(x)
              invisible(x)
          })

setValidity("oce",
            function(object) {
                slotNames <- slotNames(object)
                nslots <- length(slotNames)
                if (nslots !=3) {
                    cat("should be 3 slots, but there are", nslots, "\n")
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
                              cat("   ", names[i], ", a ", dim[1], "x", dim[2], " array with value ", d[1, 1], " at [1,1] position\n", sep="")
                          } else if (length(dim) == 3) {
                              cat("   ", names[i], ", a ", dim[1], "x", dim[2], "x", dim[3], " array with value ", d[1, 1, 1],
                                  " at [1,1,1] position\n", sep="")
                          } else {
                              cat("   ", names[i], ", an array of more than 3 dimensions\n")
                          }
                      }
                  }
              }
          })

#' @title Create a composite object by averaging across good data
#' @details
#' This only works for objects inheriting from \code{\link{amsr-class}}.
#' @param object Either a \code{\link{list}} of \link{oce-class} objects, in
#' which case this is the only argument, or a single \link{oce-class} object,
#' in which case at least one other argument (an object of the size)
#' must be supplied.
#' @param ... Ignored, if \code{object} is a list. Otherwise, one or more
#' \code{oce-class} objects of the same sub-class as the first argument.
#' @template compositeTemplate
setGeneric("composite", function(object, ...) {
           standardGeneric("composite")
         })


#' @title Create a composite object by averaging across good data stored in a list
#' @param object A \code{\link{list}} of \link{oce-class} objects. This is done
#' by calling a specialized version of the function defined in the given
#' class. In the present
#' version, the objects must inherit from \link{amsr-class}, so the
#' action is to call
#' \code{\link{composite,amsr-method}}.
#' @template compositeTemplate
setMethod("composite",
          c(object="list"),
          function(object) {
              if (length(object) < 2)
                  object
              if (inherits(object[[1]], "amsr")) {
                  do.call("composite", object)
              } else {
                  stop("In composite(list) : only AMSR objects are handled")
              }
          })



#' @title Handle flags in oce objects
#' @details
#' Each specialized variant of this function has its own defaults
#' for \code{flags} and \code{actions}.
#' @param object An object of \code{\link{oce}}.
#' @template handleFlagsTemplate
setGeneric("handleFlags", function(object, flags, actions, debug) {
           standardGeneric("handleFlags")
         })

#' Signal erroneous application to non-oce objects
#' @param object A vector, which cannot be the case for \code{oce} objects.
#' @param flags Ignored.
#' @param actions Ignored.
#' @param debug Ignored.
setMethod("handleFlags",
          c(object="vector", flags="ANY", actions="ANY", debug="ANY"),
          function(object, flags=list(), actions=list(), debug=integer()) {
              stop("handleFlags() can only be applied to objects inheriting from \"oce\"")
          })

handleFlagsInternal <- function(object, flags, actions, debug) {
    oceDebug(debug, "handleFlagsInternal() {\n", sep="", unindent=1)
    if (missing(flags)) {
        warning("no flags supplied (internal error; report to developer)")
        return(object)
    }
    if (missing(actions)) {
        warning("no actions supplied (internal error; report to developer)")
        return(object)
    }
    if (missing(debug))
        debug <- 0
    if (any(names(flags)!=names(actions)))
        stop("names of flags must match those of actions")
    oceDebug(debug, "flags=", paste(as.vector(flags), collapse=","), "\n")
    if (length(object@metadata$flags)) {
        all <- is.null(names(flags)) # "ALL" %in% names(flags)
        oceDebug(debug, "all=", all, "\n")
        if (all && length(flags) > 1)
            stop("if first flag is unnamed, no other flags can be specified")
        if (all && (length(actions) > 1 || !is.null(names(actions))))
            stop("if flags is a list of a single unnamed item, actions must be similar")
        for (name in names(object@data)) {
            flagsObject <- object@metadata$flags[[name]]
            if (!is.null(flagsObject)) {
                dataItemLength <- length(object@data[[name]])
                ##> message("name: ", name, ", flags: ", paste(object@metadata$flags[[name]], collapse=" "))
                flagsThis <- if (all) flags[[1]] else flags[[name]]
                oceDebug(debug, "flagsThis=", paste(flagsThis, collapse=","), "\n")
                oceDebug(debug, "flags[", name, "]=", paste(flagsThis, collapse=","), "\n")
                actionsThis <- if (all) actions[[1]] else actions[[name]]
                ##> message("flagsThis:");print(flagsThis)
                if (name %in% names(object@metadata$flags)) {
                    actionNeeded <- object@metadata$flags[[name]] %in% flagsThis
                    if (debug > 5)
                        print(data.frame(flagsObject=flagsObject, actionNeeded=actionNeeded))
                    if (any(actionNeeded)) {
                        oceDebug(debug, "  \"", name, "\" has ", dataItemLength, " data, of which ",
                                 100*sum(actionNeeded)/dataItemLength, "% are flagged\n", sep="")
                        if (debug > 5) {
                            message("\nactionsThis follows...")
                            print(actionsThis)
                        }
                        if (is.function(actionsThis)) {
                            object@data[[name]][actionNeeded] <- actionsThis(object)[actionNeeded]
                        } else if (is.character(actionsThis)) {
                            if (actionsThis == "NA") {
                                object@data[[name]][actionNeeded] <- NA
                            } else {
                                stop("the only permitted character action is 'NA'")
                            }
                        } else {
                            stop("action must be a character string or a function")
                        }
                    } else {
                        oceDebug(debug, "  no action needed, since no", name, " data are flagged as stated\n")
                    }
                }
            } else {
                oceDebug(debug, "\"", name, "\" is not the subject of flags\n", sep="")
            }
        }
    }
    object@processingLog <- processingLogAppend(object@processingLog,
                                                paste("handleFlags(flags=",
                                                      substitute(flags, parent.frame()),
                                                      ", actions=",
                                                      substitute(actions, parent.frame()),
                                                      ")", collapse=" ", sep=""))
    oceDebug(debug, "} # handleFlagsInternal()\n", sep="", unindent=1)
    object
}
