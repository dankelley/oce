#' Get the present time, in a stated timezone
#'
#' @param tz String indicating the desired timezone. The default is
#' to use UTC, which is used very commonly in oceanographic work. To
#' get the local time, use \code{tz=""} or \code{tz=NULL},
#'
#' @examples
#' presentTime() # UTC
#' presentTime("") # the local timezone
#'
#' @return A \code{\link{POSIXct}} object holding the present time, in the
#' indicated timezone.
## NOTE: we need to define this here so setClass() knows about it;
## NOTE: having it in NAMESPACE is not sufficient.
presentTime <- function(tz="UTC")
{
    t <- Sys.time()
    if (!is.null(tz) && nchar(tz) > 0)
        attr(t, "tzone") <- tz
    t
}


#' Base Class for oce Objects
#'
#' This is mainly used within oce to create sub-classes, although
#' users can use \code{new("oce")} to create a blank \code{oce}
#' object, if desired.
#'
#' @slot metadata A list containing information about the data. The
#' contents vary across sub-classes, e.g. an \code{\link{adp-class}}
#' object has information about beam patterns, which obviously would
#' not make sense for a \code{\link{ctd-class}} object In addition,
#' all classes have items named \code{units} and \code{flags}, used
#' to store information on the units of the data, and the data quality.
#' @slot data A list containing the data.
#' @slot processingLog A list containing time-stamped processing steps,
#' typically stored in the object by oce functions.
#'
#' @examples
#' str(new("oce"))
#'
#' @family classes provided by oce
setClass("oce",
         representation(metadata="list",
                        data="list",
                        processingLog="list"),
         prototype=list(metadata=list(units=list(), flags=list()),
                        data=list(),
                        processingLog=list(time=presentTime(), value="Create oce object"))
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
              metadataNames <- names(object@metadata)
              dataNames <- names(object@data)
              isTime <- grepl("^time$", dataNames) # OLD: more permissive name, but that breaks on some data
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
                  if (is.ad2cp(object)) {
                      threes <- matrix(nrow=3, ncol=3)
                      ## FIXME get burst and average separately
                      threes[1, ] <- threenum(object[["v"]])
                      threes[2, ] <- threenum(object[["a"]])
                      threes[3, ] <- threenum(object[["q"]])
                  } else {
                      threes <- matrix(nrow=ndata, ncol=3)
                      for (i in 1:ndata) {
                          threes[i, ] <- as.numeric(threenum(object@data[[i]]))
                      }
                  }
                  ##rownames(threes) <- paste("   ", dataNames[!isTime])
                  units <- if ("units" %in% metadataNames) object@metadata$units else NULL
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
                      if (is.ad2cp(object)) {
                          rownames(threes) <- c("v", "a", "q")
                      } else {
                          rownames(threes) <- paste("    ", dataLabel(dataNames, units), sep="")
                      }
                      threes <- cbind(threes,
                                      as.vector(lapply(object@data, function(x)
                                                       if (is.array(x)) paste(dim(x), collapse="x") else length(x))),
                                      as.vector(lapply(object@data, function(x) sum(is.na(x)))))
                      colnames(threes) <- c("Min.", "Mean", "Max.", "Dim.", "NAs")
                      cat("* Data Overview\n\n")
                      if ("dataNamesOriginal" %in% metadataNames) {
                          if (is.list(object@metadata$dataNamesOriginal)) {
                              OriginalName <- unlist(lapply(dataNames, function(n)
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
                      ## to use ctdAddColumn(), but I don't want dataNames getting recycled, so
                      ## the next if-block prevents that.
                      if (length(OriginalName) < length(dataNames))
                          OriginalName <- c(OriginalName, rep("-", length(dataNames)-length(OriginalName)))
                      ##print(OriginalName)
                      OriginalName[0==nchar(OriginalName, "bytes")] <- "-"
                      if (!is.null(OriginalName)) {
                          if (is.ad2cp(object)) {
                              threes <- cbind(threes, "-")
                          } else {
                              threes <- cbind(threes, OriginalName)
                          }
                      }
                      if ("time" %in% dataNames)
                          threes <- threes[-which("time" == dataNames), , drop=FALSE]
                      owidth <- options('width')
                      options(width=150) # make wide to avoid line breaks
                      #browser()
                      print(as.data.frame(threes), digits=5)
                      options(width=owidth$width)
                      cat("\n")
                  }
              }
              ## Get flags specifically from metadata; using [["flags"]] could extract
              ## it from data, if present there and not in metadata (as e.g. with
              ## the data("ctd") that is provided with oce).
              flags <- object@metadata$flags
              if (length(flags)) {
                  if (!is.null(object@metadata$flagScheme)) {
                      cat("* Data-quality Flag Scheme\n\n")
                      cat("    name    \"", object@metadata$flagScheme$name, "\"\n", sep="")
                      cat("    mapping ", gsub(" = ", "=", as.character(deparse(object@metadata$flagScheme$mapping,
                                                                                   width.cutoff=400))), "\n\n", sep="")
                  }
                  cat("* Data-quality Flags\n\n")
                  if (length(names(flags))) {
                      width <- 1 + max(nchar(names(flags)))
                      for (name in names(flags)) {
                          padding <- rep(" ", width - nchar(name))
                          cat("    ", name, ":", padding, sep="")
                          if (all(is.na(flags[[name]]))) {
                              cat("NA", length(flags[[name]]), "\n")
                          } else {
                              flagTable <- table(flags[[name]])
                              flagTableLength <- length(flagTable)
                              if (flagTableLength) {
                                  for (i in seq_len(flagTableLength)) {
                                      cat("\"", names(flagTable)[i], "\"", " ", flagTable[i], "", sep="")
                                      if (i != flagTableLength) cat(", ") else cat("\n")
                                  }
                              }
                          }
                      }
                  } else {
                      flagTable <- table(flags)
                      flagTableLength <- length(flagTable)
                      if (flagTableLength > 0) {
                          if (flagTableLength) {
                              cat("    ")
                              for (i in seq_len(flagTableLength)) {
                                  cat("\"", names(flagTable)[i], "\"", " ", flagTable[i], "", sep="")
                                  if (i != flagTableLength) cat(", ") else cat("\n")
                              }
                          }
                      }
                  }
                  cat("\n")
              }
              processingLogShow(object)
              invisible()
          })



setClass("satellite", contains="oce") # both amsr and landsat stem from this


#' Plot an oce Object
#'
#' @description
#' This creates a \code{\link{pairs}} plot of the elements in the \code{data}
#' slot, if there are more than 2 elements there, or a simple xy plot if 2
#' elements, or a histogram if 1 element.
#'
#' @param x An object of \link{oce-class},
#' but not from any subclass of that (because these subclasses go to the subclass
#' plot methods, e.g. a \code{\link{ctd-class}} object would go to
#' \code{\link{plot,ctd-method}}.
#' @param y Ignored; only present here because S4 object for generic \code{plot}
#' need to have a second parameter before the \code{...} parameter.
#' @param ... Passed to \code{\link{hist}}, \code{\link{plot}}, or to
#' \code{\link{pairs}}, according to whichever does the plotting.
#' @examples
#' library(oce)
#' o <- new("oce")
#' o <- oceSetData(o, 'x', rnorm(10))
#' o <- oceSetData(o, 'y', rnorm(10))
#' o <- oceSetData(o, 'z', rnorm(10))
#' plot(o)
#' @aliases plot.oce
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
#' @param x an oce object
#' @param subset a logical expression indicating how to take the subset; the form depends on the sub-class.
#' @param ... optional arguments, used in some specialized methods (e.g. \code{\link{subset,section-method}}).
#' @return An oce object.
#' @examples
#' library(oce)
#' data(ctd)
#' # Select just the top 10 metres (pressure less than 10 dbar)
#' top10 <- subset(ctd, pressure < 10)
#' par(mfrow=c(1, 2))
#' plotProfile(ctd)
#' plotProfile(top10)
#' @family functions that subset oce objects
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
              for (i in seq_along(x@metadata$flags))
                  res@metadata$flags[[i]] <- res@metadata$flag[[i]][keep]
              res@processingLog <- processingLogAppend(res@processingLog,
                                                        paste(deparse(match.call(call=sys.call(sys.parent(1)))),
                                                              sep="", collapse=""))
              res
          })

#' Extract Something From an oce Object
#'
## @description
## The named item is sought first in
## \code{metadata}, where an exact match to the name is required. If
## it is not present in the \code{metadata} slot, then a partial-name
## match is sought in the \code{data} slot. Failing both
## tests, an exact-name match is sought in a field named
## \code{dataNamesOriginal} in the object's \code{metadata}
## slot, if that field exists. Failing that, \code{NULL} is returned.
##
## The full contents of the \code{metadata} slot of an object named
## \code{x} are returned with \code{x[["metadata"]]}, and
## \code{x[["data"]]} does the same thing for the data slot.
## Even if the full contents are not needed, this
## scheme can be useful in circumventing the searching scheme described
## in the previous paragraph, e.g. \code{x[["data"]]$longitude}
## might be used to select longitude from the data slot of \code{x},
## as an alternative to \code{\link{oceGetData}(x,"longitude")}.
##
## To get information on the specialized variants of this function,
## type e.g. \code{?"[[,adv-method"} for information on extracting
## data from an object of \code{\link{adv-class}}.
#'
#' @template sub_subTemplate
#'
#' @param x An oce object
## @param i The item to extract.
## @param j Optional additional information on the \code{i} item.
## @param ... Optional additional information (ignored).
#'
#' @seealso
#' Many \code{oce} object classes have specialized versions
#' of \code{[[} that handle the details in specialized way.
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
              } else if (i == "sigmaTheta") {
                  return(swSigmaTheta(x))
              } else if (i == "sigma0") {
                  return(swSigma0(x))
              } else if (i == "spice") {
                  return(swSpice(x))
              } else {
                  if (missing(j) || j == "") {
                      ## Since 'j' is not provided, we must search for 'i'. We look first
                      ## in the metadata slot, but if it's not there, we look in the
                      ## data slot. In the 'data' case, we also permit partial-match names,
                      ## as well as non-partial matching to the original names, as
                      ## contained in a data file.
                      if (i %in% names(x@metadata))
                          return(x@metadata[[i]])
                      ## partial match allowed in data, but not in original-name of data
                      index <- pmatch(i, names(x@data))
                      if (!is.na(index[1])) {
                          return(x@data[[index]])
                      } else if (i %in% x@metadata$dataNamesOriginal) {
                          return(x@data[[which(i==x@metadata$dataNamesOriginal)[1]]])
                      } else {
                          return(NULL)
                      }
                  } else {
                      ## New in 2019-May-17: 'j' can be "data" or "metadata"
                      ## https://github.com/dankelley/oce/issues/1554
                      if (j == "metadata") {
                          return(x@metadata[[i]])
                      } else if (j == "data") {
                          ## partial match allowed in data, but not in original-name of data
                          index <- pmatch(i, names(x@data))
                          if (!is.na(index[1])) {
                              return(x@data[[i]])
                          } else if (i %in% x@metadata$dataNamesOriginal) {
                              return(x@data[[which(i==x@metadata$dataNamesOriginal)[1]]])
                          } else {
                              return(NULL)
                          }
                      } else {
                          stop("object[[\"", i, "\", \"", j, "\"]]: second arg must be \"data\" or \"metadata\"", call.=FALSE)
                      }
                  }
              }
          })


#' @title Replace Parts of an Oce Object
#' @param x An object of \link{oce-class}.
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
              dataNames <- names(object@data)
              ncol <- length(dataNames)
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
              for (i in seq_along(dataNames)) {
                  d <- object@data[[i]]
                  if (0 == length(d)) {
                      cat("  ", dataNames[i], ": empty\n")
                  } else {
                      if (inherits(d, "POSIXt")) {
                          cat(vectorShow(d, paste("  ", dataNames[i])))
                      } else if (is.vector(d)) {
                          cat(vectorShow(d, paste("  ", dataNames[i])))
                      } else if (is.array(d)) {
                          dim <- dim(object@data[[i]])
                          if (length(dim) == 1) {
                              cat(vectorShow(d, paste("  ", dataNames[i])))
                          } else if (length(dim) == 2) {
                              cat("   ", dataNames[i], ", a ", dim[1], "x", dim[2], " array with value ", d[1, 1], " at [1,1] position\n", sep="")
                          } else if (length(dim) == 3) {
                              cat("   ", dataNames[i], ", a ", dim[1], "x", dim[2], "x", dim[3], " array with value ", d[1, 1, 1],
                                  " at [1,1,1] position\n", sep="")
                          } else {
                              cat("   ", dataNames[i], ", an array of more than 3 dimensions\n")
                          }
                      } else if (is.data.frame(d)) {
                          cat("  ", dataNames[i], ", a data frame with contents:\n", sep="")
                          for (n in names(d)) {
                              #cat("    ", n, " (with ", length(d[[n]]), " elements)\n", sep="")
                              cat("    ", vectorShow(d[[n]], n), sep="")
                          }
                      }
                  }
              }
          })

#' @title Create a composite object by averaging across good data
#' @param object Either a \code{\link{list}} of \link{oce-class} objects, in
#' which case this is the only argument, or a single \link{oce-class} object,
#' in which case at least one other argument (an object of the same size)
#' must be supplied.
#' @param ... Ignored, if \code{object} is a list. Otherwise, one or more
#' \link{oce-class} objects of the same sub-class as the first argument.
#' @template compositeTemplate
setGeneric("composite",
           function(object, ...) {
               standardGeneric("composite")
          })


#' Composite by Averaging Across Data
#'
#' This is done
#' by calling a specialized version of the function defined in the given
#' class. In the present
#' version, the objects must inherit from \link{amsr-class}, so the
#' action is to call
#' \code{\link{composite,amsr-method}}.
#' @param object A \code{\link{list}} of \link{oce-class} objects.
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
#'
#' @details
#' Each specialized variant of this function has its own defaults
#' for \code{flags} and \code{actions}.
#' @param object An object of \link{oce-class}.
#' @template handleFlagsTemplate
setGeneric("handleFlags", function(object, flags=NULL, actions=NULL, where=NULL, debug=getOption("oceDebug")) {
           standardGeneric("handleFlags")
         })

#' Signal erroneous application to non-oce objects
#' @param object A vector, which cannot be the case for \code{oce} objects.
#' @param flags Ignored.
#' @param actions Ignored.
#' @param where Ignored.
#' @param debug Ignored.
setMethod("handleFlags", signature=c(object="vector", flags="ANY", actions="ANY", where="ANY", debug="ANY"),
          definition=function(object, flags=list(), actions=list(), where=list(), debug=getOption("oceDebug")) {
              stop("handleFlags() can only be applied to objects inheriting from \"oce\"")
          })

#' Handle flags in oce objects
#'
#' @details
#' Base-level handling of flags.
#' @param object An object of \link{oce-class}.
#
#' @template handleFlagsTemplate
setMethod("handleFlags", signature=c(object="oce", flags="ANY", actions="ANY", where="ANY", debug="ANY"),
          definition=function(object, flags=NULL, actions=NULL, where=NULL, debug=getOption("oceDebug")) {
              ## DEVELOPER 1: alter the next comment to explain your setup
              if (is.null(flags)) {
                  flags <- defaultFlags(object)
                  if (is.null(flags))
                      stop("must supply 'flags', or use initializeFlagScheme() on the ctd object first")
              }
              if (is.null(actions)) {
                  actions <- list("NA") # DEVELOPER 3: alter this line to suit a new data class
                  names(actions) <- names(flags)
              }
              if (any(names(actions)!=names(flags)))
                  stop("names of flags and actions must match")
              handleFlagsInternal(object=object, flags=flags, actions=actions, where=where, debug=debug)
          })


## this doc uses markdown format (see https://cran.r-project.org/web/packages/roxygen2/vignettes/markdown.html)
#' Low-level function for handling data-quality flags
#'
#' This function is designed for internal use within the `oce` package.  Its
#' purpose is to carry out low-level processing relating to data-quality flags,
#' as a support for higher-level functions such [handleFlags,ctd-method] for
#' `ctd` objects, [handleFlags,adp-method] for `adp` objects,
#' etc.
#'
#' @param object An object of [oce-class].
#'
#' @param flags A named [list] of numeric values.
#'
#' @param actions A character vector indicating actions to be carried out for the corresponding
#' `flags` values. This will be lengthened with [rep()] if necessary, to be
#' of the same length as `flags`. A common value for `actions` is `"NA"`, which
#' means that data values that are flagged are replaced by `NA` in the returned result.
#'
#' @param where An optional string that permits the function to work with
#' objects that store flags in e.g. `object@metadata$flags$where`
#' instead of in `object@metadata$flags`, and data within
#' `object@data$where` instead of within `object@data`. The
#' appropriate value for `where` within the oce package is
#' the default, `NULL`, which means that this extra subdirectory
#' is not being used.
#'
#' @param debug An integer indicating the degree of debugging requested, with value `0`
#' meaning to act silently, and value `1` meaning to print some information about the
#' steps in processing.
#'
#' @return A copy of `object`, possibly with modifications to its
#' \code{data} slot, if `object` contains flag values that have
#' actions that alter the data.
#' @md
handleFlagsInternal <- function(object, flags, actions, where, debug=0) {
    oceDebug(debug, "handleFlagsInternal() {\n", sep="", unindent=1)
    if (debug) {
        cat("flags=c(", paste(flags, collapse=","), ")\n", sep="")
        cat("actions=c(", paste(actions, collapse=","), ")\n", sep="")
        cat("where='", where, "'\n", sep="")
    }
    if (missing(flags)) {
        warning("no flags supplied (internal error; report to developer)")
        return(object)
    }
    ## Permit e.g. flags=c(1,3)
    if (!is.list(flags))
        flags <- list(flags)
    if (missing(actions)) {
        warning("no actions supplied (internal error; report to developer)")
        return(object)
    }
    if (missing(where))
        where <- NULL
    if (any(names(flags) != names(actions)))
        stop("names of flags must match those of actions")
    oceDebug(debug, "flags=", paste(as.vector(flags), collapse=","), "\n", sep="")
    oflags <- if (is.null(where)) object@metadata$flags else object@metadata$flags[[where]]
    odata <- if (is.null(where)) object@data else object@data[[where]]
    if (length(oflags)) {
        singleFlag <- is.null(names(oflags)) # TRUE if there is just one flag for all data fields
        oceDebug(debug, "singleFlag=", singleFlag, "\n", sep="")
        if (singleFlag && (length(actions) > 1 || !is.null(names(actions))))
            stop("if flags is a list of a single unnamed item, actions must be similar")
        oceDebug(debug, "names(odata)=c(\"", paste(names(odata),collapse="\", \""), "\")\n", sep="")
        if (singleFlag) {
            ## apply the same flag to *all* data.
            actionsThis <- actions[[1]] # FIXME: this seems wrong
            oflags <- as.vector(oflags) # FIXME: should we make this a list?
            oceDebug(debug, "singleFlag==TRUE case ... head(oflags)=c(", paste(head(oflags), collapse=","), ") (will be used for *all* data types)\n", sep="")
            for (name in names(odata)) {
                oceDebug(debug, "handling flags for '", name, "'\n", sep="")
                dataItemLength <- length(odata[[name]])
                oceDebug(debug, "  initially, ", sum(is.na(odata[[name]])), " out of ", dataItemLength, " are NA\n", sep="")
                actionNeeded <- oflags %in% if (length(names(flags))) flags[[name]] else flags[[1]]
                ## if (name == "salinity") browser()
                if (is.function(actionsThis)) {
                    oceDebug(debug>1, "  actionsThis is a function\n")
                    odata[[name]][actionNeeded] <- actionsThis(object)[actionNeeded]
                } else if (is.character(actionsThis)) {
                    oceDebug(debug>1, "  actionsThis is a string, '", actionsThis, "'\n", sep="")
                    oceDebug(debug>1, "  actionNeeded=c(", paste(actionNeeded, collapse=","), ")\n", sep="")
                    if (actionsThis == "NA") {
                        odata[[name]][actionNeeded] <- NA
                    } else {
                        stop("the only permitted character action is 'NA'")
                    }
                } else {
                    stop("action must be a character string or a function")
                }
                oceDebug(debug, "  after handling flags, ", sum(is.na(odata[[name]])),
                         " out of ", length(odata[[name]]), " are NA\n", sep="")
            }
            oceDebug(debug, "done handling flags for all data in object\n")
        } else { ## multiple flags: Apply individual flags to corresponding data fields
            for (name in names(odata)) {
                oceDebug(debug, "handling flags for '", name, "'\n", sep="")
                oceDebug(debug, "  initially, ", sum(is.na(odata[[name]])),
                         " out of ", length(odata[[name]]), " are NA\n", sep="")
                flagsObject <- oflags[[name]]
                if (debug > 1) {
                    cat("  unique(flagsObject) for ", name, ":\n")
                    print(table(flagsObject))
                }
                if (!is.null(flagsObject)) {
                    dataItemLength <- length(odata[[name]])
                    flagsThis <- oflags[[name]]
                    ##oceDebug(debug, "before converting to numbers, flagsThis=", paste(flagsThis, collapse=","), "\n")
                    if (name %in% names(oflags)) {
                        actionsThis <- if (length(names(actions))) actions[[name]] else actions[[1]]
                        oceDebug(debug>1, "  actionsThis: \"", paste(actionsThis, collapse=","), "\"\n", sep="")
                        actionNeeded <- oflags[[name]] %in% if (length(names(flags))) flags[[name]] else flags[[1]]
                        oceDebug(debug>1, "  actionNeeded=c(", paste(actionNeeded, collapse=","), ")\n", sep="")
                        if (any(actionNeeded)) {
                            ## oceDebug(debug, "\"", name, "\" has ", dataItemLength, " data, of which ",
                            ##          sum(actionNeeded), " are flagged\n", sep="")
                            if (debug > 1) {
                                cat("  actionsThis follows...\n")
                                print(actionsThis)
                            }
                            if (is.function(actionsThis)) {
                                odata[[name]][actionNeeded] <- actionsThis(object)[actionNeeded]
                            } else if (is.character(actionsThis)) {
                                if (actionsThis == "NA") {
                                    odata[[name]][actionNeeded] <- NA
                                } else {
                                    stop("the only permitted character action is 'NA'")
                                }
                            } else {
                                stop("action must be a character string or a function")
                            }
                        } else {
                            oceDebug(debug, "  no action needed, since no \"", name, "\" data are flagged as stated\n", sep="")
                        }
                    }
                }
                oceDebug(debug, "  finally, ", sum(is.na(odata[[name]])),
                         " out of ", length(odata[[name]]), " are NA\n", sep="")
            }
        }                              # multiple flags
    } else {
        oceDebug(debug, "object has no flags in metadata\n")
    }
    if (is.null(where)) {
        object@data <- odata
    } else {
        object@data[[where]] <- odata
    }
    object@processingLog <- processingLogAppend(object@processingLog,
                                                paste("handleFlagsInternal(flags=c(",
                                                      paste(substitute(flags, parent.frame()), collapse=","),
                                                      "), actions=c(",
                                                      paste(substitute(actions, parent.frame()), collapse=","),
                                                      "))", collapse=" ", sep=""))
    oceDebug(debug, "} # handleFlagsInternal()\n", sep="", unindent=1)
    object
}


#' Suggest a default flag vector for bad or suspicious data
#'
#' \code{defaultFlags} tries to suggest a reasonable default \code{flag} scheme
#' for use by \code{\link{handleFlags}}. It does this by looking for an item
#' named \code{flagScheme} in the \code{metadata} slot of \code{object}.
#' If \code{flagScheme} is found, and if the scheme is recognized, then a numeric
#' vector is returned that indicates bad or questionable data. If
#' \code{flagScheme$default} exists, then that scheme is returned. However,
#' if that does not exist, and if \code{flagScheme$name} is recognized,
#' then a pre-defined (very conservative) scheme is used,
#' as listed below.
#'
#'\itemize{
#'
#' \item for \code{argo}, the default is
#' \code{c(0,2,3,4,7,8,9)}, i.e. all flags except \code{passed_all_tests}.
#'
#' \item for \code{BODC}, the default is
#' \code{c(0,2,3,4,5,6,7,8,9)}, i.e. all flags except \code{good}.
#'
#' \item for \code{DFO}, the default is
#' \code{c(0,2,3,4,5,8,9)}, i.e. all flags except \code{appears_correct}.
#'
#' \item for \code{WHP bottle}, the default is
#' \code{c(1,3,4,5,6,7,8,9)}, i.e. all flags except \code{no_problems_noted}.
#'
#' \item for \code{WHP ctd}, the default is
#' \code{c(1,3,4,5,6,7,9)}, i.e. all flags except \code{acceptable}.
#'
#'}
#'
#' @param object An oce object
#'
#' @return A vector of one or more flag values, or \code{NULL} if \code{object}
#' \code{metadata} slot lacks a \code{flagScheme} (as set by \code{\link{initializeFlagScheme}}),
#' or if it has a scheme that is not in the list provide in \dQuote{Description}.
#'
#' @family functions relating to data-quality flags
defaultFlags <- function(object)
{
    if (is.null(object@metadata$flagScheme))
        return(NULL)
    default <- object@metadata$flagScheme$default
    if (!is.null(default))
        return(default)
    scheme <- object@metadata$flagScheme$name
    if (is.null(scheme))
        return(NULL)
    if (scheme == "argo")
        return(c(0, 2, 3, 4, 7, 8, 9)) # retain passed_all_tests
    if (scheme == "BODC")
        return(c(0, 2, 3, 4, 5, 6, 7, 8, 9)) # retain good
    if (scheme == "DFO")
        return(c(0, 2, 3, 4, 5, 8, 9)) # retain appears_correct
    if (scheme == "WHP bottle")
        return(c(1, 3, 4, 5, 6, 7, 8, 9)) # retain no_problems_noted
    if (scheme == "WHP ctd")
        return(c(1, 3, 4, 5, 6, 7, 9)) # retain acceptable
    warning("unable to determine default flags from 'flagScheme' in the object 'metadata' slot\n")
    return(NULL)
}


#' @templateVar class oce
#' @templateVar note This generic function is overridden by specialized functions for some object classes.
#' @template setFlagsTemplate
setGeneric("setFlags", function(object, name=NULL, i=NULL, value=NULL, debug=0) {
           standardGeneric("setFlags")
         })


#' @templateVar class oce
#' @templateVar note This generic function is overridden by specialized functions for some object classes.
#' @template setFlagsTemplate
setMethod("setFlags",
          signature=c(object="oce", name="ANY", i="ANY", value="ANY", debug="ANY"),
          definition=function(object, name=NULL, i=NULL, value=NULL, debug=getOption("oceDebug")) {
              setFlagsInternal(object, name, i, value, debug)
          })

setFlagsInternal <- function(object, name=NULL, i=NULL, value=NULL, debug=getOption("oceDebug"))
{
    oceDebug(debug, "setFlagsInternal(object, name='", name, "', value=", value,
             ", i=", paste(i, collapse=" "), ", debug=", debug, ") {\n", sep="",
             unindent=1)
    res <- object
    ## Ensure proper argument setup.
    if (is.null(name))
        stop("must supply a name")
    if (is.null(i))
        stop("must supply 'i'")
    if (is.null(value))
        stop("must supply 'value'")
    if (length(name) > 1)
        stop("must specify one 'name' at a time (this restriction may be relaxed in the future)")
    if (!(name %in% names(object@metadata$flags)))
        stop("object has no flag for \"", name, "\"; try one of: \"", paste(names(object@data), collapse=" "), "\"")
    ## Done with argument analysis.

    ## Permit 'value' to be a character string, if a scheme already
    ## exists and 'value' is one of the stated flag names.
    valueOrig <- value
    if (is.character(value)) {
        if (is.null(res@metadata$flagScheme)) {
            stop("cannot have character 'value' because initializeFlagScheme() has not been called on object")
        } else {
            if (value %in% names(res@metadata$flagScheme$mapping))
                value <- res@metadata$flagScheme$mapping[[value]]
            else
                stop("value=\"", value, "\" is not defined in the object's flagScheme; try one of: \"",
                     paste(names(res@metadata$flagScheme$mapping), "\", \""), "\"", sep="")
        }
    }
    ## Finally, apply the value
    if (is.vector(object@data[[name]])) {
        oceDebug(debug, name, " is a vector\n")
        res@metadata$flags[[name]][i] <- value
    } else if (is.array(object@data[[name]])) {
        dimData <- dim(object@data[[name]])
        if (is.array(i)) {
            if (!is.logical(i))
                stop("array 'i' must be logical")
            if (!identical(dim(i), dimData))
                stop("dim(i) is ", paste(dim(i), collapse="x"), " but need ",
                     paste(dimData, collapse="x"), " to match '", name, "'")
            res@metadata$flags[[name]][i] <- value
        } else if (is.data.frame(i)) {
            if (ncol(i) != length(dimData))
                stop("data frame 'i' must have ", length(dimData), " columns to match shape of '", name, "'")
            for (j in seq_len(nrow(i)))
                res@metadata$flags[[name]][i[j,1], i[j,2], i[j,3]] <- value
        } else {
            stop("'i' must be a matrix or a data frame")
        }
    } else{
        stop("only works for vectors and arrays (please report this as an error)")
    }
    res@processingLog <- processingLogAppend(res@processingLog,
                                             paste("setFlags(object, \"", name, "\", i, value=", valueOrig,
                                             ")", collapse=""))
    oceDebug(debug, "} # setFlagsInternal \n", unindent=1)
    res
}

#' @templateVar class oce
#' @template initializeFlagsTemplate
setGeneric("initializeFlags", function(object, name=NULL, value=NULL, debug=0) {
           standardGeneric("initializeFlags")
         })

#' @templateVar class oce
#' @template initializeFlagsTemplate
setMethod("initializeFlags",
          signature=c(object="oce", name="ANY", value="ANY", debug="ANY"),
          definition=function(object, name, value, debug=getOption("oceDebug")) {
              initializeFlagsInternal(object, name, value, debug)
          })

#' @templateVar class oce
#' @templateVar details This is a low-level internal function used by user-accessible functions.
#' @template initializeFlagsTemplate
initializeFlagsInternal <- function(object, name=NULL, value=NULL, debug=getOption("oceDebug"))
{
    oceDebug(debug, "initializeFlagsInternal(object, name=\"", name, "\", value, debug=", debug, ") {", sep="", unindent=1)
    res <- object
    if (is.null(name))
        stop("must supply name")
    if (is.null(value))
        stop("must supply value")
    valueOrig <- value
    if (!is.null(object@metadata$flags[[name]])) {
        warning("cannot re-initialize flags; use setFlags() to alter values")
    } else {
        ##. if (is.character(value)) {
        ##.     if (is.null(object@metadata$flagScheme))
        ##.         stop("cannot use character value because object has no flagScheme in its metadata")
        ##.     if (!(value %in% names(object@metadata$flagScheme$mapping)))
        ##.         stop("\"", value, "\" is not in the object's flagScheme; try one of: \"",
        ##.              paste(names(object@metadata$flagScheme$mapping), collapse="\", \""),
        ##.              "\"")
        ##.     value <- object@metadata$flagScheme$mapping[[value]]
        ##. }
        if (!(name %in% names(object@data)))
            stop("name=\"", name, "\" is not in the data slot of object; try one of: \"",
                 paste(name(object@data), collapse="\", \""), "\"")
        ## Flag is set up with dimensions matching data
        if (is.vector(object@data[[name]])) {
            oceDebug(debug, name, " is a vector\n")
            res@metadata$flags[[name]] <- rep(value, length(object@data[[name]]))
        } else if (is.array(object@data[[name]])) {
            dimData <- dim(object@data[[name]])
            res@metadata$flags[[name]] <- array(value, dim=dimData)
        } else{
            stop("only works for vectors and arrays (please report this as an error)")
        }
        res@processingLog <- processingLogAppend(res@processingLog,
                                                 paste("initializeFlags(object, name=\"",
                                                   name, "\", value=", valueOrig, ", debug)", sep=""))
    }
    oceDebug(debug, "} # initializeFlagsInternal", sep="", unindent=1)
    res
}


#' @templateVar class oce
#' @templateVar details There are no pre-defined \code{scheme}s for this object class.
#' @template initializeFlagSchemeTemplate
setGeneric("initializeFlagScheme", function(object, name=NULL, mapping=NULL, default=NULL, debug=0) {
           standardGeneric("initializeFlagScheme")
         })

#' @templateVar class oce
#' @templateVar details There are no pre-defined \code{scheme}s for this object class.
#' @template initializeFlagSchemeTemplate
setMethod("initializeFlagScheme",
          signature=c(object="oce", name="ANY", mapping="ANY", default="ANY", debug="ANY"),
          definition=function(object, name, mapping, default, debug) {
              initializeFlagSchemeInternal(object, name, mapping, default, debug)
          })

#' @templateVar class oce
#' @templateVar details This is a low-level internal function used by user-accessible functions.
#' @template initializeFlagSchemeTemplate
initializeFlagSchemeInternal <- function(object, name=NULL, mapping=NULL, default=NULL, debug=0)
{
    oceDebug(debug, "initializeFlagSchemeInternal(object, name=\"", name, "\", debug=", debug, ") {", sep="", unindent=1)
    if (is.null(name))
        stop("must supply 'name'")
    res <- object
    if (!is.null(object@metadata$flagScheme)) {
        warning("cannot alter a flagScheme that is already is place")
    } else {
        ## DEVELOPER NOTE: keep in synch with tests/testthat/test_flags.R and man-roxygen/initializeFlagScheme.R
        predefined <- c("argo", "BODC", "DFO", "WHP bottle", "WHP CTD")
        if (name %in% predefined) {
            if (!is.null(mapping))
                stop("cannot redefine the mapping for existing scheme named \"", name, "\"")
            if (name == "argo") {
                mapping <- list(not_assessed=0, passed_all_tests=1, probably_good=2,
                                probably_bad=3, bad=4, averaged=7,
                                interpolated=8, missing=9)
                if (is.null(default))
                    default <- c(0, 2, 3, 4, 7, 8, 9) # retain passed_all_tests
            } else  if (name == "BODC") {
                mapping <- list(no_quality_control=0, good=1, probably_good=2,
                                probably_bad=3, bad=4, changed=5, below_detection=6,
                                in_excess=7, interpolated=8, missing=9)
                if (is.null(default))
                    default <- c(0, 2, 3, 4, 5, 6, 7, 8, 9) # retain good
            } else  if (name == "DFO") {
                mapping <- list(no_quality_control=0, appears_correct=1, appears_inconsistent=2,
                                doubtful=3, erroneous=4, changed=5,
                                qc_by_originator=8, missing=9)
                if (is.null(default))
                    default <- c(0, 2, 3, 4, 5, 8, 9) # retain appears_correct
            } else if (name == "WHP bottle") {
                mapping <- list(no_information=1, no_problems_noted=2, leaking=3,
                                did_not_trip=4, not_reported=5, discrepency=6,
                                unknown_problem=7, did_not_trip=8, no_sample=9)
                if (is.null(default))
                    default <- c(1, 3, 4, 5, 6, 7, 8, 9) # retain no_problems_noted
            } else if (name == "WHP CTD") {
                mapping <- list(not_calibrated=1, acceptable=2, questionable=3,
                                bad=4, not_reported=5, interpolated=6,
                                despiked=7, missing=9)
                if (is.null(default))
                    default <- c(1, 3, 4, 5, 6, 7, 9) # retain acceptable
            } else {
                stop("internal coding error in initializeFlagSchemeInternal(); please report to developer")
            }
        } else {
            if (is.null(mapping))
                stop("must supply 'mapping' for new scheme named \"", name, "\"")
        }
        res@metadata$flagScheme <- list(name=name, mapping=mapping)
    }
    res@processingLog <- processingLogAppend(res@processingLog,
                                             paste("initializeFlagScheme(object, name=\"", name,
                                                   "\", mapping=",
                                                   gsub(" ", "", paste(as.character(deparse(mapping)),
                                                                                     sep="", collapse="")),
                                                   ")",
                                                   ", default=c(", paste(default, collapse=","), ")",
                                                   sep=""))
    oceDebug(debug, "} # initializeFlagSchemeInternal", sep="", unindent=1)
    res
}

#' Concatenate oce objects
#' @param object An object of \link{oce-class}.
#' @param ... Optional additional objects of \link{oce-class}.
#' @return An object of class corresponding to that of \code{object}.
#' @family functions that concatenate oce objects.
setGeneric("concatenate",
           function(object, ...) {
               standardGeneric("concatenate")
           })

#' Concatenate oce objects
#'
#' @templateVar class oce
#'
#' @template concatenateTemplate
setMethod("concatenate",
          signature="oce",
          definition=function(object, ...) {
              dots <- list(...)
              ndots <- length(dots)
              if (0 == ndots)
                  return(object)
              ##? ## Handle the case of first argument being a list (all other arguments
              ##? ## then being ignored).
              ##? if (is.list(dots[[1]])) {
              ##?     dots <- dots[[1]]
              ##?     ndots <- length(dots)
              ##? }
              ## Insist everything be an oce object.
              for (i in seq_len(ndots))
                  if (!inherits(dots[[i]], "oce"))
                      stop("concatenate() argument ", i+1, " does not inherit from \"oce\"")

              ## Concatenate the data (and flags, if there are such).
              res <- object
              n1 <- sort(names(res@data))
              f1 <- if ("flags" %in% names(object@metadata) && length(object@metadata$flags))
                  sort(names(object@metadata$flags)) else NULL
              for (i in 1:ndots) {
                  ## Data.
                  ni <- sort(names(dots[[i]]@data))
                  if (!identical(n1, ni))
                      stop("data name mismatch between argument 1 (",
                           paste(n1, collapse=" "), ") and argument ", i,
                           "(", paste(ni, collapse=" "), ")")
                  data <- dots[[i]]@data
                  for (n in ni) {
                      if (is.vector(dots[[1]]@data[[n]]) || n == "time" || is.factor(n)) {
                          res@data[[n]] <- c(res@data[[n]], data[[n]])
                      } else if (is.matrix(data[[n]])) {
                          res@data[[n]] <- rbind(res@data[[n]], data[[n]])
                      } else if (is.array(data[[n]])) {
                          ## construct a larger temporary array, fill in by 3rd index, then put in res
                          dim <- dim(res@data[[n]])
                          tmp <- array(object@data[[n]][1,1,1],
                                       dim=c(dim[1]+dim(data[[n]])[1], dim[2], dim[3]))
                          for (k in seq_len(dim[3])) {
                              tmp[,,k] <- rbind(res@data[[n]][,,k], data[[n]][,,k])
                          }
                          res@data[[n]] <- tmp
                      }
                  }
                  ## Fix up dimensionality
                  for (n in ni) {
                      if (is.array(dots[[1]]@data[[n]])) {
                          ##len <- length(res@data[[n]])
                          dim <- dim(dots[[1]]@data[[n]])
                          ndim <- length(dim)
                          denom <- if (ndim == 2) dim[2] else if (ndim == 3) dim[2] * dim[3]
                          dim[1] <- length(res@data[[n]]) / denom
                          ##message("dim=", paste(dim, collapse=" "))
                          dim(res@data[[n]]) <- dim
                      }
                  }
                  ## Flags.
                  if (!is.null(f1)) {
                      metadata <- dots[[i]]@metadata
                      fi <- sort(names(dots[[i]]@metadata$flags))
                      if (!identical(f1, fi))
                          stop("flag mismatch between argument 1 (",
                               paste(f1, collapse=" "), ") and argument ", i,
                               "(", paste(fi, collapse=" "), ")")
                      for (f in fi) {
                          res@metadata$flags[[f]] <- c(res@metadata$flags[[f]], metadata$flags[[f]])
                      }
                  }
              }
              ## for reasons unknown to me, the tzone gets localized
              attr(res@data$time, "tzone") <- attr(object@data$time, "tzone")
              res
          })

#' Concatenate a list of oce objects
#' @param object A list holding objects of \link{oce-class}.
#' @return An object of class corresponding to that in \code{object}.
#' @family functions that concatenate oce objects.
setMethod("concatenate",
          c(object="list"),
          function(object) {
              do.call("concatenate", list(object[[1]], object[[2:length(object)]]))
          })

