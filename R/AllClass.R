#' Base class for objects in the oce package
#'
#' This is mainly used within oce to create sub-classes, although
#' users can use \code{new("oce")} to create a blank \code{oce}
#' object, if desired.
#'
#' @slot metadata A list containing information about the data. The 
#' contents vary across sub-classes, e.g. an \code{\link{adp-class}}
#' object has information about beam patterns, which obviously would
#" not make sense for a \code{\link{ctd-class}} object.
#' @slot data A list containing the data.
#' @slot processingLog A list containing time-stamped processing steps,
#' typically stored in the object by oce functions.
#'
#' @seealso
#' Information on the classes that derive from this base class are found
#' at the following links:
#' \code{\link{adp-class}},
#' \code{\link{adv-class}},
#' \code{\link{argo-class}},
#' \code{\link{cm-class}},
#' \code{\link{coastline-class}},
#' \code{\link{ctd-class}},
#' \code{\link{echosounder-class}},
#' \code{\link{lisst-class}},
#' \code{\link{lobo-class}},
#' \code{\link{met-class}},
#' \code{\link{rsk-class}},
#' \code{\link{sealevel-class}},
#' \code{\link{section-class}},
#' \code{\link{tidem-class}},
#' \code{\link{topo-class}}, and
#' \code{\link{windrose-class}}.
#'
#' @examples
#' str(new("oce"))
#' @keywords classes oce
setClass("oce",
         representation(metadata="list",
                        data="list",
                        processingLog="list"),
         prototype=list(metadata=list(filename="", units=list(), flags=list()),
                        data=list(),
                        processingLog=list()))

#' Summarize an oce object
#'
#' Provide a summary of some pertinent aspects of the object, including
#' important elements of its \code{metadata} slot and \code{data} slot,
#' and a listing of the contents of its \code{processingLog} slot.
#'
#' The output is tailored to the object. This is not the way to learn
#' everything about the object; programmers, for example, will often
#' use \code{\link{str}} to get more details of the object structure.
#'
#' @param object The object to be summarized.
#' @param ... Extra arguments (ignored)
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
                      if (deltat < 60)
                          cat("* Time ranges from", format(from), "to", format(to), "with mean increment", deltat, "s\n")
                      else if (deltat < 3600)
                          cat("* Time ranges from", format(from), "to", format(to), "with mean increment", deltat/60, "min\n")
                      else if (deltat < 24*3600)
                          cat("* Time ranges from", format(from), "to", format(to), "with mean increment", deltat/3600, "hours\n")
                      else
                          cat("* Time ranges from", format(from), "to", format(to), "with mean increment", deltat/3600/24, "days\n")
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
                                             res <- as.character(res)
                                             ##> message("1. res: '", res, "'")
                                             ## Clean up notation
                                             if (nchar(res)) res <- gsub("degree[ ]+[*][ ]+C", "\u00B0C", res)
                                             if (nchar(res)) res <- gsub("degree[ ]+[*][ ]+F", "\u00B0F", res)
                                             if (nchar(res)) res <- gsub("degree[ ]+[*][ ]+E", "\u00B0E", res)
                                             if (nchar(res)) res <- gsub("degree[ ]+[*][ ]+W", "\u00B0W", res)
                                             if (nchar(res)) res <- gsub("degree[ ]+[*][ ]+N", "\u00B0N", res)
                                             if (nchar(res)) res <- gsub("degree[ ]+[*][ ]+S", "\u00B0S", res)
                                             ##> message("res: '", res, "'")
                                             if (nchar(res)) res <- gsub("degree", "\u00B0", res)
                                             ##> message("res: '", res, "'")
                                             ##> message("2. res: '", res, "'")
                                             if (nchar(res)) res <- gsub("^,[ ]*", "", res)
                                             ##> message("3. res: '", res, "'")
                                             if (nchar(res)) res <- gsub("mu . ", "\u03BC", res)
                                             ##> message("4. res: '", res, "'")
                                             if (nchar(res)) res <- gsub("per . mil", "\u2030", res)
                                             ##> message("5. res: '", res, "'")
                                             if (nchar(res)) res <- gsub("\\^2", "\u00B3", res)
                                             ##> message("6. res: '", res, "'")
                                             if (nchar(res)) res <- gsub("\\^3", "\u00B3", res)
                                             ##> message("7. res: '", res, "'")
                                             ##> message("res: '", res, "'")
                                             res
                                         }))
                  names(units) <- unitsNames
                  ##> message("units:");str(units)
                  rownames(threes) <- paste("    ", dataLabel(names[!isTime], units))
                  colnames(threes) <- c("Min.", "Mean", "Max.")
                  cat("* Statistics of data\n")
                  print(threes, indent='    ')
              }
              processingLogShow(object)
              invisible(threes)
          })

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


#' Subset an oce object
#'
#' This is a basic class for general oce objects.  It has specialised
#' versions for most sub-classes, e.g. \code{\link{subset.ctd}} will
#' be used if \code{subset} is called for an object that inherits from
#' \code{ctd}; type \code{showMethods('subset')} to see a list of objects
#' that have specialized methods, and then e.g. type \code{?subset.ctd}
#' to get help on the method for objects inheriting from the
#' \code{\link{ctd-class}}.
#'
#' @aliases subset.oce
#' @param x An oce object.
#' @param subset A logical expression indicating how to take the subset (depends on the sub-class).
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

#' Extract something from an oce object
#'
#' The named item is sought first in
#' \code{metadata}, where an exact match to the name is required. If
#' it is not present in the \code{metadata} slot, then a partial-name
#' match is sought in the \code{data} slot. 
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
              } else if (length(grep("Unit$", i))) { # returns a list
                  return(if ("units" %in% names(x@metadata)) x@metadata$units[[gsub("Unit$","",i)]] else x@metadata[[i]])
              } else if (length(grep(" unit$", i))) { # returns just the unit, an expression
                  return(if ("units" %in% names(x@metadata)) x@metadata$units[[gsub(" unit$","",i)]][[1]] else "")
              } else if (length(grep(" scale$", i))) { # returns just the scale, a character string
                  return(if ("units" %in% names(x@metadata)) as.character(x@metadata$units[[gsub(" scale$","",i)]][[2]]) else "")
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

#' Change something within an oce object
#'
#' @description
#' This is a base function that can be used to change items
#' in the \code{metadata} or \code{data} slot of an
#' object of the \code{\link{oce-class}}. See 
#' \dQuote{Details} for the case in which both slots
#' contain an item of the given name.
#'
#' The first step is to an item of the indicated name. First,
#' it is sought in the \code{metadata} slot, and if it is found
#' there, then that value is altered. If it is not found there,
#' it is sought in the \code{data} slot and modified there.
#'
#'
#' @param x An oce object.
#' @param i The item to extract.
#' @param j Optional additional information on the \code{i} item.
#' @param ... Optional additional information (ignored).
#' @param value The value to be inserted into \code{x}.
#'
#' @examples
#' data(ctd)
#' summary(ctd)
#' # Move the CTD profile a nautical mile north,
#' ctd[["latitude"]] <- 1/60 + ctd[["latitude"]] # in metadata
#' # Increase the salinity by 0.01.
#' ctd[["salinity"]] <- 0.01 + ctd[["salinity"]] # in data
#' summary(ctd)
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


