# vim:textwidth=80:expandtab:shiftwidth=4:softtabstop=4

#' Get the Present Time, in a Stated Timezone
#'
#' @param tz String indicating the desired timezone. The default is
#' to use UTC, which is used commonly in oceanographic work. To
#' get the local time, use `tz=""` or `tz=NULL`, as in \dQuote{Examples}.
#'
#' @examples
#' presentTime() # UTC
#' presentTime("") # the local timezone
#'
#' @return A [POSIXct()]-style object holding the present time, in the
#' indicated timezone.
# NOTE: we need to define this here so [setClass()] knows about it;
# NOTE: having it in NAMESPACE is not sufficient.
presentTime <- function(tz = "UTC") {
    t <- Sys.time()
    if (!is.null(tz) && nchar(tz) > 0) {
        attr(t, "tzone") <- tz
    }
    t
}

#' Base Class for oce Objects
#'
#' This is mainly used within oce to create sub-classes, although
#' users may wish to mek direct calls to `new("oce")` for
#' their own purposes.
#'
#' @slot metadata A list containing information about the data. The
#' contents vary across sub-classes, e.g. an [adp-class]
#' object has information about beam patterns, which obviously would
#' not make sense for a [ctd-class] object In addition,
#' all classes have items named `units` and `flags`, used
#' to store information on the units of the data, and the data quality.
#'
#' @slot data A list containing the data.
#'
#' @slot processingLog A list containing time-stamped processing steps,
#' typically stored in the object by oce functions.
#'
#' @examples
#' str(new("oce"))
#'
#' @family classes provided by oce
setClass("oce", slots = c(metadata = "list", data = "list", processingLog = "list"))

setMethod(
    "initialize", "oce",
    function(.Object) {
        .Object@metadata <- list(units = list(), flags = list())
        .Object@data <- list()
        .Object@processingLog <- list(time = presentTime(), value = "Create oce object")
        .Object
    }
)

#' Summarize an oce Object
#'
#' Provide a textual summary of some pertinent aspects of the object, including
#' selected components of its `metadata` slot, statistical and
#' dimensional information on the entries in the `data` slot,
#' and a listing of the contents of its `processingLog` slot.
#' The details depend on the class of the object, especially for
#' the `metadata` slot, so it can help to consult the specialized
#' documentation, e.g. [summary,ctd-method] for CTD objects
#' (i.e. objects inheriting from the [ctd-class] class.)
#' It is important to note that this is not
#' a good way to learn the details of the object contents. Instead,
#' for an object named `object`, say, one might use `str(object)`
#' to learn about all the contents, or `str(object[["metadata"]])`
#' to learn about the `metadata`, etc.
#'
#' @param object The object to be summarized.
#'
#' @param ... Extra arguments (ignored)
#'
#' @examples
#' o <- new("oce")
#' summary(o)
setMethod(
    f = "summary",
    signature = "oce",
    definition = function(object, ...) {
        metadataNames <- names(object@metadata)
        dataNames <- names(object@data)
        isTime <- grepl("^time$", dataNames) # OLD: more permissive name, but that breaks on some data
        if (any(isTime)) {
            time <- object@data[[which(isTime)[1]]]
            # Times are always in POSIXct, so the length() does something useful
            if (inherits(time, "POSIXt") && length(time) > 0) {
                from <- min(time, na.rm = TRUE)
                to <- max(time, na.rm = TRUE)
                nt <- length(time)
                deltat <- mean(diff(as.numeric(time)), na.rm = TRUE)
                if (is.na(deltat)) {
                    cat("* Time: ", format(from), "\n", sep = "")
                } else {
                    if (deltat < 60) {
                        cat("* Time: ", format(from), " to ", format(to),
                            " (", nt, " samples, mean increment ", deltat, " s)\n",
                            sep = ""
                        )
                    } else if (deltat < 3600) {
                        cat("* Time: ", format(from), " to ", format(to),
                            " (", nt, " samples, mean increment ", deltat / 60, " min)\n",
                            sep = ""
                        )
                    } else if (deltat < 24 * 3600) {
                        cat("* Time: ", format(from), " to ", format(to),
                            " (", nt, " samples, mean increment ", deltat / 3600, " hour)\n",
                            sep = ""
                        )
                    } else {
                        cat("* Time: ", format(from), " to ", format(to),
                            " (", nt, " samples, mean increment ", deltat / 3600 / 24, " day)\n",
                            sep = ""
                        )
                    }
                }
            }
        }
        ndata <- length(object@data) # skip 'time' later, if it exists (issue 2198)
        threes <- NULL
        if (ndata > 0) {
            if (is.ad2cp(object)) {
                # FIXME: this needs rewriting, but is it worth it?
                threes <- matrix(nrow = 3, ncol = 3)
                # FIXME get burst and average separately
                i <- 1
                dataNames <- NULL
                if ("v" %in% names(object@data)) {
                    threes[i, ] <- threenum(object[["v"]])
                    i <- i + 1
                    dataNames <- c(dataNames, "v")
                }
                if ("a" %in% names(object@data)) {
                    threes[i, ] <- threenum(object[["a"]])
                    i <- i + 1
                    dataNames <- c(dataNames, "a")
                }
                if ("q" %in% names(object@data)) {
                    threes[i, ] <- threenum(object[["q"]])
                    i <- i + 1
                    dataNames <- c(dataNames, "q")
                }
                # message(vectorShow(dataNames)) # https://github.com/dankelley/oce/issues/2087
            } else { # not ad2cd object
                threes <- matrix(nrow = ndata, ncol = 3)
                for (i in seq_len(ndata)) {
                    # 2023-06-19 wrap in try() because one of the R-CMD check machines does
                    # not allow.  It says that the is.finite() is being applied to a list,
                    # which it clearly is not, so I don't understand the
                    # problem. Even so, using try() shouldn't hurt anything, and
                    # I don't like seeing a red "failed" box on the homepage.
                    ok <- try(any(is.finite(object@data[[i]])), silent = TRUE)
                    if (!inherits(ok, "try-error") && ok) {
                        threes[i, ] <- as.numeric(threenum(object@data[[i]]))
                    }
                }
            }
            # rownames(threes) <- paste("   ", dataNames[!isTime])
            units <- if ("units" %in% metadataNames) object@metadata$units else NULL
            # paste the scale after the unit
            unitsNames <- names(object@metadata$units)
            units <- unlist(lapply(
                seq_along(object@metadata$units),
                function(i) {
                    u <- object@metadata$units[[i]]
                    if (0L == length(u[1][[1]])) {
                        return(if (2 == length(u)) u[2] else "")
                    }
                    if (1L == length(u)) {
                        res <- if (is.expression(u)) as.character(u) else u
                    } else if (2L == length(u)) {
                        res <- if (nchar(u[2])) paste(u[[1]], u[[2]], sep = ", ") else u[[1]]
                    } else {
                        res <- ""
                    }
                    res <- as.character(res)[1] # the [1] is in case the unit is mixed up
                    # Clean up notation, by stages. (The order may matter.)
                    if (nchar(res)) res <- gsub("degree[ ]+[*][ ]+C", "\u00B0C", res)
                    if (nchar(res)) res <- gsub("degree[ ]+[*][ ]+F", "\u00B0F", res)
                    if (nchar(res)) res <- gsub("degree[ ]+[*][ ]+E", "\u00B0E", res)
                    if (nchar(res)) res <- gsub("degree[ ]+[*][ ]+W", "\u00B0W", res)
                    if (nchar(res)) res <- gsub("degree[ ]+[*][ ]+N", "\u00B0N", res)
                    if (nchar(res)) res <- gsub("degree[ ]+[*][ ]+S", "\u00B0S", res)
                    if (nchar(res)) res <- gsub("percent", "%", res)
                    if (nchar(res)) res <- gsub("degree", "\u00B0", res)
                    if (nchar(res)) res <- gsub("^,[ ]*", "", res)
                    if (nchar(res)) res <- gsub("mu . ", "\u03BC", res)
                    if (nchar(res)) res <- gsub("per . mil", "\u2030", res)
                    if (nchar(res)) res <- gsub("10\\^\\(-8\\)[ ]*\\*", "10\u207B\u2078", res)
                    if (nchar(res)) res <- gsub("\\^2", "\u00B2", res)
                    if (nchar(res)) res <- gsub("\\^3", "\u00B3", res)
                    res
                }
            ))
            names(units) <- unitsNames
            #> message("units:");str(units)
            if (!is.null(threes)) {
                # . message("threes step 1:");print(threes)
                #<> # I don't think this next applies to the new ad2cp model.
                #<> if (is.ad2cp(object)) {
                #<>     rownames(threes) <- c("v", "a", "q")
                #<> } else {
                #<>     rownames(threes) <- paste("    ", dataLabel(dataNames, units), sep="")
                #<> }
                # deleteLater <- grep("^time", dataNames) # we show time outside 3s block
                rownames(threes) <- paste("    ", dataLabel(dataNames, units), sep = "")
                # . message("threes step 2:");print(threes)
                threes <- cbind(
                    threes,
                    as.vector(lapply(
                        dataNames, # row.names(threes),
                        function(name) {
                            xx <- object@data[[name]]
                            if (is.array(xx)) {
                                paste(dim(xx), collapse = "x")
                            } else {
                                length(xx)
                            }
                        }
                    )),
                    as.vector(lapply(
                        dataNames, # row.names(threes),
                        function(name) {
                            sum(is.na(object@data[[name]]))
                        }
                    ))
                )
                colnames(threes) <- c("Min.", "Mean", "Max.", "Dim.", "NAs")
                cat("* Data Overview\n\n")
                if ("dataNamesOriginal" %in% metadataNames) {
                    if (is.list(object@metadata$dataNamesOriginal)) {
                        OriginalName <- unlist(lapply(
                            dataNames,
                            function(n) {
                                if (n %in% names(object@metadata$dataNamesOriginal)) {
                                    object@metadata$dataNamesOriginal[[n]]
                                } else {
                                    "-"
                                }
                            }
                        ))
                    } else {
                        OriginalName <- object@metadata$dataNamesOriginal
                    }
                } else {
                    OriginalName <- NULL
                }
                # I'm not sure the following will ever happen, if we always remember
                # to use ctdAddColumn(), but I don't want dataNames getting recycled, so
                # the next if-block prevents that.
                if (length(OriginalName)) {
                    if (length(OriginalName) < length(dataNames)) {
                        OriginalName <- c(OriginalName, rep("-", length(dataNames) - length(OriginalName)))
                    }
                    # print(OriginalName)
                    # message("threes step 4:");print(threes)
                    OriginalName[0 == nchar(OriginalName, "bytes")] <- "-"
                    # message("threes step 5:");print(threes)
                    if (!is.null(OriginalName)) {
                        if (is.ad2cp(object)) {
                            threes <- cbind(threes, "-")
                        } else {
                            threes <- cbind(threes, OriginalName)
                        }
                    }
                    # browser()
                    # colnames(threes) <- c(colnames(threes), "OriginalName")
                }
                # <2304> for (row in seq_len(nrow(threes))) {
                # <2304>     if (inherits(object@data[[row]], "POSIXt")) {
                # <2304>         threes[[row, 1L]] <- numberAsPOSIXct(threes[[row, 1L]])
                # <2304>         threes[[row, 2L]] <- numberAsPOSIXct(threes[[row, 2L]])
                # <2304>         threes[[row, 3L]] <- numberAsPOSIXct(threes[[row, 3L]])
                # <2304>     }
                # <2304> }
                owidth <- options("width")
                options(width = 500) # make super-wide to avoid line breaks
                print(threes, digits = 5)
                options(width = owidth$width)
                cat("\n")
            }
        }
        # Flag scheme (may exist even if no flags are set)
        if (!is.null(object@metadata$flagScheme)) {
            cat("* Data-quality Flag Scheme\n\n")
            cat("    name    \"", object@metadata$flagScheme$name, "\"\n", sep = "")
            cat("    mapping ", gsub(" = ", "=", as.character(deparse(object@metadata$flagScheme$mapping,
                width.cutoff = 400
            ))), "\n", sep = "")
            if ("default" %in% names(object@metadata$flagScheme)) {
                cat("    default ", gsub(
                    " = ", "=",
                    as.character(deparse(object@metadata$flagScheme$default,
                        width.cutoff = 400
                    ))
                ), "\n", sep = "")
            }
            cat("\n")
        }
        # Get flags specifically from metadata; using [["flags"]] could extract
        # it from data, if present there and not in metadata (as e.g. with
        # the data("ctd") that is provided with oce).
        flags <- object@metadata$flags
        if (length(flags)) {
            cat("* Data-quality Flags\n\n")
            if (length(names(flags))) {
                width <- 1 + max(nchar(names(flags)))
                for (name in names(flags)) {
                    padding <- rep(" ", width - nchar(name))
                    cat("    ", name, ":", padding, sep = "")
                    if (all(is.na(flags[[name]]))) {
                        cat("NA", length(flags[[name]]), "\n")
                    } else {
                        flagTable <- table(flags[[name]])
                        flagTableLength <- length(flagTable)
                        if (flagTableLength) {
                            for (i in seq_len(flagTableLength)) {
                                cat("\"", names(flagTable)[i], "\"", " ", flagTable[i], "", sep = "")
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
                            cat("\"", names(flagTable)[i], "\"", " ", flagTable[i], "", sep = "")
                            if (i != flagTableLength) cat(", ") else cat("\n")
                        }
                    }
                }
            }
            cat("\n")
        }
        processingLogShow(object)
        invisible(NULL)
    }
)


setClass("satellite", contains = "oce") # both amsr and landsat stem from this

#' Plot an oce Object
#'
#' @description
#' This creates a [pairs()] plot of the elements in the `data`
#' slot, if there are more than 2 elements there, or a simple xy plot if 2
#' elements, or a histogram if 1 element.
#'
#' @param x a basic [oce-class] object,
#' but not from any subclass that derive from this base, because
#' subclasses have their own plot methods, e.g. calling `plot()` on a
#' [ctd-class] object dispatches to [plot,ctd-method()].
#'
#' @param y Ignored; only present here because S4 object for generic `plot`
#' need to have a second parameter before the `...` parameter.
#'
#' @param ... Passed to [hist()], [plot()], or to
#' [pairs()], according to whichever does the plotting.
#'
#' @examples
#' library(oce)
#' o <- new("oce")
#' o <- oceSetData(o, "x", rnorm(10))
#' o <- oceSetData(o, "y", rnorm(10))
#' o <- oceSetData(o, "z", rnorm(10))
#' plot(o)
#' @aliases plot.oce
setMethod(
    f = "plot",
    signature = "oce",
    definition = function(x, y, ...) {
        n <- length(x@data)
        if (n == 1L) {
            hist(x@data[[1]])
        } else if (n == 2L) {
            plot(x@data[[1L]], x@data[[2L]])
        } else if (n > 2L) {
            pairs(x@data, ...)
        } else {
            warning("no data to plot")
        }
    }
)

#' Subset an oce Object
#'
#' @description
#' This is a basic class for general oce objects.  It has specialised
#' versions for most sub-classes, e.g. [subset,ctd-method()]
#' for `ctd` objects.
#'
#' @param x an [oce-class] object.
#'
#' @param subset a logical expression indicating how to take the subset; the form depends on the sub-class.
#'
#' @param ... optional arguments, used in some specialized methods, e.g. [subset,section-method()].
#'
#' @return An oce object.
#'
#' @examples
#' library(oce)
#' data(ctd)
#' # Select just the top 10 metres (pressure less than 10 dbar)
#' top10 <- subset(ctd, pressure < 10)
#' par(mfrow = c(1, 2))
#' plotProfile(ctd)
#' plotProfile(top10)
#' @family functions that subset oce objects
setMethod(
    f = "subset",
    signature = "oce",
    definition = function(x, subset, ...) {
        if (missing(subset)) {
            stop("must give 'subset'")
        }
        keep <- eval(expr = substitute(expr = subset, env = environment()), envir = x@data, enclos = parent.frame())
        res <- x
        for (i in seq_along(x@data)) {
            res@data[[i]] <- res@data[[i]][keep]
        }
        for (i in seq_along(x@metadata$flags)) {
            res@metadata$flags[[i]] <- res@metadata$flag[[i]][keep]
        }
        res@processingLog <- processingLogAppend(
            res@processingLog,
            paste(deparse(match.call(call = sys.call(sys.parent(1)))),
                sep = "", collapse = ""
            )
        )
        res
    }
)

#' Extract Something From an oce Object
#'
# @description
# The named item is sought first in
# `metadata`, where an exact match to the name is required. If
# it is not present in the `metadata` slot, then a partial-name
# match is sought in the `data` slot. Failing both
# tests, an exact-name match is sought in a field named
# `dataNamesOriginal` in the object's `metadata`
# slot, if that field exists. Failing that, `NULL` is returned.
#
# The full contents of the `metadata` slot of an object named
# `x` are returned with `x[["metadata"]]`, and
# `x[["data"]]` does the same thing for the data slot.
# Even if the full contents are not needed, this
# scheme can be useful in circumventing the searching scheme described
# in the previous paragraph, e.g. `x[["data"]]$longitude`
# might be used to select longitude from the data slot of `x`,
# as an alternative to `oceGetData`(x,"longitude")`.
#
# To get information on the specialized variants of this function,
# type e.g. `?"[[,adv-method"` for information on extracting
# data from an object of [adv-class].
#'
#' @template sub_subTemplate
#'
#' @param x an [oce-class] object.
# @param i The item to extract.
# @param j Optional additional information on the `i` item.
# @param ... Optional additional information (ignored).
#'
#' @seealso
#' Many `oce` object classes have specialized versions
#' of `[[` that handle the details in specialized way.
#'
#' @author Dan Kelley
setMethod(
    f = "[[",
    signature(x = "oce", i = "ANY", j = "ANY"),
    definition = function(x, i, j, ...) {
        # message("DAN AllClass [[ method: 1 i=", i)
        metadataNames <- sort(names(x@metadata))
        dataNames <- sort(names(x@data))
        # Below was my idea for propagating 'debug' here, but it 
        # does not work. I don't want to change the signature from the
        # lowest-level definition by adding a 'debug' argument.
        #<> dots <- list(...)
        #<> debug <- if ("debug" %in% names(dots)) dots$debug else 0
        debug <- 0 # for whole file FIXME: how to transmit debug from higher-level [[ code?
        oceDebug(debug, "in lowest-level [[ method with i=\"", i, "\"\n", sep = "")
        # message("AllClass.R:497")
        # browser()
        if (i == "?") {
            return(list(
                metadata = metadataNames,
                metadataDerived = NULL,
                data = dataNames,
                dataDerived = sort(computableWaterProperties(x))
            ))
        } else if (i == "metadata") {
            return(x@metadata)
        } else if (i == "data") {
            return(x@data)
        } else if (i == "processingLog") {
            return(x@processingLog)
        } else if (grepl("Unit$", i)) { # return a list
            if ("units" %in% names(x@metadata)) {
                return(x@metadata$units[[gsub("Unit$", "", i)]])
            } else {
                return(x@metadata[[i]])
            }
        } else if (grepl(" unit$", i)) { # return just the unit, an expression
            if ("units" %in% names(x@metadata)) {
                return(x@metadata$units[[gsub(" unit$", "", i)]][[1]])
            } else {
                return("")
            }
        } else if (grepl(" scale$", i)) { # return just the scale, a character string
            if ("units" %in% names(x@metadata)) {
                return(as.character(x@metadata$units[[gsub(" scale$", "", i)]][[2]]))
            } else {
                return("")
            }
        } else if (grepl("Flag$", i)) { # return a list
            if ("flags" %in% names(x@metadata)) {
                return(x@metadata$flags[[gsub("Flag$", "", i)]])
            } else {
                return(NULL)
            }
        }
        # NOTE: we do not pass data and metadata through directly because e.g. we want
        # [[ to convert to the modern temperature scale, if the data are
        # in an old scale.
        if (i == "conductivity") {
            C <- x@data$conductivity
            if (!is.null(C) && !missing(j)) {
                if (!(j %in% c("", "ratio", "uS/cm", "mS/cm", "S/m"))) {
                    stop("unknown conductivity unit \"", j, "\"; must be \"\", \"ratio\", \"uS/cm\", \"mS/cm\" or \"S/m\"")
                }
                if (j == "") {
                    j <- "ratio"
                } # lets us use switch()
                unit <- x@metadata$units$conductivity$unit
                # FIXME: maybe should look at median value, to make a guess
                if (is.null(unit) || !length(unit)) {
                    unit <- "ratio"
                }
                unit <- as.character(unit)
                C <- x@data$conductivity
                # Rather than convert from 3 inputs to 3 outputs, express as ratio, then convert as desired
                if (!unit %in% c("ratio", "uS/cm", "mS/cm", "S/m")) {
                    stop("object has unknown conductivity unit \"", unit, "\"; must be \"ratio\", \"uS/cm\", \"mS/cm\" or \"S/m\"")
                }
                C <- C / switch(unit,
                    "uS/cm" = 42914,
                    "mS/cm" = 42.914,
                    "S/m" = 4.2914,
                    "ratio" = 1
                )
                C <- C * switch(j,
                    "uS/cm" = 42914,
                    "mS/cm" = 42.914,
                    "S/m" = 4.2914,
                    "ratio" = 1
                )
            }
            return(C)
        } else if (i %in% c("CT", "Conservative Temperature")) {
            if (!any(is.finite(x[["longitude"]])) || !any(is.finite(x[["latitude"]]))) {
                stop("need longitude and latitude to compute SA (needed for CT)")
            }
            return(gsw::gsw_CT_from_t(SA = x[["SA"]], t = x[["temperature"]], p = x[["pressure"]]))
        } else if (i == "density") {
            return(swRho(x))
        } else if (i == "depth") {
            return(if ("depth" %in% dataNames) {
                x@data$depth
            } else {
                swDepth(x, debug = debug)
            })
        } else if (i == "nitrate") {
            if ("nitrate" %in% dataNames) {
                return(x@data$nitrate)
            } else {
                if ("nitrite" %in% dataNames && "NO2+NO3" %in% dataNames) {
                    return(x@data[["NO2+NO3"]] - x@data$nitrite)
                } else {
                    return(NULL)
                }
            }
        } else if (i == "nitrite") {
            if ("nitrite" %in% dataNames) {
                return(x@data$nitrite)
            } else {
                if ("nitrate" %in% dataNames && "NO2+NO3" %in% dataNames) {
                    return(x@data[["NO2+NO3"]] - x@data$nitrate)
                } else {
                    return(NULL)
                }
            }
        } else if (i == "N2") {
            return(swN2(x))
        } else if (i == "pressure") {
            if ("pressure" %in% dataNames) {
                pressure <- x@data$pressure
                # Handle files with pressure in PSI. This unit is so weird
                # that we issue a warning, because there may be other
                # strange things about the file, that are not handled.
                if ("units" %in% metadataNames &&
                    "pressure" %in% names(x@metadata$units) &&
                    is.list(x@metadata$units$pressure) &&
                    "unit" %in% names(x@metadata$units$pressure) &&
                    "psi" == tolower(as.character(x@metadata$units$pressure$unit))) {
                    warning("converting pressure from PSI to dbar\n")
                    pressure <- pressure * 0.6894757
                }
                return(pressure)
            } else if ("depth" %in% dataNames) {
                return(swPressure(x@data$depth))
            } else {
                return(NULL)
            }
        } else if (i == "Rrho") {
            oceDebug(debug, "lowest-level [[ about to call swRrho with sense='diffusive'\n")
            return(swRrho(x, sense = "diffusive", debug = debug))
        } else if (i == "RrhoSF") {
            oceDebug(debug, "lowest-level [[ about to call swRrho with sense='finger'\n")
            return(swRrho(x, sense = "finger", debug = debug))
        } else if (i %in% c("salinity", "SP")) {
            if ("salinity" %in% dataNames) {
                S <- x@data$salinity
            } else {
                C <- x@data$conductivity
                if (!is.null(C)) {
                    if (is.null(x@metadata$units$conductivity)) {
                        warning("conductivity has no unit, so guessing it is conductivity-ratio. Be cautious on calculated salinity.")
                    } else {
                        unit <- as.character(x@metadata$units$conductivity$unit)
                        if (0 == length(unit)) {
                            S <- swSCTp(C, x[["temperature"]], x[["pressure"]])
                            warning("constructed salinity from temperature, conductivity-ratio and pressure")
                        } else if (unit == "uS/cm") {
                            S <- swSCTp(C / 42914.0, x[["temperature"]], x[["pressure"]])
                            warning("constructed salinity from temperature, conductivity and pressure")
                        } else if (unit == "mS/cm") {
                            # e.g. RSK
                            S <- swSCTp(C / 42.914, x[["temperature"]], x[["pressure"]])
                            warning("constructed salinity from temperature, conductivity and pressure")
                        } else if (unit == "S/m") {
                            S <- swSCTp(C / 4.2914, x[["temperature"]], x[["pressure"]])
                            warning("constructed salinity from temperature, conductivity and pressure")
                        } else {
                            stop("unrecognized conductivity unit '", unit, "'; only uS/cm, mS/cm and S/m are handled")
                        }
                    }
                } else {
                    stop("the object's data slot lacks \"salinity\", and it cannot be calculated since \"conductivity\" is also missing")
                }
            }
            return(S)
        } else if (i %in% c("SA", "Absolute Salinity")) {
            return(swAbsoluteSalinity(x))
        } else if (i == "sigmaTheta") {
            return(if (missing(j)) {
                swSigmaTheta(x)
            } else {
                swSigmaTheta(x, eos = j)
            })
        } else if (i == "sigma0") {
            return(if (missing(j)) swSigma0(x) else swSigma0(x, eos = j))
        } else if (i == "sigma1") {
            return(if (missing(j)) swSigma1(x) else swSigma1(x, eos = j))
        } else if (i == "sigma2") {
            return(if (missing(j)) swSigma2(x) else swSigma2(x, eos = j))
        } else if (i == "sigma3") {
            return(if (missing(j)) swSigma3(x) else swSigma3(x, eos = j))
        } else if (i == "sigma4") {
            return(if (missing(j)) swSigma4(x) else swSigma4(x, eos = j))
        } else if (i %in% paste0("spiciness", 0:2)) {
            salinity <- x[["salinity"]]
            temperature <- x[["temperature"]]
            pressure <- x[["pressure"]]
            longitude <- x[["longitude"]]
            latitude <- x[["latitude"]]
            SA <- gsw::gsw_SA_from_SP(
                SP = salinity,
                p = pressure,
                longitude = longitude,
                latitude = latitude
            )
            CT <- gsw::gsw_CT_from_t(SA, temperature, pressure)
            return(switch(i,
                "spiciness0" = gsw::gsw_spiciness0(SA, CT),
                "spiciness1" = gsw::gsw_spiciness1(SA, CT),
                "spiciness2" = gsw::gsw_spiciness2(SA, CT)
            ))
        } else if (i == "silicate") {
            return(x@data$silicate)
        } else if (i == paste("sound", "speed")) {
            return(if (missing(j)) swSoundSpeed(x) else swSoundSpeed(x, eos = j))
        } else if (i == "spice") {
            # return(if (missing(j)) swSpice(x, eos = "unesco") else swSpice(x, eos = j))
            if (missing(j)) {
                j <- getOption("oceEOS", default = "gsw")
            }
            if (j != "gsw" && j != "unesco") {
                stop("[[\"spice\", \"", j, "\"]] not understood; try \"gsw\" or \"unesco\" as second parameter")
            }
            return(swSpice(x, eos = j))
        } else if (i == "SR") {
            return(swSR(x))
        } else if (i == "Sstar") {
            return(swSstar(x))
        } else if (i == "temperature") {
            U <- x@metadata$units
            if (!is.null(U) && !is.null(U$temperature) && is.list(U$temperature) && !is.null(U$temperature$scale)) {
                scale <- U$temperature$scale
                if (identical("IPTS-48", scale)) {
                    T90fromT48(x@data$temperature)
                } else if (identical("IPTS-68", scale)) {
                    T90fromT68(x@data$temperature)
                } else {
                    x@data$temperature
                }
            } else {
                x@data$temperature
            }
        } else if (i %in% c("theta", "potential temperature")) {
            return(swTheta(x))
        } else if (i == "z") {
            if ("z" %in% names(x@data)) {
                return(x@data$z)
            } else {
                return(swZ(x)) # requires latitude
            }
        } else {
            # DEBUG oceDebug(debug, "[[ at base level. i=\"", i, "\"\n", sep="", unindent=1, style="bold")
            if (missing(j) || j == "") {
                # DEBUG oceDebug(debug, "j missing or empty ...\n")
                # Since 'j' is not provided, we must search for 'i'. We look first
                # in the metadata slot, but if it's not there, we look in the
                # data slot. In the 'data' case, we also permit partial-match names,
                # as well as non-partial matching to the original names, as
                # contained in a data file.
                if (i %in% names(x@metadata)) {
                    return(x@metadata[[i]])
                }
                # partial match allowed in data, but not in original-name of data
                index <- pmatch(i, names(x@data))
                if (!is.na(index[1])) {
                    return(x@data[[index]])
                } else if (i %in% x@metadata$dataNamesOriginal) {
                    w <- which(i == x@metadata$dataNamesOriginal)
                    name <- names(x@metadata$dataNamesOriginal)[w]
                    return(x@data[[name]])
                } else {
                    return(NULL)
                }
            } else {
                # New in 2019-May-17: 'j' can be "data" or "metadata"
                # https://github.com/dankelley/oce/issues/1554
                if (j == "metadata") {
                    return(x@metadata[[i]])
                } else if (j == "data") {
                    # partial match allowed in data, but not in original-name of data
                    index <- pmatch(i, names(x@data))
                    if (!is.na(index[1])) {
                        return(x@data[[i]])
                    } else if (i %in% x@metadata$dataNamesOriginal) {
                        return(x@data[[which(i == x@metadata$dataNamesOriginal)[1]]])
                    } else {
                        return(NULL)
                    }
                } else {
                    stop("object[[\"", i, "\", \"", j, "\"]]: second arg must be \"data\" or \"metadata\"", call. = FALSE)
                }
            }
        }
    }
)


#' @title Replace Parts of an oce Object
#'
#' @param x an [oce-class] object.
#'
#' @template sub_subsetTemplate
#' @author Dan Kelley
setMethod(
    f = "[[<-",
    signature(x = "oce", i = "ANY", j = "ANY"),
    function(x, i, j, ..., value) {
        # FIXME: use j for e.g. times
        # message("in base [[<-")
        # message("i: ", as.character(i))
        # message("value: ", paste(value, collapse=" "))
        # metadata must match exactly but data can be partially matched
        if (i == "metadata") {
            x@metadata <- value
        } else if (i %in% names(x@metadata)) {
            x@metadata[[i]] <- value
        } else {
            if (grepl("Unit$", i)) {
                if (!("units" %in% names(x@metadata))) {
                    x@metadata$units <- list()
                }
                x@metadata$units[[gsub("Unit$", "", i)]] <- value
            } else if (grepl("Flag$", i)) {
                if (!("flags" %in% names(x@metadata))) {
                    x@metadata$flags <- list()
                }
                x@metadata$flags[[gsub("Flag$", "", i)]] <- value
            } else {
                x@data[[i]] <- value
            }
        }
        validObject(x)
        invisible(x)
    }
)

setValidity(
    "oce",
    function(object) {
        slotNames <- slotNames(object)
        nslots <- length(slotNames)
        if (nslots != 3) {
            cat("should be 3 slots, but there are", nslots, "\n")
            return(FALSE)
        }
        for (name in c("metadata", "data", "processingLog")) {
            if (!(name %in% slotNames)) {
                cat("object should have a slot named \"", name, "\"\n", sep = "")
                return(FALSE)
            }
        }
        return(TRUE)
    }
)

setMethod(
    f = "show",
    signature = "oce",
    definition = function(object) {
        filename <- if ("filename" %in% names(object@metadata)) {
            object[["filename"]]
        } else {
            "(filename unknown)"
        }
        dataNames <- names(object@data)
        ncol <- length(dataNames)
        if (is.null(filename) || filename == "" || is.na(filename) || filename == "(filename unknown)") {
            if (ncol > 0) {
                cat(class(object)[1], " object has data as follows.\n", sep = "")
            } else {
                cat(class(object)[1], " object has nothing in its data slot.\n", sep = "")
            }
        } else {
            if (ncol > 0) {
                cat(class(object)[1], " object, from file \"", filename, "\", with data slot containing:\n", sep = "")
            } else {
                cat(class(object)[1], " object, from file \"", filename, "\", with nothing in its data slot.\n", sep = "")
            }
        }
        odigits <- options("digits")$digits
        options(digits = 9) # helps with e.g. CTD adjusted vs unadjusted values
        for (i in seq_along(dataNames)) {
            d <- object@data[[i]]
            if (inherits(d, "POSIXt")) {
                cat(vectorShow(d, paste("  ", dataNames[i])))
            } else if (is.list(d)) {
                cat("  ", dataNames[i], ", a list with contents:\n", sep = "")
                for (n in names(d)) {
                    cat("    ", vectorShow(d[[n]], n), sep = "")
                }
            } else if (is.data.frame(d)) {
                cat("  ", dataNames[i], ", a data frame with contents:\n", sep = "")
                for (n in names(d)) {
                    cat("    ", vectorShow(d[[n]], n), sep = "")
                }
            } else if (is.vector(d)) {
                cat(vectorShow(d, paste("  ", dataNames[i])))
            } else if (is.array(d)) {
                dim <- dim(object@data[[i]])
                if (length(dim) == 1L) {
                    cat(vectorShow(d, paste("  ", dataNames[i])))
                } else if (length(dim) == 2L) {
                    cat("   ", dataNames[i], ", a ", dim[1], "x", dim[2], " array with value ", d[1, 1], " at [1,1] position\n", sep = "")
                }
            } else if (length(dim) == 3) {
                cat("   ", dataNames[i], ", a ", dim[1], "x", dim[2], "x", dim[3], " array with value ", d[1, 1, 1],
                    " at [1,1,1] position\n",
                    sep = ""
                )
            } else {
                cat("   ", dataNames[i], ", an array of more than 3 dimensions\n", sep = "")
            }
        }
        options(digits = odigits) # return to original digits value
    }
)

#' Alter an Object to Account for Magnetic Declination (Generic)
#'
#' Current-measuring instruments that infer flow direction using magnetic
#' compasses require a correction for magnetic declination, in order to infer
#' currents with x and y oriented eastward and northward, respectively.
#' [applyMagneticDeclination()] is a generic function that handles this task by
#' altering velocity components (and heading values, if they exist).  It works
#' for objects of the [cm-class], [adp-class] and [adv-class] and [cm-class]
#' classes by calling [applyMagneticDeclination,adp-method()],
#' [applyMagneticDeclination,adv-method()], or
#' [applyMagneticDeclination,cm-method()], respectively.
#'
#' @template declinationTemplate
#'
#' @param object an object of [cm-class], [adp-class], or [adv-class] class.
#'
#' @param declination numeric value holding magnetic declination in degrees,
#' positive for clockwise from north.
#'
#' @template debugTemplate
#'
#' @return an object of the same class as `object`, modified as described
#' in \dQuote{Details}.
#'
#' @author Dan Kelley, aided, for the [adp-class] and [adv-class] variants,
#' by Clark Richards and Jaimie Harbin.
#'
#' @seealso Use [magneticField()] to determine the declination,
#' inclination and intensity at a given spot on the world, at a given time.
#'
#' @family things related to magnetism
setGeneric(
    name = "applyMagneticDeclination",
    def = function(object = "oce", declination = "ANY", debug = "ANY") {
        standardGeneric("applyMagneticDeclination")
    }
)

#' Alter an Object to Account for Magnetic Declination
#'
#' Current-measuring instruments that infer flow direction using magnetic
#' compasses require a correction for magnetic declination, in order to infer
#' currents with x and y oriented eastward and northward, respectively.
#' [applyMagneticDeclination()] is a generic function that handles this task by
#' altering velocity components (and heading values, if they exist).  It works
#' for objects of the [cm-class], [adp-class] and [adv-class] and [cm-class]
#' classes by calling [applyMagneticDeclination,adp-method()],
#' [applyMagneticDeclination,adv-method()], or
#' [applyMagneticDeclination,cm-method()], respectively.
#'
#' @template declinationTemplate
#'
#' @param object an object of [cm-class], [adp-class], or [adv-class] class.
#'
#' @param declination numeric value holding magnetic declination in degrees,
#' positive for clockwise from north.
#'
#' @param debug a debugging flag, set to a positive value to get debugging.
#'
#' @return an object of the same class as `object`, modified as outlined in
#' \dQuote{Details}.
#'
#' @author Dan Kelley, aided, for the [adp-class] and [adv-class] variants,
#' by Clark Richards and Jaimie Harbin.
#'
#' @seealso Use [magneticField()] to determine the declination,
#' inclination and intensity at a given spot on the world, at a given time.
#'
#' @family things related to magnetism
setMethod(
    f = "applyMagneticDeclination",
    signature = c(object = "oce", declination = "ANY", debug = "ANY"),
    definition = function(object, declination = 0.0, debug = getOption("oceDebug")) {
        if (length(declination) != 1L) {
            stop("length of declination must equal 1")
        }
        if (inherits(object, "cm")) {
            callNextMethod()
        } else if (inherits(object, "adp")) {
            callNextMethod()
        } else if (inherits(object, "adv")) {
            callNextMethod()
        } else {
            stop("method only works for \"adp\", \"adv\" and \"cm\" objects")
        }
    }
)


#' Create a Composite Object by Averaging Across Good Data
#'
#' @param object either a [list] of [oce-class] objects, in
#' which case this is the only argument, or a single [oce-class] object,
#' in which case at least one other argument (an object of the same size)
#' must be supplied.
#'
#' @param ... Ignored, if `object` is a list. Otherwise, one or more
#' [oce-class] objects of the same sub-class as the first argument.
#'
#' @template compositeTemplate
setGeneric(
    "composite",
    function(object, ...) {
        standardGeneric("composite")
    }
)


#' Composite by Averaging Across Data
#'
#' This is done
#' by calling a specialized version of the function defined in the given
#' class. In the present
#' version, the objects must inherit from [amsr-class], so the
#' action is to call
#' [composite,amsr-method()].
#'
#' @param object a [list] of [oce-class] objects.
#'
#' @template compositeTemplate
setMethod(
    "composite",
    c(object = "list"),
    function(object) {
        if (length(object) < 2) {
            object
        } else if (inherits(object[[1]], "amsr")) {
            do.call("composite", object)
        } else {
            stop("In composite(list) : only AMSR objects are handled")
        }
    }
)

#' Concatenate oce Objects (Generic)
#'
#' @param object an [oce-class] object.
#'
#' @param ... optional additional [oce-class] objects.
#'
#' @param debug integer indicating a debugging level. If this is 0,
#' the work is done silently.  If it is a larger integer, some information
#' may be printed during the processing.
#'
#' @return An object of class corresponding to that of `object`.
#'
#' @family functions for concatenating oce objects
setGeneric(
    "concatenate",
    function(object, ..., debug = getOption("oceDebug")) {
        standardGeneric("concatenate")
    }
)

#' Concatenate oce Objects (oce-Specific)
#'
#' @templateVar class oce
#'
#' @template debugTemplate
#'
#' @template concatenateTemplate
setMethod("concatenate",
    signature = "oce",
    definition = function(object, ..., debug = getOption("oceDebug")) {
        oceDebug(debug, "concatenate() ...\n")
        dots <- list(...)
        ndots <- length(dots)
        oceDebug(debug, "  ndots=", ndots, "\n")
        if (0 == ndots) {
            return(object)
        }
        # Insist everything be an oce object.
        for (i in seq_len(ndots)) {
            if (!inherits(dots[[i]], "oce")) {
                stop("concatenate() argument ", i + 1, " does not inherit from \"oce\"")
            }
        }
        oceDebug(debug, "  arguments seem okay\n")
        # Concatenate the data (and flags, if there are such).
        res <- object
        isDataFrame <- is.data.frame(res@data)
        if (isDataFrame) {
            oceDebug(debug, "  temporarily converting data slot from a data frame to a list\n")
            res@data <- as.list(res@data)
        }
        n1 <- sort(names(res@data))
        f1 <- if ("flags" %in% names(object@metadata) && length(object@metadata$flags)) {
            sort(names(object@metadata$flags))
        } else {
            NULL
        }
        oceDebug(debug, "  f1: c(\"", paste(f1, collapse = "\", \""), "\")\n",
            sep = ""
        )
        for (i in seq_len(ndots)) {
            oceDebug(debug, "  processing i=", i, " of ", ndots, "\n")
            # Data
            ni <- sort(names(dots[[i]]@data))
            if (!identical(n1, ni)) {
                stop(
                    "data name mismatch between argument 1 (",
                    paste(n1, collapse = " "), ") and argument ", i,
                    "(", paste(ni, collapse = " "), ")"
                )
            }
            data <- dots[[i]]@data
            oceDebug(debug, "  about to work on ni=c(", paste(ni, collapse = ","), ")\n")
            for (n in ni) {
                oceDebug(debug, "  processing n=\"", n, "\"\n")
                if (is.vector(dots[[1]]@data[[n]]) || n == "time" || is.factor(n)) {
                    oceDebug(debug, "    vector case\n")
                    res@data[[n]] <- c(res@data[[n]], data[[n]])
                } else if (is.matrix(data[[n]])) {
                    oceDebug(debug, "    matrix case\n")
                    res@data[[n]] <- rbind(res@data[[n]], data[[n]])
                } else if (is.array(data[[n]])) {
                    oceDebug(debug, "    array case\n")
                    # construct a larger temporary array, fill in by 3rd index, then put in res
                    dim <- dim(res@data[[n]])
                    tmp <- array(object@data[[n]][1, 1, 1], dim = c(dim[1] + dim(data[[n]])[1], dim[2], dim[3]))
                    for (k in seq_len(dim[3])) {
                        tmp[, , k] <- rbind(res@data[[n]][, , k], data[[n]][, , k])
                    }
                    res@data[[n]] <- tmp
                }
            }
            # Fix up dimensionality
            for (n in ni) {
                if (is.array(dots[[1]]@data[[n]])) {
                    dim <- dim(dots[[1]]@data[[n]])
                    ndim <- length(dim)
                    denom <- if (ndim == 2) dim[2] else if (ndim == 3) dim[2] * dim[3]
                    dim[1] <- length(res@data[[n]]) / denom
                    dim(res@data[[n]]) <- dim
                }
            }
            # Flags
            if (!is.null(f1)) {
                metadata <- dots[[i]]@metadata
                fi <- sort(names(dots[[i]]@metadata$flags))
                if (!identical(f1, fi)) {
                    stop(
                        "flag mismatch between argument 1 (",
                        paste(f1, collapse = " "), ") and argument ", i,
                        "(", paste(fi, collapse = " "), ")"
                    )
                }
                for (f in fi) {
                    res@metadata$flags[[f]] <- c(res@metadata$flags[[f]], metadata$flags[[f]])
                }
            }
        }
        if (isDataFrame) {
            oceDebug(debug, "  converting data slot to a data frame\n")
            res@data <- as.data.frame(res@data)
        }
        # for reasons unknown to me, the tzone gets localized
        attr(res@data$time, "tzone") <- attr(object@data$time, "tzone")
        res
    }
)

#' Concatenate a List of oce Objects
#'
#' @param object a [list] of [oce-class] objects, all of which must have the
#' same sub-class (e.g. all of [ctd-class], or [adp-class], etc).
#'
#' @param debug a debugging flag, set to a positive value to get debugging. Note
#' that `debug-1` is passed to the other `concatenate()` functions that are
#' called by the present function.
#'
#' @return An object of class corresponding to that in the elements of `object`.
#'
#' @family functions for concatenating oce objects
setMethod("concatenate",
    signature = "list",
    definition = function(object, debug = getOption("oceDebug")) {
        oceDebug(debug, "list concatenate() START\n", sep = "", unindent = 1)
        classes <- sapply(object, class)
        if (!all(classes == classes[1])) {
            stop(
                "All elements of the list must have the same class, but they are: \"",
                paste(classes, collapse = "\", \""), "\""
            )
        }
        debugOld <- getOption("oceDebug")
        options(oceDebug = debug - 1) # this passes debug-1 to the generic
        rval <- do.call("concatenate", object)
        options(oceDebug = debugOld) # restore original global oceDebug
        oceDebug(debug, "END list concatenate()\n", sep = "", unindent = 1)
        rval
    }
)


#' @title Handle Flags in oce Objects (Generic)
#'
#' @details
#' Each specialized variant of this function has its own defaults
#' for `flags` and `actions`.
#'
#' @param object an [oce-class] object.
#'
#' @template handleFlagsTemplate
setGeneric(
    name = "handleFlags",
    def = function(object = "oce", flags = NULL, actions = NULL, where = NULL, debug = getOption("oceDebug")) {
        standardGeneric("handleFlags")
    }
)

