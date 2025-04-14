# vim:textwidth=80:expandtab:shiftwidth=4:softtabstop=4


#' Signal Erroneous Application to non-oce Objects
#' @param object A vector, which cannot be the case for `oce` objects.
#' @param flags Ignored.
#' @param actions Ignored.
#' @param where Ignored.
#' @param debug Ignored.
setMethod(
    f = "handleFlags",
    signature = c(object = "vector", flags = "ANY", actions = "ANY", where = "ANY", debug = "ANY"),
    definition = function(object, flags = list(), actions = list(), where = list(), debug = getOption("oceDebug")) {
        stop("handleFlags() can only be applied to objects inheriting from \"oce\"")
    }
)

#' Handle Flags in oce Objects
#'
#' @details
#
#' Base-level handling of flags.
#
#' @param object an [oce-class] object.
#
#' @template handleFlagsTemplate
setMethod(
    f = "handleFlags",
    signature = c(object = "oce", flags = "ANY", actions = "ANY", where = "ANY", debug = "ANY"),
    definition = function(object, flags = NULL, actions = NULL, where = NULL, debug = getOption("oceDebug")) {
        # DEVELOPER 1: alter the next comment to explain your setup
        if (is.null(flags)) {
            flags <- defaultFlags(object)
            if (is.null(flags)) {
                stop("must supply \"flags\", or use initializeFlagScheme() on the ctd object first")
            }
        }
        if (is.null(actions)) {
            actions <- list("NA") # DEVELOPER 3: alter this line to suit a new data class
            names(actions) <- names(flags)
        }
        if (any(names(actions) != names(flags))) {
            stop("names of flags and actions must match")
        }
        handleFlagsInternal(object = object, flags = flags, actions = actions, where = where, debug = debug)
    }
)


#' Low-Level Function for Handling Data-Quality Flags
#'
#' This function is designed for internal use within the `oce` package.  Its
#' purpose is to carry out low-level processing relating to data-quality flags,
#' as a support for higher-level functions such [handleFlags,ctd-method] for
#' `ctd` objects, [handleFlags,adp-method] for `adp` objects,
#' etc.
#'
#' @param object an [oce-class] object.
#'
#' @param flags a named [list] of numeric values.
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
#' `data` slot, if `object` contains flag values that have
#' actions that alter the data.
handleFlagsInternal <- function(object, flags, actions, where, debug = 0) {
    oceDebug(debug, "handleFlagsInternal() START\n", sep = "", unindent = 1)
    if (debug > 0L) {
        cat("flags=c(", paste(flags, collapse = ","), ")\n", sep = "")
        cat("actions=c(", paste(actions, collapse = ","), ")\n", sep = "")
        cat("where=\"", where, "\"\n", sep = "")
    }
    if (missing(flags)) {
        warning("no flags supplied (internal error; report to developer)")
        return(object)
    }
    # Permit e.g. flags=c(1,3)
    if (!is.list(flags)) {
        flags <- list(flags)
    }
    if (missing(actions)) {
        warning("no actions supplied (internal error; report to developer)")
        return(object)
    }
    if (missing(where)) {
        where <- NULL
    }
    if (any(names(flags) != names(actions))) {
        stop("names of flags must match those of actions")
    }
    oceDebug(debug, "flags=", paste(as.vector(flags), collapse = ","), "\n", sep = "")
    oflags <- if (is.null(where)) object@metadata$flags else object@metadata$flags[[where]]
    odata <- if (is.null(where)) object@data else object@data[[where]]
    if (length(oflags)) {
        singleFlag <- is.null(names(oflags)) # TRUE if there is just one flag for all data fields
        oceDebug(debug, "singleFlag=", singleFlag, "\n", sep = "")
        if (singleFlag && (length(actions) > 1 || !is.null(names(actions)))) {
            stop("if flags is a list of a single unnamed item, actions must be similar")
        }
        oceDebug(debug, "names(odata)=c(\"", paste(names(odata),
            collapse = "\", \""
        ), "\")\n", sep = "")
        if (singleFlag) {
            # apply the same flag to *all* data.
            actionsThis <- actions[[1]] # FIXME: this seems wrong
            oflags <- unlist(oflags)
            oceDebug(debug, "singleFlag: head(oflags)=c(",
                paste(head(oflags), collapse = ","), "), to be used for *all* data types.\n",
                sep = ""
            )
            for (name in names(odata)) {
                oceDebug(debug, "handling flags for '", name, "'\n", sep = "")
                dataItemLength <- length(odata[[name]])
                oceDebug(debug, "  initially, ", sum(is.na(odata[[name]])), " out of ", dataItemLength, " are NA\n", sep = "")
                actionNeeded <- oflags %in% if (length(names(flags))) flags[[name]] else flags[[1]]
                if (is.function(actionsThis)) {
                    oceDebug(debug > 1, "  actionsThis is a function\n")
                    odata[[name]][actionNeeded] <- actionsThis(object)[actionNeeded]
                } else if (is.character(actionsThis)) {
                    oceDebug(debug > 1, "  actionsThis is a string, '", actionsThis, "'\n", sep = "")
                    oceDebug(debug > 1, "  head(actionNeeded)=c(", paste(head(actionNeeded), collapse = ","), ")\n", sep = "")
                    if (actionsThis == "NA") {
                        odata[[name]][actionNeeded] <- NA
                    } else {
                        stop("the only permitted character action is 'NA'")
                    }
                } else {
                    stop("action must be a character string or a function")
                }
                oceDebug(debug, "  after handling flags, ", sum(is.na(odata[[name]])),
                    " out of ", length(odata[[name]]), " are NA\n",
                    sep = ""
                )
            }
            oceDebug(debug, "done handling flags for all data in object\n")
        } else { # multiple flags: Apply individual flags to corresponding data fields
            for (name in names(odata)) {
                flagsObject <- oflags[[name]]
                if (length(flagsObject) > 0L) {
                    oceDebug(debug, "handling flags for '", name, "'\n", sep = "")
                    oceDebug(debug, "  initially, ", sum(is.na(odata[[name]])),
                        " out of ", length(odata[[name]]), " are NA\n",
                        sep = ""
                    )
                    # if (debug) {
                    #    tab <- table(flagsObject)
                    #    if (length(tab) > 0L) {
                    #        cat("  unique(flagsObject) for ", name, ":\n")
                    #        print(table(flagsObject))
                    #    }
                    # }
                    if (!is.null(flagsObject)) {
                        dataItemLength <- length(odata[[name]])
                        # flagsThis <- oflags[[name]]
                        # oceDebug(debug, "before converting to numbers, flagsThis=", paste(flagsThis, collapse=","), "\n")
                        if (name %in% names(oflags)) {
                            actionsThis <- if (length(names(actions))) actions[[name]] else actions[[1]]
                            oceDebug(debug > 1, "  actionsThis: \"", paste(actionsThis, collapse = ","), "\"\n", sep = "")
                            actionNeeded <- oflags[[name]] %in% if (length(names(flags))) flags[[name]] else flags[[1]]
                            oceDebug(debug > 1, "  head(actionNeeded)=c(", paste(head(actionNeeded), collapse = ","), ")\n", sep = "")
                            if (any(actionNeeded)) {
                                #  oceDebug(debug, "\"", name, "\" has ", dataItemLength, " data, of which ",
                                #           sum(actionNeeded), " are flagged\n", sep="")
                                # if (debug > 1) {
                                #    cat("  actionsThis follows...\n")
                                #    print(actionsThis)
                                # }
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
                                oceDebug(debug, "  no action needed, since no \"", name, "\" data are flagged as stated\n", sep = "")
                            }
                        }
                    }
                    oceDebug(debug, "  finally, ", sum(is.na(odata[[name]])),
                        " out of ", length(odata[[name]]), " are NA\n",
                        sep = ""
                    )
                }
            }
        } # multiple flags
    } else {
        oceDebug(debug, "object has no flags in metadata\n")
    }
    if (is.null(where)) {
        object@data <- odata
    } else {
        object@data[[where]] <- odata
    }
    object@processingLog <- processingLogAppend(
        object@processingLog,
        paste("handleFlagsInternal(flags=c(",
            paste(substitute(flags, parent.frame()), collapse = ","),
            "), actions=c(",
            paste(substitute(actions, parent.frame()), collapse = ","),
            "))",
            collapse = " ", sep = ""
        )
    )
    oceDebug(debug, "END handleFlagsInternal()\n", sep = "", unindent = 1)
    object
}


#' Suggest a Default Flag Vector for Bad or Suspicious Data
#'
#' `defaultFlags` tries to suggest a reasonable default `flag` scheme
#' for use by [handleFlags()]. It does this by looking for an item
#' named `flagScheme` in the `metadata` slot of `object`.
#' If `flagScheme` is found, and if the scheme is recognized, then a numeric
#' vector is returned that indicates bad or questionable data. If
#' `flagScheme$default` exists, then that scheme is returned. However,
#' if that does not exist, and if `flagScheme$name` is recognized,
#' then a pre-defined (very conservative) scheme is used,
#' as listed below.
#'
#' * for `argo`, the default is
#' `c(0,3,4,6,7,9)`, meaning to act upon `not_assessed` (0), `probably_bad` (3),
#' `bad` (4), `not_used_6` (6), `not_used_7` (7) and `missing` (9).  See Section
#' 3.2.2 of Carval et al. (2019).
#'
#' * for `BODC`, the default is
#' `c(0,2,3,4,5,6,7,8,9)`, i.e. all flags except `good`.
#'
#' * for `DFO`, the default is
#' `c(0,2,3,4,5,8,9)`, i.e. all flags except `appears_correct`.
#'
#' * for `WHP bottle`, the default is
#' `c(1,3,4,5,6,7,8,9)`, i.e. all flags except `no_problems_noted`.
#'
#' * for `WHP ctd`, the default is
#' `c(1,3,4,5,6,7,9)`, i.e. all flags except `acceptable`.
#'
#' @param object An oce object
#'
#' @return A vector of one or more flag values, or `NULL` if `object`
#' `metadata` slot lacks a `flagScheme` as set by [initializeFlagScheme()],
#' or if it has a scheme that is not in the list provide in \dQuote{Description}.
#'
#' @references
#'
#' * Carval, Thierry, Bob Keeley, Yasushi Takatsuki, Takashi Yoshida, Stephen Loch Loch,
#' Claudia Schmid, and Roger Goldsmith. Argo User's Manual V3.3. Ifremer, 2019.
#' \doi{10.13155/29825}
#'
#' @family functions relating to data-quality flags
defaultFlags <- function(object) {
    if (is.null(object@metadata$flagScheme)) {
        return(NULL)
    }
    default <- object@metadata$flagScheme$default
    if (!is.null(default)) {
        return(default)
    }
    scheme <- object@metadata$flagScheme$name
    if (is.null(scheme)) {
        return(NULL)
    } else if (scheme == "argo") {
        return(c(0, 3, 4, 6, 7, 9)) # prior to 2020-june-11, was c(0, 2, 3, 4, 7, 8, 9)
    } else if (scheme == "BODC") {
        return(c(0, 2, 3, 4, 5, 6, 7, 8, 9)) # retain good
    } else if (scheme == "DFO") {
        return(c(0, 2, 3, 4, 5, 8, 9)) # retain appears_correct
    } else if (scheme == "WHP bottle") {
        return(c(1, 3, 4, 5, 6, 7, 8, 9)) # retain no_problems_noted
    } else if (scheme == "WHP ctd") {
        return(c(1, 3, 4, 5, 6, 7, 9)) # retain acceptable
    }
    warning("unable to determine default flags from 'flagScheme' in the object 'metadata' slot\n")
    return(NULL)
}


#' @templateVar class oce
#' @templateVar note This generic function is overridden by specialized functions for some object classes.
#' @template setFlagsTemplate
setGeneric(
    "setFlags",
    function(object, name = NULL, i = NULL, value = NULL, debug = 0) {
        standardGeneric("setFlags")
    }
)


#' @templateVar class oce
#' @templateVar note This generic function is overridden by specialized functions for some object classes.
#' @template setFlagsTemplate
setMethod("setFlags",
    signature = c(object = "oce", name = "ANY", i = "ANY", value = "ANY", debug = "ANY"),
    definition = function(object, name = NULL, i = NULL, value = NULL, debug = getOption("oceDebug")) {
        setFlagsInternal(object, name, i, value, debug)
    }
)

setFlagsInternal <- function(object, name = NULL, i = NULL, value = NULL, debug = getOption("oceDebug")) {
    oceDebug(debug, "setFlagsInternal(object, name=\"", name, "\", value=", value,
        ", i=", paste(i, collapse = " "), ", debug=", debug, ") START\n",
        sep = "", unindent = 1
    )
    res <- object
    # Ensure proper argument setup.
    if (is.null(name)) {
        stop("must supply a name")
    }
    if (is.null(i)) {
        stop("must supply 'i'")
    }
    if (is.null(value)) {
        stop("must supply 'value'")
    }
    if (length(name) > 1) {
        stop("must specify one 'name' at a time (this restriction may be relaxed in the future)")
    }
    if (!(name %in% names(object@metadata$flags))) {
        stop("object has no flag for \"", name, "\"; try one of: \"", paste(names(object@data), collapse = " "), "\"")
    }
    # Done with argument analysis.

    # Permit 'value' to be a character string, if a scheme already
    # exists and 'value' is one of the stated flag names.
    valueOrig <- value
    if (is.character(value)) {
        if (is.null(res@metadata$flagScheme)) {
            stop("cannot have character 'value' because initializeFlagScheme() has not been called on object")
        } else {
            if (value %in% names(res@metadata$flagScheme$mapping)) {
                value <- res@metadata$flagScheme$mapping[[value]]
            } else {
                stop("value=\"", value, "\" is not defined in the object's flagScheme; try one of: \"",
                    paste(names(res@metadata$flagScheme$mapping), "\", \""), "\"",
                    sep = ""
                )
            }
        }
    }
    # Finally, apply the value
    if (is.vector(object@data[[name]])) {
        oceDebug(debug, name, " is a vector\n")
        res@metadata$flags[[name]][i] <- value
    } else if (is.array(object@data[[name]])) {
        dimData <- dim(object@data[[name]])
        if (is.array(i)) {
            if (!is.logical(i)) {
                stop("array 'i' must be logical")
            }
            if (!identical(dim(i), dimData)) {
                stop(
                    "dim(i) is ", paste(dim(i), collapse = "x"), " but need ",
                    paste(dimData, collapse = "x"), " to match '", name, "'"
                )
            }
            res@metadata$flags[[name]][i] <- value
        } else if (is.data.frame(i)) {
            if (ncol(i) != length(dimData)) {
                stop("data frame 'i' must have ", length(dimData), " columns to match shape of '", name, "'")
            }
            for (j in seq_len(nrow(i))) {
                res@metadata$flags[[name]][i[j, 1], i[j, 2], i[j, 3]] <- value
            }
        } else {
            stop("'i' must be a matrix or a data frame")
        }
    } else {
        stop("only works for vectors and arrays (please report this as an error)")
    }
    res@processingLog <- processingLogAppend(
        res@processingLog,
        paste("setFlags(object, \"", name, "\", i, value=", valueOrig,
            ")",
            collapse = ""
        )
    )
    oceDebug(debug, "END setFlagsInternal()\n", unindent = 1)
    res
}

#' @templateVar class oce
#' @template initializeFlagsTemplate
setGeneric(
    "initializeFlags",
    function(object, name = NULL, value = NULL, debug = 0) {
        standardGeneric("initializeFlags")
    }
)

#' @templateVar class oce
#' @template initializeFlagsTemplate
setMethod("initializeFlags",
    signature = c(object = "oce", name = "ANY", value = "ANY", debug = "ANY"),
    definition = function(object, name, value, debug = getOption("oceDebug")) {
        initializeFlagsInternal(object, name, value, debug)
    }
)

#' @templateVar class oce
#' @templateVar details This is a low-level internal function used by user-accessible functions.
#' @template initializeFlagsTemplate
initializeFlagsInternal <- function(object, name = NULL, value = NULL, debug = getOption("oceDebug")) {
    oceDebug(debug, "initializeFlagsInternal(object, name=\"", name, "\", value, debug=", debug, ") START\n", sep = "", unindent = 1)
    res <- object
    if (is.null(name)) {
        stop("must supply name")
    }
    if (is.null(value)) {
        stop("must supply value")
    }
    valueOrig <- value
    if (!is.null(object@metadata$flags[[name]])) {
        warning("cannot re-initialize flags; use setFlags() to alter values")
    } else {
        # if (is.character(value)) {
        #     if (is.null(object@metadata$flagScheme))
        #         stop("cannot use character value because object has no flagScheme in its metadata")
        #     if (!(value %in% names(object@metadata$flagScheme$mapping)))
        #         stop("\"", value, "\" is not in the object's flagScheme; try one of: \"",
        #              paste(names(object@metadata$flagScheme$mapping), collapse="\", \""),
        #              "\"")
        #     value <- object@metadata$flagScheme$mapping[[value]]
        # }
        if (!(name %in% names(object@data))) {
            stop(
                "name=\"", name, "\" is not in the data slot of object; try one of: \"",
                paste(name(object@data), collapse = "\", \""), "\""
            )
        }
        # Flag is set up with dimensions matching data
        if (is.vector(object@data[[name]])) {
            oceDebug(debug, name, " is a vector\n")
            res@metadata$flags[[name]] <- rep(value, length(object@data[[name]]))
        } else if (is.array(object@data[[name]])) {
            dimData <- dim(object@data[[name]])
            res@metadata$flags[[name]] <- array(value, dim = dimData)
        } else {
            stop("only works for vectors and arrays (please report this as an error)")
        }
        res@processingLog <- processingLogAppend(
            res@processingLog,
            paste("initializeFlags(object, name=\"",
                name, "\", value=", valueOrig, ", debug)",
                sep = ""
            )
        )
    }
    oceDebug(debug, "END initializeFlagsInternal", sep = "", unindent = 1)
    res
}


#' @templateVar class oce
#'
#' @templateVar details There are no pre-defined `scheme`s for this object class.
#'
#' @template initializeFlagSchemeTemplate
setGeneric(
    "initializeFlagScheme",
    function(object, name = NULL, mapping = NULL, default = NULL, update = NULL, debug = 0) {
        standardGeneric("initializeFlagScheme")
    }
)

#' @templateVar class oce
#'
#' @templateVar details There are no pre-defined `scheme`s for this object class.
#'
#' @template initializeFlagSchemeTemplate
setMethod("initializeFlagScheme",
    signature = c(object = "oce", name = "ANY", mapping = "ANY", default = "ANY", update = "ANY", debug = "ANY"),
    definition = function(object, name, mapping, default, update, debug) {
        initializeFlagSchemeInternal(object, name, mapping, default, update, debug)
    }
)

#' @templateVar class oce
#' @templateVar details This is a low-level internal function used mainly by experts.
#' @template initializeFlagSchemeTemplate
initializeFlagSchemeInternal <- function(object, name = NULL, mapping = NULL, default = NULL, update = NULL, debug = 0) {
    oceDebug(debug, "initializeFlagSchemeInternal(object, name=\"", name, "\", debug=", debug, ") START", sep = "", unindent = 1)
    if (is.null(name)) {
        stop("must supply 'name'")
    }
    res <- object
    if (!is.null(object@metadata$flagScheme) && !(is.logical(update) && update)) {
        warning("cannot alter a flagScheme that is already is place")
    } else {
        # DEVELOPER NOTE: keep in synch with tests/testthat/test_flags.R and man-roxygen/initializeFlagScheme.R
        predefined <- c("argo", "BODC", "DFO", "WHP bottle", "WHP CTD")
        if (name %in% predefined) {
            if (!is.null(mapping)) {
                stop("cannot redefine the mapping for existing scheme named \"", name, "\"")
            }
            if (name == "argo") {
                # The argo mapping and default were changed in June 2020,
                # to accomodate new understanding of argo flags, developed
                # by Jaimie Harbin for the argoCanada/argoFloats project.  See
                # https://github.com/ArgoCanada/argoFloats/issues/133
                # https://github.com/dankelley/oce/issues/1705
                mapping <- list(
                    not_assessed = 0,
                    passed_all_tests = 1,
                    probably_good = 2,
                    probably_bad = 3,
                    bad = 4,
                    changed = 5,
                    not_used_6 = 6,
                    not_used_7 = 7, # until 2020-jun-10, named 'averaged'
                    estimated = 8, # until 2020-jun-10, named 'interpolated'
                    missing = 9
                )
                if (is.null(default)) {
                    # until 2020-jun-10, next was more cautious, namely
                    # default <- c(0, 2, 3, 4, 7, 8, 9) # retain passed_all_tests
                    default <- c(0, 3, 4, 9)
                }
            } else if (name == "BODC") {
                mapping <- list(
                    no_quality_control = 0, good = 1, probably_good = 2,
                    probably_bad = 3, bad = 4, changed = 5, below_detection = 6,
                    in_excess = 7, interpolated = 8, missing = 9
                )
                if (is.null(default)) {
                    default <- c(0, 2, 3, 4, 5, 6, 7, 8, 9) # retain good
                }
            } else if (name == "DFO") {
                mapping <- list(
                    no_quality_control = 0, appears_correct = 1, appears_inconsistent = 2,
                    doubtful = 3, erroneous = 4, changed = 5,
                    qc_by_originator = 8, missing = 9
                )
                if (is.null(default)) {
                    default <- c(0, 2, 3, 4, 5, 8, 9) # retain appears_correct
                }
            } else if (name == "WHP bottle") {
                mapping <- list(
                    no_information = 1, no_problems_noted = 2, leaking = 3,
                    did_not_trip = 4, not_reported = 5, discrepency = 6,
                    unknown_problem = 7, did_not_trip = 8, no_sample = 9
                )
                if (is.null(default)) {
                    default <- c(1, 3, 4, 5, 6, 7, 8, 9) # retain no_problems_noted
                }
            } else if (name == "WHP CTD") {
                mapping <- list(
                    not_calibrated = 1, acceptable = 2, questionable = 3,
                    bad = 4, not_reported = 5, interpolated = 6,
                    despiked = 7, missing = 9
                )
                if (is.null(default)) {
                    default <- c(1, 3, 4, 5, 6, 7, 9) # retain acceptable
                }
            } else {
                stop("internal coding error in initializeFlagSchemeInternal(); please report to developer")
            }
        } else {
            if (is.null(mapping)) {
                stop("must supply 'mapping' for new scheme named \"", name, "\"")
            }
        }
        res@metadata$flagScheme <- list(name = name, mapping = mapping, default = default)
    }
    res@processingLog <- processingLogAppend(
        res@processingLog,
        paste("initializeFlagScheme(object, name=\"", name,
            "\", mapping=",
            gsub(" ", "", paste(as.character(deparse(mapping)),
                sep = "", collapse = ""
            )),
            ")",
            ", default=c(", paste(default, collapse = ","), "))",
            sep = ""
        )
    )
    oceDebug(debug, "END initializeFlagSchemeInternal", sep = "", unindent = 1)
    res
}
