# vim:textwidth=80:expandtab:shiftwidth=4:softtabstop=4

#' Coerce argo Data Into a ctd Object
#'
#' Assemble argo data into a [ctd-class] object.  This function may be
#' called by [as.ctd()] or called directly. In the first
#' case, note that the only [as.ctd()] parameters that are provide
#' are the object, the profile number, and the debug value.
#'
#' @param argo an [argo-class] object.
#'
#' @param profile an integer specifying the profile to pick within the argo
#' object. This will be set to 1, with a warning, if it is not supplied.
#'
#' @template debugTemplate
#'
#' @author Dan Kelley
#'
#' @family things related to argo data
#' @family things related to ctd data
as.ctd.argo <- function(argo, profile = NULL, debug = getOption("oceDebug")) {
    if (!inherits(argo, "argo")) {
        stop("first parameter must be an argo-class object")
    }
    res <- new("ctd")
    oceDebug(debug, "as.ctd.argo(argo, profile = ", profile, ") START\n", sep = "", unindent = 1)
    nprofiles <- tail(dim(argo[["pressure"]]), 1L)
    oceDebug(debug, "this dataset contains ", nprofiles, " profiles\n")
    if (is.null(profile)) {
        profile <- 1
        if (nprofiles != 1) {
            warning("since 'profile' not given, defaulting to 1")
        }
    }
    if (!is.numeric(profile) || length(profile) != 1L || profile < 1) {
        stop("profile must be a positive integer, but it is ", profile)
    }
    if (profile > nprofiles) {
        stop("cannot handle profile=", profile, " because dataset has only ", nprofiles, " profiles")
    }
    res@metadata$time <- as.POSIXct(argo@metadata$time[profile],
        format = "%Y-%m-%d %H:%M:%S UTC",
        tz = "UTC"
    )
    # Extract items for the requested profile
    oceDebug(debug, "STEP 1: move argo@data items to ctd@data (unless otherwise stated)\n")
    for (field in names(argo@data)) {
        # oceDebug(debug, "working on argo@data$", field, "\n", sep="")
        d <- argo@data[[field]]
        # Move some things from argo@data to ctd@metadata, for historical reasons
        if (field %in% c("longitude", "latitude", "time")) {
            res@metadata[[field]] <- d[profile]
            oceDebug(debug, "  argo@data$", field, " -> ctd@metadata$", field, "\n", sep = "")
        } else if (is.vector(d)) {
            oceDebug(debug, "  argo@data$", field, "[", profile, "\n")
            res@data[[field]] <- d[profile]
        } else if (is.matrix(d)) {
            oceDebug(debug, "  argo@data$", field, "[,", profile, "]\n")
            res@data[[field]] <- d[, profile]
        } else if (is.array(d)) {
            rank <- length(dim(d))
            if (rank == 3L) {
                res@data[[field]] <- d[, , profile]
                oceDebug(debug, "  argo@data$", field, "[,,", profile, "]\n")
            } else if (rank == 4L) {
                res@data[[field]] <- d[, , , profile]
                oceDebug(debug, "  argo@data$", field, "[,,,", profile, "]\n")
            } else {
                warning(
                    "cannot handle the argo@data$", field, " because it has dimension ",
                    paste(dim(d), collapse = "x")
                )
            }
        } else {
            warning("argo@data$", field, " is not a vector, matrix, or array; copying it as-is")
            res@data[[field]] <- d[[field]]
        }
    } # extracting argo@data
    oceDebug(debug, "STEP 2: extract argo@metadata items to ctd@metadata\n")
    for (mname in names(argo[["metadata"]])) {
        # oceDebug(debug, "working on argo@metadata$", mname, "\n", sep = "")
        m <- argo@metadata[[mname]]
        if (inherits(m, "POSIXt")) { # time is not stored as a vector in R
            if (nprofiles == length(m)) {
                oceDebug(debug, "  argo@metadata$", mname, "[", profile, "]\n")
                res@metadata[[mname]] <- m[profile]
            } else {
                oceDebug(debug, "  argo@metadata$", mname, "\n")
                res@metadata[[mname]] <- m
            }
        } else if (mname %in% c(
            "dataNamesOriginal", "flagScheme",
            "parameter", "stationParameters", "units", "history"
        )) {
            oceDebug(debug, "  argo@metadata$", mname, "\n")
            res@metadata[[mname]] <- m
        } else if (mname == "flags") {
            res@metadata$flags <- list()
            oceDebug(debug, "  argo@metadata$flags\n")
            for (fname in names(m)) {
                if (is.array(m[[fname]])) {
                    oceDebug(debug, "    argo@metadata$flags$", fname, "[, ", profile, "]\n", sep = "")
                    res@metadata$flags[[fname]] <- m[[fname]][, profile]
                } else {
                    oceDebug(debug, "    argo@metadata$flags$", fname, "\n", sep = "")
                    res@metadata$flags[[fname]] <- m[[fname]]
                }
            }
        } else {
            if (is.vector(m)) {
                if (nprofiles == length(m)) {
                    oceDebug(debug, "  argo@metadata$", mname, "[", profile, "]\n")
                    res@metadata[[mname]] <- m[profile]
                } else {
                    oceDebug(debug, "  argo@metadata$", mname, "\n")
                    res@metadata[[mname]] <- m
                }
            } else if (is.matrix(m)) {
                if (nprofiles == tail(dim(m), 1L)) {
                    oceDebug(debug, "  argo@metadata$", mname, "[,", profile, "]\n")
                    res@metadata[[mname]] <- m[, profile]
                } else {
                    oceDebug(debug, "  argo@metadata$", mname, "\n")
                    res@metadata[[mname]] <- m
                }
            } else if (is.array(m)) {
                dimm <- dim(m)
                # cat("next is dimm\n")
                # print(dimm)
                rank <- length(dimm)
                # cat("rank=",rank,"\n")
                canSelect <- nprofiles == tail(dimm, 1L)
                # cat("canSelect=",canSelect,"\n")
                if (rank == 2L) {
                    if (canSelect) {
                        oceDebug(debug, "cop  argo@metadata$", mname, "[,", profile, "]\n")
                        res@metadata[[mname]] <- m[, profile, drop = FALSE]
                    } else {
                        oceDebug(debug, "cop  argo@metadata$", mname, " in its entirety\n")
                        res@metadata[[mname]] <- m
                    }
                } else if (rank == 3L) {
                    if (canSelect) {
                        oceDebug(debug, "  argo@metadata$", mname, "[,,", profile, "]\n")
                        res@metadata[[mname]] <- m[, , profile, drop = FALSE]
                    } else {
                        oceDebug(debug, "  argo@metadata$", mname, "\n")
                        res@metadata[[mname]] <- m
                    }
                } else if (rank == 4L) {
                    if (canSelect) {
                        oceDebug(debug, "  argo@metadata$", mname, "[,,,", profile, "]\n")
                        res@metadata[[mname]] <- m[, , , profile, drop = FALSE]
                    } else {
                        oceDebug(debug, "  argo@metadata$", mname, "\n")
                        res@metadata[[mname]] <- m
                    }
                } else {
                    warning("copying argo@metadata$", mname, " in its entirety, since rank ", rank, " is not yet handled")
                }
            } else {
                warning("argo@metadata$", mname, " not copied (unknown type)\n")
            }
        }
    } # extracting argo@metadata
    res@processingLog <- argo@processingLog
    res@processingLog <- processingLogAppend(res@processingLog, paste(deparse(match.call()), sep = "", collapse = ""))
    oceDebug(debug, "END as.ctd.argo()\n", sep = "", unindent = 1)
    res
}
