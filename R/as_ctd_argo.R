# vim:textwidth=80:expandtab:shiftwidth=4:softtabstop=4

#' Coerce argo Data Into a ctd Object
#'
#' Assemble argo data into a [ctd-class] object.  This function is normally
#' called by [as.ctd()], but can also be called directly. In the first
#' casefold(the only parameters provided by [as.ctd()] are the [argo-class]
#' object, called `o` here, the profile number, and the debugging controller
#'
#' @param argo an [argo-class object.size(
#'
#' @param profile an integer specifying the profile to pick within the argo
#' object. This will be set to 1, with a warning, if it is not supplied.
#'
#' @template debugTemplate
#'
#' @author Dan Kelley, with help from Clark Richards
#'
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
    if (!is.numeric(profile) || length(profile) != 1 || profile < 1) {
        stop("profile must be a positive integer")
    }
    if (profile > nprofiles) {
        stop("cannot handle profile=", profile, " because dataset has only ", nprofiles, " profiles")
    }
    # Pull out the startTime
    if (length(res@metadata$startTime) > 1) {
        res@metadata$startTime <- res@metadata$startTime[profile]
    }
    res@metadata$time <- as.POSIXct(argo@metadata$time[profile],
        format = "%Y-%m-%d %H:%M:%S UTC",
        tz = "UTC"
    )
    res@metadata$startTime <- res@metadata$time
    #<> longitude <- argo@data$longitude[profile]
    #<> oceDebug(debug, "extracted argo@data$longitude[", profile, "] as ctd@metadata$longitude=", longitude, "\n")
    #<> latitude <- o@data$latitude[profile]
    #<> oceDebug(debug, "extracted argo@data$latitude[", profile, "] as ctd@metadata$latitude=", latitude, "\n")
    # Extract metadata item, by profile. FIXME: do we capture all cases?
    #<> getItem <- function(argo, item, profile) {
    #<>     value <- argo[[item]]
    #<>     if (is.vector(value)) {
    #<>         value[profile]
    #<>     } else if (is.matrix(value)) {
    #<>         value[, profile]
    #<>     } else {
    #<>         value
    #<>     }
    #<> }
    #<> # FIXME: for issue 2270, add more items to copy
    #<> for (item in c("id", "dataMode")) {
    #<>     res@metadata[item] <- getItem(o@metadata, item, profile)
    #<>     oceDebug(debug, "extracted argo@metadata$", item, "[", profile, "] as ctd@metadata$", item, "\n", sep = "")
    #<> }
    #<> for (item in c("latitude", "longitude")) {
    #<>     res@metadata[item] <- getItem(o@data, item, profile)
    #<>     oceDebug(debug, "extracted argo@data$", item, "[", profile, "] as ctd@metadata$", item, "\n", sep = "")
    #<> }
    # Extract items for the requested profile
    for (field in names(argo@data)) {
        d <- argo@data[[field]]
        if (field %in% c("latitude", "longitude", "time")) {
            # oceDebug(debug, "skipping argo@data$", field, " since it was already installed in ctd@metadata\n", sep = "")
        } else {
            if (field == "mtime") { # FIXME: might be handled elsewhere (code needs cleanup)
                if (is.matrix(d)) {
                    ncol <- ncol(d)
                    if (profile > ncol) {
                        stop("profile cannot exceed ", ncol, " for a data matrix with ", ncol, " columns")
                    }
                    res@data[[field]] <- as.vector(d[, profile])
                } else {
                    res@data[[field]] <- as.vector(d)
                }
                res@data$time <- res@metadata$startTime + 86400 * res@data$mtime
            } else if (is.vector(d)) {
                oceDebug(debug, "extracting argo@data$", field, "[", profile, "] as ctd@data$", field, "\n")
                res@data[[field]] <- d[profile]
            } else if (is.matrix(d)) {
                oceDebug(debug, "extracting argo@data$", field, "[,", profile, "] as ctd@data$", field, "\n")
                res@data[[field]] <- d[, profile]
            } else if (is.array(d)) { # argo can sometimes come out this (odd) way
                # warning("argo data \"", field, "\" converted from 1-D array to 1-col matrix")
                oceDebug(debug, "extracting argo@data$", field, "[,,", profile, "] as ctd@data$", field, "\n")
                rank <- length(dim(d))
                if (rank == 3L) {
                    res@data[[field]] <- d[, , profile]
                } else if (rank == 4L) {
                    res@data[[field]] <- d[, , , profile]
                } else {
                    warning(
                        "cannot handle the argo@data$", field, " because it has dimension ",
                        paste(dim(d), collapse = "x")
                    )
                }
            } else {
                warning(field, "is not a vector, not a matrix and not an array; copying as-is")
                res@data[[field]] <- d[[field]]
            }
        }
    }
    #<> if (FALSE) {
    #<>     # Convert flags from array to vector (FIXME: needed?)
    #<>     if ("flags" %in% names(o@metadata)) {
    #<>         for (iflag in seq_along(o@metadata$flags)) {
    #<>             if (is.matrix(o@metadata$flags[[iflag]])) {
    #<>                 res@metadata$flags[[iflag]] <- o@metadata$flags[[iflag]][, profile]
    #<>             }
    #<>         }
    #<>     }
    #<> }
    # Copy metadata
    mnames <- names(argo[["metadata"]])
    for (mname in mnames) {
        oceDebug(debug, "handling argo@metadata$", mname, "\n", sep = "")
        m <- argo@metadata[[mname]]
        if (inherits(m, "POSIXt")) { # time is not stored as a vector in R
            if (nprofiles == length(m)) {
                oceDebug(debug, "copying argo@metadata$", mname, "[", profile, "]\n")
                res@metadata[[mname]] <- m[profile]
            } else {
                oceDebug(debug, "copying argo@metadata$", mname, " in its entirety\n")
                res@metadata[[mname]] <- m
            }
        } else if (mname %in% c(
            "dataNamesOriginal", "flags", "flagScheme",
            "parameter", "stationParameters", "units", "history"
        )) {
            oceDebug(debug, "copying argo@metadata$", mname, " directly\n")
            res@metadata[[mname]] <- m
        } else {
            if (is.vector(m)) {
                if (nprofiles == length(m)) {
                    oceDebug(debug, "copying argo@metadata$", mname, "[", profile, "]\n")
                    res@metadata[[mname]] <- m[profile]
                } else {
                    oceDebug(debug, "copying argo@metadata$", mname, " in its entirety\n")
                    res@metadata[[mname]] <- m
                }
            } else if (is.matrix(m)) {
                if (nprofiles == tail(dim(m), 1L)) {
                    oceDebug(debug, "copying argo@metadata$", mname, "[,", profile, "]\n")
                    res@metadata[[mname]] <- m[, profile]
                } else {
                    oceDebug(debug, "copying argo@metadata$", mname, " in its entirety\n")
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
                        oceDebug(debug, "copying argo@metadata$", mname, "[,", profile, "]\n")
                        res@metadata[[mname]] <- m[, profile, drop = FALSE]
                    } else {
                        oceDebug(debug, "copying argo@metadata$", mname, " in its entirety\n")
                        res@metadata[[mname]] <- m
                    }
                } else if (rank == 3L) {
                    if (canSelect) {
                        oceDebug(debug, "copying argo@metadata$", mname, "[,,", profile, "]\n")
                        res@metadata[[mname]] <- m[, , profile, drop = FALSE]
                    } else {
                        oceDebug(debug, "copying argo@metadata$", mname, " in its entirety\n")
                        res@metadata[[mname]] <- m
                    }
                } else if (rank == 4L) {
                    if (canSelect) {
                        oceDebug(debug, "copying argo@metadata$", mname, "[,,,", profile, "]\n")
                        res@metadata[[mname]] <- m[, , , profile, drop = FALSE]
                    } else {
                        oceDebug(debug, "copying argo@metadata$", mname, " in its entirety\n")
                        res@metadata[[mname]] <- m
                    }
                } else {
                    warning("copying argo@metadata$", mname, " in its entirety, since rank ", rank, " is not yet handled")
                }
            } else {
                warning("argo@metadata$", mname, " not copied (unknown type)\n")
            }
        }
    }
    res@processingLog <- argo@processingLog
    res@processingLog <- processingLogAppend(res@processingLog, paste(deparse(match.call()), sep = "", collapse = ""))
    oceDebug(debug, "END as.ctd.argo()\n", sep = "", unindent = 1)
    res
}
