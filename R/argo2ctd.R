# vim:textwidth=80:expandtab:shiftwidth=4:softtabstop=4

argo2ctd <- function(o, profile = NULL, debug = getOption("oceDebug")) {
    if (!inherits(o, "argo")) stop("first parameter must be an argo-class object")
    res <- new("ctd")
    oceDebug(debug, "argo2ctd(o, profile = ", profile, ") START\n", sep = "", unindent = 1)
    nprofiles <- tail(dim(o[["pressure"]]), 1L)
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
    cat("metadata names\n")
    print(sort(names(o@metadata)))
    print(o@metadata$time)
    res@metadata$time <- as.POSIXct(o@metadata$time[profile],
        format = "%Y-%m-%d %H:%M:%S UTC",
        tz = "UTC"
    )
    res@metadata$startTime <- res@metadata$time
    cat("data names\n")
    print(sort(names(o@data)))
    longitude <- o@data$longitude[profile]
    oceDebug(debug, "extracted argo@data$longitude[", profile, "] as ctd@metadata$longitude=", longitude, "\n")
    latitude <- o@data$latitude[profile]
    oceDebug(debug, "extracted argo@data$latitude[", profile, "] as ctd@metadata$latitude=", latitude, "\n")
    # Extract metadata item, by profile. FIXME: do we capture all cases?
    getItem <- function(o, item, profile) {
        value <- o[[item]]
        if (is.vector(value)) {
            value[profile]
        } else if (is.matrix(value)) {
            value[, profile]
        } else {
            value
        }
    }
    # FIXME: for issue 2270, add more items to copy
    for (item in c("id", "dataMode")) {
        res@metadata[item] <- getItem(o@metadata, item, profile)
        oceDebug(debug, "extracted argo@metadata$", item, "[", profile, "] as ctd@metadata$", item, "\n", sep = "")
    }
    for (item in c("latitude", "longitude")) {
        res@metadata[item] <- getItem(o@data, item, profile)
        oceDebug(debug, "extracted argo@data$", item, "[", profile, "] as ctd@metadata$", item, "\n", sep = "")
    }

    d <- o@data
    # Convert data items from array to vector
    for (field in names(d)) {
        dataInField <- d[[field]]
        if (field %in% c("latitude", "longitude", "time")) {
            #oceDebug(debug, "skipping argo@data$", field, " since it was already installed in ctd@metadata\n", sep = "")
        } else {
            oceDebug(debug, "extracting argo@data$", field, " to ctd@data$", field, "\n", sep = "")
            # in argo objects there are both matrix (temperature,
            # salinity, etc) and vector (time, latitude, etc)
            # data fields. For the former we want to extract the
            # single column. For the longitude and latitude we extract a
            # single value.  We do that also for time, storing the single
            # value in the `metadata` slot, *unless* MTIME is
            # defined, in which case we can construct a full time vector and
            # place it in the 'data` slot.
            if (field == "mtime") {
                if (is.matrix(dataInField)) {
                    ncol <- ncol(d[[field]])
                    if (profile > ncol) {
                        stop("profile cannot exceed ", ncol, " for a data matrix with ", ncol, " columns")
                    }
                    res@data[[field]] <- as.vector(d[[field]][, profile])
                } else {
                    res@data[[field]] <- as.vector(d[[field]])
                }
                res@data$time <- res@metadata$startTime + 86400 * res@data$mtime
            } else if (is.vector(dataInField)) {
                ncol <- length(d[[field]])
                if (profile > ncol) {
                    stop("profile cannot exceed ", ncol, " for a data matrix with ", ncol, " columns")
                }
                oceDebug(debug, "FIXME: extracting argo@data$", field, "[", profile, "] as ctd@data$", field, "\n")
            } else if (is.matrix(dataInField)) {
                ncol <- ncol(d[[field]])
                if (profile > ncol) {
                    stop("profile cannot exceed ", ncol, " for a data matrix with ", ncol, " columns")
                }
                res@data[[field]] <- d[[field]][, profile]
                oceDebug(debug, "FIXME: extracting argo@data$", field, "[,", profile, "] as ctd@data$", field, "\n")
            } else if (is.array(dataInField)) { # argo can sometimes come out this (odd) way
                # warning("argo data \"", field, "\" converted from 1-D array to 1-col matrix")
                if (1 == length(dim(d[[field]]))) {
                    d[[field]] <- as.vector(d[[field]])
                }
                res@data[[field]] <- d[[field]]
            } else {
                if (1 == length(dim(d[[field]]))) {
                    d[[field]] <- as.vector(d[[field]])
                }
                res@data[[field]] <- d[[field]]
            }
        }
    }
    # Convert flags from array to vector
    if ("flags" %in% names(res@metadata)) {
        for (iflag in seq_along(res@metadata$flags)) {
            if (is.matrix(res@metadata$flags[[iflag]])) {
                res@metadata$flags[[iflag]] <- res@metadata$flags[[iflag]][, profile]
            }
        }
    }
    res@processingLog <- processingLogAppend(res@processingLog, paste(deparse(match.call()), sep = "", collapse = ""))
    oceDebug(debug, "END argo2ctd()\n", sep = "", unindent = 1)
    res
}
