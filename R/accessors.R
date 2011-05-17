## vim:textwidth=128:expandtab:shiftwidth=4:softtabstop=4
extract <- function(x, names)
{
    if (!inherits(x, "oce"))
        stop("method is only for oce objects")
    if (missing(x))
        stop("must supply 'x'")
    if (missing(names))
        stop("must supply 'names'")
    rval <- list()
    if (inherits(x, "section")) {
        for (name in names) {
            if (name %in% names(x)) {
                rval[[name]] <- x[[name]]
            } else if (name %in% names(x$metadata)) {
                if (name %in% c("longitude", "latitude", "stationId", "date")) {
                    item <- NULL
                    for (i in 1:length(x$data$station))
                        item <- c(item, rep(x$metadata[[name]][[i]], length(x$data$station[[i]]$data$salinity)))
                    rval[[name]] = item
                } else {
                    rval[[name]] = x$metadta[[name]]
                }
            } else if (name %in% names(x$data$station[[1]]$data)) {
                item <- NULL
                for (i in 1:length(x$data$station))
                    item <- c(item, x$data$station[[i]]$data[[name]])
                rval[[name]] <- item
            } else {
                stop("'", name, "' not in object's metadata or data$station[[1]]$data")
            }
        }
    } else if (inherits(x, "adp")) {
        for (name in names) {
            if (name %in% names(x)) {
                rval[[name]] <- x[[name]]
            } else if (name %in% names(x$metadata)) {
                rval[[name]] <-  x$metadata[[name]]
            } else if (name %in% names(x$data)) {
                rval[[name]] <- x$data[[name]]
            } else {
                stop("'", name, "' not in object")
            }
        }
    } else if (inherits(x, "adv")) {
        for (name in names) {
            if (name %in% names(x)) {
                rval[[name]] <- x[[name]]
            } else if (name %in% names(x$metadata)) {
                rval[[name]] <-  x$metadata[[name]]
            } else if (name %in% names(x$data)) {
                rval[[name]] <- x$data[[name]]
            } else {
                stop("'", name, "' not in object")
            }
        }
    } else {
        for (name in names) {
            if (name %in% names(x)) {
                rval[[name]] <- x[[name]]
            } else if (name %in% names(x$metadata)) {
                rval[[name]] = x$metadata[[name]]
            } else if (name %in% names(x$data)) {
                rval[[name]] = x$data[[name]]
            } else {
                stop("'", name, "' not in object")
            }
        }
    }
    rval
}

header <- function(x)
{
    if (!inherits(x, "oce"))
        stop("method is only for oce objects")
    return(x$metadata$header)
}

heading <- function(x, time)
{
    if (missing(x))
        stop("must supply 'x'")
    if (!missing(time) && inherits(time, "oce")) {
        if ("timeSlow" %in% names(time$data)) {
            time <- time$data$timeSlow
        } else if ("time" %in% names(time$data)) {
            time <- time$data$time
        } else {
            stop("cannot determine time to which to interpolate")
        }
    }
    if (inherits(x, "adp")) {
        if (missing(time)) {
            cat("x is adp, time not given\n")
            return(x$data$heading)
        } else {
            cat("x is adp, time given\n")
            return(approx(x$data$time, x$data$heading, time)$y)
        }
    } else if (inherits(x, "adv")) {
        if (missing(time)) {
            return(x$data$heading)
        } else {
            if ("timeSlow" %in% names(x$data)) {
                return(approx(x$data$timeSlow, x$data$headingSlow, time)$y)
            } else {
                return(approx(x$data$time, x$data$heading, time)$y)
            }
        }
    } else {
        stop("only works for 'adv' and 'adp' objects")
    }
}

"heading<-" <- function(x, value)
{
    if ("headingSlow" %in% names(x$data)) {
        x$data$headingSlow <- value
    } else if ("heading" %in% names(x$data)) {
        x$data$heading <- value
    } else {
        stop("object has no item named 'data$heading' or 'data$headingSlow'")
    }
    x
}

"pitch<-" <- function(x, value)
{
    if ("pitchSlow" %in% names(x$data)) {
        x$data$pitchSlow <- value
    } else if ("pitch" %in% names(x$data)) {
        x$data$pitch <- value
    } else {
        stop("object has no item named 'data$pitch' or 'data$pitchSlow'")
    }
    x
}

pitch <- function(x, time)
{
    if (missing(x))
        stop("must supply 'x'")
    if (!missing(time) && inherits(time, "oce")) {
        if ("timeSlow" %in% names(time$data)) {
            time <- time$data$timeSlow
        } else if ("time" %in% names(time$data)) {
            time <- time$data$time
        } else {
            stop("cannot determine time to which to interpolate")
        }
    }
    if (inherits(x, "adp")) {
        if (missing(time)) {
            return(x$data$pitch)
        } else {
            return(approx(x$data$time, x$data$pitch, time)$y)
        }
    } else if (inherits(x, "adv")) {
        if (missing(time)) {
            return(x$data$pitch)
        } else {
            if ("timeSlow" %in% names(x$data)) {
                return(approx(x$data$timeSlow, x$data$pitchSlow, time)$y)
            } else {
                return(approx(x$data$time, x$data$pitch, time)$y)
            }
        }
    } else {
        stop("only works for 'adv' and 'adp' objects")
    }
}

roll <- function(x, time)
{
    if (missing(x))
        stop("must supply 'x'")
    if (!missing(time) && inherits(time, "oce")) {
        if ("timeSlow" %in% names(time$data)) {
            time <- time$data$timeSlow
        } else if ("time" %in% names(time$data)) {
            time <- time$data$time
        } else {
            stop("cannot determine time to which to interpolate")
        }
    }
    if (inherits(x, "adp")) {
        if (missing(time)) {
            return(x$data$roll)
        } else {
            return(approx(x$data$time, x$data$roll, time)$y)
        }
    } else if (inherits(x, "adv")) {
        if (missing(time)) {
            return(x$data$roll)
        } else {
            if ("timeSlow" %in% names(x$data)) {
                return(approx(x$data$timeSlow, x$data$rollSlow, time)$y)
            } else {
                return(approx(x$data$time, x$data$roll, time)$y)
            }
        }
    } else {
        stop("only works for 'adv' and 'adp' objects")
    }
}

"roll<-" <- function(x, value)
{
    if ("rollSlow" %in% names(x$data)) {
        x$data$rollSlow <- value
    } else if ("roll" %in% names(x$data)) {
        x$data$roll <- value
    } else {
        stop("object has no item named 'data$roll' or 'data$rollSlow'")
    }
    x
}

time.oce <- function(x, ...)
{
    which <- if ("which" %in% names(list(...))) list(...)$which else 1
    if (inherits(x, "adp")) {
        res <- x$data$time
    } else if (inherits(x, "adv")) {
        if (which == 1) {
            res <- x$data$time
        } else if (which == 2) {
            names <- names(x$data)
            if ("timeSlow" %in% names)
                res <- x$data$timeSlow
            else
                res <- x$data$time
        } else {
            stop("unknown 'which'; must be 1 for ADP velocity timescale, or 2 for ADP heading timescale")
        }
    } else {
        stop("'x' must be an ADV or ADP object")
    }
    res
}

pressure <- function(x)
{
    if (!inherits(x, "oce"))
        stop("'x' must be an oce object")
    if ("pressure" %in% names(x$data))
        return(x$data$pressure)
    if (inherits(x, "section")) {
        pressure <- NULL
        for (s in seq_along(x$data$station)) {
            pressure <- c(pressure, x$data$station[[s]]$data$pressure)
        }
        return(pressure)
    }
    stop("cannot find pressure in 'x'")
}

salinity <- function(x)
{
    if (!inherits(x, "oce"))
        stop("'x' must be an oce object")
    if ("salinity" %in% names(x$data))
        return(x$data$salinity)
    if (inherits(x, "section")) {
        salinity <- NULL
        for (s in seq_along(x$data$station)) {
            salinity <- c(salinity, x$data$station[[s]]$data$salinity)
        }
        return(salinity)
    }
    stop("cannot find salinity in 'x'")
}

temperature <- function(x)
{
    if (!inherits(x, "oce"))
        stop("'x' must be an oce object")
    if ("temperature" %in% names(x$data))
        return(x$data$temperature)
    if (inherits(x, "section")) {
        temperature <- NULL
        for (s in seq_along(x$data$station)) {
            temperature <- c(temperature, x$data$station[[s]]$data$temperature)
        }
        return(temperature)
    }
    stop("cannot find temperature in 'x'")
}

velocity <- function(x)
{
    if (!inherits(x, "oce"))
        stop("'x' must be an oce object")
    if (!("v" %in% names(x$data)))
        stop("'x' does not contain 'data$v'")
    return(x$data$v)
}
