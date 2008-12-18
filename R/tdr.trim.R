tdr.trim <- function(x, method="water", parameters=NULL, verbose=FALSE)
{
    if (!inherits(x, "tdr")) stop("method is only for tdr objects")
    result <- x
    n <- length(x$data$temperature)
    if (verbose) cat("tdr.trim() working on dataset with", n, "points\n")
    if (n < 2) {
        warning("too few data to tdr.trim()")
    } else {
        which.method <- pmatch(method, c("water", "time", "index"), nomatch=0)
        if (verbose) cat("using method", which.method,"\n")
        if (which.method == 1) {        # "water"
            keep <- rep(FALSE, n)
            air <- x$data$pressure < 10.5 # NB. standard pressure is 10.1325
            water.indices <- which(!air)
            b <- 2                      # trim a few descending points
            i.start <- water.indices[1] + b
            i.stop <- water.indices[-b + length(water.indices)]
            keep[i.start:i.stop] <- TRUE
            cat("The mean (deleted) air pressure is", mean(x$data$pressure[air]),"dbar\n")
        } else if (which.method == 2) { # "time"
            if (verbose)	cat("trimming to time range ",as.character(parameters[1])," to ", as.character(parameters[2]), "\n");
            keep <- rep(TRUE, n)
            keep[x$data$t < as.POSIXlt(parameters[1])] <- FALSE
            keep[x$data$t > as.POSIXlt(parameters[2])] <- FALSE
        } else if (which.method == 3) { # "index"
            if (verbose)	cat("parameters:",parameters,"\n");
            if (min(parameters) < 1)
                stop("Cannot select indices < 1");
            if (max(parameters) > n)
                stop(paste("Cannot select past end of array, i.e. past ", n))
            keep <- rep(FALSE, n)
            keep[parameters[1]:parameters[2]] <- TRUE
        } else {
            stop("Unknown method")
        }
    }
    result$data <- subset(x$data, keep)
    result <- processing.log.append(result, paste(deparse(match.call()), sep="", collapse=""))
    result
}
