rbrtdr.trim <- function(x, method="median", parameters=NULL, verbose=FALSE)
{
    if (!inherits(x, "rbrtdr"))
        stop("method is only for rbrtdr objects")
    result <- x
    n <- length(x$data$temperature)
    if (verbose) cat("n=",n,"\n")
    if (n < 2) {
        warning("too few data to trim.ctd()")
    } else {
        which.method <- pmatch(method, c("median", "time", "index"), nomatch=0)
        if (verbose) cat("using method", which.method,"\n")
        keep <- rep(TRUE, n)
        if (which.method == 1) {        # "median"
            keep[1:3] <- FALSE       #BUG: faking it!
            keep[(n-3):n] <- FALSE
            #print(keep)
        } else if (which.method == 2) { # "time"
            if (verbose)	cat("parameters:",parameters,"\n");
            keep[x$data$t < parameters[1]] <- FALSE
            keep[x$data$t > parameters[2]] <- FALSE
        } else if (which.method == 3) { # "index"
            if (verbose)	cat("parameters:",parameters,"\n");
            if (min(parameters) < 1)
                stop("Cannot select indices < 1");
            if (max(parameters) > n)
                stop(paste("Cannot select past end of array, i.e. past ", n))
            keep[parameters[1]:parameters[2]] <- TRUE
        } else {
            stop("Unknown method")
        }
    }
    result$data <- subset(x$data, keep)
    if (is.null(parameters)) {
        result <- processing.log.append(result,	paste("modified by rbrtdr.trim(x, method=\"",method,"\")",sep=""))
    } else {
        result <- processing.log.append(result,	paste("modified by rbrtdr.trim(x, method=\"",method,"\",parameters=",parameters,")",sep=""))
    }
    return(result)
}
