rbrtdr.trim <- function(x, method="median", parameters=NULL, verbose=FALSE)
{
    if (!inherits(x, "rbrtdr"))
        stop("method is only for rbrtdr objects")
    result <- x
    n <- length(x$data$temperature)
    if (verbose) cat("n=",n,"\n")
    if (n < 2) {
        warning("too few data to rbrtdr.trim()")
    } else {
        which.method <- pmatch(method, c("median", "time", "index"), nomatch=0)
        if (verbose) cat("using method", which.method,"\n")
        if (which.method == 1) {        # "median"
            keep <- rep(TRUE, n)
            keep[1:3] <- FALSE       #BUG: faking it!
            keep[(n-3):n] <- FALSE
            #print(keep)
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
    if (is.null(parameters)) {
        result <- processing.log.append(result,	paste("modified by rbrtdr.trim(x, method=\"",method,"\")",sep=""))
    } else {
        pp <- as.character(parameters)
        p <- paste("c(\"", pp, sep="")
        for (i in 2:length(pp)) {
            p <- paste(p, pp[i], sep="\",\"")
        }
        p <- paste(p, "\")",sep="")     # BUG: broken; should do this above, anyway.  Surely there is a general solution
        result <- processing.log.append(result,	paste("modified by rbrtdr.trim(x, method=\"",method,"\",parameters=",p,")",sep=""))
    }
    return(result)
}
