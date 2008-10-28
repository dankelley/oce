ctd.trim <- function(x, method="downcast", parameters=NULL, verbose=FALSE)
{
    if (!inherits(x, "ctd")) stop("method is only for ctd objects")
    result <- x
    n <- length(x$data$pressure)
    if (n < 2) {
        warning("too few data to ctd.trim()")
    } else {
        which.method <- pmatch(method, c("index", "downcast"), nomatch=0)
        if (verbose) cat("using method", which.method,"\n")
        keep <- rep(TRUE, n)
        if (which.method == 1) {        # "index"
            if (verbose)	cat("parameters:",parameters,"\n");
            if (min(parameters) < 1)
                stop("Cannot select indices < 1");
            if (max(parameters) > n)
                stop(paste("Cannot select past end of array, i.e. past ", n))
            keep <- rep(FALSE, n)
            keep[parameters] <- TRUE
        } else if (which.method == 2) { # "downcast"
                                        # 1. despike to remove (rare) instrumental problems
            x$data$pressure <- smooth(x$data$pressure,kind="3R")
            keep <- (x$data$pressure > 0)
                                        # 2. in-water, descending
            delta.p <- diff(x$data$pressure)
            delta.p <- c(delta.p[1], delta.p) # to get right length
            keep <- keep & (delta.p > 0)
                                        # 3. trim the upcast and anything thereafter (ignore beginning and end)
            trim.top <- as.integer(0.1*n)
            trim.bottom <- as.integer(0.9*n)
            max.spot <- which.max(smooth(x$data$pressure[trim.top:trim.bottom],kind="3R"))
            max.location <- trim.top + max.spot
            keep[max.location:n] <- FALSE
            if (FALSE) {
                                        # deleted method: slowly-falling data
                delta.p.sorted <- sort(delta.p)
                if (!is.null(parameters)) {
                    dp.cutoff <- t.test(delta.p[keep], conf.level=parameters[1])$conf.int[1]
                } else {
                    dp.cutoff <- delta.p.sorted[0.1*n]
                }
                keep[delta.p < dp.cutoff] <- FALSE
            }
                                        # 4. remove equilibration phase
            if (FALSE) {                # old method, prior to Feb 2008
                pp <- x$data$pressure[keep]
                ss <- x$data$scan[keep]
                equilibration <- (predict(m <- lm(pp ~ ss), newdata=list(ss=x$data$scan)) < 0)
                keep[equilibration] <- FALSE
            }
            if (TRUE) {                 # new method, after Feb 2008
                bilinear <- function(s, s0, dpds) {
                    ifelse(s < s0, 0, dpds*(s-s0)) # note: get errors if fit for initial pressure
                }
                pp <- x$data$pressure[keep]
                ss <- x$data$scan[keep]
                p0 <- 0
                s0 <- ss[0.5*length(ss)]
                dpds0 <-  diff(range(pp)) / diff(range(ss))
                t <- try(m <- nls(pp ~ bilinear(ss, s0, dpds), start=list(s0=s0, dpds=dpds0)), TRUE)
                if (class(t) != "try-error") {
                    if (m$convInfo$isConv) {
                        s0 <- coef(m)[[1]]
                        keep <- keep & (x$data$scan > (coef(m)[[1]]))
                    }
                } else {
                    warning("unable to complete step 5 of the trim operation (removal of initial equilibrium phase)")
                }
            }
        } else {
            if (verbose)	cat(paste("column",method,"; parameters ", parameters[1], parameters[2]))
            l <- length(parameters)
            if (l == 1) { 		# lower limit
                keep <- (x$data[[method]] > parameters[1]);
            } else if (l == 2) {	# lower and upper limits
                keep <- (x$data[[method]] > parameters[1]) & (x$data[[method]] < parameters[2])
            }
        }
    }
    result$data <- subset(x$data, keep)
    if (is.null(parameters)) {
        result <- processing.log.append(result,	paste("modified by ctd.trim(x, method=\"",method,"\")",sep=""))
    } else {
        result <- processing.log.append(result,	paste("modified by ctd.trim(x, method=\"",method,"\",parameters=",parameters,")",sep=""))
    }
    return(result)
}
