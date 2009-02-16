ctd.trim <- function(x, method="downcast", parameters, verbose=FALSE)
{
    if (!inherits(x, "ctd")) stop("method is only for ctd objects")
    result <- x
    n <- length(x$data$pressure)
    if (n < 2) {
        warning("too few data to ctd.trim()")
    } else {
        which.method <- pmatch(method, c("index", "downcast"), nomatch=0)
        ##if (verbose) cat("ctd.trim()\n  using method", which.method,"\n")
        keep <- rep(TRUE, n)
        if (which.method == 1) {        # "index"
            ##if (verbose)	cat("  parameters:",parameters,"\n");
            if (min(parameters) < 1)
                stop("Cannot select indices < 1");
            if (max(parameters) > n)
                stop(paste("Cannot select past end of array, i.e. past ", n))
            keep <- rep(FALSE, n)
            keep[parameters] <- TRUE
        } else if (which.method == 2) { # "downcast"
                                        # 1. despike to remove (rare) instrumental problems
            x$data$pressure <- smooth(x$data$pressure,kind="3R")
            pmin <- 0
            if (!missing(parameters)) {
                if ("pmin" %in% names(parameters)) pmin <- parameters$pmin else stop("parameter not understood for this method")
            }
            keep <- (x$data$pressure > pmin) # 2. in water (or below start depth)
            delta.p <- diff(x$data$pressure)  # descending
            delta.p <- c(delta.p[1], delta.p) # to get right length
            keep <- keep & (delta.p > 0)
                                        # 3. trim the upcast and anything thereafter (ignore beginning and end)
            trim.top <- as.integer(0.1*n)
            trim.bottom <- as.integer(0.9*n)
            max.spot <- which.max(smooth(x$data$pressure[trim.top:trim.bottom],kind="3R"))
            max.location <- trim.top + max.spot
            keep[max.location:n] <- FALSE
            ##if (verbose) cat("  pressure maximum at index=",max.spot,"\n")
            if (FALSE) {
                                        # deleted method: slowly-falling data
                delta.p.sorted <- sort(delta.p)
                if (!is.null(parameters)) {
                    dp.cutoff <- t.test(delta.p[keep], conf.level=0.5)$conf.int[1]
                    print(t.test(delta.p[keep], conf.level=0.05))#$conf.int[1]
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
                bilinear1 <- function(s, s0, dpds) {
                    ifelse(s < s0, 0, dpds*(s-s0))
                }
                pp <- x$data$pressure[keep]
                ss <- x$data$scan[keep]
                ##if (verbose) plot(ss,pp,ylim=rev(range(pp)))
                p0 <- 0
                s0 <- ss[0.25*length(ss)]
                p0 <- pp[1]
                p1 <- max(pp) #pp[0.9*length(pp)]
                dpds0 <-  diff(range(pp)) / diff(range(ss))
                t <- try(m <- nls(pp ~ bilinear1(ss, s0, dpds),
                                  start=list(s0=s0, dpds=dpds0)),
                         silent=TRUE)
                if (class(t) != "try-error") {
                    if (m$convInfo$isConv) {
                        s0 <- floor(coef(m)[[1]])
                        ##if (verbose) cat("  trimming scan numbers below", s0, "\n")
                        ##if (verbose) print(summary(m))
                        keep <- keep & (x$data$scan > (coef(m)[[1]]))
                    }
                } else {
                    warning("unable to complete step 5 of the trim operation (removal of initial equilibrium phase)")
                }
                if (verbose) cat("ctd.trim(read.oce(\"", x$metadata$filename, "\"), \"scan\", c(", min(x$data$scan[keep],na.rm=TRUE), ",", max(x$data$scan[keep],na.rm=TRUE),"))\n",sep="")
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
    result <- processing.log.append(result, paste(deparse(match.call()), sep="", collapse=""))
    result
}
