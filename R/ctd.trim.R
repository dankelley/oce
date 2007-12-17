ctd.trim <- function(x, method="downcast", parameters=NULL, verbose=FALSE)
{
	if (!inherits(x, "ctd"))
		stop("method is only for ctd objects")
	result <- x
	n <- length(x$data$pressure)
	if (n < 2) {
		warning("too few data to trim.ctd()")
	} else {
		keep <- rep(TRUE, n)
		if (method == "index") {
			if (verbose)	cat("parameters:",parameters,"\n");
			if (min(parameters) < 1)
				stop("Cannot select indices < 1");
			if (max(parameters) > n)
				stop(paste("Cannot select past end of array, i.e. past ", n))
			keep <- rep(FALSE, n)
			keep[parameters] <- TRUE
		} else if (method == "downcast") {		# BUG: this is crude
			# 1. despike to remove (rare) instrumental problems
			x$data$pressure <- smooth(x$data$pressure,kind="3R")
			# 2. keep only in-water data
			keep <- (x$data$pressure > 0)
			# 3. keep only descending data
			delta.p <- diff(x$data$pressure)
			delta.p <- c(delta.p[1], delta.p) # to get right length
			keep <- keep & (delta.p > 0)
			if (FALSE) {
				# 3old. trim the upcast and anything thereafter (ignore beginning and end)
				trim.top <- as.integer(0.1*n)
				trim.bottom <- as.integer(0.9*n)
				max.spot <- which.max(smooth(x$data$pressure[trim.top:trim.bottom],kind="3R"))
				max.location <- trim.top + max.spot
				keep[max.location:n] <- FALSE
			}
			# 4. trim a possible near-surface equilibration phase
			delta.p.sorted <- sort(delta.p)
			if (!is.null(parameters)) {
				dp.cutoff <- t.test(delta.p[keep], conf.level=parameters[1])$conf.int[1]
			} else {
				dp.cutoff <- delta.p.sorted[0.1*n]
			}
			# 4a. remove equilibration data that have very little drop speed
			keep[delta.p < dp.cutoff] <- FALSE
			# 4b. remove more equilibration data by regression
			pp <- x$data$pressure[keep]
			ss <- x$data$scan[keep]
			equilibration <- (predict(m <- lm(pp ~ ss), newdata=list(ss=x$data$scan)) < 0)
			keep[equilibration] <- FALSE
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
		result <- log.append(result,	paste("modified by ctd.trim(x, method=\"",method,"\")",sep=""))
	} else {
		result <- log.append(result,	paste("modified by ctd.trim(x, method=\"",method,"\",parameters=",parameters,")",sep=""))
	}
	return(result)
}
