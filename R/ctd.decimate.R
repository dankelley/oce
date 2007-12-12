ctd.decimate <- function(x, p, method=c("boxcar","lm"), e=1)
# ADD: BIO method; spline; supsmu; ...
{
  	if (!inherits(x, "ctd"))
    	stop("method is only for ctd objects")
  	res <- x
  	n <- length(x$data$pressure)
  	if (n < 2) { 
		warning("too few data to trim.decimate()")
		return(res)
	}
	# Figure out pressure targets, pt
	if (missing(p)) {
		# autoscale
		dp.exact <- median(abs(diff(x$data$pressure)))
		dp <- pretty(3 * dp.exact)[2] # try for 3 data at least
		pt <- seq(0, dp * floor(max(x$data$pressure) / dp), dp)
		log.item <- paste("modified by ctd.decimate(x, p=NULL, method=\"", method[1], "\")",sep="")
	} else {
		if (length(p) == 1) {
			pt <- seq(0, p * floor(max(x$data$pressure) / p), p)
			log.item <- paste("modified by ctd.decimate(x, p=",p[1],", method=\"",method[1], "\")",sep="")
		} else {
			pt <- p
			log.item <- paste("modified by ctd.decimate(x, p=c(",p[1],",",p[2],",...), method=\"",method[1], "\")",sep="")
		}
	}
	npt <- length(pt)
	# Step through each variable.
	data.names <- names(x$data)
	cat("A\n")
	for (datum.name in data.names) {
		cat("B\n")
		if (datum.name != "pressure") {
			cat("C1\n")
			cat(datum.name, ":")
			length(res$data[[datum.name]]) <- npt
			cat("C2\n")
			res$data[[datum.name]] <- approx(x$data[["pressure"]], x$data[[datum.name]], pt)$y
			cat("C3\n")
		}
	}
	# Now replace pressure
#	length(res$data[["pressure"]]) <- npt
	res$data[["pressure"]][1:npt] <- pt
	res <- processing.log.append(res, log.item)
	return(res)
	stop()
  	if (n < 2) { 
		warning("too few data to trim.decimate()")
  	} else {
	  	method <- match.arg(method)
		n <- length(pout)
		# FIXME: should probably do this based on names(x$data)
		# FIXME: maybe should do this with apply()???
		scan <- NULL
		pressure <- NULL
		depth <- NULL
		temperature <- NULL
		salinity <- NULL
		flag <- NULL
		sigma.theta <- NULL
		for (i in 1:n) {
			if (i==1) {
				focus <- (x$data$pressure >= (pout[i] - e * (pout[i + 1] - pout[  i  ]))) &
				 	     (x$data$pressure <= (pout[i] + e * (pout[i + 1] - pout[  i  ])))
			} else if (i == n) {
				focus <- (x$data$pressure >= (pout[i] - e * (pout[  i  ] - pout[i - 1]))) &
				 	     (x$data$pressure <= (pout[i] + e * (pout[  i  ] - pout[i - 1])))
			} else {
				focus <- (x$data$pressure >= (pout[i] - e * (pout[  i  ] - pout[i - 1]))) &
					     (x$data$pressure <= (pout[i] + e * (pout[i + 1] - pout[  i  ])))
			}
			if (sum(focus) > 0) {
				if (method == "boxcar") {
					pressure    <- c(pressure,    pout[i])
					scan        <- c(scan,        mean(x$data$scan[focus],na.rm=TRUE))
					depth       <- c(depth,       mean(x$data$depth[focus],na.rm=TRUE))
					temperature <- c(temperature, mean(x$data$temperature[focus],na.rm=TRUE))
					salinity    <- c(salinity,    mean(x$data$salinity[focus],na.rm=TRUE))
					flag        <- c(flag,        mean(x$data$flag[focus],na.rm=TRUE))
					sigma.theta <- c(sigma.theta, mean(x$data$sigma.theta[focus],na.rm=TRUE))
				} else if (method == "lm") {
					pressure    <- c(pressure,    pout[i])
					xx <- x$data$pressure[focus]
					yy <- x$data$scan[focus]
					scan        <- c(scan,        predict(lm(yy~xx),list(xx=pout[i])))
					yy <- x$data$depth[focus];
					depth       <- c(depth,       predict(lm(yy~xx),list(xx=pout[i])))
					yy <- x$data$temperature[focus]
					temperature <- c(temperature, predict(lm(yy~xx),list(xx=pout[i])))
					yy <- x$data$salinity[focus]
					salinity    <- c(salinity,    predict(lm(yy~xx),list(xx=pout[i])))
					yy <- x$data$flag[focus]
					flag        <- c(flag,        predict(lm(yy~xx),list(xx=pout[i])))
					yy <- x$data$sigma.theta[focus]
					sigma.theta <- c(sigma.theta, predict(lm(yy~xx),list(xx=pout[i])))
				} else {
					stop("unknown method ", method)
				}
			} else {
				scan        <- c(scan,        NA)
				pressure    <- c(pressure,    pout[i])
				depth       <- c(depth,       NA)
				temperature <- c(temperature, NA)
				salinity    <- c(salinity,    NA)
				flag        <- c(flag,        NA)
				sigma.theta <- c(sigma.theta, NA)
			}
		}
		result$data <- list(scan=scan, pressure=pressure, depth=depth, 
			temperature=temperature, salinity=salinity, flag=flag, 
			sigma.theta=sigma.theta)
	}
	result <- processing.log.append(result, log.item)
	return(result) # FIXME
}
