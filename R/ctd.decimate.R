ctd.decimate <- function(x, p, method=c("approx", "boxcar","lm"), e=1)
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
		log.item <- paste("modified by ctd.decimate(x, method=\"", method[1], "\")",sep="")
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
	data.new <- as.data.frame(array(NA, dim=c(npt, dim(x$data)[2])))
	names(data.new) <- data.names
  	method <- match.arg(method)
	if (method == "approx") {
		too.deep <- pt > max(x$data[["pressure"]], na.rm=TRUE)
		for (datum.name in data.names) {
			if (datum.name != "pressure") {
				data.new[[datum.name]] <- approx(x$data[["pressure"]], x$data[[datum.name]], pt, rule=2)$y
				data.new[[datum.name]][too.deep] <- NA
			}
		}
	} else {
		for (i in 1:npt) {
			if (i==1) {
				focus <- (x$data$pressure >= (pt[i] - e * (pt[i + 1] - pt[  i  ]))) &
					(x$data$pressure <= (pt[i] + e * (pt[i + 1] - pt[  i  ])))
			} else if (i == npt) {
				focus <- (x$data$pressure >= (pt[i] - e * (pt[  i  ] - pt[i - 1]))) &
					(x$data$pressure <= (pt[i] + e * (pt[  i  ] - pt[i - 1])))
			} else {
				focus <- (x$data$pressure >= (pt[i] - e * (pt[  i  ] - pt[i - 1]))) &
					(x$data$pressure <= (pt[i] + e * (pt[i + 1] - pt[  i  ])))
			}
			if (sum(focus, na.rm=TRUE) > 0) {
				if (method == "boxcar") {
					for (datum.name in data.names) {
						if (datum.name != "pressure") {
							data.new[[datum.name]][i] <- mean(x$data[[datum.name]][focus],na.rm=TRUE)
						}
					}
				} else if (method == "lm") { # FIXME: this is far too slow
					xvar <- x$data[["pressure"]][focus]
					for (datum.name in data.names) {
						if (datum.name != "pressure") {
							yvar <- x$data[[datum.name]][focus]
							m <- lm(yvar ~ xvar)
							data.new[[datum.name]][i] <- predict(m, newdata=list(xvar=pt[i]))
						}
					}
				} else {
					stop("impossible to get here -- developer error")
				}
			} else {
				# No data in the focus region
				for (datum.name in data.names) {
					if (datum.name != "pressure") {
						data.new[[datum.name]][i] <- NA
					}
				}
			}
		}
	}
	# Now replace pressure
	data.new[["pressure"]] <- pt
	res$data <- data.new
	res <- log.append(res, log.item)
	return(res)
}
