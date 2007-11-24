ctd.decimate <- function(x, p=NULL, method=c("boxcar","lm"), e=1)
# ADD: BIO method; spline; supsmu; ...
{
  	if (!inherits(x, "ctd"))
    	stop("method is only for ctd objects")
  	result <- x
  	n <- length(x$data$pressure)
  	if (n < 2) {
    	warning("too few data to trim.decimate()")
  	}
 	else {
	  	method <- match.arg(method)
		if (is.null(p)) {
			# autoscale
			dp.exact <- median(abs(diff(x$data$pressure)))
			dp <- pretty(3 * dp.exact)[2] # try for 3 data at least
			pout <- seq(0, dp * floor(max(x$data$pressure) / dp), dp)
			log.item <- paste("modified by ctd.decimate(x, p=NULL, method=\"", method[1], "\")",sep="")
		} else {
			if (length(p) == 1) {
				pout <- seq(0, p * floor(max(x$data$pressure) / p), p)
				log.item <- paste("modified by ctd.decimate(x, p=",p[1],", method=\"",method[1], "\")",sep="")
			} else {
				pout <- p
				log.item <- paste("modified by ctd.decimate(x, p=c(",p[1],",",p[2],",...), method=\"",method[1], "\")",sep="")
			}
		}
		n <- length(pout)
		# FIXME: should probably do this based on names(x$data)
		# FIXME: maybe should do this with apply()???
		scan <- NULL
		pressure <- NULL
		depth <- NULL
		temperature <- NULL
		salinity <- NULL
		flag <- NULL
		sigma <- NULL
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
					scan        <- c(scan,        mean(x$data$scan[focus]))
					depth       <- c(depth,       mean(x$data$depth[focus]))
					temperature <- c(temperature, mean(x$data$temperature[focus]))
					salinity    <- c(salinity,    mean(x$data$salinity[focus]))
					flag        <- c(flag,        mean(x$data$flag[focus]))
					sigma       <- c(sigma,       mean(x$data$sigma[focus]))
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
					yy <- x$data$sigma[focus]
					sigma       <- c(sigma,       predict(lm(yy~xx),list(xx=pout[i])))
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
				sigma       <- c(sigma,       NA)
			}
		}
		result$data <- list(scan=scan, pressure=pressure, depth=depth, temperature=temperature, salinity=salinity, flag=flag, sigma=sigma)
	}
	result <- processing.log.append(result, log.item)
	return(result) # FIXME
}

# plot(cc$data$salinity, cc$data$pressure, cex=0.3)
# S.smooth <- smooth.spline(cc$data$pressure, cc$data$salinity, df=163 / 6)
# lines(predict(S.smooth, seq(0,50,1))$y, seq(0,50,1))
# S.smooth <- smooth.spline(cc$data$pressure, cc$data$salinity, df=163 / 3)
# lines(predict(S.smooth, seq(0,50,1))$y, seq(0,50,1), col="red")
# SS<-smooth(cc$data$salinity)
