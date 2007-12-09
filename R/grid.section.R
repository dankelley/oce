grid.section <- function(section, pressures=NULL, quiet=TRUE)
{
	n <- length(section$stations)
	dp.list <- NULL
	if (is.null(pressures)) {
		p.max <- 0
		for (i in 1:n) {
			p <- section$stations[[i]]$data$pressure
			dp.list <- c(dp.list, mean(diff(p)))
			p.max <- max(c(p.max, p))
		}
		dp <- mean(dp.list) / 1.5 # make it a little smaller
		if (!quiet) cat("Mean pressure difference =", dp,"and max p =", p.max, "\n")
		if (dp < 0.01) {
			dp <- 0.01 # prevent scale less 1 cm.
		} else if (dp < 5) { # to nearest 1 db
			dp <- 1 * floor(0.5 + dp / 1)
			p.max <- 1 * floor(1 + p.max / 1)
		} else if (dp < 20) { # to nearest 5 db
			dp <- 5 * floor(0.5 + dp / 5)
			p.max <- 5 * floor(1 + p.max / 5)
		} else if (dp < 100){ # to nearest 10 dbar
			dp <- 10 * floor(0.5 + dp / 10)
			p.max <- 10 * floor(1 + p.max / 10)
		} else if (dp < 200){ # to nearest 10 dbar
			dp <- 50 * floor(0.5 + dp / 50)
			p.max <- 50 * floor(1 + p.max / 50)
		} else { # to nearest 100 dbar
			dp <- 100 * floor(0.5 + dp / 100)
			p.max <- 100 * floor(1 + p.max / 100)
		}
		if (!quiet) cat("Round to pressure difference =", dp,"and max p =", p.max, "\n")
		p <- seq(0, p.max, dp)
		if (!quiet) cat("Using auto-selected pressures: ", p, "\n");
	} else {
		if (length(pressures) == 1) {
			if (pressures=="levitus") {
				p <- c(0,   10,   20,   30,   50,   75,  100,  125,  150,  200,
					250,  300,  400,  500,  600,  700,  800,  900, 1000, 1100,
					1200, 1300, 1400, 1500, 1750, 2000, 2500, 3000, 3500, 4000,
					4500, 5000, 5500)
				if (!quiet) cat("Using stanard atlas pressures: ", p, "\n")
			} else { # FIXME should insist numeric
				# find max in dataset
				p.max <- 0
				for (i in 1:n) {
					p <- section$stations[[i]]$data$pressure
					p.max <- max(c(p.max, p))
				}
				p <- seq(0, p.max, pressures)
				if (!quiet) cat("Pressures: ", p, "\n")
			}
		} else {
			p <- pressures
		}
	}
	# BUG should handle all variables (but how to interpolate on a flag?)
	res <- section
	lat0 <- section$stations[[1]]$latitude
	lon0 <- section$stations[[1]]$longitude
	dist <- vector("numeric", n)
	for (i in 1:n) {
		#if (!quiet) cat("Doing station number", i, "\n")
		d <- section$stations[[i]]$data
		dpressure <- d$pressure # may speed things up
		salinity <- approx(dpressure, d$salinity, p, ties=mean)$y
		temperature <- approx(dpressure, d$temperature, p, ties=mean)$y
		sigma.theta <- approx(dpressure, d$sigma.theta, p, ties=mean)$y
		res$stations[[i]]$data <- data.frame(pressure=p, salinity=salinity, temperature=temperature, sigma.theta=sigma.theta)
		dist[i] <- geod.dist(section$stations[[i]]$latitude, section$stations[[i]]$longitude, lat0, lon0)
	}

#	Tm <- matrix(NA, nrow=length(p), ncol=n)
#	for (i in 1:n) {
#		Tm[, i] <- res$stations[[i]]$data[["temperature"]]
#	}
#	dan0 <<- dist
#	dan1 <<- Tm

	if (is.null(pressures))
		log.item <- "modified by grid.section(x)"
	else
		log.item <- paste("modified by grid.section(x, pressures=",pressures,")",sep="")
	res <- processing.log.append(res, log.item)
	res
}
