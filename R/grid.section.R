grid.section <- function(section, pressures=NA, quiet=TRUE)
{
	n <- length(section$stations)
	dp.list <- NULL
	if (length(pressures)==1 && is.na(pressures)) {
		p.max <- 0
		for (i in 1:n) {
			p <- section$stations[[i]]$data$pressure
			dp.list <- c(dp.list, mean(diff(p)))
			p.max <- max(c(p.max, p))
		}
		dp <- mean(dp.list)
		if (!quiet) cat("Mean pressure difference =", dp,"and max p =", p.max, "\n")
		if (dp < 0.01) {
			dp <- 0.01 # prevent scale less 1 cm.
		} else if (dp < 2) {
			dp <- round(dp, 1)
			if (dp == 0) dp <- 0.1
			p.max <- round(p.max, 1)
		} else if (dp < 20) { # to nearest dbar
			dp <- round(dp, 0)
			if (dp == 0) dp <- 1
			p.max <- round(p.max, 0)
		} else if (dp < 200){ # to nearest 10 dbar
			dp <- round(dp, -1)
			if (dp == 0) dp <- 10
			p.max <- round(p.max, -1)
		} else { # to nearest 100 dbar
			dp <- round(dp, -2)
			if (dp == 0) dp <- 100
			p.max <- round(p.max, -2)
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
		if (!quiet) cat("Doing station number", i, "\n")
		d <- section$stations[[i]]$data
		salinity <- approx(d$pressure, d$salinity, p)$y
		temperature <- approx(d$pressure, d$temperature, p)$y
		sigma.theta <- approx(d$pressure, d$sigma.theta, p)$y
#		flag <- approx(data$pressure, data$flag, p)$y # BUG makes no sense
		res$stations[[i]]$data <- data.frame(pressure=p, salinity=salinity, temperature=temperature, sigma.theta=sigma.theta)
		dist[i] <- geod.dist(section$stations[[i]]$latitude, section$stations[[i]]$longitude, lat0, lon0)
	}

#	Tm <- matrix(NA, nrow=length(p), ncol=n)
#	for (i in 1:n) {
#		Tm[, i] <- res$stations[[i]]$data[["temperature"]]
#	}
#	dan0 <<- dist
#	dan1 <<- Tm

	res
}
