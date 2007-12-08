grid.section <- function(section, pressures=NA, quiet=TRUE)
{
	res <- section
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
		if (debug) cat("Mean pressure difference =", dp,"and max p =", p.max, "\n")
		if (dp < 20) { # to nearest dbar
			dp <- round(dp, 0)
			p.max <- round(p.max, 0)
		} else if (dp < 200){ # to nearest 10 dbar
			dp <- round(dp, -1)
			p.max <- round(p.max, -1)
		} else { # to nearest 100 dbar
			dp <- round(dp, -2)
			p.max <- round(p.max, -2)
		}
		if (debug) cat("Mean pressure difference =", dp,"and max p =", p.max, "\n")
		p <- seq(0, p.max, dp)
		if (debug) cat("Using auto-selected pressures: ", p, "\n");
		#warning("Unspecified pressure case is not implemented yet")
	} else {
		if (length(pressures) == 1) {
			if (pressures=="levitus") {
				p <- c(0,   10,   20,   30,   50,   75,  100,  125,  150,  200,
					250,  300,  400,  500,  600,  700,  800,  900, 1000, 1100,
					1200, 1300, 1400, 1500, 1750, 2000, 2500, 3000, 3500, 4000,
					4500, 5000, 5500)
				#if (debug) cat("Using Levitus pressures: ", p, "\n");
			} else { # FIXME should insist numeric
				# find max in dataset
				p.max <- 0
				for (i in 1:n) {
					p <- section$stations[[i]]$data$pressure
					p.max <- max(c(p.max, p))
				}
				p <- seq(0, p.max, pressures)
				#warning("Single pressure case is not implemented yet")
			}
		} else {
			p <- pressures
			#warning("Multiple pressure case is not implemented yet")
		}
	}
	if (debug) cat("Pressures: ", p, "\n")
	stop("Should now interpolate everything to these pressures... not coded yet")
	res <- section # FIXME do something here!
	res
}
