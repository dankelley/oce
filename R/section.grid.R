section.grid <- function(section, p, method=c("approx","boxcar","lm"), ...)
{
	method <- match.arg(method)
	n <- length(section$data$station)
	dp.list <- NULL
	if (missing(p)) {
		p.max <- 0
		for (i in 1:n) {
			p <- section$data$station[[i]]$data$pressure
			dp.list <- c(dp.list, mean(diff(p)))
			p.max <- max(c(p.max, p))
		}
		dp <- mean(dp.list) / 1.5 # make it a little smaller
                                        # cat("Mean pressure difference =", dp,"and max p =", p.max, "\n")
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
                                        # cat("Round to pressure difference =", dp,"and max p =", p.max, "\n")
		pt <- seq(0, p.max, dp)
                                        # cat("Using auto-selected pressures: ", p, "\n");
	} else {
		if (length(p) == 1) {
			if (p=="levitus") {
				pt <- c(0,   10,   20,   30,   50,   75,  100,  125,  150,  200,
                        250,  300,  400,  500,  600,  700,  800,  900, 1000, 1100,
                        1200, 1300, 1400, 1500, 1750, 2000, 2500, 3000, 3500, 4000,
                        4500, 5000, 5500)
                                        # cat("Using stanard atlas pressures: ", p, "\n")
			} else { # FIXME should insist numeric
                                        # find max in dataset
				p.max <- 0
				for (i in 1:n) {
					p <- section$data$station[[i]]$data$pressure
					p.max <- max(c(p.max, p))
				}
				p <- seq(0, p.max, p)
                                        # cat("Pressures: ", p, "\n")
			}
		} else {
			pt <- p
		}
	}
                                        # BUG should handle all variables (but how to interpolate on a flag?)
	res <- section
	for (i in 1:n) {
                                        #cat("Doing station number", i, "\n")
		res$data$station[[i]] <- ctd.decimate(section$data$station[[i]], p=pt, method=method, ...)
	}
	if (is.null(p))
		log.item <- paste("modified by section.grid(x,method=\"", method, "\")")
	else
		log.item <- paste("modified by section.grid(x, p=c(",paste(p,collapse=","),"),method=\"",method,"\")",sep="")
	res <- processing.log.append(res, log.item)
	res
}
