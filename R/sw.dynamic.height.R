sw.dynamic.height <- function(x, pref=2000)
{
	height <- function(ctd, pref)
	{
		g <- if (is.na(ctd$metadata$latitude)) 9.8 else gravity(ctd$metadata$latitude)
		if (pref > max(ctd$data$pressure, na.rm=TRUE)) return(NA)
		np <- length(ctd$data$pressure)
                                        # 1e4 dbar per pascal
		dzdp <- ((1/sw.rho(ctd) - 1/sw.rho(rep(35,np),rep(0,np),ctd$data$pressure))/g)*1e4
		integrand <- approxfun(ctd$data$pressure, dzdp, rule=2)
		integrate(integrand, 0, pref)$value
	}
	if (inherits(x, "section")) {
		lon0 <- x$data$station[[1]]$metadata$longitude
		lat0 <- x$data$station[[1]]$metadata$latitude
		ns <- length(x$data$station)
		d <- vector("numeric", ns)
		h <- vector("numeric", ns)
		for (i in 1:ns) {
			d[i] <- geod.dist(x$data$station[[i]]$metadata$latitude, x$data$station[[i]]$metadata$longitude, lat0, lon0)
			h[i] <- height(x$data$station[[i]], pref)
		}
		return(list(distance=d, height=h))
	} else if (inherits(x, "ctd")) {
		return(height(x, pref))
	} else {
		stop("method only works for 'section' or 'ctd' objects")
	}
}
