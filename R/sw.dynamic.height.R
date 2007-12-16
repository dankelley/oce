sw.dynamic.height <- function(x, pref=2000)
{
	height <- function(ctd, pref)
	{
		g <- if (is.na(ctd$latitude)) 9.8 else gravity(ctd$latitude)
		if (pref > max(ctd$data$pressure, na.rm=TRUE)) return(NA)
		np <- length(ctd$data$pressure)
		# 1e4 dbar per pascal
		dzdp <- ((1/sw.rho(ctd) - 1/sw.rho(rep(35,np),rep(0,np),ctd$data$pressure))/g)*1e4
		integrand <- approxfun(ctd$data$pressure, dzdp, rule=2)
		integrate(integrand, 0, pref)$value
	}
	if ("section" == class(x)) {
		lon0 <- x$station[[1]]$longitude
		lat0 <- x$station[[1]]$latitude
		ns <- length(x$station)
		d <- vector("numeric", ns)
		h <- vector("numeric", ns)
		for (i in 1:ns) {
			d[i] <- geod.dist(x$station[[i]]$latitude, x$station[[i]]$longitude, lat0, lon0)
			h[i] <- height(x$station[[i]], pref)
		}
		return(list(distance=d, height=h))
	} else if ("ctd" == class(x)) {
		return(height(x, pref))
	} else {
		stop("method only works for 'section' or 'ctd' objects")
	}
}
