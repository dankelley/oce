sw.dynamic.height <- function(ctd, pref=2000)
{
	if (!inherits(ctd, "ctd")) stop("method is only for ctd objects")
	g <- 9.8 # FIXEME: should have slight correction to g for latitude
	if (pref > max(ctd$data$pressure, na.rm=TRUE)) return(NA)
	n <- length(ctd$data$pressure)
	dzdp <- ((1/sw.rho(ctd) - 1/sw.rho(rep(35, n), rep(0,n), ctd$data$pressure))/g) * 1e4
	integrand <- approxfun(ctd$data$pressure, dzdp, rule=2)
	integrate(integrand, 0, pref)$value
}
