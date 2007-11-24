sw.N2 <- function(p, sigma.theta, ...) # BUG: think more about best density measure
{
	args <- list(...)
	df <- if (is.null(args$df)) length(p)/4 else args$df;
	ok <- !is.na(p) & !is.na(sigma.theta)
	#cat(paste("df=",df,"\n"))
  	sigma.smooth <- smooth.spline(p[ok], sigma.theta[ok], df=df)
  	sigma.deriv <- predict(sigma.smooth, p, deriv = 1)
  	ifelse(ok, 9.8 * 9.8 * 1e-4 * sigma.deriv$y, NA)
}
