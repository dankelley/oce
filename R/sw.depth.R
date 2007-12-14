sw.depth <- function(p, lat, degrees=TRUE)
{
	if ("ctd" == class(p)) {
		lat <- abs(p$data$latitude)
		p <- p$data$pressure # over-writes p
	}
  	if (degrees) lat <- lat * 0.0174532925199433
  	x <- sin(lat)^2
  	gr <- 9.780318*(1.0+(5.2788e-3+2.36e-5*x)*x) + 1.092e-6*p
  	(((-1.82e-15*p+2.279e-10)*p-2.2512e-5)*p+9.72659)*p / gr
}
