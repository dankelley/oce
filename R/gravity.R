gravity <- function(lat, degrees=TRUE)
{
  	if (degrees)
    	lat <- lat * 0.0174532925199433
  	9.780318*(1.0+5.3024e-3*sin(lat)^2-5.9e-6*sin(2*lat)^2)
}
