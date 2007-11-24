coriolis <- function(lat, degrees=TRUE)
{
  	if (degrees)
    	lat <- lat * 0.0174532925199433
  	1.4544410433286078e-4 * sin(lat)
}
