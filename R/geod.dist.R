## Calculation of geodetic distance on surface of earth,
## based upon datum defined by
##       a = radius of major axis of earth
##       f = flattening factor.
## The answer is returned in the same units as a; here in meters.
##
## Patterned after R code donated by Darren Gillis
geod.dist <- function (lat1, lon1=NULL, lat2=NULL, lon2=NULL) {
	a <- 6378137.00          # WGS84 major axis
	f <- 1/298.257223563     # WGS84 flattening parameter
	if (inherits(lat1, "section")) {
		copy <- lat1
		n <- length(copy$data$station)
		lat1 <- vector("numeric", n)
		lon1 <- vector("numeric", n)
		for (i in 1:n) {
			lat1[i] <- copy$data$station[[i]]$metadata$latitude
			lon1[i] <- copy$data$station[[i]]$metadata$longitude
		}
		res <- vector("numeric", n)
  		for (i in 1:n) {
    		if (is.finite(lat1[1]) && is.finite(lon1[1]) && is.finite(lat1[i]) && is.finite(lon1[i])) {
      			dist <- .Fortran("geoddist",
                                 as.double(lat1[1]),
                                 as.double(lon1[1]),
                                 as.double(lat1[i]),
                                 as.double(lon1[i]),
                                 as.double(a),
                                 as.double(f),
                                 as.double(1),
                                 as.double(1),
                                 dist = double(1),
                                 PACKAGE = "oce")$dist
    		} else {
      			dist <- NA
			}
    		res[i] <- dist
		}
	} else {
  		n1 <- length(lat1)
		if (length(lon1) != n1)	stop("lat1 and lon1 must be vectors of the same length")
		n2 <- length(lat2)
		if (length(lon2) != n2)	stop("lat2 and lon2 must be vectors of the same length")
		if (n2 < n1) { # take only first one
			if (n2 != 1) warning("Using just the first element of lat2 and lon2, even though it contains more elements")
			llat2 <- rep(lat2[1], n1)
			llon2 <- rep(lon2[1], n1)
		} else {
			llat2 <- lat2
			llon2 <- lon2
		}
                                        #subroutine geoddist(DLAT1,DLON1,DLAT2,DLON2,A,F,FAZ,BAZ,S)
  		res <- vector("numeric", n1)
  		for (i in 1:n1) {
                                        #cat("values=",lat1[i],lon1[i],llat2[i],llon2[i],"\n")
    		if (is.finite(lat1[i]) && is.finite(lon1[i]) && is.finite(llat2[i]) && is.finite(llon2[i])) {
      			res[i] <- .Fortran("geoddist",
                                   as.double(lat1[i]),
                                   as.double(lon1[i]),
                                   as.double(llat2[i]),
                                   as.double(llon2[i]),
                                   as.double(a),
                                   as.double(f),
                                   as.double(1),
                                   as.double(1),
                                   dist = double(1),
                                   PACKAGE = "oce")$dist
    		} else {
      			res[i] <- NA
			}
  		}
	}
  	res / 1000
}
