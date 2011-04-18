library(oce)
stopifnot(number.as.POSIXct(719529, "matlab")==ISOdatetime(1970,1,1,0,0,0,tz="UTC"))

buf <- as.raw(c(0xa5, 0x11, 0xaa, 0xa5, 0x11, 0x00))
stopifnot(all(c(1,4) == matchBytes(buf, 0xa5, 0x11)))

# time-series filtering
b <- rep(1,5)/5
a <- 1
x <- seq(1, 4, by=0.2)
matlab.res <- c(0.2000,0.4400,0.7200,1.0400,1.4000,1.6000,1.8000,2.0000,2.2000,2.4000,2.6000,2.8000,3.0000,3.2000,3.4000,3.6000)
stopifnot(all.equal.numeric(matlab.res, oceFilter(x, a, b)))

# Magnetic declination
stopifnot(all.equal.numeric(-16.80410, magneticDeclination(44+55/60,-(69+46/60),2008),1e-3))

# GPS time
stopifnot(number.as.POSIXct(cbind(604,134351), type="gps") == as.POSIXct("2011-03-21 13:18:56",tz="UTC"))

