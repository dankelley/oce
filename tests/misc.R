library(oce)
stopifnot(matlab2POSIXt(719529)==ISOdatetime(1970,1,1,0,0,0,tz="UTC"))

buf <- as.raw(c(0xa5, 0x11, 0xaa, 0xa5, 0x11, 0x00))
stopifnot(all(c(1,4) == match.bytes(buf, 0xa5, 0x11)))

# filtering
b <- rep(1,5)/5
a <- 1
x <- seq(1, 4, by=0.2)
matlab.res <- c(0.2000,0.4400,0.7200,1.0400,1.4000,1.6000,1.8000,2.0000,2.2000,2.4000,2.6000,2.8000,3.0000,3.2000,3.4000,3.6000)
stopifnot(all.equal.numeric(matlab.res, oce.filter(b, a, x)))

# Magnetic declination
stopifnot(all.equal.numeric(-16.80410, magnetic.declination(44+55/60,-(69+46/60),2008),1e-3))
