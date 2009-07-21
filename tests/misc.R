library(oce)
stopifnot(matlab2POSIXt(719529)==ISOdatetime(1970,1,1,0,0,0,tz="UTC"))

buf <- as.raw(c(0xa5, 0x11, 0xaa, 0xa5, 0x11, 0x00))
stopifnot(all(c(1,4) == match.bytes(buf, 0xa5, 0x11)))
