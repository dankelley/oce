library("oce")
m05VectorBeam <- read.adv("/data/archive/sleiwex/2008/moorings/m05/adv/nortek_1943/raw/adv_nortek_1943.vec",
                          from=as.POSIXct("2008-06-25 00:00:00", tz="UTC"), to=as.POSIXct("2008-07-06 00:00:00", tz = "UTC"),
                          latitude=47.87943, longitude=-69.72533)
m05VectorBeam <- retime(m05VectorBeam, a=0.58, b=6.3892e-07, t0=as.POSIXct("2008-07-01 00:00:00", tz="UTC"))
m05VectorBeam <- subset(m05VectorBeam,
                        as.POSIXct("2008-06-25 13:00:00", tz="UTC") <= time & time <= as.POSIXct("2008-07-03 00:50:00", tz="UTC"))
m05VectorBeam <- oceEdit(m05VectorBeam, item="transformationMatrix",
                         value=rbind(c(11033, -5803, -5238),
                                     c(347, -9622, 9338),
                                     c(-1418, -1476, -1333))/4096, 
                         reason = "Nortek email 2011-02-14", person="DEK")
## 'use aquadoppHR heading; despike own pitch and roll'
m05VectorEnu <- toEnu(m05VectorBeam)
adv <- window(m05VectorEnu, as.POSIXct("2008-07-01 00:00:00", tz="UTC"), as.POSIXct("2008-07-01 00:01:00",tz="UTC"))

