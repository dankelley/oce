library(oce)

#data("adp")
#data("adv")
#data("cm")
#data("coastlineWorld")

data("ctd")
stopifnot(all.equal.numeric(ctd[["latitude"]],   44.6842666666667))
stopifnot(all.equal.numeric(ctd[["longitude"]], -63.6438833333333))
stopifnot(all.equal(ctd[["startTime"]], as.POSIXct("2003-10-15 11:38:00", tz="UTC")))

#data("ctdRaw")
#data("drifter")
#data("echosounder")
#data("lisst")
#data("lobo")
#data("logger")
#data("met")

data("sealevel")
stopifnot(all.equal.numeric(sealevel[["latitude"]],   44.666667))
stopifnot(all.equal.numeric(sealevel[["longitude"]], -63.583333))
stopifnot(all.equal.numeric(sealevel[["stationNumber"]], 490))
stopifnot(all.equal.numeric(sealevel[["GMTOffset"]], 0))

#data("sealevelTuktoyaktuk")

data("section")
stopifnot(all.equal(section[["sectionId"]], "a03"))
stopifnot(all.equal.numeric(length(section@data$station), 124))
stopifnot(all.equal(section@data$station[[1]]@metadata$station, "3"))
stopifnot(all.equal(section@data$station[[1]][["station"]], "3"))
stopifnot(all.equal(section@data$station[[1]][["latitude"]], 36.8758))
stopifnot(all.equal(section@data$station[[1]][["longitude"]], -8.5263))

#data("tidedata")
#data("topoWorld")
#data("wind")

