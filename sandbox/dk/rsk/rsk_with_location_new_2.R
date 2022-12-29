# rsk_with_location_20211229b.R new method to infer lon/lat using
# geodata$origin=="auto" data only.
# https://github.com/dankelley/oce/issues/2026#issuecomment-1367308457

# This requires the latest "rsk" branch from github, and a renamed local file.

library(oce)
data(coastlineWorldFine, package="ocedata")
f <- "ctd_with_location.rsk"

# Read data.  The time offset is particular to this data file.
source("~/git/oce/R/rsk.R")
d <- read.rsk(f, allTables=TRUE, tzOffsetLocation=-8, debug=0)
png("A.png")
par(mfrow=c(2, 1), mar=c(3, 3, 2, 1), mgp=c(2, 0.7, 0))
hist(d[["longitude"]]-d[["longitudeNew"]], breaks=100)
t.test(d[["longitude"]]-d[["longitudeNew"]])
hist(d[["latitude"]]-d[["latitudeNew"]], breaks=100)
t.test(d[["latitude"]]-d[["latitudeNew"]])
dev.off()


sort(names(d@metadata))

# regionCast
regionCast <- d[["regionCast"]]
d[["regionCast"]]$regionID
str(d[["regionProfile"]])
str(d[["regionGeoData"]])

# breaksdata
breaksdata <- d[["breaksdata"]]
str(breaksdata)

# geodata
geodata <- d[["geodata"]]
sort(names(d@metadata))
str(geodata)
table(geodata$origin)
gtstamp <- geodata$tstamp
glon <- geodata$longitude
glat <- geodata$latitude
gorigin <- geodata$origin
asp <- 1 / cos(mean(range(glat))*pi/180)
auto <- geodata$origin == "auto"
manual <- geodata$origin == "manual"
plot(glon[auto], glat[auto], asp=asp, xlab="Longitude", ylab="Latitude")
points(glon[manual], glat[manual], col=2, pch=20)
polygon(coastlineWorldFine[["longitude"]], coastlineWorldFine[["latitude"]], col=rgb(0, 1, 0, alpha=0.1))

gtstamp0 <- gtstamp[auto]
glat0 <- lat[auto]
glon0 <- lon[auto]

# Comparison with "LOCATION 1" in Ruskin software indicates
# that the *second* of these auto points is LOCATION 1.
# Oddly, this indicates a 7-hour shift. However, other things (including
# lon and lat mismatch) suggest 8-hour shift.
head(gtstamp0) # Ruskin says LOCATION 1 at 2022-08-08 15:10:06
head(numberAsPOSIXct(gtstamp0/1000)) # Ruskin says LOCATION 1 at 2022-08-08 15:10:06
head(glon0) # Ruskin says LOCATION 1 at -149.7240
head(glat0) # Ruskin says LOCATION 1 at 59.9377
# Ruskin says
# location                time       lon     lat
#        1 2022-08-08 15:10:06 -149.7240 59.9377
#        2 2022-08-08 15:14:06 -149.7237 59.9376

# Chop into profiles, using information in RBR headers
ctds0 <- ctdFindProfilesRBR(as.ctd(d))
str(ctds0, 1)
A <- ctds0[[1]]
A[["longitude"]]

geodata <- d@metadata$geodata
str(geodata, 1)
look <- geodata$origin == "auto"
tstampAuto <- geodata$tstamp[look]
lonAuto <- geodata$longitude[look]
latAuto <- geodata$latitude[look]
str(lonAuto)

# Ignore top 2m and bottom 3m, which may be incorrect.
ctds1 <- lapply(ctds0,
    function(ctd) {
        p <- ctd[["pressure"]]
        subset(ctd, 2 < p & p < max(p) - 3)
    })

# Station map
png("rsk_with_location_map.png", width=7, height=7, unit="in", res=200)
plot(as.section(ctds1), which="map")
dev.off()

# Individual CTD plots, raw
png("rsk_with_location_raw_%2d.png", width=7, height=7, unit="in", res=200)
for (ctd in ctds0) {
    plot(ctd)
}
dev.off()

# Individual CTD plots, trimmed
png("rsk_with_location_trimmed_%2d.png", width=7, height=7, unit="in", res=200)
for (ctd in ctds1) {
    plot(ctd)
}
dev.off()

