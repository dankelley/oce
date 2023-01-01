# This requires the latest "rsk" branch from github, and a renamed local file.

library(oce)
data(coastlineWorldFine, package="ocedata")
f <- "ctd_with_location.rsk"

# Read data.  The time offset is particular to this data file.
d <- read.rsk(f, allTables=TRUE, tzOffsetLocation=-8, debug=0)
geodata <- d[["geodata"]]
str(geodata)
table(geodata$origin)
lon <- geodata$longitude
lat <- geodata$latitude
origin <- geodata$origin
asp <- 1 / cos(mean(range(lat))*pi/180)
plot(lon[origin=="auto"], lat[origin=="auto"], asp=asp,
xlab="Longitude", ylab="Latitude")
points(lon[origin=="manual"], lat[origin=="manual"], col=2, pch=20)
polygon(coastlineWorldFine[["longitude"]], coastlineWorldFine[["latitude"]], col=rgb(0, 1, 0, alpha=0.1))




# Chop into profiles, using information in RBR headers
ctds0 <- ctdFindProfilesRBR(as.ctd(d))

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

