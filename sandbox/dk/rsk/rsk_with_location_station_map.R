library(oce)
source("~/git/oce/R/rsk.R")
source("~/git/oce/R/ctd.R")
rda <- "rsk_with_location_station_map.rda"
if (!file.exists(rda)) {
    f <- "~/ctd_with_location.rsk"
    d <- read.rsk(f, allTables=TRUE, tzOffsetLocation=-8)
    ctds <- ctdFindProfilesRBR(as.ctd(d))
    s <- as.section(ctds)
    save(s, file=rda)
    message("saving 's' to cache (so I can fix 'develop' branch)")
} else {
    message("using cached value (from the 'rsk' branch)")
}

# draw map with white ink, to avoid drawing inter-station lines
png("rsk_with_location_station_map.png")
?'plot,section-method'
plot(s, which="map", showStations=TRUE, showStart=FALSE)
lon <- s[["longitude", "byStation"]]
lat <- s[["latitude", "byStation"]]
points(lon, lat, pch=20)
dev.off()

