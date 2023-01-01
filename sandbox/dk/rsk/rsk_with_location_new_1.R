# This requires the latest "rsk" branch from github, and a renamed local file.

library(oce)
data(coastlineWorldFine, package="ocedata")
f <- "ctd_with_location.rsk"
# NB. the time offset is particular to this data file.
d <- read.rsk(f, allTables=TRUE, tzOffsetLocation=-8, debug=0)
geodata <- d[["geodata"]]
str(geodata)
print(table(geodata$origin))
lon <- geodata$longitude
lat <- geodata$latitude
origin <- geodata$origin
asp <- 1 / cos(mean(range(lat))*pi/180)
plot(lon[origin=="auto"], lat[origin=="auto"], asp=asp,
xlab="Longitude", ylab="Latitude")
points(lon[origin=="manual"], lat[origin=="manual"], col=2, pch=20)
polygon(coastlineWorldFine[["longitude"]], coastlineWorldFine[["latitude"]],
    col=rgb(210/255, 180/255, 140/255, alpha=0.1))
