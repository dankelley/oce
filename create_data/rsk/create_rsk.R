library(oce)
raw <- read.oce('060130_20150904_1159.rsk')
raw <- oceSetMetadata(raw, 'longitude', -(56 + 26.232/60))
raw <- oceSetMetadata(raw, 'latitude', 73 + 13.727/60)
raw <- oceSetMetadata(raw, 'station', 'C18')
raw <- oceSetMetadata(raw, 'ship', 'Ault')
raw <- oceSetMetadata(raw, 'institute', 'Ocean Research Project')
focus <- structure(c(1441381041, 1441381483), class = c("POSIXct", 
"POSIXt"), tzone = "UTC")
rsk <- subset(raw, focus[1] <= time & time <= focus[2])
save(rsk, file="rsk.rda")
if (utils::compareVersion(R.Version()$minor, "3.6") >= 0) {
    tools::resaveRdaFiles("rsk.rda", version=2)
} else {
    tools::resaveRdaFiles("rsk.rda")
}

