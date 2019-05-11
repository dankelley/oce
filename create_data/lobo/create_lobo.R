library(oce)
if (!length(dir(".", "lobo.dat"))) {
    cat("Downloading lobo.dat and creating lobo.rda\n")
    download.file("http://lobo.satlantic.com/cgi-data/nph-data.cgi?min_date=20070101&max_date=20121024&y=salinity,temperature,weather_temp", "lobo.dat")
}
lobo <- read.lobo("lobo.dat")
start <- as.POSIXct("2009-03-01 00:00:00", tz="UTC")
end <- as.POSIXct("2009-04-01 23:00:00", tz="UTC")
lobo <- subset(lobo, start <= time & time <= end)
## lots of NA in there because the sampling rate is 1/hour in this time interval
lobo <- subset(lobo, !is.na(temperature))
save(lobo, file="lobo.rda")
if (utils::compareVersion(R.Version()$minor, "3.6") >= 0) {
    tools::resaveRdaFiles("lobo.rda", version=2)
} else {
    tools::resaveRdaFiles("lobo.rda")
}

