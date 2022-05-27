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

## Save in version 2, because otherwise users with R 3.5.x and earlier will not
## be able to use data("logo")
if (utils::compareVersion(paste0(R.Version()$major, ".", R.Version()$minor), '3.6.0') >= 0) {
    message("saving with version=2 since R version is 3.6.0 or later")
    save(lobo, file="lobo.rda", version=2)
    tools::resaveRdaFiles("lobo.rda", version=2)
} else {
    save(lobo, file="lobo.rda")
    tools::resaveRdaFiles("lobo.rda")
}

