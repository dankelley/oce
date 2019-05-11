library(oce)

## One day of in-water data.
cm <- read.oce("/data/archive/sleiwex/2008/moorings/m11/cm/interocean_0811786/manufacturer/cm_interocean_0811786.s4a.tab",
               from=840, to=840+24*60)
cm <- subset(cm, time < as.POSIXct("2008-06-27 00:00:00", tz="UTC"))
summary(cm)
save(cm, file="cm.rda")
if (utils::compareVersion(R.Version()$minor, '3.6') >= 0) {
    tools::resaveRdaFiles('cm.rda', version=2)
} else {
    tools::resaveRdaFiles('cm.rda')
}

