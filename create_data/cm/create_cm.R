library(oce)

## One day of in-water data.
cm <- read.oce("/data/archive/sleiwex/2008/moorings/m11/cm/interocean_0811786/manufacturer/cm_interocean_0811786.s4a.tab",
               from=840, to=840+24*60)
cm <- subset(cm, time < as.POSIXct("2008-06-27 00:00:00", tz="UTC"))
summary(cm)

## Save in version 2, because otherwise users with R 3.5.x and earlier will not
## be able to use data("cm")
if (utils::compareVersion(paste0(R.Version()$major, ".", R.Version()$minor), '3.6.0') >= 0) {
    message("saving with version=2 since R version is 3.6.0 or later")
    save(cm, file="cm.rda", version=2)
    tools::resaveRdaFiles('cm.rda', version=2)
} else {
    save(cm, file="cm.rda")
    tools::resaveRdaFiles('cm.rda')
}

