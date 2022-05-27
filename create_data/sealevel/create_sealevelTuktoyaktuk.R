library(oce)
tuk <- read.table("tuk_time_elev.dat", header=FALSE, as.is=TRUE)
## Until 2018-01-06, added 7 hours to get GMT. However, this proved
## to be confusing. For one thing, comments in the T_TIDE code suggested
## a 6 hour offset.  For another, other comments there suggested taking the
## values AS THOUGH they were GMT, to get a more direct comparison with
## Foreman who (it seems to be suggested) erroneously took the times to be UTc.
## time <- as.POSIXlt(strptime(tuk$V1, "%d-%b-%Y %H:%M:%S", tz="GMT")+7*3600, tz="GMT")
time <- as.POSIXlt(strptime(tuk$V1, "%d-%b-%Y %H:%M:%S", tz="GMT"), tz="GMT")
elevation <- tuk$V2
sealevelTuktoyaktuk <- as.sealevel(elevation=elevation, time=time,
                                   stationName="Tuktoyaktuk", region="NWT", stationNumber=6485,
                                   longitude=133.0292, latitude=69.43889,
                                   year=1975, GMTOffset=0)

## Save in version 2, because otherwise users with R 3.5.x and earlier will not
## be able to use data("sealevelTuktoyaktuk")
if (utils::compareVersion(paste0(R.Version()$major, ".", R.Version()$minor), '3.6.0') >= 0) {
    message("saving with version=2 since R version is 3.6.0 or later")
    save(sealevelTuktoyaktuk, file="sealevelTuktoyaktuk.rda", version=2)
    tools::resaveRdaFiles("sealevelTuktoyaktuk.rda", version=2)
} else {
    save(sealevelTuktoyaktuk, file="sealevelTuktoyaktuk.rda")
    tools::resaveRdaFiles("sealevelTuktoyaktuk.rda")
}

