library(oce)
tuk <- read.table("tuk_time_elev.dat", header=FALSE, as.is=TRUE)
time <- as.POSIXlt(strptime(tuk$V1, "%d-%b-%Y %H:%M:%S", tz="GMT")+7*3600, tz="GMT")
elevation <- tuk$V2
sealevelTuktoyaktuk <- as.sealevel(elevation=elevation, time=time,
                                   stationName="Tuktoyaktuk", region="NWT", stationNumber=6485,
                                   longitude=133.0292, latitude=69.43889,
                                   year=1975, GMTOffset=0)
save(sealevelTuktoyaktuk, file='sealevelTuktoyaktuk.rda')
library(tools)
tools::resaveRdaFiles("sealevelTuktoyaktuk.rda", compress="auto")
