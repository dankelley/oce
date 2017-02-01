library(oce)
if (!length(dir(".", pattern="^met.dat$"))) {
    cat("Downloading met.dat and creating met.rda\n")
    ## download.file("http://climate.weather.gc.ca/climateData/bulkdata_e.html?format=csv&stationID=6358&Year=2003&Month=9&Day=17&timeframe=1&submit=Download+Data", "met.dat")
    download.file("http://climate.weather.gc.ca/climate_data/bulk_data_e.html?format=csv&stationID=6358&Year=2003&Month=9&timeframe=1&submit=Download+Data", "met.dat")
}
met <- read.met("met.dat")
met <- oceSetData(met, "time", met[["time"]] + 4 * 3600, note="add 4h to local time to get UTC time")
save(met, file="met.rda")
tools::resaveRdaFiles("met.rda")

