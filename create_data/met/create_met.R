library(oce)
if (!length(dir(".", "met.dat"))) {
    cat("Downloading met.dat and creating met.rda\n")
    download.file("http://climate.weather.gc.ca/climateData/bulkdata_e.html?format=csv&stationID=6358&Year=2003&Month=9&Day=17&timeframe=1&submit=Download+Data", "met.dat")
}
met <- read.met("met.dat")
save(met, file="met.rda")
tools::resaveRdaFiles("met.rda")

