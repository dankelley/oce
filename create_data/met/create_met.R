library(oce)
met <- read.met('http://climate.weather.gc.ca/climateData/bulkdata_e.html?format=csv&stationID=6358&Year=2003&Month=9&Day=17&timeframe=1&submit=Download+Data')
save(met, file="met.rda")
tools::resaveRdaFiles("met.rda")

