library(oce)
data(ctd)
summary(ctd)
plot(ctd)
cat("Some of the data are...\n") 
cat("  P/dbar      S/PSU         T/degC\n")
for (i in 1:min(5,length(ctd[["salinity"]]))) {
    cat(ctd[["pressure"]][i], "\t", ctd[["salinity"]][i], "\t", ctd[["temperature"]][i], "\n")
}
