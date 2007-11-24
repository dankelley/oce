library(oce);
data(ctd)
summary(ctd)
plot(ctd)
cat("Some of the data are...\n") 
cat("  P/dbar      S/PSU         T/degC\n")
for (i in 1:min(5,length(ctd$data$salinity))) {
  cat(ctd$data$pressure[i], "\t", ctd$data$salinity[i], "\t", ctd$data$temperature[i], "\n")
}
