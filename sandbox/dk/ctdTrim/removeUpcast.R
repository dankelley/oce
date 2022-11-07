library(oce)
f <- read.oce("SBE19plus_01906009_2019_04_11.cnv")
par(mfrow=c(1,2))
plotScan(f)
max <- which(round(f[['pressure']],0) == max(round(f[['pressure']],0)))[1] # Find first rounded max
abline(v=max, col="blue")

trimByIndex<-function(data, parameters, debug) {
  parameters[1] < data$scan & data$scan < parameters[2]
}
removeUpcast <- function(x) {
  # x is an oce ctd object
  max <- which(round(x[['pressure']],0) == max(round(x[['pressure']],0)))[1] # Find first rounded max
  # Removing upcast
  clean <- ctdTrim(x, trimByIndex, parameters=c(0, max))
  return(clean)
}
f2 <- removeUpcast(f)
plotScan(f2)
