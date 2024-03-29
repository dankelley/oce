library(oce)
data(coastlineWorldFine, package="ocedata")
load("amsr.rda")
summary(amsr)

if (!interactive()) png("test_amsr.png")
plot(amsr, "SST")
lines(coastlineWorldFine[["longitude"]], coastlineWorldFine[["latitude"]])
if (!interactive()) dev.off()

