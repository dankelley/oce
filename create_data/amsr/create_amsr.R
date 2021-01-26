library(oce)
data(coastlineWorldFine, package="ocedata")

d1 <- read.amsr(download.amsr(2020, 8,  9, "~/data/amsr"))
d2 <- read.amsr(download.amsr(2020, 8, 10, "~/data/amsr"))
d3 <- read.amsr(download.amsr(2020, 8, 11, "~/data/amsr"))
d <- composite(d1, d2, d3)
amsr <- subset(d,    -80 < longitude & longitude < -40)
amsr <- subset(amsr,  30 < latitude  &  latitude <  60)

save(amsr, file="amsr.rda", version=2)
tools::resaveRdaFiles('amsr.rda', version=2)

