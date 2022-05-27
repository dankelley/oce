library(oce)
data(coastlineWorldFine, package="ocedata")

d1 <- read.amsr(download.amsr(2020, 8,  9, "~/data/amsr"))
d2 <- read.amsr(download.amsr(2020, 8, 10, "~/data/amsr"))
d3 <- read.amsr(download.amsr(2020, 8, 11, "~/data/amsr"))
d <- composite(d1, d2, d3)
amsr <- subset(d,    -71 < longitude & longitude < -60)
amsr <- subset(amsr,  36 < latitude  &  latitude <  45)

save(amsr, file="amsr.rda", version=2)
tools::resaveRdaFiles('amsr.rda', version=2)

