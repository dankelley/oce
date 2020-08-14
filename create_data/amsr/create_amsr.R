library(oce)
data(coastlineWorldFine, package="ocedata")

year <- 2020
month <- 8
day <- 9:11
d1 <- read.amsr(download.amsr(year, month, day[1], "~/data/amsr"))
d2 <- read.amsr(download.amsr(year, month, day[2], "~/data/amsr"))
d3 <- read.amsr(download.amsr(year, month, day[3], "~/data/amsr"))
d <- composite(d1, d2, d3)
W <- -80
E <- -40
S <- 30
N <- 60
amsr <- subset(d,    W < longitude & longitude < E, debug=3)
amsr <- subset(amsr, S < latitude  &  latitude < N, debug=3)

if (!interactive()) png("test_amsr.png")
plot(amsr, "SST")
lines(coastlineWorldFine[["longitude"]], coastlineWorldFine[["latitude"]])
if (!interactive()) dev.off()

save(amsr, file="amsr.rda", version=2)
tools::resaveRdaFiles('amsr.rda', version=2)

