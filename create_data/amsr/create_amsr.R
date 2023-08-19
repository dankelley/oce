library(oce)

# style=2 as of 2023 July 29.
amsr <- read.amsr(download.amsr(2023, 7, 27, destdir="~/data/amsr"))
# style=1 (prior to July 2023, but I'm not sure how much prior)
#  d1 <- read.amsr(download.amsr(2020, 8,  9, "~/data/amsr"))
#  d2 <- read.amsr(download.amsr(2020, 8, 10, "~/data/amsr"))
#  d3 <- read.amsr(download.amsr(2020, 8, 11, "~/data/amsr"))
#  amsr <- composite(d1, d2, d3)
options(warn=3) # convert warnings to errors, so we'll be sure to see them
amsr <- subset(amsr, -71 < longitude & longitude < -60)
amsr <- subset(amsr,  36 < latitude  &  latitude <  45)
save(amsr, file="amsr.rda", version=2)
tools::resaveRdaFiles('amsr.rda', version=2)
