library(oce)
devtools::install_github("dankelley/dod")

type <- "new" # as of 2023-07-30, the new-style format

if (type == "new") {
    library(dod) # from github.com/dankelley/dod
    amsr <- read.amsr(dod.amsr(2023, 7, 27, destdir="~/data/amsr"))
} else {
    d1 <- read.amsr(download.amsr(2020, 8,  9, "~/data/amsr"))
    d2 <- read.amsr(download.amsr(2020, 8, 10, "~/data/amsr"))
    d3 <- read.amsr(download.amsr(2020, 8, 11, "~/data/amsr"))
    amsr <- composite(d1, d2, d3)
}
amsr <- subset(amsr, -71 < longitude & longitude < -60, debug=2)
amsr <- subset(amsr,  36 < latitude  &  latitude <  45, debug=2)
save(amsr, file="amsr.rda", version=2)
tools::resaveRdaFiles('amsr.rda', version=2)
