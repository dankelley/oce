library(oce)
if (file.exists("~/data/archive/sleiwex/2008/moorings/m07/adp/sontek_h53/raw/adp_sontek_h53.adp")) {
    d <- read.oce("~/data/archive/sleiwex/2008/moorings/m07/adp/sontek_h53/raw/adp_sontek_h53.adp")
    summary(d)
    if (!interactive()) png("adp_sontek.png")
    plot(d)
    if (!interactive()) dev.off()
}


