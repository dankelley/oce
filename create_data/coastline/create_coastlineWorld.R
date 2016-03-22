library(oce)

coastlineWorld <- read.oce("ne_110m_admin_0_countries/ne_110m_admin_0_countries.shp")
## We happen to know the units; read.oce() does not try to infer them
coastlineWorld@metadata$units$longitude <- list(unit=expression(degree*E), scale="")
coastlineWorld@metadata$units$latitude <- list(unit=expression(degree*N), scale="")
save(coastlineWorld, file="coastlineWorld.rda")
tools::resaveRdaFiles("coastlineWorld.rda", compress="auto")
