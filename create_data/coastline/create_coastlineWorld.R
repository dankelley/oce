library(oce)

coastlineWorld <- read.oce("ne_110m_admin_0_countries/ne_110m_admin_0_countries.shp")
## We happen to know the units; read.oce() does not try to infer them
coastlineWorld@metadata$units$longitude <- list(unit=expression(degree*E), scale="")
coastlineWorld@metadata$units$latitude <- list(unit=expression(degree*N), scale="")

## Save in version 2, because otherwise users with R 3.5.x and earlier will not
## be able to use data("coastlineWorld")
if (utils::compareVersion(paste0(R.Version()$major, ".", R.Version()$minor), '3.6.0') >= 0) {
    message("saving with version=2 since R version is 3.6.0 or later")
    save(coastlineWorld, file="coastlineWorld.rda", version=2)
    tools::resaveRdaFiles("coastlineWorld.rda", version=2)
} else {
    save(coastlineWorld, file="coastlineWorld.rda")
    tools::resaveRdaFiles("coastlineWorld.rda")
}

