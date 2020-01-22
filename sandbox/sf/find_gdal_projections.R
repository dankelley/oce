## NOTE: the output matchs the python script of similar name, except that here
## we find that imw_p is valid.

debug <- TRUE
library(rgdal)
library(sf)
pi <- rgdal::projInfo()
names <- as.character(pi$name)
gdal <- rep(FALSE, length(names))
options(warn=-1)
for (i in seq_along(names)) {
    if (debug) cat(names[i], "\n")
    trialProj <- paste0("+proj=", names[i])
    t <- try(sf::st_crs(trialProj), silent=TRUE)
    if (inherits(t, "try-error") || is.na(t)) {
        trialProj <- paste(trialProj, "+lat_1=10 +lat_2=20")
        t <- try(sf::st_crs(trialProj), silent=TRUE)
        if (!inherits(t, "try-error") && !is.na(t)) {
            gdal[i] <- TRUE
            if (debug) cat(trialProj, " TRUE (first way)\n")
        }
    } else {
        gdal[i] <- TRUE
        if (debug) cat(trialProj, " TRUE (second way)\n")
    }
}
cat(length(proj), "total projections, ", sum(gdal), "supported, ", sum(!gdal), "unsupported\n")
cat("Supported: ", paste(names[gdal], collapse=", "), "\n")
cat("Not supported: ", paste(names[!gdal], collapse=", "), "\n")

