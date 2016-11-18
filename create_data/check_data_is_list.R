library(oce)

## below constructed from the files in oce/data
names <- c("adp", "adv", "argo", "cm", "coastlineWorld", "colors", "ctd",
           "ctdRaw", "echosounder", "landsat", "lisst", "lobo", "met", "rsk",
           "sealevel", "sealevelTuktoyaktuk", "section", "tidedata",
           "topoWorld", "wind")
for (name in names) {
    cat(sprintf("%20s", name))
    data(list=name)
    x <- get(name)
    if (!inherits(x, "oce")) {
        cat("    no @data        -- OK\n", sep="")
    } else {
        if (is.list(x@data)) {
            cat("    @data is a list -- OK\n", sep="")
        } else {
            cat("    @data is NOT a list -- ERROR\n", sep="")
        }
    }
}

