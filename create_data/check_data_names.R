library(oce)

## below constructed from the files in oce/data
names <- c("adp", "adv", "argo", "cm", "coastlineWorld", "colors", "ctd",
           "ctdRaw", "echosounder", "landsat", "lisst", "lobo", "met", "rsk",
           "sealevel", "sealevelTuktoyaktuk", "section", "tidedata",
           "topoWorld", "wind")
for (name in names) {
    cat(name, "\n")
    data(list=name)
    x <- get(name)
    if (inherits(x, "oce")) {
        cat("  names(x@data): ", paste(names(x@data), collapse="/"), "\n")
    } else {
        cat("  x is not an oce object\n")
    }
    cat("\n")
}

