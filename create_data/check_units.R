library(oce)

## below constructed from the files in oce/data
names <- c("adp", "adv", "argo", "cm", "coastlineWorld", "colors", "ctd",
           "ctdRaw", "echosounder", "landsat", "lisst", "lobo", "met", "rsk",
           "sealevel", "sealevelTuktoyaktuk", "section", "tidedata",
           "topoWorld", "wind")
for (name in names) {
    if (name == "section")
        next # this is a list of CTD objects
    data(list=name)
    x <- get(name)
    if ("metadata" %in% slotNames(x)) {
        if ("units" %in% names(x@metadata)) {
            if (length(x@metadata$units))
                cat(name, "@metadata$units: ", paste(names(x@metadata$units), collapse=" "), "\n", sep="") else
                    cat(name, "@metadata$units exists but has no contents\n", sep="")
        } else {
            cat(name, "@metadata has no 'units' item. FIXME: UPDATE THIS FILE\n", sep="")
        }
    } else {
        cat(name, "has no metadata so nothing to check\n")
    }
}
