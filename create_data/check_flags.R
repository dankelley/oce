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
        if ("flags" %in% names(x@metadata)) {
            if (length(x@metadata$flags))
                cat("          ", name, "@metadata$flags: ", paste(names(x@metadata$flags), collapse=" "), "\n", sep="") else
                    cat("          ", name, "@metadata$flags exists but has no contents\n", sep="")
        } else {
            cat("**FIXME** ", name, "@metadata has no 'flags' item. FIXME: UPDATE THIS FILE\n", sep="")
        }
    } else {
        cat("         ", name, "has no metadata so nothing to check\n")
    }
}
