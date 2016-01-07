library(oce)

## below constructed from the files in oce/data
names <- c("adp", "adv", "argo", "cm", "coastlineWorld", "colors", "ctd",
           "ctdRaw", "echosounder", "landsat", "lisst", "lobo", "met", "rsk",
           "sealevel", "sealevelTuktoyaktuk", "section", "tidedata",
           "topoWorld", "wind")
for (name in names) {
    cat(name, "\n")
    data(list=name)
    if (name == "section") {
        x <- get(name)
        x <- x[["station", 1]]
    } else {
        x <- get(name)
    }
    if ("metadata" %in% slotNames(x)) {
        if ("units" %in% names(x@metadata)) {
            units <- x@metadata$units
            if (length(units) > 0) {
                unitsNames <- names(units)
                for (i in seq_along(units)) {
                    cat(sprintf("    %-15s ", unitsNames[i]))
                    this <- units[[i]]
                    if (length(this) != 2) {
                        cat("length:ERROR (should be 2, is ", length(this), ")\n", sep="")
                    } else {
                        cat("length:OK ")
                        if (2 == sum(c("unit", "scale") %in% names(this))) {
                            cat("names:OK ")
                        } else {
                            cat("names:ERROR (should be 'unit' and 'scale' but are ", 
                                paste(names(this), collapse=","), ")\n")
                        }
                        if (is.expression(this$unit)) {
                            cat("unit:OK ")
                        } else {
                            cat("unit:ERROR (not an expression) ")
                        }
                        if (is.character(this$scale)) {
                            cat("scale:OK ")
                        } else {
                            cat("scale:ERROR (not not an expression) ")
                        }
                        cat("\n")
                    }
                }
            } else {
                cat("    OK; empty units\n", sep="")
            }
        }
    } else {
        cat("    OK; no metadata\n")
    }
}
