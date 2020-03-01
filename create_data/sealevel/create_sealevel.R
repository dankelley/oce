library(oce)
sealevel <- read.oce("490-01-JAN-2003_slev.csv")
sealevel <- oce.edit(sealevel, "longitude", -sealevel[["longitude"]],
                     reason="Fix longitude hemisphere")

## Save in version 2, because otherwise users with R 3.5.x and earlier will not
## be able to use data("sealevel")
if (utils::compareVersion(paste0(R.Version()$major, ".", R.Version()$minor), '3.6.0') >= 0) {
    message("saving with version=2 since R version is 3.6.0 or later")
    save(sealevel, file="sealevel.rda", version=2)
    tools::resaveRdaFiles("sealevel.rda", version=2)
} else {
    save(sealevel, file="sealevel.rda")
    tools::resaveRdaFiles("sealevel.rda")
}

