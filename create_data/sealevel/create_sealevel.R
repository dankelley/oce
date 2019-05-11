library(oce)
sealevel <- read.oce("490-01-JAN-2003_slev.csv")
sealevel <- oce.edit(sealevel, "longitude", -sealevel[["longitude"]],
                     reason="Fix longitude hemisphere")
save(sealevel, file="sealevel.rda")
if (utils::compareVersion(R.Version()$minor, "3.6") >= 0) {
    tools::resaveRdaFiles("sealevel.rda", version=2)
} else {
    tools::resaveRdaFiles("sealevel.rda")
}

