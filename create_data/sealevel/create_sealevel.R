library(oce)
sealevel <- read.oce("490-01-JAN-2003_slev.csv")
sealevel <- oce.edit(sealevel, "longitude", -sealevel[["longitude"]],
                     reason="Fix longitude hemisphere")

if (utils::compareVersion(R.Version()$minor, "3.6") >= 0) {
    save(sealevel, file="sealevel.rda", version=2)
    tools::resaveRdaFiles("sealevel.rda", version=2)
} else {
    save(sealevel, file="sealevel.rda")
    tools::resaveRdaFiles("sealevel.rda")
}

