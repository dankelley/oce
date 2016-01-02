library(oce)
sealevel <- read.oce("490-01-JAN-2003_slev.csv")
sealevel <- oce.edit(sealevel, "longitude", -sealevel[["longitude"]],
                     reason="Fix longitude hemisphere")
save(sealevel, file='sealevel.rda')
library(tools)
tools::resaveRdaFiles("sealevel.rda", compress="auto")
