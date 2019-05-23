library(oce)
data(adp)
adpOld <- adp                          # for checking
load("adp.rda")

summary(adp)

expect_equal(adpOld[["data"]], adp[["data"]])
names <- names(adp[["metadata"]])
namesOld <- names(adpOld[["metadata"]])
for (name in names) {
    if (name %in% namesOld)
        expect_equal(adpOld[["metadata"]][[name]], adp[["metadata"]][[name]])
}

