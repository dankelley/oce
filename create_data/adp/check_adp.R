library(oce)
data(adp)
adpOld <- adp                          # for checking
load("adp.rda")

summary(adp)
png("check_adp.png", unit="in", width=7, height=7, res=200)
plot(adp)
dev.off()
png("check_adp_old.png", unit="in", width=7, height=7, res=200)
plot(adpOld)
dev.off()

expect_equal(adpOld[["data"]], adp[["data"]])
names <- names(adp[["metadata"]])
namesOld <- names(adpOld[["metadata"]])
for (nameOld in namesOld) {
    cat("nameOld=", nameOld, "\n")
    ## NOTE: in early Aug, 2019, orientation became a vector
    if (nameOld %in% names && nameOld != "orientation")
        expect_equal(adpOld[["metadata"]][[nameOld]], adp[["metadata"]][[nameOld]])
}

