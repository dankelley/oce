library(oce)
data(adv)
advOLD <- adv
m05VectorEnu <- NULL # prevent code-analysis warning; the load() actually defines this.
load("~/data/archive/sleiwex/2008/moorings/m05/adv/nortek_1943/r/m05_vector_enu.rda")
adv <- window(m05VectorEnu, as.POSIXct("2008-07-01 00:00:00", tz = "UTC"), as.POSIXct("2008-07-01 00:01:00", tz = "UTC"))
adv@metadata$flags <- list() # the original object didn't have flags
adv@metadata$filename <- "(file name redacted)"
adv@metadata$serialNumber <- "(serial number redacted)"
adv@metadata$headSerialNumber <- "(head serial number redacted)"
adv@metadata$deploymentName <- "(deployment name redacted)"
adv@metadata$comments <- "sample ADV file"
adv@metadata$units$v <- list(unit = expression(m / s), scale = "")
adv@metadata$units$pressure <- list(unit = expression(dbar), scale = "")
adv@metadata$units$headingSlow <- list(unit = expression(degree), scale = "")
adv@metadata$units$pitchSlow <- list(unit = expression(degree), scale = "")
adv@metadata$units$rollSlow <- list(unit = expression(degree), scale = "")
adv@metadata$units$temperatureSlow <- list(unit = expression(degree * C), scale = "")
adv@metadata$numberOfCells <- NULL # issue 1381

if (FALSE) {
    # Save in version 2, because otherwise users with R 3.5.x and earlier will not
    # be able to use data("adv")
    if (utils::compareVersion(paste0(R.Version()$major, ".", R.Version()$minor), "3.6.0") >= 0) {
        message("saving with version=2 since R version is 3.6.0 or later")
        save(adv, file = "adv.rda", version = 2)
        tools::resaveRdaFiles("adv.rda", version = 2)
    } else {
        save(adv, file = "adv.rda")
        tools::resaveRdaFiles("adv.rda")
    }
}

pdf("adv_comparison.pdf")
plot(advOLD)
mtext("data(adv), i.e. advOLD", side = 4, col = 2)
plot(adv)
mtext("new trial for adv -- is this different because of new transformation matrix?)", side = 4, col = 2)
dev.off()
