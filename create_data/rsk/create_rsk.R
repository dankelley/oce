library(oce)
rsk <- read.oce("060130_20150904_1159.rsk", allTables=FALSE)
rsk <- oceSetMetadata(rsk, "longitude", -(56 + 26.232/60))
rsk <- oceSetMetadata(rsk, "latitude", 73 + 13.727/60)
rsk <- oceSetMetadata(rsk, "station", "C18")
rsk <- oceSetMetadata(rsk, "ship", "Ault")
rsk <- oceSetMetadata(rsk, "institute", "Ocean Research Project")
# isolate to downcast (reduces object size, and we normally do this anyway)
rsk <- subset(rsk, 1441381040 < time & time < 1441381480)

# Save in version 2, because otherwise users with R 3.5.x and earlier will not
# be able to use data("rsk")
if (utils::compareVersion(paste0(R.Version()$major, ".", R.Version()$minor), "3.6.0") >= 0) {
    message("saving with version=2 since R version is 3.6.0 or later")
    save(rsk, file="rsk.rda", version=2)
    tools::resaveRdaFiles("rsk.rda", version=2)
} else {
    save(rsk, file="rsk.rda")
    tools::resaveRdaFiles("rsk.rda")
}
