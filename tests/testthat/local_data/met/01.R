# See if any errors get reported
library(oce)
files <- list.files(pattern="^test_met", full.name=TRUE)
for (file in files) {
    cat(file, "has magic code", oceMagic(file), "\n")
    d <- read.oce(file)
    cat("    read.oce() worked\n")
    d <- read.met(file)
    cat("    read.met() worked\n")
}
