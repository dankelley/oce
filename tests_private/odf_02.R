library(oce)
options(warn=1) # print warnings as they occur

files <- system("find '/data/Barrow Strait Data' -name '*.ODF'", intern=TRUE)

i <- 1
for (file in files) {
    cat("\n ## ", file, "\n")
    d <- read.oce(file)
    summary(d)                     # VISUALLY: check .out file for incorrect units or unmatched names
    i <- i + 1
}
cat("Successfully checked", i, "ODF files in various subdirectories of /data/Barrow Strait Data\n")

