library(oce)
options(warn=1) # print warnings as they occur

dir <- "/data/FlemishCap"
files <- system(sprintf("find '%s' -name '*.ODF'", dir), intern=TRUE)

i <- 0
for (file in files) {
    cat("\n# ", file, "\n")
    d <- read.oce(file)
    summary(d)                     # VISUALLY: check .out file for incorrect units or unmatched names
    i <- i + 1
}
cat("Successfully checked", i, "ODF files in various subdirectories of ", dir, "\n")

