library(oce)
options(warn=1) # print warnings as they occur

dir <- paste(system("echo $HOME", intern=TRUE), "data/FlemishCap", sep="/")
files <- system(sprintf("find '%s' -name '*.ODF'", dir), intern=TRUE)

i <- 0
for (file in files) {
    cat("\n# ", file, "\n", sep="")
    d <- read.oce(file)
    summary(d)                     # VISUALLY: check .out file for incorrect units or unmatched names
    if ("temperature" %in% names(d[["data"]]) && min(d[["temperature"]], na.rm=TRUE) < -5)
        stop("bad min temperature in file '", file,
             "'; value is ", min(d[["temperature"]], na.rm=TRUE), "; did read.oce() catch the NullValue?")
    if ("u" %in% names(d[["data"]]) && min(d[["u"]], na.rm=TRUE) < -5)
        stop("bad min u in file '", file,
             "'; value is ", min(d[["u"]], na.rm=TRUE), "; did read.oce() catch the NullValue?")
    i <- i + 1
}
if (i > 0) {
    cat("Successfully checked", i, "ODF files in ", dir, "\n", sep="")
} else {
    cat("Found no ODF fies in", dir, "\n", sep="")
}
