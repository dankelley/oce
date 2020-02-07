library(oce)
options(warn=1) # print warnings as they occur

dir <- paste(system("echo $HOME", intern=TRUE), "data/odf/ctd", sep="/")
files <- system(sprintf("find '%s' -name '*.ODF'", dir), intern=TRUE)

i <- 0
for (file in files) {
    cat("\n# ", file, "\n")
    oce <- read.oce(file)
    ## Can we summarize? (visual check on incorrect units or mismatched names)
    summary(oce)
    ## Are temperatures or velocities crazy? (Checks handling of missing codes of -99.)
    if ("temperature" %in% names(oce[["data"]]) && min(oce[["temperature"]], na.rm=TRUE) < -5)
        stop("bad min temperature in file '", file,
             "'; value is ", min(oce[["temperature"]], na.rm=TRUE), "; did read.oce() catch the NullValue?")
    if ("u" %in% names(oce[["data"]]) && min(oce[["u"]], na.rm=TRUE) < -5)
        stop("bad min u in file '", file,
             "'; value is ", min(oce[["u"]], na.rm=TRUE), "; did read.oce() catch the NullValue?")
    ## Do read.odf() and read.ctd() give the same fields?
    odf <- read.odf(file)
    expect_equal(0, length(setdiff(names(oce@metadata), names(odf@metadata))))
    expect_equal(0, length(setdiff(names(oce@data), names(odf@data))))
    i <- i + 1
}
if (i > 0) {
    cat("Successfully checked", i, "ODF files in ", dir, "\n")
} else {
    cat("Found no ODF fies in", dir, "\n")
}

