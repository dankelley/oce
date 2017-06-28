library(oce)
options(warn=1) # print warnings as they occur

dir <- "/data/odf/ctd"
files <- system(sprintf("find '%s' -name '*.ODF'", dir), intern=TRUE)
if (length(files)) {

    i <- 0
    for (file in files) {
        cat("\n# ", file, "\n")
        d <- read.oce(file)
        ## Can we summarize?
        summary(d)                     # VISUALLY: check .out file for incorrect units or unmatched names
        ## Are temperatures or velocities crazy? (Checks handling of missing codes of -99.)
        if ("temperature" %in% names(d[["data"]]) && min(d[["temperature"]], na.rm=TRUE) < -5)
            stop("bad min temperature in file '", file,
                 "'; value is ", min(d[["temperature"]], na.rm=TRUE), "; did read.oce() catch the NullValue?")
        if ("u" %in% names(d[["data"]]) && min(d[["u"]], na.rm=TRUE) < -5)
            stop("bad min u in file '", file,
                 "'; value is ", min(d[["u"]], na.rm=TRUE), "; did read.oce() catch the NullValue?")
        ## Do read.odf() and read.ctd() give the same fields?
        dd <- read.odf(file)
        expect_equal(sort(names(d@metadata)), sort(names(dd@metadata)))
        expect_equal(sort(names(d@data)), sort(names(dd@data)))
        i <- i + 1
    }
    cat("Successfully checked", i, "ODF files in ", dir, "\n")
}
