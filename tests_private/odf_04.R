library(oce)
options(warn=1) # print warnings as they occur

dir <- "/data/odf/ctd"
files <- system(sprintf("find '%s' -name '*.ODF'", dir), intern=TRUE)
if (length(files)) {

    i <- 0
    for (file in files) {
        cat("\n# ", file, "\n")
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
    cat("Successfully checked", i, "ODF files in ", dir, "\n")

    ## test for issue 1263 (a particular file)
    if (file.exists("/data/odf/ctd/CTD_HUD2000009_1_1_DN.ODF")) {
        oce <- read.oce("/data/odf/ctd/CTD_HUD2000009_1_1_DN.ODF")
        odf <- read.odf("/data/odf/ctd/CTD_HUD2000009_1_1_DN.ODF")
        print(setdiff(names(oce@metadata), names(odf@metadata)))
        ## expect_equal(names(odf@metadata), names(oce@metadata))
        expect_equal(names(odf@data), names(oce@data))
    }
}
