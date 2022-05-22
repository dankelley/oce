library(oce)
require(testthat)
options(width=100)                     # makes summaries easier to read
#options(warn=2)                        # die on warning, to catch unrecognized SBE names
Sys.setenv(TZ="America/Halifax")       # without a TZ, problems on macos high-sierra beta

count <- 0L
years <- 2003:2012
for (year in years) {
    cat("year=", year, "\n")
    path <- paste("~/data/arctic/beaufort/", year, sep="")
    files <- sort(list.files(path=path, pattern=".cnv", full.names=TRUE))
    nfiles <- length(files)
    for (i in seq_along(files)) {
        if (!interactive())
            png(sprintf("ctd_beaufort_sea_%d_%03d.png", year, i))
        cat("\n##\n# file[", i, "]= '",  files[i], "'\n", sep="")
        d <- read.oce(files[i])
        testthat::expect_false(is.na(d[['latitude']][1]))
        testthat::expect_false(is.na(d[['longitude']][1]))
        testthat::expect_true("temperature" %in% names(d[['data']]))
        summary(d)                     # for inspection
        #cat("time[1]: ", d[["time"]][1], "\n", sep="") # not sure why
        if (files[i] == "~/data/arctic/beaufort/2007/d200720_049.cnv" && "gsw" == options()$oceEOS)
            expect_error(plot(d), "need at least two non-NA values to interpolate") # weird file, with just 1 level
        else
            plot(d)
        if (!interactive())
            dev.off()
        count <- count + 1L
    }
    cat("Successfully checked", nfiles, "CNV files in", path[1], "and sister directories\n")
}
cat("Processed", count, "files\n")


