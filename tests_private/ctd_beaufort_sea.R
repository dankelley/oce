library(oce)
require(testthat)
options(width=100)                     # makes summaries easier to read
options(warn=2)                        # die on warning, to catch unrecognized SBE names
Sys.setenv(TZ="America/Halifax")       # without a TZ, problems on macos high-sierra beta

years <- 2003:2012
path <- paste("~/data/arctic/beaufort/", years, sep="")
files <- list.files(path=path, pattern=".cnv", full.names=TRUE)
nfiles <- length(files)
if (!interactive()) png('ctd_beaufort_sea_%03d.png')
for (i in seq_along(files)) {
    cat("\n##\n# file[", i, "]= '",  files[i], "'\n", sep="")
    d <- read.oce(files[i])
    testthat::expect_false(is.na(d[['latitude']][1]))
    testthat::expect_false(is.na(d[['longitude']][1]))
    testthat::expect_true("temperature" %in% names(d[['data']]))
    summary(d)                         # so we can look ... hard to do, though
    cat("time starts: ", paste(d[["time"]][1:3], collapse=", "), "\n", sep="")
    if (files[i] == "~/data/arctic/beaufort/2007/d200720_049.cnv" && "gsw" == options()$oceEOS)
        expect_error(plot(d), "need at least two non-NA values to interpolate") # weird file, with just 1 level
    else
        plot(d)
}
cat("Successfully checked", nfiles, "CNV files in", path[1], "and sister directories\n")

if (!interactive()) dev.off()

