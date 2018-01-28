library(oce)
require(testthat)
options(width=100)                     # makes summaries easier to read
options(warn=2)                        # die to reveal unknown SBE names
Sys.setenv(TZ="America/Halifax")       # without a TZ, problems on macos high-sierra beta

years <- 2011:2014
path <- paste("/data/flemishCap/CTD/", years, sep="")
files <- list.files(path=path, pattern=".cnv", full.names=TRUE)
cat("have ", length(files), "files\n")
nfiles <- length(files)
if (!interactive()) png('flemish_cap_%03d.png')
for (i in seq_along(files)) {
    cat("\n##\n# file[", i, "]= '",  files[i], "'\n", sep="")
    d <- read.oce(files[i])
    testthat::expect_false(is.na(d[['latitude']][1]))
    testthat::expect_false(is.na(d[['longitude']][1]))
    testthat::expect_true("temperature" %in% names(d[['data']]))
    summary(d)                         # so we can look ... hard to do, though
    plot(d)
}
cat("Successfully checked", nfiles, "CNV files in /data/flemishCap/CTD/*/\n")

if (!interactive()) dev.off()
