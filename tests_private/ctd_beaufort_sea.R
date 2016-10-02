library(oce)
library(testthat)
options(width=100)                     # makes summaries easier to read
options(warn=2)                        # die on warning, to catch unrecognized SBE names
years <- 2003:2012
path <- paste("/data/arctic/beaufort/", years, sep="")
files <- list.files(path=path, pattern=".cnv", full=TRUE)
nfiles <- length(files)
if (!interactive()) png('ctd_beaufort_sea_%03d.png')
for (i in seq_along(nfiles)) {
    cat("\n##\n# file[", i, "]= '",  files[i], "'\n", sep="")
    d <- read.oce(files[i])
    expect_false(is.na(d[['latitude']][1]))
    expect_false(is.na(d[['longitude']][1]))
    expect_true("temperature" %in% names(d[['data']]))
    summary(d)                         # so we can look ... hard to do, though
    plot(d)
}
cat("Successfully checked", nfiles, "files\n")

if (!interactive()) dev.off()

