library(oce)
library(testthat)
options(width=100)                     # makes summaries easier to read
options(warn=2)                        # die on warning, to catch unrecognized SBE names

path <- "/data/archive/sleiwex/2008/ships/coriolisii/ctd/01-cnv"
files <- list.files(path=path, pattern=".cnv", full.names=TRUE)
nfiles <- length(files)
if (!interactive()) png('ctd_sleiwex_coriolisII_%03d.png')
par(oma=c(0, 0, 1, 0))
for (i in seq_along(files)) {
    cat("\n##\n# file[", i, "]= '",  files[i], "'\n", sep="")
    d <- read.oce(files[i])
    expect_false(is.na(d[['latitude']])[1])
    expect_false(is.na(d[['longitude']][1]))
    expect_true("temperature" %in% names(d[['data']]))
    summary(d)                         # so we can look ... hard to do, though
    plot(d)
}
cat("Successfully checked", nfiles, "files\n")

if (!interactive()) dev.off()

