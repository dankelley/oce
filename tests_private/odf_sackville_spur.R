library(oce)
require(testthat)
options(width=100)                     # makes summaries easier to read
options(warn=2)                        # die to reveal unknown SBE names
#Sys.setenv(TZ="America/Halifax")       # without a TZ, problems on macos high-sierra beta

dirs <- c("ProcessedMC", "Processed_ADCP", "Processed_RCMnew")
for (dir in dirs) {
    path <- paste0("~/Dropbox/data/sackville_spur/1840/", dir)
    files <- list.files(path=path, pattern=".ODF", full.names=TRUE)
    cat("Examining ", length(files), " files in ", path, "\n", sep="")
    nfiles <- length(files)
    for (i in seq_len(nfiles)) {
        cat("\n# file ", i, " of ", nfiles, ": \"",  files[i], "\"\n", sep="")
        read.oce(files[i]) |> summary()
    }
    cat("Successfully checked ", nfiles, " ODF files in ", path, "\n", sep="")
}
