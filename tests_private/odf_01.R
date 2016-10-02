library(oce)
options(warn=1) # print warnings as they occur
## I think this is all the ODFs in this dataset.
dirs <- c("/data/flemishCap/moorings/1840/ProcessedMC",
          "/data/flemishCap/moorings/1840/Processed_ADCP",
          "/data/flemishCap/moorings/1840/Processed_RCMnew",
          "/data/flemishCap/moorings/1841/ProcessedMC",
          "/data/flemishCap/moorings/1841/Processed_ADCP",
          "/data/flemishCap/moorings/1841/Processed_RCMnew",
          "/data/flemishCap/moorings/1842/ProcessedMC",
          "/data/flemishCap/moorings/1842/Processed_ADCP",
          "/data/flemishCap/CTD/2013/ODF",
          "/data/flemishCap/CTD/2013/QC",
          "/data/flemishCap/CTD/2014/ODF")
i <- 1
for (dir in dirs) {
    files <- list.files(path=dir, pattern="*.ODF", full.names=TRUE)
    for (file in files) {
        cat(file, "\n")
        d <- read.oce(file)
        i <- i + 1
    }
}
message("Tested", i, "files")

