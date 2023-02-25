## Tests some Barrow Strait ODF files for oce
## 1. ability to read them
## 2. ability to summarize them
## 3. finding certain header items
library(oce)
library(testthat)

dirs <- c("/data/Barrow Strait Data/40mODF/s2003_2004/BottomPressure",
          "/data/Barrow Strait Data//2003_2004/MooredCTD",
          "/data/Barrow Strait Data//2005_2006/ADCP",
          "/data/Barrow Strait Data//2005_2006/ADCP",
          "/data/Barrow Strait Data//2005_2006/Bottom_Pressure",
          "/data/Barrow Strait Data//2005_2006/Moored_CTD",
          "/data/Barrow Strait Data/2003_2004/Icycler/Icycler_ODF_Format",
          "/data/Barrow Strait Data/2005_2006/Moored_CTD/40mODFs",
          "/data/Barrow Strait Data/2005_2006/Standard CTD/2005")

for (dir in dirs) {
    cat("'", dir, "'\n", sep="")
    for (file in list.files(path=dir, pattern="\\.ODF$")) {
        cat("  '", file, "'\n", sep="")
        d <- read.oce(paste(dir, file, sep="/"))
        summary(d)
        expect_true(is.list(d[["header"]]))
        ## print(names(d[['header']]))
        expect_true(all(c("ODF_HEADER", "CRUISE_HEADER", "EVENT_HEADER",
                          "INSTRUMENT_HEADER", "HISTORY_HEADER",
                          "PARAMETER_HEADER", "RECORD_HEADER") %in% names(d[["header"]])))
    }
}

