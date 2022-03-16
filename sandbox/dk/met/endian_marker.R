csvs <- paste0("/Library/Frameworks/R.framework/Versions/4.1/Resources/library/oce/extdata/test_met_vsn", c(1:3), ".csv")
for (csv in csvs) {
    file  <- file(csv, "rb")
    buf <- readBin(file, "raw", 10)
    cat(csv, ":\n    0x", paste(toupper(buf), collapse=" 0x"), "\n", sep="")
    close(file)
}
